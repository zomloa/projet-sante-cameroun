# =====================================================================
# 01 - IMPORT, EXPLORATION, NETTOYAGE, TRANSFORMATION
# Dataset : consultations de centres de santé - Cameroun (2025)
# Sortie  : data/consultations_clean.rds / .csv, outputs/journal_qualite.csv
# =====================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
})

# Dossier racine du projet (fonctionne depuis R/ ou depuis la racine)
racine <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()
dir.create(file.path(racine, "outputs"), showWarnings = FALSE)
dir.create(file.path(racine, "app", "data"), showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 1. IMPORT
# ---------------------------------------------------------------------
brut <- read_csv(
  file.path(racine, "data", "consultations_raw.csv"),
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8"),
  na = character()           # on garde les "" pour les traiter explicitement
)
cat("Dimensions brutes :", nrow(brut), "lignes x", ncol(brut), "colonnes\n")

# ---------------------------------------------------------------------
# 2. EXPLORATION : on comprend avant d'agir
# ---------------------------------------------------------------------
cat("\n--- Valeurs vides par colonne ---\n")
print(brut %>% summarise(across(everything(), ~ sum(str_trim(.x) == ""))) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "vides") %>%
        filter(vides > 0))

cat("\n--- Modalités des variables catégorielles ---\n")
for (v in c("region", "gender", "diagnosis", "insurance_status",
            "consultation_type", "medication_available")) {
  cat("\n", v, ":", paste(sort(unique(brut[[v]])), collapse = " | "), "\n")
}

cat("\n--- Doublons ---\n")
cat("patient_id dupliqués        :", sum(duplicated(brut$patient_id)), "\n")
cat("lignes strictement identiques:", sum(duplicated(brut)), "\n")

age_num  <- suppressWarnings(as.numeric(brut$patient_age))
cout_num <- suppressWarnings(as.numeric(brut$treatment_cost))
cat("\n--- Âge ---\n"); print(summary(age_num))
cat("\n--- Coût ---\n"); print(summary(cout_num))
cat("Pics suspects de coût :\n"); print(sort(table(cout_num[cout_num %in% c(-100, 9999, 50000)]), decreasing = TRUE))
cat("Âge = 0 :", sum(age_num == 0, na.rm = TRUE),
    "| âge = 1 :", sum(age_num == 1, na.rm = TRUE),
    "(pic anormal à 0 => valeur par défaut/placeholder)\n")

# ---------------------------------------------------------------------
# 3. NETTOYAGE
# ---------------------------------------------------------------------
# 3.1 Référentiel district -> région (le district détermine la région)
norm_region <- function(x) {
  x <- str_to_lower(str_squish(x))
  case_when(
    x == ""                                   ~ NA_character_,
    x %in% c("centre", "centre", "ctr", "center") ~ "Centre",
    x %in% c("littoral", "litoral")           ~ "Littoral",
    x %in% c("sud-ouest", "sud ouest", "sw")  ~ "Sud-Ouest",
    x == "nord-ouest"                         ~ "Nord-Ouest",
    x == "extrême-nord"                       ~ "Extrême-Nord",
    TRUE                                      ~ str_to_title(x)
  )
}

ref_district <- brut %>%
  mutate(region_std = norm_region(region), district = str_squish(district)) %>%
  filter(!is.na(region_std)) %>%
  count(district, region_std) %>%
  group_by(district) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  select(district, region_ref = region_std)

# 3.2 Nettoyage ligne à ligne
propre <- brut %>%
  mutate(across(everything(), str_squish)) %>%
  left_join(ref_district, by = "district") %>%
  mutate(
    # Régions : harmonisation + imputation via le district
    region_std = norm_region(region),
    region     = coalesce(region_std, region_ref),

    # Dates : formats ISO (aaaa-mm-jj) et jj/mm/aaaa mélangés
    consultation_date = coalesce(
      ymd(consultation_date, quiet = TRUE),
      dmy(consultation_date, quiet = TRUE)
    ),

    # Genre : harmonisation en français
    gender = case_when(
      str_to_lower(gender) %in% c("female", "f", "feminin", "féminin") ~ "Féminin",
      str_to_lower(gender) %in% c("male", "m", "masculin")              ~ "Masculin",
      TRUE                                                             ~ NA_character_
    ),

    diagnosis        = na_if(diagnosis, ""),
    insurance_status = recode(na_if(insurance_status, ""),
                              Insured = "Assuré", Uninsured = "Non assuré"),
    medication_available = recode(medication_available,
                                  Available = "Disponible", Stockout = "Rupture"),
    consultation_type = recode(consultation_type,
                               Outpatient = "Consultation externe", Emergency = "Urgence",
                               Prenatal = "Prénatale", Vaccination = "Vaccination",
                               `Follow-up` = "Suivi"),

    patient_age    = as.numeric(patient_age),
    treatment_cost = as.numeric(na_if(treatment_cost, ""))
  )

# 3.3 Valeurs aberrantes -> NA (traçabilité conservée dans des indicateurs)
propre <- propre %>%
  mutate(
    age_aberrant  = !is.na(patient_age) & (patient_age < 0 | patient_age > 120),
    age_defaut    = !is.na(patient_age) & patient_age == 0,   # pic artificiel
    cout_aberrant = !is.na(treatment_cost) & (treatment_cost <= 0 | treatment_cost > 1000),
    patient_age    = if_else(age_aberrant | age_defaut, NA_real_, patient_age),
    treatment_cost = if_else(cout_aberrant, NA_real_, treatment_cost)
  )

# 3.4 Doublons : après harmonisation, mêmes patients = même ligne
n_avant <- nrow(propre)
divergents <- propre %>%
  group_by(patient_id) %>%
  filter(n() > 1) %>%
  summarise(across(c(consultation_date, region, district, patient_age, gender, diagnosis,
                     treatment_cost, consultation_type), n_distinct), .groups = "drop") %>%
  filter(if_any(-patient_id, ~ .x > 1)) %>% nrow()
cat("\nPatients dupliqués avec champs divergents après harmonisation :", divergents, "\n")

propre <- propre %>% distinct(patient_id, .keep_all = TRUE)
n_doublons <- n_avant - nrow(propre)

# ---------------------------------------------------------------------
# 4. TRANSFORMATION : nouvelles variables
# ---------------------------------------------------------------------
propre <- propre %>%
  mutate(
    mois        = floor_date(consultation_date, "month"),
    mois_num    = month(consultation_date),
    trimestre   = paste0("T", quarter(consultation_date)),
    jour_semaine = wday(consultation_date, week_start = 1),
    groupe_age  = cut(patient_age, breaks = c(0, 4, 14, 24, 44, 64, Inf),
                      labels = c("1-4 ans", "5-14 ans", "15-24 ans", "25-44 ans", "45-64 ans", "65 ans et +"),
                      include.lowest = TRUE),
    type_etablissement = case_when(
      str_detect(facility_name, "^District Hospital")  ~ "Hôpital de district",
      str_detect(facility_name, "^Hôpital Régional")   ~ "Hôpital régional",
      str_detect(facility_name, "^Centre de Santé")    ~ "Centre de santé urbain",
      str_detect(facility_name, "^Centre Médical")     ~ "Centre médical intégré",
      str_detect(facility_name, "^Clinique")           ~ "Clinique communautaire",
      TRUE                                             ~ "Autre"
    ),
    rupture   = medication_available == "Rupture",
    assure    = insurance_status == "Assuré",
    niveau_cout = cut(treatment_cost, breaks = c(0, 10, 20, 30, Inf),
                      labels = c("< 10", "10-20", "20-30", "> 30"), include.lowest = TRUE)
  ) %>%
  select(patient_id, consultation_date, mois, mois_num, trimestre, jour_semaine,
         region, district, facility_name, type_etablissement,
         patient_age, groupe_age, gender, diagnosis, treatment_cost, niveau_cout,
         medication_available, rupture, consultation_type, insurance_status, assure,
         age_aberrant, age_defaut, cout_aberrant) %>%
  mutate(across(c(region, district, facility_name, type_etablissement, gender, diagnosis,
                  medication_available, consultation_type, insurance_status), as.factor))

# ---------------------------------------------------------------------
# 5. CONTRÔLES FINAUX + JOURNAL QUALITÉ
# ---------------------------------------------------------------------
stopifnot(!anyDuplicated(propre$patient_id), !anyNA(propre$consultation_date),
          !anyNA(propre$region), nlevels(propre$region) == 10)

journal <- tribble(
  ~probleme, ~traitement, ~lignes_concernees,
  "Doublons (patient_id répétés)", "Suppression après harmonisation", n_doublons,
  "Régions incohérentes (LITTORAL, litoral, SW, sud ouest, Ctr, CENTER, centre)", "Harmonisées en 10 régions officielles",
      sum(!brut$region %in% c("", "Adamaoua", "Centre", "Est", "Extrême-Nord", "Littoral", "Nord",
                              "Nord-Ouest", "Ouest", "Sud", "Sud-Ouest")),
  "Régions manquantes", "Imputées à partir du district (référentiel)", sum(brut$region == ""),
  "Dates au format jj/mm/aaaa", "Converties en date ISO", sum(str_detect(brut$consultation_date, "/")),
  "Genre : modalités mixtes (M, F, male, Feminin...)", "Harmonisé : Féminin / Masculin", sum(!brut$gender %in% c("", "Male", "Female")),
  "Genre manquant", "Laissé NA (non imputable)", sum(brut$gender == ""),
  "Diagnostic manquant", "Laissé NA, exclu des analyses épidémiologiques", sum(brut$diagnosis == ""),
  "Assurance manquante", "Laissée NA", sum(brut$insurance_status == ""),
  "Coût manquant", "Laissé NA (exclu des moyennes)", sum(brut$treatment_cost == ""),
  "Coûts aberrants (<=0 ou >1000 : -100, 9999, 50000)", "Mis à NA", sum(cout_num <= 0 | cout_num > 1000, na.rm = TRUE),
  "Âges impossibles (<0 ou >120 : -5, 140, 222)", "Mis à NA", sum(brut$patient_age %in% c("-5", "140", "222")),
  "Âge = 0 (pic artificiel, valeur par défaut probable)", "Mis à NA", sum(brut$patient_age == "0")
)
write_csv(journal, file.path(racine, "outputs", "journal_qualite.csv"))
write_csv(journal, file.path(racine, "app", "data", "journal_qualite.csv"))
print(journal, n = Inf, width = Inf)

cat("\nDimensions propres :", nrow(propre), "lignes x", ncol(propre), "colonnes\n")
cat("Taux de complétude par colonne (%) :\n")
print(round(100 * colMeans(!is.na(propre)), 1))

saveRDS(propre, file.path(racine, "data", "consultations_clean.rds"))
saveRDS(propre, file.path(racine, "app", "data", "consultations_clean.rds"))
write_csv(propre, file.path(racine, "data", "consultations_clean.csv"))
cat("\nFichiers propres écrits.\n")
