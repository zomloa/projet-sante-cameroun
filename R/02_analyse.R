# =====================================================================
# 02 - ANALYSE BUSINESS, VISUALISATION, STATISTIQUES DESCRIPTIVES
# Entrée : data/consultations_clean.rds
# Sorties: outputs/figures/*.png, outputs/tables/*.csv, outputs/resultats.rds
# =====================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(broom)
})

racine <- if (basename(getwd()) == "R") dirname(getwd()) else getwd()
source(file.path(racine, "R", "theme_sante.R"))
dir.create(file.path(racine, "outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(racine, "outputs", "tables"),  recursive = TRUE, showWarnings = FALSE)

df <- readRDS(file.path(racine, "data", "consultations_clean.rds"))
res <- list()   # tout ce que le rapport réutilise

save_fig <- function(p, nom, w = 9, h = 5.2) {
  ggsave(file.path(racine, "outputs", "figures", paste0(nom, ".png")), p,
         width = w, height = h, dpi = 150, bg = "white")
}
save_tab <- function(t, nom) write_csv(t, file.path(racine, "outputs", "tables", paste0(nom, ".csv")))
pct <- function(x) percent(x, accuracy = 0.1, decimal.mark = ",")

# ---------------------------------------------------------------------
# A. INDICATEURS CLÉS
# ---------------------------------------------------------------------
res$kpi <- tibble(
  consultations   = nrow(df),
  patients_uniques = n_distinct(df$patient_id),
  periode_debut   = min(df$consultation_date),
  periode_fin     = max(df$consultation_date),
  age_moyen       = mean(df$patient_age, na.rm = TRUE),
  cout_moyen      = mean(df$treatment_cost, na.rm = TRUE),
  cout_median     = median(df$treatment_cost, na.rm = TRUE),
  cout_total      = sum(df$treatment_cost, na.rm = TRUE),
  taux_rupture    = mean(df$rupture),
  taux_assures    = mean(df$assure, na.rm = TRUE),
  part_feminin    = mean(df$gender == "Féminin", na.rm = TRUE),
  nb_regions      = n_distinct(df$region), nb_districts = n_distinct(df$district),
  nb_etablissements = n_distinct(df$facility_name)
)
print(res$kpi, width = Inf)

# ---------------------------------------------------------------------
# B. QUESTIONS MÉTIER
# ---------------------------------------------------------------------
# B1. Où se concentre l'activité ? (région, district, établissement)
res$par_region <- df %>% group_by(region) %>%
  summarise(consultations = n(), cout_moyen = mean(treatment_cost, na.rm = TRUE),
            taux_rupture = mean(rupture), taux_assures = mean(assure, na.rm = TRUE),
            age_median = median(patient_age, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(consultations))
save_tab(res$par_region, "par_region")

res$par_district <- df %>% group_by(region, district) %>%
  summarise(consultations = n(), taux_rupture = mean(rupture),
            cout_moyen = mean(treatment_cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(consultations))
save_tab(res$par_district, "par_district")

res$top_etab <- df %>% group_by(facility_name, district) %>%
  summarise(consultations = n(), taux_rupture = mean(rupture), .groups = "drop") %>%
  arrange(desc(consultations)) %>% slice_head(n = 10)
save_tab(res$top_etab, "top10_etablissements")

p <- res$par_region %>% mutate(region = fct_reorder(region, consultations)) %>%
  ggplot(aes(consultations, region)) +
  geom_col(fill = pal_sante$teal, width = .7) +
  geom_text(aes(label = comma(consultations, big.mark = " ")), hjust = -.15, size = 3.6, colour = pal_sante$encre) +
  scale_x_continuous(expand = expansion(mult = c(0, .12)), labels = label_comma(big.mark = " ")) +
  labs(title = "Consultations par région", subtitle = "Année 2025, après nettoyage (n = 10 000)",
       x = "Nombre de consultations", y = NULL) +
  theme_sante() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E2E8F0"))
save_fig(p, "01_consultations_region")

# B2. Quelles maladies dominent ? Où ? Chez qui ?
res$diag <- df %>% filter(!is.na(diagnosis)) %>% count(diagnosis, name = "cas") %>%
  mutate(part = cas / sum(cas)) %>% arrange(desc(cas))
save_tab(res$diag, "diagnostics")

p <- res$diag %>% mutate(diagnosis = fct_reorder(diagnosis, cas)) %>%
  ggplot(aes(cas, diagnosis)) +
  geom_col(fill = pal_sante$bleu, width = .7) +
  geom_text(aes(label = paste0(comma(cas, big.mark = " "), "  (", pct(part), ")")),
            hjust = -.05, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, .25))) +
  labs(title = "Répartition des diagnostics",
       subtitle = "Les 9 pathologies sont quasi équi-réparties : aucune ne domine nettement",
       x = "Nombre de cas", y = NULL) +
  theme_sante() + theme(panel.grid.major.y = element_blank())
save_fig(p, "02_diagnostics")

res$diag_region <- df %>% filter(!is.na(diagnosis)) %>% count(region, diagnosis) %>%
  group_by(region) %>% mutate(part = n / sum(n)) %>% ungroup()
save_tab(res$diag_region, "diagnostic_par_region")
p <- res$diag_region %>%
  ggplot(aes(diagnosis, region, fill = part)) +
  geom_tile(colour = "white", linewidth = .6) +
  geom_text(aes(label = pct(part)), size = 3) +
  scale_fill_gradient(low = "#E6F4F1", high = pal_sante$teal, labels = label_percent()) +
  labs(title = "Profil épidémiologique par région",
       subtitle = "Part de chaque diagnostic dans les consultations de la région",
       x = NULL, y = NULL, fill = "Part") +
  theme_sante() + theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid = element_blank())
save_fig(p, "03_diagnostic_region_heatmap", w = 10, h = 5.8)

res$diag_age <- df %>% filter(!is.na(diagnosis), !is.na(groupe_age)) %>% count(groupe_age, diagnosis) %>%
  group_by(groupe_age) %>% mutate(part = n / sum(n)) %>% ungroup()
res$diag_sexe <- df %>% filter(!is.na(diagnosis), !is.na(gender)) %>% count(gender, diagnosis) %>%
  group_by(gender) %>% mutate(part = n / sum(n)) %>% ungroup()
save_tab(res$diag_age, "diagnostic_par_age"); save_tab(res$diag_sexe, "diagnostic_par_sexe")

# B3. Combien coûtent les soins ? Qui paie le plus ?
res$cout_diag <- df %>% filter(!is.na(diagnosis), !is.na(treatment_cost)) %>%
  group_by(diagnosis) %>%
  summarise(n = n(), moyenne = mean(treatment_cost), mediane = median(treatment_cost),
            ecart_type = sd(treatment_cost), cout_total = sum(treatment_cost), .groups = "drop") %>%
  arrange(desc(moyenne))
save_tab(res$cout_diag, "cout_par_diagnostic")

p <- df %>% filter(!is.na(diagnosis), !is.na(treatment_cost)) %>%
  mutate(diagnosis = fct_reorder(diagnosis, treatment_cost, .fun = median)) %>%
  ggplot(aes(treatment_cost, diagnosis)) +
  geom_boxplot(fill = "#CCEBE6", colour = pal_sante$teal, outlier.colour = pal_sante$gris,
               outlier.alpha = .4, width = .6) +
  scale_x_continuous(labels = label_number(suffix = " $")) +
  labs(title = "Coût du traitement par diagnostic",
       subtitle = "Diabète et tuberculose (~28-30 $) coûtent 3 à 4 fois plus que diarrhée ou anémie (~8-10 $)",
       x = "Coût du traitement (USD)", y = NULL) +
  theme_sante() + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#E2E8F0"))
save_fig(p, "04_cout_par_diagnostic")

res$cout_assur <- df %>% filter(!is.na(insurance_status), !is.na(treatment_cost)) %>%
  group_by(insurance_status) %>%
  summarise(n = n(), moyenne = mean(treatment_cost), mediane = median(treatment_cost), .groups = "drop")
save_tab(res$cout_assur, "cout_par_assurance")

res$assur_region <- df %>% filter(!is.na(assure)) %>% group_by(region) %>%
  summarise(taux_assures = mean(assure), n = n(), .groups = "drop") %>% arrange(desc(taux_assures))
p <- res$assur_region %>% mutate(region = fct_reorder(region, taux_assures)) %>%
  ggplot(aes(taux_assures, region)) +
  geom_col(fill = pal_sante$bleu_clair, width = .7) +
  geom_vline(xintercept = res$kpi$taux_assures, linetype = "dashed", colour = pal_sante$rouge) +
  geom_text(aes(label = pct(taux_assures)), hjust = -.15, size = 3.6) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, .12))) +
  labs(title = "Taux de patients assurés par région",
       subtitle = paste0("Moyenne nationale : ", pct(res$kpi$taux_assures), " (ligne pointillée)"),
       x = "Part de patients assurés", y = NULL) +
  theme_sante() + theme(panel.grid.major.y = element_blank())
save_fig(p, "05_assurance_region")

# B4. Les médicaments sont-ils disponibles ?
res$rupture_region <- df %>% group_by(region) %>%
  summarise(taux_rupture = mean(rupture), n = n(), .groups = "drop") %>% arrange(desc(taux_rupture))
res$rupture_district <- df %>% group_by(region, district) %>%
  summarise(taux_rupture = mean(rupture), n = n(), .groups = "drop") %>% arrange(desc(taux_rupture))
res$rupture_diag <- df %>% filter(!is.na(diagnosis)) %>% group_by(diagnosis) %>%
  summarise(taux_rupture = mean(rupture), n = n(), .groups = "drop") %>% arrange(desc(taux_rupture))
res$rupture_etab <- df %>% group_by(facility_name, district) %>%
  summarise(taux_rupture = mean(rupture), n = n(), .groups = "drop") %>%
  filter(n >= 50) %>% arrange(desc(taux_rupture))
res$rupture_type_etab <- df %>% group_by(type_etablissement) %>%
  summarise(taux_rupture = mean(rupture), n = n(), .groups = "drop") %>% arrange(desc(taux_rupture))
save_tab(res$rupture_region, "rupture_par_region"); save_tab(res$rupture_district, "rupture_par_district")
save_tab(res$rupture_etab, "rupture_par_etablissement")

p <- res$rupture_region %>% mutate(region = fct_reorder(region, taux_rupture)) %>%
  ggplot(aes(taux_rupture, region)) +
  geom_col(aes(fill = taux_rupture > res$kpi$taux_rupture), width = .7, show.legend = FALSE) +
  geom_vline(xintercept = res$kpi$taux_rupture, linetype = "dashed", colour = pal_sante$encre) +
  geom_text(aes(label = pct(taux_rupture)), hjust = -.15, size = 3.6) +
  scale_fill_manual(values = c(`TRUE` = pal_sante$rouge, `FALSE` = "#94A3B8")) +
  scale_x_continuous(labels = label_percent(), expand = expansion(mult = c(0, .15))) +
  labs(title = "Ruptures de médicaments par région",
       subtitle = paste0("Rouge = au-dessus de la moyenne nationale (", pct(res$kpi$taux_rupture), ")"),
       x = "Part des consultations avec rupture de stock", y = NULL) +
  theme_sante() + theme(panel.grid.major.y = element_blank())
save_fig(p, "06_rupture_region")

# B5. Dynamique temporelle
res$mensuel <- df %>% group_by(mois) %>%
  summarise(consultations = n(), cout_moyen = mean(treatment_cost, na.rm = TRUE),
            taux_rupture = mean(rupture), .groups = "drop")
save_tab(res$mensuel, "evolution_mensuelle")
p <- res$mensuel %>% ggplot(aes(mois, consultations)) +
  geom_line(colour = pal_sante$teal, linewidth = 1.1) +
  geom_point(colour = pal_sante$teal, size = 2.6) +
  geom_hline(yintercept = mean(res$mensuel$consultations), linetype = "dashed", colour = pal_sante$gris) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, NA), labels = label_comma(big.mark = " ")) +
  labs(title = "Évolution mensuelle des consultations",
       subtitle = "Activité stable sur l'année (ligne pointillée = moyenne mensuelle)",
       x = NULL, y = "Consultations") + theme_sante()
save_fig(p, "07_evolution_mensuelle")

res$diag_mois <- df %>% filter(!is.na(diagnosis)) %>% count(mois, diagnosis)
res$type_cons <- df %>% count(consultation_type, name = "n") %>% mutate(part = n / sum(n)) %>% arrange(desc(n))
res$jour <- df %>% count(jour_semaine, name = "n")
save_tab(res$type_cons, "types_consultation")

# ---------------------------------------------------------------------
# C. STATISTIQUES DESCRIPTIVES
# ---------------------------------------------------------------------
desc_num <- function(x) {
  x <- x[!is.na(x)]
  tibble(n = length(x), moyenne = mean(x), mediane = median(x), ecart_type = sd(x),
         variance = var(x), min = min(x), q1 = quantile(x, .25), q3 = quantile(x, .75),
         max = max(x), iqr = IQR(x), cv = sd(x) / mean(x),
         asymetrie = mean((x - mean(x))^3) / sd(x)^3,
         aplatissement = mean((x - mean(x))^4) / sd(x)^4 - 3)
}
res$desc <- bind_rows(Âge = desc_num(df$patient_age),
                      `Coût (USD)` = desc_num(df$treatment_cost), .id = "variable")
print(res$desc, width = Inf)
save_tab(res$desc, "statistiques_descriptives")

res$desc_cout_diag <- df %>% filter(!is.na(diagnosis)) %>% group_by(diagnosis) %>%
  reframe(desc_num(treatment_cost))
save_tab(res$desc_cout_diag, "stats_cout_par_diagnostic")

p1 <- ggplot(df, aes(patient_age)) +
  geom_histogram(binwidth = 5, fill = pal_sante$teal, colour = "white", boundary = 0) +
  geom_vline(xintercept = res$desc$moyenne[1], colour = pal_sante$rouge, linetype = "dashed") +
  labs(title = "Distribution de l'âge des patients",
       subtitle = paste0("Moyenne = ", round(res$desc$moyenne[1], 1), " ans ; médiane = ", res$desc$mediane[1], " ans"),
       x = "Âge (années)", y = "Patients") + theme_sante()
save_fig(p1, "08_distribution_age")

p2 <- ggplot(df, aes(treatment_cost)) +
  geom_histogram(binwidth = 2.5, fill = pal_sante$bleu, colour = "white", boundary = 0) +
  geom_vline(xintercept = res$desc$mediane[2], colour = pal_sante$rouge, linetype = "dashed") +
  labs(title = "Distribution du coût de traitement",
       subtitle = paste0("Distribution asymétrique à droite (asymétrie = ", round(res$desc$asymetrie[2], 2),
                         ") ; médiane = ", round(res$desc$mediane[2], 1), " USD"),
       x = "Coût (USD)", y = "Consultations") + theme_sante()
save_fig(p2, "09_distribution_cout")

res$corr_age_cout <- cor.test(df$patient_age, df$treatment_cost, method = "spearman", exact = FALSE)

# ---------------------------------------------------------------------
# D. TESTS D'HYPOTHÈSES (les différences observées sont-elles réelles ?)
# ---------------------------------------------------------------------
res$tests <- list(
  rupture_region   = chisq.test(table(df$region, df$rupture)),
  rupture_district = chisq.test(table(df$district, df$rupture)),
  rupture_diag     = chisq.test(table(df$diagnosis, df$rupture)),
  assurance_rupture = chisq.test(table(df$insurance_status, df$rupture)),
  diag_region      = chisq.test(table(df$diagnosis, df$region)),
  diag_age         = chisq.test(table(df$diagnosis, df$groupe_age)),
  diag_sexe        = chisq.test(table(df$diagnosis, df$gender)),
  cout_diag        = kruskal.test(treatment_cost ~ diagnosis, data = df),
  cout_assurance   = wilcox.test(treatment_cost ~ insurance_status, data = df),
  cout_type_cons   = kruskal.test(treatment_cost ~ consultation_type, data = df),
  cout_region      = kruskal.test(treatment_cost ~ region, data = df),
  age_cout         = res$corr_age_cout
)
res$tests_resume <- imap_dfr(res$tests, ~ tibble(
  test = .y, methode = .x$method, statistique = unname(.x$statistic), p_value = .x$p.value)) %>%
  mutate(significatif_5pct = p_value < 0.05)
print(res$tests_resume, width = Inf)
save_tab(res$tests_resume, "tests_hypotheses")

# Tendance annuelle : activité mensuelle uniforme ?
res$test_uniform_mois <- chisq.test(table(df$mois))

# Incohérences logiques résiduelles (signalées, non corrigées)
res$incoherences <- tibble(
  controle = c("Consultations prénatales de patients masculins",
               "Consultations prénatales hors tranche 12-50 ans"),
  lignes = c(sum(df$consultation_type == "Prénatale" & df$gender == "Masculin", na.rm = TRUE),
             sum(df$consultation_type == "Prénatale" & (df$patient_age < 12 | df$patient_age > 50), na.rm = TRUE))
)
print(res$incoherences)

# ---------------------------------------------------------------------
# E. SYNTHÈSE DES CONSTATS (utilisée dans le rapport et les recommandations)
# ---------------------------------------------------------------------
cat("\n===== CONSTATS CLÉS =====\n")
cat("Région la plus active   :", as.character(res$par_region$region[1]), "(", res$par_region$consultations[1], ")\n")
cat("Région la moins active  :", as.character(tail(res$par_region$region, 1)), "\n")
cat("Pathologie n°1          :", as.character(res$diag$diagnosis[1]), "\n")
cat("Dx le plus coûteux      :", as.character(res$cout_diag$diagnosis[1]), round(res$cout_diag$moyenne[1], 1), "USD\n")
cat("Taux de rupture national:", pct(res$kpi$taux_rupture), "\n")
cat("Région la + en rupture  :", as.character(res$rupture_region$region[1]), pct(res$rupture_region$taux_rupture[1]), "\n")
cat("District + en rupture   :", as.character(res$rupture_district$district[1]), pct(res$rupture_district$taux_rupture[1]), "\n")
cat("Assurés                 :", pct(res$kpi$taux_assures), "\n")

saveRDS(res, file.path(racine, "outputs", "resultats.rds"))
cat("\nAnalyse terminée. Figures:", length(list.files(file.path(racine, "outputs", "figures"))), "\n")
