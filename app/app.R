# =====================================================================
# OBSERVATOIRE DES CONSULTATIONS - CAMEROUN 2025
# Dashboard Shiny (bslib + plotly + leaflet + DT) - charte graphique santé
# Lancer :  shiny::runApp("app")
# =====================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(plotly)
  library(leaflet)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(stringr)
  library(readr)
})

# ---------------------------------------------------------------------
# DONNÉES & CONSTANTES
# ---------------------------------------------------------------------
df <- readRDS("data/consultations_clean.rds") |>
  mutate(across(c(diagnosis, gender, insurance_status), ~ fct_na_value_to_level(.x, "Non renseigné")))

journal <- if (file.exists("data/journal_qualite.csv")) read_csv("data/journal_qualite.csv", show_col_types = FALSE) else NULL

NR <- "Non renseigné"
pal <- list(teal = "#0F766E", teal2 = "#14B8A6", teal_l = "#CCEBE6", bleu = "#0369A1", bleu_l = "#7DD3FC",
            ambre = "#F59E0B", rouge = "#DC2626", vert = "#16A34A", encre = "#0F2A37", gris = "#64748B",
            grid = "#E8EEF1")
diag_levels <- sort(setdiff(levels(df$diagnosis), NR))
col_diag <- setNames(c("#0F766E", "#0369A1", "#F59E0B", "#7C3AED", "#DB2777",
                       "#16A34A", "#EA580C", "#0891B2", "#65A30D"), diag_levels)
mois_fr <- c("janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc.")

coords <- tribble(
  ~district, ~lat, ~lng,
  "Douala I", 4.050, 9.700, "Douala II", 4.075, 9.760, "Yaoundé I", 3.870, 11.520, "Yaoundé II", 3.900, 11.480,
  "Bafoussam", 5.478, 10.418, "Bamenda", 5.960, 10.160, "Batouri", 4.434, 14.358, "Bertoua", 4.577, 13.684,
  "Buea", 4.153, 9.243, "Dschang", 5.447, 10.053, "Ebolowa", 2.900, 11.150, "Garoua", 9.301, 13.397,
  "Guider", 9.934, 13.947, "Kousseri", 12.077, 15.031, "Kribi", 2.940, 9.910, "Kumba", 4.636, 9.446,
  "Kumbo", 6.200, 10.680, "Limbe", 4.024, 9.215, "Maroua", 10.591, 14.316, "Mbalmayo", 3.520, 11.500,
  "Mbouda", 5.627, 10.254, "Meiganga", 6.520, 14.290, "Mokolo", 10.740, 13.800, "Ngaoundéré", 7.322, 13.584,
  "Nkongsamba", 4.955, 9.940, "Pitoa", 9.380, 13.520, "Sangmélima", 2.940, 11.980, "Tibati", 6.470, 12.630,
  "Wum", 6.384, 10.067, "Yokadouma", 3.520, 15.050
)

nat <- list(  # repères nationaux (jeu complet) pour comparer les sélections
  rupture = mean(df$rupture), assures = mean(df$assure, na.rm = TRUE),
  cout = mean(df$treatment_cost, na.rm = TRUE), n = nrow(df)
)

fmt_n   <- function(x, d = 0) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ","))
fmt_pct <- function(x, d = 1) ifelse(is.na(x), "–", paste0(formatC(100 * x, format = "f", digits = d, decimal.mark = ","), " %"))
fmt_p   <- function(p) ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 3, decimal.mark = ","))

# Habillage commun des graphiques plotly
sty <- function(p, xt = NULL, yt = NULL, legend = FALSE, xfmt = NULL, yfmt = NULL, legy = -0.2, ...) {
  p |>
    layout(
      font = list(family = "Inter, sans-serif", color = pal$encre, size = 12),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = list(text = xt, font = list(size = 11, color = pal$gris)), gridcolor = pal$grid,
                   zeroline = FALSE, tickformat = xfmt, linecolor = pal$grid),
      yaxis = list(title = list(text = yt, font = list(size = 11, color = pal$gris)), gridcolor = pal$grid,
                   zeroline = FALSE, tickformat = yfmt),
      showlegend = legend,
      legend = list(orientation = "h", y = legy, yanchor = if (legy > 0) "bottom" else "top", x = 0, font = list(size = 11)),
      hoverlabel = list(bgcolor = "white", bordercolor = pal$grid, font = list(color = pal$encre, family = "Inter")),
      margin = list(l = 8, r = 8, t = 8, b = 8), ...
    ) |>
    config(displayModeBar = FALSE, responsive = TRUE)
}

kpi_row <- function(...) div(class = "kpi-grid", ...)

kpi_card <- function(label, value, unit = NULL, sub = NULL, icon, theme = "teal") {
  div(class = paste("kpi", paste0("kpi-", theme)),
      div(class = "kpi-ico", bs_icon(icon)),
      div(div(class = "kpi-label", label),
          div(class = "kpi-value", value, if (!is.null(unit)) tags$small(unit)),
          if (!is.null(sub)) div(class = "kpi-sub", sub)))
}

# Écart par rapport à une référence : flèche + couleur (hausse = rouge si c'est une mauvaise chose)
delta_tag <- function(x, ref, bad_up = TRUE, ref_lbl = "national") {
  if (is.na(x) || is.na(ref)) return(NULL)
  d <- (x - ref) * 100
  if (abs(d) < 0.05) return(span(class = "flat", paste0("= ", ref_lbl)))
  up <- d > 0
  cl <- if (up == bad_up) "up" else "down"
  span(span(class = cl, paste0(if (up) "▲ " else "▼ ", formatC(abs(d), format = "f", digits = 1, decimal.mark = ","), " pt")),
       paste0(" vs ", ref_lbl, " (", fmt_pct(ref), ")"))
}

page_title <- function(titre, sous, id) div(class = "page-head", div(h2(titre), p(sous)), span(class = "pill", textOutput(paste0("pill_", id), inline = TRUE)))
card_h <- function(titre, sous = NULL) card_header(titre, if (!is.null(sous)) span(class = "sub", sous))

logo_svg <- HTML('<svg width="26" height="26" viewBox="0 0 24 24" fill="none"><rect x="9" y="2" width="6" height="20" rx="2" fill="#0F766E"/><rect x="2" y="9" width="20" height="6" rx="2" fill="#0F766E"/><path d="M3 12h4l2-4 3 8 2-5 1.5 3H21" stroke="#fff" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/></svg>')

# ---------------------------------------------------------------------
# INTERFACE
# ---------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = "#F4F8F9", fg = "#0F2A37",
  primary = "#0F766E", secondary = "#64748B", success = "#16A34A",
  info = "#0369A1", warning = "#F59E0B", danger = "#DC2626",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Poppins", local = FALSE),
  font_scale = 0.97
)

ui <- page_navbar(
  title = div(class = "brand",
              div(class = "brand-logo", logo_svg),
              div(class = "brand-text", div(class = "brand-title", "Observatoire des Consultations"),
                  div(class = "brand-sub", "Santé publique · Cameroun 2025"))),
  window_title = "Observatoire des Consultations – Cameroun",
  id = "nav", theme = theme, fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),

  # ----- Filtres globaux -----
  sidebar = sidebar(
    width = 300, open = "desktop",
    div(class = "filter-head", span(class = "filter-title", bs_icon("funnel"), "Filtres")),
    dateRangeInput("dates", "Période", start = min(df$consultation_date), end = max(df$consultation_date),
                   min = min(df$consultation_date), max = max(df$consultation_date),
                   format = "dd/mm/yyyy", language = "fr", separator = " au ", weekstart = 1),
    selectizeInput("f_region", "Région", choices = levels(df$region), multiple = TRUE,
                   options = list(placeholder = "Toutes", plugins = list("remove_button"))),
    selectizeInput("f_diag", "Diagnostic", choices = levels(df$diagnosis), multiple = TRUE,
                   options = list(placeholder = "Tous", plugins = list("remove_button"))),
    selectizeInput("f_type", "Type de consultation", choices = levels(df$consultation_type), multiple = TRUE,
                   options = list(placeholder = "Tous", plugins = list("remove_button"))),
    selectizeInput("f_sexe", "Sexe", choices = levels(df$gender), multiple = TRUE,
                   options = list(placeholder = "Tous", plugins = list("remove_button"))),
    selectizeInput("f_assur", "Couverture assurance", choices = levels(df$insurance_status), multiple = TRUE,
                   options = list(placeholder = "Toutes", plugins = list("remove_button"))),
    uiOutput("sel_count"),
    actionButton("reset", tagList(bs_icon("arrow-counterclockwise"), " Réinitialiser les filtres"), class = "btn-reset w-100 mt-2"),
    div(class = "side-foot",
        "Source : registre de consultations simulé (10 centres régionaux, 30 districts, 150 établissements). ",
        "Données nettoyées : voir l'onglet « Qualité des données ».")
  ),

  # ===== 1. VUE D'ENSEMBLE =====
  nav_panel(
    title = tagList(bs_icon("speedometer2"), "Vue d'ensemble"),
    page_title("Vue d'ensemble", "Activité, coûts et accès aux médicaments sur la sélection courante", 1),
    uiOutput("kpis"),
    layout_columns(
      col_widths = c(8, 4), class = "mt-3",
      card(card_h("Évolution mensuelle des consultations", "Volume de consultations par mois"),
           plotlyOutput("p_mensuel", height = "300px"), full_screen = TRUE),
      card(card_h("Constats clés", "Calculés sur la sélection"), uiOutput("insights"))
    ),
    layout_columns(
      col_widths = c(5, 4, 3), class = "mt-3",
      card(card_h("Diagnostics les plus fréquents", "Cas enregistrés (diagnostic renseigné)"),
           plotlyOutput("p_top_diag", height = "320px"), full_screen = TRUE),
      card(card_h("Profil des patients", "Tranche d'âge et sexe"),
           plotlyOutput("p_age_sexe", height = "320px"), full_screen = TRUE),
      card(card_h("Types de consultation"), plotlyOutput("p_type", height = "320px"), full_screen = TRUE)
    )
  ),

  # ===== 2. ÉPIDÉMIOLOGIE =====
  nav_panel(
    title = tagList(bs_icon("virus2"), "Épidémiologie"),
    page_title("Profil épidémiologique", "Quelles pathologies, où, chez qui et à quel moment ?", 2),
    card(card_h("Carte thermique : diagnostic × région", "Part de chaque diagnostic dans les consultations de la région"),
         plotlyOutput("p_heat", height = "420px"), full_screen = TRUE),
    layout_columns(
      col_widths = c(6, 6), class = "mt-3",
      card(card_h("Diagnostics par tranche d'âge", "Composition (100 %) des diagnostics dans chaque tranche"),
           plotlyOutput("p_diag_age", height = "360px"), full_screen = TRUE),
      card(card_h("Diagnostics selon le sexe", "Part de chaque diagnostic parmi les patients de chaque sexe"),
           plotlyOutput("p_diag_sexe", height = "360px"), full_screen = TRUE)
    ),
    card(class = "mt-3",
         card_header(span("Tendance mensuelle par diagnostic"),
                     span(class = "sub", "Sélectionnez une ou plusieurs pathologies à comparer")),
         selectizeInput("diag_focus", NULL, choices = diag_levels, multiple = TRUE,
                        selected = c("Paludisme", "Tuberculose", "Diabète"), width = "100%",
                        options = list(plugins = list("remove_button"))),
         plotlyOutput("p_diag_mois", height = "320px"), full_screen = TRUE)
  ),

  # ===== 3. GÉOGRAPHIE =====
  nav_panel(
    title = tagList(bs_icon("geo-alt"), "Géographie"),
    page_title("Analyse territoriale", "Comparer les 10 régions et les 30 districts", 3),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(span("Carte des districts"), span(class = "sub", "Taille = nombre de consultations ; couleur = indicateur choisi")),
        radioButtons("geo_metric", NULL, inline = TRUE,
                     choices = c("Consultations" = "n", "Taux de rupture" = "rupture",
                                 "Coût moyen" = "cout", "Taux d'assurés" = "assures")),
        leafletOutput("map", height = "480px"), full_screen = TRUE
      ),
      card(card_h("Classement des régions", "Selon l'indicateur choisi"),
           plotlyOutput("p_geo_bar", height = "520px"), full_screen = TRUE)
    ),
    card(class = "mt-3", card_h("Tableau de bord par région"), DTOutput("t_region", fill = FALSE))
  ),

  # ===== 4. MÉDICAMENTS =====
  nav_panel(
    title = tagList(bs_icon("capsule"), "Médicaments"),
    page_title("Disponibilité des médicaments", "Où et quand les ruptures de stock pénalisent-elles les soins ?", 4),
    uiOutput("kpis_rupture"),
    layout_columns(
      col_widths = c(6, 6), class = "mt-3",
      card(card_h("Taux de rupture par région", "Rouge = au-dessus de la moyenne de la sélection"),
           plotlyOutput("p_rup_region", height = "360px"), full_screen = TRUE),
      card(card_h("Districts les plus touchés", "12 districts avec le plus fort taux de rupture"),
           plotlyOutput("p_rup_district", height = "360px"), full_screen = TRUE)
    ),
    layout_columns(
      col_widths = c(7, 5), class = "mt-3",
      card(card_h("Taux de rupture mensuel", "Évolution et moyenne de la sélection"),
           plotlyOutput("p_rup_mois", height = "300px"), full_screen = TRUE),
      card(card_h("Rupture selon le diagnostic"), plotlyOutput("p_rup_diag", height = "300px"), full_screen = TRUE)
    ),
    card(class = "mt-3", card_h("Établissements à surveiller", "Établissements avec au moins 30 consultations, triés par taux de rupture"),
         DTOutput("t_rup_etab", fill = FALSE))
  ),

  # ===== 5. COÛTS & ASSURANCE =====
  nav_panel(
    title = tagList(bs_icon("cash-coin"), "Coûts & assurance"),
    page_title("Coûts des soins et couverture assurance", "Combien coûtent les traitements et qui est couvert ?", 5),
    uiOutput("kpis_cout"),
    layout_columns(
      col_widths = c(7, 5), class = "mt-3",
      card(card_h("Coût du traitement par diagnostic", "Médiane, quartiles et valeurs extrêmes (USD)"),
           plotlyOutput("p_cout_diag", height = "380px"), full_screen = TRUE),
      card(card_h("Distribution des coûts", "Histogramme (USD)"),
           plotlyOutput("p_cout_hist", height = "380px"), full_screen = TRUE)
    ),
    layout_columns(
      col_widths = c(6, 6), class = "mt-3",
      card(card_h("Couverture assurance par région", "Part de patients assurés (statut renseigné)"),
           plotlyOutput("p_assur_region", height = "340px"), full_screen = TRUE),
      card(card_h("Coût moyen selon le type de consultation et l'assurance"),
           plotlyOutput("p_cout_type", height = "340px"), full_screen = TRUE)
    ),
    card(class = "mt-3", card_h("Lecture statistique", "Tests d'hypothèses recalculés sur la sélection (seuil 5 %)"),
         uiOutput("stats_tests"))
  ),

  # ===== 6. QUALITÉ =====
  nav_panel(
    title = tagList(bs_icon("shield-check"), "Qualité des données"),
    page_title("Qualité des données", "Traçabilité complète : de la donnée brute à la donnée fiable", 6),
    uiOutput("kpis_qualite"),
    card(class = "mt-3", fill = FALSE, card_h("Journal de nettoyage", "Problèmes détectés et traitements appliqués"), DTOutput("t_journal", fill = FALSE)),
    layout_columns(
      col_widths = c(5, 7), class = "mt-3",
      card(card_h("Complétude après nettoyage", "% de valeurs renseignées"),
           plotlyOutput("p_complet", height = "340px")),
      card(card_h("Points de vigilance", "Limites connues des données"),
           div(class = "note note-warn mb-2", bs_icon("exclamation-triangle"), HTML(" <b>Incohérences logiques non corrigées.</b> "),
               textOutput("incoh", inline = TRUE)),
           div(class = "note mb-2", bs_icon("currency-exchange"), HTML(" <b>Devise.</b> Le coût de traitement n'a pas d'unité dans la source : il est interprété en dollars US (USD).")),
           div(class = "note mb-2", bs_icon("question-circle"), HTML(" <b>Valeurs manquantes.</b> Diagnostic, sexe, assurance et coût manquent chacun dans ~5 % des lignes : elles sont exclues des indicateurs concernés, jamais imputées (sauf la région, déduite du district).")),
           div(class = "note", bs_icon("rulers"), HTML(" <b>Seuils d'aberration.</b> Âge hors de 1-120 ans ou égal à 0 (pic artificiel) ; coût ≤ 0 ou > 1 000 USD. Ces valeurs sont mises à NA.")))
    )
  ),

  # ===== 7. DONNÉES =====
  nav_panel(
    title = tagList(bs_icon("table"), "Données"),
    page_title("Données détaillées", "Consultations correspondant aux filtres, exportables en CSV", 7),
    card(card_header(span("Registre des consultations"),
                     span(class = "sub", "Triez, recherchez et exportez la sélection courante")),
         downloadButton("dl", "Télécharger (CSV)", class = "btn-dl mb-3", style = "width:fit-content"),
         DTOutput("t_data", fill = FALSE))
  )
)

# ---------------------------------------------------------------------
# SERVEUR
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # ----- Filtrage global -----
  d <- reactive({
    x <- df |> filter(consultation_date >= input$dates[1], consultation_date <= input$dates[2])
    if (length(input$f_region)) x <- x |> filter(region %in% input$f_region)
    if (length(input$f_diag))   x <- x |> filter(diagnosis %in% input$f_diag)
    if (length(input$f_type))   x <- x |> filter(consultation_type %in% input$f_type)
    if (length(input$f_sexe))   x <- x |> filter(gender %in% input$f_sexe)
    if (length(input$f_assur))  x <- x |> filter(insurance_status %in% input$f_assur)
    x
  })
  need_data <- function(x, n = 1) validate(need(nrow(x) >= n, "Aucune donnée pour cette sélection. Élargissez les filtres."))
  d_diag <- reactive(d() |> filter(diagnosis != NR) |> mutate(diagnosis = droplevels(diagnosis)))

  observeEvent(input$reset, {
    updateDateRangeInput(session, "dates", start = min(df$consultation_date), end = max(df$consultation_date))
    for (id in c("f_region", "f_diag", "f_type", "f_sexe", "f_assur")) updateSelectizeInput(session, id, selected = character(0))
  })

  for (i in 1:7) local({
    id <- paste0("pill_", i)
    output[[id]] <- renderText(paste0(format(input$dates[1], "%d/%m/%Y"), " → ", format(input$dates[2], "%d/%m/%Y")))
  })
  output$sel_count <- renderUI({
    n <- nrow(d())
    div(class = "sel-count", "Sélection courante", tags$b(fmt_n(n)),
        paste0("consultations sur ", fmt_n(nat$n), " (", fmt_pct(n / nat$n), ")"))
  })

  # ======================= VUE D'ENSEMBLE =======================
  output$kpis <- renderUI({
    x <- d(); need_data(x)
    kpi_row(
      kpi_card("Consultations", fmt_n(nrow(x)), sub = paste0(fmt_n(n_distinct(x$facility_name)), " établissements"), icon = "clipboard2-pulse"),
      kpi_card("Âge médian", fmt_n(median(x$patient_age, na.rm = TRUE)), "ans", sub = paste0(fmt_pct(mean(x$gender == "Féminin", na.rm = TRUE), 0), " de femmes"), icon = "people", theme = "blue"),
      kpi_card("Coût moyen", fmt_n(mean(x$treatment_cost, na.rm = TRUE), 1), "USD", sub = paste0("médiane ", fmt_n(median(x$treatment_cost, na.rm = TRUE), 1), " USD"), icon = "wallet2", theme = "violet"),
      kpi_card("Coûts cumulés", fmt_n(sum(x$treatment_cost, na.rm = TRUE) / 1000, 1), "k USD", sub = "coûts renseignés uniquement", icon = "graph-up-arrow", theme = "green"),
      kpi_card("Ruptures", fmt_pct(mean(x$rupture)), sub = delta_tag(mean(x$rupture), nat$rupture, bad_up = TRUE), icon = "exclamation-octagon", theme = "red"),
      kpi_card("Assurés", fmt_pct(mean(x$assure, na.rm = TRUE)), sub = delta_tag(mean(x$assure, na.rm = TRUE), nat$assures, bad_up = FALSE), icon = "shield-plus", theme = "amber")
    )
  })

  output$p_mensuel <- renderPlotly({
    x <- d(); need_data(x)
    m <- x |> mutate(m = as.integer(format(consultation_date, "%m"))) |> count(m) |> arrange(m) |>
      mutate(lab = factor(mois_fr[m], levels = mois_fr[m]), avg = mean(n))
    plot_ly(m, x = ~lab, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = pal$teal, width = 3, shape = "spline"), marker = list(color = "white", size = 9, line = list(color = pal$teal, width = 2.5)),
            fill = "tozeroy", fillcolor = "rgba(20,184,166,0.13)",
            hovertemplate = "<b>%{x}</b><br>%{y:,} consultations<extra></extra>") |>
      add_lines(y = ~avg, line = list(color = pal$gris, dash = "dot", width = 1.2), hoverinfo = "skip", inherit = FALSE, x = ~lab, data = m) |>
      sty(yt = "Consultations", yfmt = ",d") |> layout(yaxis = list(rangemode = "tozero"))
  })

  output$p_top_diag <- renderPlotly({
    x <- d_diag(); need_data(x)
    t <- x |> count(diagnosis) |> arrange(n) |> mutate(diagnosis = factor(diagnosis, levels = diagnosis), part = n / sum(n))
    plot_ly(t, x = ~n, y = ~diagnosis, type = "bar", orientation = "h",
            marker = list(color = pal$teal, opacity = .92),
            text = ~paste0(fmt_n(n), " · ", fmt_pct(part, 0)), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "<b>%{y}</b><br>%{x:,} cas<extra></extra>") |>
      sty(xt = NULL) |> layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(t$n) * 1.3)),
                               yaxis = list(gridcolor = "rgba(0,0,0,0)"), margin = list(l = 8, r = 30, t = 8, b = 8))
  })

  output$p_age_sexe <- renderPlotly({
    x <- d() |> filter(!is.na(groupe_age), gender != NR); need_data(x)
    t <- x |> count(groupe_age, gender)
    plot_ly(t, x = ~groupe_age, y = ~n, color = ~gender, colors = c("Féminin" = "#DB2777", "Masculin" = pal$bleu), type = "bar",
            hovertemplate = "<b>%{x}</b> · %{fullData.name}<br>%{y:,} patients<extra></extra>") |>
      sty(yt = "Patients", legend = TRUE, yfmt = ",d", legy = 1.12) |> layout(barmode = "group", bargap = .25, xaxis = list(tickangle = -30), margin = list(t = 34))
  })

  output$p_type <- renderPlotly({
    x <- d(); need_data(x)
    t <- x |> count(consultation_type)
    plot_ly(t, labels = ~consultation_type, values = ~n, type = "pie", hole = .62, sort = TRUE,
            marker = list(colors = c(pal$teal, pal$bleu, pal$ambre, "#7C3AED", "#DB2777"), line = list(color = "white", width = 2)),
            textinfo = "percent", textfont = list(color = "white", size = 11), insidetextorientation = "horizontal",
            hovertemplate = "<b>%{label}</b><br>%{value:,} (%{percent})<extra></extra>") |>
      sty(legend = TRUE) |> layout(legend = list(orientation = "h", y = -0.05, font = list(size = 10)),
                                   annotations = list(list(text = paste0("<b>", fmt_n(sum(t$n)), "</b><br><span style='font-size:10px;color:#64748B'>consultations</span>"),
                                                           showarrow = FALSE, font = list(size = 15))))
  })

  output$insights <- renderUI({
    x <- d(); need_data(x, 30)
    xd <- d_diag()
    ins <- list()
    top_d <- x |> count(district, sort = TRUE) |> slice(1)
    ins[[1]] <- div(class = "insight", div(class = "dot", bs_icon("geo-alt")),
                    div(HTML(sprintf("<b>%s</b> est le district le plus actif avec %s consultations (%s).",
                                     top_d$district, fmt_n(top_d$n), fmt_pct(top_d$n / nrow(x))))))
    if (nrow(xd) > 0) {
      t <- xd |> count(diagnosis, sort = TRUE)
      ins[[2]] <- div(class = "insight info", div(class = "dot", bs_icon("virus2")),
                      div(HTML(sprintf("<b>%s</b> est le diagnostic n°1 (%s cas, %s). Écart avec le dernier : %s.",
                                       t$diagnosis[1], fmt_n(t$n[1]), fmt_pct(t$n[1] / sum(t$n), 0),
                                       fmt_pct((t$n[1] - t$n[nrow(t)]) / t$n[nrow(t)], 0)))))
      cd <- xd |> filter(!is.na(treatment_cost)) |> group_by(diagnosis) |> summarise(m = mean(treatment_cost), .groups = "drop") |> arrange(desc(m))
      if (nrow(cd) >= 2)
        ins[[3]] <- div(class = "insight warn", div(class = "dot", bs_icon("wallet2")),
                        div(HTML(sprintf("<b>%s</b> est le soin le plus coûteux (%s USD en moyenne), soit %s× %s (%s USD).",
                                         cd$diagnosis[1], fmt_n(cd$m[1], 1), fmt_n(cd$m[1] / cd$m[nrow(cd)], 1), cd$diagnosis[nrow(cd)], fmt_n(cd$m[nrow(cd)], 1)))))
    }
    rd <- x |> group_by(district) |> summarise(n = n(), r = mean(rupture), .groups = "drop") |> filter(n >= 30) |> arrange(desc(r))
    if (nrow(rd) > 0)
      ins[[4]] <- div(class = "insight alert", div(class = "dot", bs_icon("exclamation-triangle")),
                      div(HTML(sprintf("<b>%s</b> subit le plus de ruptures : %s des consultations (moyenne de la sélection : %s).",
                                       rd$district[1], fmt_pct(rd$r[1]), fmt_pct(mean(x$rupture))))))
    comp <- mean(x$insurance_status != NR)
    ins[[5]] <- div(class = "insight", div(class = "dot", bs_icon("shield-check")),
                    div(HTML(sprintf("Statut d'assurance renseigné pour <b>%s</b> des consultations ; %s sont assurés.", fmt_pct(comp, 0), fmt_pct(mean(x$assure, na.rm = TRUE), 0)))))
    tagList(ins)
  })

  # ======================= ÉPIDÉMIOLOGIE =======================
  output$p_heat <- renderPlotly({
    x <- d_diag(); need_data(x, 20)
    t <- x |> count(region, diagnosis) |> complete(region, diagnosis, fill = list(n = 0)) |>
      group_by(region) |> mutate(part = n / sum(n)) |> ungroup()
    regs <- sort(unique(as.character(t$region)), decreasing = TRUE); dgs <- sort(unique(as.character(t$diagnosis)))
    z <- sapply(dgs, \(g) sapply(regs, \(r) t$part[t$region == r & t$diagnosis == g]))
    n <- sapply(dgs, \(g) sapply(regs, \(r) t$n[t$region == r & t$diagnosis == g]))
    if (is.null(dim(z))) { z <- matrix(z, nrow = length(regs)); n <- matrix(n, nrow = length(regs)) }
    ann <- expand.grid(i = seq_along(regs), j = seq_along(dgs)) |>
      mutate(x = dgs[j], y = regs[i], text = fmt_pct(z[cbind(i, j)], 1), col = ifelse(z[cbind(i, j)] > max(z, na.rm = TRUE) * .72, "white", pal$encre))
    plot_ly(x = dgs, y = regs, z = z, type = "heatmap", xgap = 3, ygap = 3,
            colorscale = list(c(0, "#EEF8F6"), c(.5, "#5EC9BC"), c(1, "#0A4F49")), zmin = min(z), zmax = max(z),
            customdata = n, colorbar = list(title = list(text = "Part", font = list(size = 11)), tickformat = ".0%", thickness = 12, len = .8),
            hovertemplate = "<b>%{y}</b> · %{x}<br>%{z:.1%} des consultations (%{customdata:,} cas)<extra></extra>") |>
      layout(annotations = lapply(seq_len(nrow(ann)), \(k) list(x = ann$x[k], y = ann$y[k], text = ann$text[k], showarrow = FALSE,
                                                                font = list(size = 10.5, color = ann$col[k])))) |>
      sty() |> layout(xaxis = list(side = "top", tickangle = -25, gridcolor = "rgba(0,0,0,0)"), yaxis = list(gridcolor = "rgba(0,0,0,0)"))
  })

  stacked_share <- function(x, grp, lbl) {
    t <- x |> mutate(g = .data[[grp]]) |> count(g, diagnosis) |> group_by(g) |> mutate(part = n / sum(n)) |> ungroup()
    plot_ly(t, x = ~g, y = ~part, color = ~diagnosis, colors = col_diag, type = "bar", customdata = ~n,
            hovertemplate = "<b>%{fullData.name}</b><br>%{y:.1%} (%{customdata:,} cas)<extra></extra>") |>
      sty(yt = "Part des diagnostics", legend = TRUE, yfmt = ".0%") |> layout(barmode = "stack", legend = list(font = list(size = 10)))
  }
  output$p_diag_age <- renderPlotly({
    x <- d_diag() |> filter(!is.na(groupe_age)); need_data(x, 20); stacked_share(x, "groupe_age", "Tranche d'âge")
  })
  output$p_diag_sexe <- renderPlotly({
    x <- d_diag() |> filter(gender != NR); need_data(x, 20)
    t <- x |> count(gender, diagnosis) |> group_by(gender) |> mutate(part = n / sum(n)) |> ungroup()
    plot_ly(t, x = ~diagnosis, y = ~part, color = ~gender, colors = c("Féminin" = "#DB2777", "Masculin" = pal$bleu), type = "bar", customdata = ~n,
            hovertemplate = "<b>%{x}</b> · %{fullData.name}<br>%{y:.1%} (%{customdata:,} cas)<extra></extra>") |>
      sty(yt = "Part parmi les patients du sexe", legend = TRUE, yfmt = ".0%", legy = 1.12) |> layout(barmode = "group", xaxis = list(tickangle = -35), bargap = .25, margin = list(t = 34))
  })
  output$p_diag_mois <- renderPlotly({
    sel <- input$diag_focus
    validate(need(length(sel) > 0, "Choisissez au moins un diagnostic ci-dessus."))
    x <- d_diag() |> filter(diagnosis %in% sel); need_data(x)
    t <- x |> mutate(m = as.integer(format(consultation_date, "%m"))) |> count(m, diagnosis) |>
      mutate(lab = factor(mois_fr[m], levels = mois_fr))
    plot_ly(t, x = ~lab, y = ~n, color = ~diagnosis, colors = col_diag, type = "scatter", mode = "lines+markers",
            line = list(width = 2.6, shape = "spline"), marker = list(size = 7),
            hovertemplate = "<b>%{fullData.name}</b> · %{x}<br>%{y} cas<extra></extra>") |>
      sty(yt = "Cas par mois", legend = TRUE) |> layout(yaxis = list(rangemode = "tozero"))
  })

  # ======================= GÉOGRAPHIE =======================
  geo_district <- reactive({
    x <- d(); need_data(x)
    x |> group_by(region, district) |>
      summarise(n = n(), rupture = mean(rupture), cout = mean(treatment_cost, na.rm = TRUE),
                assures = mean(assure, na.rm = TRUE), .groups = "drop") |>
      left_join(coords, by = "district")
  })
  geo_label <- c(n = "Consultations", rupture = "Taux de rupture", cout = "Coût moyen (USD)", assures = "Taux d'assurés")
  geo_fmt <- function(v, m) switch(m, n = fmt_n(v), rupture = fmt_pct(v), cout = paste(fmt_n(v, 1), "USD"), assures = fmt_pct(v))

  output$map <- renderLeaflet({
    g <- geo_district(); m <- input$geo_metric
    vals <- g[[m]]
    pal_fn <- colorNumeric(switch(m, rupture = c("#FDE68A", "#F59E0B", "#B91C1C"), assures = c("#DCEFFB", "#0369A1", "#0C2D48"),
                                  c("#D9F2EE", "#14B8A6", "#0A4F49")), domain = vals, na.color = "#CBD5E1")
    leaflet(g, options = leafletOptions(zoomControl = TRUE, minZoom = 5)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      setView(12.4, 6.3, zoom = 6) |>
      addCircleMarkers(lng = ~lng, lat = ~lat, radius = ~6 + 14 * sqrt(n / max(n)), fillColor = ~pal_fn(vals), fillOpacity = .85,
                       color = "white", weight = 1.5,
                       label = ~lapply(sprintf("<b>%s</b> (%s)<br>%s : %s", district, region, geo_label[m], geo_fmt(vals, m)), HTML),
                       popup = ~sprintf("<div style='font-family:Inter,sans-serif;min-width:170px'><b style='color:#0F766E;font-size:14px'>%s</b><br><span style='color:#64748B'>%s</span><hr style='margin:6px 0'>Consultations : <b>%s</b><br>Taux de rupture : <b>%s</b><br>Coût moyen : <b>%s USD</b><br>Assurés : <b>%s</b></div>",
                                         district, region, fmt_n(n), fmt_pct(rupture), fmt_n(cout, 1), fmt_pct(assures))) |>
      addLegend("bottomright", pal = pal_fn, values = vals, title = unname(geo_label[m]), opacity = .9,
                labFormat = function(type, cuts, p) vapply(cuts, \(z) geo_fmt(z, m), character(1)))
  })

  output$p_geo_bar <- renderPlotly({
    x <- d(); need_data(x); m <- input$geo_metric
    t <- x |> group_by(region) |>
      summarise(n = n(), rupture = mean(rupture), cout = mean(treatment_cost, na.rm = TRUE), assures = mean(assure, na.rm = TRUE), .groups = "drop") |>
      arrange(.data[[m]]) |> mutate(region = factor(region, levels = region), v = .data[[m]])
    col <- switch(m, rupture = pal$rouge, assures = pal$bleu, pal$teal)
    plot_ly(t, x = ~v, y = ~region, type = "bar", orientation = "h", marker = list(color = col, opacity = .9),
            text = ~geo_fmt(v, m), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = paste0("<b>%{y}</b><br>", geo_label[m], " : %{text}<extra></extra>")) |>
      sty() |> layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(t$v, na.rm = TRUE) * 1.25)),
                      yaxis = list(gridcolor = "rgba(0,0,0,0)"), margin = list(l = 8, r = 40, t = 8, b = 8))
  })

  output$t_region <- renderDT({
    x <- d(); need_data(x)
    t <- x |> group_by(Région = region) |>
      summarise(Consultations = n(), `Âge médian` = median(patient_age, na.rm = TRUE), `Coût moyen (USD)` = round(mean(treatment_cost, na.rm = TRUE), 2),
                `Taux de rupture` = mean(rupture), `Taux d'assurés` = mean(assure, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(Consultations))
    datatable(t, rownames = FALSE, options = dt_opts(dom = "t", pageLength = 10), class = "compact hover") |>
      formatPercentage(c("Taux de rupture", "Taux d'assurés"), 1, dec.mark = ",") |>
      formatRound("Consultations", 0, mark = " ") |>
      formatStyle("Taux de rupture", background = styleColorBar(c(0, max(t$`Taux de rupture`) * 1.1), "#FCA5A5"),
                  backgroundSize = "98% 60%", backgroundRepeat = "no-repeat", backgroundPosition = "center") |>
      formatStyle("Consultations", background = styleColorBar(c(0, max(t$Consultations) * 1.1), "#99E0D5"),
                  backgroundSize = "98% 60%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  # ======================= MÉDICAMENTS =======================
  output$kpis_rupture <- renderUI({
    x <- d(); need_data(x)
    e <- x |> group_by(facility_name) |> summarise(n = n(), r = mean(rupture), .groups = "drop") |> filter(n >= 30) |> arrange(desc(r))
    dd <- x |> group_by(district) |> summarise(n = n(), r = mean(rupture), .groups = "drop") |> filter(n >= 30)
    kpi_row(
      kpi_card("Taux de rupture", fmt_pct(mean(x$rupture)), sub = delta_tag(mean(x$rupture), nat$rupture), icon = "exclamation-octagon", theme = "red"),
      kpi_card("Consultations en rupture", fmt_n(sum(x$rupture)), sub = paste0("sur ", fmt_n(nrow(x)), " consultations"), icon = "capsule-pill", theme = "amber"),
      kpi_card("Districts > moyenne", fmt_n(sum(dd$r > mean(x$rupture))), sub = paste0("sur ", nrow(dd), " districts analysés"), icon = "geo-alt", theme = "blue"),
      kpi_card("Établissement le + touché", if (nrow(e)) fmt_pct(e$r[1]) else "–", sub = if (nrow(e)) as.character(e$facility_name[1]) else NULL, icon = "hospital", theme = "violet")
    )
  })

  output$p_rup_region <- renderPlotly({
    x <- d(); need_data(x); mu <- mean(x$rupture)
    t <- x |> group_by(region) |> summarise(r = mean(rupture), n = n(), .groups = "drop") |> arrange(r) |> mutate(region = factor(region, levels = region))
    plot_ly(t, x = ~r, y = ~region, type = "bar", orientation = "h",
            marker = list(color = ifelse(t$r > mu, pal$rouge, "#94A3B8")),
            text = ~fmt_pct(r), textposition = "outside", cliponaxis = FALSE, customdata = ~n,
            hovertemplate = "<b>%{y}</b><br>Rupture : %{text}<br>%{customdata:,} consultations<extra></extra>") |>
      sty() |> layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(t$r) * 1.25)),
                      yaxis = list(gridcolor = "rgba(0,0,0,0)"),
                      shapes = list(list(type = "line", x0 = mu, x1 = mu, y0 = -.5, y1 = nrow(t) - .5, line = list(color = pal$encre, dash = "dot", width = 1.3))),
                      margin = list(l = 8, r = 40, t = 8, b = 8))
  })
  output$p_rup_district <- renderPlotly({
    x <- d(); need_data(x)
    t <- x |> group_by(district, region) |> summarise(r = mean(rupture), n = n(), .groups = "drop") |> filter(n >= 20) |> arrange(desc(r)) |> slice_head(n = 12) |>
      arrange(r) |> mutate(district = factor(district, levels = district))
    validate(need(nrow(t) > 0, "Échantillon trop faible pour classer les districts."))
    plot_ly(t, x = ~r, y = ~district, type = "bar", orientation = "h", marker = list(color = pal$rouge, opacity = .88),
            text = ~fmt_pct(r), textposition = "outside", cliponaxis = FALSE, customdata = ~region,
            hovertemplate = "<b>%{y}</b> (%{customdata})<br>Rupture : %{text}<extra></extra>") |>
      sty() |> layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(t$r) * 1.25)), yaxis = list(gridcolor = "rgba(0,0,0,0)"),
                      margin = list(l = 8, r = 40, t = 8, b = 8))
  })
  output$p_rup_mois <- renderPlotly({
    x <- d(); need_data(x)
    t <- x |> mutate(m = as.integer(format(consultation_date, "%m"))) |> group_by(m) |> summarise(r = mean(rupture), n = n(), .groups = "drop") |>
      arrange(m) |> mutate(lab = factor(mois_fr[m], levels = mois_fr[m]))
    plot_ly(t, x = ~lab, y = ~r, type = "scatter", mode = "lines+markers", line = list(color = pal$rouge, width = 3, shape = "spline"),
            marker = list(color = "white", size = 9, line = list(color = pal$rouge, width = 2.5)), customdata = ~n,
            hovertemplate = "<b>%{x}</b><br>Rupture : %{y:.1%}<br>%{customdata:,} consultations<extra></extra>") |>
      sty(yt = "Taux de rupture", yfmt = ".0%") |>
      layout(shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = mean(x$rupture), y1 = mean(x$rupture), line = list(color = pal$gris, dash = "dot", width = 1.3))),
             yaxis = list(rangemode = "tozero"))
  })
  output$p_rup_diag <- renderPlotly({
    x <- d_diag(); need_data(x)
    t <- x |> group_by(diagnosis) |> summarise(r = mean(rupture), n = n(), .groups = "drop") |> arrange(r) |> mutate(diagnosis = factor(diagnosis, levels = diagnosis))
    plot_ly(t, x = ~r, y = ~diagnosis, type = "bar", orientation = "h", marker = list(color = pal$ambre),
            text = ~fmt_pct(r), textposition = "outside", cliponaxis = FALSE, customdata = ~n,
            hovertemplate = "<b>%{y}</b><br>Rupture : %{text}<br>%{customdata:,} consultations<extra></extra>") |>
      sty() |> layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(t$r) * 1.25)), yaxis = list(gridcolor = "rgba(0,0,0,0)"),
                      margin = list(l = 8, r = 40, t = 8, b = 8))
  })
  output$t_rup_etab <- renderDT({
    x <- d(); need_data(x)
    t <- x |> group_by(Établissement = facility_name, District = district, Région = region, Type = type_etablissement) |>
      summarise(Consultations = n(), `Taux de rupture` = mean(rupture), .groups = "drop") |> filter(Consultations >= 30) |> arrange(desc(`Taux de rupture`))
    validate(need(nrow(t) > 0, "Aucun établissement avec au moins 30 consultations dans cette sélection."))
    datatable(t, rownames = FALSE, class = "compact hover", options = dt_opts(pageLength = 8)) |>
      formatPercentage("Taux de rupture", 1, dec.mark = ",") |>
      formatStyle("Taux de rupture", background = styleColorBar(c(0, max(t$`Taux de rupture`) * 1.1), "#FCA5A5"),
                  backgroundSize = "98% 60%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  # ======================= COÛTS & ASSURANCE =======================
  output$kpis_cout <- renderUI({
    x <- d(); need_data(x)
    ci <- x |> filter(assure %in% c(TRUE, FALSE)) |> group_by(assure) |> summarise(m = mean(treatment_cost, na.rm = TRUE), .groups = "drop")
    ecart <- if (nrow(ci) == 2) ci$m[ci$assure] - ci$m[!ci$assure] else NA
    kpi_row(
      kpi_card("Coût moyen", fmt_n(mean(x$treatment_cost, na.rm = TRUE), 2), "USD", sub = paste0("médiane ", fmt_n(median(x$treatment_cost, na.rm = TRUE), 2), " USD"), icon = "wallet2", theme = "violet"),
      kpi_card("Coûts cumulés", fmt_n(sum(x$treatment_cost, na.rm = TRUE) / 1000, 1), "k USD", sub = paste0(fmt_pct(mean(!is.na(x$treatment_cost)), 0), " des coûts renseignés"), icon = "graph-up-arrow", theme = "green"),
      kpi_card("Patients assurés", fmt_pct(mean(x$assure, na.rm = TRUE)), sub = delta_tag(mean(x$assure, na.rm = TRUE), nat$assures, bad_up = FALSE), icon = "shield-plus", theme = "blue"),
      kpi_card("Écart assurés / non assurés", if (is.na(ecart)) "–" else paste0(ifelse(ecart >= 0, "+", ""), fmt_n(ecart, 2)), "USD", sub = "coût moyen, assurés − non assurés", icon = "arrow-left-right", theme = "amber")
    )
  })
  output$p_cout_diag <- renderPlotly({
    x <- d_diag() |> filter(!is.na(treatment_cost)); need_data(x, 20)
    ord <- x |> group_by(diagnosis) |> summarise(m = median(treatment_cost), .groups = "drop") |> arrange(m)
    x <- x |> mutate(diagnosis = factor(diagnosis, levels = ord$diagnosis))
    plot_ly(x, x = ~treatment_cost, y = ~diagnosis, type = "box", orientation = "h", fillcolor = "rgba(15,118,110,0.22)",
            boxmean = TRUE, marker = list(size = 3, opacity = .35, color = pal$teal), line = list(width = 1.6, color = pal$teal),
            hovertemplate = "%{x:.2f} USD<extra>%{y}</extra>") |>
      sty(xt = "Coût du traitement (USD)") |> layout(yaxis = list(gridcolor = "rgba(0,0,0,0)"))
  })
  output$p_cout_hist <- renderPlotly({
    x <- d() |> filter(!is.na(treatment_cost)); need_data(x, 20)
    plot_ly(x, x = ~treatment_cost, type = "histogram", xbins = list(start = 0, size = 2.5), marker = list(color = pal$bleu, line = list(color = "white", width = 1)),
            hovertemplate = "%{x} USD : %{y} consultations<extra></extra>") |>
      sty(xt = "Coût (USD)", yt = "Consultations") |>
      layout(shapes = list(list(type = "line", x0 = median(x$treatment_cost), x1 = median(x$treatment_cost), y0 = 0, y1 = 1, yref = "paper", line = list(color = pal$rouge, dash = "dash", width = 2))),
             annotations = list(list(x = median(x$treatment_cost), y = 1, yref = "paper", text = paste0("médiane ", fmt_n(median(x$treatment_cost), 1)), showarrow = FALSE, xanchor = "left", xshift = 6, font = list(color = pal$rouge, size = 11))))
  })
  output$p_assur_region <- renderPlotly({
    x <- d() |> filter(insurance_status != NR); need_data(x)
    t <- x |> group_by(region) |> summarise(r = mean(assure), n = n(), .groups = "drop") |> arrange(r) |> mutate(region = factor(region, levels = region))
    plot_ly(t, x = ~r, y = ~region, type = "bar", orientation = "h", marker = list(color = pal$bleu_l), text = ~fmt_pct(r), textposition = "outside",
            cliponaxis = FALSE, customdata = ~n, hovertemplate = "<b>%{y}</b><br>Assurés : %{text}<br>%{customdata:,} consultations<extra></extra>") |>
      sty() |> layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, range = c(0, max(t$r) * 1.25)), yaxis = list(gridcolor = "rgba(0,0,0,0)"),
                      margin = list(l = 8, r = 40, t = 8, b = 8))
  })
  output$p_cout_type <- renderPlotly({
    x <- d() |> filter(insurance_status != NR, !is.na(treatment_cost)); need_data(x, 20)
    t <- x |> group_by(consultation_type, insurance_status) |> summarise(m = mean(treatment_cost), n = n(), .groups = "drop")
    plot_ly(t, x = ~consultation_type, y = ~m, color = ~insurance_status, colors = c("Assuré" = pal$teal, "Non assuré" = "#94A3B8"), type = "bar", customdata = ~n,
            hovertemplate = "<b>%{x}</b> · %{fullData.name}<br>%{y:.2f} USD (%{customdata:,} consultations)<extra></extra>") |>
      sty(yt = "Coût moyen (USD)", legend = TRUE, legy = 1.12) |> layout(barmode = "group", bargap = .25, xaxis = list(tickangle = -20), yaxis = list(rangemode = "tozero"), margin = list(t = 34))
  })
  output$stats_tests <- renderUI({
    x <- d(); need_data(x, 50)
    row <- function(titre, detail, p) {
      sig <- !is.na(p) && p < .05
      div(class = "stat-row", div(tags$b(titre), tags$br(), span(class = "legend-mini", detail)),
          div(style = "text-align:right", span(class = if (sig) "badge-sig" else "badge-ns", if (sig) "Différence significative" else "Non significatif"),
              tags$br(), span(class = "legend-mini", paste0("p = ", fmt_p(p)))))
    }
    safe <- function(expr) tryCatch(expr, error = function(e) NA_real_)
    xd <- d_diag() |> filter(!is.na(treatment_cost))
    p1 <- safe(if (n_distinct(xd$diagnosis) > 1) kruskal.test(treatment_cost ~ droplevels(diagnosis), data = xd)$p.value else NA)
    xa <- x |> filter(insurance_status != NR, !is.na(treatment_cost))
    p2 <- safe(if (n_distinct(xa$insurance_status) > 1) wilcox.test(treatment_cost ~ droplevels(insurance_status), data = xa)$p.value else NA)
    p3 <- safe(if (n_distinct(x$region) > 1) chisq.test(table(x$region, x$rupture))$p.value else NA)
    p4 <- safe(if (n_distinct(x$district) > 1) chisq.test(table(x$district, x$rupture))$p.value else NA)
    tagList(
      row("Le coût varie-t-il selon le diagnostic ?", "Test de Kruskal-Wallis", p1),
      row("Les assurés paient-ils un coût différent ?", "Test de Wilcoxon-Mann-Whitney", p2),
      row("Le taux de rupture diffère-t-il entre régions ?", "Test du χ² d'indépendance", p3),
      row("Le taux de rupture diffère-t-il entre districts ?", "Test du χ² d'indépendance", p4),
      div(class = "note mt-2", bs_icon("info-circle"), " Un résultat « non significatif » signifie que l'écart observé peut s'expliquer par le hasard d'échantillonnage. Sur de petites sélections, la puissance des tests est faible.")
    )
  })

  # ======================= QUALITÉ =======================
  output$kpis_qualite <- renderUI({
    nb <- nrow(df) + (if (!is.null(journal)) journal$lignes_concernees[1] else 0)
    comp <- mean(!is.na(as.matrix(readRDS("data/consultations_clean.rds")[, c("patient_age", "gender", "diagnosis", "treatment_cost", "insurance_status")])))
    kpi_row(
      kpi_card("Lignes brutes", fmt_n(nb), sub = "fichier source", icon = "file-earmark-x", theme = "amber"),
      kpi_card("Lignes fiabilisées", fmt_n(nrow(df)), sub = "patients uniques", icon = "file-earmark-check", theme = "green"),
      kpi_card("Doublons supprimés", fmt_n(nb - nrow(df)), sub = fmt_pct((nb - nrow(df)) / nb) |> paste("des lignes brutes"), icon = "files", theme = "red"),
      kpi_card("Complétude moyenne", fmt_pct(comp), sub = "âge, sexe, diagnostic, coût, assurance", icon = "patch-check", theme = "blue")
    )
  })
  output$t_journal <- renderDT({
    validate(need(!is.null(journal), "Journal de qualité indisponible."))
    t <- journal |> rename(Problème = probleme, Traitement = traitement, Lignes = lignes_concernees)
    datatable(t, rownames = FALSE, class = "compact hover", options = dt_opts(dom = "t", pageLength = 15, ordering = FALSE)) |>
      formatRound("Lignes", 0, mark = " ")
  })
  output$p_complet <- renderPlotly({
    cols <- c("Âge" = "patient_age", "Sexe" = "gender", "Diagnostic" = "diagnosis", "Coût" = "treatment_cost", "Assurance" = "insurance_status", "Région" = "region", "Date" = "consultation_date")
    raw <- readRDS("data/consultations_clean.rds")
    t <- tibble(v = names(cols), p = sapply(cols, \(c) mean(!is.na(raw[[c]])))) |> arrange(p) |> mutate(v = factor(v, levels = v))
    plot_ly(t, x = ~p, y = ~v, type = "bar", orientation = "h", marker = list(color = ifelse(t$p < .96, pal$ambre, pal$teal)),
            text = ~fmt_pct(p), textposition = "outside", cliponaxis = FALSE, hovertemplate = "<b>%{y}</b> : %{text}<extra></extra>") |>
      sty() |> layout(xaxis = list(range = c(0, 1.12), tickformat = ".0%"), yaxis = list(gridcolor = "rgba(0,0,0,0)"), margin = list(l = 8, r = 40, t = 8, b = 8))
  })
  output$incoh <- renderText({
    a <- sum(df$consultation_type == "Prénatale" & df$gender == "Masculin")
    b <- sum(df$consultation_type == "Prénatale" & !is.na(df$patient_age) & (df$patient_age < 12 | df$patient_age > 50))
    paste0(fmt_n(a), " consultations prénatales concernent des patients masculins et ", fmt_n(b), " des patients hors de la tranche 12-50 ans.")
  })

  # ======================= DONNÉES =======================
  table_data <- reactive({
    d() |> transmute(ID = patient_id, Date = consultation_date, Région = region, District = district, Établissement = facility_name,
                     Âge = patient_age, Sexe = gender, Diagnostic = diagnosis, `Coût (USD)` = treatment_cost,
                     Médicament = medication_available, Consultation = consultation_type, Assurance = insurance_status)
  })
  output$t_data <- renderDT({
    datatable(table_data(), rownames = FALSE, filter = "top", class = "compact hover stripe", options = dt_opts(pageLength = 12, scrollX = TRUE)) |>
      formatStyle("Médicament", color = styleEqual("Rupture", pal$rouge), fontWeight = styleEqual("Rupture", "bold"))
  })
  output$dl <- downloadHandler(
    filename = function() paste0("consultations_selection_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) write_csv(table_data(), file)
  )
}

dt_opts <- function(dom = "frtip", pageLength = 10, scrollX = FALSE, ordering = TRUE) {
  list(dom = dom, pageLength = pageLength, scrollX = scrollX, ordering = ordering, autoWidth = FALSE,
       language = list(search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes", info = "_START_ à _END_ sur _TOTAL_ lignes",
                       infoEmpty = "Aucune ligne", infoFiltered = "(filtré sur _MAX_)", zeroRecords = "Aucun résultat",
                       emptyTable = "Aucune donnée", infoThousands = "00a0", paginate = list(`next` = "Suivant", previous = "Précédent")))
}

shinyApp(ui, server)
