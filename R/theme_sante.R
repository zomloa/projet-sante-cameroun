# Charte graphique "Santé" partagée par les analyses, le rapport et le dashboard
pal_sante <- list(
  teal       = "#0F766E",   # couleur principale (médical)
  teal_clair = "#14B8A6",
  bleu       = "#0369A1",   # information
  bleu_clair = "#38BDF8",
  vert       = "#16A34A",   # positif
  ambre      = "#F59E0B",   # vigilance
  rouge      = "#DC2626",   # alerte / rupture
  encre      = "#0F2A37",   # texte
  gris       = "#64748B",
  fond       = "#F4F8F9"
)

# Palette qualitative sobre (accessible) pour les catégories
pal_cat <- c("#0F766E", "#0369A1", "#F59E0B", "#7C3AED", "#DB2777",
             "#16A34A", "#EA580C", "#0891B2", "#65A30D", "#64748B")

theme_sante <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = pal_sante$encre),
      plot.title       = ggplot2::element_text(face = "bold", size = base_size + 3, colour = pal_sante$teal),
      plot.subtitle    = ggplot2::element_text(colour = pal_sante$gris, margin = ggplot2::margin(b = 10)),
      plot.caption     = ggplot2::element_text(colour = pal_sante$gris, size = base_size - 3),
      axis.title       = ggplot2::element_text(colour = pal_sante$gris, size = base_size - 1),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E2E8F0"),
      legend.position  = "bottom",
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin      = ggplot2::margin(12, 14, 10, 12)
    )
}
