# Exécute toute la chaîne : nettoyage -> analyse -> rapport -> (option) dashboard
# Usage : Rscript run_all.R        puis    shiny::runApp("app")
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("R/01_import_nettoyage.R", encoding = "UTF-8")
source("R/02_analyse.R", encoding = "UTF-8")
pd <- file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc")
if (!rmarkdown::pandoc_available() && dir.exists(pd)) Sys.setenv(RSTUDIO_PANDOC = pd)
if (rmarkdown::pandoc_available()) {
  rmarkdown::render("rapport/rapport_analyse.Rmd", output_dir = "rapport", quiet = TRUE)
  message("Rapport généré : rapport/rapport_analyse.html")
} else message("Pandoc absent : rapport non généré.")
message("Terminé. Lancer le dashboard avec : shiny::runApp('app')")
