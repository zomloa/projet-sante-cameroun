# Analyse Santé – Cameroun 2025

Workflow complet : import → exploration → nettoyage → transformation → analyse → visualisation → statistiques → dashboard → rapport → recommandations.

| Fichier | Rôle |
|---|---|
| `R/01_import_nettoyage.R` | Import, exploration, nettoyage, nouvelles variables, journal qualité |
| `R/02_analyse.R` | Analyses métier, 9 graphiques, statistiques descriptives, tests d'hypothèses |
| `R/theme_sante.R` | Charte graphique (palette teal/bleu clinique, thème ggplot) |
| `rapport/rapport_analyse.Rmd` → `.html` | Rapport de synthèse et recommandations |
| `app/app.R` | Dashboard Shiny (7 onglets, 6 filtres globaux) |
| `run_all.R` | Exécute toute la chaîne |
| `data/` | `consultations_raw.csv` (source), `consultations_clean.rds/.csv` |
| `outputs/` | `figures/*.png`, `tables/*.csv`, `journal_qualite.csv`, `resultats.rds` |

## Utilisation

```r
source("run_all.R")          # ou : Rscript run_all.R
shiny::runApp("app")         # lancer le dashboard
```

Packages : tidyverse, janitor, broom, shiny, bslib, bsicons, plotly, leaflet, DT, rmarkdown (+ pandoc).
Le dashboard charge des polices Google et un fond de carte Esri : une connexion internet est nécessaire pour l'affichage complet.

Hypothèse : le coût de traitement est supposé en USD (non précisé dans la source).
