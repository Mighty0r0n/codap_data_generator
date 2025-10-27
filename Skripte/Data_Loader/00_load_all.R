############################################################
# 00_load_all.R
# Lädt alle benötigten Rohdaten für das Projekt
############################################################

# ---- 1. ReSurveyGermany Daten laden ----
source("Skripte/01_load_ReSurveyGermany.R")

# ---- 2. DWD Daten laden ----
# source("Skripte/02_download_dwd_asc_grids.R")

message("[DONE] Alle Rohdaten wurden erfolgreich in das Verzeichnis raw_data heruntergeladen.")