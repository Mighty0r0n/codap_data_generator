############################################################
# 00_load_all.R
# Lädt alle benötigten Rohdaten für das Projekt
############################################################

# ---- 1. ReSurveyGermany Daten laden ----
source("Skripte/Data_Loader/01_load_ReSurveyGermany.R")

# ---- 2. DWD Daten laden ----
source("Skripte/Data_Loader/02_load_dwd_asc_grids.R")

message("[DONE] Alle Rohdaten wurden erfolgreich in das Verzeichnis raw_data heruntergeladen.")