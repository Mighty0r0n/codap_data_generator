############################################################
# 00_load_all.R
# Lädt alle benötigten Rohdaten für das Projekt
############################################################


# Alle Skripte




# ---- 1. ReSurveyGermany download Funktion initialisieren ----
source("Skripte/Data_Loader/01_load_ReSurveyGermany.R")

# # ---- 2. DWD Daten download Funktion initialisieren ----
source("Skripte/Data_Loader/02_load_dwd_asc_grids.R")




# Hier werden die eigentlichen Funktionen zum Laden der Dateien aufgerufen.
download_resurvey_germany()
download_dwd_grids()

message("[DONE] Alle Rohdaten wurden erfolgreich in das Verzeichnis raw_data heruntergeladen.")