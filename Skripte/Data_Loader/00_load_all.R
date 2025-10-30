############################################################
# 00_load_all.R
# Lädt alle benötigten Rohdaten für das Projekt
############################################################


# ACHTUNG, meinen Plan den ich für dieses Skript hatte kann ich bisher nicht umsetzen.
# Dieses Skript führt leider jeden download 2 mal aus. Lieber die 01-03 skripte nutzen





# Alle Skripte




# ---- 1. ReSurveyGermany download Funktion initialisieren ----
source("Skripte/Data_Loader/01_load_ReSurveyGermany.R")

# # ---- 2. DWD Daten download Funktion initialisieren ----
source("Skripte/Data_Loader/02_load_dwd_asc_grids.R")

# # ---- 3. Fisch Abundanzdaten download Funktion initialisieren  ----
source("Skripte/Data_Loader/03_load_fish_occurrences.R")


# Hier werden die eigentlichen Funktionen zum Laden der Dateien aufgerufen.
download_resurvey_germany()
download_dwd_grids()

message("[DONE] Alle Rohdaten wurden erfolgreich in das Verzeichnis raw_data heruntergeladen.")