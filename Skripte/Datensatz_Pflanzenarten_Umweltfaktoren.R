############################################################
# 1. Daten einlesen
############################################################



# Pfad zum Ordner der raw_dateien als Variable setzen.
ordner_pfad <- "raw_data/arten_umwelt_modellierung"

# Pfäde der genutzten Dateien in Variablen zur späteren Verwendung speichern.
re_survey_germany_pfad <- paste0(ordner_pfad, "/ReSurveyGermany.csv")




# Die Daten werden hier nun mit der readr::read_csv Funktion als Objekte geladen
# und in einer Variablen zur späteren Verwendung gespeichert
re_survey_germany_data <- readr::read_csv(re_survey_germany_pfad) 


# Hinweis für den Nutzer, dass die Daten fehlerfrei eingelesen werden konnten.
cat(" -> Daten wurden eingelesen.")

############################################################
# 2. Daten bereinigen
############################################################


