suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
})
source("Skripte/utils_data_description.R")
# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion eingenen Gültigkeitsbereich um die "uniqueness" der Variable zu gewährleisten.
generate_plant_env_data <- function() {
  
  # Log vorbereiten
  log_dir <- path("logs", "ReSurveyGermany")
  dir_create(log_dir, recurse = TRUE)
  log_file <- path(log_dir, "ReSurvey_merged.log")
  ############################################################
  # 1. Daten einlesen
  ############################################################
  
  
  
  # Pfad zum Ordner der raw_dateien als Variable setzen.
  survey_dir <- path("raw_data", "ReSurveyGermany")
  mean_temp_dir <- path("raw_data", "air_temperature_mean")
  precipitation_path <- path("raw_data", "precipitation")
  
  # Pfäde der genutzten Dateien in Variablen zur späteren Verwendung speichern.
  re_survey_germany_pfad <- path(survey_dir, "ReSurveyGermany.csv")
  header_survey_germany_pfad <- path(survey_dir, "Header_ReSurveyGermany.csv")
  
  asc_file <- path(mean_temp_dir, "grids_germany_annual_air_temp_mean_189117.asc")
  
  
  # Die Daten werden hier nun mit der read_csv Funktion als Objekte geladen
  # und in einer Variablen zur späteren Verwendung gespeichert
  re_survey_germany_df <- read_csv(re_survey_germany_pfad) 
  header_survey_germany_df <- read_csv(header_survey_germany_pfad)
  
  
  # Hinweis für den Nutzer, dass die Daten fehlerfrei eingelesen werden konnten.
  cat(" -> Daten wurden eingelesen.")
  
  
  # Zusammenführen der Arten- und Headerdaten."PROJECT_ID_RELEVE_NR" ist hierbei das
  # Keyword um beide Tabellen zusammenzufügen. Dies wird dann über einen
  # leftjoin gemacht, sodass jede Zeile in der Artentabelle
  # die Einträge für das ensprechende Releve angehängt werden.
  re_survey_germany_merged_df <- re_survey_germany_df %>%
    left_join(header_survey_germany_df, by = "PROJECT_ID_RELEVE_NR")
  
  describe_df(
    df = re_survey_germany_merged_df,
    log_file = log_file
  )
  dwd_raster <- terra::rast(asc_file)
  
  plot(dwd_raster)
  
  
  
  invisible(TRUE)
}

# Das entspricht dem Python `if __name__ == "__main__":`. Der untere Teil des Skripts
# wird mit dieser if Abfrage nur dann ausgeführt, genau diese Datei ausgeführt wird.
if (identical(environment(), globalenv())) {
  generate_plant_env_data()
}