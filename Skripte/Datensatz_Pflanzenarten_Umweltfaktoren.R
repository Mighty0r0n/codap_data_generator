suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  })

# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion einen separaten Namespace um die "uniqueness" der Variable zu gewährleisten.
generate_plant_env_data <- function() {


  
  ############################################################
  # 1. Daten einlesen
  ############################################################

  
  
  # Pfad zum Ordner der raw_dateien als Variable setzen.
  survey_pfad <- "raw_data/ReSurveyGermany/"
  mean_temp_path <- "raw_data/air_temperature_mean/"
  precipitation_path <- "raw_data/precipitation/"
  
  # Pfäde der genutzten Dateien in Variablen zur späteren Verwendung speichern.
  re_survey_germany_pfad <- paste0(survey_pfad, "ReSurveyGermany.csv")
  header_survey_germany_pfad <- paste0(survey_pfad, "Header_ReSurveyGermany.csv")
  
  asc_file <- paste0(mean_temp_path, "grids_germany_annual_air_temp_mean_189117.asc")
  
  
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
  
  
  dwd_raster <- terra::rast(asc_file)
  
  plot(dwd_raster)
  
  
  
  invisible(TRUE)
}

# Das entspricht dem Python `if __name__ == "__main__":`. Der untere Teil des Skripts
# wird mit dieser if Abfrage nur dann ausgeführt, wenn die Datei separat ausgeführt wird
# und nicht wenn sie über das skript 00_load_all.R ausgeführt wird. Sonst würden
# wir alles doppelt runterladen. Da die oben definierte Funktion sonst in 00_load_all.R
# über source() ausgeführt wird, was jede Zeile in der gesourcten datei ausführt und über
# den separaten Aufruf in 00 ein zweites mal ausgeführt wird.
# Ich möchte aber die Funktionalität, dass man alles über 00_load_all runter laden kann und jeweils
# separat über die einzelnen Skripte.
if (identical(environment(), globalenv())) {
  generate_plant_env_data()
}
