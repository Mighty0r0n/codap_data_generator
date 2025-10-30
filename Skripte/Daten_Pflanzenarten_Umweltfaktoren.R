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
  
  
  # Ordner für eventuelle tmp_files anlegen
  tmp_dir <- path("tmp_data", "ReSurveyGermany")
  dir_create(tmp_dir, recurse = TRUE)
  ############################################################
  # 1. Daten einlesen
  ############################################################
  
  
  
  # Hier werden erstmal alle Pfäde zu den Ordnern gesetzt, die brauchen wir später noch
  survey_dir <- path("raw_data", "ReSurveyGermany")
  mean_temperature_dir <- path("raw_data", "air_temperature_mean")
  precipitation_dir <- path("raw_data", "precipitation")
  
  
  # Hier werden jetzt die Pfäde zu den Dateien an sich definiert.
  re_survey_germany_path <- path(survey_dir, "ReSurveyGermany.csv")
  header_survey_germany_path <- path(survey_dir, "Header_ReSurveyGermany.csv")
  
  mean_temperature_file_paths <- dir_ls(mean_temperature_dir, glob = "*.asc", recurse = FALSE)
  precipitation_file_paths <- dir_ls(precipitation_dir, glob = "*.asc", recurse = FALSE)
  

  
  # Hier werden die Dateien dann in R in tibbles eingeladen
  re_survey_germany_df <- read_csv(re_survey_germany_path) 
  header_survey_germany_df <- read_csv(header_survey_germany_path)
  
  
  # Hinweis für den Nutzer, dass die Daten fehlerfrei eingelesen werden konnten.
  cat(" -> Daten wurden eingelesen.")
  
  # Zusammenführen der Arten- und Headerdaten."PROJECT_ID_RELEVE_NR" ist hierbei das
  # Keyword um beide Tabellen zusammenzufügen. Dies wird dann über einen
  # leftjoin gemacht, sodass jede Zeile in der Artentabelle
  # die Einträge für das ensprechende Releve angehängt werden.
  re_survey_germany_merged_df <- re_survey_germany_df %>%
    left_join(header_survey_germany_df, by = "PROJECT_ID_RELEVE_NR")
  
  
  # Wir nutzen die Rasterdaten des DWD von den Jahren 2000-2024, dementsprechend
  # filtere ich alle Datenpunkte raus, die nicht in dieser Jahresspanne liegen.
  re_survey_germany_filtered_years_df <- re_survey_germany_merged_df %>% filter(YEAR > 2000)
  
  
  # Ersten Überblick über das DF bekommen
  describe_df(
    df = re_survey_germany_filtered_years_df,
    log_file = log_file
  )

  
  # Hier werden die asc files as Stack eingeladen.
  # Dabei liegen alle Raster geordnet nach dem Jahr übereinander
  mean_temperature_grid_stack <- rast(mean_temperature_file_paths)
  precipitation_grid_stack <- rast(precipitation_file_paths)
  
  
  # Hier werden den Layern nun namen gegeben damit man diese
  # später besser abgreifen kann
  names(mean_temperature_grid_stack) <- 2000:2024
  names(precipitation_grid_stack) <- 2000:2024
  
  # Das CRS der Rasterdaten wird hier festgelegt, diese INFO habe ich den
  # DWD Rastermetadaten entnommen
  crs(mean_temperature_grid_stack) <- "EPSG:31467"
  crs(precipitation_grid_stack) <- "EPSG:31467"
  
  
  # Helperfunktion um Rasterdaten dem Df hinzuzufügen
  re_survey_germany_filtered_years_df = add_grid_data_to_dataframe(
    re_survey_germany_filtered_years_df,
    mean_temperature_grid_stack,
    column_name="MEAN_TEMPERATURE"
  )
  
  # Laut den Metadaten liegen die Temperaturen in 1/10°C vor, dementsprechend
  # Rechnen wir es auf unsere "gängige" skala um
  re_survey_germany_filtered_years_df$MEAN_TEMPERATURE <-
    re_survey_germany_filtered_years_df$MEAN_TEMPERATURE / 10
  
  
  re_survey_germany_filtered_years_df = add_grid_data_to_dataframe(
    re_survey_germany_filtered_years_df,
    precipitation_grid_stack,
    column_name="PRECIPITATION"
  )
  
  # Hier wird die Datei nun erstmal zwischengespeichert falls man die Tabelle
  # außerhalb dieses Workflows anschauen möchte oder muss
  write.csv(re_survey_germany_filtered_years_df,
            file = path(tmp_dir, "re_survey_germany_filtered_years.csv"),
            row.names = FALSE)
  
  #-----------------------------------------------------------------------------
  # Kleiner NA-Check
  # Hier wurden nur 0,12% der übrigen 300 000 Daten als NA markiert.
  # Diese NA Zeilen verwerfe ich im folgenden einfach, da es echt wenige sind
  # und wir genug Daten übrig haben
  na_temp <- mean(is.na(re_survey_germany_filtered_years_df$MEAN_TEMPERATURE)) * 100
  na_prec <- mean(is.na(re_survey_germany_filtered_years_df$PRECIPITATION)) * 100
  
  cat(sprintf("Fehlende Werte:\n  MEAN_TEMPERATURE: %.2f%%\n  PRECIPITATION: %.2f%%\n",
              na_temp, na_prec))
  
  
  re_survey_germany_filtered_years_df <- re_survey_germany_filtered_years_df[
    !is.na(re_survey_germany_filtered_years_df$MEAN_TEMPERATURE) &
      !is.na(re_survey_germany_filtered_years_df$PRECIPITATION),
  ]
  #-----------------------------------------------------------------------------
  message("ALL DONE")
  
  invisible(TRUE)
}


generate_plant_env_data()
