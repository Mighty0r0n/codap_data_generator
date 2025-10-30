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
  
  survey_df <- merge_re_survey_with_dwd_grids()
  
  
  
  invisible(TRUE)
}


generate_plant_env_data()
