suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
})

source("Skripte/utils_data_description.R")

# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion eingenen Gültigkeitsbereich um die "uniqueness" der Variable zu gewährleisten.
generate_fish_env_data <- function() {
  
  
  fisch_dir <- path("raw_data", "fish_gbif")
  fisch_occurrence_pfad <- path(fisch_dir, "occurrence.txt")
  
  fisch_occurrence_df <- read_tsv(fisch_occurrence_pfad)
  
  
  ################################################################################
  #  read_tsv zeigt nach einlesen der Fischdaten eine warning an. 
  #  problems(fisch_occurrence_df) zeigt das in Spalte 187 der Datentyp nicht
  #  eingelesen werden kann. Diese Spalte hat nur 44 nicht NA einträge
  #  und wird deshalb direkt entsorgt.
  ################################################################################  
  
  
  # Log-Zeug vorbereiten
  log_dir <- path("logs", "fish_gbif")
  
  # Kurzer Check ob das log_dir bereits existiert, wenn nicht wird es erstellt.
  dir_create(log_dir, recurse = TRUE)
  
  # Pfad zur eigentlichen log-Datei speziell zu dem df
  occurrence_df_log_file <- path(log_dir, "occurrence.log")
  
  describe_df(
    df = fisch_occurrence_df,
    log_file = occurrence_df_log_file
  )
  
  
  invisible(TRUE)
}


generate_fish_env_data()
