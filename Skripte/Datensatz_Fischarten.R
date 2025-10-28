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
  
  
  fisch_dir <- fs::path("raw_data", "fish_gbif")
  fisch_occurrence_pfad <- fs::path(fisch_dir, "occurrence.txt")
  
  fisch_occurrence_df <- read_tsv(fisch_occurrence_pfad)
  
  
################################################################################
#  read_tsv zeigt nach einlesen der Fischdaten eine warning an. 
#  problems(fisch_occurrence_df) zeigt das in Spalte 187 der Datentyp nicht
#  eingelesen werden kann. Diese Spalte hat nur 44 nicht NA einträge
#  und wird deshalb direkt entsorgt.
################################################################################  
  
  
  # Log-Zeug vorbereiten
  log_dir <- fs::path("logs", "fish_gbif")
  
  # Kurzer Check ob das log_dir bereits existiert, wenn nicht wird es erstellt.
  fs::dir_create(log_dir, recurse = TRUE)
  
  # Pfad zur eigentlichen log-Datei speziell zu dem df
  occurrence_df_log_file <- fs::path(log_dir, "occurrence.log")
  
  describe_df(
    df = fisch_occurrence_df,
    log_file = occurrence_df_log_file
  )

  
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
  generate_fish_env_data()
}