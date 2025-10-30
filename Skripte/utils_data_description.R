################################################################################

# Hier soll eine Sammlung von kleinen Hilfsfunktionen entstehen, damit 
# wiederkehrende Ausdrücke nicht jedes mal Copy-Pasted werden müssen.

################################################################################




# Diese Funktion soll erstmal einen allgemeinen Überblick über die Daten verschaffen

describe_df <- function(df, log_file) {
  suppressPackageStartupMessages({
    library(skimr)
    library(dplyr)
    library(fs)     
  })
  
  
  # Die Ausgaben von tibble und skimr-Auswertungen sind idr gekürzt
  # ich würde aber gerne alle Informationen ausgegeben haben. Dies passiert
  # über options(inf). Außerdem gibt options die alten parameter zurück,
  # diese sollen nach dem Funktionsaufruf wieder hergestellt werden, damit
  # spätere Ausgaben über die Konsole nicht überladen werden.
  old_options <- options(
    tibble.print_max = Inf,
    tibble.width = Inf
  )
  
  
  # sink leitet nun alle folgenden Konsolenausgaben in eine Datei um
  # Dort stehen dann die Beschreibungen zu dem Datensatz.
  sink(log_file)
  
  # on.exit() führt alle mitgegebenen Befehle am Ende der Funktion aus.
  # Das wird aus Gründen der Wart und Lesbarkeit hier benutzt, dann stehen diese Befehle nicht
  # Zusammenhangslos unten in der Funktion.
  on.exit({
    options(old_options)
    sink()
    message("Log gespeichert unter: ", normalizePath(log_file))
    },
    add=FALSE
    )
  
  
  
  # Hier kommen die ersten allgemeinen Informationen
  cat("=== Datensatz:", log_file, "===\n")

  
  cat("Zeilen:", nrow(df), " | Spalten:", ncol(df), "\n\n")
  
  # Erstellung eines tibbles für Strukturiertere Ausgabe der Ergebnisse
  # DataFrames werden immer schön und sauber als Tabelle formatiert geprintet
  overview <- tibble(
    column = names(df),
    type   = sapply(df, function(x) class(x)[1]),
    n_na   = sapply(df, function(x) sum(is.na(x))),
    n_unique = sapply(df, dplyr::n_distinct)
  )
  
  cat("--- Spaltenübersicht ---\n")
  print(overview)
  cat("\n")
  
  
  # Hier wird nochmal mit Skimr über den Datensatz geschaut.
  cat("--- Skim-Statistik ---\n")
  print(skimr::skim(df))
  cat("\n")
  invisible(TRUE)
}

# Das entspricht dem Python `if __name__ == "__main__":`. Das ist ein kleiner
# Trick der es ermöglicht, dass alles was hier unten steht NUR dann ausgeführt
# wird, wenn wir auch GENAU diese Datei ausführen.
# Explizit hier wird nicht wichtiges stehen. In utils files teste ich hier idr die
# Funktionen direkt die ich schreibe.
# Bei den Datensatz_* Files wird es als kleine "Funktionalität" benutzt um die
# Skripte zum einen separat aber zum anderen auch "gepiped" mit dem 00-Skript 
# laufen lassen zu können
if (sys.nframe() == 0) {
  suppressPackageStartupMessages({
    library(readr)
    library(fs)
    })
  
  fisch_dir <- fs::path("raw_data", "fish_gbif")
  fisch_occurrence_pfad <- fs::path(fisch_dir, "occurrence.txt")
  
  fisch_occurrence_df <- read_tsv(fisch_occurrence_pfad)
  
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

}
