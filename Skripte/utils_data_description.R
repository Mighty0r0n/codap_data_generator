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

get_value_from_grid <- function(r_stack, year, longitude, latitude) {
  # Hier suchen wir das entsprechende Objekt zum passendem Jahr raus
  layer_name <- as.character(year)
  if (!layer_name %in% names(r_stack)) {
    stop("Fehler beim Datenfiltern. Jahr nicht im Stack: ", layer_name)
  }
  r_year <- r_stack[[layer_name]]
  
  # Die Koordinaten der Re_Survey_Germany liegen als WGS-84 vor. Zum abgleich zu den
  # DWD Rastern werden diese jetzt auf EPSG:31467 projeziert.
  pt_wgs <- vect(
    data.frame(lon = longitude, lat = latitude),
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
    )
  
  pt_gk  <- project(pt_wgs, crs(r_year))
  
  # Jetzt wird die Value an dem entsprechendem Punkt zurückgegeben
  val <- extract(r_year, pt_gk, ID = FALSE)[[1]]
  
  
  # Laut den Metadaten liegen die Temperaturen in 1/10°C vor, dementsprechend
  # Rechnen wir es auf unsere "gängige" skala um
  val_celsius <- val / 10
  return(val_celsius)
}



# Diese Funktion wird genutzt um Daten aus DWD Rasterdaten unseren CSVs anzuhängen
add_grid_data_to_dataframe <- function(df, grid_stack, column_name) {
  # Funktion die hauptsächlich bisher mit den ReSurveyGermany und DWD grids benutzt wird
  # Wenn weitere Daten als ReSurveyGermany hinzukommen muss ein check für das benutzte
  # Eingangskoordinatensystem gemacht werden und die punkte richtig zu projezieren
  
  
  # Die Koordinaten der Re_Survey_Germany liegen als WGS-84 vor. Zum abgleich zu den
  # DWD Rastern werden diese jetzt auf EPSG:31467 projeziert.
  coordinates_wgs <- terra::vect(df, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")
  coordinates_epsg  <- terra::project(coordinates_wgs, terra::crs(grid_stack))
  
  # Rasterwerte extrahieren (alle Jahre gleichzeitig)
  all_values <- terra::extract(grid_stack, coordinates_epsg, ID = FALSE)
  
  
  # Hier werden jetzt die WERTE der vorher extrahierten Koordinaten mit den Jahreszahlen
  # abgeglichen und dem DataFrame angehangen
  df[[column_name]] <- all_values[
    cbind(seq_len(nrow(df)), match(as.character(df$YEAR), names(all_values)))
  ]
  
  # Das DataFrame wird jetzt mit der neuen Spalte zurückgegeben
  return(df)
}
