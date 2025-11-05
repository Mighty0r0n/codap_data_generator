################################################################################

# Hier soll eine Sammlung von kleinen Hilfsfunktionen entstehen, damit 
# wiederkehrende Ausdrücke nicht jedes mal Copy-Pasted werden müssen.

################################################################################



# Diese Funktion soll erstmal einen allgemeinen Überblick über die Daten verschaffen

describe_df <- function(df, log_dir, log_file) {
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
  dir_create(log_dir, recurse = TRUE)
  
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


merge_re_survey_with_dwd_grids <- function() {
  suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(terra)
    library(fs)
  })
  # Log vorbereiten
  log_dir <- path("logs", "ReSurveyGermany")
 
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
  
  
  # Unwichtige Spalten entfernen
  
  columns_to_remove <- c(
    "PROJECT_ID.x",
    #"RELEVE_NR.x",
    #"PROJECT_ID_RELEVE_NR",
    #"RS_PROJECT",
    "PROJECT_ID.y",
    #"RS_PLOT",
    "LOCALITY",
    #"RS_OBSERV",
    #"RELEVE_NR.y",
    "DATE",
    "LOC_METH_COMMENT",
    "COUNTRY",
    "REFERENCE",
    "YEAR_PUBL",
    "TABLE_NR",
    "NR_IN_TAB",
    "ORIG_NR",
    "ORIG_DB",
    "GEO_LEV"
  )
  
  re_survey_germany_filtered_years_df <- re_survey_germany_filtered_years_df %>% select(-all_of(columns_to_remove))
  
  
  # Ersten Überblick über das DF bekommen
  describe_df(df = re_survey_germany_filtered_years_df, log_dir = log_dir, log_file = log_file)
  
  
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
  re_survey_germany_filtered_years_df = add_grid_data_to_dataframe(re_survey_germany_filtered_years_df,
                                                                   mean_temperature_grid_stack,
                                                                   column_name = "TEMPERATURE")
  
  # Laut den Metadaten liegen die Temperaturen in 1/10°C vor, dementsprechend
  # Rechnen wir es auf unsere "gängige" skala um
  re_survey_germany_filtered_years_df$TEMPERATURE <-
    re_survey_germany_filtered_years_df$TEMPERATURE / 10
  
  
  re_survey_germany_filtered_years_df = add_grid_data_to_dataframe(re_survey_germany_filtered_years_df,
                                                                   precipitation_grid_stack,
                                                                   column_name = "PRECIPITATION")
  

  #-----------------------------------------------------------------------------
  # Kleiner NA-Check
  # Hier wurden nur 0,12% der übrigen 300 000 Daten als NA markiert.
  # Diese NA Zeilen verwerfe ich im folgenden einfach, da es echt wenige sind
  # und wir genug Daten übrig haben
  na_temp <- mean(is.na(re_survey_germany_filtered_years_df$TEMPERATURE)) * 100
  na_prec <- mean(is.na(re_survey_germany_filtered_years_df$PRECIPITATION)) * 100
  
  cat(
    sprintf(
      "Fehlende Werte:\n  MEAN_TEMPERATURE: %.2f%%\n  PRECIPITATION: %.2f%%\n",
      na_temp,
      na_prec
    )
  )
  
  
  re_survey_germany_filtered_years_df <- re_survey_germany_filtered_years_df[!is.na(re_survey_germany_filtered_years_df$TEMPERATURE) &
                                                                               !is.na(re_survey_germany_filtered_years_df$PRECIPITATION), ]
  
  # Hier wird die Datei nun erstmal zwischengespeichert falls man die Tabelle
  # außerhalb dieses Workflows anschauen möchte oder muss
  write.csv(
    re_survey_germany_filtered_years_df,
    file = path(tmp_dir, "re_survey_germany_filtered_years.csv"),
    row.names = FALSE
  )
  
  message("ALL DONE")
  
  return(re_survey_germany_filtered_years_df)
}

# Kleine Funktion mit der RS_PLOTS aus den Daten entfernen kann,
# an denen generell wenige Messungen stattfanden
# Dient lediglich dazu Datensätze um das codap zeilen limit von 5000 zu filtern.
filter_low_density_plots <- function(df, 
                               n_rows_threshold,
                               presence_percent_threshold) {
  library(dplyr)
  
  plot_counts <- df %>%
    count(RS_SITE, RS_PLOT, name = "n_rows")
  
  keep_plots <- plot_counts %>%
    filter(n_rows >= n_rows_threshold) %>%
    select(RS_SITE, RS_PLOT)
  
  df_filtered <- df %>%
    semi_join(keep_plots, by = c("RS_SITE", "RS_PLOT")) 
  
  return(df_filtered)
}

