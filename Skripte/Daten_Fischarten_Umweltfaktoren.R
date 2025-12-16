suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(tidyr)
  library(readxl)
  library(purrr)
  library(lubridate)
  library(sf)
  library(dbscan)
})

source("Skripte/utils_data_description.R")


add_water_data <- function(df) {
  water_path <- path("raw_data", "Gewässer")
  
  water_file <- path(water_path, "Gewässerdaten_Datenanfrage_UBA.xlsx")
  
  
  water_sheets <- excel_sheets(water_file)
  
  sheets <- setdiff(water_sheets, "Messstellenangaben")
  sheets <- sheets[sheets != "Impressum"]
  
  
  messdaten <- map_dfr(
    sheets,
    ~ read_xlsx(water_file, sheet = .x) |>
      mutate(parameter = .x)
  ) |>
    mutate(
      jahr = year(messzeit),
      bestgrenze = na_if(bestgrenze, -999),
      messwert = ifelse(is.na(messwert) & !is.na(bestgrenze),
                        bestgrenze / 2,
                        messwert)
    )
  
  messdaten <- messdaten |>
    select(stat_lawa, messart, parameter, jahr, messwert)
  
  
  return(df)
}

# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion eingenen Gültigkeitsbereich um die "uniqueness" der Variable zu gewährleisten.
generate_fish_env_data <- function() {
  fish_dir <- path("raw_data", "fish_gbif")
  fish_occurrence_pfad <- path(fish_dir, "occurrence.txt")
  
  col_to_drop <- c(
    "bibliographicCitation",
    "license",
    "language",
    "modified",
    "publisher",
    "references",
    "rightsHolder",
    "type",
    "datasetID",
    "institutionCode",
    "collectionCode",
    "datasetName",
    "ownerInstitutionCode",
    "basisOfRecord",
    "occurrenceStatus",
    "preparations",
    "continent",
    "countryCode",
    "kingdom",
    "phylum",
    "class",
    "taxonRank",
    "taxonomicStatus",
    "datasetKey",
    "publishingCountry",
    "lastInterpreted",
    "issue",
    "hasCoordinate",
    "hasGeospatialIssues",
    "kingdomKey",
    "phylumKey",
    "classKey",
    "protocol",
    "lastParsed",
    "lastCrawled",
    "repatriated",
    "projectId",
    "isSequenced",
    "gbifRegion",
    "publishedByGbifRegion",
    "eventID",
    "higherGeographyID",
    "taxonomicIssue",
    "eventDate",
    "month",
    "day"
  )
  
  
  
  
  fish_occurrence_df <- read_tsv(fish_occurrence_pfad)
  
  
  # Hier werden alle Spalten die ausschließlich NA's enthalten und weitere unintressante Spalten gefiltert
  fish_reduced <- fish_occurrence_df %>%
    select(where(~ !all(is.na(.))), -all_of(col_to_drop)) %>%
    drop_na()
  
  # Hier wird gefiltert und der Datensatz fürs mapping vorbereitet
  fish_handler <- fish_reduced %>%
    filter(!habitat %in% c("Uebergangsgewaesser", "Graben")) %>%
    select(individualCount, year, locality, decimalLatitude, decimalLongitude, species, level1Name)
  
  
  # sf ist ein Tool um koordinaten über ein grid zu clustern. Das verwende ich um die Standorte zusammenzuführen bei leicht abweichenden Koordinaten
  # gibt sonst keine Spalte um sauber und schnell eine Zuweisung nach Sampling Location zu machen. Die vorhandene Spalte locality ist zu heterogen um
  # dort eine saubere aufteilung zu garantieren bei der Datenmenge
  
  fish_with_id <- st_as_sf(
    fish_handler,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(25833)
  
  coords <- st_coordinates(fish_with_id)
  
  
  # Alle Punkte in einem 100Meter raster werden zu einer ID zusammengefasst und als Spalte dem dataframe hinzugefügt für das spätere mapping
  fish_with_id$site_id <- dbscan(coords, eps = 100, minPts = 1)$cluster
  
  fish_with_id <- st_drop_geometry(fish_with_id)
  
  
  
  # Aggregieren der Daten nach Spezies, Jahr und Standort und berechnen der zusätzlichen Spalten
  species_site_year <- fish_with_id |>
    group_by(site_id, year, species) |>
    summarise(
      Spezies_Count = sum(individualCount, na.rm = TRUE),
      Longitude = mean(decimalLongitude, na.rm = TRUE),
      Latitude  = mean(decimalLatitude,  na.rm = TRUE),
      sampling_Ort = first(locality),
      Bundesland = first(level1Name),
      .groups = "drop"
    ) 
  
  
  # Muss getrennt gemacht werden, da ich hier nicht nach Spezeis trennen möchte um den Gesamtcount zu erhalten
  all_site_year <- species_site_year |>
    group_by(site_id, year) |>
    summarise(
      Gesamt_Individuen = sum(Spezies_Count),
      Gesamt_Spezies = n_distinct(species),
      .groups = "drop"
    )
  
  
  # Mergen und berechnen der fehlenden Spalte
  species_site_year <- species_site_year |>
    left_join(all_site_year, by = c("site_id", "year")) |>
    mutate(
      Spezies_Anteil =
        round(
        (Spezies_Count / Gesamt_Individuen) * 100,
        2
        )
    )
  
  
  # WIP Hier können dann die Gewässerdaten hinzugefügt werden
  df <- add_water_data(df = species_site_year)
  
  
  
  # Datensätze die man später evtl benutzen könnte
  
  see_df <- fish_reduced %>%
    filter(habitat == "See")
  
  
  fliess_df <- fish_reduced %>%
    filter(habitat == "Fliessgewaesser") 
  
  
  
  
  # Log-Zeug vorbereiten
  log_dir <- path("logs", "fish_gbif")
  
  # Kurzer Check ob das log_dir bereits existiert, wenn nicht wird es erstellt.
  dir_create(log_dir, recurse = TRUE)
  
  # Pfad zur eigentlichen log-Datei speziell zu dem df
  occurrence_df_log_file <- path(log_dir, "occurrence.log")
  
  describe_df(df = fish_reduced,
              log_dir = log_dir,
              log_file = occurrence_df_log_file)
  
  
  invisible(TRUE)
}


generate_fish_env_data()
