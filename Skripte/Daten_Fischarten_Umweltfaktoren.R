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
  library(ggplot2)
})

source("Skripte/utils_data_description.R")



add_water_data <- function(df) {
  water_path <- path("raw_data", "Gewässer")
  
  water_file <- path(water_path, "waterbase.csv")
  
  water_df <- read_csv(water_file)
  
  
  # als sf (WGS84)
  fish_sf <- st_as_sf(
    df,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )
  water_sf <- st_as_sf(
    water_df,
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )
  
  # in metrisches CRS für Distanzen
  fish_sf  <- st_transform(fish_sf, 3035)
  water_sf <- st_transform(water_sf, 3035)
  
  # pro Jahr matchen
  years <- sort(unique(fish_sf$year))
  
  out_list <- lapply(years, function(y) {
    fish_y  <- fish_sf  %>% filter(year == y)
    water_y <- water_sf %>% filter(year == y)
    
    
    # Wenn keine Messung in dem Jahr an der Messstelle stattfand, werden die Daten entfernt
    if (nrow(water_y) == 0)
      return(NULL)
    
    
    idx <- st_nearest_feature(fish_y, water_y)
    
    # Distanz zur gematchten Station
    fish_y$water_dist_m <- as.numeric(st_distance(fish_y, water_y[idx, ], by_element = TRUE))
    
    # Wasser-Features dranhängen
    water_feats <- water_y[idx, ]
    
    
    
    fish_tbl <- st_drop_geometry(fish_y) %>%
      rename(LONGITUDE = Longitude,
             LATITUDE  = Latitude,
             Messung_Jahr      = year)
    
    water_tbl <- st_drop_geometry(water_y[idx, ]) %>%
      rename(
        Messstelle_Longitude = Longitude,
        Messstelle_Latitude  = Latitude,
        Messstelle_Jahr      = year
      )
    
    
    bind_cols(fish_tbl, water_tbl)
  })
  
  
  df <- bind_rows(out_list)
  
  # Erst filtern bevor die Messstellen Koordinaten raus gehen, Einigen Messungen konnten keine Gewässermessungen
  # zugefügt werden, so werden die rausgeschmissen
  df <- df %>%
    filter(!is.na(Messstelle_Longitude)) %>%
    select(-Messstelle_Latitude, -Messstelle_Longitude)
  
  return(df)
}



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
    #"eventID",
    "higherGeographyID",
    "taxonomicIssue",
    #"eventDate",
    "month",
    "day"
  )
  
  

  fish_occurrence_df <- read_tsv(fish_occurrence_pfad)
  
  
  

  
  
  
  # Hier werden alle Spalten die ausschließlich NA's enthalten und weitere unintressante Spalten gefiltert
  fish_reduced <- fish_occurrence_df %>%
    select(where( ~ !all(is.na(.))), -all_of(col_to_drop)) %>%
    drop_na() %>%
    select(
      individualCount,
      year,
      locality,
      decimalLatitude,
      decimalLongitude,
      species,
      level1Name,
      eventID,
      eventDate
    ) %>%
    group_by(
      locality,
      year,
      eventDate,
      species
    ) %>%
    summarise(
      individualCount  = mean(individualCount, na.rm = TRUE),
      Latitude  = median(decimalLatitude,  na.rm = TRUE),
      Longitude = median(decimalLongitude, na.rm = TRUE),
      level1Name       = first(level1Name),
      eventID          = first(eventID),
      .groups = "drop"
    ) %>% # Filtern von Localitys die insgesamt über alle jahre nur 3 mal bemessen worden sind. Das betrifft 6 localitys die über alle Jahre nur 1-2 mal bemessen worden sind
    group_by(locality) %>%
    filter(n() > 4) %>%
    ungroup()

  
  species_site_year <- fish_reduced %>%
    group_by(locality, year, species) %>%
    summarise(
      Spezies_Count = floor(mean(individualCount, na.rm = TRUE)),
      n_days = n_distinct(eventDate),
      Longitude = first(Longitude),
      Latitude  = first(Latitude),
      level1Name       = first(level1Name),
      eventID          = first(eventID),
      eventDate        = first(eventDate),
      .groups = "drop"
    )
  
  
  
  # Muss getrennt gemacht werden, da ich hier nicht nach Spezeis trennen möchte um den Gesamtcount zu erhalten
  all_site_year <- species_site_year |>
    group_by(locality, year) |>
    summarise(
      Gesamt_Individuen = sum(Spezies_Count),
      Gesamt_Spezies = n_distinct(species),
      .groups = "drop"
    )
  
  
  # Mergen und berechnen der fehlenden Spalte
  species_site_year <- species_site_year |>
    left_join(all_site_year, by = c("locality", "year")) |>
    mutate(Spezies_Anteil =
             round((Spezies_Count / Gesamt_Individuen) * 100, 2))
  
  
  check_share <- species_site_year %>%
    group_by(locality, year) %>%
    summarise(sum_share = sum(Spezies_Anteil, na.rm = TRUE),
              .groups = "drop")

  
  
  # WIP Hier können dann die Gewässerdaten hinzugefügt werden
  df <- add_water_data(df = species_site_year)
  
  
  df <- df %>%
    select(-Messstelle_Jahr) %>%
    rename(
      Messstelle_ID = locality,
      `Ammonium [mg/L]`             = Ammonium,
      `Gelöster Sauerstoff [mg/L]`  = `Dissolved oxygen`,
      `Nitrat [mg/L]`               = Nitrate,
      `Phosphat [mg/L]`             = Phosphate,
      `Wassertemperatur [°C]`       = `Water temperature`,
      `Gesamtphosphor [mg/L]`       = `Total phosphorus`,
      `Distanz Messstelle [m]`      = water_dist_m
    )
  
  out_dir  <- "result_data/gewässer"
  out_file <- file.path(out_dir, "Süßwasserfische.csv")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  
  # Placeholder wegen codap zeilen limit
  #df <- df %>% slice_sample(n = 4900)
  
  df <- df %>%
    filter(`Distanz Messstelle [m]` < 1500)
  
  write.csv(df, out_file, row.names = FALSE)
  #DataExplorer::create_report(df)
  

  
  
  
  # Datensätze die man später evtl benutzen könnte
  
  # see_df <- fish_reduced %>%
  #   filter(habitat == "See")
  # 
  # 
  # fliess_df <- fish_reduced %>%
  #   filter(habitat == "Fliessgewaesser")
  
  
  
  
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
