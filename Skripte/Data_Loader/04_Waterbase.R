suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tidyr)
})


download_and_prepare_waterbasedata <- function() {
  url <- "https://sdi.eea.europa.eu/datashare/s/3JiTia3qePyGxyA/download?path=%2F&files=Waterbase_v2024_1_WISE6.sqlite"

  dest_dir  <- "raw_data/Gewässer"
  dest_file <- file.path(dest_dir, "Waterbase_v2024_1_WISE6.sqlite")

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  download.file(
    url      = url,
    destfile = dest_file,
    method   = "curl",
    extra    = "-L"
  )
  
  
  con <- dbConnect(RSQLite::SQLite(), dest_file)
  
  
  
  
  params <- c(
    "Water temperature",
    "pH",
    "Dissolved oxygen",
    "Nitrate",
    "Ammonium",
    "Total phosphorus",
    "Phosphate"
  )
  
  params_sql <- paste(sprintf("'%s'", params), collapse = ",")
  
  q <- paste0("
    WITH spatial_unique AS (
      SELECT
        countryCode,
        monitoringSiteIdentifier,
        monitoringSiteIdentifierScheme,
        MAX(lat) AS lat,
        MAX(lon) AS lon
      FROM S_WISE6_SpatialObject_DerivedData
      WHERE countryCode = 'DE'
      GROUP BY countryCode, monitoringSiteIdentifier, monitoringSiteIdentifierScheme
    )
    SELECT
      s.lat  AS LAT,
      s.lon  AS LON,
      a.phenomenonTimeReferenceYear AS year,
      a.observedPropertyDeterminandLabel AS parameter,
      a.resultMeanValue AS value,
      a.resultUom AS unit,
      a.monitoringSiteIdentifier AS site_id,
      a.resultNumberOfSamples AS n_samples
    FROM T_WISE6_AggregatedData a
    JOIN spatial_unique s
      ON a.countryCode = s.countryCode
     AND a.monitoringSiteIdentifier = s.monitoringSiteIdentifier
     AND a.monitoringSiteIdentifierScheme = s.monitoringSiteIdentifierScheme
    WHERE a.countryCode = 'DE'
      AND a.observedPropertyDeterminandLabel IN (", params_sql, ")
      AND a.resultMeanValue IS NOT NULL
      AND s.lat IS NOT NULL
      AND s.lon IS NOT NULL
      AND a.phenomenonTimeReferenceYear BETWEEN 1985 AND 2020
  ")
  
  df <- dbGetQuery(con, q)
  
  
  # Kleiner Check zum schaun ob man units umrechnen muss
  unit_checker <- with(df, table(parameter, unit))
  
  df_wide <- df |>
    select(site_id, year, LAT, LON, parameter, value) |>
    group_by(site_id, year, LAT, LON, parameter) |>
    summarise(value = round(mean(value, na.rm = TRUE), 2), .groups = "drop") |>
    pivot_wider(
      names_from  = parameter,
      values_from = value,
      values_fill = NA_real_
    )
  
  
  na_by_param_pct <- df_wide |>
    summarise(across(
      -c(site_id, year, LAT, LON),
      ~ mean(is.na(.x)) * 100
    )) |>
    pivot_longer(everything(), names_to = "parameter", values_to = "pct_na")
  

  
  
  # Wieviele Messtellen haben 7/7 Messwerte
  df_wide_checker <- df_wide |>
    mutate(
      n_params_present = rowSums(!is.na(across(c(
        "Ammonium","Dissolved oxygen","Nitrate","Phosphate",
        "Water temperature","Total phosphorus","pH"
      ))))
    )
  
  
  value_checker <- table(df_wide_checker$n_params_present)

  
  # Zielordner und -datei
  out_dir  <- "raw_data/gewässer"
  out_file <- file.path(out_dir, "waterbase.csv")
  
  # Ordner anlegen (falls noch nicht vorhanden)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  df_wide <- df_wide %>%
    rename(Longitude = LON, 
           Latitude = LAT) 
  
  # Speichern
  write.csv(df_wide, out_file, row.names = FALSE)
  
  
  dbDisconnect(con)
  
}





download_and_prepare_waterbasedata()