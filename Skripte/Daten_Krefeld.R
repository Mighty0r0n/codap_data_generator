suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(sf)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(R2jags)
  library(terra)
})
source("Skripte/utils_data_description.R")

add_hyras_season_temp <- function(
    df,
    hyras_dir,
    column_name,
    lon_col = "LONGITUDE",
    lat_col = "LATITUDE",
    year_col = "YEAR",
    season_col = "Jahreszeit"
) {
  df_out <- df
  df_out[[column_name]] <- NA_real_
  
  years <- sort(unique(df_out[[year_col]]))
  
  for (yr in years) {
    message("Jahr: ", yr)
    
    idx <- which(df_out[[year_col]] == yr)
    df_year <- df_out[idx, , drop = FALSE]
    
    nc_file <- path(hyras_dir, sprintf("tas_hyras_1_%d_v6-1_de.nc", yr))
    
    
    r <- rast(nc_file)
    
    pts <- vect(
      df_year,
      geom = c(lon_col, lat_col),
      crs = "EPSG:4326"
    )
    
    pts_proj <- project(pts, crs(r))

    vals <- extract(r, pts_proj, ID = FALSE)

    dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", yr)),
      by = "day",
      length.out = ncol(vals)
    )
    months <- as.integer(format(dates, "%m"))
    
    res <- rep(NA_real_, nrow(df_year))
    
    for (i in seq_len(nrow(df_year))) {
      season <- df_year[[season_col]][i]
      
      layer_idx <- switch(
        season,
        "Frühling" = which(months %in% c(3, 4, 5)),
        "Sommer"   = which(months %in% c(6, 7, 8)),
        "Herbst"   = which(months %in% c(9, 10, 11)),
        "Winter"   = which(months %in% c(12, 1, 2)),
        integer(0)
      )
      
      if (length(layer_idx) > 0) {
        res[i] <- mean(as.numeric(vals[i, layer_idx]), na.rm = TRUE)
      }
    }
    
    df_out[[column_name]][idx] <- res
  }
  
  return(df_out)
}

add_hyras_by_daynr <- function(
    df,
    hyras_dir,
    column_name = "tas_hyras",
    lon_col = "LONGITUDE",
    lat_col = "LATITUDE",
    year_col = "year",
    daynr_col = "daynr"
) {

  df_out <- df
  df_out[[column_name]] <- NA_real_
  
  years <- sort(unique(df_out[[year_col]]))
  
  for (yr in years) {
    message("Jahr: ", yr)
    
    idx <- which(df_out[[year_col]] == yr)
    df_year <- df_out[idx, , drop = FALSE]
    
    nc_file <- path(hyras_dir, sprintf("tas_hyras_1_%d_v6-1_de.nc", yr))
  
    
    r <- rast(nc_file)
    
    pts <- vect(
      df_year,
      geom = c(lon_col, lat_col),
      crs = "EPSG:4326"
    )
    
    pts_proj <- terra::project(pts, crs(r))
    
    vals <- terra::extract(r, pts_proj, ID = FALSE)
    
    layer_idx <- df_year[[daynr_col]]

    layer_idx[layer_idx < 1 | layer_idx > ncol(vals)] <- NA
    
    res <- vals[cbind(seq_len(nrow(vals)), layer_idx)]
    
    df_out[[column_name]][idx] <- res
  }
  
  return(df_out)
}


generate_krefeld_data <- function() {
  data <- read_csv("raw_data/Krefeld/krefeld1.csv")
  frame <- read_csv("raw_data/Krefeld/krefeld2.csv")
  
  
  frame <- frame %>%
    mutate(plot_id = as.numeric(factor(plot)))
  

  
  coords <- data %>%
    distinct(plot, E, N)
  
  
  frame <- frame %>%
    left_join(
      data %>%
        distinct(plot, E, N) |>
        rename(LONGITUDE = E, LATITUDE = N),
      by = "plot"
    )
  
  
  frame <- add_hyras_by_daynr(
    frame,
    hyras_dir = "raw_data/hyras_tas_v6_1",
    column_name = "temp_daily"
  ) %>%
    mutate(
      temp_diff = temp_daily - temperature
    )
  
  data <- data %>%
    mutate(
      loctype_id = as.numeric(factor(location.type)),
      Fangdauer = to.daynr - from.daynr,
      Jahreszeit = case_when(
        mean.daynr >= 60  &
          mean.daynr < 152 ~ "Frühling",
        mean.daynr >= 152 &
          mean.daynr < 244 ~ "Sommer",
        mean.daynr >= 244 &
          mean.daynr < 335 ~ "Herbst",
        TRUE ~ "Winter"
      )
    ) %>%
    filter(Jahreszeit != "Winter")
  
  

  krefeld_plot_year_season <- data %>%
    group_by(plot, year, Jahreszeit, potID) %>%
    summarise(
      biomass = median(biomass, na.rm = TRUE),
      LONGITUDE = mean(E),
      LATITUDE = mean(N),
      StartDatum = first(from),
      .groups = "drop",
      Fangdauer = first(Fangdauer),
      location.type = first(location.type)
    )
  
  
  frame_season <- frame %>%
    mutate(
      Jahreszeit = case_when(
        daynr >= 60  & daynr < 152 ~ "Frühling",
        daynr >= 152 & daynr < 244 ~ "Sommer",
        daynr >= 244 & daynr < 335 ~ "Herbst",
        TRUE ~ "Winter"
      )
    ) %>%
    filter(Jahreszeit != "Winter")
  
  
  krefeld_features_season <- frame_season %>%
    group_by(plot, year, Jahreszeit, potID) %>%
    summarise(
      median_Temperatur = median(temperature, na.rm = TRUE),
      sum_Niederschlag = sum(precipitation, na.rm = TRUE),
      median_Windgeschwindigkeit = median(wind.speed, na.rm = TRUE),
      Frosttage = max(frostdays),
      median_sum_precW = median(sum.precW, na.rm = TRUE),
      median_nKräuter = median(nHerbs, na.rm = TRUE),
      median_nBäume = median(nTrees, na.rm = TRUE),
      median_Stickstoff = median(Nitrogen, na.rm = TRUE),
      median_pH = median(pH, na.rm = TRUE),
      median_Bodenfeuchte = median(Moisture, na.rm = TRUE),
      median_Licht = median(Light, na.rm = TRUE),
      median_ellenTemperatur = median(ellenTemperature, na.rm = TRUE),
      Ackerland = median(Arable.land, na.rm = TRUE),
      Wald = median(Forest, na.rm = TRUE),
      Grünland = median(Grassland, na.rm = TRUE),
      Wasser = median(Water, na.rm = TRUE),
      temp_fangdauer = median(temp_daily),
      .groups = "drop"
    ) %>%
    filter(Jahreszeit != "Winter")
  
  temp_fang_saison <- frame_season %>%
    group_by(plot, year, Jahreszeit) %>%
    summarise(
      Saison_Jahresmittel_Krefeld = mean(temp_daily, na.rm = TRUE),
      .groups = "drop"
    )
  
  krefeld_features_season <- krefeld_features_season %>%
    left_join(temp_fang_saison, by = c("plot", "year", "Jahreszeit"))
  
  krefeld_final <- krefeld_plot_year_season %>%
    left_join(krefeld_features_season,
              by = c("plot", "year", "Jahreszeit", "potID")) %>%
    mutate(
      StartDatum = as.Date(paste(StartDatum, year, sep = "-"), format = "%d-%m-%Y"),
      #StartDatum = format(StartDatum, "%d.%m.%Y"),
      max_land = pmax(Ackerland, Grünland, Wald),
      Landnutzung = case_when(
        max_land < 0.2 ~ "gemischt",
        Ackerland == max_land ~ "Ackerland",
        Grünland == max_land ~ "Grünland",
        Wald == max_land ~ "Wald"
      ),
      temp_diff = temp_fangdauer - median_Temperatur
    ) %>%
    filter(!is.na(biomass)) %>%
    select(-max_land) %>%
    rename(YEAR = year)
  
  
  krefeld_final <- add_hyras_season_temp(
    df = krefeld_final,
    hyras_dir = "raw_data/hyras_tas_v6_1",
    column_name = "Saison_Jahresmittel_HYRAS"
  ) %>%
    mutate(
      saison_temp_diff = Saison_Jahresmittel_HYRAS - Saison_Jahresmittel_Krefeld,
      location.type.label = case_when(
        location.type == 1 ~ "nährstoffarm (Heide/Sand/Dünen)",
        location.type == 2 ~ "nährstoffreich (Grünland/Randflächen)",
        location.type == 3 ~ "Pionier-/Strauchgesellschaft"
      )
    ) 

  
  write_csv(krefeld_final, "Krefeld_new.csv")
  
  
}


generate_krefeld_data()