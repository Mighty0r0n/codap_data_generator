suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(sf)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(BIEN)
  library(mvabund)
})
source("Skripte/utils_data_description.R")

generate_eunis_data <- function(eunis_code_list, write_tmp_file) {
  # Hier wird sich die Grund-Datei geholt
  survey_df <- merge_re_survey_with_dwd_grids()
  
  # Debugzeile
  #survey_df <- read_csv("tmp_data/ReSurveyGermany/re_survey_germany_filtered_years.csv")
  
  
  ######################################################################################
  # --------------------------------------SOIL------------------------------------------
  
  # 1. Vorbereiten der Bodendaten
  soil_df <- read_csv2("raw_data/Bodendaten/Bodendaten_Abfrage_UBA.csv") %>%
    filter(
      `Bodenmesswert - Oberkante [cm]` > 0,
      `Bodenmesswert - Unterkante [cm]` <= 30,
      stringr::str_count(
        as.character(`Bodenmesswert - Messwert`),
        ","  # Komische Einträge entfernen
      ) <= 1
    ) %>%
    select(-`Messung - Probenahmedatum`) %>%
    mutate(
      `Bodenmesswert - Messwert` = as.numeric(sub(",", ".", `Bodenmesswert - Messwert`)),
      # Einfacher Datatype cast
      `Parameter - Messgröße` = ifelse(
        # angleichen der pH-Spalte
        `Parameter - Messgröße` == "pH",
        "pH-Wert",
        `Parameter - Messgröße`
      ),
      `Parameter - Einheit` = ifelse(
        `Parameter - Messgröße` == "pH-Wert",
        "ohne",
        `Parameter - Einheit`
      ),
      `Bodenmesswert - Messwert` = dplyr::case_when(
        `Parameter - Messgröße` == "pH-Wert" &
          `Bodenmesswert - Messwert` > 14 ~ `Bodenmesswert - Messwert` / 100,
        TRUE ~ `Bodenmesswert - Messwert`
      )
    ) %>%
    group_by(`Messstellennummer`, `Parameter - Messgröße`) %>%
    summarise(
      # Gruppieren nach Messstellennummer und Messgröße
      value = median(`Bodenmesswert - Messwert`, na.rm = TRUE),
      latitude  = first(Latitude),
      longitude = first(Longitude),
      n = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      # long to wide formatierung
      id_cols = c(`Messstellennummer`, latitude, longitude),
      names_from  = `Parameter - Messgröße`,
      values_from = value
    ) %>% # komische Formatierungen entfernen
    mutate(`pH-Wert` = ifelse(`pH-Wert` <= 0 |
                                `pH-Wert` > 14, NA, `pH-Wert`))
  
  
  # Nur ein kleiner check um zu sehen, wieviele Parameter pro Messstelle bemessen worden sind
  coverage <- soil_df %>%
    summarise(across(
      -c(`Messstellennummer`, latitude, longitude),
      ~ mean(!is.na(.))
    )) %>%
    pivot_longer(everything(), names_to = "messgroesse", values_to = "share_present") %>%
    arrange(share_present)
  
  #####################################################################################
  # 2. Survey Data vorbereiten
  
  survey_df <- survey_df %>% filter(EUNIS %in% eunis_code_list)
  
  survey_df <- survey_df %>%
    mutate(
      Taxon_clean = TaxonName |>
        # alles ab " agg.", " sect.", " x", " ×" wegschneiden
        gsub(" agg\\..*$", "", x = _) |>
        gsub(" sect\\..*$", "", x = _) |>
        gsub(" x .*$", "", x = _)     |>
        gsub(" × .*$", "", x = _)     |>
        gsub(" x .*$", "", x = _)  |>
        gsub(" ×.*$", "", x = _)   |>
        trimws()
    ) %>%
    filter(!is.na(Taxon_clean),
           grepl("^[A-Za-z]{2,}\\s+[A-Za-z]{2,}", Taxon_clean))
  survey_df <- survey_df %>%
    mutate(across(
      matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"),
      ~ replace_na(.x, 0)
    ))
  
  #####################################################################################
  # 3. Modell befüllen
  
  # Presence/Absence Matrix
  survey_pa_long <- survey_df %>%
    mutate(Presence = if_else(Cover_Perc >= 0.1, 1L, 0L)) %>%
    group_by(RS_PLOT, YEAR, Taxon_clean) %>%
    summarise(Presence = as.integer(any(Presence == 1L)), .groups = "drop")
  
  # Matrix drehen um die Taxa in die Spalten zu kriegen
  Y <- survey_pa_long %>%
    select(RS_PLOT, YEAR, Taxon_clean, Presence) %>%
    pivot_wider(
      names_from  = Taxon_clean,
      values_from = Presence,
      values_fill = 0L
    )
  
  
  # Unsere Features
  env_df <- survey_df %>%
    group_by(RS_PLOT, YEAR) %>%
    summarise(
      TEMPERATURE       = first(TEMPERATURE),
      PRECIPITATION     = first(PRECIPITATION),
      COV_TOTAL         = mean(COV_TOTAL),
      EUNIS             = first(EUNIS),
      .groups = "drop"
    ) %>%
    mutate(EUNIS = factor(EUNIS))
  
  # Features und P/A matrix kombiniert
  glm_data <- Y %>%
    left_join(env_df, by = c("RS_PLOT", "YEAR"))
  
  
  # für das manyglm passende format
  species_mat <- glm_data %>%
    select(-RS_PLOT,-YEAR,-TEMPERATURE,-PRECIPITATION,-COV_TOTAL,-EUNIS) %>%
    as.matrix()
  
  # manyglm format werden nun die prediktoren hinzugefügt
  Y_mv <- mvabund(species_mat)
  
  fit_full <- manyglm(
    Y_mv ~ TEMPERATURE + PRECIPITATION + EUNIS + COV_TOTAL,
    data   = glm_data,
    family = "binomial"
  )
  
  
  # Hier wird eine Prediction Matrix erzeugt
  newdata_plus15 <- glm_data %>%
    mutate(TEMPERATURE = TEMPERATURE + 1.5)
  
  #####################################################################################
  # 4. Predictions
  pred_plus15 <- predict(fit_full, newdata = newdata_plus15, type    = "response")
  
  
  pred_df <- pred_plus15 %>%
    as.data.frame() %>%
    {
      setNames(., gsub("\\.", " ", names(.)))
    } %>%
    mutate(RS_PLOT      = glm_data$RS_PLOT, YEAR         = glm_data$YEAR) %>%
    pivot_longer(
      cols = -c(RS_PLOT, YEAR),
      names_to = "Taxon_clean",
      values_to = "Pred_prob_plus15"
    ) %>%
    mutate(Presence15 = as.integer(Pred_prob_plus15 >= 0.5)) %>%
    select(-Pred_prob_plus15)
  
  
  
  survey_df_pred <- survey_df %>%
    left_join(
      pred_df %>%
        select(RS_PLOT, YEAR, Taxon_clean, Presence15),
      by = c("RS_PLOT", "YEAR", "Taxon_clean")
    ) %>%
    mutate(Presence = as.integer(Cover_Perc >= 0.1))  %>%
    drop_na(Presence15)
  
  survey_df_pred <- add_growth_form(df = survey_df_pred)
  
  combined_eunis_df = tibble()
  
  #-----------------------------------------------------------------------------
  for (eunis_code in c("R22")) {
    eunis_code_df <- survey_df_pred %>%
      filter(EUNIS == eunis_code)
    
    
    
    # Manche Eunis-codes existieren nicht in ReSurveyGermany. Kleiner Fallback falls weitere eunis codes dazukommen sollten
    if (length(eunis_code_df) == 0) {
      return(cat("File does not contain any entries for this EUNIS-Code"))
    }
    
    
    scenarios <- c("Presence", "Presence15")#, "presence2", "presence3", "presence4")
    
    eunis_code_df <- eunis_code_df %>%
      mutate(obs_id = paste(RS_PROJECT, RELEVE_NR.y, sep = ":"))
    
    for (scn in scenarios) {
      new_col <- paste0("Artenzahl_", scn)
      
      
      monitor_counts <- eunis_code_df %>%
        group_by(RS_PLOT, YEAR, obs_id) %>%
        summarise(Artenzahl_monitor = n_distinct(TaxonName[.data[[scn]] == 1]),
                  .groups = "drop")
      
      
      year_mean <- monitor_counts %>%
        group_by(RS_PLOT, YEAR) %>%
        summarise(!!new_col := mean(Artenzahl_monitor), .groups = "drop")
      
      
      eunis_code_df <- eunis_code_df %>%
        left_join(year_mean, by = c("RS_PLOT", "YEAR"))
    }
    
    eunis_code_df <- eunis_code_df %>% select(-obs_id)
    
    
    # T18 hätte eine angenehme Datenmenge, T17 hat einige Datenpunkte, da müsste ich viel wegfiltern
    # T19 hat fast keine datenpunkte- weg
    # Für T18 wird hier erstmal nichts gefiltert
    
    
    tmp_dir <- path("tmp_data", "EUNIS")
    dir_create(tmp_dir, recurse = TRUE)
    
    eunis_file_name <- paste0(eunis_code, "_EUNIS_CODE.csv")
    tmp_data_file <- path(tmp_dir, eunis_file_name)
    message("Anzahl Einträge: ", nrow(eunis_code_df), " ", eunis_code)
    
    
    if (write_tmp_file) {
      # Dieser Datensatz ist schonmal in CODAP kopierbar.
      write.csv(head(eunis_code_df, 4900),
                file = tmp_data_file,
                row.names = FALSE)
    }
    
    # Ich nehme erstmal die ersten paar Einträge jeder Datei
    # Die Zahl ist so gewählt, dass die enddatei etwas knapp unter 5000 Einträgen bleibt.
    
    
    #eunis_code_df <- head(eunis_code_df, 1300) %>%
    
    eunis_code_df <- eunis_code_df %>%
      mutate(TEMPERATURE15 = TEMPERATURE + 1.5)
    
    combined_eunis_df <- bind_rows(combined_eunis_df, eunis_code_df)
  }
  
  
  combined_eunis_df <- combined_eunis_df %>%
    mutate(
      Layer = case_when(
        LAYER == 1 ~ "Baumschicht (oberste)",
        LAYER == 2 ~ "Baumschicht (mittlere)",
        LAYER == 3 ~ "Baumschicht (unterste)",
        LAYER == 4 ~ "Strauchschicht (oberste)",
        LAYER == 5 ~ "Strauchschicht (untere)",
        LAYER == 6 ~ "Krautschicht",
        LAYER %in% c(7, 8) ~ "Jungpflanzen",
        LAYER == 9 ~ "Moosschicht",
        TRUE ~ NA_character_
      )
    )
  
  
  # Nochmal kleines Aufäumen, hier wird das finale DF mit den zu nutzenden Spalten für codap erzeugt
  combined_eunis_df <- combined_eunis_df %>%
    select(
      TaxonName,
      RS_SITE,
      RS_PLOT,
      RS_PROJECT,
      RELEVE_NR.x,
      EUNIS,
      LONGITUDE,
      LATITUDE,
      YEAR,
      LAYER,
      Layer,
      Wuchsform,
      COV_TREES,
      COV_SHRUBS,
      COV_HERBS,
      COV_LITTER,
      COV_MOSSES,
      COV_ROCK,
      TEMPERATURE,
      TEMPERATURE15,
      PRECIPITATION,
      Presence,
      Presence15,
      Artenzahl_Presence,
      Artenzahl_Presence15
    )
  
  
  
  
  soil_df <- soil_df %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    mutate(
      latitude  = as.numeric(latitude),
      longitude = as.numeric(longitude),
      
      lat_1e6 = latitude / 1e6,
      lat_1e7 = latitude / 1e7,
      lat_1e8 = latitude / 1e8,
      lon_1e6 = longitude / 1e6,
      lon_1e7 = longitude / 1e7,
      lon_1e8 = longitude / 1e8,
      
      latitude = dplyr::case_when(
        latitude >= 45 & latitude <= 55 ~ latitude,
        # schon Grad
        lat_1e8  >= 45 & lat_1e8  <= 55 ~ lat_1e8,
        lat_1e7  >= 45 & lat_1e7  <= 55 ~ lat_1e7,
        lat_1e6  >= 45 & lat_1e6  <= 55 ~ lat_1e6,
        TRUE ~ NA_real_
      ),
      
      longitude = dplyr::case_when(
        longitude >= 5 & longitude <= 16 ~ longitude,
        # schon Grad
        lon_1e8   >= 5 & lon_1e8   <= 16 ~ lon_1e8,
        lon_1e7   >= 5 & lon_1e7   <= 16 ~ lon_1e7,
        lon_1e6   >= 5 & lon_1e6   <= 16 ~ lon_1e6,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-lat_1e6, -lat_1e7, -lat_1e8, -lon_1e6, -lon_1e7, -lon_1e8) %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  
  
  soil_sf <- soil_df %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )
  
  eunis_sf <- combined_eunis_df %>%
    st_as_sf(
      coords = c("LONGITUDE", "LATITUDE"),
      crs = 4326,
      remove = FALSE
    )
  
  
  soil_m  <- st_transform(soil_sf, 3035)
  eunis_m <- st_transform(eunis_sf, 3035)
  
  nearest_idx <- st_nearest_feature(eunis_m, soil_m)
  
  dist_m <- st_distance(eunis_m, soil_m[nearest_idx, ], by_element = TRUE)
  dist_m <- as.numeric(dist_m)  # units -> numeric
  
  
  soil_attr <- soil_m %>%
    st_drop_geometry() %>%
    mutate(soil_row_id = row_number())
  
  eunis_out <- eunis_m %>%
    st_drop_geometry() %>%
    mutate(soil_row_id = soil_attr$soil_row_id[nearest_idx],
           Entfernung_Messstelle = dist_m) %>%
    left_join(soil_attr, by = "soil_row_id")
  
  
  eunis_out <- eunis_out %>%
    filter(Entfernung_Messstelle < 5000) %>% # Erstmal filtere ich nach einer entfernung von 10km der Messstelle zur RS_Site, variabel anpassbar
    mutate(Releve_Nr = paste(RS_PROJECT, RELEVE_NR.x, sep = ":")) %>%
    select(-soil_row_id,-Messstellennummer,-latitude,-longitude,-RELEVE_NR.x) %>%
    relocate(Releve_Nr, .after = RS_PROJECT)
  
  # Speichern des Datensatzes
  result_dir <- path("result_data", "EUNIS")
  dir_create(result_dir, recurse = TRUE)
  result_data_file <- path(result_dir, "combined_eunis.csv")
  
  # Nur ein kleiner check um zu sehen, wieviele Parameter pro Messstelle bemessen worden sind
  coverage <- eunis_out %>%
    summarise(across(-c(`RS_SITE`, LATITUDE, LONGITUDE), ~ mean(!is.na(.)))) %>%
    pivot_longer(everything(), names_to = "messgroesse", values_to = "share_present") %>%
    arrange(share_present)
  
  # Für reproduzierbarkeit beim sampeln
  set.seed(42)
  
  # # Samplen fürs codap limit
  # eunis_out <- eunis_out[sample(nrow(eunis_out), 4950), ] %>%
  #   select(-`Stickstoff gesamt`, -`Phosphor gesamt`) # Bei Messstellenentfernung von unter 10km haben wir keine Messwerte für die Messgrößen
  
  # Dieser Datensatz ist schonmal in CODAP kopierbar.
  write.csv(eunis_out, file = result_data_file, row.names = FALSE)
  
  return(eunis_out)
}


# V13 hat leider keine Einträge in ReSurvey

eunis_code_list = c("T17", "T18", "R22", "V11", "V15")



# write_tmp_file = TRUE wenn einzeldatensätze zu den einzelnen EUNIS Flächen mitgeneriert werden sollen. Diese landen im tmp_data ordner
tmp_eunis_code_df = generate_eunis_data(eunis_code_list = eunis_code_list, write_tmp_file = TRUE)


# for (df in eunis_df_list) {
#   message("Anzahl Einträge: ", nrow(df))
# }

# Anzahl Einträge: 12813 T17
# Anzahl Einträge: 1595 T18
# Anzahl Einträge: 16748 R22
# Anzahl Einträge: 472 V11
# Anzahl Einträge: 380 V15