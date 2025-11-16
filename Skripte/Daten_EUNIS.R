suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(BIEN)
  library(mvabund)
})
source("Skripte/utils_data_description.R")

generate_eunis_data <- function(eunis_code_list, write_tmp_file) {
  # Hier wird sich die Grund-Datei geholt
  # survey_df <- merge_re_survey_with_dwd_grids()
  
  survey_df <- read_csv("tmp_data/ReSurveyGermany/re_survey_germany_filtered_years.csv")
  
  # Sind wenige Einträge und enthält wenig zeigbares
  survey_df <- survey_df %>% filter(LAYER != 0,
                                    EUNIS %in% eunis_code_list)
  
  survey_df <- survey_df %>% 
    mutate(Taxon_clean = TaxonName |> 
             # alles ab " agg.", " sect.", " x", " ×" wegschneiden
             gsub(" agg\\..*$", "", x = _) |>
             gsub(" sect\\..*$", "", x = _) |>
             gsub(" x .*$", "", x = _)     |>
             gsub(" × .*$", "", x = _)     |>
             gsub(" x .*$", "", x = _)  |>
             gsub(" ×.*$", "", x = _)   |>
             trimws()
    ) %>%
    filter(
      !is.na(Taxon_clean),
      grepl("^[A-Za-z]{2,}\\s+[A-Za-z]{2,}", Taxon_clean)
    )
  survey_df <- survey_df %>%
    mutate(across(
      matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"),
      ~ replace_na(.x, 0)
    ))
  
  
  
  # Presence/Absence Matrix
  survey_pa_long <- survey_df %>%
    mutate(
      Presence = if_else(Cover_Perc >= 0.1, 1L, 0L)  # Threshold kannst du bei Bedarf justieren
    ) %>%
    group_by(RS_PLOT, YEAR, Taxon_clean) %>%
    summarise(
      Presence = as.integer(any(Presence == 1L)),  # wenn Taxon irgendwo im Plot/Jahr ≥0.1%, dann 1
      .groups = "drop"
    )
  
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
    mutate(
      EUNIS = factor(EUNIS)
    )
  
  # Features und P/A matrix kombiniert
  glm_data <- Y %>%
    left_join(env_df, by = c("RS_PLOT", "YEAR"))
  
  
  # für das manyglm passende format
  species_mat <- glm_data %>%
    select(
      -RS_PLOT,
      -YEAR,
      -TEMPERATURE,
      -PRECIPITATION,
      -COV_TOTAL,
      -EUNIS
    ) %>%
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
  
  
  pred_plus15 <- predict(
    fit_full,
    newdata = newdata_plus15,
    type    = "response"
  )
  
  
  pred_df <- pred_plus15 %>%
    as.data.frame() %>%
    { setNames(., gsub("\\.", " ", names(.))) } %>%
    mutate(
      RS_PLOT = glm_data$RS_PLOT,
      YEAR    = glm_data$YEAR
    ) %>%
    pivot_longer(
      cols = -c(RS_PLOT, YEAR),
      names_to = "Taxon_clean",
      values_to = "Pred_prob_plus15"
    ) %>%
    mutate(
      Presence15 = as.integer(Pred_prob_plus15 >= 0.5)
    ) %>%
    select(-Pred_prob_plus15)
  
  
  survey_df_pred <- survey_df %>%
    left_join(
      pred_df %>%
        select(RS_PLOT, YEAR, Taxon_clean, Presence15), 
      by = c("RS_PLOT", "YEAR", "Taxon_clean")
    ) %>%
    mutate(
      Presence = as.integer(Cover_Perc >= 0.1)
    )  %>%
    drop_na(Presence15)
  
  survey_df_pred <- add_growth_form(df = survey_df_pred)
  
  combined_eunis_df = tibble()
  
  #-----------------------------------------------------------------------------
  # Hier werden einige Filterebenen auf den einzelnen Eunis code dfs durchgeführt um rechenzeit zu sparen.
  for (eunis_code in eunis_code_list){
    eunis_code_df <- survey_df_pred %>% 
      filter(EUNIS == eunis_code) 
    

     
    
    
    
    # Manche Eunis-codes existieren nicht in ReSurveyGermany. Kleiner Fallback falls weitere eunis codes dazukommen sollten
    if (length(eunis_code_df) == 0) {
      return(cat("File does not contain any entries"))
    }
    

    scenarios <- c("Presence", "Presence15")#, "presence2", "presence3", "presence4")
    
    
    # Hier wird anhand der Presence Spalte dann die Artenzahl pro RS_PLOT berechnet.
    # Mit n_distinct wird jede Art pro Plot nur einmal gezählt
    for (scn in scenarios) {
      new_col <- paste0("Artenzahl_", scn)
      
      eunis_code_df <- eunis_code_df %>%
        group_by(RS_PLOT) %>%
        mutate(!!new_col := n_distinct(TaxonName[.data[[scn]] == 1])) %>%
        ungroup()
    }
    
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
    write.csv(head(eunis_code_df, 4900), file = tmp_data_file, row.names = FALSE)
    }
    
    # Ich nehme erstmal die ersten paar Einträge jeder Datei
    # Die Zahl ist so gewählt, dass die enddatei etwas knapp unter 5000 Einträgen bleibt.
    eunis_code_df <- head(eunis_code_df, 1300) %>%
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
  
  # Speichern des Datensatzes
  result_dir <- path("result_data", "EUNIS")
  dir_create(result_dir, recurse = TRUE)
  result_data_file <- path(result_dir, "combined_eunis.csv")
  
  # Dieser Datensatz ist schonmal in CODAP kopierbar.
  write.csv(combined_eunis_df, file = result_data_file, row.names = FALSE)
  
  return(combined_eunis_df)
}


# V13 hat leider keine Einträge in ReSurvey

eunis_code_list = c("T17", "T18", "R22", "V11", "V15")

  

# write_tmp_file = TRUE wenn einzeldatensätze zu den einzelnen EUNIS Flächen mitgeneriert werden sollen. Diese landen im tmp_data ordner
tmp_eunis_code_df = generate_eunis_data(
  eunis_code = eunis_code_list,
  write_tmp_file = TRUE
  )


# for (df in eunis_df_list) {
#   message("Anzahl Einträge: ", nrow(df))
# }

# Anzahl Einträge: 12813 T17
# Anzahl Einträge: 1595 T18
# Anzahl Einträge: 16748 R22
# Anzahl Einträge: 472 V11
# Anzahl Einträge: 380 V15