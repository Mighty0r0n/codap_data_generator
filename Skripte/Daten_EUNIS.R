suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(tidyr)
  library(stringr)
  library(tibble)
})
source("Skripte/utils_data_description.R")

generate_eunis_data <- function(eunis_code_list, write_tmp_file) {
  # Hier wird sich die Grund-Datei geholt
  #survey_df <- merge_re_survey_with_dwd_grids()
  
  survey_df <- read_csv("tmp_data/ReSurveyGermany/re_survey_germany_filtered_years.csv")
  
  combined_eunis_df = tibble()
  
  #-----------------------------------------------------------------------------
  for (eunis_code in eunis_code_list){
    eunis_code_df <- survey_df %>% filter(EUNIS == eunis_code)
    
    
    # Manche Eunis-codes existieren nicht in ReSurveyGermany
    if (length(eunis_code_df) == 0) {
      return(cat("File does not contain any entries"))
    }
    
    eunis_code_df <- eunis_code_df %>%
      mutate(across(
        matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"),
        ~ replace_na(.x, 0)
      ))
    
    # T18 hätte eine angenehme Datenmenge, T17 hat einige Datenpunkte, da müsste ich viel wegfiltern
    # T19 hat fast keine datenpunkte- weg
    # Für T18 wird hier erstmal nichts gefiltert
    
    feature_list <- c(
      "TEMPERATURE",
      "PRECIPITATION",
      "COV_HERBS",
      "COV_LITTER",
      "COV_MOSSES",
      "COV_TREES",
      "COV_SHRUBS"
                      )
    
    eunis_code_df <- add_glm_predictions(
      df = eunis_code_df,
      feature_list = feature_list
    )

    
    # Ich nehme erstmal die ersten paar Einträge jeder Datei
    # Die Zahl ist so gewählt, dass die enddatei etwas knapp unter 5000 Einträgen bleibt.
    eunis_code_df <- head(eunis_code_df, 1300) %>%
      mutate(TEMPERATURE15 = TEMPERATURE + 1.5)
    
    
    
    eunis_code_df <- add_growth_form(df = eunis_code_df)
    
    # Speichern des Datensatzes
    tmp_dir <- path("tmp_data", "EUNIS")
    dir_create(tmp_dir, recurse = TRUE)
    
    eunis_file_name <- paste0(eunis_code, "_EUNIS_CODE.csv")
    tmp_data_file <- path(tmp_dir, eunis_file_name)
    message("Anzahl Einträge: ", nrow(eunis_code_df), " ", eunis_code)
    
    combined_eunis_df <- bind_rows(combined_eunis_df, eunis_code_df)
    
    if (write_tmp_file) {
    
    # Dieser Datensatz ist schonmal in CODAP kopierbar.
    write.csv(eunis_code_df, file = tmp_data_file, row.names = FALSE)
    }
  }
  
  
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
      COV_HERBS,
      COV_LITTER,
      COV_MOSSES,
      TEMPERATURE,
      TEMPERATURE15,
      PRECIPITATION,
      Presence,
      presence15,
      Artenzahl_Presence,
      Artenzahl_presence15
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

# Anzahl Einträge: 14438
# Anzahl Einträge: 1660
# Anzahl Einträge: 17776
# Anzahl Einträge: 517
# Anzahl Einträge: 426
