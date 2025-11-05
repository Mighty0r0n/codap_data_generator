suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(tidyr)
  library(stringr)
})
source("Skripte/utils_data_description.R")

generate_eunis_data <- function() {
  # Hier wird sich die Grund-Datei geholt
  #survey_df <- merge_re_survey_with_dwd_grids()
  
  survey_df <- read_csv("tmp_data/ReSurveyGermany/re_survey_germany_filtered_years.csv")
  
  #-----------------------------------------------------------------------------
  
  eunis_code_df <- survey_df %>% filter(EUNIS == "T18")
  
  eunis_code_df <- eunis_code_df %>%
    mutate(across(
      matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"),
      ~ replace_na(.x, 0)
    ))
  
  # T18 hätte eine angenehme Datenmenge, T17 hat einige Datenpunkte, da müsste ich viel wegfiltern
  # T19 hat fast keine datenpunkte
  # Für T18 wird hier erstmal nichts gefiltert
  
  
  # eunis_code_df <- filter_low_density_plots(
  #   df = eunis_code_df,
  #   n_rows_threshold = 30,
  #   presence_percent_threshold = 1
  # )
  #
  
  
  # Threshold für presence/absence nach https://doi.org/10.5281/zenodo.16895007 auf 1% gesetzt
  eunis_code_df <- eunis_code_df %>%
    mutate(Presence = if_else(Cover_Perc >= 1, 1, 0))
  
  predictors <- c("TEMPERATURE", "COV_LITTER", "COV_TREES", "COV_SHRUBS")
  
  
  # Hier werden die zu überprüfenden Spalten ausgewählt
  cov_vars <- eunis_code_df %>%
    select(predictors)
  
  # Korrelationen überprüfen
  cor_matrix <- cor(cov_vars, use = "complete.obs")
  
  
  eunis_lm <- glm(
    Presence ~ TEMPERATURE +
      COV_TREES + COV_SHRUBS + COV_LITTER,
    data = eunis_code_df,
    family = binomial(link = "logit"),
    control = glm.control(maxit = 100)
  )
  
  # Für jedes Scenario ein Datensatz für die Vorhersage durchs lm erzeugen
  scenario15 <- eunis_code_df
  scenario15$TEMPERATURE <- scenario15$TEMPERATURE + 1.5
  
  # Hier werden die predictions angehängt
  eunis_code_df$presence15 <- ifelse(predict(eunis_lm, newdata = scenario15, type = "response") >= 0.5,
                                     1,
                                     0)
  
  
  
  scenarios <- c("presence15")#, "presence2", "presence3", "presence4")
  
  
  # Kleiner Helper um die Schrittweisen "gains" und "losses" durch die
  # temp erhöhung zu betrachten
  check_df <- lapply(scenarios, function(scn) {
    data.frame(
      scenario = scn,
      gain  = sum(eunis_code_df$Presence == 0 &
                    eunis_code_df[[scn]] == 1),
      loss  = sum(eunis_code_df$Presence == 1 &
                    eunis_code_df[[scn]] == 0),
      stay_present = sum(eunis_code_df$Presence == 1 &
                           eunis_code_df[[scn]] == 1),
      stay_absent  = sum(eunis_code_df$Presence == 0 &
                           eunis_code_df[[scn]] == 0)
    )
  }) %>%
    bind_rows()
  
  scenarios <- c("Presence", "presence15")#, "presence2", "presence3", "presence4")
  
  
  # Hier wird anhand der Presence Spalte dann die Artenzahl pro RS_PLOT berechnet.
  # Mit n_distinct wird jede Art pro Plot nur einmal gezählt
  for (scn in scenarios) {
    new_col <- paste0("Artenzahl_", scn)
    
    eunis_code_df <- eunis_code_df %>%
      group_by(RS_PLOT) %>%
      mutate(!!new_col := n_distinct(TaxonName[.data[[scn]] == 1])) %>%
      ungroup()
  }
  
  # Nochmal kleines Aufäumen, hier wird das finale DF mit den zu nutzenden Spalten für codap erzeugt
  eunis_code_df <- eunis_code_df %>%
    mutate(TEMPERATURE15 = TEMPERATURE + 1.5) %>%
    select(
      TaxonName,
      RS_SITE,
      RS_PLOT,
      RELEVE_NR.x,
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
  tmp_dir <- path("tmp_data", "EUNIS")
  dir_create(tmp_dir, recurse = TRUE)
  tmp_data_file <- path(tmp_dir, "EUNIS_CODE.csv")
  
  
  # Dieser Datensatz ist schonmal in CODAP kopierbar.
  write.csv(eunis_code_df, file = tmp_data_file, row.names = FALSE)
  
  invisible(TRUE)
}


generate_eunis_data()