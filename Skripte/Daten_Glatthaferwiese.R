suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(tidyr)
  library(stringr)
})
source("Skripte/utils_data_description.R")
# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion eingenen Gültigkeitsbereich um die "uniqueness" der Variable zu gewährleisten.
generate_oat_data <- function() {
  # Hier wird sich die Grund-Datei geholt
  #survey_df <- merge_re_survey_with_dwd_grids()
  
  survey_df <- read_csv("tmp_data/ReSurveyGermany/re_survey_germany_filtered_years.csv")
  
  #-----------------------------------------------------------------------------
  
  oat_df <- survey_df %>% filter(EUNIS == "R22")
  
  oat_df <- oat_df %>%
    mutate(across(
      matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"),
      ~ replace_na(.x, 0)
    ))
  
  # TODO: Wir verlieren sehr viele Datenpunkte wenn wir ALTITUDE und SLope einbeziehen wollen
  # NAs könnten nur mit absoluter Vorsicht imputed werden.
  # Nach den logs:
  
  #    column           type       n_na n_unique
  # 7 ALTITUDE         numeric     197      125
  # 8 SLOPE            numeric     276       67
  # TODO Man könnte offline Karten nutzen um diese parameter an den gegebenen koordinaten fest zu machen
  
  # NA is ALTITUDE und Slope, diese nicht auf 0 setzten. Diese Daten werden entfernt.
  #survey_df <- survey_df %>%
  # filter(!is.na(ALTITUDE), !is.na(SLOPE))
  
  
  # Alle Spezies die in Spalte 2-5 mit mind II gesehen worden sind
  oat_species <- c(
    "Galium mollugo agg.",
    "Arrhenatherum elatius",
    "Bromus hordeaceus",
    "Crepis biennis",
    #"Agropyron repens",
    #"Urtica dioica",
    #"Cirsium arvense",
    "Ranunculus repens",
    #"Rumex crispus",
    #"Agrostis stolonifera",
    "Daucus carota",
    "Pastinaca sativa",
    "Glechoma hederacea",
    "Alopecurus pratensis",
    "Agrostis tenuis",
    "Alchemilla vulgaris agg.",
    "Cynosurus cristatus",
    "Leontodon hispidus",
    "Knautia arvensis",
    "Colchicum autumnale",
    "Plantago media",
    "Campanula patula",
    "Bromus erectus",
    "Geranium pratense",
    "Silaum silaus",
    "Betonica officinalis",
    "Salvia pratensis",
    "Dactylis glomerata",
    "Achillea millefolium",
    "Leucanthemum vulgare",
    "Bellis perennis",
    "Trisetum flavescens",
    "Heracleum sphondylium",
    "Veronica chamaedrys",
    "Anthriscus sylvestris",
    "Vicia sepium",
    "Lotus corniculatus",
    "Pimpinella major",
    "Trifolium dubium",
    "Lolium perenne",
    "Tragopogon pratensis agg.",
    "Rhinanthus minor",
    #"Carum carvi",
    "Taraxacum officinale",
    "Rumex acetosa",
    "Ranunculus acris",
    "Trifolium pratense",
    "Holcus lanatus",
    "Cerastium holosteoides",
    "Festuca pratensis",
    "Trifolium repens",
    "Lathyrus pratensis",
    "Centaurea jacea",
    "Cardamine pratensis",
    "Ajuga reptans",
    "Vicia cracca",
    "Deschampsia cespitosa",
    "Avenochloa pubescens",
    "Lychnis flos-cuculi",
    "Prunella vulgaris",
    "Sanguisorba officinalis",
    "Filipendula ulmaria",
    #"Myosotis palustris agg.",
    "Festuca rubra agg.",
    "Plantago lanceolata",
    "Poa pratensis agg.",
    "Poa trivialis",
    "Anthoxanthum odoratum",
    "Luzula campestris",
    "Lysimachia nummularia",
    "Medicago lupulina",
    "Hypochoeris radicata",
    "Ranunculus bulbosus",
    "Veronica arvensis",
    "Saxifraga granulata",
    "Briza media",
    "Pimpinella saxifraga",
    "Sanguisorba minor"
    #"Plantago major",
    #"Convolvulus arvensis",
    #"Tripleurospermum inodorum",
    #"Tanacetum vulgare",
    #"Silene alba"
  )
  
  
  oat_df <- oat_df %>% filter(TaxonName %in% oat_species)
  
  
  # Datensatz verkleinern
  df_r22_filtered <- filter_low_density_plots(
    df = oat_df,
    n_rows_threshold = 22,
    presence_percent_threshold = 1
  )
  
  # Threshold für presence/absence nach https://doi.org/10.5281/zenodo.16895007 auf 1% gesetzt
  df_r22_filtered <- df_r22_filtered %>%
    mutate(Presence = if_else(Cover_Perc >= 1, 1, 0))
  
  
  
  #-----------------------------------------------------------------------------
  # Hier beginnt die Modellierung
  # Lose nach DOI:10.14471/2017.37.010 ausgewählt
  # Dort wurde aufgeführt das COV_LITTER und COV_ROCK unter umständen problematisch
  # für die Modellierung sein können, da sie oft stark miteinander korrelieren.
  # Dementsprechend wird das hier erstmal kurz überprüft
  predictors <- c(
    "TEMPERATURE",
    "PRECIPITATION",
    "COV_HERBS",
    "COV_LITTER",
    "COV_MOSSES",
    "LONGITUDE",
    "LATITUDE"
  )
  
  
  # Hier werden die zu überprüfenden Spalten ausgewählt
  cov_vars <- df_r22_filtered %>%
    select(predictors)
  
  # Korrelationen überprüfen
  cor_matrix <- cor(cov_vars, use = "complete.obs")
  
  
  oat_lm <- glm(
    Presence ~ TEMPERATURE + PRECIPITATION +
      COV_HERBS + COV_LITTER +
      COV_MOSSES + LONGITUDE + LATITUDE,
    data = df_r22_filtered,
    family = binomial(link = "logit"),
    control = glm.control(maxit = 100)
  )
  
  
  
  # Für jedes Scenario ein Datensatz für die Vorhersage durchs lm erzeugen
  scenario15 <- df_r22_filtered
  scenario15$TEMPERATURE <- scenario15$TEMPERATURE + 1.5
  
  
  # Kann man raus lassen, thermophiler effekt hier schon deutlich ab erhöhung von temp 1,5°C zu erkennen
  # scenario2 <- df_r22_filtered
  # scenario2$TEMPERATURE <- scenario2$TEMPERATURE + 2
  #
  # scenario3 <- df_r22_filtered
  # scenario3$TEMPERATURE <- scenario3$TEMPERATURE + 3
  #
  # scenario4 <- df_r22_filtered
  # scenario4$TEMPERATURE <- scenario4$TEMPERATURE + 4
  
  
  # Hier werden die predictions angehängt
  df_r22_filtered$presence15 <- ifelse(predict(oat_lm, newdata = scenario15, type = "response") >= 0.5,
                                       1,
                                       0)
  # df_r22_filtered$presence2 <- ifelse(
  #   predict(oat_lm, newdata = scenario2, type = "response") >= 0.5,
  #   1, 0
  # )
  # df_r22_filtered$presence3 <- ifelse(
  #   predict(oat_lm, newdata = scenario3, type = "response") >= 0.5,
  #   1, 0
  # )
  # df_r22_filtered$presence4 <- ifelse(
  #   predict(oat_lm, newdata = scenario4, type = "response") >= 0.5,
  #   1, 0
  # )
  
  
  scenarios <- c("presence15")#, "presence2", "presence3", "presence4")
  
  
  # Kleiner Helper um die Schrittweisen "gains" und "losses" durch die
  # temp erhöhung zu betrachten
  check_df <- lapply(scenarios, function(scn) {
    data.frame(
      scenario = scn,
      gain  = sum(df_r22_filtered$Presence == 0 &
                    df_r22_filtered[[scn]] == 1),
      loss  = sum(df_r22_filtered$Presence == 1 &
                    df_r22_filtered[[scn]] == 0),
      stay_present = sum(df_r22_filtered$Presence == 1 &
                           df_r22_filtered[[scn]] == 1),
      stay_absent  = sum(df_r22_filtered$Presence == 0 &
                           df_r22_filtered[[scn]] == 0)
    )
  }) %>%
    bind_rows()
  
  scenarios <- c("Presence", "presence15")#, "presence2", "presence3", "presence4")
  
  # Hier wird anhand der Presence Spalte dann die Artenzahl pro RS_PLOT berechnet.
  # Mit n_distinct wird jede Art pro Plot nur einmal gezählt
  for (scn in scenarios) {
    new_col <- paste0("Artenzahl_", scn)
    
    df_r22_filtered <- df_r22_filtered %>%
      group_by(RS_PLOT) %>%
      mutate(!!new_col := n_distinct(TaxonName[.data[[scn]] == 1])) %>%
      ungroup()
  }
  
  
  # Nochmal kleines Aufäumen, hier wird das finale DF mit den zu nutzenden Spalten für codap erzeugt
  df_r22_filtered <- df_r22_filtered %>%
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
  
  log_dir <- path("logs", "Glatthafer")
  log_file <- path(log_dir, "aggregated.log")
  
  describe_df(df = df_r22_filtered,
              log_dir = log_dir,
              log_file = log_file)
  
  
  # Speichern des Datensatzes
  tmp_dir <- path("tmp_data", "Glatthafer")
  dir_create(tmp_dir, recurse = TRUE)
  tmp_data_file <- path(tmp_dir, "Glatthafer.csv")
  
  
  # Dieser Datensatz ist schonmal in CODAP kopierbar.
  write.csv(df_r22_filtered, file = tmp_data_file, row.names = FALSE)
  
  
  invisible(TRUE)
}


generate_oat_data()
