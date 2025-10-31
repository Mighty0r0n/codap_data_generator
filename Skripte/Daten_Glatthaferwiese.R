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
  # survey_df <- merge_re_survey_with_dwd_grids()
  
  survey_df <- read_csv("tmp_data/ReSurveyGermany/re_survey_germany_filtered_years.csv")
  
  survey_df <- survey_df %>%
    mutate(across(matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"), ~ replace_na(.x, 0))) %>%
    mutate(
      Presence = if_else(!is.na(Cover_Perc) & Cover_Perc > 0.1, 1L, 0L)
    )
  
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
  
  
  # Alle Spezies die in Spalte 2-5 gesehen worden sind.
  oat_species <- c(
    "Galium mollugo agg.",
    "Arrhenatherum elatius",
    "Bromus hordeaceus",
    "Crepis biennis",
    "Agropyron repens",
    "Urtica dioica",
    "Cirsium arvense",
    "Ranunculus repens",
    "Rumex crispus",
    "Agrostis stolonifera",
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
    "Carum carvi",
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
    "Myosotis palustris agg.",
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
    "Sanguisorba minor",
    "Plantago major",
    "Convolvulus arvensis",
    "Tripleurospermum inodorum",
    "Tanacetum vulgare",
    "Silene alba"
  )
  
  
  survey_df <- survey_df %>% filter(TaxonName %in% oat_species)
  
  #-----------------------------------------------------------------------------
  # Hier beginnt die Modellierung
  # Lose nach DOI:10.14471/2017.37.010 ausgewählt 
  # Dort wurde aufgeführt das COV_LITTER und COV_ROCK unter umständen problematisch
  # für die Modellierung sein können, da sie oft stark miteinander korrelieren.
  # Dementsprechend wird das hier erstmal kurz überprüft
  predictors <- c(
    "TEMPERATURE", "PRECIPITATION",
    "COV_HERBS", "COV_TREES", "COV_SHRUBS", "COV_LITTER", "COV_ROCK",
    "COV_MOSSES", "Cover_Perc", "HERB_HIGH", "SURF_AREA",
    "LONGITUDE", "LATITUDE"
  )
  survey_scaled <- survey_df
  
  scaling_params <- survey_df %>%
    summarise(across(all_of(predictors),
                     list(center = ~mean(.x, na.rm = TRUE),
                          scale  = ~sd(.x, na.rm = TRUE))))
  survey_scaled[predictors] <- scale(survey_scaled[predictors])
  
  # Hier werden die zu überprüfenden Spalten ausgewählt
  cov_vars <- survey_df %>%
    select(TEMPERATURE, PRECIPITATION,
             COV_HERBS, COV_TREES, COV_SHRUBS, COV_LITTER, COV_ROCK,
             COV_MOSSES, Cover_Perc, HERB_HIGH, SURF_AREA, LONGITUDE,
             LATITUDE,)
  
  # Korrelationen berechnen (ohne NAs)
  cor_matrix <- cor(cov_vars, use = "complete.obs")

  
  oat_lm <- glm(
    Presence ~ TEMPERATURE + PRECIPITATION +
      COV_HERBS + COV_TREES + COV_SHRUBS + COV_LITTER + COV_ROCK +
      COV_MOSSES + Cover_Perc + HERB_HIGH + SURF_AREA + 
      LONGITUDE + LATITUDE,
    data = survey_scaled,
    family = binomial(link = "logit"),
    control = glm.control(maxit = 100)
  )
  
  
  
  # summary(oat_lm) gibt an:
  # Multiple R-squared:  0.1461,	Adjusted R-squared:  0.1155 was sehr dürftig ist.
  # Scenarien werden nicht wirklich durch die Temperatur mit diesem Modell beeinflusst.
  # Mit plot(oat_lm) in der interaktiven console können diverese Regressionsplots betrachtet werden
  # Zeigen generell starke streuung, aber es sind "brauchbare" in dem sinne vorhersagen.
  
  
  # Für jedes Scenario ein Datensatz für die Vorhersage durchs lm erzeugen
  scenario15 <- survey_df
  scenario15$TEMPERATURE <- scenario15$TEMPERATURE + 1.5
  
  scenario2 <- survey_df
  scenario2$TEMPERATURE <- scenario2$TEMPERATURE + 2
  
  scenario3 <- survey_df
  scenario3$TEMPERATURE <- scenario3$TEMPERATURE + 3
  
  scenario4 <- survey_df
  scenario4$TEMPERATURE <- scenario4$TEMPERATURE + 4
  
  
  # Hier werden die predictions angehängt
  survey_df$presence15 <- predict(oat_lm, newdata = scenario15, type="response")
  survey_df$presence2 <- predict(oat_lm, newdata = scenario2, type="response")
  survey_df$presence3 <- predict(oat_lm, newdata = scenario3, type="response")
  survey_df$presence4 <- predict(oat_lm, newdata = scenario4, type="response")
  
  
  # Hier werden die Daten nach RS_Site und Year aggregiert.
  # vorher wird für jedes Taxon pro rs_site gezählt ob es vorkommt oder nicht.
  per_taxon_bin <- survey_df %>%
    group_by(RS_SITE, YEAR, TaxonName) %>%
    summarise(across(all_of(
      c("Presence","presence15","presence2","presence3","presence4")
      ), ~ as.integer(any(. > 0))), .groups = "drop")
  
  
  # Anschließend wird das dann pro rs_site aufsummiert
  richness <- per_taxon_bin %>%
    group_by(RS_SITE, YEAR) %>%
    summarise(across(all_of(
      c("Presence","presence15","presence2","presence3","presence4")
      ),
                     ~ sum(.x, na.rm = TRUE),
                     .names = "Artenzahl_{.col}"),
              .groups = "drop")
  

  # Auch hier werden die cov_ werte über alle releve_nr getrennt berechnet um übergewichtung zu vermeiden
  hdr_per_releve <- survey_df %>%
    distinct(RS_SITE, YEAR, RELEVE_NR.x, across(all_of(c(
      "LONGITUDE","LATITUDE","ALTITUDE","SLOPE",
      names(survey_df)[str_detect(names(survey_df), "^(COV_|TREE_|HERB_|SURF_)")],
      "TEMPERATURE","PRECIPITATION"
    ))))
  
  # Wir aggregieren für jeden Standort nach Ort die Daten und bilden mittelwerte aus den entsprechenden coverages
  env_agg <- hdr_per_releve %>%
    group_by(RS_SITE, YEAR) %>%
    summarise(
      LONGITUDE         = first(LONGITUDE),
      LATITUDE          = first(LATITUDE),
      across(matches("^(COV_|TREE_|HERB_|SURF_)"), ~ mean(.x)),
      TEMPERATURE = mean(TEMPERATURE),
      PRECIPITATION = mean(PRECIPITATION), # Lässt sich nicht gut bisher für codap visualisierung skalieren
      .groups = "drop"
    )
  
  
  # Hier wird dann alles in einem DataFrame zusammengefügt
  agg_site_year <- richness %>%
    left_join(env_agg, by = c("RS_SITE","YEAR")) %>%
    arrange(RS_SITE, YEAR) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  

  
  log_dir <- path("logs", "Glatthafer")
  log_file <- path(log_dir, "aggregated.log")
  
  describe_df(df = agg_site_year,log_dir = log_dir, log_file = log_file)
  

  # Speichern des Datensatzes
  # TODO Werte von Temparatur und Niederschlag noch an die Skala für die CODAP webapp anpassen
  # TODO Artenzahl noch richtig runden
  tmp_dir <- path("tmp_data", "Glatthafer")
  dir_create(tmp_dir, recurse = TRUE)
  tmp_data_file <- path(tmp_dir, "Glatthafer.csv")
  
  
  # Dieser Datensatz ist schonmal in CODAP kopierbar. 
  write.csv(agg_site_year, file = tmp_data_file, row.names = FALSE)
 
   
  invisible(TRUE)
}


generate_oat_data()
