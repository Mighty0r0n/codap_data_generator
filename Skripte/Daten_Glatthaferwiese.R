suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(tidyr)
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
  
  survey_df <- survey_df %>%
    mutate(across(matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"), ~ replace_na(.x, 0)))
  
  
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
  
  
  
  # Wir aggregieren für jeden Standort nach Ort die Daten und bilden mittelwerte aus den entsprechenden coverages
  agg_site_year <- survey_df %>%
    group_by(RS_SITE, YEAR) %>%
    summarise(
      Artenzahl         = n_distinct(TaxonName),         
      Gesamtdeckung     = mean(Cover_Perc), 
      LONGITUDE         = first(LONGITUDE),
      LATITUDE          = first(LATITUDE),
      SURF_AREA_mean    = mean(SURF_AREA),
      n_plots           = n_distinct(paste(LONGITUDE, LATITUDE, sep = "_")), 
      across(matches("^(COV_|TREE_|HERB_)"), ~ mean(.x)),
      MEAN_TEMPERATURE = round(mean(MEAN_TEMPERATURE)),
      PRECIPITATION = round((mean(PRECIPITATION) / 10) /10) * 10, # Lässt sich nicht gut bisher für codap visualisierung skalieren
      .groups = "drop"
    )
  
  
  # Alle zahlenwerte auf 2 Stellen runden
  agg_site_year <- agg_site_year %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  tmp_dir <- path("tmp_data", "Glatthafer")
  dir_create(tmp_dir, recurse = TRUE)
  tmp_data_file <- path(tmp_dir, "Glatthafer.csv")
  
  write.csv(agg_site_year, file = tmp_data_file, row.names = FALSE)
  invisible(TRUE)
}


generate_oat_data()
