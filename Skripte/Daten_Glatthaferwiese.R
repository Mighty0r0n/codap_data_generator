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
    mutate(across(matches("^(COV_|TREE_|HERB_|SHRUB_|SURF_)"), ~ replace_na(.x, 0)))
  
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
  

  # Wir ermitteln hier die Artenzahl für jedes releve pro RS_Site
  # Dies vermeidet übergewichtung von releves mit mehr arten
  rich_per_releve <- survey_df %>%
    distinct(RS_SITE, YEAR, RELEVE_NR.x, TaxonName) %>%
    group_by(RS_SITE, YEAR, RELEVE_NR.x) %>%
    summarise(
      richness_releve = n_distinct(TaxonName),
      .groups = "drop")
  
  
  # Da wir über distinct werte für Coverage rausschmeißen, ich aber
  # die cover_perc auch pro releve_nr mitteln will mache ich das hier getrennt
  # Erstmal werden die means für jede relevenummer gebildet anschließend werden die
  # Mittelwerte dann für die RS_Site genommen
  coverage <- survey_df %>%
    group_by(RS_SITE, YEAR, RELEVE_NR.x) %>%
    summarise(
      mean_cover_releve = mean(Cover_Perc),       
      .groups = "drop"
    )  %>%
    group_by(RS_SITE, YEAR) %>%
    summarise(
      Cover_Perc = mean(mean_cover_releve),   
      .groups = "drop"
    )
  
  
  # Hier wird dann die Artenzahl pro RS_Site ermittelt, pro releve sind
  # hier doppelte arten entfernt und werden nicht mitgezählt
  richness <- rich_per_releve %>%
    group_by(RS_SITE, YEAR) %>%
    summarise(
      Artenzahl     = n_distinct(richness_releve),                 
      n_releves        = n(),
      .groups = "drop"
    )
  
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
    left_join(coverage,  by = c("RS_SITE","YEAR")) %>%
    left_join(env_agg, by = c("RS_SITE","YEAR")) %>%
    arrange(RS_SITE, YEAR) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  
  log_dir <- path("logs", "Glatthafer")
  log_file <- path(log_dir, "aggregated.log")
  
  describe_df(df = agg_site_year,log_dir = log_dir, log_file = log_file)
  
  #-----------------------------------------------------------------------------
  # Hier beginnt die Modellierung
  # Lose nach DOI:10.14471/2017.37.010 ausgewählt 
  # Dort wurde aufgeführt das COV_LITTER und COV_ROCK unter umständen problematisch
  # für die Modellierung sein können, da sie oft stark miteinander korrelieren.
  # Dementsprechend wird das hier erstmal kurz überprüft
  
  
  # Hier werden die zu überprüfenden Spalten ausgewählt
  cov_vars <- agg_site_year %>%
    select(COV_LITTER, COV_ROCK)
  
  # Korrelationen berechnen (ohne NAs)
  cor_matrix <- cor(cov_vars, use = "complete.obs")
  
  # Runde und ausgeben
  # Werte sind |r| < 0,7 also würde ich sie drinnen lassen
  round(cor_matrix, 2)
  
  
  oat_lm <- lm(
    Artenzahl ~ TEMPERATURE + PRECIPITATION +
      COV_HERBS + COV_TREES + COV_SHRUBS + COV_LITTER + COV_ROCK +
      COV_MOSSES + Cover_Perc + HERB_HIGH + SURF_AREA + LONGITUDE +
      LATITUDE,
    data = agg_site_year
  )
  # summary(oat_lm) gibt an:
  # Multiple R-squared:  0.1461,	Adjusted R-squared:  0.1155 was sehr dürftig ist.
  # Scenarien werden nicht wirklich durch die Temperatur mit diesem Modell beeinflusst.
  # Mit plot(oat_lm) in der interaktiven console können diverese Regressionsplots betrachtet werden
  # Zeigen generell starke streuung, aber es sind "brauchbare" in dem sinne vorhersagen.

  scenario15 <- agg_site_year
  scenario15$TEMPERATURE <- scenario15$TEMPERATURE + 1.5
  
  scenario2 <- agg_site_year
  scenario2$TEMPERATURE <- scenario2$TEMPERATURE + 2
  
  scenario3 <- agg_site_year
  scenario3$TEMPERATURE <- scenario3$TEMPERATURE + 3
  
  scenario4 <- agg_site_year
  scenario4$TEMPERATURE <- scenario4$TEMPERATURE + 4
 

  agg_site_year$Artenzahl_plus15 <- predict(oat_lm, newdata = scenario15)
  agg_site_year$Artenzahl_plus2 <- predict(oat_lm, newdata = scenario2)
  agg_site_year$Artenzahl_plus3 <- predict(oat_lm, newdata = scenario3)
  agg_site_year$Artenzahl_plus4 <- predict(oat_lm, newdata = scenario4)
 
  
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
