suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

generate_insect_data <- function(min_years, min_measures) {
  
  abundance <- read_csv("raw_data/Insects/data/InsectAbundanceBiomassData.csv") %>%
    filter(
      MetricAB == "abundance",
      Year >= 2000,
      !is.na(Number)
    )
  plots <- read_csv("raw_data/Insects/data/PlotData_2023.csv") %>%
    filter(
      !is.na(Latitude),
      !is.na(Longitude)
    )
  sources <- read_csv("raw_data/Insects/data/DataSources_2023.csv") %>%
    mutate(
      ClimateZone = str_to_title(ClimateZone)
    )

  
  sample <- read_csv("raw_data/Insects/data/SampleData_2023.csv")
  
  
  raw <- read_csv("raw_data/Insects/data/rawData_2023.csv") %>%
    mutate(
      Unit = str_to_title(Unit)
    ) %>%
    filter(
      Unit == "Abundance",
      Year >= 2000
    ) %>%
    left_join(
      sample %>% select(Sample_ID, Stratum),
      by = "Sample_ID"
    )
  # 
  df_taxa <- raw %>%
    group_by(Plot_ID, Year, Stratum, Taxon) %>%
    summarise(
      abundance = sum(Number, na.rm = TRUE),
      .groups = "drop"
    )

  df_div <- df_taxa %>%
    group_by(Plot_ID, Year, Stratum) %>%
    mutate(
      p_i = abundance / sum(abundance)
    ) %>%
    summarise(
      Shannon = -sum(p_i * log(p_i), na.rm = TRUE),
      Simpson = {
        n <- sum(abundance)
        if (n > 1) {
          1 - sum(abundance * (abundance - 1)) / (n * (n - 1))
        } else {
          0
        }
      },
      .groups = "drop"
    )

  df_richness <- raw %>%
    group_by(Plot_ID, Year, Stratum) %>%
    summarise(
      Artenzahl = n_distinct(Taxon),
      .groups = "drop"
    )
  
  
  df_full <- abundance %>%
    left_join(plots %>% select(-DataSource_ID), by = "Plot_ID")  %>%
    left_join(sources, by = "DataSource_ID")
  # %>%
  #   filter(NationState %in% c("United Kingdom", "Sweden", "USA"))
  

  nationCount <- df_full %>%
    count(NationState, sort = TRUE)
  
  
  
  # Hier wird gezählt über wieviele Jahre Messwerte für eine Plot_ID vorhanden sind
  # Damit genügend Zeitreihen vorhanden sind um Trends über Plots sichtbar zu machen
  long_measured_plots <- df_full %>%
    group_by(Plot_ID) %>%
    summarise(
      n_years = n_distinct(Year),
      first = min(Year),
      last = max(Year)
    ) %>%
    filter(n_years >= min_years) %>%
    pull(Plot_ID)
  
  
  df_full <- df_full %>%
    filter(Plot_ID %in% long_measured_plots
    ) %>%
    select(-starts_with("..."))
  
  
  
  df_yearly <- df_full %>%
    group_by(Plot_ID, Year, Stratum) %>%
    summarise(
      abundance = median(Number, na.rm = TRUE),
      n_measurements = n(),
      DataSource_ID = first(DataSource_ID),
      Latitude = first(Latitude),
      Longitude = first(Longitude),
      Ort = first(Location),
      Land = first(NationState),
      Klima = first(ClimateZone),
      .groups = "drop"
    ) %>%
    filter(n_measurements >= min_measures)
  
  df_yearly <- df_yearly %>%
    inner_join(df_richness, by = c("Plot_ID", "Year", "Stratum"))
  
  
  
  df_yearly_rel <- df_yearly %>%
    group_by(Plot_ID, Stratum) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
      abundance_rel_median = abundance / median(abundance),
      abundance_rel_first = abundance / first(abundance)
    ) %>%
    ungroup() %>%
    filter(
           is.finite(abundance_rel_median),
           !is.na(Klima)
           )

  
  df_yearly_rel <- df_yearly_rel %>%
    left_join(df_div, by = c("Plot_ID", "Year", "Stratum"))
  
  
  foo <- read_csv("codap_insect_abundance_global_n=10.csv")
  

  
  write_csv(df_yearly_rel, "codap_insect_abundance_global_indices_n=5_z=3.csv")
  
  
  

  
}

# df_periods <- df_yearly_rel %>%
#   mutate(
#     Period = case_when(
#       Year >= 2000 & Year <= 2004 ~ "Anfang (2000–2004)",
#       Year >= 2005 & Year <= 2010 ~ "Mitte (2005–2010)",
#       Year >= 2011 & Year <= 2018 ~ "Ende (2011–2018)"
#     )
#   ) %>%
#   filter(!is.na(Period)) %>%
#   group_by(Plot_ID, Period) %>%
#   summarise(
#     abundance_rel = mean(abundance_rel, na.rm = TRUE),
#     DataSource_ID = first(DataSource_ID),
#     Latitude = first(Latitude),
#     Longitude = first(Longitude),
#     Ort = first(Ort),
#     .groups = "drop"
#   )


#write_csv(df_periods, "codap_insect_abundance_periods_global.csv")


generate_insect_data(min_years = 5, min_measures = 3)