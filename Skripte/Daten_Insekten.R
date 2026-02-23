suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

generate_insect_data <- function() {
  
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
  
  foo <- read_csv("raw_data/Insects/data/rawData_2023.csv")
  
  raw <- read_csv("raw_data/Insects/data/rawData_2023.csv") %>%
    mutate(
      Unit = str_to_title(Unit)
    ) %>%
    filter(
      Unit == "Abundance",
      Year >= 2000,
      !is.na(Taxon)
    )
  
  df_taxa <- raw %>%
    group_by(Plot_ID, Year, Taxon) %>%
    summarise(
      abundance = sum(Number, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_div <- df_taxa %>%
    group_by(Plot_ID, Year) %>%
    mutate(
      p_i = abundance / sum(abundance)
    ) %>%
    summarise(
      Shannon = -sum(p_i * log(p_i), na.rm = TRUE),
      Simpson = 1 - sum(p_i^2),
      .groups = "drop"
    )
  
  df_richness <- raw %>%
    group_by(Plot_ID, Year) %>%
    summarise(
      Artenzahl = n_distinct(Taxon),
      .groups = "drop"
    )
  
  
  df_full <- abundance %>%
    left_join(plots %>% select(-DataSource_ID), by = "Plot_ID") %>%
    left_join(sources, by = "DataSource_ID")
  # %>%
  #   filter(NationState %in% c("United Kingdom", "Sweden", "USA"))
  

  
  df_full_seasons <- df_full %>%
    mutate(
      period_clean = str_to_lower(str_trim(Period)),
      
      # Monat als Zahl extrahieren:
      # - "july", "7", "07" etc.
      # - "14-6" -> 6
      month_num = case_when(
        str_detect(period_clean, "^[0-9]{1,2}$") ~ as.numeric(period_clean),
        str_detect(period_clean, "^[0-9]{1,2}-[0-9]{1,2}$") ~ as.numeric(str_extract(period_clean, "(?<=-)[0-9]{1,2}")),
        period_clean %in% c("january") ~ 1,
        period_clean %in% c("february") ~ 2,
        period_clean %in% c("march") ~ 3,
        period_clean %in% c("april") ~ 4,
        period_clean %in% c("may") ~ 5,
        period_clean %in% c("june") ~ 6,
        period_clean %in% c("july") ~ 7,
        period_clean %in% c("august") ~ 8,
        period_clean %in% c("september") ~ 9,
        period_clean %in% c("october") ~ 10,
        period_clean %in% c("november") ~ 11,
        period_clean %in% c("december") ~ 12,
        TRUE ~ NA_real_
      ),
      
      season = case_when(
        period_clean %in% c("spring") ~ "Frühling",
        period_clean %in% c("summer") ~ "Sommer",
        period_clean %in% c("autumn", "fall") ~ "Herbst",
        period_clean %in% c("winter") ~ "Winter",
        month_num %in% c(12, 1, 2) ~ "Winter",
        month_num %in% c(3, 4, 5) ~ "Frühling",
        month_num %in% c(6, 7, 8) ~ "Sommer",
        month_num %in% c(9, 10, 11) ~ "Herbst",
        TRUE ~ NA_character_
      )
    )
  
  
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
    filter(n_years >= 5) %>%
    pull(Plot_ID)
  
  
  df_full <- df_full %>%
    filter(Plot_ID %in% long_measured_plots
    ) %>%
    select(-starts_with("..."))
  
  
  df_full_seasons <- df_full_seasons %>%
    filter(Plot_ID %in% long_measured_plots) %>%
    select(-starts_with("..."))
  
  
  df_seasonal <- df_full_seasons %>%
    filter(!is.na(season)) %>%
    group_by(Plot_ID, Year, season) %>%
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
    )
  
  raw_seasons <- raw %>%
    mutate(
      period_clean = str_to_lower(str_trim(Period)),
      month_num = case_when(
        str_detect(period_clean, "^[0-9]{1,2}$") ~ as.numeric(period_clean),
        str_detect(period_clean, "^[0-9]{1,2}-[0-9]{1,2}$") ~ as.numeric(str_extract(period_clean, "(?<=-)[0-9]{1,2}")),
        period_clean == "january" ~ 1,
        period_clean == "february" ~ 2,
        period_clean == "march" ~ 3,
        period_clean == "april" ~ 4,
        period_clean == "may" ~ 5,
        period_clean == "june" ~ 6,
        period_clean == "july" ~ 7,
        period_clean == "august" ~ 8,
        period_clean == "september" ~ 9,
        period_clean == "october" ~ 10,
        period_clean == "november" ~ 11,
        period_clean == "december" ~ 12,
        TRUE ~ NA_real_
      ),
      season = case_when(
        period_clean == "spring" ~ "Frühling",
        period_clean == "summer" ~ "Sommer",
        period_clean %in% c("autumn", "fall") ~ "Herbst",
        period_clean == "winter" ~ "Winter",
        month_num %in% c(12, 1, 2) ~ "Winter",
        month_num %in% c(3, 4, 5) ~ "Frühling",
        month_num %in% c(6, 7, 8) ~ "Sommer",
        month_num %in% c(9, 10, 11) ~ "Herbst",
        TRUE ~ NA_character_
      )
    )
  
  df_richness_seasonal <- raw_seasons %>%
    filter(!is.na(season)) %>%
    group_by(Plot_ID, Year, season) %>%
    summarise(
      Artenzahl = n_distinct(Taxon),
      .groups = "drop"
    )
  
  df_seasonal <- df_seasonal %>%
    inner_join(df_richness_seasonal, by = c("Plot_ID", "Year", "season"))
  
  df_seasonal_rel <- df_seasonal %>%
    group_by(Plot_ID, season) %>%
    mutate(abundance_rel = abundance / median(abundance)
    ) %>%
    ungroup()
  
  
  
  
  df_yearly <- df_full %>%
    group_by(Plot_ID, Year) %>%
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
    )
  
  df_yearly <- df_yearly %>%
    inner_join(df_richness, by = c("Plot_ID", "Year"))
  
  
  
  df_yearly_rel <- df_yearly %>%
    group_by(Plot_ID) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
      abundance_rel = abundance / median(abundance)
    ) %>%
    ungroup() %>%
    filter(abundance_rel > 0,
           is.finite(abundance_rel),
           !is.na(Klima)
           )
  
  df_yearly_rel <- df_yearly_rel %>%
    left_join(df_div, by = c("Plot_ID", "Year"))
  
  df_slopes_div <- df_yearly_rel %>%
    group_by(Plot_ID) %>%
    filter(sum(Artenzahl > 1) >= 2) %>%
    summarise(
      slope_shannon = coef(lm(Shannon ~ Year))[2],
      slope_simpson = coef(lm(Simpson ~ Year))[2],
      n_years = n(),
      .groups = "drop"
    )
  
  
  df_agg_year <- df_yearly_rel %>%
  group_by(Year, Klima) %>%
    summarise(
      n_monitorings  = n(),
      median_abundance = median(abundance_rel, na.rm = TRUE),
      sd_abundance   = sd(abundance_rel, na.rm = TRUE),
      .groups = "drop"
    ) %>%
  arrange(Year)

  # df_slopes_richness <- df_yearly_rel %>%
  #   group_by(Plot_ID) %>%
  #   do({
  #     m <- lm(Artenzahl ~ Year, data = .)
  #     data.frame(slope_Artenzahl = coef(m)[2])
  #   }) %>%
  #   ungroup()
  
  write_csv(df_slopes_div, "codap_insects_slope.csv")
  write_csv(df_agg_year, "codap_insect_short.csv")
  write_csv(df_seasonal_rel, "codap_insect_seasonal.csv")
  write_csv(df_yearly_rel, "codap_insect_abundance_global.csv")
  
  y <- 1
  
  

  
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


generate_insect_data()