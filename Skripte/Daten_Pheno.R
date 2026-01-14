suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(fs)
  library(sf)
  library(tidyr)
  library(stringr)
  library(tibble)
})


generate_pheno_data <- function() {
  
  
  asc_file <- "raw_data/phenology/APFB/grids_germany_annual_phenology_APFB_2000.asc"
  
  
  
  r <- rast(asc_file)
  
  # NODATA-Flag setzen (steht bei dir im Header: -9999)
  NAflag(r) <- -9999
  

  plot(r, main = basename(asc_file))
  
  y <- 1
  
  
  
}


generate_pheno_data()