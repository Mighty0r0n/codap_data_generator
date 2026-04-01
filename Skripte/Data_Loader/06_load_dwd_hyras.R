suppressPackageStartupMessages({
  library(httr)
  library(stringr)
  library(fs)
  library(R.utils)
})

download_hyras_tas_v61 <- function(
    out_dir = "raw_data/hyras_tas_v6_1",
    years = NULL,
    overwrite = FALSE
) {

  
  base_url <- paste0(
    "https://opendata.dwd.de/climate_environment/CDC/grids_germany/",
    "daily/hyras_de/air_temperature_mean/"
  )
  
  dir_create(out_dir)

  resp <- GET(base_url)
  stop_for_status(resp)
  html <- content(resp, as = "text", encoding = "UTF-8")
  
  hrefs <- str_extract_all(html, '(?<=href=")[^"]+')[[1]]
  

  nc_files <- hrefs[
    grepl("^tas_hyras_1_[0-9]{4}_v6-1_de\\.nc$", hrefs)
  ]
  

  file_years <- as.integer(str_extract(nc_files, "[0-9]{4}"))
  

  if (!is.null(years)) {
    years <- as.integer(years)
    nc_files <- nc_files[file_years %in% years]
    file_years <- file_years[file_years %in% years]
    
    missing_years <- setdiff(years, file_years)
  }
  
  
  nc_files <- nc_files[order(file_years)]
  
  downloaded <- character(0)
  skipped <- character(0)
  failed <- character(0)
  
  for (file_name in nc_files) {
    url <- paste0(base_url, file_name)
    dest <- path(out_dir, file_name)
    
    if (file_exists(dest) && !overwrite) {
      message("Übersprungen: ", file_name)
      skipped <- c(skipped, dest)
      next
    }
    
    message("Lade herunter: ", file_name)
    
    ok <- tryCatch({
      download.file(
        url = url,
        destfile = dest,
        mode = "wb",
        quiet = TRUE
      )
      TRUE
    }, error = function(e) {
      message("  Fehlgeschlagen: ", file_name, " -> ", conditionMessage(e))
      FALSE
    })
    
    if (ok && file_exists(dest)) {
      downloaded <- c(downloaded, dest)
    } else {
      failed <- c(failed, file_name)
    }
  }
  
  invisible(list(
    downloaded = downloaded,
    skipped = skipped,
    failed = failed,
    out_dir = out_dir
  ))
}

hyras_files <- download_hyras_tas_v61(years = 1989:2016)