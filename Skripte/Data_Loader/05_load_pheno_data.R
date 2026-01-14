suppressPackageStartupMessages({
  library(httr)
  library(stringr)
  library(fs)
  library(R.utils)
})

download_dwd_phenology <- function() {
  years = 2000:2024
  out_dir = "raw_data"
  base_url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/phenology/"
  
  extract_compressed_file <- function(input_path) {
    header_bytes <- readBin(input_path, what = "raw", n = 2)
    
    is_zip <- identical(header_bytes, as.raw(c(0x50, 0x4b))) # "PK"  = ZIP Signatur
    is_gz  <- identical(header_bytes, as.raw(c(0x1f, 0x8b))) # 1f 8b = gzip Signatur
    
    output_path <- sub("\\.gz$", "", input_path, ignore.case = TRUE)
    output_path <- sub("\\.zip$", "", output_path, ignore.case = TRUE)
    
    if (is_gz) {
      gunzip(input_path, remove = TRUE, overwrite = TRUE)
      return(invisible(output_path))
    }
    
    if (is_zip) {
      utils::unzip(input_path, exdir = dirname(input_path))
      file_delete(input_path)
      return(invisible(output_path))
    }
    
    invisible(NULL)
  }
  
  dir_create(out_dir)
  phen_out_dir <- path(out_dir, "phenology")
  dir_create(phen_out_dir)
  
  list_dwd_files <- function(folder_url, years = 2000:2024) {
    dwd_page <- GET(folder_url)
    stop_for_status(dwd_page)
    html <- content(dwd_page, as = "text", encoding = "UTF-8")
    
    hrefs <- str_extract_all(html, '(?<=href=")[^"]+')[[1]]
    hrefs <- hrefs[!grepl("/$", hrefs)]  # nur Dateien
    
    asc_gz_files <- hrefs[grepl("\\.asc\\.gz$", hrefs, ignore.case = TRUE)]
    
    file_years <- as.integer(str_extract(asc_gz_files, "20\\d{2}"))
    asc_gz_files <- asc_gz_files[!is.na(file_years) &
                                   file_years %in% years]
    
    pdf_files <- hrefs[grepl("\\.pdf$", hrefs, ignore.case = TRUE)]
    
    list(
      asc_gz_urls = paste0(folder_url, asc_gz_files),
      pdf_urls    = paste0(folder_url, pdf_files)
    )
  }
  
  list_dwd_dirs <- function(folder_url) {
    dwd_page <- GET(folder_url)
    stop_for_status(dwd_page)
    html <- content(dwd_page, as = "text", encoding = "UTF-8")
    
    hrefs <- str_extract_all(html, '(?<=href=")[^"]+')[[1]]
    dirs <- hrefs[grepl("/$", hrefs)]
    dirs <- dirs[dirs != "../"]
    dirs <- dirs[!grepl("^\\?", dirs)]
    dirs
  }
  
  pheno_codes <- list_dwd_dirs(base_url)
  
  for (code_dir_name in pheno_codes) {
    code <- sub("/$", "", code_dir_name)
    
    code_url <- paste0(base_url, code_dir_name)
    
    
    code_out_dir <- path(phen_out_dir, code)
    dir_create(code_out_dir)
    
    dwd_links <- list_dwd_files(code_url, years = years)
    dwd_raster_links <- dwd_links$asc_gz_urls
    dwd_pdf_links    <- dwd_links$pdf_urls
    
    
    for (file_url in dwd_raster_links) {
      gz_path  <- path(code_out_dir, basename(file_url))
      asc_path <- sub("\\.gz$", "", gz_path, ignore.case = TRUE)
      
      if (file_exists(asc_path)) {
        next
      }
      
      
      GET(file_url,
          write_disk(gz_path, overwrite = TRUE),
          progress())
      
      extract_compressed_file(gz_path)
      
    }
    
    for (pdf_url in dwd_pdf_links) {
      pdf_path <- path(code_out_dir, basename(pdf_url))
      
      if (file_exists(pdf_path)) {
        next
      }
      
      
      GET(pdf_url,
          httr::write_disk(pdf_path, overwrite = TRUE),
          progress())
    }
    
  }
  
  invisible(TRUE)
}

download_dwd_phenology()