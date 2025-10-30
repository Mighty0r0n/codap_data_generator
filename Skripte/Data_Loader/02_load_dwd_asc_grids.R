##############################################################################
# Dieses Skript lädt die jährlichen DWD Rasterdaten für Deutschland herunter.
# Es werden die beiden Variablen air_temperature_mean und precipitation
# aus den annual grids heruntergeladen.
# Um alle Daten auf einmal zu laden, bitte 00_load_all verwenden.
##############################################################################

# Zuerst alle benötigten Pakete laden.
suppressPackageStartupMessages({
  library(httr)      # HTTP Requests
  library(stringr)   # Regex / Link-Suche
  library(fs)        # Pfad-Handling
  library(R.utils)   # Entpacken von gzip Dateien
})

##############################################################################
# 1. Vorbereitung
##############################################################################


# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion eingenen Gültigkeitsbereich um die "uniqueness" der Variable zu gewährleisten.
download_dwd_grids <- function() {
  
  # Hilfsfunktion: erkennt Dateityp (.gz vs .zip) und entpackt passend
  # Diese Funktion kümmert sich darum, dass unterschiedliche Kompressions-
  # formate korrekt behandelt werden. Manche Dateien vom DWD enden zwar auf
  # .asc.gz sind aber intern ein .zip Archiv. Das wird hier abgefangen.
  extract_compressed_file <- function(input_path) {
    # Lies die ersten 2 Bytes, um den Typ zu erkennen
    header_bytes <- readBin(input_path, what = "raw", n = 2)
    
    is_zip <- identical(header_bytes, as.raw(c(0x50, 0x4b))) # "PK"  = ZIP Signatur
    is_gz  <- identical(header_bytes, as.raw(c(0x1f, 0x8b))) # 1f 8b = gzip Signatur
    
    # Zielpfad ohne .gz (oder .zip, falls falsch benannt)
    # Beispiel: "foo.asc.gz" -> "foo.asc"
    output_path <- sub("\\.gz$",  "", input_path, ignore.case = TRUE)
    output_path <- sub("\\.zip$", "", output_path, ignore.case = TRUE)
    
    if (is_gz) {
      message("    -> Detektiert: gzip")
      R.utils::gunzip(input_path, remove = TRUE, overwrite = TRUE)
      return(invisible(output_path))
    }
    
    if (is_zip) {
      message("    -> Detektiert: zip")
      # entpacke ZIP in dasselbe Verzeichnis
      utils::unzip(input_path, exdir = dirname(input_path))
      file_delete(input_path)
      return(invisible(output_path))
    }
    
    warning("    -> Unbekanntes Format, nichts entpackt: ", basename(input_path))
    invisible(NULL)
  }
  
  
  # Der Ordner raw_data ist der Sammelort für alle Datensätze.
  out_dir <- "raw_data"
  dir_create(out_dir)
  
  
  # Basis-URL der DWD-Datenquelle
  dwd_base_url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/"
  
  # Die beiden Datentypen, die heruntergeladen werden sollen
  dwd_subfolders <- c("air_temperature_mean/", "precipitation/")
  
  
  
  ##############################################################################
  # 2. Hilfsfunktion um alle .asc.gz und .pdf download Links aus jeweils 
  # air_temperature_mean und precipitation zu bekommen.
  #
  # Zusatz:
  #   - Wir filtern hier direkt auf Jahr >= 2000.
  #     Hintergrund: Die DWD Rasterdateien haben in der Regel das Jahr 
  #     im Dateinamen (z.B. "..._2017.asc.gz"). Wir extrahieren nur Dateien,
  #     deren Jahr in /2000|2001|...|2024/ fällt, um nicht unnötig alte
  #     Raster runterzuladen.
  #
  #   - PDFs werden separat gesammelt. PDFs sind z.B. Metadaten/Dokumentation
  #     und sollen im selben Zielordner gespeichert werden.
  #
  # Diese Funktion wird für beide Ordner gleich benutzt damit der Code
  # nicht doppelt im Skript stehen muss (reine Lesbarkeit/Wartbarkeit).
  ##############################################################################
  
  list_dwd_files <- function(folder_url) {
    # HTML der Seite abrufen
    dwd_page <- GET(folder_url)
    stop_for_status(dwd_page)
    html <- content(dwd_page, as = "text", encoding = "UTF-8")
    
    # Links extrahieren
    hrefs <- str_extract_all(html, '(?<=href=")[^"]+')[[1]]
    
    # Nur Dateien (keine Unterverzeichnisse)
    hrefs <- hrefs[!grepl("/$", hrefs)]
    
    # Raster-Dateien (.asc.gz) ab Jahr 2000
    # Wir prüfen, dass die Jahreszahl direkt vor der Endung steht.
    asc_gz_files <- hrefs[
      grepl("\\.asc\\.gz$", hrefs, ignore.case = TRUE) &
        grepl("_(20[0-9]{2})", hrefs)  # matcht 2000-2099, naiv implementiert. Man könnte in regex bestimmt irgendwie genau 2000-2024 angeben
    ]
    
    # PDF Dateien (Metadaten/Dokumentation)
    pdf_files <- hrefs[
      grepl("\\.pdf$", hrefs, ignore.case = TRUE)
    ]
    
    # Absolute URLs erstellen
    list(
      asc_gz_urls = paste0(folder_url, asc_gz_files),
      pdf_urls    = paste0(folder_url, pdf_files)
    )
  }
  
  
  
  ##############################################################################
  # 3. Hier wird nun über beide Unterordner für die ASCII tabellen mit for "iteriert".
  # Hier wird dann auch die oben definierte Funktion aufgerufen um die Daten zu laden.
  # 
  # Ablauf innerhalb der Schleife:
  #   - Alle relevanten .asc.gz Dateien ab Jahr 2000 sammeln
  #   - Diese Dateien herunterladen
  #   - Direkt nach Download entpacken (gzip oder zip) und Original löschen
  #   - PDF Dateien im selben Ordner speichern (werden nicht entpackt)
  ##############################################################################
  
  for (subfolder in dwd_subfolders) {
    
    message("\n[INFO] Bearbeite Datentyp: ", subfolder)
    
    # --- Hier sind wieder ein paar Einstellungen
    
    # Download-URL für den aktuellen Datentyp
    dwd_load_url <- paste0(dwd_base_url, subfolder)
    
    # Zielverzeichnis unter raw_data
    dwd_extract_dir <- path(out_dir, gsub("/$", "", subfolder))
    dir_create(dwd_extract_dir)
    
    # Alle verfügbaren Dateien einsammeln (.asc.gz ab Jahr 2000 + PDFs)
    dwd_links <- list_dwd_files(dwd_load_url)
    dwd_raster_links <- dwd_links$asc_gz_urls
    dwd_pdf_links    <- dwd_links$pdf_urls
    
    message("[INFO] Gefundene Raster-Dateien (>= Jahr 2000): ", length(dwd_raster_links))
    message("[INFO] Gefundene PDF-Dateien: ", length(dwd_pdf_links))
    
    
    ########################################
    # 3.1 Raster-Dateien herunterladen
    ########################################
    
    # Hier werden nun alle zuvor gesammelten Downloadlinks mit GET runtergeladen.
    for (file_url in dwd_raster_links) {
      
      gz_path  <- path(dwd_extract_dir, basename(file_url))  # Zielort für die komprimierte Datei
      asc_path <- sub("\\.gz$", "", gz_path, ignore.case = TRUE)  # Zielname nach Entpacken
      
      # Falls entpackte Zieldatei bereits vorhanden ist, nichts mehr tun.
      if (file_exists(asc_path)) {
        message("  [SKIP] Bereits vorhanden: ", basename(asc_path))
        next
      }
      
      message("  [DL] ", basename(file_url))
      # Hier findet dann der Download statt.
      GET(file_url, write_disk(gz_path, overwrite = TRUE), progress())
      
      # ---- Nach Download: Entpacken und komprimierte Datei löschen ----
      message("  [UNZIP] ", basename(gz_path))
      extract_compressed_file(gz_path)
      
      # Hinweis für die Konsole.
      if (file_exists(asc_path)) {
        message("  [OK] Entpackt nach: ", basename(asc_path))
      } else {
        message("  [OK] Entpackt (ggf. mehrere Dateien oder abweichender Name erzeugt)")
      }
    }
    
    
    ########################################
    # 3.2 PDF-Dateien herunterladen
    ########################################
    
    # PDFs sind üblicherweise Metainformation und sollen unverändert
    # im selben Verzeichnis gespeichert werden.
    for (pdf_url in dwd_pdf_links) {
      
      pdf_path <- path(dwd_extract_dir, basename(pdf_url))
      
      # Falls PDF bereits vorhanden, nichts mehr tun.
      if (file_exists(pdf_path)) {
        message("  [SKIP PDF] Bereits vorhanden: ", basename(pdf_path))
        next
      }
      
      message("  [DL PDF] ", basename(pdf_url))
      GET(pdf_url, write_disk(pdf_path, overwrite = TRUE), progress())
      message("  [OK PDF] Gespeichert unter: ", basename(pdf_path))
    }
    
    
    message("[OK] Fertig für ", subfolder, " -> ", path_abs(dwd_extract_dir))
  }
  
  message("\n[DONE] DWD-Daten (>= Jahr 2000) heruntergeladen, entpackt und PDFs gespeichert")
  
  invisible(TRUE)
}


download_dwd_grids()

