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
})

##############################################################################
# 1. Vorbereitung
##############################################################################



# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion einen separaten Namespace um die "uniqueness" der Variable zu gewährleisten.
download_dwd_grids <- function() {

  # Der Ordner raw_data ist der Sammelort für alle Datensätze.
  out_dir <- "raw_data"
  dir_create(out_dir)
  
  
  # Basis-URL der DWD-Datenquelle
  dwd_base_url <- "https://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/"
  
  # Die beiden Datentypen, die heruntergeladen werden sollen
  dwd_subfolders <- c("air_temperature_mean/", "precipitation/")
  
  
  
  ##############################################################################
  # 2. Hilfsfunktion um alle .asc.gz download Links aus jeweils 
  # air_temperature und percipitation zu bekommen
  # Da das im R-Code für beide Ordner nach selbem schema funktioniert, bietet es
  # sich hier an eine Funktion zu definieren, damit man diesen part
  # nicht doppelt ins Skript schreiben muss. Hat nur Lesbarkeitsgründe.
  ##############################################################################
  
  list_dwd_files <- function(folder_url) {
    # HTML der Seite abrufen
    dwd_page <- GET(folder_url)
    stop_for_status(dwd_page)
    html <- content(dwd_page, as = "text", encoding = "UTF-8")
    
    # Links extrahieren
    hrefs <- str_extract_all(html, '(?<=href=")[^"]+')[[1]]
    
    # Nur .asc.gz Dateien behalten
    asc_gz_files <- hrefs[grepl("\\.asc\\.gz$", hrefs, ignore.case = TRUE)]
    
    # Absolute URLs erstellen
    paste0(folder_url, asc_gz_files)
  }
  
  
  
  ##############################################################################
  # 3. Hier wird nun über beide Unterordner für die ASCII tabellen mit for "iteriert"
  # Hier wird dann auch die oben definierte Funktion aufgerufen um die Daten zu laden
  ##############################################################################
  
  for (subfolder in dwd_subfolders) {
    
    message("\n[INFO] Bearbeite Datentyp: ", subfolder)
    
    # --- Hier sind wieder ein paar Einstellungen
    
    # Download-URL für den aktuellen Datentyp
    dwd_load_url <- paste0(dwd_base_url, subfolder)
    
    # Zielverzeichnis unter raw_data
    dwd_extract_dir <- path(out_dir, gsub("/$", "", subfolder))
    dir_create(dwd_extract_dir)
    
    # Alle verfügbaren .asc.gz Dateien als String in einer Liste sammeln
    dwd_file_links <- list_dwd_files(dwd_load_url)
    message("[INFO] Gefundene .asc.gz Dateien: ", length(dwd_file_links))
    
    
    # Hier werden nun alle zuvor gesammelten Downloadlinks mit GET runtergeladen.
    for (file_url in dwd_file_links) {
      
      gz_path  <- path(dwd_extract_dir, basename(file_url)) # Zielort der Extraktion der .gz Datei bestimmen.
      asc_path <- sub("\\.gz$", "", gz_path)  # Zielname nach Entpacken
      
      # Falls Datei bereits vorhanden, nichts runterladen.
      if (file_exists(asc_path)) {
        message("  [SKIP] Bereits vorhanden: ", basename(asc_path))
        next
      }
      
      message("  [DL] ", basename(file_url))
      # Hier findet dann der Download statt.
      GET(file_url, write_disk(gz_path, overwrite = TRUE), progress())
      
      # ---- Nach Download: Entpacken und .gz file löschen ----
      message("  [UNZIP] ", basename(gz_path))
      R.utils::gunzip(gz_path, remove = TRUE, overwrite = TRUE)
      message("  [OK] Entpackt und gelöscht: ", basename(asc_path))
    }
    
    message("[OK] Fertig für ", subfolder, " -> ", path_abs(dwd_extract_dir))
  }
  
  message("\n[DONE] DWD-Daten heruntergeladen, entpackt und .gz gelöscht")
  
  invisible(TRUE)
}

# Das entspricht dem Python `if __name__ == "__main__":`. Der untere Teil des Skripts
# wird mit dieser if Abfrage nur dann ausgeführt, wenn die Datei separat ausgeführt wird
# und nicht wenn sie über das skript 00_load_all.R ausgeführt wird. Sonst würden
# wir alles doppelt runterladen. Da die oben definierte Funktion sonst in 00_load_all.R
# über source() ausgeführt wird, was jede Zeile in der gesourcten datei ausführt und über
# den separaten Aufruf in 00 ein zweites mal ausgeführt wird.
# Ich möchte aber die Funktionalität, dass man alles über 00_load_all runter laden kann und jeweils
# separat über die einzelnen Skripte.
if (identical(environment(), globalenv())) {
  download_dwd_grids()
}
