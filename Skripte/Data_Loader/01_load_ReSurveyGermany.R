##############################################################################
# Dieses Skript lädt die Daten zu ReSurveyGermany separat runter.
# Um alle Daten auf einmal zu laden, bitte 00_load_all verwenden.
##############################################################################

# Zuerst alle benötigten Pakete laden.
suppressPackageStartupMessages({
  library(httr)      # HTTP-Requests
  library(stringr)   # Regex für Link-Suche
  library(fs)        # Pfad-Handling
})

##############################################################################
# 1. Alles für das runterladen der Daten vorbereiten.
##############################################################################


# Das ganze wird als Funktion definiert, um zu Verhindern dass wir zu viele
# Globale Variablen erzeugen. Globale Variablen sind zu jeder Zeit der Laufzeit
# gespeichert. Haben 2 Skripte nun die selben Variablen aber einen anderen Inhalt
# können wir im schlimmsten Fall zuvor erstelle Daten ausversehen löschen oder
# sie manipulieren ohne es zu merken. Deshalb bekommt jede Variable mithilfe dieser
# Funktion eingenen Gültigkeitsbereich um die "uniqueness" der Variable zu gewährleisten.
download_resurvey_germany <- function() {

  # Ordner raw_data als Sammelort für alle Datensätze
  out_dir <- "raw_data"
  dir_create(out_dir)
  
  
  # Hier wird der Pfad zum Download der ReSurveyGermany Daten gebaut
  re_survey_dataset_id <- 3514
  re_survey_base_url <- "https://idata.idiv.de/ddm"
  re_survey_load_url <- paste0(
    re_survey_base_url,
    "/data/Showdata/",
    re_survey_dataset_id
    )
  
  
  ############################################################
  # Hier wird noch ein Unterordner für die zu runterladene .zip erstellt.
  # Diese enthält Metadaten und soll so von den weiteren raw_daten die noch
  # geladen werden getrennt auffindbar sein.
  re_survey_extract_dir <- path(out_dir, "ReSurveyGermany")
  re_survey_zip_path <- path(re_survey_extract_dir, paste0(re_survey_dataset_id, ".zip"))
  dir_create(re_survey_extract_dir)
  
  
  ############################################################
  # 2. Falls keine API-Abfrage für die Daten möglich ist, sind die
  # Download-Links oft bei vielen Seiten ins HTML eingebettet
  # und somit über einen Inspect der Seite im Browser einsehbar.
  # Das mache ich mir hier zu Nutzen und suche mithilfe eines Regex Matchs
  # den entsprechenden Download Link herraus. (Die Stelle und entsprechende
  # Regex pattern lassen sich nur nach einer manuellen Sichtung des
  # HTML codes ermitteln)
  ############################################################
  
  # Hier wird erstmal mittels GET die Webpage für den Download abgerufen.
  message("[INFO] Lade Webpage ...")
  re_survey_page <- GET(re_survey_load_url)
  stop_for_status(re_survey_page)
  
  # Mit Content bekomme ich den HTML Code der Webpage ausgegeben
  re_survey_html <- content(re_survey_page, as = "text", encoding = "UTF-8")
  
  # Hier kann ich nun den Download link an der zuvor ermittelten Stelle im HTML suchen
  # und als Variable zur weiteren verarbeitung nutzen.
  re_survey_pattern <- paste0("/ddm/Data/DownloadZip/", re_survey_dataset_id, "\\?version=\\d+")
  re_survey_match <- str_match(re_survey_html, re_survey_pattern)[, 1]
  
  
  # Kleiner Check ob alles geklappt hat.
  if (is.na(re_survey_match)) {
    stop("Kein Download-Link gefunden. Eventuell hat sich die Seite geändert.")
  }
  
  re_survey_download_url <- paste0("https://idata.idiv.de", re_survey_match)
  message("[INFO] Gefundene Download-URL: ", re_survey_download_url)
  
  
  ############################################################
  # 3. Jetzt werden die Daten endlich runtergeladen.
  ############################################################
  
  message("[INFO] Lade ZIP-Datei herunter ...")
  GET(re_survey_download_url, write_disk(re_survey_zip_path, overwrite = TRUE), progress())
  message("[OK] ZIP-Datei gespeichert unter: ", path_abs(re_survey_zip_path))
  
  
  ############################################################
  # 4. Die runtergeladene .zip enthält Metadaten und eine weitere .zip
  # Wir wollen die ReSurveyGermany.csv in der zweiten .zip haben.
  ############################################################
  
  # Hier wird nur der Inhalt der .zip betrachtet. Es wird nichts entpackt
  zip_content <- utils::unzip(re_survey_zip_path, list = TRUE)
  
  # Die Datei "ReSurveyGermany.zip" finden
  target_file <- zip_content$Name[grepl("ReSurveyGermany\\.zip$", zip_content$Name)]
  
  if (length(target_file) == 0) {
    stop("Datei 'ReSurveyGermany.zip' wurde in 3514.zip nicht gefunden.")
  }
  
  message("[INFO] Extrahiere nur: ", target_file)
  
  # Nur diese eine Datei extrahieren, direkt in den gleichen Ordner
  utils::unzip(
    zipfile = re_survey_zip_path,
    files   = target_file,
    exdir   = re_survey_extract_dir
  )
  
  # Hier speichern wir nun den Pfad zur inneren .zip um diese gleich separat
  # entpacken zu können
  re_survey_inner_zip <- path(re_survey_extract_dir, "ReSurveyGermany.zip")
  
  message("[OK] ReSurveyGermany.zip extrahiert nach: ", path_abs(re_survey_inner_zip))
  
  #############################################################
  # 5. Aus ReSurveyGermany.zip nur die CSV extrahieren
  ############################################################
  
  
  
  # Hier wird wieder der Inhalt der .zip angeschaut und zurückgegeben. 
  inner_content <- utils::unzip(re_survey_inner_zip, list = TRUE)
  # Dateien "ReSurveyGermany.csv" und "header_ReSurveyGermany.csv" finden
  csv_files <- inner_content$Name[
    grepl("(?:^|/|\\\\)(Header_)?ReSurveyGermany\\.csv$", inner_content$Name)
  ]
  
  
  if (length(csv_files) == 0) {
    stop("Datei 'ReSurveyGermany.csv' wurde in ReSurveyGermany.zip nicht gefunden.")
  }
  
  message("[INFO] Extrahiere nur: ", csv_files)
  
  # Nun wird der Datensatz ReSurveyGermany.csv extrahiert und im
  # vorgesehenen Ordner abgelegt
  utils::unzip(
    zipfile = re_survey_inner_zip,
    files   = csv_files,
    exdir   = re_survey_extract_dir
  )
  
  message("[OK] ReSurveyGermany.csv extrahiert nach: ", path_abs(re_survey_extract_dir))
  
  
  # Das folgende ist eine Persönliche Präferenz meinerseits.
  # Ich arbeite lieber mit Semikolons als Separator für csv_dateien, einfach
  # aus dem Grund, dass es Robuster gegenüber der Europäischen und Amerikanischen
  # Dezimaltrennerstandards, also komma oder punkt ist. 
  # Außerdem können in Metafeldern von Datenbanken Kommas als Metainformationstrenner
  # innerhalb einer Spalte verwendet werden.
  
  for (csv_file in csv_files) {
    
    csv_path <- path(re_survey_extract_dir, csv_file)
    
    message("[INFO] Konvertiere ", basename(csv_path), " zu Semikolon-Trennung ...")
    
    # Datei einlesen (kommagetrennt)
    data <- readr::read_csv(csv_path, show_col_types = FALSE)
    
    # Mit Semikolon wieder überschreiben
    readr::write_delim(data, csv_path, delim = ";")
    
    message("[OK] ", basename(csv_path), " erfolgreich konvertiert und überschrieben.")
  }
  
  
  message("[DONE] Download und Entpacken abgeschlossen.")
  
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
  download_resurvey_germany()
}