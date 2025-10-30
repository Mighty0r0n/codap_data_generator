##############################################################################
# Dieses Skript lädt die Daten zu ReSurveyGermany separat runter.
# Um alle Daten auf einmal zu laden, bitte 00_load_all verwenden.
##############################################################################

##############################################################################
# Achtung! Dieses Skript verwendet die API der GBIF Datenbank. Es wird zum
# Download der Daten ein Account mit Nutzernamen und Passwort benötigt!
# Also keine Account verlinkung über Google oder GitHub!
# Aus Datenschutzgründen werden in diesem Skript KEINE ZUGANGSDATEN EINGETRAGEN.
# Die Zugangsdaten nimmt das Skript hier aus einer .Renviron Datei.
# Für Windows:
#   Unter C:\Users\USERNAME\Documents
# die Datei .Renviron erstellen. (Keine Textdatei. Oben im Menü Ansicht Klicken ->
# Danach ein Häkchen bei "Dateiendungen anzeigen" setzen -> Rechtsklick -> Neu 
# -> Textdatei -> Datei nun .Renviron nennen und das von windows erzeugt .txt löschen 
# -> Die Warnung ignorieren und OK klicken)
# Für Linux/Mac:
#   Unter ~\.Renviron erstellen
#
# Folgendes soll in der Datei stehen.
# 
# GBIF_USER=USERNAME
# GBIF_PWD=PASSWORD
# GBIF_EMAIL=EMAIL
#
# USERNAME, PASSWORD, MAIL hier entsprechend durch die Zugangsdaten ändern.
# Somit sind Zugangsdaten unter keinen Umständen in diesem Projekt Sichtbar.
##############################################################################

suppressPackageStartupMessages({
  library(rgbif)
  library(data.table)
  library(fs)
})


download_fish_occurrences <- function() {
  # ---- Konfiguration ----
  
  dataset_key <- "e0908eee-ad49-4e91-b4d0-1f05dd17b291"
  
  # Zielordner für die rohen GBIF-Daten
  outdir <- "raw_data/fish_gbif"
  dir_create(outdir)
  
  # Hier werden die Zugangsdaten aus der .Renviron eingefügt.
  Sys.getenv("GBIF_USER")
  Sys.getenv("GBIF_PWD")
  Sys.getenv("GBIF_EMAIL")
  
  # ---- 1. Download-Request bei GBIF anlegen ----
  # Wir filtern nur nach datasetKey, sonst nix. Das heißt: alle Zeilen.
  download_key <- occ_download(
    pred_in("datasetKey", dataset_key)
  )
  
  # occ_download() gibt ein Objekt zurück, aber wir holen uns die ID explizit:
  download_key <- download_key[1]
  
  cat("Download-Key:\n", download_key, "\n")
  
  # ---- 2. ZIP-Datei von GBIF holen ----
  # occ_download_get() lädt das Archiv zum lokalen Repo.
  
  
  # Der Download wird für uns erst vorbereitet und steht nach einiger Zeit erst bereit
  # Wir warten hier auf den Status des requests und laden die Datei erst wenn
  # der Download bereit ist. Sonst stürtz das Programm ab
  repeat {
    meta <- occ_download_meta(download_key)
    if (meta$status == "SUCCEEDED") break
    if (meta$status %in% c("FAILED", "KILLED")) stop("Abgebrochen.")
    Sys.sleep(60)
  }
  
  
  # Hier findet der eigentliche download dann statt
  zip_path <- occ_download_get(download_key, overwrite = TRUE)
  
  cat("ZIP gespeichert unter:\n", zip_path, "\n")
  
  # ---- 3. ZIP entpacken ----
  unzip(zip_path, exdir = outdir)
  
  cat("Dateien im Output-Ordner:\n")
  print(dir_ls(outdir))
  
  # Typischerweise liegen da jetzt:
  # - occurrence.txt
  # - event.txt (falls vorhanden)
  # - meta.xml
  # - eml.xml (Metadaten)
  # - rights.txt / citation.txt etc.
  
  # ---- 4. Dateien in R einlesen ----
  occurrence_file <- file.path(outdir, "occurrence.txt")

  
  # Occurrence.txt ist eigentlich schon das, allerdings als tab-separeted-values
  # ; und , werden bereits in einzelnen metafeldern als subtrenner benutzt.
  # Unten wird nochmal schnell überprüft ob die Datei auch wirklich runtergeladen
  # worden ist. Zusätzlich wird nochmal ausgegeben wieviele Daten da jetzt wirklich
  # runter geladen worden sind.
  if (file_exists(occurrence_file)) {
    occurrences <- fread(occurrence_file)
    cat("occ Dimension:\n")
    print(dim(occurrences))
  } else {
    warning("occurrence.txt nicht gefunden")
  }


  
  cat("Fertig. Rohdaten liegen in:\n", outdir, "\n")
  
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
if (sys.nframe() == 0) {
  download_fish_occurrences()
}
