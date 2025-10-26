---
title: "CODAP Data Generator"
output: html_document
---

# CODAP Data Generator

Dieses Repository enthält R-Skripte zur Aufbereitung und Bereinigung von Datensätzen für die Plattform https://codap.concord.org/.  

Das Projekt wurde mit RStudio entwickelt und nutzt `renv` für reproduzierbare Umgebungen.

## Voraussetzungen

- R Version ≥ 4.5.0  
- RStudio (empfohlen)  
- Git installiert und eingerichtet  
- Optional: GitHub-Personal-Access-Token (für private Repositories)

## Installation und Setup

### 1. Repository klonen

Im Terminal oder in RStudio:

```bash
git clone https://github.com/DEINNAME/codap_data_cleaner.git
```

### 2. R-Projekt öffnen

Öffne die Datei `codap_data_cleaner.Rproj` in RStudio.
Alle Pfade im Projekt sind relativ und basieren auf `here::here()`.


### 3. Umgebung wiederherstellen

Das Projekt verwendet renv zur Verwaltung der Paketversionen. 

In der RStudio-Konsole ausführen:

```r
renv::restore()
```

Dadurch werden alle benötigten Pakete in einer lokalen, projektbezogenen Library installiert.
Die globale R-Installation wird dabei nicht verändert.



### Projektstruktur

```text
codap_data_cleaner/
├─ data_raw/                                     # Eingangsdatensätze
│   └─ arten_umwelt_modellierung/
├─ result_data/                                  # Ordner für die Finalen Datensätze
├─ tmp_data/                                     # Ordner für eventuelle tempörare Dateien
├─ Skripte/
│   ├─ 00_setup_environment.R                    # Init des Repos
│   ├─ 01_load_data.R                            # IN PLANUNG
│   ├─ Datensatz_Pflanzenarten_Umweltfaktoren.R  # Skript für Datensatz für Pflanzenarten und Umweltfaktoren 
│   └─ ...
├─ renv/                                         # Lokale renv-Library (nicht versioniert)
├─ renv.lock                                     # Paket-Snapshot (wird versioniert)
├─ .Rprofile
├─ .gitignore                                    
└─ README.md                                      
```









