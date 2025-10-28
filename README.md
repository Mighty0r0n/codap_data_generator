# CODAP Data Generator

[![main](https://img.shields.io/badge/branch-main-brightgreen)](https://github.com/Mighty0r0n/codap_data_generator/tree/main)
[![dev](https://img.shields.io/badge/branch-dev-yellow)](https://github.com/Mighty0r0n/codap_data_generator/tree/dev)

Dieses Repository enthält R-Skripte zur Aufbereitung und Bereinigung von Datensätzen für die Plattform https://codap.concord.org/.  

Das Projekt wurde mit RStudio entwickelt und nutzt `renv` für reproduzierbare Umgebungen.

---

## Branch-Übersicht

- **`main`** – stabiler und getesteter Code, geeignet für Reproduktion und Nutzung  
- **`dev`** – Entwicklungszweig mit Work-in-Progress-Skripten

---

## Voraussetzungen

- R Version ≥ 4.5.0  
- RStudio (empfohlen)  
- Git installiert und eingerichtet  
- Renv


Linux/Mac:
Das Repo wurde unter Windows entwickelt. Falls dieses Repo unter Linux/MacOS
gestartet wird, kann es sein, dass das R-package terra einen Fehler wirft.
```bash
sudo apt-get install libgdal-dev libgeos-dev libproj-dev libtbb-dev libnetcdf-dev
```
sollte diesen Fehler laut dem [Installationshandbuch](https://rspatial.github.io/terra/) lösen.

Für einige weitere detailliertere Hintergrundinformationen, Erklärungen zur Projektstruktur und Hilfestellungen zur Nutzung des Codes siehe das **[Projekt-Wiki](https://github.com/Mighty0r0n/codap_data_generator/wiki)**


## Installation und Setup

### 1. Repository klonen

Im Terminal oder in RStudio:

```bash
git clone https://github.com/DEINNAME/codap_data_generator.git
```

### 2. R-Projekt öffnen

Öffne die Datei `codap_data_generator.Rproj` in RStudio.
Alle Pfade im Projekt sind relativ und basieren auf `here::here()`.


### 3. Umgebung wiederherstellen

Das Projekt verwendet renv zur Verwaltung der Paketversionen. 

In der RStudio-Konsole ausführen:

```r
renv::restore()
```

Dadurch werden alle benötigten Pakete in einer lokalen, projektbezogenen Library installiert.
Die globale R-Installation wird dabei nicht verändert.



### 4. Rohdaten herunterladen

Im Ordner `Skripte/Data_Loader/` sind R-Skripte enthalten zum laden der Rohdaten.
Das Skript `00_load_all.R` lädt hierbei alle verwendeten Datensätze herunter.
Skripte nummeriert ab `01` dienen zum herunterladen der separaten verwendeten Datensätze

> Einige der Skripte erfordern **Benutzerkonten oder API-Zugänge** (z. B. für GBIF).  
> Benutzerkonten und API-Zugangsdaten werden **manuell lokal im eigenen Benutzerverzeichnis in der Datei `.Renviron`** gespeichert, um sensible Informationen (z. B. Benutzernamen, Passwörter oder Tokens) **nicht versehentlich öffentlich zu machen**.  
> Die Datei `.Renviron` wird **nicht vom Repository bereitgestellt** und ist außerdem **in `.gitignore` enthalten**.  
> Dadurch wird verhindert, dass vertrauliche Informationen versehentlich in das Repository hochgeladen werden. 
> Eine Anleitung zur Erstellung der Datei findet sich im [Wiki-Eintrag „.Renviron erstellen“](https://github.com/Mighty0r0n/codap_data_generator/wiki/.Renviron-erstellen).

### 5. Datensätze erstellen

Im Ordner `Skripte/` können nun die Skripte beginnend mit `Datensatz_*` ausgeführt werden um
die entsprechenden Datensätze nun zu generieren.

---

### Projektstruktur

```text
codap_data_generator/
├─ raw_data/                                     # Rohdaten (automatisch erstellt, nicht versioniert)
│   ├─ ReSurveyGermany/                          # heruntergeladener iDiv-Datensatz (ID 3514)
│   ├─ air_temperature_mean/                     # DWD Rasterdaten: mittlere Lufttemperatur (ASC)
│   ├─ precipitation/                            # DWD Rasterdaten: Niederschlag (ASC)
│   └─ fish_gbif                                 # GBIF Daten zu Süßwasserfischen
│
├─ result_data/                                  # Endgültig bereinigte und zusammengeführte Datensätze (automatisch erstellt, nicht versioniert)
│   └─ ...
│
├─ tmp_data/                                     # Zwischenstände oder temporäre Dateien (automatisch erstellt, nicht versioniert)
│   └─ ...
│
├─ Skripte/                                      # Alle R-Skripte für das Projekt
│   ├─ Data_Loader/                              # Unterordner für alle Download- und Ladeskripte
│   │   ├─ 00_load_all.R                         # führt alle Data-Loader-Skripte sequentiell aus
│   │   ├─ 01_load_ReSurveyGermany.R             # lädt ReSurveyGermany-Daten (ID 3514)
│   │   ├─ 02_load_dwd_asc_grids.R               # lädt DWD-Jahresraster (air_temperature_mean & precipitation)
│   │   ├─ 03_load_fish_occurrences.R            # lädt die GBIF Datenbank zu den Süßwasserfischen (NUTZERZUGANG BENÖTIGT!)
│   │   └─ ...                                   # ggf. weitere Quellen (z. B. Boden, Vegetation, etc.)
│   │
│   ├─ Datensatz_Fischarten.R                    # Skript für Datensatzgenerierung
│   ├─ Datensatz_Pflanzenarten_Umweltfaktoren.R  # Skript für Datensatzgenerierung
│   ├─ utils_data_description.R                  # utils-Funktionssammlung
│   └─ ...                                       # weitere Analyse- oder Verarbeitungs-Skripte
│
├─ renv/                                         # Lokale renv-Library (automatisch erstellt, nicht versioniert)
│
├─ renv.lock                                     # Paket-Snapshot für reproduzierbare Umgebung
├─ .Rprofile                                     # startet automatisch renv beim Öffnen
├─ .gitignore                                    # enthält renv/, tmp_data/, ggf. .Rhistory etc.
└─ README.md                                     # Projektbeschreibung & Reproduktionshinweise
                                    
```









