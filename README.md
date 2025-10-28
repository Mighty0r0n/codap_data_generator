# CODAP Data Generator

Dieses Repository enthält R-Skripte zur Aufbereitung und Bereinigung von Datensätzen für die Plattform https://codap.concord.org/.  

Das Projekt wurde mit RStudio entwickelt und nutzt `renv` für reproduzierbare Umgebungen.

## Voraussetzungen

- R Version ≥ 4.5.0  
- RStudio (empfohlen)  
- Git installiert und eingerichtet  
- Renv
- Optional: GitHub-Personal-Access-Token (für private Repositories)


Linux/Mac:
Das Repo wurde unter Windows entwickelt. Falls dieses Repo unter Linux/MacOS
gestartet wird, kann es sein, dass das R-package terra einen Fehler wirft.
```bash
sudo apt-get install libgdal-dev libgeos-dev libproj-dev libtbb-dev libnetcdf-dev
```
sollte diesen Fehler laut dem [Installationshandbuch](https://rspatial.github.io/terra/) lösen.

Für einige weitere detailliertere Hintergrundinformationen, Erklärungen zur Projektstruktur und Hilfestellungen zur Nutzung des Codes  
siehe das **[Projekt-Wiki](https://github.com/Mighty0r0n/codap_data_generator/wiki)**:


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



### 5. Datensätze erstellen

Im Ordner `Skripte/` können nun die Skripte beginnend mit `Datensatz_*` ausgeführt werden um
die entsprechenden Datensätze nun zu generieren.



### Projektstruktur

```text
codap_data_generator/
├─ raw_data/                                     # Eingangsdatensätze (Rohdaten)
│   ├─ ReSurveyGermany/                          # heruntergeladener iDiv-Datensatz (ID 3514)
│   ├─ air_temperature_mean/                     # DWD Rasterdaten: mittlere Lufttemperatur (ASC)
│   └─ precipitation/                            # DWD Rasterdaten: Niederschlag (ASC)
│
├─ result_data/                                  # Endgültig bereinigte und zusammengeführte Datensätze
│   └─ ...
│
├─ tmp_data/                                     # Zwischenstände oder temporäre Dateien
│   └─ ...
│
├─ Skripte/                                      # Alle R-Skripte für das Projekt
│   ├─ Data_Loader/                              # Unterordner für alle Download- und Ladeskripte
│   │   ├─ 00_load_all.R                         # führt alle Data-Loader-Skripte sequentiell aus
│   │   ├─ 01_load_ReSurveyGermany.R             # lädt ReSurveyGermany-Daten (ID 3514)
│   │   ├─ 02_download_dwd_asc_grids.R           # lädt DWD-Jahresraster (air_temperature_mean & precipitation)
│   │   └─ ...                                   # ggf. weitere Quellen (z. B. Boden, Vegetation, etc.)
│   │
│   ├─ Datensatz_Pflanzenarten_Umweltfaktoren.R  # Haupt-Skript zur Zusammenstellung des kombinierten Datensatzes
│   └─ ...                                       # weitere Analyse- oder Verarbeitungs-Skripte
│
├─ renv/                                         # Lokale renv-Library (nicht versioniert)
│
├─ renv.lock                                     # Paket-Snapshot für reproduzierbare Umgebung
├─ .Rprofile                                     # startet automatisch renv beim Öffnen
├─ .gitignore                                    # enthält renv/, tmp_data/, ggf. .Rhistory etc.
└─ README.md                                     # Projektbeschreibung & Reproduktionshinweise
                                    
```









