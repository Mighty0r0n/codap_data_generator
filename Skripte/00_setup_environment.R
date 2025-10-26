#########################################################################
# Dieses Skript dient zur Reproduzierbarkeit der Datensatazerstellung
# - Ausführen bei erstmaliger einrichtung dieses Repositorys!
# - Es wird R-Version überprüft und im Anschluss alle benötigten
#   Zusatzpackete runtergeladen.
#########################################################################

required_r_version <- "4.5.1"
cat("Aktuelle R-Version:", as.character(getRversion()), "\n")

if (as.character(getRversion()) != required_r_version) {
  warning(paste(
    "Hinweis: Dieses Projekt wurde ursprünglich mit R",
    required_r_version, "erstellt."
  ))
}

# renv aktivieren, aber nur falls noch nicht aktiv
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# prüfen, ob renv aktiv ist
if (is.null(Sys.getenv("RENV_PROJECT"))) {
  message("renv nicht aktiv – aktiviere Projektumgebung ...")
  renv::activate()
} else {
  message("renv ist bereits aktiv.")
}


# Liste der benötigten Pakete
packages_needed <- c(
  "tidyverse",
  "janitor",
  "skimr",
  "stringr",
  "lubridate",
  "forcats",
  "here",
  "assertthat"
)

# Installieren & laden
for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

cat("Alle Basispakete erfolgreich geladen.\n")

