### global.R --------------------------------------------------------
## 0) Pakete
libs <- c("shiny", "dplyr", "readxl", "tidyr")
invisible(lapply(libs, require, character.only = TRUE))

## 1) Hilfs-Funktion
parse_number <- function(x) {
  as.numeric(gsub(",", ".", gsub("\\.", "", x)))
}

# 1) Alle Excel-Files einmalig einlesen (guess_max dämpft Typ-Warnings)
bom   <- read_excel("data/bom.xlsx",    guess_max = 10000)
MARA  <- read_excel("data/MARA.xlsx",   guess_max = 10000)
MARC  <- read_excel("data/MARC.xlsx",   guess_max = 10000)
MVER  <- read_excel("data/MVER.xlsx",   guess_max = 10000)

# Procurement
EBAN  <- read_excel("data/Procurement/EBAN.xlsx", guess_max = 10000)
EKBE  <- read_excel("data/Procurement/EKBE.xlsx", guess_max = 10000)
EKES  <- read_excel("data/Procurement/EKES.xlsx", guess_max = 10000)
EKET  <- read_excel("data/Procurement/EKET.xlsx", guess_max = 10000)
EKKO  <- read_excel("data/Procurement/EKKO.xlsx", guess_max = 10000)
EKPO  <- read_excel("data/Procurement/EKPO.xlsx", guess_max = 10000)

# Production
AFKO  <- read_excel("data/Production/AFKO.xlsx", guess_max = 10000)
AFPO  <- read_excel("data/Production/AFPO.xlsx", guess_max = 10000)
AFVC  <- read_excel("data/Production/AFVC.xlsx", guess_max = 10000)
AFVV  <- read_excel("data/Production/AFVV.xlsx", guess_max = 10000)

plant <- EKPO %>% select(EBELN, EBELP,
                         WERKS = dplyr::any_of("WERKS"))   # wird NA, wenn Spalte fehlt

if (all(is.na(plant$WERKS))) {                             # Fallback über EBAN
  plant <- EBAN %>%
    select(EBELN, EBELP, WERKS = RESWK)
}


## 1) EKKO einmalig auf Kopf‐Ebene reduzieren
ekko_head <- EKKO %>%
  distinct(EBELN, .keep_all = TRUE) %>%      # nur 1 Zeile je EBELN!
  select(EBELN, AEDAT_EKKO = AEDAT)          # gleich umbenennen

## 2) EKET auf 1 Termin je Position bringen
eket_head <- EKET %>%
  distinct(EBELN, EBELP, .keep_all = TRUE) %>%   # erster Schedule-Line
  select(EBELN, EBELP, EINDT)

## 3) orders aufbauen
orders <- EKPO %>%
  left_join(ekko_head, by = "EBELN") %>%         # hat jetzt AEDAT_EKKO
  left_join(eket_head, by = c("EBELN", "EBELP")) %>% 
  left_join(plant,     by = c("EBELN", "EBELP")) %>% 
  mutate(
    Erstelldatum  = as.Date(AEDAT_EKKO, format = "%d.%m.%Y"),
    Lieferdatum   = as.Date(EINDT,      format = "%d.%m.%Y"),
    Durchlaufzeit = as.numeric(Lieferdatum - Erstelldatum)
  ) %>% 
  select(EBELN, EBELP, MATNR, WERKS, Erstelldatum, Lieferdatum, Durchlaufzeit)

## 3) alle KPI-Funktionsdateien sourcen
lapply(list.files("kpi", "^calculate_.*\\.R$", full.names = TRUE), source)