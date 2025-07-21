### global.R --------------------------------------------------------
## 0) Pakete
libs <- c("shiny", "dplyr", "readxl", "tidyr")
invisible(lapply(libs, require, character.only = TRUE))

## 1) Hilfs-Funktion
parse_number <- function(x) {
  as.numeric(gsub(",", ".", gsub("\\.", "", x)))
}

# 1) Alle Excel-Files einmalig einlesen (guess_max dämpft Typ-Warnings)
MARC  <- read_excel("data/MARC.xlsx",   guess_max = 10000)

# Procurement
EBAN  <- read_excel("data/Procurement/EBAN.xlsx", guess_max = 10000)
EKBE  <- read_excel("data/Procurement/EKBE.xlsx", guess_max = 10000)
EKES  <- read_excel("data/Procurement/EKES.xlsx", guess_max = 10000)
EKET  <- read_excel("data/Procurement/EKET.xlsx", guess_max = 10000)
EKKO  <- read_excel("data/Procurement/EKKO.xlsx", guess_max = 10000)
EKPO  <- read_excel("data/Procurement/EKPO.xlsx", guess_max = 10000)

# nur Materialien mit Fremdbezug
materials_ext <- MARC %>%
  filter(BESKZ == "F") %>%
  select(MATNR)

# Datumsfilter ab 2023
EKKO <- EKKO %>%
  mutate(AEDAT = as.Date(AEDAT, format = "%d.%m.%Y")) %>%
  filter(AEDAT >= as.Date("2023-01-01")) %>%
  select(EBELN, AEDAT)

EKPO <- EKPO %>%
  filter(MATNR %in% materials_ext$MATNR) %>%
  semi_join(EKKO, by = "EBELN")

EKET <- EKET %>%
  # EKET hat kein MATNR, daher über Positionen filtern
  inner_join(EKPO %>% select(EBELN, EBELP), by = c("EBELN", "EBELP")) %>%
  mutate(EINDT = as.Date(EINDT, format = "%d.%m.%Y")) %>%
  filter(EINDT >= as.Date("2023-01-01")) %>%
  select(EBELN, EBELP, ETENR, EINDT, MENGE)

EKBE <- EKBE %>%
  filter(MATNR %in% materials_ext$MATNR) %>%
  semi_join(EKKO, by = "EBELN") %>%
  mutate(BUDAT = as.Date(BUDAT, format = "%d.%m.%Y")) %>%
  filter(BUDAT >= as.Date("2023-01-01")) %>%
  select(EBELN, EBELP, MATNR, MENGE, BWART, SHKZG, BUDAT, VGABE)

EKES <- EKES %>%
  # EKES hat kein MATNR, daher über Positionen filtern
  inner_join(EKPO %>% select(EBELN, EBELP), by = c("EBELN", "EBELP")) %>%
  mutate(EINDT = as.Date(EINDT, format = "%d.%m.%Y")) %>%
  filter(EINDT >= as.Date("2023-01-01")) %>%
  select(EBELN, EBELP, ETENS, EINDT)

EBAN <- EBAN %>%
  filter(MATNR %in% materials_ext$MATNR) %>%
  semi_join(EKKO, by = "EBELN") %>%
  mutate(ERDAT = as.Date(ERDAT, format = "%d.%m.%Y")) %>%
  filter(ERDAT >= as.Date("2023-01-01")) %>%
  select(EBELN, EBELP, MATNR, ERDAT, RESWK)


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

## 3) Master-Tabelle aufbauen
master <- EKPO %>%
  left_join(ekko_head, by = "EBELN") %>%         # hat jetzt AEDAT_EKKO
  left_join(eket_head, by = c("EBELN", "EBELP")) %>%
  left_join(plant,     by = c("EBELN", "EBELP")) %>%
  mutate(
    Erstelldatum  = AEDAT_EKKO,
    Lieferdatum   = EINDT,
    Durchlaufzeit = as.numeric(Lieferdatum - Erstelldatum)
  ) %>% 
  select(EBELN, EBELP, MATNR, WERKS, Erstelldatum, Lieferdatum, Durchlaufzeit)

# für die Shiny-App weiterhin unter dem Namen 'orders'
orders <- master

## 3) alle KPI-Funktionsdateien sourcen
lapply(list.files("kpi", "^calculate_.*\\.R$", full.names = TRUE), source)