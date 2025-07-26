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




## 1) EKKO einmalig auf Kopf‐Ebene reduzieren
ekko_head <- EKKO %>%
  distinct(EBELN, .keep_all = TRUE) %>%      # nur 1 Zeile je EBELN!
  select(EBELN, AEDAT_EKKO = AEDAT)          # gleich umbenennen

## 2) EKET auf 1 Termin je Position bringen
eket_head <- EKET %>%
  distinct(EBELN, EBELP, .keep_all = TRUE) %>%   # erster Schedule-Line
  select(EBELN, EBELP, EINDT)

## 2a) Planmenge je Position (Summe aller Schedule-Lines)
planned_qty_df <- EKET %>%
  group_by(EBELN, EBELP) %>%
  summarise(
    planned_qty = sum(parse_number(MENGE), na.rm = TRUE),
    .groups      = "drop"
  )

## 2b) Goods Receipt Qty (101 S)
gr_qty_df <- EKBE %>%
  filter(BWART == "101", SHKZG == "S") %>%
  group_by(EBELN, EBELP) %>%
  summarise(
    goods_receipt_qty = sum(parse_number(MENGE), na.rm = TRUE),
    .groups           = "drop"
  )

## 2c) Return / Storno Qty (102 H)
return_qty_df <- EKBE %>%
  filter(BWART == "102", SHKZG == "H") %>%
  group_by(EBELN, EBELP) %>%
  summarise(
    return_qty = sum(parse_number(MENGE), na.rm = TRUE),
    .groups    = "drop"
  )

## 3) Master-Tabelle aufbauen
master <- EKPO %>%
  left_join(ekko_head, by = "EBELN") %>%         # hat jetzt AEDAT_EKKO
  left_join(eket_head, by = c("EBELN", "EBELP")) %>%
  left_join(planned_qty_df, by = c("EBELN", "EBELP")) %>%
  left_join(gr_qty_df,      by = c("EBELN", "EBELP")) %>%
  left_join(return_qty_df,  by = c("EBELN", "EBELP")) %>%
  mutate(
    Erstelldatum  = AEDAT_EKKO,
    Lieferdatum   = EINDT,
    Durchlaufzeit = as.numeric(Lieferdatum - Erstelldatum),
    planned_qty       = coalesce(planned_qty, 0),
    goods_receipt_qty = coalesce(goods_receipt_qty, 0),
    return_qty        = coalesce(return_qty, 0)
  ) %>%
  select(
    EBELN, EBELP, MATNR, Erstelldatum, Lieferdatum, Durchlaufzeit,
    planned_qty, goods_receipt_qty, return_qty
  )

# für die Shiny-App weiterhin unter dem Namen 'orders'
orders <- master

## Farbdefinitionen fuer KPI-Balken
kpi_colors <- list(
  red    = "#e74c3c",
  yellow = "#f1c40f",
  blue   = "#3498db",
  green  = "#2ecc71",
  grey   = "#cccccc"
)

## Durchschnittswerte fuer IFR und OTD
avg_ifr <- with(orders, {
  fill_qty <- pmax(pmin(goods_receipt_qty - return_qty, planned_qty), 0)
  if (sum(planned_qty, na.rm = TRUE) > 0) {
    sum(fill_qty, na.rm = TRUE) / sum(planned_qty, na.rm = TRUE) * 100
  } else {
    NA_real_
  }
})

avg_otd <- {
  ekes_df <- EKES %>% rename(ETENR = ETENS)
  merged <- EKET %>%
    inner_join(EKPO %>% select(EBELN, EBELP), by = c("EBELN", "EBELP")) %>%
    inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
               by = c("EBELN", "EBELP", "ETENR")) %>%
    mutate(
      plannedDate   = as.Date(EINDT.x, format = "%d.%m.%Y"),
      confirmedDate = as.Date(EINDT.y, format = "%d.%m.%Y")
    )
  if (nrow(merged) > 0) {
    mean(merged$confirmedDate <= merged$plannedDate, na.rm = TRUE) * 100
  } else {
    NA_real_
  }
}

## 3) alle KPI-Funktionsdateien sourcen
lapply(list.files("kpi", "^calculate_.*\\.R$", full.names = TRUE), source)