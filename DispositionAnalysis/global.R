### global.R --------------------------------------------------------
## 0) Pakete
libs <- c("shiny", "dplyr", "readxl", "tidyr", "Hmisc")
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


# ---------- 1) Planmenge beim Anlegen der PO einfrieren --------------
plan_orig <- EKPO %>%                      # nur 1 Zeile je EBELN/EBELP
  transmute(EBELN, EBELP,
            plan_qty_orig = parse_number(MENGE))

# ---------- 2) Netto-WE kumuliert (bis Termin & gesamt) --------------
gr <- EKBE %>% 
  mutate(qty = case_when(
    BWART == "101" & SHKZG == "S" ~  parse_number(MENGE),
    BWART == "102" & SHKZG == "H" ~ -parse_number(MENGE),
    TRUE                          ~  0)) %>% 
  arrange(EBELN, EBELP, BUDAT) %>% 
  group_by(EBELN, EBELP) %>% 
  mutate(cum_qty = cumsum(qty)) %>% 
  ungroup()

# bis Fälligkeitstermin:
gr_due <- gr %>% group_by(EBELN, EBELP) %>% 
  summarise(cum_qty_at_due = max(cum_qty), .groups="drop")

# kompletter Wareneingang:
gr_fin <- gr %>% group_by(EBELN, EBELP) %>% 
  summarise(cum_qty_final = last(cum_qty), .groups="drop")

# ---------- 3) Schedule-Lines IFR berechnen --------------------------
sched_ifr <- EKET %>% 
  inner_join(EKPO %>% select(EBELN, EBELP, MATNR), by=c("EBELN","EBELP")) %>% 
  left_join(plan_orig,     by=c("EBELN","EBELP")) %>% 
  left_join(gr_due,        by=c("EBELN","EBELP")) %>% 
  left_join(gr_fin,        by=c("EBELN","EBELP")) %>% 
  mutate(
    IFR_service  = pmin(cum_qty_at_due , plan_qty_orig) / plan_qty_orig * 100,
    IFR_complete = pmin(cum_qty_final  , plan_qty_orig) / plan_qty_orig * 100
  )


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

avg_rr <- with(orders, {
  if (sum(goods_receipt_qty, na.rm = TRUE) > 0) {
    sum(pmin(return_qty, goods_receipt_qty), na.rm = TRUE) /
      sum(goods_receipt_qty, na.rm = TRUE) * 100
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
source(file.path("kpi", "ifr_helpers.R"))
lapply(list.files("kpi", "^calculate_.*\\.R$", full.names = TRUE), source)
source(file.path("kpi", "timeline_delay.R"))
