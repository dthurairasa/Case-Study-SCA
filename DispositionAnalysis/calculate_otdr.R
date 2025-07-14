library(dplyr)

# Funktion zur Berechnung der On-Time Delivery Rate (OTDR) für ein Material

calculate_otdr <- function(
    material_id,                   # Materialnummer als Zeichen, z.B. "310586"
    file_eket = "EKET.csv",      # Dateiname für EKET-Daten
    file_ekes = "EKES.csv",      # Dateiname für EKES-Daten
    file_ekpo = "EKPO.csv"       # Dateiname für EKPO-Daten
) {
  # 1) CSV-Dateien einlesen
  eket <- read.csv2(file_eket, stringsAsFactors = FALSE, dec = ",", sep = ";")
  ekes <- read.csv2(file_ekes, stringsAsFactors = FALSE, dec = ",", sep = ";")
  ekpo <- read.csv2(file_ekpo, stringsAsFactors = FALSE, dec = ",", sep = ";")
  
  # 2) MATNR aus EKPO an EKET koppeln
  eket_mat <- eket %>%
    inner_join(
      ekpo %>% select(EBELN, EBELP, MATNR),
      by = c("EBELN", "EBELP")
    )
  
  # 3) Nach dem ausgewählten Material filtern
  eket_mat <- eket_mat %>%
    filter(MATNR == as.character(material_id))
  if (nrow(eket_mat) == 0) {
    stop("Kein Schedule-Line-Datensatz für Material ", material_id, " gefunden")
  }
  
  # 4) Bestätigungen umbenennen und joinen (ETENS -> ETENR)
  ekes_mat <- ekes %>% 
    rename(ETENR = ETENS)
  
  merged <- eket_mat %>%
    inner_join(
      ekes_mat %>% select(EBELN, EBELP, ETENR, EINDT),
      by = c("EBELN", "EBELP", "ETENR")
    )
  if (nrow(merged) == 0) {
    stop("Keine Bestätigungs-Daten für die gefilterten Schedule-Lines gefunden")
  }
  
  # 5) Datumsfelder parsen (Format TT.MM.JJJJ)
  merged <- merged %>%
    mutate(
      plannedDate   = as.Date(EINDT.x, format = "%d.%m.%Y"),
      confirmedDate = as.Date(EINDT.y, format = "%d.%m.%Y")
    )
  
  # 6) On-Time-Flag setzen (TRUE wenn bestätigt ≤ geplant)
  merged <- merged %>%
    mutate(on_time = confirmedDate <= plannedDate)
  
  # 7) OTDR berechnen
  rate <- mean(merged$on_time, na.rm = TRUE)
  
  # 8) Ergebnis ausgeben
  message(sprintf(
    "On-Time Delivery Rate für Material %s: %.1f%%",
    material_id, rate * 100
  ))
  
  return(rate)
}


calculate_otdr("310586")