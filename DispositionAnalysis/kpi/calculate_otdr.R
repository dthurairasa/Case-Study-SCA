library(dplyr)

# Funktion zur Berechnung der On-Time Delivery Rate (OTDR) für ein Material

calculate_otdr <- function(
    material_id,                   # Materialnummer als Zeichen, z.B. "310586"
    EKET,
    EKES,
    EKPO,
    po_filter = NULL
) {
  # 1) MATNR aus EKPO an EKET koppeln
  eket_mat <- EKET %>%
    inner_join(
      EKPO %>% select(EBELN, EBELP, MATNR),
      by = c("EBELN", "EBELP")
    )
  if (!is.null(po_filter)) {
    eket_mat <- semi_join(eket_mat, po_filter, by = c("EBELN", "EBELP"))
  }
  
  # 2) Nach dem ausgewählten Material filtern
  eket_mat <- eket_mat %>%
    filter(MATNR == as.character(material_id))
  if (nrow(eket_mat) == 0) {
    stop("Kein Schedule-Line-Datensatz für Material ", material_id, " gefunden")
  }
  
  # 3) Bestätigungen umbenennen und joinen (ETENS -> ETENR)
  ekes_mat <- EKES %>% 
    rename(ETENR = ETENS)
  
  merged <- eket_mat %>%
    inner_join(
      ekes_mat %>% select(EBELN, EBELP, ETENR, EINDT),
      by = c("EBELN", "EBELP", "ETENR")
    )
  if (nrow(merged) == 0) {
    stop("Keine Bestätigungs-Daten für die gefilterten Schedule-Lines gefunden")
  }
  
  # 4) Datumsfelder parsen (Format TT.MM.JJJJ)
  merged <- merged %>%
    mutate(
      plannedDate   = as.Date(EINDT.x, format = "%d.%m.%Y"),
      confirmedDate = as.Date(EINDT.y, format = "%d.%m.%Y"),
      on_time       = confirmedDate <= plannedDate            # ACHTUNG: habe ich eingefügt (Anton)
    )
  
  # 5) On-Time-Flag setzen (TRUE wenn bestätigt ≤ geplant)
  merged <- merged %>%
    mutate(on_time = confirmedDate <= plannedDate)
  
  # 6) OTDR berechnen
  rate <- mean(merged$on_time, na.rm = TRUE)
  
  # 7) Ergebnis ausgeben
  message(sprintf(
    "On-Time Delivery Rate für Material %s: %.1f%%",
    material_id, rate * 100
  ))
  
  return(rate)
}
