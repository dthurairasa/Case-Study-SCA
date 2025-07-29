# On‑Time‑Delivery‑Rate (0‑1) für das ganze Unternehmen
calculate_otdr_company <- function(master_df,          # = orders
                                   EKET,
                                   EKES,
                                   po_filter = NULL) { # optional EBELN/EBELP‑Subset
  # 0) POs eingrenzen (falls Filter geliefert wurde)
  rows <- if (is.null(po_filter)) master_df
  else semi_join(master_df, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze nach dem Filter gefunden.")
  
  # 1) Schedule-Keys bestimmen
  po_keys <- rows %>% select(EBELN, EBELP)
  
  # 2) EKET/EKES mergen und On-Time prüfen
  ekes_df <- EKES %>% rename(ETENR = ETENS)
  merged <- EKET %>%
    semi_join(po_keys, by = c("EBELN", "EBELP")) %>%
    inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
               by = c("EBELN", "EBELP", "ETENR")) %>%
    mutate(
      plannedDate   = as.Date(EINDT.x, "%d.%m.%Y"),
      confirmedDate = as.Date(EINDT.y, "%d.%m.%Y")
    )
  
  # 3) OTDR berechnen: bestätigtes ≤ geplantes Datum?
  otdr <- mean(merged$confirmedDate <= merged$plannedDate, na.rm = TRUE)
  
  message(sprintf("On‑Time‑Delivery‑Rate: %.2f %%", otdr * 100))
  return(otdr)          # 0–1‑Wert 
}