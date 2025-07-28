# On‑Time‑Delivery‑Rate (0‑1) für das ganze Unternehmen
calculate_otdr_company <- function(master_df,          # = orders
                                   po_filter = NULL) { # optional EBELN/EBELP‑Subset
  # 0) POs eingrenzen (falls Filter geliefert wurde)
  rows <- if (is.null(po_filter)) master_df
  else semi_join(master_df, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze nach dem Filter gefunden.")
  
  # 1) OTDR berechnen: bestätigtes ≤ geplantes Datum?
  otdr <- rows %>%
    summarise(
      otdr_value = mean(
        as.Date(EINDT.y, "%d.%m.%Y") <= as.Date(EINDT.x, "%d.%m.%Y"),
        na.rm = TRUE
      )
    ) %>% pull(otdr_value)
  
  message(sprintf("On‑Time‑Delivery‑Rate: %.2f %%", otdr * 100))
  return(otdr)          # 0–1‑Wert 
}