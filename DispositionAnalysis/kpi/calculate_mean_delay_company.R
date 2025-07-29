# Ø‑Verspätung in Tagen
calculate_mean_delay_company <- function(master_df,          # = orders
                                         po_filter = NULL,  # optional EBELN/EBELP-Subset
                                         positive_only = FALSE) {
  # 0) Zeilen einschränken (PO‑Filter)
  rows <- if (is.null(po_filter)) master_df
  else semi_join(master_df, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze nach dem Filter gefunden.")
  
  # 1) Verspätung berechnen (EINDT.x = geplant, EINDT.y = bestätigt)
  rows <- rows %>% mutate(
    delay_days = as.numeric(as.Date(EINDT.y, "%d.%m.%Y") -
                              as.Date(EINDT.x, "%d.%m.%Y"))
  )
  if (positive_only)
    rows <- filter(rows, delay_days > 0)
  
  # 2) Ø‑Delay zurückgeben
  mean(rows$delay_days, na.rm = TRUE)
}