# Ø‑Verspätung für EIN Material (in Tagen)
calculate_mean_delay <- function(material_id,
                                 master_df,          # = orders (enthält MATNR, EINDT.x, EINDT.y …)
                                 po_filter = NULL,   # optional: EBELN/EBELP‑Subset
                                 positive_only = FALSE) {
  
  # 0) Zeilen für das gewünschte Material (+ optionaler PO‑Filter)
  rows <- master_df %>% filter(MATNR == as.character(material_id))
  if (!is.null(po_filter))
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Schedule‑Lines für Material ", material_id)
  
  # 1) Delay in Tagen berechnen
  rows <- rows %>%
    mutate(delay_days = as.numeric(
      as.Date(EINDT.y, "%d.%m.%Y") - as.Date(EINDT.x, "%d.%m.%Y"))
    )
  if (positive_only)
    rows <- filter(rows, delay_days > 0)
  if (nrow(rows) == 0)
    stop("Keine gültigen Delay‑Daten für Material ", material_id)
  
  # 2) Ø‑Delay zurückgeben
  mean_delay <- mean(rows$delay_days, na.rm = TRUE)
  message(sprintf("Mean Delay für Material %s: %.1f Tage", material_id, mean_delay))
  return(mean_delay)
}