calculate_mean_delay <- function(material_id,
                                 master_df,
                                 EKET,
                                 EKES,
                                 po_filter = NULL,
                                 positive_only = TRUE) {
  # Try-catch to ensure numeric output
  tryCatch({
    # 0) Zeilen für das gewünschte Material (+ optionaler PO-Filter)
    rows <- master_df %>% filter(MATNR == as.character(material_id))
    if (!is.null(po_filter)) {
      rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
    }
    if (nrow(rows) == 0) return(NA_real_)
    po_keys <- rows %>% select(EBELN, EBELP)
    
    # 1) Delay in Tagen berechnen über EKET/EKES
    ekes_df <- EKES %>% rename(ETENR = ETENS)
    merged <- EKET %>%
      semi_join(po_keys, by = c("EBELN", "EBELP")) %>%
      inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
                 by = c("EBELN", "EBELP", "ETENR")) %>%
      mutate(delay_days = as.numeric(
        as.Date(EINDT.y, "%d.%m.%Y") - as.Date(EINDT.x, "%d.%m.%Y")))
    if (positive_only) {
      merged <- filter(merged, delay_days > 0)
    }
    if (nrow(merged) == 0) return(NA_real_)
    
    # 2) Ø-Delay zurückgeben
    mean_delay <- mean(merged$delay_days, na.rm = TRUE)
    if (is.nan(mean_delay)) return(NA_real_)
    return(mean_delay)
  }, error = function(e) {
    message(sprintf("Fehler in calculate_mean_delay() für Material %s: %s", material_id, e$message))
    return(NA_real_)
  })
}