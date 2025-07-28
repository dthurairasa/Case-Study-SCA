calculate_mean_delay <- function(material_id,
                                 master_df,
                                 po_filter = NULL,
                                 positive_only = FALSE) {
  # Try-catch to ensure numeric output
  tryCatch({
    # 0) Zeilen für das gewünschte Material (+ optionaler PO-Filter)
    rows <- master_df %>% filter(MATNR == as.character(material_id))
    if (!is.null(po_filter)) {
      rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
    }
    if (nrow(rows) == 0) return(NA_real_)
    
    # 1) Delay in Tagen berechnen
    rows <- rows %>%
      mutate(delay_days = as.numeric(
        as.Date(EINDT.y, "%d.%m.%Y") - as.Date(EINDT.x, "%d.%m.%Y"))
      )
    if (positive_only) {
      rows <- filter(rows, delay_days > 0)
    }
    if (nrow(rows) == 0) return(NA_real_)
    
    # 2) Ø-Delay zurückgeben
    mean_delay <- mean(rows$delay_days, na.rm = TRUE)
    if (is.nan(mean_delay)) return(NA_real_)
    return(mean_delay)
  }, error = function(e) {
    message(sprintf("Fehler in calculate_mean_delay() für Material %s: %s", material_id, e$message))
    return(NA_real_)
  })
}
