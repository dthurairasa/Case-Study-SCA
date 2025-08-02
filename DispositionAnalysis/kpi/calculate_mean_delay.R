calculate_mean_delay <- function(material_id,
                                 master_df,
                                 EKET,
                                 EKES,
                                 po_filter     = NULL,
                                 positive_only = TRUE) {   # ← Default: nur Verspätungen
  tryCatch({
    
    # 0) Zeilen für das Material (+ optional PO-Filter)
    rows <- master_df %>% filter(MATNR == as.character(material_id))
    if (!is.null(po_filter)) rows <- semi_join(rows, po_filter, by = c("EBELN","EBELP"))
    if (nrow(rows) == 0) return(NA_real_)
    po_keys <- rows %>% select(EBELN, EBELP)
    
    # 1) EKET ↔ EKES joinen und Delay berechnen
    merged <- EKET %>%
      semi_join(po_keys, by = c("EBELN","EBELP")) %>%
      inner_join(
        EKES %>% select(EBELN, EBELP, ETENS, EINDT) %>% rename(ETENR = ETENS),
        by = c("EBELN","EBELP","ETENR")
      ) %>%
      mutate(
        plannedDate   = as.Date(trimws(EINDT.x),
                                tryFormats = c("%d.%m.%Y", "%Y-%m-%d")),
        confirmedDate = as.Date(trimws(EINDT.y),
                                tryFormats = c("%d.%m.%Y", "%Y-%m-%d"))
      ) %>%
      filter(!is.na(plannedDate) & !is.na(confirmedDate)) %>%  # nur valide Zeilen
      mutate(delay_days = as.numeric(confirmedDate - plannedDate))
    
    # Debug: wie viele Zeilen haben wir nach Parsing & Filter?
    message("#Zeilen nach Datums-Parse für MATNR ", material_id, ": ", nrow(merged))
    
    # 2) Nur positive Delays zulassen (falls gewünscht) 
    if (positive_only) merged <- filter(merged, delay_days > 0)
    
    # Fallback: Wenn nach Filter keine Verspätungen mehr übrig → 0 zurück
    if (positive_only && nrow(merged) == 0) return(0)
    
    # 3) Mittelwert zurückgeben
    mean_delay <- mean(merged$delay_days, na.rm = TRUE)
    if (is.nan(mean_delay)) return(NA_real_)
    return(mean_delay)
    
  }, error = function(e) {
    message(sprintf(
      "Fehler in calculate_mean_delay() für Material %s: %s",
      material_id, e$message)
    )
    NA_real_
  })
}