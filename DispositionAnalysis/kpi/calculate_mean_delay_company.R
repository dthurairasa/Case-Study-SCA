# Ø-Verspätung in Tagen für das gesamte Unternehmen
calculate_mean_delay_company <- function(master_df,
                                         EKET,
                                         EKES,
                                         po_filter = NULL,
                                         positive_only = TRUE) {
  rows <- if (is.null(po_filter)) master_df
  else semi_join(master_df, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze nach dem Filter gefunden.")
  po_keys <- rows %>% select(EBELN, EBELP)
  
  ekes_df <- EKES %>% rename(ETENR = ETENS)
  merged <- EKET %>%
    semi_join(po_keys, by = c("EBELN", "EBELP")) %>%
    inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
               by = c("EBELN", "EBELP", "ETENR")) %>%
    mutate(delay_days = as.numeric(
      as.Date(EINDT.y, "%d.%m.%Y") - as.Date(EINDT.x, "%d.%m.%Y")))
  if (positive_only)
    merged <- filter(merged, delay_days > 0)
  
  mean(merged$delay_days, na.rm = TRUE)
}