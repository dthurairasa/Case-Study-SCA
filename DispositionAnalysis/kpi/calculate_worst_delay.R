calculate_worst_delay <- function(material_id, master_df, EKET, EKES, po_filter = NULL) {
  rows <- master_df %>% filter(MATNR == as.character(material_id))
  if (!is.null(po_filter)) {
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  }
  if (nrow(rows) == 0) return(NA_real_)
  po_keys <- rows %>% select(EBELN, EBELP)
  
  ekes_df <- EKES %>% rename(ETENR = ETENS)
  merged <- EKET %>%
    semi_join(po_keys, by = c("EBELN", "EBELP")) %>%
    inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
               by = c("EBELN", "EBELP", "ETENR")) %>%
    mutate(delay_days = as.numeric(as.Date(EINDT.y, "%d.%m.%Y") - as.Date(EINDT.x, "%d.%m.%Y")))
  if (nrow(merged) == 0) return(NA_real_)
  max(merged$delay_days, na.rm = TRUE)
}