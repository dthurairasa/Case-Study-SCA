timeline_delay <- function(material_id,
                           master_df,
                           EKET,
                           EKES,
                           po_filter = NULL,
                           positive_only = TRUE) {
  rows <- master_df %>% filter(MATNR == as.character(material_id))
  if (!is.null(po_filter)) {
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  }
  if (nrow(rows) == 0) return(NULL)
  po_keys <- rows %>% select(EBELN, EBELP, Lieferdatum)
  
  ekes_df <- EKES %>% rename(ETENR = ETENS)
  merged <- EKET %>%
    semi_join(po_keys, by = c("EBELN", "EBELP")) %>%
    inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
               by = c("EBELN", "EBELP", "ETENR")) %>%
    mutate(delay_days = as.numeric(as.Date(EINDT.y, "%d.%m.%Y") -
                                     as.Date(EINDT.x, "%d.%m.%Y"))) %>%
    left_join(po_keys, by = c("EBELN", "EBELP")) %>%
    select(Lieferdatum, delay_days)
  
  if (positive_only) {
    merged <- filter(merged, delay_days > 0)
  }
  
  merged
}