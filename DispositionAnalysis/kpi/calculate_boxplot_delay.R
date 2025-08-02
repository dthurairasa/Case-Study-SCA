# ▸ Boxplot der Verspätungen für EIN Material
boxplot_delay <- function(material_id,
                          master_df,          # = orders
                          EKET,
                          EKES,
                          po_filter = NULL,   # optional EBELN/EBELP‑Subset
                          positive_only = TRUE) {
  
  # 0) Zeilen für das Material holen (+ optionaler PO‑Filter)
  rows <- master_df %>% filter(MATNR == as.character(material_id))
  if (!is.null(po_filter))
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Kein Delay‑Datensatz für Material ", material_id)
  po_keys <- rows %>% select(EBELN, EBELP)
  
  # 1) Verspätung berechnen über EKET/EKES
  ekes_df <- EKES %>% rename(ETENR = ETENS)
  rows <- EKET %>%
    semi_join(po_keys, by = c("EBELN", "EBELP")) %>%
    inner_join(ekes_df %>% select(EBELN, EBELP, ETENR, EINDT),
               by = c("EBELN", "EBELP", "ETENR")) %>%
    mutate(delay_days = as.numeric(
      as.Date(EINDT.y, "%d.%m.%Y") - as.Date(EINDT.x, "%d.%m.%Y")))
  
  if (positive_only) {
    rows <- filter(rows, delay_days > 0)
  }
  
  # 2) Box‑&‑Whisker‑Plot zurückgeben
  ggplot(rows, aes(x = "", y = delay_days)) +
    geom_boxplot(fill = "grey85", width = .25, outlier.shape = NA) +
    geom_jitter(width = .1, colour = "steelblue", size = 2, alpha = .7) +
    labs(
      title = paste("Delay Distribution – Material", material_id),
      y     = "Delay in days",
      x     = NULL
    ) +
    theme_minimal() +
    coord_flip()
}
