# kpi/calculate_ifr.R  (ersetzt die alte Version)
# --------------------------------------------------
# Item Fill Rate (IFR) auf Basis der Master-Tabelle  'orders'
#  • Planmenge   = planned_qty
#  • Netto-Liefermenge = goods_receipt_qty – return_qty
#  • IFR         = min(Netto, Plan) / Plan   (0-100 %)

calculate_ifr <- function(material_id,
                          master_df,          # = orders
                          po_filter = NULL) { # optional: DF mit EBELN / EBELP
  
  # 0) Zeilen für das Material holen (+ optionaler PO-Filter)
  rows <- master_df %>% filter(MATNR == as.character(material_id))
  if (!is.null(po_filter))
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze für Material ", material_id)
  
  # 1) Netto-Liefermenge & gekappte Liefermenge
  rows <- rows %>%
    mutate(netto_qty = goods_receipt_qty - return_qty,
           fill_qty  = pmin(netto_qty, planned_qty))   # Überlieferung kappen
  
  # 2) Aggregation auf Materialebene
  ifr <- rows %>% summarise(
    total_planned   = sum(planned_qty),
    total_delivered = sum(fill_qty),
    ifr_value       = total_delivered / total_planned * 100
  ) %>% pull(ifr_value)
  
  message(sprintf("Item Fill Rate für Material %s: %.2f %%",
                  material_id, ifr))
  return(ifr)
}