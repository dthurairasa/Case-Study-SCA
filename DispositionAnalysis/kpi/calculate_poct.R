# --------------------------------------------------------------------
# Purchase Order Cycle Time (POCT)
#  - mittlere Zeitspanne von Bestellanlage (AEDAT) bis erstem 101-Wareneingang
# --------------------------------------------------------------------


# 2) Purchase Order Cycle Time (POCT)
calculate_poct <- function(
    material_id,
    EKPO,
    EKKO,
    EKBE,
    goods_indicator = "1",  # Code für Wareneingang in EKBE$VGABE
    po_filter = NULL
) {
  
  ebelns <- EKPO %>%
    filter(MATNR == as.character(material_id)) %>%
    { if (!is.null(po_filter)) semi_join(., po_filter, by = c("EBELN", "EBELP")) else . } %>%
    pull(EBELN) %>% unique()
  if(length(ebelns)==0) stop("Keine Bestellungen für Material ", material_id)
  
  headers <- EKKO %>%
    filter(EBELN %in% ebelns) %>%
    mutate(orderDate = as.Date(AEDAT, format = "%d.%m.%Y")) %>%
    select(EBELN, orderDate)
  if(nrow(headers)==0) stop("Keine Bestelldaten in EKKO für Material ", material_id)
  
  receipts <- EKBE %>%
    filter(EBELN %in% ebelns, VGABE == goods_indicator) %>%
    mutate(receiptDate = as.Date(BUDAT, format = "%d.%m.%Y")) %>%
    group_by(EBELN) %>%
    summarise(firstReceipt = min(receiptDate, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(firstReceipt))
  if(nrow(receipts)==0) stop("Keine Wareneingänge (VGABE='", goods_indicator,
                             "') für Material ", material_id)
  
  cycle <- headers %>%
    inner_join(receipts, by = "EBELN") %>%
    mutate(cycleDays = as.numeric(firstReceipt - orderDate))
  
  avgCycle <- mean(cycle$cycleDays, na.rm = TRUE)
  message(sprintf("POCT für Material %s: %.1f Tage", material_id, avgCycle))
  return(avgCycle)
}
