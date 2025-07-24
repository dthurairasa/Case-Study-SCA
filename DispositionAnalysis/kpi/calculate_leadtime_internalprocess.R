# Berechnet die mittlere Zeit (in Tagen) von Bedarfsmeldung bis Bestellung.
calculate_lead_time <- function(
    material_id,
    EBAN,
    EKKO,
    po_filter = NULL
) {
  
  # Requisitions für MATNR
  reqs <- EBAN %>%
    filter(MATNR == as.character(material_id), EBELN != "") %>%
    { if (!is.null(po_filter)) semi_join(., po_filter, by = c("EBELN", "EBELP")) else . } %>%
    mutate(requestDate = as.Date(ERDAT, format = "%d.%m.%Y")) %>%
    select(EBELN, requestDate)
  if (nrow(reqs) == 0) stop("Keine Requisitions mit Bestellungen für Material ", material_id)
  
  # Bestelldatum parsen
  headers <- EKKO %>%
    filter(EBELN %in% reqs$EBELN) %>%
    mutate(orderDate = as.Date(AEDAT, format = "%d.%m.%Y")) %>%
    select(EBELN, orderDate)
  if (nrow(headers) == 0) stop("Keine Bestelldaten (EKKO) für gefundene Requisitions")
  
  # Lead Time berechnen & Durchschnitt
  lead <- reqs %>% inner_join(headers, by = "EBELN") %>%
    mutate(leadDays = as.numeric(orderDate - requestDate))
  avgLead <- mean(lead$leadDays, na.rm = TRUE)
  message(sprintf("Lead Time für Material %s: %.1f Tage", material_id, avgLead))
  return(avgLead)
}
