# Berechnet die mittlere Zeit (in Tagen) von Bedarfsmeldung bis Bestellung.
calculate_lead_time <- function(
    material_id,
    file_eban = "EBAN.csv",   # CSV mit Requisition-Daten
    file_ekko = "EKKO.csv"    # CSV mit PO Header (enthält AEDAT)
) {
  # Einlesen
  eban <- read.csv2(file_eban, stringsAsFactors = FALSE, dec = ",", sep = ";")
  ekko <- read.csv2(file_ekko, stringsAsFactors = FALSE, dec = ",", sep = ";")
  
  # Requisitions für MATNR
  reqs <- eban %>%
    filter(MATNR == as.character(material_id), EBELN != "") %>%
    mutate(requestDate = as.Date(ERDAT, format = "%d.%m.%Y")) %>%
    select(EBELN, requestDate)
  if (nrow(reqs) == 0) stop("Keine Requisitions mit Bestellungen für Material ", material_id)
  
  # Bestelldatum parsen
  headers <- ekko %>%
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



calculate_lead_time("310586")