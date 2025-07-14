# --------------------------------------------------------------------
# Item Fill Rate (IFR) – Netto-Verfügbarkeit je Material
#  • Planmenge  = Σ EKET-MENGE je PO-Position
#  • Liefermenge = 101-Wareneingang  minus  102-Storno  (Netto)
#  • IFR        = min(Liefer, Plan) / Plan   (0-100 %)
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# material_id : Materialnummer (Char oder Numeric)
# EKET, EKBE, EKPO : bereits eingelesene Data-Frames aus global.R
# --------------------------------------------------------------------
calculate_ifr <- function(material_id,
                          EKET,
                          EKBE,
                          EKPO) {
  ## 0) Alle PO-Positionen für das Material ermitteln -----------------
  pos_keys <- EKPO %>%
    filter(MATNR == as.character(material_id)) %>%
    select(EBELN, EBELP)
  if (nrow(pos_keys) == 0)
    stop("Keine Bestellpositionen für Material ", material_id)
  
  ## 1) Planmenge ------------------------------------------------------
  planned <- EKET %>%
    inner_join(pos_keys, by = c("EBELN", "EBELP")) %>%
    group_by(EBELN, EBELP) %>%
    summarise(
      planned_qty = sum(parse_number(MENGE), na.rm = TRUE),
      .groups      = "drop"
    )
  
  ## 2) Netto-Liefermenge (101-S  –  102-H) ---------------------------
  delivered <- EKBE %>%
    filter(BWART %in% c("101", "102")) %>%          # nur 101 & 102
    inner_join(pos_keys, by = c("EBELN", "EBELP")) %>%
    mutate(
      sign = case_when(
        BWART == "101" & SHKZG == "S" ~  1,
        BWART == "102" & SHKZG == "H" ~ -1,
        TRUE                          ~  0
      )
    ) %>%
    group_by(EBELN, EBELP) %>%
    summarise(
      delivered_qty = sum(sign * parse_number(MENGE), na.rm = TRUE),
      .groups        = "drop"
    )
  
  ## 3) IFR je Position (0-100 %) -------------------------------------
  ifr_pos <- planned %>%
    left_join(delivered, by = c("EBELN", "EBELP")) %>%
    mutate(
      delivered_qty = coalesce(delivered_qty, 0),
      fill_qty      = pmax(pmin(delivered_qty, planned_qty), 0),
      ifr_pos       = fill_qty / planned_qty * 100
    )
  
  ## 4) Aggregiert auf Material ---------------------------------------
  ifr_material <- ifr_pos %>%
    summarise(
      total_planned   = sum(planned_qty,   na.rm = TRUE),
      total_delivered = sum(fill_qty,      na.rm = TRUE),
      ifr_raw         = total_delivered / total_planned * 100
    ) %>%
    pull(ifr_raw)
  
  ## 5) Ergebnis -------------------------------------------------------
  message(sprintf("Item Fill Rate für Material %s: %.2f %%",
                  material_id, ifr_material))
  return(ifr_material)
}