# --------------------------------------------------------------------
# IFR-Detailauswertung inkl. Ampel & *logischer* Handlungsempfehlung
# --------------------------------------------------------------------
get_ifr_details <- function(material_id,
                            master_df,
                            top_n       = 25,   # NULL → alle Zeilen
                            target_ifr  = 95,   # Ziel-Service-Level in %
                            min_up_pct  = 5,    # nie weniger als …
                            max_up_pct  = 200)  # nie mehr als …
{
  ## -- 0  Filter ------------------------------------------------------
  rows <- master_df |>
    dplyr::filter(MATNR == material_id, planned_qty > 0) |>
    dplyr::arrange(dplyr::desc(Lieferdatum))
  if (!is.null(top_n)) rows <- dplyr::slice_head(rows, n = top_n)
  stopifnot(nrow(rows) > 0)
  
  ## -- 1  IFR & Shortage pro Zeile -----------------------------------
  rows <- rows |>
    dplyr::mutate(
      net_recv  = pmax(goods_receipt_qty - return_qty, 0),
      fill_qty  = pmin(net_recv, planned_qty),
      ifr_line  = fill_qty / planned_qty * 100,
      shortage  = 1 - ifr_line / 100            # 0 = voll, 1 = nix
    )
  
  ## -- 2  Kennzahlen gesamt ------------------------------------------
  item_ifr    <- mean(rows$ifr_line, na.rm = TRUE)
  
  avg_ifr_all <- master_df |>
    dplyr::mutate(
      net_recv = pmax(goods_receipt_qty - return_qty, 0),
      fill_qty = pmin(net_recv, planned_qty),
      ifr_line = fill_qty / planned_qty * 100) |>
    dplyr::summarise(avg = mean(ifr_line, na.rm = TRUE)) |>
    dplyr::pull()
  
  flag <- dplyr::case_when(
    item_ifr <  (avg_ifr_all - 10) ~ "red",
    item_ifr <= (avg_ifr_all - 5)  ~ "yellow",
    item_ifr <= (avg_ifr_all + 5)  ~ "blue",
    TRUE                           ~ "green")
  
  ## -- 3  Empfehlung --------------------------------------------------
  if (item_ifr > 0) {
    # Zielfaktor (z.B. 95/80 = 1.1875)
    safety_factor <- target_ifr / item_ifr
    
    raw_rec <- (safety_factor - 1) * 100           # in Prozent ausdrücken
  } else {
    raw_rec <- Inf                                  # Sonderfall IFR = 0
  }
  
  rec_pct <- round(
    pmax(min_up_pct, raw_rec),                      # nie unter Mindestzuschlag
    1)
  
  recommendation <- if (item_ifr < target_ifr) {
    sprintf("≈ %.1f %% Sicherheitszuschlag einplanen, um künftig ≥ %s %% IFR zu erreichen.",
            rec_pct, target_ifr)
  } else {
    "✓ IFR-Ziel bereits erreicht – keine Maßnahme nötig."
  }
  
  ## -- 4  Rückgabe ----------------------------------------------------
  list(
    item_ifr       = item_ifr,
    avg_ifr_all    = avg_ifr_all,
    deviation_pct  = item_ifr - avg_ifr_all,
    flag           = flag,
    recommendation = recommendation,
    boxplot_vec    = rows$ifr_line,
    timeline_tbl   = rows |> dplyr::select(Lieferdatum, ifr_line)
  )
}