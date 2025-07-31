get_rr_details <- function(material_id,
                           master_df,
                           top_n = 25) {
  rows <- master_df |>
    dplyr::filter(MATNR == material_id) |>
    dplyr::arrange(dplyr::desc(Lieferdatum))
  if (!is.null(top_n)) rows <- dplyr::slice_head(rows, n = top_n)
  stopifnot(nrow(rows) > 0)
  
  rows <- rows |>
    dplyr::mutate(
      rr_line = ifelse(goods_receipt_qty > 0,
                       return_qty / goods_receipt_qty * 100,
                       NA_real_)
    )
  
  item_rr <- mean(rows$rr_line, na.rm = TRUE)
  
  avg_rr_all <- master_df |>
    dplyr::filter(goods_receipt_qty > 0) |>
    dplyr::mutate(rr_line = return_qty / goods_receipt_qty * 100) |>
    dplyr::summarise(avg = mean(rr_line, na.rm = TRUE)) |>
    dplyr::pull()
  
  list(
    item_rr      = item_rr,
    avg_rr_all   = avg_rr_all,
    boxplot_vec  = rows$rr_line,
    timeline_tbl = rows |> dplyr::select(Lieferdatum, rr_line)
  )
}