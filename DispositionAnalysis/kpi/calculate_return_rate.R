calculate_return_rate <- function(material_id,
                                  master_df,
                                  po_filter = NULL) {
  rows <- master_df %>% dplyr::filter(MATNR == as.character(material_id))
  if (!is.null(po_filter))
    rows <- dplyr::semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze für Material ", material_id)
  
  total_return <- sum(rows$return_qty, na.rm = TRUE)
  total_receipt <- sum(rows$goods_receipt_qty, na.rm = TRUE)
  if (total_receipt == 0)
    return(NA_real_)
  
  rate <- total_return / total_receipt * 100
  message(sprintf("Return Rate für Material %s: %.2f %%", material_id, rate))
  rate
}