calculate_return_rate_company <- function(master_df,
                                          po_filter = NULL) {
  rows <- if (is.null(po_filter)) master_df
  else dplyr::semi_join(master_df, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("Keine Datensätze nach dem Filter gefunden.")
  
  total_return <- sum(rows$return_qty, na.rm = TRUE)
  total_receipt <- sum(rows$goods_receipt_qty, na.rm = TRUE)
  if (total_receipt == 0)
    return(NA_real_)
  
  rate <- total_return / total_receipt * 100
  message(sprintf("Return Rate Company: %.2f %%", rate))
  rate
}