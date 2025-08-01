# --------------------------------------------------------------------
# Return Rate (RR) functions
#   - Return quantity = return_qty
#   - Goods receipt quantity = goods_receipt_qty
#   - RR = min(return_qty, goods_receipt_qty) / goods_receipt_qty * 100
# --------------------------------------------------------------------

# Calculate average return rate for one material
calculate_rr <- function(material_id,
                         master_df,        # = orders
                         po_filter = NULL) {
  rows <- master_df %>% filter(MATNR == as.character(material_id))
  if (!is.null(po_filter))
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  if (nrow(rows) == 0)
    stop("No data for material ", material_id)
  
  rows <- rows %>%
    mutate(rr_line = ifelse(goods_receipt_qty > 0,
                            pmin(return_qty, goods_receipt_qty) / goods_receipt_qty * 100,
                            NA_real_))
  
  rr <- mean(rows$rr_line, na.rm = TRUE)
  message(sprintf("Return Rate for material %s: %.2f %%", material_id, rr))
  return(rr)
}

# Detailed RR info (boxplot vector and timeline)
get_rr_details <- function(material_id,
                           master_df,
                           top_n = 25,
                           po_filter = NULL) {
  rows <- master_df %>%
    filter(MATNR == as.character(material_id)) %>%
    arrange(desc(Lieferdatum))
  if (!is.null(po_filter))
    rows <- semi_join(rows, po_filter, by = c("EBELN", "EBELP"))
  if (!is.null(top_n))
    rows <- slice_head(rows, n = top_n)
  if (nrow(rows) == 0)
    stop("No data for material ", material_id)
  
  rows <- rows %>%
    mutate(rr_line = ifelse(goods_receipt_qty > 0,
                            pmin(return_qty, goods_receipt_qty) / goods_receipt_qty * 100,
                            NA_real_))
  
  rr_value <- mean(rows$rr_line, na.rm = TRUE)
  
  list(
    rr           = rr_value,
    boxplot_vec  = rows$rr_line,
    timeline_tbl = rows %>% select(Lieferdatum, rr_line)
  )
}