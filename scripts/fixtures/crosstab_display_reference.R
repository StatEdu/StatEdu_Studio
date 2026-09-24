crosstab_display_reference <- function(tab, row_var, col_var, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  row_labels <- crosstab_value_labels(row_var, rownames(tab), category_table)
  col_labels <- crosstab_value_labels(col_var, colnames(tab), category_table)
  row_percent <- crosstab_percent_matrix(tab, "row")
  col_percent <- crosstab_percent_matrix(tab, "column")
  total_percent <- crosstab_percent_matrix(tab, "total")
  show_total_n <- !identical(options$total_n, FALSE)

  rows <- list()
  for (row_index in seq_len(nrow(tab))) {
    out <- list(Row = row_labels[[row_index]])
    for (col_index in seq_len(ncol(tab))) {
      pieces <- as.character(tab[row_index, col_index])
      if (isTRUE(options$row_percent)) {
        pieces <- c(pieces, paste0("row ", crosstab_format_number(row_percent[row_index, col_index], 1), "%"))
      }
      if (isTRUE(options$column_percent)) {
        pieces <- c(pieces, paste0("col ", crosstab_format_number(col_percent[row_index, col_index], 1), "%"))
      }
      if (isTRUE(options$total_percent)) {
        pieces <- c(pieces, paste0("total ", crosstab_format_number(total_percent[row_index, col_index], 1), "%"))
      }
      out[[col_labels[[col_index]]]] <- paste(pieces, collapse = "\n")
    }
    if (isTRUE(show_total_n)) {
      out[["Total"]] <- as.character(rowSums(tab)[[row_index]])
    }
    rows[[length(rows) + 1]] <- out
  }
  total_row <- c(list(Row = "Total"), stats::setNames(as.list(as.character(colSums(tab))), col_labels))
  if (isTRUE(show_total_n)) {
    total_row <- c(total_row, list(Total = as.character(sum(tab))))
  }
  rows[[length(rows) + 1]] <- total_row
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE))
}
