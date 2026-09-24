collect_category_label_inputs_from_table <- function(
  table_data,
  input,
  max_pairs = statedu_category_label_max_pairs()
) {
  if (is.null(table_data) || !is.data.frame(table_data) || !all(c("source_order", "name") %in% names(table_data))) {
    return(NULL)
  }

  fields <- category_label_edit_columns(max_pairs)
  collected <- list()
  for (row_index in seq_len(nrow(table_data))) {
    source_order <- as.character(table_data$source_order[[row_index]] %||% "")
    name <- as.character(table_data$name[[row_index]] %||% "")
    if (!nzchar(source_order) || !nzchar(name)) {
      next
    }

    row_values <- list()
    for (field in fields) {
      input_id <- if (identical(field, "var_label")) {
        paste0("category_var_label_input_", source_order)
      } else {
        paste0("category_", field, "_input_", source_order)
      }
      value <- input[[input_id]]
      if (is.null(value) || length(value) == 0) {
        next
      }
      row_values[[field]] <- as.character(value[[1]] %||% "")
    }

    if (length(row_values) > 0) {
      collected[[name]] <- row_values
    }
  }

  collected
}

