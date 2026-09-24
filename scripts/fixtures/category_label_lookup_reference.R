# Data helpers for categorical value label editing.

category_label_value_columns <- function(max_pairs = statedu_category_label_max_pairs()) {
  as.vector(rbind(paste0("value_", seq_len(max_pairs)), paste0("label_", seq_len(max_pairs))))
}

category_label_edit_columns <- function(max_pairs = statedu_category_label_max_pairs()) {
  c("var_label", "reference", "reference_label", category_label_value_columns(max_pairs))
}

category_label_save_columns <- function(max_pairs = statedu_category_label_max_pairs()) {
  c("reference", "reference_label", category_label_value_columns(max_pairs))
}

category_label_display_data <- function(
  info,
  selected_names = character(0),
  dependent = character(0),
  independent = character(0),
  controls = character(0),
  saved_values = NULL,
  measurement_overrides = character(0),
  max_pairs = statedu_category_label_max_pairs()
) {
  if (is.null(info) || nrow(info) == 0) {
    return(NULL)
  }

  info <- apply_measurement_overrides(info, measurement_overrides)
  info <- info[info$name %in% as.character(selected_names), , drop = FALSE]
  if (nrow(info) == 0) {
    return(data.frame(Message = "No categorical variables are selected.", check.names = FALSE))
  }
  info$selected <- TRUE
  info$role <- vapply(
    info$name,
    role_for_variable,
    character(1),
    dependent = dependent,
    independent = independent,
    controls = controls
  )
  info <- info[info$measurement %in% c("binary", "category", "ordered"), , drop = FALSE]
  if (nrow(info) == 0) {
    return(data.frame(Message = "No categorical variables are selected.", check.names = FALSE))
  }

  value_columns <- category_label_value_columns(max_pairs)
  edit_columns <- category_label_edit_columns(max_pairs)
  for (column in edit_columns) {
    if (!column %in% names(info)) {
      info[[column]] <- ""
    }
  }

  if (is.data.frame(saved_values) && "name" %in% names(saved_values)) {
    for (row_index in seq_len(nrow(info))) {
      saved_index <- match(info$name[[row_index]], saved_values$name)
      if (!is.na(saved_index)) {
        for (column in edit_columns) {
          if (column %in% names(saved_values)) {
            info[[column]][[row_index]] <- as.character(saved_values[[column]][[saved_index]] %||% "")
          }
        }
      }
    }
  }

  info[, c("source_order", "name", "var_label", "measurement", "n_unique", "reference", value_columns), drop = FALSE]
}
