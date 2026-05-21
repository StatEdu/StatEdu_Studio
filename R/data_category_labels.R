# Data helpers for categorical value label editing.

category_label_value_columns <- function(max_pairs = 6) {
  as.vector(rbind(paste0("value_", seq_len(max_pairs)), paste0("label_", seq_len(max_pairs))))
}

category_label_edit_columns <- function(max_pairs = 6) {
  c("var_label", "reference", "reference_label", category_label_value_columns(max_pairs))
}

category_label_save_columns <- function(max_pairs = 6) {
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
  max_pairs = 6
) {
  if (is.null(info) || nrow(info) == 0) {
    return(NULL)
  }

  info <- apply_measurement_overrides(info, measurement_overrides)
  info <- info[info$name %in% as.character(selected_names), , drop = FALSE]
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

selected_variable_summary_data <- function(
  info,
  selected_names = character(0),
  saved_values = NULL,
  measurement_overrides = character(0)
) {
  if (is.null(info) || nrow(info) == 0) {
    return(data.frame(Message = "No variables are selected.", check.names = FALSE))
  }

  info <- apply_measurement_overrides(info, measurement_overrides)
  info <- info[info$name %in% as.character(selected_names), , drop = FALSE]
  if (nrow(info) == 0) {
    return(data.frame(Message = "No variables are selected.", check.names = FALSE))
  }

  if (!"reference" %in% names(info)) {
    info$reference <- ""
  }
  if (is.data.frame(saved_values) && all(c("name", "reference") %in% names(saved_values))) {
    matched <- match(info$name, saved_values$name)
    has_saved <- !is.na(matched)
    info$reference[has_saved] <- as.character(saved_values$reference[matched[has_saved]] %||% "")
  }
  if ("measurement" %in% names(info)) {
    info$reference[!info$measurement %in% c("binary", "category", "ordered")] <- ""
  }

  output <- data.frame(
    Variable = as.character(info$name),
    Label = as.character(info$var_label %||% ""),
    Measurement = ifelse(as.character(info$measurement) == "ordered", "ordinal", as.character(info$measurement)),
    Reference = as.character(info$reference %||% ""),
    Min = as.character(info$min_value %||% ""),
    Max = as.character(info$max_value %||% ""),
    Missing = as.character(info$n_missing %||% ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  output
}

category_label_seed_table <- function(base, max_pairs = 6) {
  edit_columns <- category_label_edit_columns(max_pairs)
  if (is.null(base) || !"name" %in% names(base)) {
    return(NULL)
  }
  base[, c("name", edit_columns), drop = FALSE]
}

normalize_category_label_table <- function(table, columns, base = NULL) {
  if (!is.data.frame(table)) {
    if (!is.null(base) && is.data.frame(base) && "name" %in% names(base)) {
      table <- base[, intersect(c("name", columns), names(base)), drop = FALSE]
    } else {
      table <- data.frame(name = character(0), stringsAsFactors = FALSE, check.names = FALSE)
    }
  }

  if (!"name" %in% names(table)) {
    table$name <- character(nrow(table))
  }

  for (column in columns) {
    if (!column %in% names(table)) {
      table[[column]] <- rep("", nrow(table))
    }
  }

  table[, c("name", columns), drop = FALSE]
}

new_category_label_row <- function(name, columns) {
  as.data.frame(
    as.list(c(name = name, stats::setNames(rep("", length(columns)), columns))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

update_category_label_table <- function(table, base, name, field, value, max_pairs = 6) {
  edit_columns <- category_label_edit_columns(max_pairs)
  if (!nzchar(name) || !field %in% edit_columns) {
    return(list(table = table, changed = FALSE, var_label_update = NULL, ok = FALSE))
  }

  table <- normalize_category_label_table(table, edit_columns, base)
  if (!is.data.frame(table)) {
    return(list(table = table, changed = FALSE, var_label_update = NULL, ok = FALSE))
  }

  if (!name %in% table$name) {
    table <- rbind(table, new_category_label_row(name, edit_columns))
  }

  row_index <- match(name, table$name)
  value <- as.character(value)
  changed <- !identical(as.character(table[[field]][[row_index]] %||% ""), value)
  table[[field]][row_index] <- value
  var_label_update <- if (identical(field, "var_label")) stats::setNames(value, name) else NULL

  if (identical(field, "reference")) {
    reference <- trimws(as.character(value))
    reference_label <- ""
    if (nzchar(reference)) {
      for (i in seq_len(max_pairs)) {
        if (identical(trimws(as.character(table[[paste0("value_", i)]][[row_index]])), reference)) {
          reference_label <- as.character(table[[paste0("label_", i)]][[row_index]] %||% "")
          break
        }
      }
    }
    changed <- changed || !identical(as.character(table$reference_label[[row_index]] %||% ""), reference_label)
    table$reference_label[[row_index]] <- reference_label
  }

  list(table = table, changed = changed, var_label_update = var_label_update, ok = TRUE)
}

merge_category_label_save_request <- function(current, incoming, base = NULL, max_pairs = 6) {
  value_columns <- category_label_save_columns(max_pairs)
  if (is.null(incoming)) {
    return(current)
  }

  current <- normalize_category_label_table(current, value_columns, base)

  for (name in names(incoming)) {
    if (!nzchar(trimws(as.character(name %||% "")))) {
      next
    }
    if (!name %in% current$name) {
      current <- rbind(current, new_category_label_row(name, value_columns))
    }
    row_index <- match(name, current$name)
    for (field in intersect(names(incoming[[name]]), value_columns)) {
      current[[field]][[row_index]] <- as.character(incoming[[name]][[field]] %||% "")
    }
  }

  current
}

apply_category_label_snapshot <- function(current, incoming, base = NULL, max_pairs = 6) {
  edit_columns <- category_label_edit_columns(max_pairs)
  if (is.null(incoming)) {
    return(list(table = current, changed = FALSE, var_label_updates = character(0)))
  }

  current <- normalize_category_label_table(current, edit_columns, base)
  var_label_updates <- character(0)
  changed <- FALSE

  for (name in names(incoming)) {
    name <- trimws(as.character(name %||% ""))
    if (!nzchar(name)) {
      next
    }
    if (!name %in% current$name) {
      current <- rbind(current, new_category_label_row(name, edit_columns))
      changed <- TRUE
    }

    row_index <- match(name, current$name)
    for (field in intersect(names(incoming[[name]]), edit_columns)) {
      value <- as.character(incoming[[name]][[field]] %||% "")
      if (!identical(as.character(current[[field]][[row_index]] %||% ""), value)) {
        current[[field]][[row_index]] <- value
        changed <- TRUE
      }
      if (identical(field, "var_label")) {
        var_label_updates[[name]] <- value
      }
    }
  }

  for (row_index in seq_len(nrow(current))) {
    reference <- trimws(as.character(current$reference[[row_index]] %||% ""))
    reference_label <- ""
    if (nzchar(reference)) {
      for (i in seq_len(max_pairs)) {
        if (identical(trimws(as.character(current[[paste0("value_", i)]][[row_index]] %||% "")), reference)) {
          reference_label <- as.character(current[[paste0("label_", i)]][[row_index]] %||% "")
          break
        }
      }
    }
    if (!identical(as.character(current$reference_label[[row_index]] %||% ""), reference_label)) {
      current$reference_label[[row_index]] <- reference_label
      changed <- TRUE
    }
  }

  list(table = current, changed = changed, var_label_updates = var_label_updates)
}

collect_category_label_inputs_from_table <- function(table_data, input, max_pairs = 6) {
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

