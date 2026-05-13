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
  info <- info[info$role != "exclude" & info$measurement %in% c("binary", "category", "ordered"), , drop = FALSE]
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

  info[, c("source_order", "selected", "name", "var_label", "role", "measurement", "n_unique", "reference", "reference_label", value_columns), drop = FALSE]
}

category_label_seed_table <- function(base, max_pairs = 6) {
  edit_columns <- category_label_edit_columns(max_pairs)
  if (is.null(base) || !"name" %in% names(base)) {
    return(NULL)
  }
  base[, c("name", edit_columns), drop = FALSE]
}

update_category_label_table <- function(table, base, name, field, value, max_pairs = 6) {
  edit_columns <- category_label_edit_columns(max_pairs)
  if (!nzchar(name) || !field %in% edit_columns) {
    return(list(table = table, changed = FALSE, var_label_update = NULL, ok = FALSE))
  }

  if (!is.data.frame(table)) {
    table <- category_label_seed_table(base, max_pairs)
    if (!is.data.frame(table)) {
      return(list(table = table, changed = FALSE, var_label_update = NULL, ok = FALSE))
    }
  } else {
    for (column in edit_columns) {
      if (!column %in% names(table)) {
        table[[column]] <- ""
      }
    }
  }

  if (!name %in% table$name) {
    table <- rbind(
      table,
      as.data.frame(
        as.list(c(name = name, stats::setNames(rep("", length(edit_columns)), edit_columns))),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
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

  if (!is.data.frame(current)) {
    current <- if (is.null(base) || !"name" %in% names(base)) {
      data.frame(name = character(0), stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      base[, c("name", intersect(value_columns, names(base))), drop = FALSE]
    }
  }

  for (column in value_columns) {
    if (!column %in% names(current)) {
      current[[column]] <- rep("", nrow(current))
    }
  }

  for (name in names(incoming)) {
    if (!nzchar(trimws(as.character(name %||% "")))) {
      next
    }
    if (!name %in% current$name) {
      current <- rbind(
        current,
        as.data.frame(
          as.list(c(name = name, stats::setNames(rep("", length(value_columns)), value_columns))),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    }
    row_index <- match(name, current$name)
    for (field in intersect(names(incoming[[name]]), value_columns)) {
      current[[field]][[row_index]] <- as.character(incoming[[name]][[field]] %||% "")
    }
  }

  current
}

