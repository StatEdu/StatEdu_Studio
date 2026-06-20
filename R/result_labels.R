# Display label helpers for result tables.

display_variable_name_static <- function(name, table = NULL, labels = character(0), label_only = FALSE) {
  name <- as.character(name %||% "")
  if (length(name) == 0 || !nzchar(name[[1]])) {
    return("")
  }
  name <- name[[1]]
  label <- named_value(labels, name, "")
  if (!nzchar(label) && !is.null(table) && all(c("name", "var_label") %in% names(table))) {
    row_index <- match(name, table$name)
    if (!is.na(row_index)) {
      label <- as.character(table$var_label[[row_index]] %||% "")
    }
  }
  label <- trimws(label)
  if (isTRUE(label_only)) {
    if (nzchar(label)) label else name
  } else {
    if (nzchar(label)) sprintf("%s(%s)", name, label) else name
  }
}

display_variable_choices_static <- function(names, table = NULL, labels = character(0)) {
  names <- as.character(names %||% character(0))
  stats::setNames(
    names,
    vapply(names, display_variable_name_static, character(1), table = table, labels = labels)
  )
}

display_term_name_static <- function(term, labels = character(0), value_labels = list()) {
  term <- as.character(term %||% "")
  if (!nzchar(term) || identical(term, "(Intercept)")) {
    return(term)
  }

  term_clean <- gsub("`", "", term, fixed = TRUE)
  labels <- labels[!is.na(names(labels)) & nzchar(names(labels)) & nzchar(trimws(as.character(labels)))]
  if (length(labels) == 0) {
    return(term)
  }

  variable_names <- names(labels)
  variable_names <- variable_names[order(nchar(variable_names), decreasing = TRUE)]

  for (name in variable_names) {
    if (identical(term_clean, name)) {
      return(as.character(labels[[name]]))
    }
    if (startsWith(term_clean, name)) {
      level <- substring(term_clean, nchar(name) + 1)
      if (!nzchar(level)) next
      variable_label <- as.character(labels[[name]])
      category_label <- named_value(value_labels[[name]], level, "")
      if (nzchar(category_label)) {
        return(sprintf("%s:%s", variable_label, category_label))
      }
      return(sprintf("%s:%s", variable_label, level))
    }
  }

  term
}

display_term_name_with_variables_static <- function(
  term,
  variable_names,
  labels = character(0),
  value_labels = list(),
  fallback_labels = character(0)
) {
  term <- as.character(term %||% "")
  if (!nzchar(term) || identical(term, "(Intercept)")) {
    return(term)
  }

  term_clean <- gsub("`", "", term, fixed = TRUE)
  variable_names <- unique(c(as.character(variable_names), names(labels), names(fallback_labels)))
  variable_names <- variable_names[nzchar(variable_names)]
  variable_names <- variable_names[order(nchar(variable_names), decreasing = TRUE)]
  labels[names(fallback_labels)] <- fallback_labels

  for (name in variable_names) {
    variable_label <- named_value(labels, name, name)
    if (identical(term_clean, name)) {
      return(variable_label)
    }
    if (startsWith(term_clean, name)) {
      level <- substring(term_clean, nchar(name) + 1)
      if (!nzchar(level)) next
      category_label <- named_value(value_labels[[name]], level, "")
      if (nzchar(category_label)) {
        return(sprintf("%s:%s", variable_label, category_label))
      }
      return(sprintf("%s:%s", variable_label, level))
    }
  }

  display_term_name_static(term, labels, value_labels)
}

raw_term_variable <- function(term, variable_names) {
  term <- gsub("`", "", as.character(term %||% ""), fixed = TRUE)
  variable_names <- variable_names[order(nchar(variable_names), decreasing = TRUE)]
  for (name in variable_names) {
    if (identical(term, name) || startsWith(term, name)) {
      return(name)
    }
  }
  ""
}

raw_term_level <- function(term, variable_name) {
  term <- gsub("`", "", as.character(term %||% ""), fixed = TRUE)
  if (!nzchar(variable_name) || identical(term, variable_name)) return("")
  if (!startsWith(term, variable_name)) return("")
  substring(term, nchar(variable_name) + 1)
}

categorical_reference_rows_static <- function(
  predictors,
  columns,
  variable_info,
  refs = character(0),
  value_labels = list(),
  labels = character(0),
  category_table = NULL
) {
  if (is.null(variable_info) || nrow(variable_info) == 0 || length(predictors) == 0) {
    return(NULL)
  }

  categorical <- variable_info[
    variable_info$name %in% predictors & variable_info$measurement %in% c("binary", "category", "ordered"),
    ,
    drop = FALSE
  ]
  if (nrow(categorical) == 0) {
    return(NULL)
  }

  rows <- lapply(seq_len(nrow(categorical)), function(index) {
    name <- as.character(categorical$name[[index]])
    reference <- trimws(named_value(refs, name, ""))
    if (!nzchar(reference)) {
      values <- value_labels[[name]]
      if (!is.null(values) && length(values) > 0) {
        reference <- names(values)[[1]]
      }
    }
    if (!nzchar(reference)) {
      return(NULL)
    }

    variable_label <- named_value(labels, name, "")
    if (!nzchar(variable_label) && is.data.frame(category_table) && all(c("name", "var_label") %in% names(category_table))) {
      row_index <- match(name, as.character(category_table$name))
      if (!is.na(row_index)) {
        variable_label <- as.character(category_table$var_label[[row_index]] %||% "")
      }
    }
    if (!nzchar(variable_label)) variable_label <- name
    reference_label <- named_value(value_labels[[name]], reference, "")
    term <- if (nzchar(reference_label)) {
      sprintf("%s:%s", variable_label, reference_label)
    } else {
      sprintf("%s:%s", variable_label, reference)
    }

    row <- stats::setNames(as.list(rep("", length(columns))), columns)
    row$Term <- term
    if ("B" %in% columns) {
      row$B <- "reference"
    } else {
      estimate_column <- intersect(c("OR", "RR", "Mean ratio", "Rate ratio", "exp(B)", "Estimate"), columns)
      if (length(estimate_column) > 0) {
        row[[estimate_column[[1]]]] <- "reference"
      }
    }
    row$.raw_variable <- name
    row$.raw_level <- reference
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}


category_value_label_lookup_static <- function(table) {
  if (!is.data.frame(table) || !"name" %in% names(table)) {
    return(list())
  }

  lookup <- list()
  value_columns <- paste0("value_", seq_len(6))
  label_columns <- paste0("label_", seq_len(6))
  for (row_index in seq_len(nrow(table))) {
    name <- as.character(table$name[[row_index]] %||% "")
    if (!nzchar(name)) next
    values <- character(0)
    for (i in seq_along(value_columns)) {
      value <- if (value_columns[[i]] %in% names(table)) as.character(table[[value_columns[[i]]]][[row_index]] %||% "") else ""
      label <- if (label_columns[[i]] %in% names(table)) as.character(table[[label_columns[[i]]]][[row_index]] %||% "") else ""
      value <- trimws(value)
      label <- trimws(label)
      if (nzchar(value)) {
        values[value] <- if (nzchar(label)) label else value
      }
    }
    lookup[[name]] <- values
  }
  lookup
}

category_var_label_lookup_static <- function(table) {
  if (!is.data.frame(table) || !all(c("name", "var_label") %in% names(table))) {
    return(character(0))
  }
  labels <- stats::setNames(as.character(table$var_label), as.character(table$name))
  labels[nzchar(names(labels)) & nzchar(trimws(as.character(labels)))]
}

