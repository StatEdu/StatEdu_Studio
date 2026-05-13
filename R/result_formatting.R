coefficient_display_table <- function(result) {
  coef_table <- result$coef_table
  if (!is.data.frame(coef_table) || nrow(coef_table) == 0) {
    return(coef_table)
  }

  use_hc3 <- isTRUE(result$use_hc3)
  use_bootstrap <- isTRUE(result$use_bootstrap)

  keep_columns <- function(table, columns) {
    table[, intersect(columns, names(table)), drop = FALSE]
  }

  if (!use_bootstrap) {
    if (use_hc3) {
      return(keep_columns(coef_table, c("Term", "B", "HC3 SE", "t", "p", "sr2", "f2", "Tolerance", "VIF")))
    }
    return(keep_columns(coef_table, c("Term", "B", "SE", "beta", "t", "p", "sr2", "f2", "Tolerance", "VIF")))
  }

  boot_table <- result$boot_table
  if (!is.data.frame(boot_table) || nrow(boot_table) == 0) {
    if (use_hc3) {
      return(keep_columns(coef_table, c("Term", "B", "HC3 SE", "sr2", "f2", "Tolerance", "VIF")))
    }
    return(keep_columns(coef_table, c("Term", "B", "sr2", "f2", "Tolerance", "VIF")))
  }

  boot_match <- match(coef_table$Term, boot_table$Term)

  if (use_hc3) {
    data.frame(
      Term = coef_table$Term,
      B = coef_table$B,
      `HC3 SE` = coef_table[["HC3 SE"]],
      LLCI = boot_table$Boot_LLCI[boot_match],
      ULCI = boot_table$Boot_ULCI[boot_match],
      `Boot p` = boot_table$Boot_p[boot_match],
      sr2 = coef_table$sr2,
      f2 = coef_table$f2,
      Tolerance = coef_table$Tolerance,
      VIF = coef_table$VIF,
      check.names = FALSE
    )
  } else {
    data.frame(
      Term = coef_table$Term,
      B = coef_table$B,
      `Boot SE` = boot_table$Boot_SE[boot_match],
      LLCI = boot_table$Boot_LLCI[boot_match],
      ULCI = boot_table$Boot_ULCI[boot_match],
      `Boot p` = boot_table$Boot_p[boot_match],
      sr2 = coef_table$sr2,
      f2 = coef_table$f2,
      Tolerance = coef_table$Tolerance,
      VIF = coef_table$VIF,
      check.names = FALSE
    )
  }
}

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

regression_method_label <- function(result) {
  if (isTRUE(result$use_hc3) && isTRUE(result$use_bootstrap)) {
    "Bootstrap + HC3 Regression"
  } else if (isTRUE(result$use_bootstrap)) {
    "Bootstrap Regression"
  } else if (isTRUE(result$use_hc3)) {
    "HC3 Regression"
  } else {
    "OLS Regression"
  }
}

coefficient_panel_title_static <- function(result, variable_table = NULL, labels = character(0)) {
  dependent <- all.vars(result$formula)[[1]]
  dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
  sprintf("%s(%s)", regression_method_label(result), dependent_label)
}

coefficient_output_table_with_context <- function(
  table,
  predictors = character(0),
  include_references = TRUE,
  variable_info = NULL,
  refs = character(0),
  value_labels = list(),
  labels = character(0),
  category_table = NULL
) {
  reference_rows <- if (isTRUE(include_references)) {
    categorical_reference_rows_static(
      predictors,
      names(table),
      variable_info,
      refs,
      value_labels,
      labels,
      category_table
    )
  } else {
    NULL
  }
  coefficient_output_table_static(
    table,
    predictors,
    include_references,
    labels,
    value_labels,
    reference_rows
  )
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
      if (nzchar(trimws(value)) && nzchar(trimws(label))) {
        values[trimws(value)] <- trimws(label)
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

model_overview_data_frame <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  dependents <- vapply(results, function(result) all.vars(result$formula)[[1]], character(1))
  dependent_labels <- vapply(
    dependents,
    display_variable_name_static,
    character(1),
    table = variable_table,
    labels = labels,
    label_only = TRUE
  )
  rows <- c("Independent variables", "N", "R\u00B2(adj. R\u00B2)", "F(p)", "Residual normality", "Residual homoscedasticity", "Selected method")
  values <- lapply(results, function(result) {
    predictor_labels <- vapply(
      result$predictors,
      display_variable_name_static,
      character(1),
      table = variable_table,
      labels = labels
    )
    c(
      "Independent variables" = paste(predictor_labels, collapse = ", "),
      "N" = as.character(result$n),
      "R\u00B2(adj. R\u00B2)" = sprintf("%s (%s)", format_decimal3(result$r_squared), format_decimal3(result$adjusted_r_squared)),
      "F(p)" = sprintf("%s (%s)", format_decimal3(result$f_statistic), format_p(result$f_p)),
      "Residual normality" = sprintf("%s (%s)", format_decimal3(result$normality_statistic), format_p(result$normality_p)),
      "Residual homoscedasticity" = sprintf("%s (%s)", format_decimal3(result$homogeneity_statistic), format_p(result$homogeneity_p)),
      "Selected method" = result$method
    )
  })
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(values)) {
    table[[dependent_labels[[index]]]] <- unname(values[[index]][rows])
  }
  table
}

combined_dw_data_frame <- function(results, variable_table = NULL, labels = character(0)) {
  if (!is.list(results) || length(results) == 0) {
    return(data.frame())
  }
  dependents <- vapply(results, function(result) all.vars(result$formula)[[1]], character(1))
  dependent_labels <- vapply(
    dependents,
    display_variable_name_static,
    character(1),
    table = variable_table,
    labels = labels,
    label_only = TRUE
  )
  rows <- as.character(results[[1]]$dw_result$Item)
  table <- data.frame(Item = rows, stringsAsFactors = FALSE, check.names = FALSE)
  for (index in seq_along(results)) {
    result <- results[[index]]
    values <- vapply(rows, function(row_name) {
      row_index <- match(row_name, result$dw_result$Item)
      if (is.na(row_index)) {
        return("")
      }
      value <- result$dw_result$Value[[row_index]]
      if (is.numeric(value)) {
        format_decimal3(value)
      } else {
        as.character(value %||% "")
      }
    }, character(1))
    table[[dependent_labels[[index]]]] <- values
  }
  table
}

filter_coefficient_export_table <- function(table, show_sr2 = FALSE, show_f2 = FALSE, show_vif = FALSE) {
  if (!is.data.frame(table)) {
    return(data.frame())
  }
  if (!isTRUE(show_sr2) && "sr2" %in% names(table)) {
    table$sr2 <- NULL
  }
  if (!isTRUE(show_f2) && "f2" %in% names(table)) {
    table$f2 <- NULL
  }
  if (!isTRUE(show_vif)) {
    if ("Tolerance" %in% names(table)) table$Tolerance <- NULL
    if ("VIF" %in% names(table)) table$VIF <- NULL
  }
  names(table)[names(table) == "sr2"] <- "sr\u00B2"
  names(table)[names(table) == "f2"] <- "f\u00B2"
  table
}

coefficient_output_table_static <- function(
  table,
  predictors = character(0),
  include_references = TRUE,
  labels = character(0),
  value_labels = list(),
  reference_rows = NULL
) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }

  raw_terms <- as.character(table$Term)
  variable_names <- unique(c(as.character(predictors), names(labels)))
  table$.raw_variable <- vapply(raw_terms, raw_term_variable, character(1), variable_names = variable_names)
  table$.raw_level <- mapply(raw_term_level, raw_terms, table$.raw_variable, USE.NAMES = FALSE)
  table$Term <- vapply(
    table$Term,
    display_term_name_with_variables_static,
    character(1),
    variable_names = variable_names,
    labels = labels,
    value_labels = value_labels
  )

  p_columns <- intersect(c("p", "Boot_p", "Boot p"), names(table))
  for (column in p_columns) {
    table[[column]] <- vapply(table[[column]], format_p, character(1))
  }
  for (column in setdiff(names(table), c("Term", p_columns))) {
    if (is.numeric(table[[column]])) {
      table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
    }
  }

  if (!isTRUE(include_references)) {
    return(table)
  }

  if (is.null(reference_rows) || nrow(reference_rows) == 0) {
    return(table[, setdiff(names(table), c(".raw_variable", ".raw_level")), drop = FALSE])
  }

  output <- table[0, , drop = FALSE]
  categorical_names <- unique(reference_rows$.raw_variable)
  used_reference <- rep(FALSE, nrow(reference_rows))
  handled_categorical <- character(0)
  level_order <- function(values) {
    values <- as.character(values %||% "")
    numeric_values <- suppressWarnings(as.numeric(values))
    if (all(!is.na(numeric_values))) {
      order(numeric_values, values)
    } else {
      order(values)
    }
  }

  for (row_index in seq_len(nrow(table))) {
    variable <- table$.raw_variable[[row_index]]
    if (nzchar(variable) && variable %in% categorical_names) {
      if (!variable %in% handled_categorical) {
        rows <- rbind(
          table[table$.raw_variable == variable, , drop = FALSE],
          reference_rows[reference_rows$.raw_variable == variable, , drop = FALSE]
        )
        rows <- rows[level_order(rows$.raw_level), , drop = FALSE]
        output <- rbind(output, rows)
        used_reference[reference_rows$.raw_variable == variable] <- TRUE
        handled_categorical <- c(handled_categorical, variable)
      }
      next
    }
    output <- rbind(output, table[row_index, , drop = FALSE])
  }
  if (any(!used_reference)) {
    output <- rbind(output, reference_rows[!used_reference, , drop = FALSE])
  }
  output[, setdiff(names(output), c(".raw_variable", ".raw_level")), drop = FALSE]
}

coefficient_fit_line <- function(result) {
  r2_label <- if (isTRUE(result$use_hc3) || isTRUE(result$use_bootstrap)) {
    "OLS R\u00B2(adj. R\u00B2)"
  } else {
    "R\u00B2(adj. R\u00B2)"
  }
  paste(
    sprintf(
      "%s = %s (%s)",
      r2_label,
      format_decimal3(result$r_squared),
      format_decimal3(result$adjusted_r_squared)
    ),
    sprintf(
      "F(%s, %s) = %s, p %s",
      result$f_df1,
      result$f_df2,
      format_decimal3(result$f_statistic),
      format_p(result$f_p)
    )
  )
}

coefficient_stat_lines <- function(result) {
  c(
    sprintf(
      "d(d\u1D64~4-d\u1D64) = %s (%s~%s)",
      format_decimal3(result$dw_d),
      format_decimal3(result$dw_crit$dU),
      format_decimal3(4 - result$dw_crit$dU)
    ),
    sprintf(
      "z(p) = %s (%s)",
      format_decimal3(result$normality_statistic),
      format_p(result$normality_p)
    ),
    sprintf(
      "\u03C7\u00B2(p) = %s (%s)",
      format_decimal3(result$homogeneity_statistic),
      format_p(result$homogeneity_p)
    )
  )
}

coefficient_max_vif <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || !"VIF" %in% names(table)) {
    return(NA_real_)
  }
  values <- suppressWarnings(as.numeric(table$VIF))
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(NA_real_)
  }
  max(values, na.rm = TRUE)
}

coefficient_vif_warning_line <- function(result) {
  max_vif <- coefficient_max_vif(result)
  if (is.na(max_vif) || max_vif <= 5) {
    return(character(0))
  }
  if (max_vif > 10) {
    return(sprintf(
      "Multicollinearity warning: VIF exceeds 10 (max VIF = %s). Consider Ridge regression, LASSO, or Elastic Net as alternative analyses.",
      format_decimal3(max_vif)
    ))
  }
  sprintf(
    "Multicollinearity caution: VIF exceeds 5 (max VIF = %s). Interpret individual regression coefficients with caution.",
    format_decimal3(max_vif)
  )
}

has_severe_vif <- function(results) {
  if (!is.list(results) || length(results) == 0) {
    return(FALSE)
  }
  any(vapply(results, function(result) {
    max_vif <- coefficient_max_vif(result)
    !is.na(max_vif) && max_vif > 10
  }, logical(1)))
}

coefficient_note_line <- function(result, show_vif = FALSE, show_sr2 = FALSE, show_f2 = FALSE) {
  paste(
    if (isTRUE(show_vif)) "Tolerance = 1 - R\u00B2 for each predictor;" else NULL,
    if (isTRUE(show_vif)) "VIF = Variance Inflation Factor;" else NULL,
    if (isTRUE(result$use_hc3)) "HC3 SE = heteroskedasticity-consistent standard error type 3;" else NULL,
    if (isTRUE(result$use_bootstrap)) "Boot SE, LLCI, ULCI, and Boot p are bootstrap estimates based on the selected bootstrap resamples and seed number;" else NULL,
    if (isTRUE(show_sr2)) "sr\u00B2 = squared semi-partial correlation, unique R\u00B2 contribution for each coefficient;" else NULL,
    if (isTRUE(show_f2)) "f\u00B2 = sr\u00B2 / (1 - model R\u00B2);" else NULL,
    if (isTRUE(result$use_hc3) || isTRUE(result$use_bootstrap)) "OLS R\u00B2 and adjusted R\u00B2 are ordinary least squares model fit indices;" else NULL,
    "d(d\u1D64~4-d\u1D64) = Durbin-Watson statistic (upper critical value~4-upper critical value);",
    "z(p) = Lilliefors corrected Kolmogorov-Smirnov residual normality test statistic (p-value);",
    "\u03C7\u00B2(p) = Breusch-Pagan homoscedasticity test statistic (p-value)"
  )
}
