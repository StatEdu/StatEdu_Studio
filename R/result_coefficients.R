# Coefficient table formatting helpers.

coefficient_panel_title_static <- function(result, variable_table = NULL, labels = character(0)) {
  dependent <- all.vars(result$formula)[[1]]
  dependent_label <- display_variable_name_static(dependent, variable_table, labels, label_only = TRUE)
  title <- sprintf("%s(%s)", regression_method_label(result), dependent_label)
  if (!is.null(result$hierarchical_step) && nzchar(result$hierarchical_step)) {
    title <- sprintf("%s: %s", result$hierarchical_step, title)
  }
  title
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
      formatter <- if (column %in% c("sr2", "f2")) format_effect_size else format_decimal3
      table[[column]] <- vapply(table[[column]], formatter, character(1))
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
      "%s = %s (%s)",
      stat_chisq_label(with_p = TRUE),
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
    sprintf("%s = Breusch-Pagan homoscedasticity test statistic (p-value)", stat_chisq_label(with_p = TRUE))
  )
}
