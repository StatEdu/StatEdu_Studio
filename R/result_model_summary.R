# Model summary table helpers.

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

