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

package_version_label <- function(package) {
  version <- tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(e) NA_character_
  )
  if (is.na(version) || !nzchar(version)) {
    package
  } else {
    sprintf("%s %s", package, version)
  }
}

regression_package_label <- function(result) {
  packages <- c("stats", "lmtest", "nortest")
  if (isTRUE(result$use_hc3) || isTRUE(result$use_bootstrap)) {
    packages <- c(packages, "sandwich")
  }
  paste(vapply(unique(packages), package_version_label, character(1)), collapse = "; ")
}

regression_bootstrap_value <- function(result, field) {
  if (!isTRUE(result$use_bootstrap)) {
    return("")
  }
  value <- result[[field]]
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return("")
  }
  if (identical(field, "bootstrap_seed")) {
    return(as.character(as.integer(value[[1]])))
  }
  format(as.integer(value[[1]]), big.mark = ",", scientific = FALSE, trim = TRUE)
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
  dependent_labels <- mapply(function(label, result) {
    if (!is.null(result$hierarchical_step) && nzchar(result$hierarchical_step)) {
      sprintf("%s %s", label, result$hierarchical_step)
    } else {
      label
    }
  }, dependent_labels, results, USE.NAMES = FALSE)
  include_bootstrap_rows <- any(vapply(results, function(result) isTRUE(result$use_bootstrap), logical(1)))
  rows <- c(
    "Independent variables",
    "N",
    "R\u00B2(adj. R\u00B2)",
    "F(p)",
    "Residual normality",
    "Residual homoscedasticity",
    "Selected method"
  )
  if (include_bootstrap_rows) {
    rows <- c(rows, "Bootstrap samples", "Seed number")
  }
  rows <- c(
    rows,
    "Package"
  )
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
      "Selected method" = result$method,
      "Bootstrap samples" = regression_bootstrap_value(result, "bootstrap_r"),
      "Seed number" = regression_bootstrap_value(result, "bootstrap_seed"),
      "Package" = regression_package_label(result)
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

