# Reliability analysis helpers.

reliability_measurements_for <- function(variables, variable_info = NULL) {
  measurements <- character(0)
  if (!is.null(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  }
  out <- vapply(as.character(variables), function(name) named_value(measurements, name, ""), character(1))
  out[out == "ordinal"] <- "ordered"
  out[out == "nominal"] <- "category"
  out
}

reliability_numeric_matrix <- function(data, variables, measurement) {
  frame <- data[, variables, drop = FALSE]
  converted <- lapply(frame, function(values) {
    if (identical(measurement, "binary")) {
      non_missing <- unique(values[!is.na(values)])
      ordered <- frequency_value_order(non_missing)
      numeric <- match(as.character(values), ordered) - 1L
      return(as.numeric(numeric))
    }
    suppressWarnings(as.numeric(values))
  })
  matrix <- as.data.frame(converted, check.names = FALSE)
  names(matrix) <- variables
  matrix
}

reliability_complete_matrix <- function(matrix) {
  if (!is.data.frame(matrix) || ncol(matrix) == 0) {
    return(matrix[0, , drop = FALSE])
  }
  matrix[stats::complete.cases(matrix), , drop = FALSE]
}

reliability_alpha_value <- function(matrix) {
  matrix <- reliability_complete_matrix(matrix)
  k <- ncol(matrix)
  if (k < 2 || nrow(matrix) < 2) {
    return(NA_real_)
  }
  item_vars <- vapply(matrix, stats::var, numeric(1), na.rm = TRUE)
  total_var <- stats::var(rowSums(matrix), na.rm = TRUE)
  if (!is.finite(total_var) || total_var <= 0) {
    return(NA_real_)
  }
  value <- k / (k - 1) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
  if (is.finite(value)) value else NA_real_
}

reliability_kr20_value <- function(matrix) {
  matrix <- reliability_complete_matrix(matrix)
  k <- ncol(matrix)
  if (k < 2 || nrow(matrix) < 2) {
    return(NA_real_)
  }
  p <- vapply(matrix, mean, numeric(1), na.rm = TRUE)
  total_var <- stats::var(rowSums(matrix), na.rm = TRUE)
  if (!is.finite(total_var) || total_var <= 0) {
    return(NA_real_)
  }
  value <- k / (k - 1) * (1 - sum(p * (1 - p), na.rm = TRUE) / total_var)
  if (is.finite(value)) value else NA_real_
}

reliability_pearson_correlation <- function(matrix) {
  matrix <- reliability_complete_matrix(matrix)
  if (ncol(matrix) < 2 || nrow(matrix) < 2) {
    return(NULL)
  }
  corr <- stats::cor(matrix, use = "pairwise.complete.obs")
  if (!is.matrix(corr) || any(!is.finite(corr))) {
    return(NULL)
  }
  diag(corr) <- 1
  corr
}

reliability_polychoric_correlation <- function(matrix) {
  matrix <- reliability_complete_matrix(matrix)
  ordered_matrix <- as.data.frame(lapply(matrix, function(values) ordered(values)), check.names = FALSE)
  names(ordered_matrix) <- names(matrix)
  rho <- NULL
  invisible(utils::capture.output({
    rho <- suppressWarnings(suppressMessages(tryCatch(
      psych::polychoric(ordered_matrix, correct = 0)$rho,
      error = function(e) NULL
    )))
  }))
  rho
}

reliability_alpha_from_correlation_formula <- function(corr) {
  if (!is.matrix(corr) || nrow(corr) < 2 || any(!is.finite(corr))) {
    return(NA_real_)
  }
  diag(corr) <- 1
  k <- ncol(corr)
  total <- sum(corr)
  if (!is.finite(total) || total <= 0) {
    return(NA_real_)
  }
  value <- k / (k - 1) * (1 - k / total)
  if (is.finite(value)) value else NA_real_
}

reliability_psych_alpha_from_correlation <- function(corr, n_obs) {
  if (!is.matrix(corr) || nrow(corr) < 2 || any(!is.finite(corr))) {
    return(NA_real_)
  }
  diag(corr) <- 1
  fit <- NULL
  invisible(utils::capture.output({
    fit <- suppressWarnings(suppressMessages(tryCatch(
      psych::alpha(corr, n.obs = n_obs, check.keys = FALSE, warnings = FALSE),
      error = function(e) NULL
    )))
  }))
  value <- if (!is.null(fit) && is.data.frame(fit$total) && "std.alpha" %in% names(fit$total)) {
    fit$total$std.alpha[[1]]
  } else {
    NA_real_
  }
  if (is.finite(value)) value else reliability_alpha_from_correlation_formula(corr)
}

reliability_psych_omega_from_correlation <- function(corr, n_obs) {
  if (!is.matrix(corr) || any(!is.finite(corr))) {
    return(NA_real_)
  }
  diag(corr) <- 1
  fit <- NULL
  invisible(utils::capture.output({
    fit <- suppressWarnings(suppressMessages(tryCatch(
      psych::omega(corr, nfactors = 1, n.obs = n_obs, plot = FALSE, warnings = FALSE),
      error = function(e) NULL
    )))
  }))
  if (is.null(fit)) {
    return(NA_real_)
  }
  value <- if (!is.null(fit$omega.tot)) fit$omega.tot[[1]] else NA_real_
  if (is.finite(value)) value else NA_real_
}

reliability_omega_value <- function(matrix, measurement = "continuous") {
  matrix <- reliability_complete_matrix(matrix)
  if (ncol(matrix) < 3 || nrow(matrix) < 4) {
    return(NA_real_)
  }
  corr <- if (identical(measurement, "ordered")) {
    reliability_polychoric_correlation(matrix)
  } else {
    stats::cor(matrix, use = "pairwise.complete.obs")
  }
  reliability_psych_omega_from_correlation(corr, nrow(matrix))
}

reliability_pearson_values <- function(matrix) {
  matrix <- reliability_complete_matrix(matrix)
  corr <- reliability_pearson_correlation(matrix)
  c(
    pearson_alpha = reliability_psych_alpha_from_correlation(corr, nrow(matrix)),
    pearson_omega = reliability_psych_omega_from_correlation(corr, nrow(matrix))
  )
}

reliability_ordinal_values <- function(matrix) {
  matrix <- reliability_complete_matrix(matrix)
  corr <- reliability_polychoric_correlation(matrix)
  c(
    ordinal_alpha = reliability_psych_alpha_from_correlation(corr, nrow(matrix)),
    ordinal_omega = reliability_psych_omega_from_correlation(corr, nrow(matrix))
  )
}

reliability_max_response_categories <- function(matrix) {
  counts <- vapply(matrix, function(values) {
    length(unique(values[!is.na(values)]))
  }, integer(1))
  if (length(counts) == 0 || all(is.na(counts))) {
    return(NA_integer_)
  }
  max(counts, na.rm = TRUE)
}

reliability_normality_table <- function(matrix, variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  rows <- lapply(variables, function(name) {
    values <- suppressWarnings(as.numeric(matrix[[name]]))
    values <- values[!is.na(values)]
    skewness <- sample_skewness(values)
    kurtosis <- sample_excess_kurtosis(values)
    normal <- is.finite(skewness) && is.finite(kurtosis) && abs(skewness) < 2 && abs(kurtosis) < 7
    data.frame(
      Item = frequency_variable_display_name(name, variable_info, labels, category_table),
      N = length(values),
      Skewness = format_frequency_decimal(skewness, digits = 3),
      Kurtosis = format_frequency_decimal(kurtosis, digits = 3),
      Normality = if (isTRUE(normal)) "Satisfied" else "Not satisfied",
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

reliability_item_descriptives <- function(matrix, variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  rows <- lapply(variables, function(name) {
    values <- suppressWarnings(as.numeric(matrix[[name]]))
    observed <- values[!is.na(values)]
    data.frame(
      Item = frequency_variable_display_name(name, variable_info, labels, category_table),
      N = length(observed),
      Missing = sum(is.na(values)),
      Min = format_frequency_decimal(if (length(observed) > 0) min(observed) else NA_real_),
      Max = format_frequency_decimal(if (length(observed) > 0) max(observed) else NA_real_),
      M = format_frequency_decimal(if (length(observed) > 0) mean(observed) else NA_real_),
      SD = format_frequency_decimal(if (length(observed) > 1) stats::sd(observed) else NA_real_),
      Skewness = format_frequency_decimal(sample_skewness(observed), digits = 3),
      Kurtosis = format_frequency_decimal(sample_excess_kurtosis(observed), digits = 3),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

reliability_compute_value <- function(matrix, method, measurement) {
  switch(
    method,
    kr20 = reliability_kr20_value(matrix),
    pearson = reliability_pearson_values(matrix),
    ordinal = reliability_ordinal_values(matrix),
    reliability_pearson_values(matrix)
  )
}

reliability_include_omega <- function(options = list()) {
  isTRUE(options$ordinal)
}

reliability_method_label <- function(method, include_omega = TRUE) {
  switch(
    method,
    kr20 = "KR-20",
    pearson = if (isTRUE(include_omega)) "Cronbach's alpha / Pearson omega" else "Cronbach's alpha",
    ordinal = if (isTRUE(include_omega)) "Ordinal alpha / Ordinal omega" else "Ordinal alpha",
    method
  )
}

reliability_format_decimal <- function(value) {
  if (length(value) == 0 || is.na(value[[1]])) {
    return("-")
  }
  format_decimal3(value[[1]])
}

reliability_item_diagnostics <- function(matrix, variables, method, measurement, variable_info = NULL, labels = character(0), category_table = NULL) {
  complete <- reliability_complete_matrix(matrix)
  total <- if (nrow(complete) > 0) rowSums(complete) else numeric(0)
  rows <- lapply(seq_along(variables), function(index) {
    name <- variables[[index]]
    item <- if (nrow(complete) > 0) complete[[name]] else numeric(0)
    total_without_item <- if (nrow(complete) > 0) total - item else numeric(0)
    item_sd <- stats::sd(item)
    total_sd <- stats::sd(total)
    total_without_item_sd <- stats::sd(total_without_item)
    item_total <- if (length(item) > 2 && is.finite(item_sd) && item_sd > 0 && is.finite(total_sd) && total_sd > 0) {
      stats::cor(item, total, method = if (identical(measurement, "ordered")) "spearman" else "pearson")
    } else {
      NA_real_
    }
    corrected <- if (length(item) > 2 && is.finite(item_sd) && item_sd > 0 && is.finite(total_without_item_sd) && total_without_item_sd > 0) {
      stats::cor(item, total_without_item, method = if (identical(measurement, "ordered")) "spearman" else "pearson")
    } else {
      NA_real_
    }
    deletion_matrix <- matrix[, setdiff(variables, name), drop = FALSE]
    deletion_value <- reliability_compute_value(deletion_matrix, method, measurement)
    reliability_cells <- if (identical(method, "ordinal")) {
      list(
        `Ordinal alpha if item deleted` = reliability_format_decimal(deletion_value[["ordinal_alpha"]]),
        `Ordinal omega if item deleted` = reliability_format_decimal(deletion_value[["ordinal_omega"]])
      )
    } else if (identical(method, "pearson")) {
      list(
        `Cronbach's alpha if item deleted` = reliability_format_decimal(deletion_value[["pearson_alpha"]]),
        `Pearson omega if item deleted` = reliability_format_decimal(deletion_value[["pearson_omega"]])
      )
    } else {
      list(`Reliability if item deleted` = reliability_format_decimal(deletion_value))
    }
    data.frame(
      Item = frequency_variable_display_name(name, variable_info, labels, category_table),
      reliability_cells,
      `Corrected item-total correlation` = reliability_format_decimal(corrected),
      `Item-total correlation` = reliability_format_decimal(item_total),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

prepare_reliability_results <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 2, "Select at least two items for reliability analysis."))

  measurements <- reliability_measurements_for(variables, variable_info)
  allowed <- c("binary", "ordered", "continuous")
  shiny::validate(shiny::need(all(measurements %in% allowed), "Reliability analysis accepts only binary, ordinal, or continuous items."))
  unique_measurements <- unique(unname(measurements))
  shiny::validate(shiny::need(length(unique_measurements) == 1, "Select items with the same measurement level. Mixed item types cannot be analyzed together."))

  measurement <- unique_measurements[[1]]
  matrix <- reliability_numeric_matrix(data, variables, measurement)
  complete <- reliability_complete_matrix(matrix)
  shiny::validate(shiny::need(nrow(complete) >= 2, "Not enough complete cases for reliability analysis."))

  normality <- NULL
  response_categories <- reliability_max_response_categories(matrix)
  method <- switch(measurement, binary = "kr20", ordered = "ordinal", continuous = "pearson")
  normality_decision <- NULL

  if (identical(measurement, "ordered")) {
    method <- "ordinal"
  }

  if (identical(measurement, "continuous") && isTRUE(options$ordinal)) {
    if (is.finite(response_categories) && response_categories <= 5) {
      method <- "ordinal"
    } else {
      normality <- reliability_normality_table(matrix, variables, variable_info, labels, category_table)
      normality_decision <- all(as.character(normality$Normality) == "Satisfied")
      method <- if (isTRUE(normality_decision)) "pearson" else "ordinal"
    }
  } else if (identical(measurement, "continuous") && isTRUE(options$normality)) {
    normality <- reliability_normality_table(matrix, variables, variable_info, labels, category_table)
  }
  include_omega <- reliability_include_omega(options)
  shiny::validate(shiny::need(!isTRUE(include_omega) || !identical(method, "pearson") || length(variables) >= 3, "Pearson omega requires at least three items."))
  shiny::validate(shiny::need(!isTRUE(include_omega) || !identical(method, "ordinal") || length(variables) >= 3, "Ordinal omega requires at least three items."))

  value <- reliability_compute_value(matrix, method, measurement)
  details_enabled <- isTRUE(options$reliability_if_deleted) || isTRUE(options$item_total_correlation)
  diagnostics <- if (details_enabled) {
    reliability_item_diagnostics(matrix, variables, method, measurement, variable_info, labels, category_table)
  } else {
    NULL
  }
  if (is.data.frame(diagnostics)) {
    keep <- "Item"
    if (isTRUE(options$reliability_if_deleted)) {
      if (identical(method, "ordinal")) {
        keep <- c(keep, "Ordinal alpha if item deleted", "Ordinal omega if item deleted")
        if (!isTRUE(include_omega)) keep <- setdiff(keep, "Ordinal omega if item deleted")
      } else if (identical(method, "pearson")) {
        keep <- c(keep, "Cronbach's alpha if item deleted", "Pearson omega if item deleted")
        if (!isTRUE(include_omega)) keep <- setdiff(keep, "Pearson omega if item deleted")
      } else {
        keep <- c(keep, "Reliability if item deleted")
      }
    }
    if (isTRUE(options$item_total_correlation)) {
      keep <- c(keep, "Corrected item-total correlation", "Item-total correlation")
    }
    diagnostics <- diagnostics[, keep, drop = FALSE]
  }

  overview <- if (identical(method, "ordinal")) {
    data.frame(
      Items = length(variables),
      N = nrow(complete),
      `Measurement level` = "Ordinal",
      Method = reliability_method_label(method, include_omega),
      `Ordinal alpha` = reliability_format_decimal(value[["ordinal_alpha"]]),
      check.names = FALSE
    )
  } else if (identical(method, "pearson")) {
    data.frame(
      Items = length(variables),
      N = nrow(complete),
      `Measurement level` = "Continuous",
      Method = reliability_method_label(method, include_omega),
      `Cronbach's alpha` = reliability_format_decimal(value[["pearson_alpha"]]),
      check.names = FALSE
    )
  } else {
    data.frame(
      Items = length(variables),
      N = nrow(complete),
      `Measurement level` = switch(measurement, binary = "Binary", ordered = "Ordinal", continuous = "Continuous"),
      Method = reliability_method_label(method, include_omega),
      Reliability = reliability_format_decimal(value),
      check.names = FALSE
    )
  }
  if (identical(method, "ordinal") && isTRUE(include_omega)) {
    overview$`Ordinal omega` <- reliability_format_decimal(value[["ordinal_omega"]])
  }
  if (identical(method, "pearson") && isTRUE(include_omega)) {
    overview$`Pearson omega` <- reliability_format_decimal(value[["pearson_omega"]])
  }

  list(
    variables = variables,
    measurement = measurement,
    method = method,
    reliability = value,
    recommended = switch(
      method,
      pearson = reliability_method_label(method, include_omega),
      ordinal = reliability_method_label(method, include_omega),
      kr20 = "KR-20",
      reliability_method_label(method, include_omega)
    ),
    response_categories = response_categories,
    normality_decision = normality_decision,
    overview = overview,
    normality_table = normality,
    item_descriptives = reliability_item_descriptives(matrix, variables, variable_info, labels, category_table),
    item_diagnostics = diagnostics,
    options = options,
    data = data,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table
  )
}
