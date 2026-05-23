# Factor analysis helpers.

factor_analysis_method_choices <- function() {
  c(
    "Principal axis factoring" = "pa",
    "Maximum likelihood" = "ml"
  )
}

factor_analysis_rotation_choices <- function() {
  c(
    "Varimax" = "varimax",
    "Oblimin" = "oblimin"
  )
}

factor_analysis_criterion_choices <- function() {
  c(
    "Eigenvalue >= 1.0" = "eigen",
    "Fixed number of factors" = "fixed"
  )
}

factor_analysis_normality_method_choices <- function() {
  c(
    "Skewness / kurtosis" = "skew_kurt",
    "Mardia test" = "mardia"
  )
}

factor_analysis_method_label <- function(method) {
  choices <- factor_analysis_method_choices()
  names(choices)[match(method, choices)] %||% method
}

factor_analysis_rotation_label <- function(rotation) {
  choices <- factor_analysis_rotation_choices()
  names(choices)[match(rotation, choices)] %||% rotation
}

factor_analysis_criterion_label <- function(criterion, n_factors = NULL) {
  if (identical(criterion, "fixed")) {
    return(sprintf("Fixed number of factors: %s", as.integer(n_factors %||% 1L)))
  }
  "Eigenvalue >= 1.0"
}

factor_analysis_normality_method_label <- function(method) {
  choices <- factor_analysis_normality_method_choices()
  names(choices)[match(method, choices)] %||% method
}

factor_analysis_measurements_for <- function(variables, variable_info = NULL) {
  measurements <- character(0)
  if (!is.null(variable_info) && all(c("name", "measurement") %in% names(variable_info))) {
    measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  }
  out <- vapply(as.character(variables), function(name) named_value(measurements, name, ""), character(1))
  out[out == "ordinal"] <- "ordered"
  out[out == "nominal"] <- "category"
  out
}

factor_analysis_numeric_matrix <- function(data, variables) {
  frame <- data[, variables, drop = FALSE]
  matrix <- as.data.frame(lapply(frame, function(values) suppressWarnings(as.numeric(values))), check.names = FALSE)
  names(matrix) <- variables
  matrix
}

factor_analysis_complete_matrix <- function(matrix) {
  if (!is.data.frame(matrix) || ncol(matrix) == 0) {
    return(matrix[0, , drop = FALSE])
  }
  matrix[stats::complete.cases(matrix), , drop = FALSE]
}

factor_analysis_display_names <- function(variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  stats::setNames(
    vapply(variables, correlation_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    variables
  )
}

factor_analysis_normality_table <- function(matrix, variables, display_names) {
  rows <- lapply(variables, function(name) {
    values <- suppressWarnings(as.numeric(matrix[[name]]))
    values <- values[!is.na(values)]
    skewness <- sample_skewness(values)
    kurtosis <- sample_excess_kurtosis(values)
    normal <- is.finite(skewness) && is.finite(kurtosis) && abs(skewness) < 2 && abs(kurtosis) < 7
    data.frame(
      Variable = display_names[[name]] %||% name,
      N = length(values),
      Skewness = format_decimal3(skewness),
      Kurtosis = format_decimal3(kurtosis),
      Normality = if (is.finite(skewness) && is.finite(kurtosis)) {
        if (isTRUE(normal)) "Satisfied" else "Not satisfied"
      } else {
        "Not tested"
      },
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

factor_analysis_normality_satisfied <- function(normality_table) {
  is.data.frame(normality_table) &&
    nrow(normality_table) > 0 &&
    "Normality" %in% names(normality_table) &&
    all(as.character(normality_table$Normality) == "Satisfied")
}

factor_analysis_mardia_table <- function(complete) {
  complete <- as.data.frame(complete, check.names = FALSE)
  n_obs <- nrow(complete)
  n_vars <- ncol(complete)
  if (n_obs < 5 || n_vars < 2) {
    return(data.frame(
      Check = "Mardia test",
      Statistic = "",
      df = "",
      p = "",
      Normality = "Not tested",
      check.names = FALSE
    ))
  }
  centered <- scale(as.matrix(complete), center = TRUE, scale = FALSE)
  cov_matrix <- stats::cov(centered)
  inv_cov <- tryCatch(solve(cov_matrix), error = function(e) NULL)
  if (is.null(inv_cov) || any(!is.finite(inv_cov))) {
    return(data.frame(
      Check = "Mardia test",
      Statistic = "",
      df = "",
      p = "",
      Normality = "Not tested",
      check.names = FALSE
    ))
  }

  distances <- centered %*% inv_cov %*% t(centered)
  skewness <- mean(distances^3)
  skewness_df <- n_vars * (n_vars + 1) * (n_vars + 2) / 6
  skewness_chisq <- n_obs * skewness / 6
  skewness_p <- stats::pchisq(skewness_chisq, df = skewness_df, lower.tail = FALSE)

  squared_distances <- diag(distances)
  kurtosis <- mean(squared_distances^2)
  kurtosis_z <- (kurtosis - n_vars * (n_vars + 2)) / sqrt(8 * n_vars * (n_vars + 2) / n_obs)
  kurtosis_p <- 2 * stats::pnorm(abs(kurtosis_z), lower.tail = FALSE)

  normal <- is.finite(skewness_p) && is.finite(kurtosis_p) && skewness_p >= 0.05 && kurtosis_p >= 0.05
  data.frame(
    Check = c("Mardia skewness", "Mardia kurtosis"),
    Statistic = c(format_decimal3(skewness_chisq), format_decimal3(kurtosis_z)),
    df = c(format_decimal3(skewness_df), ""),
    p = c(format_p(skewness_p), format_p(kurtosis_p)),
    Normality = if (isTRUE(normal)) "Satisfied" else "Not satisfied",
    check.names = FALSE
  )
}

factor_analysis_suitability_tables <- function(corr, n_obs) {
  kmo <- suppressWarnings(suppressMessages(tryCatch(psych::KMO(corr), error = function(e) NULL)))
  bartlett <- suppressWarnings(suppressMessages(tryCatch(psych::cortest.bartlett(corr, n = n_obs), error = function(e) NULL)))
  overview <- data.frame(
    Check = c("KMO", "Bartlett's test of sphericity"),
    Value = c(
      if (!is.null(kmo) && is.finite(kmo$MSA)) format_decimal3(kmo$MSA) else "",
      if (!is.null(bartlett) && is.finite(bartlett$chisq)) format_decimal3(bartlett$chisq) else ""
    ),
    df = c("", if (!is.null(bartlett) && is.finite(bartlett$df)) format_decimal3(bartlett$df) else ""),
    p = c("", if (!is.null(bartlett) && is.finite(bartlett$p.value)) format_p(bartlett$p.value) else ""),
    check.names = FALSE
  )
  list(overview = overview, kmo = kmo, bartlett = bartlett)
}

factor_analysis_select_factor_count <- function(eigenvalues, criterion, requested_n = 1L) {
  max_factors <- max(1L, length(eigenvalues) - 1L)
  if (identical(criterion, "fixed")) {
    return(min(max(1L, as.integer(requested_n %||% 1L)), max_factors))
  }
  selected <- sum(is.finite(eigenvalues) & eigenvalues >= 1)
  min(max(1L, selected), max_factors)
}

factor_analysis_overview_table <- function(result) {
  table <- data.frame(
    N = result$n_obs,
    Variables = length(result$variables),
    Factors = result$n_factors,
    Method = factor_analysis_method_label(result$method),
    Rotation = factor_analysis_rotation_label(result$rotation),
    Criterion = factor_analysis_criterion_label(result$criterion, result$n_factors),
    check.names = FALSE
  )
  if (isTRUE(result$options$normality)) {
    table$`Normality check` <- factor_analysis_normality_method_label(result$normality_method)
    table$Normality <- if (isTRUE(result$normality_decision)) "Satisfied" else "Not satisfied"
  }
  table
}

factor_analysis_loading_table <- function(result, cutoff = 0.30) {
  loadings <- result$loadings
  if (!is.matrix(loadings) || nrow(loadings) == 0) {
    return(NULL)
  }
  factors <- colnames(loadings)
  table <- data.frame(Variable = result$display_names[rownames(loadings)], check.names = FALSE)
  for (factor in factors) {
    table[[factor]] <- vapply(loadings[, factor], function(value) {
      if (!is.finite(value) || abs(value) < cutoff) "" else format_decimal3(value)
    }, character(1))
  }
  table$h2 <- vapply(result$communality[rownames(loadings)], format_decimal3, character(1))
  table$u2 <- vapply(result$uniqueness[rownames(loadings)], format_decimal3, character(1))
  table$Complexity <- vapply(result$complexity[rownames(loadings)], format_decimal3, character(1))
  table
}

factor_analysis_variance_table <- function(result) {
  accounted <- result$fit$Vaccounted
  if (is.null(accounted)) {
    return(NULL)
  }
  table <- as.data.frame(accounted, check.names = FALSE)
  table <- data.frame(Index = rownames(table), table, check.names = FALSE)
  for (column in setdiff(names(table), "Index")) {
    table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
  }
  rownames(table) <- NULL
  table
}

factor_analysis_factor_correlation_table <- function(result) {
  phi <- result$fit$Phi
  if (!is.matrix(phi) || nrow(phi) == 0) {
    return(NULL)
  }
  table <- as.data.frame(phi, check.names = FALSE)
  table <- data.frame(Factor = rownames(phi), table, check.names = FALSE)
  for (column in setdiff(names(table), "Factor")) {
    table[[column]] <- vapply(table[[column]], format_decimal3, character(1))
  }
  rownames(table) <- NULL
  table
}

factor_analysis_eigen_table <- function(result) {
  data.frame(
    Factor = seq_along(result$eigenvalues),
    Eigenvalue = vapply(result$eigenvalues, format_decimal3, character(1)),
    Selected = ifelse(seq_along(result$eigenvalues) <= result$n_factors, "Yes", ""),
    check.names = FALSE
  )
}

prepare_factor_analysis_results <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL, options = list()) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  shiny::validate(shiny::need(length(variables) >= 3, "Select at least three variables for factor analysis."))

  measurements <- factor_analysis_measurements_for(variables, variable_info)
  allowed <- c("ordered", "continuous")
  shiny::validate(shiny::need(all(measurements %in% allowed), "Factor analysis accepts only ordinal or continuous variables."))

  matrix <- factor_analysis_numeric_matrix(data, variables)
  complete <- factor_analysis_complete_matrix(matrix)
  shiny::validate(shiny::need(nrow(complete) >= 5, "Not enough complete cases for factor analysis."))

  variable_sd <- vapply(complete, stats::sd, numeric(1), na.rm = TRUE)
  non_constant <- names(variable_sd)[is.finite(variable_sd) & variable_sd > 0]
  shiny::validate(shiny::need(length(non_constant) >= 3, "At least three non-constant variables are required."))
  variables <- intersect(variables, non_constant)
  matrix <- matrix[, variables, drop = FALSE]
  complete <- complete[, variables, drop = FALSE]

  corr <- stats::cor(complete, use = "pairwise.complete.obs")
  shiny::validate(shiny::need(is.matrix(corr) && all(is.finite(corr)), "The correlation matrix could not be estimated."))
  diag(corr) <- 1
  eigenvalues <- eigen(corr, symmetric = TRUE, only.values = TRUE)$values

  display_names <- factor_analysis_display_names(variables, variable_info, labels, category_table)
  normality_method <- as.character(options$normality_method %||% "skew_kurt")
  if (!normality_method %in% unname(factor_analysis_normality_method_choices())) normality_method <- "skew_kurt"
  normality <- if (isTRUE(options$normality)) {
    if (identical(normality_method, "mardia")) {
      factor_analysis_mardia_table(complete)
    } else {
      factor_analysis_normality_table(matrix, variables, display_names)
    }
  } else {
    NULL
  }
  normality_decision <- if (isTRUE(options$normality)) factor_analysis_normality_satisfied(normality) else NULL

  requested_method <- as.character(options$method %||% "pa")
  if (!requested_method %in% unname(factor_analysis_method_choices())) requested_method <- "pa"
  method <- if (isTRUE(options$normality)) {
    if (isTRUE(normality_decision)) "ml" else "pa"
  } else {
    requested_method
  }
  rotation <- as.character(options$rotation %||% "varimax")
  if (!rotation %in% unname(factor_analysis_rotation_choices())) rotation <- "varimax"
  criterion <- as.character(options$criterion %||% "eigen")
  if (!criterion %in% unname(factor_analysis_criterion_choices())) criterion <- "eigen"
  n_factors <- factor_analysis_select_factor_count(eigenvalues, criterion, options$n_factors %||% 1L)

  fit <- suppressWarnings(suppressMessages(psych::fa(
    r = corr,
    nfactors = n_factors,
    n.obs = nrow(complete),
    rotate = rotation,
    fm = method,
    warnings = FALSE
  )))

  loadings <- as.matrix(unclass(fit$loadings))
  rownames(loadings) <- variables

  suitability <- factor_analysis_suitability_tables(corr, nrow(complete))

  result <- list(
    type = "factor_analysis",
    variables = variables,
    display_names = display_names,
    matrix = matrix,
    complete = complete,
    n_obs = nrow(complete),
    method = method,
    requested_method = requested_method,
    rotation = rotation,
    criterion = criterion,
    n_factors = n_factors,
    eigenvalues = eigenvalues,
    corr = corr,
    fit = fit,
    loadings = loadings,
    communality = fit$communality,
    uniqueness = fit$uniquenesses,
    complexity = fit$complexity,
    suitability = suitability,
    normality_table = normality,
    normality_method = normality_method,
    normality_decision = normality_decision,
    options = options,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table
  )
  result$overview <- factor_analysis_overview_table(result)
  result$loadings_table <- factor_analysis_loading_table(result)
  result$variance_table <- factor_analysis_variance_table(result)
  result$factor_correlation_table <- factor_analysis_factor_correlation_table(result)
  result$eigen_table <- factor_analysis_eigen_table(result)
  result
}
