# Correlation and association analysis helpers.

correlation_variable_display_name <- function(name, variable_info = NULL, labels = character(0), category_table = NULL) {
  frequency_variable_display_name(name, variable_info, labels, category_table)
}

correlation_measurement_lookup <- function(variable_info = NULL) {
  if (!is.data.frame(variable_info) || !all(c("name", "measurement") %in% names(variable_info))) {
    return(character(0))
  }
  values <- tolower(as.character(variable_info$measurement))
  values[values == "ordinal"] <- "ordered"
  stats::setNames(values, as.character(variable_info$name))
}

correlation_measurement <- function(name, variable_info = NULL) {
  measurements <- correlation_measurement_lookup(variable_info)
  measurement <- named_value(measurements, name, "continuous")
  if (measurement %in% c("continuous", "ordered", "binary", "category")) {
    return(measurement)
  }
  "continuous"
}

correlation_measurement_label <- function(measurement) {
  switch(
    measurement,
    continuous = "Continuous",
    ordered = "Ordinal",
    binary = "Binary",
    category = "Nominal",
    measurement
  )
}

correlation_numeric_vector <- function(values) {
  suppressWarnings(as.numeric(values))
}

correlation_ordered_score <- function(values) {
  if (is.character(values) || is.factor(values)) {
    values <- as.character(values)
    values[!nzchar(trimws(values))] <- NA_character_
  }
  numeric <- suppressWarnings(as.numeric(values))
  non_missing <- values[!is.na(values)]
  if (length(non_missing) == 0) {
    return(numeric)
  }
  if (sum(!is.na(numeric)) >= 3) {
    return(numeric)
  }
  ordered_values <- frequency_value_order(non_missing)
  as.numeric(match(as.character(values), ordered_values))
}

correlation_binary_score <- function(values) {
  if (is.factor(values)) {
    raw <- as.character(values)
  } else {
    raw <- as.character(values)
  }
  raw[is.na(values)] <- NA_character_
  raw[!is.na(raw) & !nzchar(trimws(raw))] <- NA_character_
  levels <- sort(unique(raw[!is.na(raw)]))
  if (length(levels) != 2) {
    return(rep(NA_real_, length(values)))
  }
  ifelse(raw == levels[[2]], 1, ifelse(raw == levels[[1]], 0, NA_real_))
}

correlation_factor_vector <- function(values) {
  raw <- as.character(values)
  raw[is.na(values)] <- NA_character_
  raw[!is.na(raw) & !nzchar(trimws(raw))] <- NA_character_
  factor(raw)
}

correlation_analysis_vector <- function(values, measurement) {
  switch(
    measurement,
    continuous = correlation_numeric_vector(values),
    ordered = correlation_ordered_score(values),
    binary = correlation_binary_score(values),
    category = correlation_factor_vector(values),
    correlation_numeric_vector(values)
  )
}

correlation_numeric_data <- function(data, variables, variable_info = NULL) {
  variables <- intersect(as.character(variables %||% character(0)), names(data))
  out <- data.frame(lapply(variables, function(name) {
    measurement <- correlation_measurement(name, variable_info)
    if (identical(measurement, "category")) {
      return(rep(NA_real_, nrow(data)))
    }
    correlation_analysis_vector(data[[name]], measurement)
  }), check.names = FALSE)
  names(out) <- variables
  out
}

correlation_complete_pair <- function(x, y) {
  complete <- stats::complete.cases(x, y)
  list(x = x[complete], y = y[complete], n = sum(complete))
}

correlation_normality_summary <- function(data, variables, variable_info = NULL, labels = character(0), category_table = NULL) {
  numeric_data <- correlation_numeric_data(data, variables, variable_info)
  rows <- lapply(names(numeric_data), function(name) {
    measurement <- correlation_measurement(name, variable_info)
    values <- numeric_data[[name]]
    values <- values[!is.na(values)]
    if (!identical(measurement, "continuous")) {
      return(data.frame(
        Name = name,
        Variable = correlation_variable_display_name(name, variable_info, labels, category_table),
        N = "",
        Skewness = "",
        Kurtosis = "",
        Normality = "not assessed",
        normal = FALSE,
        check.names = FALSE
      ))
    }
    skew <- sample_skewness(values)
    kurtosis <- sample_excess_kurtosis(values)
    satisfied <- is.finite(skew) && is.finite(kurtosis) && abs(skew) <= 2 && abs(kurtosis) <= 7
    data.frame(
      Name = name,
      Variable = correlation_variable_display_name(name, variable_info, labels, category_table),
      N = length(values),
      Skewness = format_decimal3(skew),
      Kurtosis = format_decimal3(kurtosis),
      Normality = if (isTRUE(satisfied)) "satisfied" else "not satisfied",
      normal = isTRUE(satisfied),
      check.names = FALSE
    )
  })
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

correlation_ci <- function(r, n, level = 0.95) {
  if (!is.finite(r) || n < 4 || abs(r) >= 1) {
    return(c(NA_real_, NA_real_))
  }
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  critical <- stats::qnorm(1 - (1 - level) / 2)
  tanh(c(z - critical * se, z + critical * se))
}

correlation_sig <- function(p) {
  if (!is.finite(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

correlation_normality_satisfied <- function(normality_table, name) {
  if (!is.data.frame(normality_table) || nrow(normality_table) == 0 || !"Name" %in% names(normality_table)) {
    return(FALSE)
  }
  row <- normality_table[as.character(normality_table$Name) == as.character(name), , drop = FALSE]
  if (nrow(row) == 0 || !"normal" %in% names(row)) {
    return(FALSE)
  }
  isTRUE(row$normal[[1]])
}

correlation_method_for_pair <- function(
  x_measure,
  y_measure,
  continuous_method = "auto",
  x_name = NULL,
  y_name = NULL,
  normality_table = NULL,
  normality_checked = FALSE
) {
  pair <- sort(c(x_measure, y_measure))
  if (identical(pair, c("continuous", "continuous"))) {
    method <- as.character(continuous_method %||% "auto")
    method <- if (method %in% c("auto", "pearson", "spearman", "kendall")) method else "auto"
    if (identical(method, "auto")) {
      x_normal <- isTRUE(normality_checked) && correlation_normality_satisfied(normality_table, x_name)
      y_normal <- isTRUE(normality_checked) && correlation_normality_satisfied(normality_table, y_name)
      if (isTRUE(x_normal) && isTRUE(y_normal)) {
        return(list(method = "pearson", label = "Pearson", reason = "Auto selected Pearson because both continuous variables satisfied normality."))
      }
      return(list(method = "spearman", label = "Spearman", reason = "Auto selected Spearman because at least one continuous variable did not satisfy normality."))
    }
    return(list(method = method, label = tools::toTitleCase(method), reason = sprintf("%s was selected for two continuous variables.", tools::toTitleCase(method))))
  }
  if (all(pair %in% c("continuous", "binary"))) {
    return(list(method = "point_biserial", label = "Point-biserial", reason = "Point-biserial was selected for a continuous variable and a binary variable."))
  }
  if (all(pair %in% c("continuous", "ordered"))) {
    return(list(method = "spearman", label = "Spearman", reason = "Spearman was selected because one variable is ordinal; polyserial can be added as an advanced option."))
  }
  if (identical(pair, c("ordered", "ordered"))) {
    return(list(method = "spearman", label = "Spearman", reason = "Spearman was selected for two ordinal variables; polychoric can be added as an advanced option."))
  }
  if (identical(pair, c("binary", "binary"))) {
    return(list(method = "phi", label = "Phi", reason = "Phi was selected for two binary variables."))
  }
  if (all(pair %in% c("binary", "ordered"))) {
    return(list(method = "spearman", label = "Spearman", reason = "Spearman was selected for binary-ordinal variables; polychoric can be added as an advanced option."))
  }
  if (any(pair == "category") && any(pair == "continuous")) {
    return(list(method = "eta", label = "Eta", reason = "Eta was selected for a nominal and continuous variable; ANOVA is recommended for detailed group comparison."))
  }
  if (all(pair %in% c("binary", "category", "ordered"))) {
    return(list(method = "cramers_v", label = "Cramer's V", reason = "Cramer's V was selected for categorical variables."))
  }
  list(method = "spearman", label = "Spearman", reason = "Spearman was selected as a stable fallback for mixed measurement levels.")
}

correlation_latent_method_for_pair <- function(x_measure, y_measure) {
  pair <- sort(c(x_measure, y_measure))
  if (identical(pair, c("continuous", "continuous"))) {
    return(list(method = "pearson", label = "Pearson", reason = "Pearson was retained for two continuous variables in the latent-variable set."))
  }
  if (all(pair %in% c("continuous", "ordered", "binary")) && any(pair == "continuous") && any(pair %in% c("ordered", "binary"))) {
    return(list(method = "polyserial", label = "Polyserial", reason = "Polyserial was selected for one continuous variable and one ordinal/binary variable."))
  }
  if (identical(pair, c("binary", "binary"))) {
    return(list(method = "tetrachoric", label = "Tetrachoric", reason = "Tetrachoric was selected for two binary variables."))
  }
  if (all(pair %in% c("ordered", "binary"))) {
    return(list(method = "polychoric", label = "Polychoric", reason = "Polychoric was selected for ordinal/binary variables."))
  }
  if (any(pair == "category") && any(pair == "continuous")) {
    return(list(method = "eta", label = "Eta", reason = "Eta was retained because latent-variable correlations are not applied to nominal-continuous pairs."))
  }
  if (all(pair %in% c("binary", "category", "ordered"))) {
    return(list(method = "cramers_v", label = "Cramer's V", reason = "Cramer's V was retained because latent-variable correlations are not applied to nominal categorical pairs."))
  }
  list(method = "spearman", label = "Spearman", reason = "Spearman was retained as a stable fallback for this measurement-level combination.")
}

correlation_test_result <- function(x, y, method, label) {
  pair <- correlation_complete_pair(x, y)
  x <- pair$x
  y <- pair$y
  n <- pair$n
  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method, label = label))
  }
  cor_method <- if (identical(method, "point_biserial")) "pearson" else method
  test <- try(stats::cor.test(x, y, method = cor_method, exact = FALSE), silent = TRUE)
  if (inherits(test, "try-error")) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method, label = label))
  }
  coefficient <- unname(as.numeric(test$estimate[[1]]))
  ci <- if (!is.null(test$conf.int)) {
    as.numeric(test$conf.int[1:2])
  } else if (cor_method %in% c("pearson", "spearman", "kendall")) {
    correlation_ci(coefficient, n)
  } else {
    c(NA_real_, NA_real_)
  }
  list(n = n, coefficient = coefficient, p = as.numeric(test$p.value), ci = ci, method = method, label = label)
}

correlation_phi_result <- function(x, y, label = "Phi") {
  correlation_test_result(x, y, "pearson", label)
}

correlation_cramers_v_result <- function(x, y) {
  pair <- correlation_complete_pair(x, y)
  x <- droplevels(factor(pair$x))
  y <- droplevels(factor(pair$y))
  n <- length(x)
  if (n < 3 || nlevels(x) < 2 || nlevels(y) < 2) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "cramers_v", label = "Cramer's V"))
  }
  table <- table(x, y)
  test <- suppressWarnings(try(stats::chisq.test(table, correct = FALSE), silent = TRUE))
  if (inherits(test, "try-error")) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "cramers_v", label = "Cramer's V"))
  }
  min_dim <- min(nrow(table) - 1, ncol(table) - 1)
  coefficient <- if (min_dim > 0) sqrt(as.numeric(test$statistic) / (n * min_dim)) else NA_real_
  list(n = n, coefficient = coefficient, p = as.numeric(test$p.value), ci = c(NA_real_, NA_real_), method = "cramers_v", label = "Cramer's V")
}

correlation_eta_result <- function(continuous, group) {
  pair <- correlation_complete_pair(continuous, group)
  values <- as.numeric(pair$x)
  groups <- droplevels(factor(pair$y))
  n <- length(values)
  if (n < 3 || stats::sd(values) == 0 || nlevels(groups) < 2) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "eta", label = "Eta"))
  }
  grand <- mean(values)
  ss_total <- sum((values - grand)^2)
  means <- tapply(values, groups, mean)
  counts <- table(groups)
  ss_between <- sum(as.numeric(counts) * (means[names(counts)] - grand)^2)
  eta <- if (ss_total > 0) sqrt(ss_between / ss_total) else NA_real_
  fit <- try(stats::aov(values ~ groups), silent = TRUE)
  p <- if (inherits(fit, "try-error")) {
    NA_real_
  } else {
    anova <- summary(fit)[[1]]
    as.numeric(anova[["Pr(>F)"]][[1]])
  }
  list(n = n, coefficient = eta, p = p, ci = c(NA_real_, NA_real_), method = "eta", label = "Eta")
}

correlation_polyserial_result <- function(continuous, ordinal, label = "Polyserial") {
  pair <- correlation_complete_pair(continuous, ordinal)
  x <- as.numeric(pair$x)
  y <- ordered(pair$y)
  n <- pair$n
  if (n < 4 || stats::sd(x) == 0 || length(unique(y)) < 2 || !requireNamespace("polycor", quietly = TRUE)) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "polyserial", label = label))
  }
  coefficient <- suppressWarnings(try(polycor::polyserial(x, y, ML = FALSE, std.err = FALSE), silent = TRUE))
  if (inherits(coefficient, "try-error") || !is.finite(as.numeric(coefficient))) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = "polyserial", label = label))
  }
  coefficient <- max(min(as.numeric(coefficient), .9999), -.9999)
  list(n = n, coefficient = coefficient, p = NA_real_, ci = correlation_ci(coefficient, n), method = "polyserial", label = label)
}

correlation_polychoric_result <- function(x, y, method = "polychoric", label = "Polychoric") {
  pair <- correlation_complete_pair(x, y)
  x <- ordered(pair$x)
  y <- ordered(pair$y)
  n <- pair$n
  if (n < 4 || length(unique(x)) < 2 || length(unique(y)) < 2 || !requireNamespace("polycor", quietly = TRUE)) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method, label = label))
  }
  coefficient <- suppressWarnings(try(polycor::polychor(x, y, ML = FALSE, std.err = FALSE), silent = TRUE))
  if (inherits(coefficient, "try-error") || !is.finite(as.numeric(coefficient))) {
    return(list(n = n, coefficient = NA_real_, p = NA_real_, ci = c(NA_real_, NA_real_), method = method, label = label))
  }
  coefficient <- max(min(as.numeric(coefficient), .9999), -.9999)
  list(n = n, coefficient = coefficient, p = NA_real_, ci = correlation_ci(coefficient, n), method = method, label = label)
}

correlation_pair_result <- function(
  data,
  x_name,
  y_name,
  x_measure,
  y_measure,
  continuous_method = "auto",
  normality_table = NULL,
  normality_checked = FALSE
) {
  selection <- correlation_method_for_pair(
    x_measure,
    y_measure,
    continuous_method,
    x_name = x_name,
    y_name = y_name,
    normality_table = normality_table,
    normality_checked = normality_checked
  )
  x <- correlation_analysis_vector(data[[x_name]], x_measure)
  y <- correlation_analysis_vector(data[[y_name]], y_measure)

  result <- switch(
    selection$method,
    pearson = correlation_test_result(x, y, "pearson", selection$label),
    spearman = correlation_test_result(x, y, "spearman", selection$label),
    kendall = correlation_test_result(x, y, "kendall", selection$label),
    point_biserial = correlation_test_result(x, y, "point_biserial", selection$label),
    phi = correlation_phi_result(x, y, selection$label),
    cramers_v = correlation_cramers_v_result(x, y),
    eta = {
      if (identical(x_measure, "continuous")) correlation_eta_result(x, y) else correlation_eta_result(y, x)
    },
    correlation_test_result(x, y, "spearman", "Spearman")
  )
  result$reason <- selection$reason
  result$type1 <- correlation_measurement_label(x_measure)
  result$type2 <- correlation_measurement_label(y_measure)
  result
}

correlation_latent_pair_result <- function(data, x_name, y_name, x_measure, y_measure) {
  selection <- correlation_latent_method_for_pair(x_measure, y_measure)
  x <- correlation_analysis_vector(data[[x_name]], x_measure)
  y <- correlation_analysis_vector(data[[y_name]], y_measure)
  result <- switch(
    selection$method,
    pearson = correlation_test_result(x, y, "pearson", selection$label),
    polyserial = {
      if (identical(x_measure, "continuous")) {
        correlation_polyserial_result(x, y, selection$label)
      } else {
        correlation_polyserial_result(y, x, selection$label)
      }
    },
    polychoric = correlation_polychoric_result(x, y, "polychoric", selection$label),
    tetrachoric = correlation_polychoric_result(x, y, "tetrachoric", selection$label),
    cramers_v = correlation_cramers_v_result(x, y),
    eta = {
      if (identical(x_measure, "continuous")) correlation_eta_result(x, y) else correlation_eta_result(y, x)
    },
    correlation_test_result(x, y, "spearman", "Spearman")
  )
  result$reason <- selection$reason
  result$type1 <- correlation_measurement_label(x_measure)
  result$type2 <- correlation_measurement_label(y_measure)
  result
}

correlation_matrix_from_pairs <- function(variables, display_names, pair_results, value = "coefficient") {
  matrix <- matrix(NA_real_, nrow = length(variables), ncol = length(variables))
  dimnames(matrix) <- list(unname(display_names[variables]), unname(display_names[variables]))
  if (length(pair_results) == 0) {
    return(matrix[0, 0, drop = FALSE])
  }
  for (item in pair_results) {
    i <- match(item$x_name, variables)
    j <- match(item$y_name, variables)
    if (!is.na(i) && !is.na(j)) {
      cell_value <- if (identical(value, "p")) item$result$p else item$result$coefficient
      matrix[i, j] <- cell_value
      matrix[j, i] <- cell_value
    }
  }
  matrix
}

correlation_ci_matrix_from_pairs <- function(variables, display_names, pair_results) {
  matrix <- matrix("", nrow = length(variables), ncol = length(variables))
  dimnames(matrix) <- list(unname(display_names[variables]), unname(display_names[variables]))
  if (length(pair_results) == 0) {
    return(matrix[0, 0, drop = FALSE])
  }
  for (item in pair_results) {
    i <- match(item$x_name, variables)
    j <- match(item$y_name, variables)
    if (!is.na(i) && !is.na(j) && all(is.finite(item$result$ci))) {
      cell_value <- sprintf("%s~%s", format_decimal3(item$result$ci[[1]]), format_decimal3(item$result$ci[[2]]))
      matrix[i, j] <- cell_value
      matrix[j, i] <- cell_value
    }
  }
  matrix
}

correlation_method_matrix_from_pairs <- function(variables, display_names, pair_results) {
  matrix <- matrix("", nrow = length(variables), ncol = length(variables))
  dimnames(matrix) <- list(unname(display_names[variables]), unname(display_names[variables]))
  if (length(pair_results) == 0) {
    return(matrix[0, 0, drop = FALSE])
  }
  for (item in pair_results) {
    i <- match(item$x_name, variables)
    j <- match(item$y_name, variables)
    if (!is.na(i) && !is.na(j)) {
      matrix[i, j] <- item$result$label %||% ""
      matrix[j, i] <- item$result$label %||% ""
    }
  }
  matrix
}

correlation_pair_rows_and_matrices <- function(data, variables, display_names, measurements, pair_results, label_prefix = "") {
  rows <- list()
  for (item in pair_results) {
    x_name <- item$x_name
    y_name <- item$y_name
    pair <- item$result
    rows[[length(rows) + 1]] <- data.frame(
      Variable1 = display_names[[x_name]],
      Variable2 = display_names[[y_name]],
      Type1 = pair$type1,
      Type2 = pair$type2,
      N = pair$n,
      Method = pair$label,
      r = format_decimal3(pair$coefficient),
      p = format_p(pair$p),
      `95% CI` = if (all(is.finite(pair$ci))) {
        sprintf("%s~%s", format_decimal3(pair$ci[[1]]), format_decimal3(pair$ci[[2]]))
      } else {
        ""
      },
      Sig = correlation_sig(pair$p),
      Reason = pair$reason,
      check.names = FALSE
    )
  }
  list(
    pairwise_table = if (length(rows) > 0) do.call(rbind, rows) else data.frame(),
    correlation_matrix = correlation_matrix_from_pairs(variables, display_names, pair_results),
    p_matrix = correlation_matrix_from_pairs(variables, display_names, pair_results, value = "p"),
    ci_matrix = correlation_ci_matrix_from_pairs(variables, display_names, pair_results),
    method_matrix = correlation_method_matrix_from_pairs(variables, display_names, pair_results)
  )
}

prepare_correlation_results <- function(
  data,
  variables,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  options = list()
) {
  requested_variables <- as.character(variables %||% character(0))
  variables <- intersect(requested_variables, names(data))
  if (length(variables) < 2) {
    missing <- setdiff(requested_variables, names(data))
    detail <- if (length(missing) > 0) {
      sprintf("Selected variables were not found in the active data: %s.", paste(head(missing, 5), collapse = ", "))
    } else {
      "Select at least two variables for correlation analysis."
    }
    stop(detail, call. = FALSE)
  }

  measurements <- stats::setNames(
    vapply(variables, correlation_measurement, character(1), variable_info = variable_info),
    variables
  )
  valid_counts <- vapply(variables, function(name) {
    values <- correlation_analysis_vector(data[[name]], measurements[[name]])
    sum(!is.na(values))
  }, integer(1))
  unique_counts <- vapply(variables, function(name) {
    values <- correlation_analysis_vector(data[[name]], measurements[[name]])
    length(unique(values[!is.na(values)]))
  }, integer(1))
  keep_variables <- valid_counts >= 3 & unique_counts >= 2
  omitted_names <- names(valid_counts)[!keep_variables]
  variables <- names(valid_counts)[keep_variables]
  if (length(variables) < 2) {
    count_text <- paste(sprintf("%s=N %s, unique %s", names(valid_counts), valid_counts, unique_counts), collapse = "; ")
    stop(sprintf("At least two selected variables must have three or more valid values and at least two unique values. Current counts: %s.", count_text), call. = FALSE)
  }
  measurements <- measurements[variables]

  continuous_method <- as.character(options$continuous_method %||% "auto")
  if (!continuous_method %in% c("auto", "pearson", "spearman", "kendall")) {
    continuous_method <- "auto"
  }
  options$continuous_method <- continuous_method
  normality_checked <- isTRUE(options$normality) || identical(continuous_method, "auto")
  if (identical(continuous_method, "auto") && !isTRUE(options$normality)) {
    options$normality <- TRUE
    options$normality_for_auto <- TRUE
  }
  normality_table <- if (isTRUE(normality_checked)) {
    correlation_normality_summary(data, variables, variable_info, labels, category_table)
  } else {
    data.frame()
  }
  display_names <- stats::setNames(
    vapply(variables, correlation_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
    variables
  )
  use_latent_correlations <- isTRUE(options$latent_correlations)
  pair_results <- list()
  for (i in seq_len(length(variables) - 1)) {
    for (j in seq.int(i + 1, length(variables))) {
      x_name <- variables[[i]]
      y_name <- variables[[j]]
      pair <- if (isTRUE(use_latent_correlations)) {
        correlation_latent_pair_result(data, x_name, y_name, measurements[[x_name]], measurements[[y_name]])
      } else {
        correlation_pair_result(
          data,
          x_name,
          y_name,
          measurements[[x_name]],
          measurements[[y_name]],
          continuous_method,
          normality_table = normality_table,
          normality_checked = normality_checked
        )
      }
      pair_results[[length(pair_results) + 1]] <- list(x_name = x_name, y_name = y_name, result = pair)
    }
  }
  primary <- correlation_pair_rows_and_matrices(data, variables, display_names, measurements, pair_results)

  list(
    variables = variables,
    labels = display_names,
    measurements = measurements,
    data = data[, variables, drop = FALSE],
    options = options,
    normality_table = normality_table,
    omitted_table = if (length(omitted_names) > 0) {
      data.frame(
        Variable = vapply(omitted_names, correlation_variable_display_name, character(1), variable_info = variable_info, labels = labels, category_table = category_table),
        `Valid N` = as.integer(valid_counts[omitted_names]),
        `Unique values` = as.integer(unique_counts[omitted_names]),
        Reason = ifelse(
          valid_counts[omitted_names] < 3,
          "Omitted because fewer than three valid values were available.",
          "Omitted because fewer than two unique values were available."
        ),
        check.names = FALSE
      )
    } else {
      NULL
    },
    pairwise_table = primary$pairwise_table,
    correlation_matrix = primary$correlation_matrix,
    p_matrix = primary$p_matrix,
    ci_matrix = primary$ci_matrix,
    method_matrix = primary$method_matrix,
    latent = NULL,
    matrix_method = "heterogeneous"
  )
}
