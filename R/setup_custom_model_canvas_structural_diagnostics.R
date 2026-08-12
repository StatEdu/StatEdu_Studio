# Structural equation canvas input and fit diagnostic helpers.

structural_canvas_missing_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (!length(variables)) return(list(available = FALSE))
  values <- data[variables]
  n <- nrow(values)
  missing_count <- vapply(values, function(value) sum(is.na(value)), integer(1))
  variable_table <- data.frame(
    Variable = variables, Missing = unname(missing_count),
    Percent = if (n > 0) 100 * unname(missing_count) / n else NA_real_,
    stringsAsFactors = FALSE
  )
  patterns <- apply(is.na(values), 1L, function(row) paste(ifelse(row, "1", "0"), collapse = ""))
  pattern_table <- as.data.frame(table(patterns), stringsAsFactors = FALSE)
  names(pattern_table) <- c("Pattern", "Count")
  pattern_table <- pattern_table[order(-pattern_table$Count), , drop = FALSE]
  pattern_table$Description <- vapply(pattern_table$Pattern, function(pattern) {
    bits <- strsplit(pattern, "", fixed = TRUE)[[1L]]
    missing_variables <- variables[bits == "1"]
    if (length(missing_variables)) paste("Missing:", paste(missing_variables, collapse = ", ")) else "Complete"
  }, character(1))
  list(
    available = TRUE, n = n, complete_n = sum(stats::complete.cases(values)),
    incomplete_n = sum(!stats::complete.cases(values)), pattern_count = nrow(pattern_table),
    variables = variable_table, patterns = pattern_table
  )
}

structural_canvas_mahalanobis_diagnostics <- function(data, variables, alpha = .001) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L || !all(vapply(data[variables], is.numeric, logical(1)))) return(list(available = FALSE, reason = "At least two numeric continuous indicators are required."))
  complete_rows <- which(stats::complete.cases(data[variables]))
  values <- as.matrix(data[complete_rows, variables, drop = FALSE])
  p <- ncol(values)
  if (nrow(values) <= p + 1L) return(list(available = FALSE, reason = "Too few complete cases for multivariate outlier diagnostics."))
  covariance <- stats::cov(values)
  inverse <- tryCatch(solve(covariance), error = function(error) NULL)
  if (is.null(inverse)) return(list(available = FALSE, reason = "The complete-case covariance matrix is singular."))
  centered <- sweep(values, 2L, colMeans(values), "-")
  distances <- rowSums((centered %*% inverse) * centered)
  pvalues <- stats::pchisq(distances, df = p, lower.tail = FALSE)
  flagged <- pvalues < alpha
  table <- data.frame(
    Row = complete_rows[flagged], Mahalanobis = distances[flagged], df = p,
    p = pvalues[flagged], stringsAsFactors = FALSE
  )
  if (nrow(table)) table <- table[order(-table$Mahalanobis), , drop = FALSE]
  list(available = TRUE, n = nrow(values), p = p, alpha = alpha, flagged_n = sum(flagged), table = table)
}

structural_canvas_fit_guidance <- function(fit_values) {
  values <- as.numeric(fit_values)
  if (length(values) < 8L) stop("Fit guidance requires the standard fit-measure vector.")
  df <- values[[2L]]
  metrics <- c(CFI = values[[5L]], TLI = values[[6L]], SRMR = values[[7L]], RMSEA = values[[8L]])
  if (!is.finite(df) || df <= 0) {
    return(data.frame(Metric = names(metrics), Value = metrics, Guidance = "Not assessed", Reference = "Saturated/df <= 0", stringsAsFactors = FALSE))
  }
  classify_incremental <- function(value) if (!is.finite(value)) "Not assessed" else if (value >= .95) "Good" else if (value >= .90) "Marginal" else "Review"
  classify_rmsea <- function(value) if (!is.finite(value)) "Not assessed" else if (value <= .06) "Good" else if (value <= .08) "Marginal" else "Review"
  classify_srmr <- function(value) if (!is.finite(value)) "Not assessed" else if (value <= .08) "Good" else if (value <= .10) "Marginal" else "Review"
  guidance <- c(classify_incremental(metrics[["CFI"]]), classify_incremental(metrics[["TLI"]]), classify_srmr(metrics[["SRMR"]]), classify_rmsea(metrics[["RMSEA"]]))
  references <- c("Good >= .95; Marginal >= .90", "Good >= .95; Marginal >= .90", "Good <= .08; Marginal <= .10", "Good <= .06; Marginal <= .08")
  data.frame(Metric = names(metrics), Value = unname(metrics), Guidance = guidance, Reference = references, stringsAsFactors = FALSE)
}

structural_canvas_ordered_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  # Nominal indicators have no defensible category ordering for lavaan's
  # threshold/polychoric CFA. Do not silently impose their factor level order.
  ordered_levels <- c("binary", "ordinal", "ordered")
  is_ordered <- indicators %in% names(measurements) & measurements[indicators] %in% ordered_levels
  indicators[!is.na(is_ordered) & is_ordered]
}

structural_canvas_nominal_indicators <- function(snapshot, variable_table = NULL) {
  if (is.null(variable_table) || !all(c("name", "measurement") %in% names(variable_table))) return(character(0))
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), snapshot$nodes %||% list()), structural_canvas_name, character(1)))
  measurements <- tolower(as.character(variable_table$measurement))
  names(measurements) <- as.character(variable_table$name)
  nominal_levels <- c("category", "categorical", "factor", "nominal")
  is_nominal <- indicators %in% names(measurements) & measurements[indicators] %in% nominal_levels
  indicators[!is.na(is_nominal) & is_nominal]
}

structural_canvas_missing_exogenous_covariances <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  endogenous_ids <- unique(vapply(Filter(function(edge) {
    if (identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "latent") && identical(to$role, "latent")
  }, edges), function(edge) as.character(edge$to), character(1)))
  exogenous <- Filter(function(node) !as.character(node$id) %in% endogenous_ids, latents)
  if (length(exogenous) < 2L) return(character(0))
  pairs <- utils::combn(exogenous, 2L, simplify = FALSE)
  missing <- Filter(function(pair) {
    ids <- vapply(pair, function(node) as.character(node$id), character(1))
    !any(vapply(edges, function(edge) {
      if (!identical(edge$kind, "covariance")) return(FALSE)
      endpoints <- c(as.character(edge$from), as.character(edge$to))
      setequal(endpoints, ids)
    }, logical(1)))
  }, pairs)
  vapply(missing, function(pair) paste(vapply(pair, structural_canvas_name, character(1)), collapse = " ↔ "), character(1))
}

structural_canvas_mardia <- function(data, variables, max_n = 2000L) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L) return(list(available = FALSE, reason = "At least two continuous indicators are required."))
  values <- data[variables]
  if (!all(vapply(values, is.numeric, logical(1)))) return(list(available = FALSE, reason = "All indicators must be numeric and continuous."))
  values <- values[stats::complete.cases(values), , drop = FALSE]
  original_n <- nrow(values)
  p <- ncol(values)
  if (original_n <= p + 1L) return(list(available = FALSE, reason = "Too few complete cases for the number of indicators."))
  sampled <- original_n > max_n
  if (sampled) values <- values[unique(round(seq(1, original_n, length.out = max_n))), , drop = FALSE]
  n <- nrow(values)
  centered <- sweep(as.matrix(values), 2L, colMeans(values), "-")
  covariance <- crossprod(centered) / n
  inverse <- tryCatch(solve(covariance), error = function(error) NULL)
  if (is.null(inverse)) return(list(available = FALSE, reason = "The indicator covariance matrix is singular."))
  distances <- centered %*% inverse %*% t(centered)
  skewness <- mean(distances^3)
  skew_statistic <- n * skewness / 6
  skew_df <- p * (p + 1L) * (p + 2L) / 6
  skew_p <- stats::pchisq(skew_statistic, df = skew_df, lower.tail = FALSE)
  kurtosis <- mean(diag(distances)^2)
  expected_kurtosis <- p * (p + 2L)
  kurtosis_z <- (kurtosis - expected_kurtosis) / sqrt(8 * p * (p + 2L) / n)
  kurtosis_p <- 2 * stats::pnorm(abs(kurtosis_z), lower.tail = FALSE)
  nonnormal <- is.finite(skew_p) && is.finite(kurtosis_p) && (skew_p < .05 || kurtosis_p < .05)
  list(
    available = TRUE, n = n, original_n = original_n, p = p, sampled = sampled,
    skewness = skewness, skew_statistic = skew_statistic, skew_df = skew_df, skew_p = skew_p,
    kurtosis = kurtosis, expected_kurtosis = expected_kurtosis, kurtosis_z = kurtosis_z, kurtosis_p = kurtosis_p,
    recommendation = if (nonnormal) "MLR recommended" else "ML is acceptable",
    nonnormal = nonnormal
  )
}

structural_canvas_estimator_recommendation <- function(snapshot, data, variable_table, analysis_type = "cfa", estimator = "ML") {
  estimator <- toupper(as.character(estimator %||% "ML"))
  if (!analysis_type %in% c("cfa", "cbsem") || !identical(estimator, "ML")) {
    return(list(recommend = FALSE, reason = "Estimator recommendation is only evaluated for ML CFA/SEM."))
  }
  ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
  if (length(ordered)) return(list(recommend = FALSE, reason = "Ordered indicators are handled by WLSMV selection."))
  nodes <- snapshot$nodes %||% list()
  indicators <- unique(vapply(Filter(function(node) identical(node$role, "indicator"), nodes), structural_canvas_name, character(1)))
  indicators <- intersect(indicators, names(data %||% data.frame()))
  diagnosis <- structural_canvas_mardia(data, indicators)
  if (!isTRUE(diagnosis$available)) return(list(recommend = FALSE, reason = diagnosis$reason %||% "Mardia diagnostic unavailable.", diagnosis = diagnosis))
  list(
    recommend = isTRUE(diagnosis$nonnormal),
    recommended_estimator = if (isTRUE(diagnosis$nonnormal)) "MLR" else "ML",
    reason = if (isTRUE(diagnosis$nonnormal)) "Mardia skewness or kurtosis test indicates nonnormal continuous indicators." else "Mardia diagnostics do not flag nonnormality.",
    diagnosis = diagnosis
  )
}

structural_canvas_constrained_single_indicators <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  result <- character(0)
  for (latent in latents) {
    latent_id <- as.character(latent$id %||% "")
    measurement_edges <- Filter(function(edge) {
      if (identical(edge$kind, "covariance")) return(FALSE)
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(as.character(from$id %||% ""), latent_id) && identical(to$role, "indicator")) ||
        (identical(as.character(to$id %||% ""), latent_id) && identical(from$role, "indicator"))
    }, edges)
    indicators <- unique(vapply(measurement_edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      as.character(if (identical(from$role, "indicator")) from$id else to$id)
    }, character(1)))
    if (length(indicators) != 1L) next
    constrained <- any(vapply(edges, function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      identical(as.character(edge$to %||% ""), indicators[[1L]]) && !is.null(from) && identical(from$role, "error") &&
        identical(edge$free, FALSE) && is.finite(suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))) &&
        suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_)) > 0
    }, logical(1)))
    if (constrained) result <- c(result, structural_canvas_name(latent))
  }
  unique(result)
}

structural_canvas_fixed_residual_scale_diagnostics <- function(snapshot, data, ordered = character(0)) {
  if (!is.data.frame(data)) return(data.frame())
  ordered <- unique(as.character(ordered))
  rows <- list()
  for (edge in snapshot$edges %||% list()) {
    if (!identical(edge$free, FALSE) || identical(edge$kind, "covariance")) next
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    if (is.null(from) || is.null(to) || !from$role %in% c("error", "disturbance") || !identical(to$role, "indicator")) next
    indicator <- structural_canvas_name(to)
    if (!indicator %in% names(data) || indicator %in% ordered || !is.numeric(data[[indicator]])) next
    fixed_value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
    observed_variance <- stats::var(data[[indicator]], na.rm = TRUE)
    if (!is.finite(fixed_value) || !is.finite(observed_variance)) next
    indicator_id <- as.character(to$id %||% "")
    parent_latents <- unique(vapply(Filter(function(measurement_edge) {
      if (identical(measurement_edge$kind, "covariance")) return(FALSE)
      measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
      measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
      (identical(as.character(measurement_from$id %||% ""), indicator_id) && identical(measurement_to$role, "latent")) ||
        (identical(as.character(measurement_to$id %||% ""), indicator_id) && identical(measurement_from$role, "latent"))
    }, snapshot$edges %||% list()), function(measurement_edge) {
      measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
      measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
      as.character(if (identical(measurement_from$role, "latent")) measurement_from$id else measurement_to$id)
    }, character(1)))
    parent_indicator_counts <- vapply(parent_latents, function(parent_id) {
      length(unique(vapply(Filter(function(measurement_edge) {
        if (identical(measurement_edge$kind, "covariance")) return(FALSE)
        measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
        measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
        (identical(as.character(measurement_from$id %||% ""), parent_id) && identical(measurement_to$role, "indicator")) ||
          (identical(as.character(measurement_to$id %||% ""), parent_id) && identical(measurement_from$role, "indicator"))
      }, snapshot$edges %||% list()), function(measurement_edge) {
        measurement_from <- structural_canvas_node(snapshot, measurement_edge$from)
        measurement_to <- structural_canvas_node(snapshot, measurement_edge$to)
        as.character(if (identical(measurement_from$role, "indicator")) measurement_from$id else measurement_to$id)
      }, character(1))))
    }, integer(1))
    single_indicator_factor <- length(parent_indicator_counts) > 0L && any(parent_indicator_counts == 1L)
    status <- if (fixed_value > observed_variance) "Exceeds observed variance" else if (fixed_value == observed_variance) "Equals observed variance" else "Within observed variance"
    rows[[length(rows) + 1L]] <- data.frame(
      Indicator = indicator, `Fixed residual variance` = fixed_value,
      `Observed variance` = observed_variance,
      `Implied maximum common variance` = observed_variance - fixed_value,
      `Single-indicator factor` = single_indicator_factor,
      Status = status, check.names = FALSE
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_indicator_loading_guidance <- function(beta, ci_lower, ci_upper, residual_variance, cross_loaded = FALSE) {
  if (!is.finite(residual_variance) || residual_variance < 0 || residual_variance > 1) return("Review residual variance")
  if (isTRUE(cross_loaded)) return("Review cross-loading")
  if (!is.finite(beta) || !is.finite(ci_lower) || !is.finite(ci_upper)) return("Not assessed")
  if (ci_lower <= 0 && ci_upper >= 0) return("Loading CI includes 0")
  if (abs(beta) < .40) return("Weak loading review")
  "No loading flag"
}

structural_canvas_residual_diagnostics <- function(fit, cutoff = 1.96, top_n = 20L) {
  residual_covariance <- function(type) {
    value <- tryCatch(lavaan::resid(fit, type = type), error = function(error) NULL)
    if (is.null(value)) return(NULL)
    if (!is.null(value$cov)) return(value$cov)
    groups <- Filter(function(group_value) is.list(group_value) && !is.null(group_value$cov), value)
    if (!length(groups)) return(NULL)
    result <- lapply(groups, `[[`, "cov")
    names(result) <- names(groups)
    result
  }
  standardized_raw <- residual_covariance("standardized")
  correlation_raw <- residual_covariance("cor")
  if (is.null(correlation_raw)) return(list(available = FALSE))
  standardized_available <- !is.null(standardized_raw)
  if (is.null(standardized_raw)) standardized_raw <- correlation_raw
  group_labels <- tryCatch(as.character(lavaan::lavInspect(fit, "group.label")), error = function(error) character(0))
  group_count <- tryCatch(as.integer(lavaan::lavInspect(fit, "ngroups")), error = function(error) 1L)
  normalize_residual_list <- function(value) {
    if (is.list(value) && !is.data.frame(value)) {
      result <- lapply(value, as.matrix)
      if (is.null(names(result)) || any(!nzchar(names(result)))) {
        labels <- group_labels
        if (length(labels) != length(result)) labels <- paste("Group", seq_along(result))
        names(result) <- labels
      }
      result
    } else {
      result <- list(as.matrix(value))
      names(result) <- if (length(group_labels) == 1L) group_labels else "Overall"
      result
    }
  }
  standardized_list <- normalize_residual_list(standardized_raw)
  correlation_list <- normalize_residual_list(correlation_raw)
  common_groups <- intersect(names(standardized_list), names(correlation_list))
  if (!length(common_groups)) return(list(available = FALSE))
  pair_table <- function(standardized, correlation, group_name = NULL) {
    standardized <- as.matrix(standardized)
    correlation <- as.matrix(correlation)
    if (!nrow(standardized) || !nrow(correlation)) return(data.frame())
    standardized[upper.tri(standardized, diag = TRUE)] <- NA_real_
    correlation[upper.tri(correlation, diag = TRUE)] <- NA_real_
    locations <- which(is.finite(standardized), arr.ind = TRUE)
    if (!nrow(locations)) return(data.frame())
    result <- data.frame(
      Indicator1 = rownames(standardized)[locations[, "row"]],
      Indicator2 = colnames(standardized)[locations[, "col"]],
      `Standardized residual` = standardized[locations],
      `Correlation residual` = correlation[locations],
      `Residual scale` = if (standardized_available) "Standardized" else "Correlation residual fallback",
      `Exceeds cutoff` = abs(standardized[locations]) >= cutoff,
      check.names = FALSE
    )
    if (!is.null(group_name)) result <- data.frame(Group = group_name, result, row.names = NULL, check.names = FALSE)
    result[order(-abs(result[["Standardized residual"]])), , drop = FALSE]
  }
  by_group <- stats::setNames(lapply(common_groups, function(group_name) {
    standardized <- standardized_list[[group_name]]
    correlation <- correlation_list[[group_name]]
    pairs <- pair_table(standardized, correlation, group_name)
    list(
      standardized = {
        value <- as.matrix(standardized)
        value[upper.tri(value, diag = TRUE)] <- NA_real_
        value
      },
      correlation = {
        value <- as.matrix(correlation)
        value[upper.tri(value, diag = TRUE)] <- NA_real_
        value
      },
      pairs = pairs,
      largest = pairs[pairs[["Exceeds cutoff"]], , drop = FALSE]
    )
  }), common_groups)
  all_pairs <- Filter(function(value) nrow(value), lapply(by_group, `[[`, "pairs"))
  all_pairs <- if (length(all_pairs)) do.call(rbind, all_pairs) else data.frame()
  group_largest <- if (nrow(all_pairs)) all_pairs[all_pairs[["Exceeds cutoff"]], , drop = FALSE] else data.frame()
  if (nrow(group_largest)) group_largest <- utils::head(group_largest[order(-abs(group_largest[["Standardized residual"]])), , drop = FALSE], as.integer(top_n))
  group_summary <- do.call(rbind, lapply(names(by_group), function(group_name) {
    pairs <- by_group[[group_name]]$pairs
    if (!nrow(pairs)) return(data.frame(Group = group_name, `Max |standardized residual|` = NA_real_, Indicator1 = NA_character_, Indicator2 = NA_character_, `Flagged residuals` = 0L, check.names = FALSE))
    top <- pairs[1L, , drop = FALSE]
    data.frame(
      Group = group_name,
      `Max |standardized residual|` = abs(top[["Standardized residual"]][[1L]]),
      Indicator1 = top$Indicator1[[1L]], Indicator2 = top$Indicator2[[1L]],
      `Flagged residuals` = sum(pairs[["Exceeds cutoff"]], na.rm = TRUE),
      check.names = FALSE
    )
  }))
  first_group <- common_groups[[1L]]
  largest <- by_group[[first_group]]$largest
  if (!is.null(group_count) && is.finite(group_count) && group_count == 1L && "Group" %in% names(largest)) {
    largest <- largest[, setdiff(names(largest), c("Group", "Exceeds cutoff")), drop = FALSE]
  }
  list(
    available = TRUE,
    standardized_available = standardized_available,
    standardized = by_group[[first_group]]$standardized,
    correlation = by_group[[first_group]]$correlation,
    largest = largest,
    cutoff = cutoff,
    by_group = by_group,
    group_summary = group_summary,
    group_largest = group_largest,
    group_pairs = all_pairs
  )
}

structural_canvas_factor_correlation_diagnostics <- function(fit) {
  correlations <- as.matrix(lavaan::lavInspect(fit, "cor.lv"))
  if (nrow(correlations) < 2L) return(data.frame())
  locations <- which(lower.tri(correlations), arr.ind = TRUE)
  values <- correlations[locations]
  severity <- vapply(abs(values), function(value) {
    if (!is.finite(value)) "Unavailable"
    else if (value >= 1) "Inadmissible"
    else if (value >= .95) "Severe"
    else if (value >= .90) "High"
    else if (value >= .85) "Review"
    else "Acceptable"
  }, character(1))
  data.frame(
    Factor1 = rownames(correlations)[locations[, "row"]],
    Factor2 = colnames(correlations)[locations[, "col"]],
    Correlation = values, `Absolute correlation` = abs(values), Severity = severity,
    check.names = FALSE
  )
}

structural_canvas_minimum_eigenvalue <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value) || any(!is.finite(matrix_value))) return(NA_real_)
  suppressWarnings(min(eigen((matrix_value + t(matrix_value)) / 2, symmetric = TRUE, only.values = TRUE)$values))
}

structural_canvas_symmetric_condition_number <- function(matrix_value) {
  matrix_value <- as.matrix(matrix_value)
  if (!length(matrix_value) || any(!is.finite(matrix_value))) return(NA_real_)
  values <- suppressWarnings(abs(eigen((matrix_value + t(matrix_value)) / 2, symmetric = TRUE, only.values = TRUE)$values))
  if (!length(values) || max(values) == 0 || min(values) == 0) return(Inf)
  max(values) / min(values)
}

structural_canvas_latent_correlation_intervals <- function(fit, level = .95) {
  latent <- lavaan::lavNames(fit, "lv")
  if (length(latent) < 2L) return(data.frame())
  standardized <- lavaan::standardizedSolution(fit, type = "std.lv", ci = TRUE, level = level)
  rows <- standardized$op == "~~" & standardized$lhs != standardized$rhs &
    standardized$lhs %in% latent & standardized$rhs %in% latent
  values <- standardized[rows, c("lhs", "rhs", "est.std", "ci.lower", "ci.upper", "pvalue"), drop = FALSE]
  if (!nrow(values)) return(data.frame())
  duplicated_pair <- duplicated(vapply(seq_len(nrow(values)), function(index) {
    paste(sort(c(values$lhs[[index]], values$rhs[[index]])), collapse = "\r")
  }, character(1)))
  values <- values[!duplicated_pair, , drop = FALSE]
  parameter_table <- lavaan::parameterTable(fit)
  covariance_parameters <- parameter_table[
    parameter_table$op == "~~" & parameter_table$lhs != parameter_table$rhs &
      parameter_table$lhs %in% latent & parameter_table$rhs %in% latent,
    c("lhs", "rhs", "free"), drop = FALSE
  ]
  pair_key <- function(lhs, rhs) paste(sort(c(lhs, rhs)), collapse = "\r")
  value_keys <- vapply(seq_len(nrow(values)), function(index) pair_key(values$lhs[[index]], values$rhs[[index]]), character(1))
  parameter_keys <- vapply(seq_len(nrow(covariance_parameters)), function(index) pair_key(covariance_parameters$lhs[[index]], covariance_parameters$rhs[[index]]), character(1))
  free <- covariance_parameters$free[match(value_keys, parameter_keys)]
  data.frame(
    `Factor 1` = values$lhs, `Factor 2` = values$rhs,
    r = values$est.std, `CI lower` = values$ci.lower, `CI upper` = values$ci.upper,
    p = values$pvalue, Type = ifelse(!is.na(free) & free > 0L, "Estimated", "Fixed"),
    `CI reaches |1|` = ifelse(
      is.finite(values$ci.lower) & is.finite(values$ci.upper),
      ifelse(values$ci.lower <= -1 | values$ci.upper >= 1, "Yes", "No"), "Not assessed"
    ),
    check.names = FALSE
  )
}

structural_canvas_ordered_category_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  rows <- list()
  for (name in variables) {
    value <- data[[name]]
    levels_value <- if (is.factor(value)) levels(value) else sort(unique(value[!is.na(value)]))
    counts <- table(factor(value, levels = levels_value), useNA = "no")
    valid_n <- sum(counts)
    sparse_limit <- max(5L, ceiling(.01 * valid_n))
    for (index in seq_along(counts)) {
      count <- as.integer(counts[[index]])
      rows[[length(rows) + 1L]] <- data.frame(
        Indicator = name, Category = names(counts)[[index]], Count = count,
        Percent = if (valid_n > 0) 100 * count / valid_n else NA_real_,
        Status = if (count == 0L) "Empty" else if (count <= sparse_limit) "Sparse" else if (valid_n > 0 && count / valid_n >= .95) "Dominant (>=95%)" else "Adequate",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

structural_canvas_ordered_pair_diagnostics <- function(data, variables) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L) return(data.frame())
  pairs <- utils::combn(variables, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    first <- data[[pair[[1L]]]]
    second <- data[[pair[[2L]]]]
    first_levels <- if (is.factor(first)) levels(first) else sort(unique(first[!is.na(first)]))
    second_levels <- if (is.factor(second)) levels(second) else sort(unique(second[!is.na(second)]))
    contingency <- table(factor(first, levels = first_levels), factor(second, levels = second_levels), useNA = "no")
    valid_n <- sum(contingency)
    sparse_limit <- max(5L, ceiling(.01 * valid_n))
    counts <- as.integer(contingency)
    zero <- sum(counts == 0L)
    sparse <- sum(counts > 0L & counts <= sparse_limit)
    data.frame(
      `Indicator 1` = pair[[1L]], `Indicator 2` = pair[[2L]], `Valid pairs` = valid_n,
      `Cells` = length(counts), `Empty cells` = zero, `Sparse nonempty cells` = sparse,
      `Minimum nonzero count` = if (any(counts > 0L)) min(counts[counts > 0L]) else 0L,
      `Empty %` = if (length(counts)) 100 * zero / length(counts) else NA_real_,
      Status = if (zero > 0L || sparse > 0L) "Review" else "Adequate", check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_error_covariance_diagnostics <- function(snapshot) {
  edges <- snapshot$edges %||% list()
  covariance_edges <- Filter(function(edge) {
    if (!identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && identical(from$role, "error") && identical(to$role, "error")
  }, edges)
  indicator_count <- sum(vapply(snapshot$nodes %||% list(), function(node) identical(node$role, "indicator"), logical(1)))
  possible <- if (indicator_count >= 2L) choose(indicator_count, 2L) else 0
  count <- length(covariance_edges)
  ratio <- if (possible > 0) count / possible else 0
  list(count = count, possible = possible, ratio = ratio, status = if (count == 0L) "None" else if (count >= 3L || ratio > .20) "Review complexity" else "Limited")
}

structural_canvas_identification_diagnostics <- function(snapshot) {
  nodes <- snapshot$nodes %||% list()
  edges <- snapshot$edges %||% list()
  latents <- Filter(function(node) identical(node$role, "latent"), nodes)
  issues <- list()
  add <- function(severity, element, code, message) {
    issues[[length(issues) + 1L]] <<- data.frame(Severity = severity, Element = element, Code = code, Message = message, stringsAsFactors = FALSE)
  }
  higher_edges <- Filter(function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), edges)
  for (latent in latents) {
    latent_id <- as.character(latent$id)
    latent_name <- structural_canvas_name(latent)
    observed <- unique(vapply(Filter(function(edge) {
      if (identical(edge$kind, "covariance")) return(FALSE)
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      (identical(as.character(from$id %||% ""), latent_id) && identical(to$role, "indicator")) ||
        (identical(as.character(to$id %||% ""), latent_id) && identical(from$role, "indicator"))
    }, edges), function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "indicator")) from else to)
    }, character(1)))
    children <- unique(vapply(Filter(function(edge) identical(as.character(edge$from), latent_id), higher_edges), function(edge) structural_canvas_name(structural_canvas_node(snapshot, edge$to)), character(1)))
    if (!length(observed) && !length(children)) add("Error", latent_name, "unmeasured_latent", "The latent variable has neither observed indicators nor lower-order factors.")
    single_indicator_constrained <- FALSE
    if (length(observed) == 1L && !length(children)) {
      indicator_node <- Filter(function(node) identical(node$role, "indicator") && identical(structural_canvas_name(node), observed[[1L]]), nodes)
      indicator_id <- if (length(indicator_node)) as.character(indicator_node[[1L]]$id) else ""
      single_indicator_constrained <- any(vapply(edges, function(edge) {
        from <- structural_canvas_node(snapshot, edge$from)
        identical(as.character(edge$to), indicator_id) && !is.null(from) && identical(from$role, "error") &&
          identical(edge$free, FALSE) && is.finite(suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))) &&
          suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_)) > 0
      }, logical(1)))
      if (!single_indicator_constrained) add("Error", latent_name, "single_indicator", "A single-indicator factor requires an externally justified fixed residual variance on its error path.")
      else add("Warning", latent_name, "single_indicator_constrained", "The single-indicator factor is identified using a fixed residual variance; document the external reliability basis for this constraint.")
    }
    if (length(observed) == 2L && !length(children)) add("Warning", latent_name, "two_indicators", "A two-indicator factor may require additional constraints or structural information for stable identification.")
    if (length(children) > 0L && length(children) < 3L) add("Error", latent_name, "few_lower_order_factors", "A higher-order factor requires at least three lower-order factors in the current automatic identification scheme.")
    if (length(observed) && length(children)) add("Warning", latent_name, "mixed_measurement_level", "The factor has both observed indicators and lower-order factors; verify that this hybrid measurement specification is intentional.")
  }
  if (length(higher_edges)) {
    child_ids <- vapply(higher_edges, function(edge) as.character(edge$to), character(1))
    for (child_id in unique(child_ids[duplicated(child_ids)])) {
      child <- structural_canvas_node(snapshot, child_id)
      add("Warning", structural_canvas_name(child), "multiple_higher_order_parents", "The lower-order factor loads on more than one higher-order factor; standard higher-order reliability summaries may not apply.")
    }
  }
  measurement_edges <- Filter(function(edge) {
    if (identical(edge$kind, "covariance") || identical(as.character(edge$pathType %||% ""), "higherOrder")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) &&
      ((identical(from$role, "latent") && identical(to$role, "indicator")) ||
        (identical(to$role, "latent") && identical(from$role, "indicator")))
  }, edges)
  indicator_parents <- split(measurement_edges, vapply(measurement_edges, function(edge) {
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    structural_canvas_name(if (identical(from$role, "indicator")) from else to)
  }, character(1)))
  for (indicator_name in names(indicator_parents)) {
    parents <- unique(vapply(indicator_parents[[indicator_name]], function(edge) {
      from <- structural_canvas_node(snapshot, edge$from)
      to <- structural_canvas_node(snapshot, edge$to)
      structural_canvas_name(if (identical(from$role, "latent")) from else to)
    }, character(1)))
    if (length(parents) > 1L) add("Warning", indicator_name, "cross_loading", paste0("The indicator loads on multiple factors: ", paste(parents, collapse = ", "), ". Review simple-structure assumptions and reliability/validity summaries."))
  }
  fixed_residual_edges <- Filter(function(edge) {
    if (!identical(edge$free, FALSE) || identical(edge$kind, "covariance")) return(FALSE)
    from <- structural_canvas_node(snapshot, edge$from)
    to <- structural_canvas_node(snapshot, edge$to)
    !is.null(from) && !is.null(to) && from$role %in% c("error", "disturbance") && to$role %in% c("indicator", "latent")
  }, edges)
  for (edge in fixed_residual_edges) {
    target <- structural_canvas_node(snapshot, edge$to)
    value <- suppressWarnings(as.numeric(edge$fixedValue %||% NA_real_))
    if (!is.finite(value)) {
      add("Error", structural_canvas_name(target), "invalid_fixed_residual", "A fixed residual variance must be a finite nonnegative number.")
    } else if (value < 0) {
      add("Error", structural_canvas_name(target), "negative_fixed_residual", "A residual variance cannot be fixed to a negative value.")
    } else if (value == 0) {
      add("Warning", structural_canvas_name(target), "boundary_fixed_residual", "A zero fixed residual variance is a boundary constraint implying perfect residual-free measurement; provide strong substantive justification.")
    }
  }
  directed <- Filter(function(edge) !identical(edge$kind, "covariance"), edges)
  signatures <- vapply(directed, function(edge) paste(edge$from, edge$to, edge$pathType %||% "regression", sep = "|"), character(1))
  if (anyDuplicated(signatures)) add("Error", "Model", "duplicate_path", "Duplicate directed paths were found between the same endpoints.")
  result <- if (length(issues)) do.call(rbind, issues) else data.frame(Severity = character(0), Element = character(0), Code = character(0), Message = character(0), stringsAsFactors = FALSE)
  rownames(result) <- NULL
  result
}

structural_canvas_higher_order_results <- function(snapshot, fit) {
  higher_edges <- Filter(function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), snapshot$edges %||% list())
  if (!length(higher_edges)) return(list(available = FALSE, reason = "No higher-order loading paths are specified."))
  raw <- lavaan::parameterEstimates(fit)
  standardized <- lavaan::standardizedSolution(fit, ci = TRUE, level = .95)
  rows <- lapply(higher_edges, function(edge) {
    parent <- structural_canvas_name(structural_canvas_node(snapshot, edge$from))
    child <- structural_canvas_name(structural_canvas_node(snapshot, edge$to))
    raw_row <- raw[raw$lhs == parent & raw$op == "=~" & raw$rhs == child, , drop = FALSE]
    std_row <- standardized[standardized$lhs == parent & standardized$op == "=~" & standardized$rhs == child, , drop = FALSE]
    child_r2 <- lavaan::lavInspect(fit, "r2")
    residual_row <- standardized[standardized$lhs == child & standardized$op == "~~" & standardized$rhs == child, , drop = FALSE]
    data.frame(
      HigherOrderFactor = parent, LowerOrderFactor = child,
      B = if (nrow(raw_row)) raw_row$est[[1L]] else NA_real_,
      BCILower = if (nrow(raw_row)) raw_row$ci.lower[[1L]] else NA_real_,
      BCIUpper = if (nrow(raw_row)) raw_row$ci.upper[[1L]] else NA_real_,
      SE = if (nrow(raw_row)) raw_row$se[[1L]] else NA_real_,
      z = if (nrow(raw_row)) raw_row$z[[1L]] else NA_real_,
      p = if (nrow(raw_row)) raw_row$pvalue[[1L]] else NA_real_,
      Beta = if (nrow(std_row)) std_row$est.std[[1L]] else NA_real_,
      BetaCILower = if (nrow(std_row)) std_row$ci.lower[[1L]] else NA_real_,
      BetaCIUpper = if (nrow(std_row)) std_row$ci.upper[[1L]] else NA_real_,
      R2 = as.numeric(child_r2[child]),
      R2CILower = if (nrow(residual_row)) 1 - residual_row$ci.upper[[1L]] else NA_real_,
      R2CIUpper = if (nrow(residual_row)) 1 - residual_row$ci.lower[[1L]] else NA_real_,
      ResidualVariance = if (nrow(residual_row)) residual_row$est.std[[1L]] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  list(available = TRUE, table = do.call(rbind, rows), higher_edges = higher_edges)
}

structural_canvas_omega_h <- function(snapshot, fit) {
  higher <- structural_canvas_higher_order_results(snapshot, fit)
  if (!isTRUE(higher$available)) return(list(available = FALSE, reason = higher$reason))
  parents <- unique(higher$table$HigherOrderFactor)
  if (length(parents) != 1L) return(list(available = FALSE, reason = "Omega-h requires exactly one higher-order general factor."))
  if (anyDuplicated(higher$table$LowerOrderFactor)) return(list(available = FALSE, reason = "Omega-h is not reported when a lower-order factor has multiple higher-order loadings."))
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  first_loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
  children <- higher$table$LowerOrderFactor
  first_loadings <- first_loadings[first_loadings$lhs %in% children, , drop = FALSE]
  if (!nrow(first_loadings)) return(list(available = FALSE, reason = "No observed indicators were found under the lower-order factors."))
  if (anyDuplicated(first_loadings$rhs)) return(list(available = FALSE, reason = "Omega-h is not reported with cross-loaded observed indicators."))
  second_loadings <- stats::setNames(higher$table$Beta, higher$table$LowerOrderFactor)
  general_loadings <- first_loadings$est.std * second_loadings[first_loadings$lhs]
  implied_covariance <- as.matrix(lavaan::fitted(fit)$cov)
  indicator_names <- first_loadings$rhs
  if (!all(indicator_names %in% rownames(implied_covariance))) return(list(available = FALSE, reason = "The model-implied indicator covariance matrix is unavailable."))
  implied_correlation <- stats::cov2cor(implied_covariance[indicator_names, indicator_names, drop = FALSE])
  denominator <- sum(implied_correlation, na.rm = TRUE)
  omega_h <- if (is.finite(denominator) && denominator > 0) sum(general_loadings, na.rm = TRUE)^2 / denominator else NA_real_
  list(
    available = is.finite(omega_h), omega_h = omega_h, higher_order_factor = parents[[1L]],
    indicators = length(indicator_names), general_loadings = stats::setNames(as.numeric(general_loadings), indicator_names),
    reason = if (is.finite(omega_h)) "" else "Omega-h denominator is not positive and finite."
  )
}

structural_canvas_higher_order_loading_guidance <- function(value) {
  if (!is.finite(value)) "Not assessed"
  else if (abs(value) < .40) "Weak loading review"
  else "No loading flag"
}

structural_canvas_omega_h_guidance <- function(value) {
  if (!is.finite(value) || value < 0 || value > 1) "Review inadmissible coefficient"
  else if (value < .70) "Below common .70 guideline"
  else "Meets common .70 guideline"
}

structural_canvas_factor_score_quality <- function(fit) {
  predicted <- tryCatch(lavaan::lavPredict(fit, method = "regression", rel = TRUE), error = function(error) NULL)
  if (is.null(predicted)) return(data.frame())
  reliability <- attr(predicted, "rel", exact = TRUE)
  if (is.list(reliability)) reliability <- unlist(reliability, use.names = TRUE)
  reliability <- as.numeric(reliability)
  factor_names <- names(unlist(attr(predicted, "rel", exact = TRUE), use.names = TRUE))
  if (!length(factor_names) || length(factor_names) != length(reliability)) factor_names <- colnames(as.matrix(predicted))
  if (!length(reliability) || length(factor_names) != length(reliability)) return(data.frame())
  determinacy <- ifelse(is.finite(reliability) & reliability >= 0, sqrt(reliability), NA_real_)
  guidance <- vapply(determinacy, function(value) {
    if (!is.finite(value) || value > 1) "Not assessed"
    else if (value >= .90) "Strong"
    else if (value >= .80) "Acceptable for cautious use"
    else "Low; avoid individual-score use"
  }, character(1))
  data.frame(Factor = factor_names, Determinacy = determinacy, `Score reliability` = reliability, Guidance = guidance, check.names = FALSE)
}
