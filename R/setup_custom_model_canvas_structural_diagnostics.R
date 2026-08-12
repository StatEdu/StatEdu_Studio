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
