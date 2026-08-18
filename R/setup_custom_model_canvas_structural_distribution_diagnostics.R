# Structural distribution and data diagnostic helpers.

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
  observed <- !is.na(values)
  pairwise_n <- crossprod(as.matrix(observed) * 1L)
  dimnames(pairwise_n) <- list(variables, variables)
  off_diagonal <- pairwise_n[lower.tri(pairwise_n)]
  list(
    available = TRUE, n = n, complete_n = sum(stats::complete.cases(values)),
    incomplete_n = sum(!stats::complete.cases(values)), pattern_count = nrow(pattern_table),
    incomplete_percent = if (n > 0) 100 * mean(!stats::complete.cases(values)) else NA_real_,
    variables = variable_table, patterns = pattern_table, pairwise_n = pairwise_n,
    minimum_pairwise_n = if (length(off_diagonal)) min(off_diagonal) else if (length(pairwise_n)) pairwise_n[[1L]] else NA_real_
  )
}

structural_canvas_missing_sensitivity_rows <- function(bundle) {
  method <- as.character(bundle$missing_sensitivity_method %||% "not_assessed")
  details <- trimws(as.character(bundle$missing_sensitivity_details %||% ""))
  labels <- c(
    not_assessed = "Not assessed", complete_case_comparison = "Complete-case comparison",
    multiple_imputation = "Multiple-imputation comparison", delta_pattern_mixture = "Delta/pattern-mixture",
    external_analysis = "External sensitivity analysis", other_documented = "Other documented assessment"
  )
  has_missing <- isTRUE(bundle$missing_diagnostics$available) && as.integer(bundle$missing_diagnostics$incomplete_n %||% 0L) > 0L
  requires_review <- has_missing && identical(bundle$missing %||% "", "fiml") && (identical(method, "not_assessed") || !nzchar(details))
  data.frame(
    `Primary missing-data method` = as.character(bundle$missing %||% "Not recorded"),
    `Sensitivity assessment` = unname(labels[method] %||% method),
    `Assumptions, results, and conclusion` = details,
    Status = if (!has_missing) "Not required - no incomplete indicator cases" else if (requires_review) "Review" else if (identical(method, "not_assessed")) "Not assessed" else "Documented",
    Guidance = if (requires_review) "FIML relies on MAR conditional on modeled variables. Document a sensitivity assessment and whether key conclusions change under plausible departures." else "Sensitivity records are user-supplied evidence and do not cause this engine to perform or validate the stated external analysis.",
    stringsAsFactors = FALSE, check.names = FALSE
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

structural_canvas_mardia <- function(data, variables, max_n = 2000L, seed = 20260818L) {
  variables <- intersect(unique(as.character(variables)), names(data))
  if (length(variables) < 2L) return(list(available = FALSE, reason = "At least two continuous indicators are required."))
  values <- data[variables]
  if (!all(vapply(values, is.numeric, logical(1)))) return(list(available = FALSE, reason = "All indicators must be numeric and continuous."))
  complete_rows <- which(stats::complete.cases(values))
  values <- values[complete_rows, , drop = FALSE]
  original_n <- nrow(values)
  p <- ncol(values)
  if (original_n <= p + 1L) return(list(available = FALSE, reason = "Too few complete cases for the number of indicators."))
  sampled <- original_n > max_n
  sample_indices <- seq_len(original_n)
  if (sampled) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (old_seed_exists) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(as.integer(seed))
    sample_indices <- sort(sample.int(original_n, size = as.integer(max_n), replace = FALSE))
    values <- values[sample_indices, , drop = FALSE]
  }
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
    sampling_method = if (sampled) "seeded simple random sample without replacement" else "all complete cases",
    sampling_seed = if (sampled) as.integer(seed) else NA_integer_,
    sampled_rows = complete_rows[sample_indices],
    skewness = skewness, skew_statistic = skew_statistic, skew_df = skew_df, skew_p = skew_p,
    kurtosis = kurtosis, expected_kurtosis = expected_kurtosis, kurtosis_z = kurtosis_z, kurtosis_p = kurtosis_p,
    recommendation = if (nonnormal) "MLR recommended" else "No Mardia test flag; normality not established",
    nonnormal = nonnormal, test_flag = nonnormal,
    interpretation = if (nonnormal) "At least one sample-size-sensitive omnibus test flagged departure from multivariate normality." else "Neither omnibus test was significant; this is not evidence that multivariate normality holds."
  )
}

structural_canvas_estimator_recommendation <- function(snapshot, data, variable_table, analysis_type = "cfa", estimator = "ML") {
  estimator <- toupper(as.character(estimator %||% "ML"))
  if (!analysis_type %in% c("cfa", "cbsem", "sem") || !identical(estimator, "ML")) {
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
    reason = if (isTRUE(diagnosis$nonnormal)) "At least one sample-size-sensitive Mardia test flags departure from multivariate normality." else "Mardia tests did not flag departure; this does not establish multivariate normality.",
    diagnosis = diagnosis
  )
}
