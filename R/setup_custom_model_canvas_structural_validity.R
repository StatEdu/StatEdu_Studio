# Structural equation canvas convergent and discriminant validity helpers.

structural_canvas_fornell_larcker <- function(ave, correlations, indicator_counts = NULL, assessable = TRUE) {
  latent_names <- names(ave)
  correlations <- as.matrix(correlations)
  if (is.null(indicator_counts)) indicator_counts <- stats::setNames(rep(2L, length(latent_names)), latent_names)
  max_correlation <- stats::setNames(rep(NA_real_, length(latent_names)), latent_names)
  criterion <- stats::setNames(rep("Not assessed", length(latent_names)), latent_names)
  if (length(latent_names) < 2L || !isTRUE(assessable)) return(list(max_correlation = max_correlation, criterion = criterion))
  for (name in latent_names) {
    others <- setdiff(latent_names, name)
    values <- abs(correlations[name, others, drop = TRUE])
    values <- values[is.finite(values)]
    if (length(values)) max_correlation[[name]] <- max(values)
    if ((indicator_counts[[name]] %||% 0L) < 2L || !is.finite(ave[[name]]) || !is.finite(max_correlation[[name]])) next
    criterion[[name]] <- if (sqrt(ave[[name]]) > max_correlation[[name]]) "Below reference" else "Review needed"
  }
  list(max_correlation = max_correlation, criterion = criterion)
}

structural_canvas_htmt <- function(correlations, indicators_by_factor, threshold = .85, include_pairs = TRUE) {
  correlations <- as.matrix(correlations)
  factor_names <- names(indicators_by_factor)
  matrix_result <- matrix(NA_real_, length(factor_names), length(factor_names), dimnames = list(factor_names, factor_names))
  pairs <- list()
  if (length(factor_names) < 2L) return(list(matrix = matrix_result, pairs = data.frame(), threshold = threshold))
  pair_index <- 0L
  # Reuse each factor's within-factor mean only for this correlation matrix.
  # Fill lazily after pair validation so unavailable/overlapping indicators
  # keep their original diagnostic and never trigger an extra matrix lookup.
  within_means <- stats::setNames(vector("list", length(factor_names)), factor_names)
  for (indices in utils::combn(seq_along(factor_names), 2L, simplify = FALSE)) {
    first <- factor_names[[indices[[1L]]]]
    second <- factor_names[[indices[[2L]]]]
    first_indicators <- unique(indicators_by_factor[[first]])
    second_indicators <- unique(indicators_by_factor[[second]])
    reason <- ""
    value <- NA_real_
    if (length(first_indicators) < 2L || length(second_indicators) < 2L) {
      reason <- "At least two indicators per factor are required"
    } else if (length(intersect(first_indicators, second_indicators))) {
      reason <- "Cross-loaded indicators prevent standard HTMT calculation"
    } else if (!all(c(first_indicators, second_indicators) %in% rownames(correlations))) {
      reason <- "Indicator correlations are unavailable"
    } else {
      heterotrait <- abs(correlations[first_indicators, second_indicators, drop = FALSE])
      if (is.null(within_means[[first]])) {
        within <- correlations[first_indicators, first_indicators, drop = FALSE]
        within_means[[first]] <- mean(abs(within[lower.tri(within)]), na.rm = TRUE)
      }
      if (is.null(within_means[[second]])) {
        within <- correlations[second_indicators, second_indicators, drop = FALSE]
        within_means[[second]] <- mean(abs(within[lower.tri(within)]), na.rm = TRUE)
      }
      denominator <- sqrt(within_means[[first]] * within_means[[second]])
      if (is.finite(denominator) && denominator > 0) value <- mean(heterotrait, na.rm = TRUE) / denominator else reason <- "Within-factor correlations are insufficient"
    }
    matrix_result[first, second] <- matrix_result[second, first] <- value
    # Resampling consumes the matrix only; retain the public table by default.
    if (!isTRUE(include_pairs)) next
    pair_index <- pair_index + 1L
    pairs[[pair_index]] <- data.frame(
      Factor1 = first, Factor2 = second, HTMT = value,
      Criterion = if (is.finite(value)) if (value < threshold) "Below reference" else "Review needed" else "Not assessed",
      Reason = reason,
      stringsAsFactors = FALSE
    )
  }
  list(matrix = matrix_result, pairs = if (isTRUE(include_pairs)) do.call(rbind, pairs) else data.frame(), threshold = threshold)
}
