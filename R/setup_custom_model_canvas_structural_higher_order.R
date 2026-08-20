# Structural higher-order factor diagnostics.

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
  else "At/above descriptive .40 reference"
}

structural_canvas_omega_h_guidance <- function(value) {
  if (!is.finite(value) || value < 0 || value > 1) "Review inadmissible coefficient"
  else if (value < .70) "Below common .70 guideline"
  else "At/above descriptive .70 reference"
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
    else if (value >= .90) "At/above descriptive .90 reference"
    else if (value >= .80) "Between descriptive .80 and .90 references"
    else "Below descriptive .80; review score use"
  }, character(1))
  data.frame(Factor = factor_names, Determinacy = determinacy, `Score reliability` = reliability, Guidance = guidance, check.names = FALSE)
}
