# CFA/SEM canvas reliability, validity, and sample export tables.

structural_canvas_export_latent_correlations <- function(fit) {
  covariance <- as.matrix(lavaan::lavInspect(fit, "cov.lv"))
  latent_names <- lavaan::lavNames(fit, "lv")
  if (!length(dim(covariance))) covariance <- matrix(covariance, nrow = 1L, ncol = 1L)
  correlations <- if (all(is.finite(diag(covariance))) && all(diag(covariance) > 0)) {
    stats::cov2cor(covariance)
  } else {
    matrix(NA_real_, nrow = nrow(covariance), ncol = ncol(covariance), dimnames = dimnames(covariance))
  }
  if (is.null(rownames(correlations))) rownames(correlations) <- latent_names
  if (is.null(colnames(correlations))) colnames(correlations) <- latent_names
  data.frame(Factor = rownames(correlations), correlations, check.names = FALSE, row.names = NULL)
}

structural_canvas_export_reliability_validity <- function(bundle) {
  fit <- bundle$fit
  snapshot <- bundle$snapshot %||% list()
  estimates <- structural_canvas_reliability_estimates(fit, bundle$validity_formula %||% "standardized")
  if (!nrow(estimates)) return(data.frame())
  standardized <- lavaan::standardizedSolution(fit)
  observed <- lavaan::lavNames(fit, "ov")
  loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs"), drop = FALSE]
  counts <- stats::setNames(vapply(estimates$Factor, function(name) sum(loadings$lhs == name), integer(1)), estimates$Factor)
  cross_loaded_indicators <- unique(loadings$rhs[duplicated(loadings$rhs) | duplicated(loadings$rhs, fromLast = TRUE)])
  cross_loaded <- stats::setNames(vapply(estimates$Factor, function(name) {
    any(loadings$rhs[loadings$lhs == name] %in% cross_loaded_indicators)
  }, logical(1)), estimates$Factor)
  constrained <- estimates$Factor %in% structural_canvas_constrained_single_indicators(snapshot)
  single <- counts[estimates$Factor] < 2L
  unconstrained_single <- single & !constrained
  estimates$AVE[unconstrained_single] <- NA_real_
  estimates$CR[unconstrained_single] <- NA_real_
  estimates$Alpha[single] <- NA_real_
  estimates$Omega[unconstrained_single] <- NA_real_
  latent_covariance <- as.matrix(lavaan::lavInspect(fit, "cov.lv"))
  correlations <- if (all(is.finite(diag(latent_covariance))) && all(diag(latent_covariance) > 0)) {
    stats::cov2cor(latent_covariance)
  } else {
    matrix(NA_real_, nrow = nrow(latent_covariance), ncol = ncol(latent_covariance), dimnames = dimnames(latent_covariance))
  }
  factor_correlations <- correlations[estimates$Factor, estimates$Factor, drop = FALSE]
  missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
  fl <- structural_canvas_fornell_larcker(
    stats::setNames(estimates$AVE, estimates$Factor), factor_correlations,
    counts, assessable = !length(missing_covariances)
  )
  sqrt_ave <- ifelse(is.finite(estimates$AVE) & estimates$AVE >= 0, sqrt(estimates$AVE), NA_real_)
  assessed <- !single & !length(missing_covariances) & is.finite(sqrt_ave) & is.finite(fl$max_correlation[estimates$Factor])
  data.frame(
    Factor = estimates$Factor,
    k = unname(counts[estimates$Factor]),
    AVE = estimates$AVE,
    `sqrt(AVE)` = sqrt_ave,
    CR = estimates$CR,
    `Cronbach's alpha` = estimates$Alpha,
    `Omega total` = estimates$Omega,
    `Max absolute latent correlation` = unname(fl$max_correlation[estimates$Factor]),
    `Fornell-Larcker criterion` = unname(fl$criterion[estimates$Factor]),
    `Fornell-Larcker assessed` = assessed,
    `Single indicator` = unname(single),
    `Externally constrained single indicator` = unname(constrained),
    `Contains cross-loaded indicator` = unname(cross_loaded[estimates$Factor]),
    check.names = FALSE
  )
}

structural_canvas_export_sample_statistics <- function(fit) {
  sample_statistics <- lavaan::lavInspect(fit, "sampstat")
  groups <- if (!is.null(sample_statistics$cov) || !is.null(sample_statistics$th)) list(sample_statistics) else sample_statistics
  group_labels <- as.character(lavaan::lavInspect(fit, "group.label"))
  if (!length(group_labels)) group_labels <- if (length(groups) == 1L) "Overall" else paste("Group", seq_along(groups))
  if (length(group_labels) != length(groups)) group_labels <- paste("Group", seq_along(groups))
  group_n <- as.numeric(lavaan::lavInspect(fit, "nobs"))
  if (length(group_n) != length(groups)) group_n <- rep(NA_real_, length(groups))
  descriptives <- covariance_rows <- threshold_rows <- vector("list", length(groups))
  for (index in seq_along(groups)) {
    statistics <- groups[[index]]
    covariance <- as.matrix(statistics$cov %||% matrix(numeric(0), 0L, 0L))
    variables <- rownames(covariance) %||% character(0)
    means <- as.numeric(statistics$mean %||% rep(NA_real_, length(variables)))
    if (length(means) != length(variables)) means <- rep(NA_real_, length(variables))
    variances <- if (length(variables)) diag(covariance) else numeric(0)
    descriptives[[index]] <- data.frame(
      Group = rep(group_labels[[index]], length(variables)), Variable = variables, Mean = means,
      Variance = variances, SD = ifelse(is.finite(variances) & variances >= 0, sqrt(variances), NA_real_),
      `Model N` = rep(group_n[[index]], length(variables)), check.names = FALSE
    )
    if (length(variables)) {
      correlation <- if (all(is.finite(variances)) && all(variances > 0)) stats::cov2cor(covariance) else matrix(NA_real_, nrow(covariance), ncol(covariance))
      grid <- expand.grid(Row = variables, Column = variables, stringsAsFactors = FALSE)
      grid$Group <- group_labels[[index]]
      grid$Covariance <- as.numeric(covariance[cbind(match(grid$Row, variables), match(grid$Column, variables))])
      grid$Correlation <- as.numeric(correlation[cbind(match(grid$Row, variables), match(grid$Column, variables))])
      covariance_rows[[index]] <- grid[, c("Group", "Row", "Column", "Covariance", "Correlation")]
    }
    thresholds <- statistics$th %||% numeric(0)
    if (length(thresholds)) {
      threshold_names <- names(thresholds)
      if (is.null(threshold_names)) threshold_names <- paste0("Threshold_", seq_along(thresholds))
      threshold_rows[[index]] <- data.frame(
      Group = group_labels[[index]], Parameter = threshold_names, Threshold = as.numeric(thresholds),
      check.names = FALSE
      )
    }
  }
  combine <- function(values) {
    values <- Filter(function(value) !is.null(value) && nrow(value), values)
    if (length(values)) do.call(rbind, values) else data.frame()
  }
  list(
    Descriptives = combine(descriptives),
    Covariance = combine(covariance_rows),
    Thresholds = combine(threshold_rows)
  )
}
