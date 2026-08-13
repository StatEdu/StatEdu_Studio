# CFA/SEM canvas numeric workbook export tables.

structural_canvas_export_parameter_estimates <- function(fit) {
  estimates <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
  columns <- intersect(
    c("lhs", "op", "rhs", "label", "est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.lv", "std.all", "std.nox"),
    names(estimates)
  )
  table <- estimates[, columns, drop = FALSE]
  table$Fixed <- with(table, is.finite(se) & se == 0 & is.na(z) & is.na(pvalue))
  names(table)[names(table) == "pvalue"] <- "p"
  rownames(table) <- NULL
  table
}

structural_canvas_export_fit_estimates <- function(bundle) {
  ci_level <- as.numeric(bundle$rmsea_ci %||% .90)
  fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) {
    list(bundle$baseline_fit, bundle$fit)
  } else list(bundle$fit)
  selections <- structural_canvas_common_fit_measures(fits, bundle$estimator %||% "ML", ci_level)
  values <- do.call(rbind, lapply(selections, function(item) item$values))
  models <- if (length(selections) > 1L) {
    c("Original model", as.character(bundle$comparison_label %||% "Modified model"))
  } else "Fitted model"
  data.frame(
    Model = models,
    `Chi-square` = values[, 1L], df = values[, 2L], p = values[, 3L], Q = values[, 4L],
    CFI = values[, 5L], TLI = values[, 6L], SRMR = values[, 7L], RMSEA = values[, 8L],
    `RMSEA CI lower` = values[, 9L], `RMSEA CI upper` = values[, 10L],
    `RMSEA CI level` = rep(ci_level, length(selections)),
    `Chi-square source` = vapply(selections, function(item) item$keys[["chisq"]], character(1)),
    `CFI source` = vapply(selections, function(item) item$keys[["cfi"]], character(1)),
    `TLI source` = vapply(selections, function(item) item$keys[["tli"]], character(1)),
    `RMSEA source` = vapply(selections, function(item) item$keys[["rmsea"]], character(1)),
    check.names = FALSE
  )
}

structural_canvas_rmsea_hypothesis_tests <- function(bundle) {
  ci_level <- as.numeric(bundle$rmsea_ci %||% .90)
  fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
  selections <- structural_canvas_common_fit_measures(fits, bundle$estimator %||% "ML", ci_level)
  models <- if (length(fits) > 1L) c("Original model", as.character(bundle$comparison_label %||% "Modified model")) else "Fitted model"
  rows <- lapply(seq_along(fits), function(index) {
    measures <- selections[[index]]$measures
    rmsea_key <- selections[[index]]$keys[["rmsea"]]
    suffix <- sub("^rmsea", "", rmsea_key)
    get_value <- function(keys) {
      for (key in keys) if (key %in% names(measures) && is.finite(measures[[key]])) return(unname(measures[[key]]))
      NA_real_
    }
    data.frame(
      Model = models[[index]],
      RMSEA = selections[[index]]$values[[8L]],
      `Close-fit H0` = get_value("rmsea.close.h0"),
      `Close-fit p` = get_value(c(paste0("rmsea.pvalue", suffix), "rmsea.pvalue.scaled", "rmsea.pvalue")),
      `Not-close H0` = get_value("rmsea.notclose.h0"),
      `Not-close p` = get_value(c(paste0("rmsea.notclose.pvalue", suffix), "rmsea.notclose.pvalue.scaled", "rmsea.notclose.pvalue")),
      Source = rmsea_key,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

structural_canvas_information_criteria <- function(bundle) {
  fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
  models <- if (length(fits) > 1L) c("Original model", as.character(bundle$comparison_label %||% "Modified model")) else "Fitted model"
  value <- function(measures, key) if (key %in% names(measures) && is.finite(measures[[key]])) unname(measures[[key]]) else NA_real_
  metadata <- lapply(fits, function(fit) {
    estimator <- toupper(as.character(lavaan::lavInspect(fit, "options")$estimator %||% ""))
    analyzed_data <- as.matrix(lavaan::lavInspect(fit, "data"))
    if (!is.null(colnames(analyzed_data))) analyzed_data <- analyzed_data[, sort(colnames(analyzed_data)), drop = FALSE]
    list(
      n = as.numeric(lavaan::lavInspect(fit, "ntotal")),
      observed = sort(lavaan::lavNames(fit, "ov")),
      data = analyzed_data,
      estimator = estimator,
      family = if (estimator %in% c("ML", "MLR")) "ML" else estimator,
      admissibility = structural_canvas_fit_admissibility(fit)
    )
  })
  comparison_valid <- length(fits) > 1L &&
    length(unique(vapply(metadata, function(item) item$n, numeric(1)))) == 1L &&
    all(vapply(metadata[-1L], function(item) identical(item$observed, metadata[[1L]]$observed), logical(1))) &&
    all(vapply(metadata[-1L], function(item) isTRUE(all.equal(item$data, metadata[[1L]]$data, check.attributes = FALSE)), logical(1))) &&
    length(unique(vapply(metadata, function(item) item$family, character(1)))) == 1L &&
    all(vapply(metadata, function(item) isTRUE(item$admissibility$admissible), logical(1)))
  inadmissible <- length(fits) > 1L && any(!vapply(metadata, function(item) isTRUE(item$admissibility$admissible), logical(1)))
  comparison_status <- if (length(fits) == 1L) "Single model; delta not applicable" else if (comparison_valid) "Comparable on observations, variables, estimator family, and admissibility" else if (inadmissible) "Not comparable; inadmissible model; delta suppressed" else "Not comparable; delta suppressed"
  rows <- lapply(seq_along(fits), function(index) {
    measures <- suppressWarnings(lavaan::fitMeasures(fits[[index]]))
    data.frame(
      Model = models[[index]], N = metadata[[index]]$n,
      `Observed variables` = length(metadata[[index]]$observed), Estimator = metadata[[index]]$estimator,
      Admissible = isTRUE(metadata[[index]]$admissibility$admissible),
      `Admissibility reasons` = if (length(metadata[[index]]$admissibility$reasons)) paste(metadata[[index]]$admissibility$reasons, collapse = "; ") else "None",
      LogLik = value(measures, "logl"),
      `Free parameters` = value(measures, "npar"), AIC = value(measures, "aic"),
      BIC = value(measures, "bic"), `Adjusted BIC` = value(measures, "bic2"),
      `Comparison status` = comparison_status,
      check.names = FALSE
    )
  })
  table <- do.call(rbind, rows)
  for (metric in c("AIC", "BIC", "Adjusted BIC")) {
    finite <- table[[metric]][is.finite(table[[metric]])]
    table[[paste0("Delta ", metric)]] <- if (comparison_valid && length(finite) == length(fits)) table[[metric]] - min(finite) else NA_real_
  }
  table
}

structural_canvas_export_admissibility <- function(bundle) {
  fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(`Original model` = bundle$baseline_fit, `Modified model` = bundle$fit) else list(`Fitted model` = bundle$fit)
  rows <- lapply(names(fits), function(model) {
    diagnostics <- structural_canvas_fit_admissibility(fits[[model]])
    data.frame(
      Model = model,
      Admissible = isTRUE(diagnostics$admissible),
      Reasons = if (length(diagnostics$reasons)) paste(diagnostics$reasons, collapse = "; ") else "None",
      `Residual min eigenvalue` = diagnostics$residual_min_eigenvalue,
      `Latent min eigenvalue` = diagnostics$latent_min_eigenvalue,
      `Parameter min eigenvalue` = diagnostics$parameter_min_eigenvalue,
      `Residual condition number` = diagnostics$residual_condition_number,
      `Latent condition number` = diagnostics$latent_condition_number,
      `Parameter condition number` = diagnostics$parameter_condition_number,
      `Parameter boundary dimensions` = diagnostics$parameter_boundary_dimensions,
      `Explicit equality constraints` = diagnostics$equality_constraint_count,
      `Ill-conditioned warning` = any(c(diagnostics$residual_condition_number, diagnostics$latent_condition_number, diagnostics$parameter_condition_number) > 1e8),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

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
