# CFA/SEM canvas fit and parameter export tables.

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
