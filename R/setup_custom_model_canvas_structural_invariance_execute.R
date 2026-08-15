# Structural equation canvas measurement-invariance execution helpers.

structural_canvas_measurement_only_syntax <- function(syntax) {
  lines <- trimws(strsplit(as.character(syntax %||% ""), "\n", fixed = TRUE)[[1L]])
  measurement <- lines[grepl("=~", lines, fixed = TRUE)]
  if (!length(measurement)) stop("A measurement model is required before structural path group comparison.")
  factors <- unique(trimws(vapply(strsplit(measurement, "=~", fixed = TRUE), `[[`, character(1), 1L)))
  covariance <- if (length(factors) > 1L) vapply(utils::combn(factors, 2L, simplify = FALSE), function(pair) paste(pair, collapse = " ~~ "), character(1)) else character(0)
  paste(c(measurement, covariance), collapse = "\n")
}

structural_canvas_metric_invariance_gate <- function(invariance) {
  table <- invariance$table %||% data.frame()
  row <- table[table$Model == "Metric", , drop = FALSE]
  if (!nrow(row)) return(list(passed = FALSE, reason = "Metric invariance was not estimated."))
  passed <- isTRUE(row$Converged[[1L]]) && isTRUE(row$Admissible[[1L]]) &&
    is.finite(row$DeltaCFI[[1L]]) && row$DeltaCFI[[1L]] >= -.010 &&
    is.finite(row$DeltaRMSEA[[1L]]) && row$DeltaRMSEA[[1L]] <= .015 &&
    is.finite(row$DeltaSRMR[[1L]]) && row$DeltaSRMR[[1L]] <= .030
  list(passed = passed, reason = if (passed) "Metric invariance gate passed." else paste0(
    "Metric invariance gate failed (DeltaCFI=", format_decimal3(row$DeltaCFI[[1L]]),
    ", DeltaRMSEA=", format_decimal3(row$DeltaRMSEA[[1L]]),
    ", DeltaSRMR=", format_decimal3(row$DeltaSRMR[[1L]]),
    ", converged=", row$Converged[[1L]], ", admissible=", row$Admissible[[1L]], ")."
  ))
}

structural_canvas_run_measurement_invariance <- function(analysis_type, invariance_enabled, result, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered, snapshot = NULL, micom_permutations = 500L, micom_seed = 20260816L) {
  invariance_result <- NULL
  if (identical(analysis_type, "cfa") && invariance_enabled) {
    if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
    if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
    if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for measurement invariance analysis.")
    if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop("The grouping variable cannot also be an indicator in the CFA model.")
    group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
    if (group_count < 2L || group_count > 20L) stop("The grouping variable must contain between 2 and 20 non-empty groups.")
    invariance_result <- structural_canvas_with_progress(message = "Estimating measurement-invariance models", value = 0, {
      structural_canvas_inc_progress(.15, detail = "Configural, metric, scalar, and strict models")
      value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
      structural_canvas_inc_progress(.85, detail = "Preparing robust comparisons")
      value
    })
  }
  if (analysis_type %in% c("cbsem", "sem") && invariance_enabled) {
    if (length(ordered) || !toupper(estimator) %in% c("ML", "MLR")) stop("Structural path group comparison requires continuous ML or MLR SEM/CB-SEM.")
    if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for structural path group comparison.")
    if (length(result$moderation_definitions %||% list())) stop("Structural path group comparison with latent product-indicator interactions requires a dedicated invariance workflow and is not supported by the current gate.")
    invariance_result <- structural_canvas_with_progress(message = "Testing measurement invariance before structural path comparison", value = 0, {
      structural_canvas_inc_progress(.10, detail = "Configural and metric measurement models")
      measurement <- structural_canvas_measurement_invariance(structural_canvas_measurement_only_syntax(result$syntax), data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
      gate <- structural_canvas_metric_invariance_gate(measurement)
      if (!isTRUE(gate$passed)) stop(paste0(gate$reason, " Structural path equality tests were not run."))
      structural_canvas_inc_progress(.45, detail = "Metric gate passed; fitting free and equal structural paths")
      value <- structural_canvas_structural_path_group_comparison(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
      value$measurement_invariance <- measurement
      value$measurement_gate <- gate
      structural_canvas_inc_progress(.45, detail = "Preparing group-specific path comparisons")
      value
    })
  }
  if (identical(analysis_type, "plssem") && invariance_enabled) {
    if (!toupper(estimator) %in% c("PLS", "PLSC")) stop("MICOM requires the PLS or PLSc estimator.")
    invariance_result <- structural_canvas_with_progress(message = "Running MICOM permutation analysis", value = 0, {
      structural_canvas_inc_progress(.10, detail = "Checking configural invariance and group sizes")
      value <- structural_canvas_micom(snapshot %||% list(), data, invariance_group, estimator, micom_permutations, micom_seed)
      structural_canvas_inc_progress(.90, detail = "Evaluating compositional, mean, and variance invariance")
      value
    })
  }
  invariance_result
}
