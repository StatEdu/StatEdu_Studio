structural_canvas_execute_analysis <- function(snapshot, settings = NULL, input, session, dataset_fn, variable_table_fn, analysis_type, prefix, fit_result) {
  is_mi_refit <- !is.null(settings) && !is.null(settings$fit)
  settings <- settings %||% list()
  identification <- structural_canvas_identification_diagnostics(snapshot)
  identification_errors <- identification[identification$Severity == "Error", , drop = FALSE]
  if (nrow(identification_errors)) {
    stop(paste0("Model identification check failed: ", paste(paste0(identification_errors$Element, " — ", identification_errors$Message), collapse = "; ")))
  }
  identification_warnings <- identification[identification$Severity == "Warning", , drop = FALSE]
  if (nrow(identification_warnings)) {
    showNotification(paste0("Identification warning: ", paste(paste0(identification_warnings$Element, " — ", identification_warnings$Message), collapse = "; ")), type = "warning", duration = 12)
  }
  estimator <- settings$estimator %||% input[[paste0(prefix, "_estimator")]] %||% "ML"
  missing <- settings$missing %||% input[[paste0(prefix, "_missing")]] %||% "fiml"
  std_lv <- settings$std_lv %||% identical(input[[paste0(prefix, "_scale")]], "variance")
  mi_mode <- settings$mi_mode %||% input[[paste0(prefix, "_mi_mode")]] %||% "theory"
  rmsea_ci <- settings$rmsea_ci %||% as.numeric(input[[paste0(prefix, "_rmsea_ci")]] %||% .90)
  validity_formula <- settings$validity_formula %||% input[[paste0(prefix, "_validity_formula")]] %||% "standardized"
  reliability_bootstrap <- suppressWarnings(as.integer(settings$reliability_bootstrap %||% input[[paste0(prefix, "_reliability_bootstrap")]] %||% 0L))
  if (is.na(reliability_bootstrap) || !reliability_bootstrap %in% c(0L, 500L, 1000L, 2000L)) reliability_bootstrap <- 0L
  reliability_seed <- suppressWarnings(as.integer(settings$reliability_seed %||% input[[paste0(prefix, "_reliability_seed")]] %||% 24680L))
  if (is.na(reliability_seed) || reliability_seed < 1L) reliability_seed <- 24680L
  reliability_ci_method <- structural_canvas_bootstrap_ci_method(settings$reliability_ci_method %||% input[[paste0(prefix, "_reliability_ci_method")]] %||% "percentile")
  bollen_stine_bootstrap <- suppressWarnings(as.integer(settings$bollen_stine_bootstrap %||% input[[paste0(prefix, "_bollen_stine_bootstrap")]] %||% 0L))
  if (is.na(bollen_stine_bootstrap) || !bollen_stine_bootstrap %in% c(0L, 500L, 1000L, 2000L)) bollen_stine_bootstrap <- 0L
  bollen_stine_seed <- suppressWarnings(as.integer(settings$bollen_stine_seed %||% input[[paste0(prefix, "_bollen_stine_seed")]] %||% 97531L))
  if (is.na(bollen_stine_seed) || bollen_stine_seed < 1L) bollen_stine_seed <- 97531L
  htmt_threshold <- as.numeric(settings$htmt_threshold %||% input[[paste0(prefix, "_htmt_threshold")]] %||% .85)
  if (!is.finite(htmt_threshold) || !htmt_threshold %in% c(.85, .90)) htmt_threshold <- .85
  htmt_bootstrap <- suppressWarnings(as.integer(settings$htmt_bootstrap %||% input[[paste0(prefix, "_htmt_bootstrap")]] %||% 0L))
  if (is.na(htmt_bootstrap) || !htmt_bootstrap %in% c(0L, 500L, 1000L, 2000L)) htmt_bootstrap <- 0L
  htmt_seed <- suppressWarnings(as.integer(settings$htmt_seed %||% input[[paste0(prefix, "_htmt_seed")]] %||% 12345L))
  if (is.na(htmt_seed) || htmt_seed < 1L) htmt_seed <- 12345L
  htmt_ci_method <- structural_canvas_bootstrap_ci_method(settings$htmt_ci_method %||% input[[paste0(prefix, "_htmt_ci_method")]] %||% "percentile")
  invariance_enabled <- isTRUE(settings$invariance_enabled %||% input[[paste0(prefix, "_invariance_enabled")]] %||% FALSE)
  invariance_group <- as.character(settings$invariance_group %||% input[[paste0(prefix, "_invariance_group")]] %||% "")
  mi_holdout_enabled <- isTRUE(settings$mi_holdout_enabled %||% input[[paste0(prefix, "_mi_holdout_enabled")]] %||% FALSE)
  mi_holdout_fraction <- as.numeric(settings$mi_holdout_fraction %||% input[[paste0(prefix, "_mi_holdout_fraction")]] %||% .30)
  mi_holdout_seed <- suppressWarnings(as.integer(settings$mi_holdout_seed %||% input[[paste0(prefix, "_mi_holdout_seed")]] %||% 13579L))
  if (is.na(mi_holdout_seed) || mi_holdout_seed < 1L) mi_holdout_seed <- 13579L
  result_coefficient <- settings$result_coefficient %||% input[[paste0(prefix, "_result_coefficient")]] %||% "beta"
  residual_variance_fixes <- settings$residual_variance_fixes %||% numeric(0)
  full_data <- dataset_fn()
  variable_table <- variable_table_fn()
  nominal <- structural_canvas_nominal_indicators(snapshot, variable_table)
  if (length(nominal)) {
    stop(sprintf(
      "Nominal indicators are not supported by standard CFA/SEM: %s. Reclassify them as ordered only when their categories have a meaningful order.",
      paste(nominal, collapse = ", ")
    ))
  }
  ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
  if (length(ordered) > 0 || identical(toupper(estimator), "WLSMV")) {
    if (toupper(estimator) %in% c("ML", "MLR")) estimator <- "WLSMV"
    if (identical(missing, "fiml")) missing <- "pairwise"
  }
  structural_canvas_validate_holdout_options(
    mi_holdout_enabled, analysis_type, estimator, ordered,
    invariance_enabled, residual_variance_fixes
  )
  if (mi_holdout_enabled) {
    if (!is.null(settings$analysis_data) && !is.null(settings$validation_data)) {
      data <- settings$analysis_data
      validation_data <- settings$validation_data
      holdout_rows <- settings$holdout_rows %||% list()
    } else {
      split <- structural_canvas_holdout_split(full_data, mi_holdout_fraction, mi_holdout_seed)
      data <- split$exploration
      validation_data <- split$validation
      holdout_rows <- list(exploration = split$exploration_rows, validation = split$validation_rows)
    }
  } else {
    data <- full_data
    validation_data <- NULL
    holdout_rows <- list()
  }
  missing_covariances <- structural_canvas_missing_exogenous_covariances(snapshot)
  if (analysis_type %in% c("cfa", "cbsem") && length(missing_covariances)) {
    showNotification(paste0("Missing covariance paths between exogenous latent variables: ", paste(missing_covariances, collapse = ", "), ". These covariances will be fixed to zero."), type = "warning", duration = 10)
  }
  result <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, nominal = nominal, residual_variance_fixes = residual_variance_fixes)
  if (!isTRUE(result$admissible)) {
    details <- c(
      if (!isTRUE(result$converged)) "the model did not converge",
      if (!isTRUE(result$post_check)) "lavaan post-estimation checks failed",
      if (!isTRUE(result$identified)) paste0("model degrees of freedom are invalid (df = ", format_decimal3(result$df), ")"),
      if (length(result$negative_residuals)) paste0("negative residual variances: ", paste(result$negative_residuals, collapse = ", ")),
      if (length(result$negative_latent_variances)) paste0("negative latent variances: ", paste(result$negative_latent_variances, collapse = ", ")),
      if (isTRUE(result$non_psd_theta)) paste0("residual covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_latent_covariance)) paste0("latent covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_theta)) paste0("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_latent_covariance)) paste0("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_parameter_covariance)) paste0("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$near_singular_parameter_covariance)) paste0("parameter-estimate covariance matrix is near singular (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$invalid_correlations)) "one or more absolute latent correlations are at least 1"
    )
    showNotification(paste0("Potentially inadmissible solution: ", paste(details, collapse = "; "), ". Interpret fit, AVE, CR, and validity results with caution."), type = "error", duration = NULL)
  }
  conditioning_details <- c(
    if (isTRUE(result$ill_conditioned_theta)) paste0("residual covariance condition number = ", format(result$theta_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_latent_covariance)) paste0("latent covariance condition number = ", format(result$latent_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_parameter_covariance)) paste0("parameter-estimate covariance condition number = ", format(result$parameter_condition_number, scientific = TRUE, digits = 3))
  )
  if (length(conditioning_details)) {
    showNotification(paste0("Numerically ill-conditioned solution: ", paste(conditioning_details, collapse = "; "), ". Small data or specification changes may produce unstable estimates."), type = "warning", duration = 12)
  }
  if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
    if (invariance_enabled) stop("Bollen-Stine bootstrap cannot be combined with measurement-invariance analysis; assess global fit within the appropriate group model instead of the pooled CFA.")
    eligibility <- structural_canvas_bollen_stine_eligibility(result$fit)
    if (!isTRUE(eligibility$available)) stop(eligibility$reason)
  }
  invariance_result <- NULL
  if (identical(analysis_type, "cfa") && invariance_enabled) {
    if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
    if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
    if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for measurement invariance analysis.")
    if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop("The grouping variable cannot also be an indicator in the CFA model.")
    group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
    if (group_count < 2L || group_count > 20L) stop("The grouping variable must contain between 2 and 20 non-empty groups.")
    invariance_result <- shiny::withProgress(message = "Estimating measurement-invariance models", value = 0, {
      shiny::incProgress(.15, detail = "Configural, metric, scalar, and strict models")
      value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
      shiny::incProgress(.85, detail = "Preparing robust comparisons")
      value
    })
  }
  reliability_bootstrap_result <- structural_canvas_run_reliability_bootstrap(
    analysis_type, reliability_bootstrap, result, data, reliability_seed,
    estimator, missing, std_lv, ordered, validity_formula, reliability_ci_method
  )
  bollen_stine_result <- structural_canvas_run_bollen_stine_bootstrap(
    analysis_type, bollen_stine_bootstrap, result, bollen_stine_seed
  )
  mi <- if (analysis_type %in% c("cfa", "cbsem")) structural_canvas_mi_refits(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = mi_mode, ordered = ordered) else NULL
  htmt_bootstrap_result <- structural_canvas_run_htmt_bootstrap(
    analysis_type, htmt_bootstrap, result, data, htmt_seed, ordered,
    htmt_threshold, htmt_ci_method
  )
  baseline_fit <- if (is_mi_refit) settings$baseline_fit %||% settings$fit else result$fit
  baseline_diagnostics <- if (is_mi_refit) settings$baseline_diagnostics %||% settings$diagnostics else result
  baseline_syntax <- if (is_mi_refit) settings$baseline_syntax %||% settings$syntax else result$syntax
  holdout_comparison <- NULL
  if (mi_holdout_enabled && identical(settings$comparison_type %||% "", "mi") && !is.null(validation_data)) {
    holdout_comparison <- structural_canvas_holdout_model_comparison(
      baseline_syntax, result$syntax, validation_data,
      estimator = estimator, missing = missing, std_lv = std_lv, ci_level = rmsea_ci
    )
  }
  fit_result(list(
    fit = result$fit, syntax = result$syntax, snapshot = snapshot, mi = mi, mi_mode = mi_mode,
    rmsea_ci = rmsea_ci, validity_formula = validity_formula,
    reliability_bootstrap = reliability_bootstrap, reliability_seed = reliability_seed,
    reliability_ci_method = reliability_ci_method,
    reliability_bootstrap_result = reliability_bootstrap_result,
    bollen_stine_bootstrap = bollen_stine_bootstrap, bollen_stine_seed = bollen_stine_seed,
    bollen_stine_result = bollen_stine_result,
    htmt_threshold = htmt_threshold, htmt_bootstrap = htmt_bootstrap, htmt_seed = htmt_seed,
    htmt_ci_method = htmt_ci_method,
    htmt_bootstrap_result = htmt_bootstrap_result,
    invariance_enabled = invariance_enabled, invariance_group = invariance_group, invariance_result = invariance_result,
    mi_holdout_enabled = mi_holdout_enabled, mi_holdout_fraction = mi_holdout_fraction, mi_holdout_seed = mi_holdout_seed,
    analysis_data = data, validation_data = validation_data, holdout_rows = holdout_rows, holdout_comparison = holdout_comparison,
    estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered,
    result_coefficient = result_coefficient, diagnostics = result,
    baseline_fit = baseline_fit, modified_from_baseline = is_mi_refit || isTRUE(settings$modified_from_baseline),
    baseline_syntax = baseline_syntax,
    baseline_diagnostics = baseline_diagnostics,
    comparison_label = settings$comparison_label %||% NULL,
    comparison_type = settings$comparison_type %||% NULL,
    residual_variance_fixes = residual_variance_fixes,
    identification = identification,
    mi_history = settings$mi_history %||% data.frame()
  ))
  session$sendCustomMessage(
    "custom-model-canvas-result",
    list(
      rootId = paste0(prefix, "-canvas-root"),
      source = snapshot,
      result = structural_canvas_result_snapshot(snapshot, result$fit, result_coefficient),
      show = TRUE
    )
  )
  result
}
