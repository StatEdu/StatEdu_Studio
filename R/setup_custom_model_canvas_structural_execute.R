structural_canvas_execute_analysis <- function(snapshot, settings = NULL, input, session, dataset_fn, variable_table_fn, analysis_type, prefix, fit_result, app_language_fn = NULL) {
  is_mi_refit <- !is.null(settings) && !is.null(settings$fit)
  settings <- settings %||% list()
  identification <- if (analysis_type %in% c("cfa", "cbsem", "sem")) structural_canvas_identification_diagnostics(snapshot) else data.frame()
  identification_errors <- identification[identification$Severity == "Error", , drop = FALSE]
  if (nrow(identification_errors)) {
    stop(structural_canvas_identification_issue_text(identification_errors, statedu_current_language(app_language_fn)))
  }
  identification_warnings <- identification[identification$Severity == "Warning", , drop = FALSE]
  if (nrow(identification_warnings)) structural_canvas_notify_identification_warnings(identification_warnings, statedu_current_language(app_language_fn))
  options <- structural_canvas_execute_settings(settings, input, prefix)
  estimator <- options$estimator
  objective <- options$objective
  missing <- options$missing
  std_lv <- options$std_lv
  mi_mode <- options$mi_mode
  rmsea_ci <- options$rmsea_ci
  validity_formula <- options$validity_formula
  reliability_bootstrap <- options$reliability_bootstrap
  reliability_seed <- options$reliability_seed
  reliability_ci_method <- options$reliability_ci_method
  bollen_stine_bootstrap <- options$bollen_stine_bootstrap
  bollen_stine_seed <- options$bollen_stine_seed
  htmt_threshold <- options$htmt_threshold
  htmt_bootstrap <- options$htmt_bootstrap
  htmt_seed <- options$htmt_seed
  htmt_ci_method <- options$htmt_ci_method
  pls_bootstrap <- options$pls_bootstrap
  pls_seed <- options$pls_seed
  pls_predict_folds <- options$pls_predict_folds
  pls_predict_reps <- options$pls_predict_reps
  redundancy_construct <- options$redundancy_construct
  redundancy_criterion <- options$redundancy_criterion
  parcel_enabled <- options$parcel_enabled
  parcel_construct <- options$parcel_construct
  parcel_count <- options$parcel_count
  parcel_purpose <- options$parcel_purpose
  invariance_enabled <- options$invariance_enabled
  invariance_group <- options$invariance_group
  mi_holdout_enabled <- options$mi_holdout_enabled
  mi_holdout_fraction <- options$mi_holdout_fraction
  mi_holdout_seed <- options$mi_holdout_seed
  common_method_enabled <- options$common_method_enabled
  common_method_methods <- options$common_method_methods
  result_coefficient <- options$result_coefficient
  residual_variance_fixes <- options$residual_variance_fixes
  full_data <- dataset_fn()
  variable_table <- variable_table_fn()
  method_recommendation <- structural_canvas_method_recommendation(snapshot, variable_table, objective)
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
  structural_canvas_notify_missing_covariances(missing_covariances, analysis_type, statedu_current_language(app_language_fn))
  result <- run_structural_canvas_analysis(snapshot, data, analysis_type, estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, nominal = nominal, residual_variance_fixes = residual_variance_fixes)
  structural_canvas_notify_ignored_pls_covariances(result, analysis_type, statedu_current_language(app_language_fn))
  structural_canvas_notify_solution_diagnostics(result, statedu_current_language(app_language_fn))
  if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
    if (invariance_enabled) stop("Bollen-Stine bootstrap cannot be combined with measurement-invariance analysis; assess global fit within the appropriate group model instead of the pooled CFA.")
    eligibility <- structural_canvas_bollen_stine_eligibility(result$fit)
    if (!isTRUE(eligibility$available)) stop(eligibility$reason)
  }
  invariance_result <- structural_canvas_run_measurement_invariance(
    analysis_type, invariance_enabled, result, data, invariance_group,
    estimator, missing, std_lv, rmsea_ci, ordered
  )
  reliability_bootstrap_result <- structural_canvas_run_reliability_bootstrap(
    analysis_type, reliability_bootstrap, result, data, reliability_seed,
    estimator, missing, std_lv, ordered, validity_formula, reliability_ci_method
  )
  bollen_stine_result <- structural_canvas_run_bollen_stine_bootstrap(
    analysis_type, bollen_stine_bootstrap, result, bollen_stine_seed
  )
  mi <- if (analysis_type %in% c("cfa", "cbsem", "sem")) {
    tryCatch(
      structural_canvas_mi_refits(snapshot, result, data, analysis_type, estimator, missing, std_lv, mode = mi_mode, ordered = ordered),
      error = function(error) {
        structural_canvas_show_notification(
          structural_canvas_error_message(error, statedu_current_language(app_language_fn)),
          type = "warning",
          duration = 12
        )
        NULL
      }
    )
  } else {
    NULL
  }
  htmt_bootstrap_result <- structural_canvas_run_htmt_bootstrap(
    analysis_type, htmt_bootstrap, result, data, htmt_seed, ordered,
    htmt_threshold, htmt_ci_method
  )
  pls_bootstrap_result <- structural_canvas_run_pls_bootstrap(
    analysis_type, pls_bootstrap, result, pls_seed
  )
  pls_predict_result <- structural_canvas_run_pls_predict(
    analysis_type, pls_predict_folds, pls_predict_reps, result
  )
  redundancy_result <- if (identical(analysis_type, "plssem")) {
    structural_canvas_pls_redundancy_analysis(result, snapshot, data, redundancy_construct, redundancy_criterion)
  } else {
    NULL
  }
  parcel_result <- if (identical(analysis_type, "cfa")) {
    structural_canvas_parcel_plan(result, snapshot, parcel_enabled, parcel_construct, parcel_count, parcel_purpose)
  } else {
    NULL
  }
  common_method_result <- if (isTRUE(common_method_enabled) && analysis_type %in% c("cfa", "cbsem", "sem")) {
    tryCatch(
      structural_canvas_run_common_method_diagnostics(
        result, data, analysis_type, estimator, missing, std_lv, ordered, common_method_methods
      ) %||% structural_canvas_common_method_unavailable_result(common_method_methods),
      error = function(error) {
        structural_canvas_show_notification(
          structural_canvas_error_message(error, statedu_current_language(app_language_fn)),
          type = "warning",
          duration = 12
        )
        structural_canvas_common_method_unavailable_result(common_method_methods, conditionMessage(error))
      }
    )
  } else {
    NULL
  }
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
    pls_bootstrap = pls_bootstrap, pls_seed = pls_seed, pls_bootstrap_result = pls_bootstrap_result,
    pls_predict_folds = pls_predict_folds, pls_predict_reps = pls_predict_reps, pls_predict_result = pls_predict_result,
    redundancy_construct = redundancy_construct, redundancy_criterion = redundancy_criterion, redundancy_result = redundancy_result,
    parcel_enabled = parcel_enabled, parcel_construct = parcel_construct, parcel_count = parcel_count,
    parcel_purpose = parcel_purpose, parcel_result = parcel_result,
    invariance_enabled = invariance_enabled, invariance_group = invariance_group, invariance_result = invariance_result,
    mi_holdout_enabled = mi_holdout_enabled, mi_holdout_fraction = mi_holdout_fraction, mi_holdout_seed = mi_holdout_seed,
    analysis_data = data, validation_data = validation_data, holdout_rows = holdout_rows, holdout_comparison = holdout_comparison,
    common_method_enabled = common_method_enabled, common_method_methods = common_method_methods,
    common_method_result = common_method_result,
    estimator = estimator, objective = objective, method_recommendation = method_recommendation,
    structural_effect_plan = result$structural_effect_plan,
    selected_method = structural_canvas_selected_method_label(analysis_type, estimator),
    missing = missing, std_lv = std_lv, ordered = ordered,
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
