structural_canvas_execute_settings <- function(settings, input, prefix) {
  settings <- settings %||% list()

  reliability_bootstrap <- suppressWarnings(as.integer(settings$reliability_bootstrap %||% input[[paste0(prefix, "_reliability_bootstrap")]] %||% 0L))
  if (is.na(reliability_bootstrap) || !reliability_bootstrap %in% c(0L, 500L, 1000L, 2000L)) reliability_bootstrap <- 0L

  reliability_seed <- suppressWarnings(as.integer(settings$reliability_seed %||% input[[paste0(prefix, "_reliability_seed")]] %||% 24680L))
  if (is.na(reliability_seed) || reliability_seed < 1L) reliability_seed <- 24680L

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

  pls_bootstrap <- suppressWarnings(as.integer(settings$pls_bootstrap %||% input[[paste0(prefix, "_pls_bootstrap")]] %||% 0L))
  if (is.na(pls_bootstrap) || !pls_bootstrap %in% c(0L, 500L, 1000L, 2000L)) pls_bootstrap <- 0L

  pls_seed <- suppressWarnings(as.integer(settings$pls_seed %||% input[[paste0(prefix, "_pls_seed")]] %||% 24680L))
  if (is.na(pls_seed) || pls_seed < 1L) pls_seed <- 24680L

  pls_predict_folds <- suppressWarnings(as.integer(settings$pls_predict_folds %||% input[[paste0(prefix, "_pls_predict_folds")]] %||% 0L))
  if (is.na(pls_predict_folds) || !pls_predict_folds %in% c(0L, 5L, 10L)) pls_predict_folds <- 0L

  pls_predict_reps <- suppressWarnings(as.integer(settings$pls_predict_reps %||% input[[paste0(prefix, "_pls_predict_reps")]] %||% 1L))
  if (is.na(pls_predict_reps) || !pls_predict_reps %in% c(1L, 3L, 5L)) pls_predict_reps <- 1L

  mi_holdout_seed <- suppressWarnings(as.integer(settings$mi_holdout_seed %||% input[[paste0(prefix, "_mi_holdout_seed")]] %||% 13579L))
  if (is.na(mi_holdout_seed) || mi_holdout_seed < 1L) mi_holdout_seed <- 13579L

  list(
    estimator = settings$estimator %||% input[[paste0(prefix, "_estimator")]] %||% "ML",
    missing = settings$missing %||% input[[paste0(prefix, "_missing")]] %||% "fiml",
    std_lv = settings$std_lv %||% identical(input[[paste0(prefix, "_scale")]], "variance"),
    mi_mode = settings$mi_mode %||% input[[paste0(prefix, "_mi_mode")]] %||% "theory",
    rmsea_ci = settings$rmsea_ci %||% as.numeric(input[[paste0(prefix, "_rmsea_ci")]] %||% .90),
    validity_formula = settings$validity_formula %||% input[[paste0(prefix, "_validity_formula")]] %||% "standardized",
    reliability_bootstrap = reliability_bootstrap,
    reliability_seed = reliability_seed,
    reliability_ci_method = structural_canvas_bootstrap_ci_method(settings$reliability_ci_method %||% input[[paste0(prefix, "_reliability_ci_method")]] %||% "percentile"),
    bollen_stine_bootstrap = bollen_stine_bootstrap,
    bollen_stine_seed = bollen_stine_seed,
    htmt_threshold = htmt_threshold,
    htmt_bootstrap = htmt_bootstrap,
    htmt_seed = htmt_seed,
    htmt_ci_method = structural_canvas_bootstrap_ci_method(settings$htmt_ci_method %||% input[[paste0(prefix, "_htmt_ci_method")]] %||% "percentile"),
    pls_bootstrap = pls_bootstrap,
    pls_seed = pls_seed,
    pls_predict_folds = pls_predict_folds,
    pls_predict_reps = pls_predict_reps,
    invariance_enabled = isTRUE(settings$invariance_enabled %||% input[[paste0(prefix, "_invariance_enabled")]] %||% FALSE),
    invariance_group = as.character(settings$invariance_group %||% input[[paste0(prefix, "_invariance_group")]] %||% ""),
    mi_holdout_enabled = isTRUE(settings$mi_holdout_enabled %||% input[[paste0(prefix, "_mi_holdout_enabled")]] %||% FALSE),
    mi_holdout_fraction = as.numeric(settings$mi_holdout_fraction %||% input[[paste0(prefix, "_mi_holdout_fraction")]] %||% .30),
    mi_holdout_seed = mi_holdout_seed,
    result_coefficient = settings$result_coefficient %||% input[[paste0(prefix, "_result_coefficient")]] %||% "beta",
    residual_variance_fixes = settings$residual_variance_fixes %||% numeric(0)
  )
}
