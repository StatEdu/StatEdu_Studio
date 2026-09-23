#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x
default_seed <- function() 24680L
structural_canvas_with_progress <- function(message, value = 0, expr) force(expr)
structural_canvas_set_progress <- function(...) invisible(NULL)

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(value, expected, message, tolerance = NULL) {
  equal <- if (is.null(tolerance)) identical(value, expected) else isTRUE(all.equal(value, expected, tolerance = tolerance))
  if (!equal) stop(message, call. = FALSE)
}

if (!requireNamespace("seminr", quietly = TRUE)) {
  stop("seminr is required for the PLSc PLSpredict validation.", call. = FALSE)
}

source("R/setup_custom_model_canvas_structural_pls_engine.R", local = globalenv(), encoding = "UTF-8")
source("scripts/pls_validation_fixture.R", local = environment(), encoding = "UTF-8")

mobi <- statedu_pls_validation_fixture()
variables <- c(paste0("IMAG", 1:5), paste0("CUEX", 1:3))
complete_data <- mobi[, variables, drop = FALSE]
measurement_model <- seminr::constructs(
  seminr::composite("Image", paste0("IMAG", 1:5), weights = seminr::mode_A),
  seminr::composite("Expect", paste0("CUEX", 1:3), weights = seminr::mode_A)
)
structural_model <- seminr::relationships(seminr::paths(from = "Image", to = "Expect"))

fit_model <- function(data) {
  seminr::estimate_pls(
    data = data,
    measurement_model = measurement_model,
    structural_model = structural_model,
    missing = seminr::mean_replacement,
    maxIt = structural_canvas_pls_max_iterations(),
    stopCriterion = structural_canvas_pls_stop_criterion(),
    assess_syntax = FALSE
  )
}

# Complete-data PLS remains numerically compatible with seminr's direct-
# antecedent out-of-sample item and LM benchmark metrics.
complete_fit <- fit_model(complete_data)
invalid_loading_fit <- complete_fit
nonzero_loading <- which(abs(invalid_loading_fit$outer_loadings) > 0, arr.ind = TRUE)[1L, , drop = FALSE]
invalid_loading_fit$outer_loadings[nonzero_loading] <- 1.01
invalid_loading_gate <- structural_canvas_pls_bootstrap_draw_gate(invalid_loading_fit, "PLS")
assert_true(!isTRUE(invalid_loading_gate$valid) && identical(invalid_loading_gate$reason, "inadmissible"),
  "A standardized outer loading outside [-1, 1] passed the common PLS admissibility gate.")
fold_count_error <- tryCatch(
  {
    structural_canvas_pls_predict_repetition(complete_fit, nrow(complete_data) + 1L, "PLS")
    ""
  },
  error = conditionMessage
)
assert_true(grepl("folds must be between 2", fold_count_error, fixed = TRUE),
  "A fold count larger than N was not rejected.")
rank_deficient_training <- complete_data[1:100, , drop = FALSE]
rank_deficient_testing <- complete_data[101:110, , drop = FALSE]
rank_deficient_training$IMAG2 <- rank_deficient_training$IMAG1
rank_deficient_testing$IMAG2 <- rank_deficient_testing$IMAG1
rank_deficient_error <- tryCatch(
  {
    structural_canvas_pls_predict_lm_fold(complete_fit, rank_deficient_training, rank_deficient_testing)
    ""
  },
  error = conditionMessage
)
assert_true(grepl("rank deficient", rank_deficient_error, fixed = TRUE),
  "A rank-deficient direct-antecedent LM benchmark did not fail closed.")
set.seed(42L)
seminr_summary <- summary(seminr::predict_pls(
  complete_fit, technique = seminr::predict_DA, noFolds = 5L, reps = 1L, cores = NULL
))
set.seed(42L)
statedu_summary <- structural_canvas_pls_predict_repetition(complete_fit, 5L, "PLS")
assert_equal(
  statedu_summary$PLS_out_of_sample, seminr_summary$PLS_out_of_sample,
  "Complete-data PLS out-of-sample indicator metrics changed.", tolerance = 1e-12
)
assert_equal(
  statedu_summary$LM_out_of_sample, seminr_summary$LM_out_of_sample,
  "Complete-data direct-antecedent LM benchmark metrics changed.", tolerance = 1e-12
)

# Missing values are imputed from training rows only. A holdout extreme value
# and a holdout missing value cannot change the recorded training mean.
training <- data.frame(x = c(1, 3, NA_real_), y = c(2, 4, 6))
testing <- data.frame(x = c(NA_real_, 10000), y = c(100000, NA_real_))
prepared <- structural_canvas_pls_predict_fold_data(training, testing, c("x", "y"))
assert_equal(unname(prepared$training_means), c(2, 4), "Fold imputation used holdout values.", tolerance = 0)
assert_equal(prepared$testing$x[[1L]], 2, "Holdout missing value did not use the training-fold mean.", tolerance = 0)
assert_equal(prepared$testing$y[[2L]], 4, "Holdout missing value did not use the training-fold mean.", tolerance = 0)
infinite_error <- tryCatch(
  {
    structural_canvas_pls_predict_fold_data(data.frame(x = c(1, Inf)), data.frame(x = 2), "x")
    ""
  },
  error = conditionMessage
)
assert_true(grepl("infinite values", infinite_error, fixed = TRUE),
  "Infinite values were silently treated as missing values.")
sentinel_prepared <- structural_canvas_pls_predict_fold_data(
  data.frame(x = c(1, 3, -999)), data.frame(x = -999), "x", missing_value = -999
)
assert_equal(sentinel_prepared$testing$x[[1L]], 2,
  "Configured missing sentinel did not use the training-fold mean.", tolerance = 0)

missing_data <- complete_data
missing_data[c(1L, 7L, 31L), "IMAG1"] <- NA_real_
missing_data[c(9L, 43L), "CUEX2"] <- NA_real_
base_fit <- fit_model(missing_data)
base_fit$statedu_common_factor_constructs <- c("Image", "Expect")
pls_result <- list(fit = base_fit, estimator = "PLS")
plsc_fit <- structural_canvas_apply_plsc(base_fit, c("Image", "Expect"))
plsc_result <- list(
  fit = plsc_fit, estimator = "PLSc", common_factor_constructs = c("Image", "Expect")
)

# Wrapper-level reproducibility and caller RNG restoration are part of the
# existing contract for repeated PLSpredict.
set.seed(1618L)
rng_before <- .Random.seed
kind_before <- RNGkind()
pls_prediction <- structural_canvas_run_pls_predict("plssem", 5L, 2L, pls_result, 20260825L)
pls_repeat <- structural_canvas_run_pls_predict("plssem", 5L, 2L, pls_result, 20260825L)
assert_equal(pls_prediction$repetition_summaries, pls_repeat$repetition_summaries, "PLS PLSpredict is not reproducible.")
assert_equal(.Random.seed, rng_before, "PLSpredict changed the caller's RNG state.")
assert_equal(RNGkind(), kind_before, "PLSpredict changed the caller's RNG kind.")
assert_true(identical(pls_prediction$technique, "Direct antecedents"), "Direct-antecedent prediction semantics changed.")
assert_true(identical(pls_prediction$missing_preprocessing, "Training-fold means applied to training and holdout rows"),
  "Training-fold missing-data preprocessing was not recorded.")
assert_true(length(pls_prediction$repetition_diagnostics) == 2L,
  "Fold diagnostics were not retained at the PLSpredict result level.")
assert_true(length(pls_prediction$effective_n) == 2L,
  "Observed-cell evaluation counts were not retained at the PLSpredict result level.")
expected_cuex2_n <- nrow(missing_data) - sum(is.na(missing_data$CUEX2))
assert_equal(
  unname(pls_prediction$repetition_summaries[[1L]]$effective_n$PLS_out_of_sample[["CUEX2"]]),
  as.integer(expected_cuex2_n),
  "Imputed holdout outcomes were incorrectly counted as observed prediction errors.",
  tolerance = 0
)

plsc_prediction <- structural_canvas_run_pls_predict("plssem", 5L, 2L, plsc_result, 20260825L)
plsc_repeat <- structural_canvas_run_pls_predict("plssem", 5L, 2L, plsc_result, 20260825L)
assert_equal(plsc_prediction$repetition_summaries, plsc_repeat$repetition_summaries, "PLSc PLSpredict is not reproducible.")
assert_true(identical(plsc_prediction$estimator, "PLSc"), "PLSc estimator metadata was not preserved.")
metric_difference <- max(abs(
  as.matrix(pls_prediction$summary$PLS_out_of_sample) -
    as.matrix(plsc_prediction$summary$PLS_out_of_sample)
))
assert_true(is.finite(metric_difference) && metric_difference > 1e-6,
  "The fixture did not distinguish fold-native PLS and PLSc predictions.")
fold_diagnostics <- attr(plsc_prediction$repetition_summaries[[1L]], "statedu_fold_diagnostics")
assert_true(length(fold_diagnostics) == 5L, "PLSc fold diagnostics were not retained.")
assert_true(all(vapply(fold_diagnostics, function(value) {
  identical(value$estimator, "PLSc") && isTRUE(value$converged) && isTRUE(value$admissible)
}, logical(1))), "A PLSc prediction fold bypassed the PLSc convergence/admissibility gate.")
assert_true(all(vapply(fold_diagnostics, function(value) {
  length(value$training_imputations) == length(variables) &&
    length(value$holdout_imputations) == length(variables)
}, logical(1))), "Fold-level missing-data replacement counts were not retained.")

# Any fold estimation failure aborts the repetition instead of returning a
# partial metric matrix that could be interpreted as valid prediction output.
original_estimator <- structural_canvas_pls_predict_estimate_fold
fold_calls <- 0L
structural_canvas_pls_predict_estimate_fold <- function(...) {
  fold_calls <<- fold_calls + 1L
  if (fold_calls == 2L) stop("validation-forced fold failure", call. = FALSE)
  original_estimator(...)
}
on.exit(assign("structural_canvas_pls_predict_estimate_fold", original_estimator, envir = .GlobalEnv), add = TRUE)
set.seed(77L)
failure_message <- tryCatch(
  {
    structural_canvas_pls_predict_repetition(complete_fit, 5L, "PLS")
    ""
  },
  error = conditionMessage
)
assign("structural_canvas_pls_predict_estimate_fold", original_estimator, envir = .GlobalEnv)
assert_true(grepl("PLSpredict fold 2 failed closed", failure_message, fixed = TRUE),
  "A failed prediction fold did not fail closed with its fold position.")

audit_source <- paste(readLines("R/setup_custom_model_canvas_export_report.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
assert_true(
  all(vapply(c("missing_preprocessing", "effective_n", "repetition_diagnostics"), function(field) {
    grepl(field, audit_source, fixed = TRUE)
  }, logical(1))),
  "PLSpredict fold preprocessing/effective-N diagnostics were not retained in the audit manifest."
)

cat("PLSc PLSpredict consistency validation PASS\n")
