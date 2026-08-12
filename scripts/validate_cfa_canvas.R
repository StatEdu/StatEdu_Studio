source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")
stopifnot(grepl("Latent construct correlations, reliability, and convergent/discriminant validity", ui_source, fixed = TRUE))
stopifnot(length(gregexpr('class = "table-responsive"', ui_source, fixed = TRUE)[[1L]]) >= 8L)

stopifnot(requireNamespace("lavaan", quietly = TRUE))
stopifnot(
  structural_canvas_minimum_eigenvalue(diag(2L)) > 0,
  structural_canvas_minimum_eigenvalue(matrix(c(1, 1.2, 1.2, 1), 2L)) < 0,
  abs(structural_canvas_minimum_eigenvalue(diag(c(1, 0)))) < 1e-12,
  identical(structural_canvas_symmetric_condition_number(diag(2L)), 1),
  structural_canvas_symmetric_condition_number(diag(c(1, 1e-9))) > 1e8,
  is.infinite(structural_canvas_symmetric_condition_number(diag(c(1, 0))))
)
stopifnot(
  identical(structural_canvas_measurement_quality_guidance(.60, .80, .75, .82), "Meets common cutoffs"),
  grepl("AVE", structural_canvas_measurement_quality_guidance(.40, .80, .75, .82), fixed = TRUE),
  grepl("Cronbach's α", structural_canvas_measurement_quality_guidance(.60, .80, .65, .82), fixed = TRUE),
  grepl("ωtotal", structural_canvas_measurement_quality_guidance(.60, .80, .75, .65), fixed = TRUE),
  identical(structural_canvas_measurement_quality_guidance(.60, 1.05, .75, .82), "Review inadmissible coefficient(s)")
)
stopifnot(
  identical(structural_canvas_indicator_loading_guidance(.70, .55, .82, .51), "No loading flag"),
  identical(structural_canvas_indicator_loading_guidance(.30, .10, .49, .91), "Weak loading review"),
  identical(structural_canvas_indicator_loading_guidance(.20, -.05, .45, .96), "Loading CI includes 0"),
  identical(structural_canvas_indicator_loading_guidance(.70, .55, .82, .51, cross_loaded = TRUE), "Review cross-loading"),
  identical(structural_canvas_indicator_loading_guidance(.70, .55, .82, -.02), "Review residual variance"),
  identical(structural_canvas_indicator_loading_guidance(NA_real_, NA_real_, NA_real_, .50), "Not assessed")
)

latent <- list(id = "latent_1", role = "latent", name = "eta1", x = 200, y = 200)
indicators <- lapply(seq_len(3L), function(index) {
  list(id = paste0("indicator_", index), role = "indicator", name = paste0("x", index), x = 400, y = index * 100)
})
snapshot <- list(
  nodes = c(list(latent), indicators),
  edges = lapply(seq_len(3L), function(index) list(from = "latent_1", to = paste0("indicator_", index)))
)

set.seed(20260812)
n <- 400L
factor_score <- stats::rnorm(n)
continuous <- data.frame(
  x1 = .8 * factor_score + stats::rnorm(n, sd = .6),
  x2 = .7 * factor_score + stats::rnorm(n, sd = .7),
  x3 = .9 * factor_score + stats::rnorm(n, sd = .5)
)

ml <- run_structural_canvas_analysis(snapshot, continuous, "cfa")
stopifnot(isTRUE(ml$converged))
factor_score_quality <- structural_canvas_factor_score_quality(ml$fit)
reliability_estimates <- structural_canvas_reliability_estimates(ml$fit)
model_implied_reliability <- structural_canvas_reliability_estimates(ml$fit, "model_implied")
reproducibility_record <- structural_canvas_reproducibility_record(list(
  fit = ml$fit, syntax = ml$syntax, estimator = "ML", missing = "fiml", std_lv = FALSE,
  ordered = character(0), validity_formula = "standardized", rmsea_ci = .90,
  htmt_threshold = .85, htmt_bootstrap = 0L, htmt_seed = 12345L,
  reliability_bootstrap = 0L, reliability_seed = 24680L,
  invariance_enabled = FALSE, mi_mode = "theory", diagnostics = ml
), as.POSIXct("2026-08-12 12:00:00", tz = "Asia/Seoul"))
stopifnot(
  isTRUE(ml$post_check), isTRUE(ml$admissible),
  !length(ml$negative_residuals), !length(ml$negative_latent_variances),
  is.finite(ml$theta_min_eigenvalue), ml$theta_min_eigenvalue > 0, !isTRUE(ml$non_psd_theta), !isTRUE(ml$near_singular_theta),
  is.finite(ml$latent_min_eigenvalue), ml$latent_min_eigenvalue > 0, !isTRUE(ml$non_psd_latent_covariance), !isTRUE(ml$near_singular_latent_covariance),
  is.finite(ml$theta_condition_number), !isTRUE(ml$ill_conditioned_theta),
  is.finite(ml$latent_condition_number), !isTRUE(ml$ill_conditioned_latent_covariance),
  is.finite(ml$parameter_min_eigenvalue), ml$parameter_min_eigenvalue > 0,
  !isTRUE(ml$non_psd_parameter_covariance), !isTRUE(ml$near_singular_parameter_covariance),
  is.finite(ml$parameter_condition_number), !isTRUE(ml$ill_conditioned_parameter_covariance),
  nrow(factor_score_quality) == 1L, identical(factor_score_quality$Factor, "eta1"),
  factor_score_quality$Determinacy > 0, factor_score_quality$Determinacy <= 1,
  abs(factor_score_quality[["Score reliability"]] - factor_score_quality$Determinacy^2) < 1e-12
)
stopifnot(
  grepl("CFA analysis reproducibility record", reproducibility_record, fixed = TRUE),
  grepl("Analysis context: Prespecified/original model.", reproducibility_record, fixed = TRUE),
  grepl("Estimator: ML", reproducibility_record, fixed = TRUE),
  grepl("CI method: percentile", reproducibility_record, fixed = TRUE),
  grepl("eta1 =~ x1 + x2 + x3", reproducibility_record, fixed = TRUE),
  grepl("lavaan version:", reproducibility_record, fixed = TRUE)
)
stopifnot(
  nrow(reliability_estimates) == 1L, identical(reliability_estimates$Factor, "eta1"),
  all(is.finite(unlist(reliability_estimates[c("AVE", "CR", "Alpha", "Omega")], use.names = FALSE))),
  abs(reliability_estimates$CR - reliability_estimates$Omega) < 1e-12
)
standardized_reliability_solution <- lavaan::standardizedSolution(ml$fit)
standardized_reliability_loadings <- standardized_reliability_solution$est.std[standardized_reliability_solution$op == "=~"]
reliability_indicator_names <- c("x1", "x2", "x3")
standardized_reliability_theta <- standardized_reliability_solution[
  standardized_reliability_solution$op == "~~" & standardized_reliability_solution$lhs %in% reliability_indicator_names & standardized_reliability_solution$rhs %in% reliability_indicator_names,
  c("lhs", "rhs", "est.std"), drop = FALSE
]
expected_theta <- matrix(0, 3L, 3L, dimnames = list(reliability_indicator_names, reliability_indicator_names))
for (index in seq_len(nrow(standardized_reliability_theta))) {
  expected_theta[standardized_reliability_theta$lhs[[index]], standardized_reliability_theta$rhs[[index]]] <- standardized_reliability_theta$est.std[[index]]
  expected_theta[standardized_reliability_theta$rhs[[index]], standardized_reliability_theta$lhs[[index]]] <- standardized_reliability_theta$est.std[[index]]
}
expected_standardized_cr <- sum(standardized_reliability_loadings)^2 / (sum(standardized_reliability_loadings)^2 + sum(expected_theta))
stopifnot(abs(reliability_estimates$CR - expected_standardized_cr) < 1e-12)
stopifnot(nrow(model_implied_reliability) == 1L, all(is.finite(unlist(model_implied_reliability[c("AVE", "CR", "Alpha", "Omega")], use.names = FALSE))))
seed_before_reliability_bootstrap <- .Random.seed
reliability_progress_events <- list()
reliability_bootstrap <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", continuous, reps = 20L, seed = 20260812L, estimator = "ML", original_fit = ml$fit,
  progress = function(done, total, valid) {
    reliability_progress_events[[length(reliability_progress_events) + 1L]] <<- c(done = done, total = total, valid = valid)
  }
)
reliability_cancel_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 2L, seed = 20260812L, estimator = "ML",
    original_fit = ml$fit, cancel = function() TRUE
  )
  ""
}, error = conditionMessage)
stopifnot(
  isTRUE(structural_canvas_validate_model_based_bootstrap(ml$fit)),
  identical(structural_canvas_bootstrap_status(c(80, 79, 50, 49, 0), 100), c("Adequate", "Caution", "Caution", "Unreliable", "Unreliable")),
  nrow(reliability_bootstrap) == 4L,
  identical(reliability_bootstrap$Statistic, c("AVE", "CR", "Alpha", "Omega")),
  all(reliability_bootstrap$Lower <= reliability_bootstrap$Upper),
  all(reliability_bootstrap[["Valid replicates"]] > 0L),
  all(reliability_bootstrap[["Requested replicates"]] == 20L),
  all(reliability_bootstrap[["Valid %"]] > 0 & reliability_bootstrap[["Valid %"]] <= 100),
  all(reliability_bootstrap[["CI method"]] == "Percentile"),
  all(reliability_bootstrap$Status == "Adequate"),
  identical(.Random.seed, seed_before_reliability_bootstrap),
  length(reliability_progress_events) >= 2L,
  reliability_progress_events[[1L]][["done"]] == 0L,
  tail(reliability_progress_events, 1L)[[1L]][["done"]] == 20L,
  tail(reliability_progress_events, 1L)[[1L]][["valid"]] > 0L,
  grepl("canceled", reliability_cancel_error, fixed = TRUE),
  identical(structural_canvas_bootstrap_ci_method("BCa (slower)"), "bca"),
  grepl('progress(index, total_iterations, length(Filter(function(value) !is.null(value) && nrow(value), estimates[seq_len(index)])))', ui_source, fixed = TRUE),
  grepl("_reliability_ci_method", ui_source, fixed = TRUE)
)
small_continuous <- continuous[seq_len(80L), , drop = FALSE]
small_ml <- lavaan::cfa("eta1 =~ x1 + x2 + x3", data = small_continuous, auto.cov.lv.x = FALSE)
reliability_bca <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", small_continuous, reps = 25L, seed = 20260813L,
  estimator = "ML", missing = "listwise", original_fit = small_ml, ci_method = "bca"
)
stopifnot(
  nrow(reliability_bca) == 4L,
  all(reliability_bca[["CI method"]] %in% c("BCa", "BCa unavailable")),
  all(reliability_bca[["Valid replicates"]] > 0L)
)
inadmissible_bootstrap_error <- tryCatch({
  structural_canvas_validate_model_based_bootstrap(NULL, "Test bootstrap")
  ""
}, error = conditionMessage)
stopifnot(identical(inadmissible_bootstrap_error, "Test bootstrap requires a fitted lavaan CFA model."))
invalid_reliability_bootstrap_error <- tryCatch({
  structural_canvas_reliability_bootstrap("eta1 =~ missing_variable + x1", continuous, reps = 2L)
  ""
}, error = conditionMessage)
stopifnot(grepl("could not fit the original CFA model", invalid_reliability_bootstrap_error, fixed = TRUE))
multigroup_reliability_data <- continuous
multigroup_reliability_data$group <- rep(c("A", "B"), each = n / 2L)
multigroup_reliability_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3", data = multigroup_reliability_data,
  group = "group", auto.cov.lv.x = FALSE
)
multigroup_reliability_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", multigroup_reliability_data, reps = 2L,
    estimator = "ML", missing = "listwise", original_fit = multigroup_reliability_fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("only single-group CFA models", multigroup_reliability_error, fixed = TRUE))
mismatched_reliability_fit_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 2L, estimator = "MLR", original_fit = ml$fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("estimator does not match", mismatched_reliability_fit_error, fixed = TRUE))
stopifnot(
  identical(structural_canvas_normalize_missing_option("FIML"), "ml"),
  identical(structural_canvas_normalize_missing_option("direct"), "ml"),
  identical(structural_canvas_normalize_missing_option("default"), "listwise"),
  identical(structural_canvas_normalize_missing_option("fiml.x"), "ml.x")
)
listwise_reliability_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3", data = continuous, missing = "listwise", auto.cov.lv.x = FALSE
)
mismatched_reliability_missing_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 2L, estimator = "ML",
    missing = "fiml", original_fit = listwise_reliability_fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("missing-data option does not match", mismatched_reliability_missing_error, fixed = TRUE))
mismatched_reliability_scale_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 2L, estimator = "ML", std_lv = TRUE, original_fit = ml$fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("latent-scaling option does not match", mismatched_reliability_scale_error, fixed = TRUE))
mismatched_reliability_ordered_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 2L, estimator = "ML", ordered = "x1", original_fit = ml$fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("ordered-indicator specification does not match", mismatched_reliability_ordered_error, fixed = TRUE))
perturbed_reliability_data <- continuous
perturbed_reliability_data$x1[[1L]] <- perturbed_reliability_data$x1[[1L]] + .01
mismatched_reliability_data_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", perturbed_reliability_data, reps = 2L, estimator = "ML", original_fit = ml$fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("does not use the same analyzed observations and values", mismatched_reliability_data_error, fixed = TRUE))
mismatched_reliability_structure_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3\nx1 ~~ x2", continuous, reps = 2L, estimator = "ML", original_fit = ml$fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("parameter structure does not match", mismatched_reliability_structure_error, fixed = TRUE))

invariance_data <- continuous
invariance_data$group <- rep(c("A", "B"), each = n / 2L)
invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3", invariance_data, "group", estimator = "MLR"
)
stopifnot(
  identical(invariance$table$Model, c("Configural", "Metric", "Scalar", "Strict")),
  nrow(invariance$table) == 4L, length(invariance$fits) == 4L,
  all(invariance$table$Converged), all(invariance$table$Admissible),
  all(invariance$table[["Admissibility reasons"]] == "None"),
  all(invariance$table[["Parameter boundary dimensions"]] <= invariance$table[["Explicit equality constraints"]]),
  all(vapply(invariance$table[c("Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue")], function(values) all(is.finite(values)), logical(1))),
  all(vapply(invariance$table[c("Residual condition number", "Latent condition number", "Parameter condition number")], function(values) all(is.finite(values) | is.infinite(values)), logical(1))),
  is.logical(invariance$table[["Ill-conditioned warning"]]),
  all(invariance$table[["Residual min eigenvalue"]] > 0),
  all(invariance$table[["Latent min eigenvalue"]] > 0),
  all(vapply(invariance$fits, function(fit) structural_canvas_fit_admissibility(fit)$admissible, logical(1))),
  all(vapply(invariance$fits, function(fit) identical(structural_canvas_fit_admissibility(fit)$group_labels, c("A", "B")), logical(1))),
  nrow(invariance$group_reliability) == 2L,
  identical(invariance$group_reliability$Group, c("A", "B")),
  identical(invariance$group_reliability$Factor, c("eta1", "eta1")),
  all(is.finite(invariance$group_reliability$AVE)),
  nrow(invariance$group_htmt) == 0L,
  all(diff(invariance$table$df) >= 0),
  all(is.finite(invariance$table$CFI)), all(is.finite(invariance$table$RMSEA)), all(is.finite(invariance$table$SRMR)),
  all(is.finite(invariance$table$DeltaCFI[-1L])),
  all(is.finite(invariance$table$DeltaChisq[-1L])), all(is.finite(invariance$table$DeltaDf[-1L])), all(is.finite(invariance$table$DeltaP[-1L])),
  identical(names(invariance$score_diagnostics), c("Configural", "Metric", "Scalar", "Strict")),
  nrow(invariance$score_diagnostics$Configural) == 0L,
  all(vapply(invariance$score_diagnostics[-1L], nrow, integer(1)) > 0L),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(diff(value[["Score χ²"]]) <= 0), logical(1))),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(value[["BH-adjusted p"]] >= value$p, na.rm = TRUE), logical(1))),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(value[["BH-adjusted p"]] >= 0 & value[["BH-adjusted p"]] <= 1, na.rm = TRUE), logical(1))),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(is.finite(value[["Max |standardized EPC|"]]) & value[["Max |standardized EPC|"]] >= 0), logical(1))),
  all(vapply(invariance$score_diagnostics[-1L], function(value) all(grepl("group A|group B", value$Constraint)), logical(1)))
)
stopifnot(inherits(try(structural_canvas_measurement_invariance("eta1 =~ x1 + x2 + x3", invariance_data, "missing_group"), silent = TRUE), "try-error"))
holdout_data <- continuous
holdout_data$x4 <- .65 * factor_score + stats::rnorm(n, sd = .75)
seed_before_holdout_split <- .Random.seed
holdout_split <- structural_canvas_holdout_split(holdout_data, validation_fraction = .30, seed = 13579L)
holdout_split_repeat <- structural_canvas_holdout_split(holdout_data, validation_fraction = .30, seed = 13579L)
stopifnot(
  nrow(holdout_split$exploration) == 280L, nrow(holdout_split$validation) == 120L,
  identical(holdout_split$validation_rows, holdout_split_repeat$validation_rows),
  !length(intersect(holdout_split$exploration_rows, holdout_split$validation_rows)),
  identical(sort(c(holdout_split$exploration_rows, holdout_split$validation_rows)), seq_len(n)),
  identical(.Random.seed, seed_before_holdout_split)
)
holdout_comparison <- structural_canvas_holdout_model_comparison(
  "eta1 =~ x1 + x2 + x3 + x4", "eta1 =~ x1 + x2 + x3 + x4\nx1 ~~ x2",
  holdout_split$validation, estimator = "MLR", missing = "fiml"
)
stopifnot(
  identical(holdout_comparison$table$Model, c("Original model", "Modified model")),
  all(holdout_comparison$table$Converged), all(holdout_comparison$table$Admissible),
  all(holdout_comparison$table[["Admissibility reasons"]] == "None"),
  all(vapply(holdout_comparison$fits, function(fit) structural_canvas_fit_admissibility(fit)$admissible, logical(1))),
  holdout_comparison$validation_n_raw == 120L,
  all(holdout_comparison$validation_n_used == 120L),
  all(holdout_comparison$table[["N used"]] == 120L),
  identical(holdout_comparison$changes$`Comparison status`[[1L]], "Both validation models admissible"),
  all(is.finite(unlist(holdout_comparison$changes[vapply(holdout_comparison$changes, is.numeric, logical(1))], use.names = FALSE)))
)
stopifnot(inherits(try(structural_canvas_holdout_split(holdout_data[1:50, ], .30), silent = TRUE), "try-error"))
stopifnot(isTRUE(structural_canvas_validate_holdout_options(TRUE, "cfa", "MLR")))
stopifnot(inherits(try(structural_canvas_validate_holdout_options(TRUE, "cfa", "WLSMV", ordered = "x1"), silent = TRUE), "try-error"))
stopifnot(inherits(try(structural_canvas_validate_holdout_options(TRUE, "cfa", "MLR", invariance_enabled = TRUE), silent = TRUE), "try-error"))
stopifnot(inherits(try(structural_canvas_validate_holdout_options(TRUE, "cfa", "MLR", residual_variance_fixes = c(x1 = .001)), silent = TRUE), "try-error"))
stopifnot(isTRUE(structural_canvas_validate_holdout_reuse(TRUE, FALSE)))
stopifnot(inherits(try(structural_canvas_validate_holdout_reuse(TRUE, TRUE), silent = TRUE), "try-error"))
stopifnot(isTRUE(structural_canvas_validate_holdout_reuse(FALSE, TRUE)))
standardized_loadings_ci <- lavaan::standardizedSolution(ml$fit, ci = TRUE, level = .95)
standardized_loadings_ci <- standardized_loadings_ci[standardized_loadings_ci$op == "=~", , drop = FALSE]
raw_loadings_ci <- lavaan::parameterEstimates(ml$fit)
raw_loadings_ci <- raw_loadings_ci[raw_loadings_ci$op == "=~", , drop = FALSE]
stopifnot(
  nrow(standardized_loadings_ci) == 3L,
  nrow(raw_loadings_ci) == 3L,
  all(is.finite(raw_loadings_ci$ci.lower)), all(is.finite(raw_loadings_ci$ci.upper)),
  all(raw_loadings_ci$ci.lower <= raw_loadings_ci$est), all(raw_loadings_ci$est <= raw_loadings_ci$ci.upper),
  all(is.finite(standardized_loadings_ci$ci.lower)),
  all(is.finite(standardized_loadings_ci$ci.upper)),
  all(standardized_loadings_ci$ci.lower <= standardized_loadings_ci$est.std),
  all(standardized_loadings_ci$est.std <= standardized_loadings_ci$ci.upper)
)
standardized_solution_ml <- lavaan::standardizedSolution(ml$fit)
standardized_residual_ml <- standardized_solution_ml[
  standardized_solution_ml$op == "~~" & standardized_solution_ml$lhs == standardized_solution_ml$rhs &
    standardized_solution_ml$lhs %in% c("x1", "x2", "x3"),
  c("lhs", "est.std", "ci.lower", "ci.upper"), drop = FALSE
]
r2_ml <- lavaan::lavInspect(ml$fit, "r2")
stopifnot(
  nrow(standardized_residual_ml) == 3L,
  all(abs(standardized_residual_ml$est.std + as.numeric(r2_ml[standardized_residual_ml$lhs]) - 1) < 1e-6),
  all(1 - standardized_residual_ml$ci.upper <= as.numeric(r2_ml[standardized_residual_ml$lhs])),
  all(as.numeric(r2_ml[standardized_residual_ml$lhs]) <= 1 - standardized_residual_ml$ci.lower)
)
correlations <- as.matrix(lavaan::lavInspect(ml$fit, "cor.lv"))
stopifnot(
  identical(ml$admissible, structural_canvas_fit_admissibility(ml$fit)$admissible),
  identical(ml$admissibility_reasons, structural_canvas_fit_admissibility(ml$fit)$reasons)
)
stopifnot(identical(dim(correlations), c(1L, 1L)), identical(rownames(correlations), "eta1"))
single_factor_correlation_export <- structural_canvas_export_latent_correlations(ml$fit)
single_factor_reliability_export <- structural_canvas_export_reliability_validity(list(
  fit = ml$fit, snapshot = snapshot, validity_formula = "standardized"
))
stopifnot(
  identical(names(single_factor_correlation_export), c("Factor", "eta1")),
  identical(single_factor_correlation_export$Factor, "eta1"),
  is.numeric(single_factor_correlation_export$eta1),
  abs(single_factor_correlation_export$eta1 - 1) < 1e-12,
  nrow(single_factor_reliability_export) == 1L,
  isTRUE(single_factor_reliability_export$`Fornell-Larcker assessed`[[1L]] == FALSE),
  is.numeric(single_factor_reliability_export$AVE)
)
two_factor_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6",
  data = lavaan::HolzingerSwineford1939
)
two_factor_invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6\neta1 ~~ eta2",
  lavaan::HolzingerSwineford1939, "school", estimator = "MLR"
)
stopifnot(
  nrow(two_factor_invariance$group_reliability) == 4L,
  all(c("Group", "Factor", "AVE", "CR", "Cronbach's alpha", "Omega total") %in% names(two_factor_invariance$group_reliability)),
  identical(sort(unique(two_factor_invariance$group_reliability$Factor)), c("eta1", "eta2")),
  all(is.finite(two_factor_invariance$group_reliability$AVE)),
  nrow(two_factor_invariance$group_htmt) == 2L,
  all(c("Group", "Factor1", "Factor2", "HTMT", "Criterion") %in% names(two_factor_invariance$group_htmt)),
  all(is.finite(two_factor_invariance$group_htmt$HTMT)),
  isTRUE(two_factor_invariance$group_residuals$available),
  nrow(two_factor_invariance$group_residuals$group_summary) == 2L,
  all(c("Group", "Max |standardized residual|", "Flagged residuals") %in% names(two_factor_invariance$group_residuals$group_summary)),
  nrow(two_factor_invariance$group_residuals$group_pairs) > 0L,
  all(c("Group", "Indicator1", "Indicator2", "Standardized residual", "Correlation residual", "Exceeds cutoff") %in% names(two_factor_invariance$group_residuals$group_pairs))
)
automatic_covariance_bootstrap_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6",
    lavaan::HolzingerSwineford1939,
    reps = 2L,
    estimator = "ML",
    missing = "listwise",
    original_fit = two_factor_fit
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("automatic exogenous latent covariances", automatic_covariance_bootstrap_error, fixed = TRUE))
two_factor_mlr_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6",
  data = lavaan::HolzingerSwineford1939, estimator = "MLR"
)
alternative_two_factor_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x4\neta2 =~ x3 + x5 + x6",
  data = lavaan::HolzingerSwineford1939
)
perturbed_hs_data <- lavaan::HolzingerSwineford1939
perturbed_hs_data$x1[[1L]] <- perturbed_hs_data$x1[[1L]] + .01
perturbed_two_factor_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6",
  data = perturbed_hs_data
)
different_observation_eligibility <- structural_canvas_nested_comparison_eligibility(two_factor_fit, perturbed_two_factor_fit)
stopifnot(
  !isTRUE(different_observation_eligibility$available),
  identical(different_observation_eligibility$reason, "Models do not use the same analyzed observations and values.")
)
nonnested_eligibility <- structural_canvas_nested_comparison_eligibility(two_factor_fit, alternative_two_factor_fit)
nonnested_difference_report <- structural_canvas_model_difference_report(list(
  baseline_fit = two_factor_fit, fit = alternative_two_factor_fit, comparison_type = "mi"
))
stopifnot(
  !isTRUE(nonnested_eligibility$available),
  grepl("degrees of freedom", nonnested_eligibility$reason, fixed = TRUE) || grepl("nesting", nonnested_eligibility$reason, fixed = TRUE),
  is.null(structural_canvas_model_difference(two_factor_fit, alternative_two_factor_fit)),
  !isTRUE(nonnested_difference_report$Available[[1L]]),
  grepl("degrees of freedom", nonnested_difference_report$Reason[[1L]], fixed = TRUE) || grepl("nesting", nonnested_difference_report$Reason[[1L]], fixed = TRUE)
)
rmsea_tests_ml <- structural_canvas_rmsea_hypothesis_tests(list(fit = two_factor_fit, estimator = "ML", rmsea_ci = .90))
rmsea_tests_mlr <- structural_canvas_rmsea_hypothesis_tests(list(fit = two_factor_mlr_fit, estimator = "MLR", rmsea_ci = .90))
information_criteria_mlr <- structural_canvas_information_criteria(list(fit = two_factor_mlr_fit))
information_criteria_comparison <- structural_canvas_information_criteria(list(
  fit = two_factor_mlr_fit, baseline_fit = two_factor_mlr_fit,
  modified_from_baseline = TRUE, comparison_label = "Modified model"
))
information_criteria_noncomparable <- structural_canvas_information_criteria(list(
  fit = two_factor_mlr_fit, baseline_fit = ml$fit,
  modified_from_baseline = TRUE, comparison_label = "Different-data model"
))
information_criteria_different_observations <- structural_canvas_information_criteria(list(
  fit = perturbed_two_factor_fit, baseline_fit = two_factor_fit,
  modified_from_baseline = TRUE, comparison_label = "Different observations"
))
mlr_fit_measures <- lavaan::fitMeasures(two_factor_mlr_fit)
stopifnot(
  identical(rmsea_tests_ml$Source[[1L]], "rmsea"),
  identical(rmsea_tests_mlr$Source[[1L]], "rmsea.robust"),
  abs(rmsea_tests_mlr$`Close-fit p`[[1L]] - mlr_fit_measures[["rmsea.pvalue.robust"]]) < 1e-12,
  abs(rmsea_tests_mlr$`Not-close p`[[1L]] - mlr_fit_measures[["rmsea.notclose.pvalue.robust"]]) < 1e-12,
  abs(information_criteria_mlr$AIC[[1L]] - mlr_fit_measures[["aic"]]) < 1e-12,
  abs(information_criteria_mlr$BIC[[1L]] - mlr_fit_measures[["bic"]]) < 1e-12,
  is.na(information_criteria_mlr$`Delta AIC`[[1L]]),
  identical(information_criteria_mlr$`Comparison status`[[1L]], "Single model; delta not applicable"),
  identical(information_criteria_comparison$Model, c("Original model", "Modified model")),
  all(information_criteria_comparison$`Comparison status` == "Comparable on observations, variables, estimator family, and admissibility"),
  all(information_criteria_comparison$`Delta AIC` == 0),
  all(information_criteria_comparison$`Delta BIC` == 0),
  all(information_criteria_noncomparable$`Comparison status` == "Not comparable; delta suppressed"),
  all(is.na(information_criteria_noncomparable$`Delta AIC`)),
  all(is.na(information_criteria_noncomparable$`Delta BIC`)),
  all(information_criteria_different_observations$`Comparison status` == "Not comparable; delta suppressed"),
  all(is.na(information_criteria_different_observations$`Delta AIC`))
)
latent_correlation_ci <- structural_canvas_latent_correlation_intervals(two_factor_fit)
numeric_parameter_table <- structural_canvas_export_parameter_estimates(two_factor_fit)
two_factor_correlation_export <- structural_canvas_export_latent_correlations(two_factor_fit)
two_factor_reliability_export <- structural_canvas_export_reliability_validity(list(
  fit = two_factor_fit, snapshot = list(), validity_formula = "standardized"
))
two_factor_sample_statistics <- structural_canvas_export_sample_statistics(two_factor_fit)
stopifnot(
  nrow(latent_correlation_ci) == 1L,
  identical(latent_correlation_ci$Type[[1L]], "Estimated"),
  is.finite(latent_correlation_ci$r[[1L]]),
  is.finite(latent_correlation_ci$`CI lower`[[1L]]),
  is.finite(latent_correlation_ci$`CI upper`[[1L]]),
  latent_correlation_ci$`CI lower`[[1L]] <= latent_correlation_ci$r[[1L]],
  latent_correlation_ci$r[[1L]] <= latent_correlation_ci$`CI upper`[[1L]],
  is.numeric(numeric_parameter_table$est),
  is.numeric(numeric_parameter_table$se),
  is.numeric(numeric_parameter_table$std.all),
  is.logical(numeric_parameter_table$Fixed),
  any(numeric_parameter_table$op == "=~" & numeric_parameter_table$Fixed),
  !any(grepl("Fixed", numeric_parameter_table$se, fixed = TRUE)),
  identical(two_factor_correlation_export$Factor, c("eta1", "eta2")),
  all(vapply(two_factor_correlation_export[c("eta1", "eta2")], is.numeric, logical(1))),
  max(abs(as.matrix(two_factor_correlation_export[c("eta1", "eta2")]) -
    stats::cov2cor(as.matrix(lavaan::lavInspect(two_factor_fit, "cov.lv"))))) < 1e-12,
  nrow(two_factor_reliability_export) == 2L,
  identical(two_factor_reliability_export$k, c(3L, 3L)),
  all(vapply(two_factor_reliability_export[c("AVE", "sqrt(AVE)", "CR", "Cronbach's alpha", "Omega total")], is.numeric, logical(1))),
  all(two_factor_reliability_export$`Fornell-Larcker assessed`),
  !any(two_factor_reliability_export$`Contains cross-loaded indicator`),
  nrow(two_factor_sample_statistics$Descriptives) == 6L,
  nrow(two_factor_sample_statistics$Covariance) == 36L,
  !nrow(two_factor_sample_statistics$Thresholds),
  all(vapply(two_factor_sample_statistics$Descriptives[c("Mean", "Variance", "SD", "Model N")], is.numeric, logical(1))),
  all(vapply(two_factor_sample_statistics$Covariance[c("Covariance", "Correlation")], is.numeric, logical(1)))
)
ml_measures <- structural_canvas_fit_measures(ml$fit, "ML", .90)
stopifnot(identical(ml_measures$labels[[5L]], "CFI"), !isTRUE(ml_measures$adjusted))
stopifnot(is.na(ml_measures$values[[4L]]))
numeric_fit_table <- structural_canvas_export_fit_estimates(list(
  fit = two_factor_fit, estimator = "ML", rmsea_ci = .90
))
admissibility_export <- structural_canvas_export_admissibility(list(fit = two_factor_fit))
set.seed(86420L)
bollen_seed_before <- .Random.seed
bollen_stine_test <- structural_canvas_bollen_stine(two_factor_fit, reps = 20L, seed = 97531L)
bollen_stine_eligible <- structural_canvas_bollen_stine_eligibility(two_factor_fit)
stopifnot(
  nrow(numeric_fit_table) == 1L,
  identical(numeric_fit_table$Model[[1L]], "Fitted model"),
  is.numeric(numeric_fit_table$`Chi-square`),
  is.numeric(numeric_fit_table$p),
  is.numeric(numeric_fit_table$RMSEA),
  identical(numeric_fit_table$`RMSEA CI level`[[1L]], .90),
  identical(numeric_fit_table$`CFI source`[[1L]], "cfi"),
  nrow(admissibility_export) == 1L,
  isTRUE(admissibility_export$Admissible[[1L]]),
  identical(admissibility_export$Reasons[[1L]], "None"),
  all(vapply(admissibility_export[c("Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue", "Residual condition number", "Latent condition number", "Parameter condition number")], is.numeric, logical(1))),
  identical(.Random.seed, bollen_seed_before),
  isTRUE(bollen_stine_eligible$available),
  nrow(bollen_stine_test) == 1L,
  is.finite(bollen_stine_test$`Observed chi-square`[[1L]]),
  bollen_stine_test$`Bootstrap p`[[1L]] > 0,
  bollen_stine_test$`Bootstrap p`[[1L]] <= 1,
  is.finite(bollen_stine_test$`Monte Carlo SE`[[1L]]),
  bollen_stine_test$`Monte Carlo SE`[[1L]] >= 0,
  bollen_stine_test$`Monte Carlo 95% lower`[[1L]] >= 0,
  bollen_stine_test$`Monte Carlo 95% lower`[[1L]] <= bollen_stine_test$`Bootstrap p`[[1L]],
  bollen_stine_test$`Bootstrap p`[[1L]] <= bollen_stine_test$`Monte Carlo 95% upper`[[1L]],
  bollen_stine_test$`Monte Carlo 95% upper`[[1L]] <= 1,
  bollen_stine_test$`Valid replicates`[[1L]] > 0L,
  identical(bollen_stine_test$`Requested replicates`[[1L]], 20L)
)
stopifnot(
  grepl("structural_canvas_fit_admissibility(candidate)$admissible", ui_source, fixed = TRUE),
  grepl("admissibility <- structural_canvas_fit_admissibility(fit)", ui_source, fixed = TRUE)
)
saturated_bollen_eligibility <- structural_canvas_bollen_stine_eligibility(ml$fit)
stopifnot(
  !isTRUE(saturated_bollen_eligibility$available),
  grepl("df = 0", saturated_bollen_eligibility$reason, fixed = TRUE),
  inherits(try(structural_canvas_bollen_stine(ml$fit, reps = 2L), silent = TRUE), "try-error")
)
loading_table <- lavaan::parameterTable(ml$fit)
loading_table <- loading_table[loading_table$op == "=~", , drop = FALSE]
stopifnot(loading_table$free[[1L]] == 0L, loading_table$free[[2L]] > 0L)
fixed_value <- stats::var(continuous$x1) * .001
constrained <- suppressWarnings(run_structural_canvas_analysis(
  snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = fixed_value)
))
stopifnot(
  grepl("x1 ~~", constrained$syntax, fixed = TRUE), grepl("*x1", constrained$syntax, fixed = TRUE),
  identical(constrained$admissible, structural_canvas_fit_admissibility(constrained$fit)$admissible)
)
negative_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = -.001))
  ""
}, error = conditionMessage)
unknown_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", residual_variance_fixes = c(not_in_data = .001))
  ""
}, error = conditionMessage)
excessive_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = stats::var(continuous$x1)))
  ""
}, error = conditionMessage)
conflicting_residual_snapshot <- snapshot
conflicting_residual_snapshot$nodes <- c(conflicting_residual_snapshot$nodes, list(list(id = "e_conflict", role = "error", name = "e_conflict")))
conflicting_residual_snapshot$edges <- c(conflicting_residual_snapshot$edges, list(list(from = "e_conflict", to = "indicator_1", free = FALSE, fixedValue = .10)))
conflicting_sensitivity_error <- tryCatch({
  run_structural_canvas_analysis(conflicting_residual_snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = fixed_value))
  ""
}, error = conditionMessage)
stopifnot(
  grepl("finite and greater than zero", negative_sensitivity_error, fixed = TRUE),
  grepl("not found in the data", unknown_sensitivity_error, fixed = TRUE),
  grepl("smaller than its indicator's observed variance", excessive_sensitivity_error, fixed = TRUE),
  grepl("conflict with existing canvas residual constraints", conflicting_sensitivity_error, fixed = TRUE)
)
common_ml_measures <- structural_canvas_common_fit_measures(list(ml$fit, constrained$fit), "ML", .90)
stopifnot(length(common_ml_measures) == 2L, identical(common_ml_measures[[1L]]$keys, common_ml_measures[[2L]]$keys))
model_difference <- structural_canvas_model_difference(ml$fit, constrained$fit)
mi_significance_candidates <- structural_canvas_allowed_mi(list(nodes = list(), edges = list()), two_factor_fit, mode = "conventional")
two_factor_admissibility <- structural_canvas_fit_admissibility(two_factor_fit)
mi_refit_nodes <- list(
  list(id = "mf1", role = "latent", name = "eta1"),
  list(id = "mf2", role = "latent", name = "eta2")
)
mi_refit_edges <- list(list(from = "mf1", to = "mf2", kind = "covariance"))
for (index in 1:6) {
  indicator_id <- paste0("mx", index)
  mi_refit_nodes <- c(mi_refit_nodes, list(list(id = indicator_id, role = "indicator", name = paste0("x", index))))
  mi_refit_edges <- c(mi_refit_edges, list(list(from = if (index <= 3L) "mf1" else "mf2", to = indicator_id)))
}
mi_refit_snapshot <- list(nodes = mi_refit_nodes, edges = mi_refit_edges)
sequential_mi_candidates <- suppressWarnings(structural_canvas_mi_refits(
  mi_refit_snapshot,
  list(fit = two_factor_fit, syntax = "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6\neta1 ~~ eta2"),
  lavaan::HolzingerSwineford1939, "cfa", "ML", "listwise", FALSE, mode = "theory"
))
stopifnot(
  isTRUE(two_factor_admissibility$admissible),
  !length(two_factor_admissibility$reasons),
  nrow(mi_significance_candidates) > 0L,
  all(c("MI p", "BH-adjusted p", "Multiplicity family size") %in% names(mi_significance_candidates)),
  all(c("epc", "sepc.all") %in% names(mi_significance_candidates)),
  all(is.finite(mi_significance_candidates$epc)),
  all(is.finite(mi_significance_candidates$sepc.all)),
  length(unique(mi_significance_candidates$`Multiplicity family size`)) == 1L,
  unique(mi_significance_candidates$`Multiplicity family size`) >= nrow(mi_significance_candidates),
  all(is.finite(mi_significance_candidates$`MI p`)),
  all(mi_significance_candidates$`MI p` >= 0 & mi_significance_candidates$`MI p` <= 1),
  all(mi_significance_candidates$`BH-adjusted p` >= mi_significance_candidates$`MI p` - 1e-15),
  max(abs(mi_significance_candidates$`MI p` - stats::pchisq(mi_significance_candidates$mi, 1L, lower.tail = FALSE))) < 1e-12
)
stopifnot(
  nrow(sequential_mi_candidates) > 0L,
  all(c("step", "skipped_inadmissible", "skipped_details") %in% names(sequential_mi_candidates)),
  identical(sequential_mi_candidates$step, seq_len(nrow(sequential_mi_candidates))),
  all(sequential_mi_candidates$skipped_inadmissible >= 0L),
  all(nzchar(sequential_mi_candidates$skipped_details) == (sequential_mi_candidates$skipped_inadmissible > 0L))
)
stopifnot(
  grepl("candidate_row$step <- step", ui_source, fixed = TRUE),
  grepl("candidate_row$skipped_inadmissible <- skipped_candidates", ui_source, fixed = TRUE),
  grepl("candidate_row$skipped_details <- paste(skipped_details, collapse = \" | \")", ui_source, fixed = TRUE),
  grepl("table <- data.frame(Step = step, `Skipped unsafe` = skipped, `Skipped details` = skipped_details, relation", ui_source, fixed = TRUE),
  grepl("Each Step is sequential", ui_source, fixed = TRUE)
)
nested_difference_eligibility <- structural_canvas_nested_comparison_eligibility(ml$fit, constrained$fit)
nested_difference_report <- structural_canvas_model_difference_report(list(
  baseline_fit = ml$fit, fit = constrained$fit, comparison_type = "mi"
))
stopifnot(
  isTRUE(nested_difference_eligibility$available),
  !is.null(model_difference), is.finite(model_difference$df),
  isTRUE(nested_difference_report$Available[[1L]]),
  is.finite(nested_difference_report$`Delta chi-square`[[1L]]),
  identical(nested_difference_report$Context[[1L]], "Exploratory same-sample MI modification")
)

second_latent <- list(id = "latent_2", role = "latent", name = "eta2", x = 700, y = 200)
two_factor_snapshot <- snapshot
two_factor_snapshot$nodes <- c(two_factor_snapshot$nodes, list(second_latent))
stopifnot(identical(structural_canvas_missing_exogenous_covariances(two_factor_snapshot), "eta1 ↔ eta2"))
two_factor_snapshot$edges <- c(two_factor_snapshot$edges, list(list(from = "latent_1", to = "latent_2", kind = "covariance")))
stopifnot(length(structural_canvas_missing_exogenous_covariances(two_factor_snapshot)) == 0L)
cross_loading_snapshot <- two_factor_snapshot
cross_loading_snapshot$edges <- c(cross_loading_snapshot$edges, list(list(from = "latent_2", to = "indicator_1")))
cross_loading_diagnostics <- structural_canvas_identification_diagnostics(cross_loading_snapshot)
stopifnot(any(cross_loading_diagnostics$Code == "cross_loading" & cross_loading_diagnostics$Element == "x1" & cross_loading_diagnostics$Severity == "Warning"))

set.seed(20260813)
second_order_score <- stats::rnorm(n)
first_scores <- lapply(c(.8, .7, .9), function(loading) loading * second_order_score + stats::rnorm(n, sd = sqrt(1 - loading^2)))
second_order_data <- data.frame(
  a1 = .8 * first_scores[[1L]] + stats::rnorm(n, sd = .6), a2 = .7 * first_scores[[1L]] + stats::rnorm(n, sd = .7), a3 = .9 * first_scores[[1L]] + stats::rnorm(n, sd = .5),
  b1 = .8 * first_scores[[2L]] + stats::rnorm(n, sd = .6), b2 = .7 * first_scores[[2L]] + stats::rnorm(n, sd = .7), b3 = .9 * first_scores[[2L]] + stats::rnorm(n, sd = .5),
  c1 = .8 * first_scores[[3L]] + stats::rnorm(n, sd = .6), c2 = .7 * first_scores[[3L]] + stats::rnorm(n, sd = .7), c3 = .9 * first_scores[[3L]] + stats::rnorm(n, sd = .5)
)
second_order_nodes <- list(list(id = "G", role = "latent", name = "G"))
second_order_edges <- list()
for (factor_index in seq_len(3L)) {
  factor_id <- paste0("F", factor_index)
  second_order_nodes <- c(second_order_nodes, list(list(id = factor_id, role = "latent", name = factor_id)))
  second_order_edges <- c(second_order_edges, list(list(from = "G", to = factor_id, pathType = "higherOrder")))
  for (item_index in seq_len(3L)) {
    indicator_name <- paste0(letters[[factor_index]], item_index)
    indicator_id <- paste0("i_", indicator_name)
    second_order_nodes <- c(second_order_nodes, list(list(id = indicator_id, role = "indicator", name = indicator_name)))
    second_order_edges <- c(second_order_edges, list(list(from = factor_id, to = indicator_id)))
  }
}
second_order_snapshot <- list(nodes = second_order_nodes, edges = second_order_edges)
second_order_fit <- run_structural_canvas_analysis(second_order_snapshot, second_order_data, "cfa")
stopifnot(isTRUE(second_order_fit$converged), !length(second_order_fit$negative_latent_variances), grepl("G =~ F1 + F2 + F3", second_order_fit$syntax, fixed = TRUE), !grepl("F1 ~ G", second_order_fit$syntax, fixed = TRUE))
second_order_results <- structural_canvas_higher_order_results(second_order_snapshot, second_order_fit$fit)
stopifnot(
  isTRUE(second_order_results$available), nrow(second_order_results$table) == 3L,
  all(is.finite(second_order_results$table$Beta)),
  all(is.finite(second_order_results$table$BCILower)),
  all(is.finite(second_order_results$table$BCIUpper)),
  all(second_order_results$table$BCILower <= second_order_results$table$B),
  all(second_order_results$table$B <= second_order_results$table$BCIUpper),
  all(is.finite(second_order_results$table$BetaCILower)),
  all(is.finite(second_order_results$table$BetaCIUpper)),
  all(second_order_results$table$BetaCILower <= second_order_results$table$Beta),
  all(second_order_results$table$Beta <= second_order_results$table$BetaCIUpper),
  all(is.finite(second_order_results$table$R2)),
  all(is.finite(second_order_results$table$R2CILower)),
  all(is.finite(second_order_results$table$R2CIUpper)),
  all(second_order_results$table$R2CILower <= second_order_results$table$R2),
  all(second_order_results$table$R2 <= second_order_results$table$R2CIUpper)
)
omega_h <- structural_canvas_omega_h(second_order_snapshot, second_order_fit$fit)
stopifnot(isTRUE(omega_h$available), is.finite(omega_h$omega_h), omega_h$omega_h > 0, omega_h$omega_h <= 1, omega_h$indicators == 9L)
stopifnot(
  identical(structural_canvas_higher_order_loading_guidance(.30), "Weak loading review"),
  identical(structural_canvas_higher_order_loading_guidance(-.60), "No loading flag"),
  identical(structural_canvas_omega_h_guidance(.65), "Below common .70 guideline"),
  identical(structural_canvas_omega_h_guidance(.80), "Meets common .70 guideline"),
  identical(structural_canvas_omega_h_guidance(1.05), "Review inadmissible coefficient")
)
second_order_identification <- structural_canvas_identification_diagnostics(second_order_snapshot)
stopifnot(!any(second_order_identification$Severity == "Error"))

single_indicator_snapshot <- list(
  nodes = list(list(id = "F", role = "latent", name = "F"), list(id = "x", role = "indicator", name = "x")),
  edges = list(list(from = "F", to = "x"))
)
single_indicator_diagnostics <- structural_canvas_identification_diagnostics(single_indicator_snapshot)
stopifnot(any(single_indicator_diagnostics$Code == "single_indicator" & single_indicator_diagnostics$Severity == "Error"))
two_indicator_snapshot <- single_indicator_snapshot
two_indicator_snapshot$nodes <- c(two_indicator_snapshot$nodes, list(list(id = "y", role = "indicator", name = "y")))
two_indicator_snapshot$edges <- c(two_indicator_snapshot$edges, list(list(from = "F", to = "y")))
two_indicator_diagnostics <- structural_canvas_identification_diagnostics(two_indicator_snapshot)
stopifnot(any(two_indicator_diagnostics$Code == "two_indicators" & two_indicator_diagnostics$Severity == "Warning"), !any(two_indicator_diagnostics$Severity == "Error"))
duplicate_snapshot <- snapshot
duplicate_snapshot$edges <- c(duplicate_snapshot$edges, list(duplicate_snapshot$edges[[1L]]))
stopifnot(any(structural_canvas_identification_diagnostics(duplicate_snapshot)$Code == "duplicate_path"))

stopifnot(identical(structural_canvas_parameter_term(list(free = FALSE, fixedValue = .8), "x1"), "0.8*x1"))
stopifnot(identical(structural_canvas_parameter_term(list(startValue = .7, parameterName = "loading2"), "x2"), "start(0.7)*loading2*x2"))
invalid_label_error <- tryCatch({
  structural_canvas_parameter_term(list(parameterName = "2bad label"), "x1")
  ""
}, error = conditionMessage)
stopifnot(grepl("Invalid lavaan parameter label", invalid_label_error, fixed = TRUE))

modified_snapshot <- snapshot
modified_snapshot$edges[[2L]]$free <- FALSE
modified_snapshot$edges[[2L]]$fixedValue <- .75
modified_snapshot$edges[[3L]]$startValue <- .9
modified_snapshot$edges[[3L]]$parameterName <- "lambda3"
modified_fit <- run_structural_canvas_analysis(modified_snapshot, continuous, "cfa")
stopifnot(grepl("0.75*x2", modified_fit$syntax, fixed = TRUE), grepl("start(0.9)*lambda3*x3", modified_fit$syntax, fixed = TRUE))

single_constrained_snapshot <- single_indicator_snapshot
single_constrained_snapshot$nodes <- c(single_constrained_snapshot$nodes, list(list(id = "e", role = "error", name = "e")))
single_constrained_snapshot$edges <- c(single_constrained_snapshot$edges, list(list(from = "e", to = "x", free = FALSE, fixedValue = .20)))
single_constrained_diagnostics <- structural_canvas_identification_diagnostics(single_constrained_snapshot)
stopifnot(!any(single_constrained_diagnostics$Severity == "Error"), any(single_constrained_diagnostics$Code == "single_indicator_constrained"))
stopifnot(
  length(structural_canvas_constrained_single_indicators(single_indicator_snapshot)) == 0L,
  identical(structural_canvas_constrained_single_indicators(single_constrained_snapshot), "F")
)
negative_residual_snapshot <- single_constrained_snapshot
negative_residual_snapshot$edges[[2L]]$fixedValue <- -.10
negative_residual_diagnostics <- structural_canvas_identification_diagnostics(negative_residual_snapshot)
stopifnot(any(negative_residual_diagnostics$Code == "negative_fixed_residual" & negative_residual_diagnostics$Severity == "Error"))
zero_residual_snapshot <- single_constrained_snapshot
zero_residual_snapshot$edges[[2L]]$fixedValue <- 0
zero_residual_diagnostics <- structural_canvas_identification_diagnostics(zero_residual_snapshot)
stopifnot(
  any(zero_residual_diagnostics$Code == "boundary_fixed_residual" & zero_residual_diagnostics$Severity == "Warning"),
  any(zero_residual_diagnostics$Code == "single_indicator" & zero_residual_diagnostics$Severity == "Error"),
  length(structural_canvas_constrained_single_indicators(zero_residual_snapshot)) == 0L
)
single_constrained_data <- data.frame(x = stats::rnorm(300))
residual_scale_ok <- structural_canvas_fixed_residual_scale_diagnostics(single_constrained_snapshot, single_constrained_data)
stopifnot(
  nrow(residual_scale_ok) == 1L,
  isTRUE(residual_scale_ok[["Single-indicator factor"]][[1L]]),
  identical(residual_scale_ok$Status[[1L]], "Within observed variance")
)
excessive_residual_snapshot <- single_constrained_snapshot
excessive_residual_snapshot$edges[[2L]]$fixedValue <- 2 * stats::var(single_constrained_data$x)
residual_scale_bad <- structural_canvas_fixed_residual_scale_diagnostics(excessive_residual_snapshot, single_constrained_data)
stopifnot(nrow(residual_scale_bad) == 1L, identical(residual_scale_bad$Status[[1L]], "Exceeds observed variance"))
multi_indicator_residual_snapshot <- snapshot
multi_indicator_residual_snapshot$nodes <- c(multi_indicator_residual_snapshot$nodes, list(list(id = "e_multi", role = "error", name = "e_multi")))
multi_indicator_residual_snapshot$edges <- c(multi_indicator_residual_snapshot$edges, list(list(from = "e_multi", to = "indicator_1", free = FALSE, fixedValue = 2 * stats::var(continuous$x1))))
multi_indicator_scale <- structural_canvas_fixed_residual_scale_diagnostics(multi_indicator_residual_snapshot, continuous)
stopifnot(
  nrow(multi_indicator_scale) == 1L,
  identical(multi_indicator_scale$Status[[1L]], "Exceeds observed variance"),
  identical(multi_indicator_scale[["Single-indicator factor"]][[1L]], FALSE)
)
single_constrained_fit <- run_structural_canvas_analysis(single_constrained_snapshot, single_constrained_data, "cfa")
stopifnot(grepl("x ~~ 0.2*x", single_constrained_fit$syntax, fixed = TRUE), isTRUE(single_constrained_fit$converged))
negative_residual_error <- tryCatch({
  run_structural_canvas_analysis(negative_residual_snapshot, single_constrained_data, "cfa")
  ""
}, error = conditionMessage)
stopifnot(grepl("cannot be fixed to a negative value", negative_residual_error, fixed = TRUE))
excessive_residual_error <- tryCatch({
  run_structural_canvas_analysis(excessive_residual_snapshot, single_constrained_data, "cfa")
  ""
}, error = conditionMessage)
stopifnot(grepl("must be smaller than the observed variance", excessive_residual_error, fixed = TRUE))

fl <- structural_canvas_fornell_larcker(
  ave = c(eta1 = .50, eta2 = .64),
  correlations = matrix(c(1, .60, .60, 1), 2L, dimnames = list(c("eta1", "eta2"), c("eta1", "eta2"))),
  indicator_counts = c(eta1 = 3L, eta2 = 3L)
)
stopifnot(all.equal(unname(fl$max_correlation), c(.60, .60)), identical(unname(fl$criterion), c("Criterion met", "Criterion met")))
fl_review <- structural_canvas_fornell_larcker(
  ave = c(eta1 = .25, eta2 = .64),
  correlations = matrix(c(1, .60, .60, 1), 2L, dimnames = list(c("eta1", "eta2"), c("eta1", "eta2"))),
  indicator_counts = c(eta1 = 3L, eta2 = 1L)
)
stopifnot(identical(fl_review$criterion[["eta1"]], "Review needed"), identical(fl_review$criterion[["eta2"]], "Not assessed"))
fl_orthogonal <- structural_canvas_fornell_larcker(c(eta1 = .5, eta2 = .5), diag(2L), assessable = FALSE)
stopifnot(all(fl_orthogonal$criterion == "Not assessed"))

htmt_correlations <- matrix(c(
  1, .50, .20, .20,
  .50, 1, .20, .20,
  .20, .20, 1, .40,
  .20, .20, .40, 1
), 4L, byrow = TRUE, dimnames = list(c("x1", "x2", "y1", "y2"), c("x1", "x2", "y1", "y2")))
htmt <- structural_canvas_htmt(htmt_correlations, list(eta1 = c("x1", "x2"), eta2 = c("y1", "y2")), .85)
stopifnot(abs(htmt$matrix["eta1", "eta2"] - (.20 / sqrt(.50 * .40))) < 1e-10, identical(htmt$pairs$Criterion[[1L]], "Criterion met"))
htmt_review <- structural_canvas_htmt(htmt_correlations, list(eta1 = c("x1", "x2"), eta2 = c("x2", "y2")), .85)
stopifnot(identical(htmt_review$pairs$Criterion[[1L]], "Not assessed"), grepl("Cross-loaded", htmt_review$pairs$Reason[[1L]], fixed = TRUE))

set.seed(77)
htmt_boot_data <- data.frame(
  x1 = stats::rnorm(180), x2 = stats::rnorm(180),
  y1 = stats::rnorm(180), y2 = stats::rnorm(180)
)
seed_before_htmt_boot <- .Random.seed
htmt_progress_events <- list()
htmt_boot <- structural_canvas_htmt_bootstrap(
  htmt_boot_data, list(eta1 = c("x1", "x2"), eta2 = c("y1", "y2")),
  reps = 100L, confidence = .95, seed = 42L,
  progress = function(done, total, valid) {
    htmt_progress_events[[length(htmt_progress_events) + 1L]] <<- c(done = done, total = total, valid = valid)
  }
)
htmt_cancel_error <- tryCatch({
  structural_canvas_htmt_bootstrap(
    htmt_boot_data, list(eta1 = c("x1", "x2"), eta2 = c("y1", "y2")),
    reps = 2L, confidence = .95, seed = 42L, cancel = function() TRUE
  )
  ""
}, error = conditionMessage)
stopifnot(
  nrow(htmt_boot) == 1L,
  all(c("Lower", "Upper", "One-sided upper", "Upper < threshold", "Upper < 1", "CI method", "Valid replicates", "Requested replicates", "Valid %", "Status") %in% names(htmt_boot)),
  htmt_boot$`Valid replicates`[[1L]] == 100L,
  htmt_boot$`Requested replicates`[[1L]] == 100L, htmt_boot$`Valid %`[[1L]] == 100,
  identical(htmt_boot[["CI method"]][[1L]], "Percentile"),
  identical(htmt_boot$Status[[1L]], "Adequate"),
  is.finite(htmt_boot$Lower[[1L]]), htmt_boot$Lower[[1L]] <= htmt_boot$`One-sided upper`[[1L]],
  htmt_boot$`One-sided upper`[[1L]] <= htmt_boot$Upper[[1L]],
  identical(.Random.seed, seed_before_htmt_boot),
  length(htmt_progress_events) >= 2L,
  htmt_progress_events[[1L]][["done"]] == 0L,
  tail(htmt_progress_events, 1L)[[1L]][["done"]] == 100L,
  tail(htmt_progress_events, 1L)[[1L]][["valid"]] == 100L,
  grepl("canceled", htmt_cancel_error, fixed = TRUE),
  grepl("_htmt_ci_method", ui_source, fixed = TRUE)
)
htmt_bca <- structural_canvas_htmt_bootstrap(
  htmt_boot_data, list(eta1 = c("x1", "x2"), eta2 = c("y1", "y2")),
  reps = 25L, confidence = .95, seed = 44L, ci_method = "bca"
)
stopifnot(
  nrow(htmt_bca) == 1L,
  htmt_bca[["CI method"]][[1L]] %in% c("BCa", "BCa unavailable"),
  htmt_bca$`Valid replicates`[[1L]] == 25L
)

ordinal_boot_data <- as.data.frame(lapply(htmt_boot_data, function(values) {
  ordered(cut(values, breaks = c(-Inf, -.5, .5, Inf), labels = c("low", "mid", "high")))
}))
htmt_ordinal_boot <- structural_canvas_htmt_bootstrap(
  ordinal_boot_data, list(eta1 = c("x1", "x2"), eta2 = c("y1", "y2")),
  reps = 30L, confidence = .95, seed = 43L, ordered = names(ordinal_boot_data)
)
stopifnot(
  nrow(htmt_ordinal_boot) == 1L,
  htmt_ordinal_boot$`Valid replicates`[[1L]] >= 20L,
  htmt_ordinal_boot$Status[[1L]] %in% c("Adequate", "Caution"),
  is.finite(htmt_ordinal_boot$Lower[[1L]]),
  htmt_ordinal_boot$Lower[[1L]] <= htmt_ordinal_boot$Upper[[1L]]
)

mardia_normal <- structural_canvas_mardia(continuous, names(continuous))
stopifnot(isTRUE(mardia_normal$available), mardia_normal$n == nrow(continuous), is.finite(mardia_normal$skewness), is.finite(mardia_normal$kurtosis))
set.seed(20260814)
nonnormal_data <- data.frame(x1 = stats::rexp(800), x2 = stats::rexp(800), x3 = stats::rexp(800))
mardia_nonnormal <- structural_canvas_mardia(nonnormal_data, names(nonnormal_data))
stopifnot(isTRUE(mardia_nonnormal$available), isTRUE(mardia_nonnormal$nonnormal), identical(mardia_nonnormal$recommendation, "MLR recommended"))
estimator_recommendation <- structural_canvas_estimator_recommendation(snapshot, nonnormal_data, data.frame(name = names(nonnormal_data), measurement = "continuous"), "cfa", "ML")
estimator_no_recommendation <- structural_canvas_estimator_recommendation(snapshot, nonnormal_data, data.frame(name = names(nonnormal_data), measurement = "continuous"), "cfa", "MLR")
estimator_ordered_recommendation <- structural_canvas_estimator_recommendation(snapshot, nonnormal_data, data.frame(name = names(nonnormal_data), measurement = c("ordered", "continuous", "continuous")), "cfa", "ML")
stopifnot(
  isTRUE(estimator_recommendation$recommend),
  identical(estimator_recommendation$recommended_estimator, "MLR"),
  !isTRUE(estimator_no_recommendation$recommend),
  !isTRUE(estimator_ordered_recommendation$recommend),
  grepl("Estimator recommendation", ui_source, fixed = TRUE),
  grepl("_run_with_mlr", ui_source, fixed = TRUE),
  grepl("_run_with_ml", ui_source, fixed = TRUE)
)
singular_data <- data.frame(x1 = 1:20, x2 = 1:20)
mardia_singular <- structural_canvas_mardia(singular_data, names(singular_data))
stopifnot(!isTRUE(mardia_singular$available), grepl("singular", mardia_singular$reason, fixed = TRUE))

alpha_covariance <- matrix(c(1, .5, .5, 1), 2L, dimnames = list(c("x1", "x2"), c("x1", "x2")))
stopifnot(abs(structural_canvas_cronbach_alpha(alpha_covariance, c("x1", "x2")) - 2 / 3) < 1e-10)
stopifnot(is.na(structural_canvas_cronbach_alpha(alpha_covariance, "x1")))
residual_diagnostics <- structural_canvas_residual_diagnostics(ml$fit)
stopifnot(isTRUE(residual_diagnostics$available), identical(dim(residual_diagnostics$standardized), c(3L, 3L)), all(is.na(residual_diagnostics$standardized[upper.tri(residual_diagnostics$standardized, diag = TRUE)])))

factor_correlation_diagnostics <- structural_canvas_factor_correlation_diagnostics(second_order_fit$fit)
stopifnot(nrow(factor_correlation_diagnostics) > 0L, all(factor_correlation_diagnostics$Severity %in% c("Acceptable", "Review", "High", "Severe", "Inadmissible", "Unavailable")))
ordered_category_data <- data.frame(x1 = ordered(c("A", rep("B", 98), "C"), levels = c("A", "B", "C", "D")))
category_diagnostics <- structural_canvas_ordered_category_diagnostics(ordered_category_data, "x1")
stopifnot(identical(as.character(category_diagnostics$Status), c("Sparse", "Dominant (>=95%)", "Sparse", "Empty")))
error_snapshot <- snapshot
error_snapshot$nodes <- c(error_snapshot$nodes, list(list(id = "e1", role = "error"), list(id = "e2", role = "error")))
error_snapshot$edges <- c(error_snapshot$edges, list(list(from = "e1", to = "e2", kind = "covariance")))
error_diagnostics <- structural_canvas_error_covariance_diagnostics(error_snapshot)
stopifnot(error_diagnostics$count == 1L, error_diagnostics$possible == 3L, identical(error_diagnostics$status, "Review complexity"))

mi_fixture <- data.frame(
  lhs = c("x1", "x2"), op = c("~~", "~~"), rhs = c("x2", "x1"),
  mi = c(12, 12), epc = c(.20, .20), cfi_after = c(.95, .95),
  tli_after = c(.94, .94), rmsea_after = c(.05, .05), srmr_after = c(.04, .04),
  stringsAsFactors = FALSE
)
mi_history <- structural_canvas_mi_history_rows(mi_fixture, 1:2, justification = "Shared wording")
stopifnot(nrow(mi_history) == 1L, identical(mi_history$Step, 1L), identical(mi_history$Justification, "Shared wording"))
mi_history_again <- structural_canvas_mi_history_rows(mi_fixture, 1L, existing = mi_history, justification = "Duplicate")
stopifnot(nrow(mi_history_again) == 1L)

missing_fixture <- continuous
missing_fixture$x1[c(1L, 2L)] <- NA_real_
missing_fixture$x2[[2L]] <- NA_real_
missing_diagnostics <- structural_canvas_missing_diagnostics(missing_fixture, names(missing_fixture))
stopifnot(isTRUE(missing_diagnostics$available), missing_diagnostics$complete_n == nrow(continuous) - 2L, missing_diagnostics$variables$Missing[missing_diagnostics$variables$Variable == "x1"] == 2L, missing_diagnostics$pattern_count == 3L)
outlier_fixture <- continuous
outlier_fixture[1L, ] <- outlier_fixture[1L, ] + 20
outlier_diagnostics <- structural_canvas_mahalanobis_diagnostics(outlier_fixture, names(outlier_fixture), alpha = .001)
stopifnot(isTRUE(outlier_diagnostics$available), outlier_diagnostics$flagged_n >= 1L, 1L %in% outlier_diagnostics$table$Row)

fit_good <- structural_canvas_fit_guidance(c(10, 10, .4, 1, .96, .95, .07, .05, .03, .07))
stopifnot(identical(as.character(fit_good$Guidance), c("Good", "Good", "Good", "Good")))
fit_mixed <- structural_canvas_fit_guidance(c(20, 10, .01, 2, .92, .89, .09, .075, .05, .10))
stopifnot(identical(as.character(fit_mixed$Guidance), c("Marginal", "Review", "Marginal", "Marginal")))
fit_saturated <- structural_canvas_fit_guidance(c(0, 0, 1, NA, 1, 1, 0, 0, 0, 0))
stopifnot(all(fit_saturated$Guidance == "Not assessed"))

round_trip_snapshot <- list(
  modelSchemaVersion = 3L,
  nodes = list(list(id = "G", role = "latent", name = "G"), list(id = "F1", role = "latent", name = "F1")),
  edges = list(list(from = "G", to = "F1", pathType = "higherOrder", free = FALSE, fixedValue = .8, startValue = .7, parameterName = "gamma1", equalityLabel = "equal_general"))
)
round_trip_json <- jsonlite::toJSON(round_trip_snapshot, auto_unbox = TRUE, null = "null")
round_trip_restored <- jsonlite::fromJSON(round_trip_json, simplifyVector = FALSE)
restored_edge <- round_trip_restored$edges[[1L]]
stopifnot(identical(restored_edge$pathType, "higherOrder"), identical(restored_edge$free, FALSE), identical(restored_edge$fixedValue, .8), identical(restored_edge$startValue, .7), identical(restored_edge$parameterName, "gamma1"), identical(restored_edge$equalityLabel, "equal_general"))

variable_table <- data.frame(
  name = c("x1", "x2", "x3"),
  measurement = c("ordered", "ordinal", "continuous"),
  stringsAsFactors = FALSE
)
ordered_names <- structural_canvas_ordered_indicators(snapshot, variable_table)
stopifnot(identical(ordered_names, c("x1", "x2")))

nominal_table <- data.frame(
  name = c("x1", "x2", "x3"),
  measurement = c("category", "nominal", "factor"),
  stringsAsFactors = FALSE
)
stopifnot(length(structural_canvas_ordered_indicators(snapshot, nominal_table)) == 0L)
stopifnot(identical(structural_canvas_nominal_indicators(snapshot, nominal_table), c("x1", "x2", "x3")))
engine_nominal_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", nominal = "x1")
  ""
}, error = conditionMessage)
stopifnot(grepl("Nominal indicators are not supported", engine_nominal_error, fixed = TRUE))

ordinal <- as.data.frame(lapply(continuous, function(value) {
  as.integer(cut(value, breaks = stats::quantile(value, probs = seq(0, 1, .2)), include.lowest = TRUE))
}))
ordered_pair_diagnostics <- structural_canvas_ordered_pair_diagnostics(ordinal, ordered_names)
stopifnot(
  nrow(ordered_pair_diagnostics) == choose(length(ordered_names), 2L),
  all(ordered_pair_diagnostics[["Cells"]] == 25L),
  all(ordered_pair_diagnostics[["Valid pairs"]] == n)
)
sparse_ordinal <- data.frame(
  a = factor(c(rep(1L, 99L), 2L), levels = 1:3),
  b = factor(c(rep(1L, 98L), 2L, 3L), levels = 1:3)
)
sparse_categories <- structural_canvas_ordered_category_diagnostics(sparse_ordinal, c("a", "b"))
sparse_pairs <- structural_canvas_ordered_pair_diagnostics(sparse_ordinal, c("a", "b"))
stopifnot(
  any(sparse_categories$Status == "Empty"), any(sparse_categories$Status == "Sparse"),
  any(sparse_categories$Status == "Dominant (>=95%)"),
  sparse_pairs[["Empty cells"]] > 0L, sparse_pairs[["Sparse nonempty cells"]] > 0L,
  identical(sparse_pairs$Status, "Review")
)
wlsmv <- run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise", ordered = ordered_names)
wlsmv_theta_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3", data = ordinal, estimator = "WLSMV",
  missing = "pairwise", ordered = ordered_names, parameterization = "theta",
  auto.cov.lv.x = FALSE
)
theta_reliability_bootstrap <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", ordinal, reps = 5L, estimator = "WLSMV",
  missing = "pairwise", ordered = ordered_names, original_fit = wlsmv_theta_fit
)
stopifnot(
  identical(lavaan::lavInspect(wlsmv_theta_fit, "options")$parameterization, "theta"),
  nrow(theta_reliability_bootstrap) == 4L,
  identical(theta_reliability_bootstrap$Statistic, c("AVE", "CR", "Alpha", "Omega")),
  all(theta_reliability_bootstrap[["Requested replicates"]] == 5L)
)
wlsmv_sample_statistics <- structural_canvas_export_sample_statistics(wlsmv$fit)
stopifnot(
  nrow(wlsmv_sample_statistics$Descriptives) == length(lavaan::lavNames(wlsmv$fit, "ov")),
  nrow(wlsmv_sample_statistics$Thresholds) > 0L,
  is.numeric(wlsmv_sample_statistics$Thresholds$Threshold)
)
stopifnot(isTRUE(wlsmv$converged))
wlsmv_bollen_eligibility <- structural_canvas_bollen_stine_eligibility(wlsmv$fit)
stopifnot(
  !isTRUE(wlsmv_bollen_eligibility$available),
  grepl("ML estimation", wlsmv_bollen_eligibility$reason, fixed = TRUE),
  inherits(try(structural_canvas_bollen_stine(wlsmv$fit, reps = 2L), silent = TRUE), "try-error")
)
stopifnot(identical(sort(lavaan::lavNames(wlsmv$fit, "ov.ord")), sort(ordered_names)))
wlsmv_measures <- structural_canvas_fit_measures(wlsmv$fit, "WLSMV", .90)
stopifnot(isTRUE(wlsmv_measures$adjusted))
ordinal_invariance_data <- ordinal
ordinal_invariance_data$group <- rep(c("A", "B"), each = n / 2L)
ordinal_invariance <- structural_canvas_measurement_invariance(
  "eta1 =~ x1 + x2 + x3", ordinal_invariance_data, "group",
  estimator = "WLSMV", missing = "pairwise", ordered = ordered_names
)
stopifnot(
  isTRUE(ordinal_invariance$ordinal),
  identical(ordinal_invariance$table$Model, c("Configural", "Thresholds", "Scalar (thresholds + loadings)", "Strict")),
  all(ordinal_invariance$table$Converged), all(ordinal_invariance$table$Admissible),
  all(ordinal_invariance$table[["Admissibility reasons"]] == "None"),
  all(ordinal_invariance$table[["Parameter boundary dimensions"]] <= ordinal_invariance$table[["Explicit equality constraints"]]),
  all(vapply(ordinal_invariance$table[c("Residual min eigenvalue", "Latent min eigenvalue", "Parameter min eigenvalue")], function(values) all(is.finite(values)), logical(1))),
  all(vapply(ordinal_invariance$table[c("Residual condition number", "Latent condition number", "Parameter condition number")], function(values) all(is.finite(values) | is.infinite(values)), logical(1))),
  is.logical(ordinal_invariance$table[["Ill-conditioned warning"]]),
  all(ordinal_invariance$table[["Residual min eigenvalue"]] > 0),
  all(ordinal_invariance$table[["Latent min eigenvalue"]] > 0),
  all(vapply(ordinal_invariance$fits, function(fit) structural_canvas_fit_admissibility(fit)$admissible, logical(1))),
  all(vapply(ordinal_invariance$fits, function(fit) identical(structural_canvas_fit_admissibility(fit)$group_labels, c("A", "B")), logical(1))),
  all(is.finite(ordinal_invariance$table$CFI)),
  all(is.finite(ordinal_invariance$table$DeltaChisq[-1L])),
  all(vapply(ordinal_invariance$fits, function(fit) identical(lavaan::lavInspect(fit, "options")$parameterization, "theta"), logical(1))),
  nrow(ordinal_invariance$group_diagnostics) == 2L,
  all(ordinal_invariance$group_diagnostics$N == n / 2L),
  all(ordinal_invariance$group_diagnostics[["Absent ordered categories"]] == "None"),
  isTRUE(ordinal_invariance$group_residuals$available),
  nrow(ordinal_invariance$group_residuals$group_summary) == 2L,
  nrow(ordinal_invariance$group_residuals$group_pairs) > 0L,
  all(c("Group", "Indicator1", "Indicator2", "Standardized residual", "Correlation residual", "Exceeds cutoff") %in% names(ordinal_invariance$group_residuals$group_pairs)),
  identical(names(ordinal_invariance$score_diagnostics), names(ordinal_invariance$fits))
)
absent_category_data <- ordinal_invariance_data
absent_category_data$x1[absent_category_data$group == "A" & absent_category_data$x1 == 5L] <- 4L
absent_category_error <- tryCatch({
  structural_canvas_measurement_invariance(
    "eta1 =~ x1 + x2 + x3", absent_category_data, "group",
    estimator = "WLSMV", missing = "pairwise", ordered = ordered_names
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("categories are absent within group", absent_category_error, fixed = TRUE))
fixed_ordered_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise", ordered = ordered_names, residual_variance_fixes = c(x1 = .001))
  ""
}, error = conditionMessage)
stopifnot(grepl("supported only for continuous", fixed_ordered_error, fixed = TRUE))

missing_ordered_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise")
  ""
}, error = conditionMessage)
stopifnot(grepl("requires at least one", missing_ordered_error, fixed = TRUE))

cfa_toolbar <- htmltools::renderTags(structural_equation_toolbar("cfa", "en"))$html
cfa_id_parts <- strsplit(cfa_toolbar, 'id="', fixed = TRUE)[[1L]][-1L]
cfa_ids <- sub('".*$', "", cfa_id_parts)
stopifnot(!anyDuplicated(cfa_ids))
stopifnot(grepl("AVE/reliability bootstrap CI", cfa_toolbar, fixed = TRUE))
stopifnot(grepl("Download analysis record", ui_source, fixed = TRUE))
stopifnot(grepl("Download result tables", ui_source, fixed = TRUE))
stopifnot(!grepl("ko <- FALSE", ui_source, fixed = TRUE))
stopifnot(!grepl("�", ui_source, fixed = TRUE))
stopifnot(!grepl("structural-covariate-toolbar-button", cfa_toolbar, fixed = TRUE))
stopifnot(!grepl("data-action=\"structuralCovariateTargets\"", cfa_toolbar, fixed = TRUE))

cat("CFA canvas validations passed.\n")
