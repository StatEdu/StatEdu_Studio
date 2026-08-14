source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))
edge_source <- paste(readLines(file.path("www", "model-canvas", "edges.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
state_source <- paste(readLines(file.path("www", "model-canvas", "state.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
dialog_source <- paste(readLines(file.path("www", "model-canvas", "dialogs.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("ARROW_MARKER_CLEARANCE", edge_source, fixed = TRUE),
  grepl("var ARROW_MARKER_CLEARANCE = 0;", edge_source, fixed = TRUE),
  grepl("arrowHead === \"open\" ? \"8\" : \"9\"", edge_source, fixed = TRUE),
  grepl("function visibleArrowEndpoints", edge_source, fixed = TRUE),
  grepl("to: pointShiftedToward(endpoints.to, endpoints.from, ARROW_MARKER_CLEARANCE)", edge_source, fixed = TRUE),
  grepl("return visibleArrowEndpoints(edge, {\n        from: radialNodeAnchor(instance, from, to)", edge_source, fixed = TRUE),
  grepl("return visibleArrowEndpoints(edge, endpoints);", edge_source, fixed = TRUE),
  !grepl("edge.directAnchors) return endpoints", edge_source, fixed = TRUE),
  grepl('arrowHead: "triangle"', state_source, fixed = TRUE),
  grepl('sourceVersion < 5 && state.style.arrowHead === "line"', state_source, fixed = TRUE),
  grepl('current.arrowHead || "triangle"', dialog_source, fixed = TRUE),
  grepl("function measurementGroupUsesSharedStart", edge_source, fixed = TRUE),
  grepl("if (!measurementGroupUsesSharedStart(instance, bundle.latent, group)) return;", edge_source, fixed = TRUE)
)
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
stopifnot(
  isTRUE(structural_canvas_validate_model_based_bootstrap(ml$fit)),
  identical(structural_canvas_bootstrap_status(c(80, 79, 50, 49, 0), 100), c("Adequate", "Caution", "Caution", "Unreliable", "Unreliable")),
  identical(structural_canvas_bootstrap_ci_method("BCa (slower)"), "bca"),
  grepl('progress(index, total_iterations, length(Filter(function(value) !is.null(value) && nrow(value), estimates[seq_len(index)])))', bootstrap_source, fixed = TRUE)
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
two_factor_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6",
  data = lavaan::HolzingerSwineford1939
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
mlr_fit_measures <- lavaan::fitMeasures(two_factor_mlr_fit)
additional_fit_mlr <- structural_canvas_additional_fit_indices_table(
  list(two_factor_mlr_fit),
  "Research model",
  structural_canvas_common_fit_measures(list(two_factor_mlr_fit), "MLR", .90)
)
additional_fit_wide <- structural_canvas_additional_fit_indices_wide_tables(additional_fit_mlr)
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
  length(additional_fit_wide) > 0L,
  any(vapply(additional_fit_wide, function(table) ncol(table) > 2L, logical(1))),
  !all(names(additional_fit_wide) == "other")
)
latent_correlation_ci <- structural_canvas_latent_correlation_intervals(two_factor_fit)
stopifnot(
  nrow(latent_correlation_ci) == 1L,
  identical(latent_correlation_ci$Type[[1L]], "Estimated"),
  is.finite(latent_correlation_ci$r[[1L]]),
  is.finite(latent_correlation_ci$`CI lower`[[1L]]),
  is.finite(latent_correlation_ci$`CI upper`[[1L]]),
  latent_correlation_ci$`CI lower`[[1L]] <= latent_correlation_ci$r[[1L]],
  latent_correlation_ci$r[[1L]] <= latent_correlation_ci$`CI upper`[[1L]]
)
ml_measures <- structural_canvas_fit_measures(ml$fit, "ML", .90)
stopifnot(identical(ml_measures$labels[[5L]], "CFI"), !isTRUE(ml_measures$adjusted))
stopifnot(is.na(ml_measures$values[[4L]]))
bollen_stine_eligible <- structural_canvas_bollen_stine_eligibility(two_factor_fit)
stopifnot(isTRUE(bollen_stine_eligible$available))
stopifnot(
  grepl("structural_canvas_fit_admissibility(candidate)$admissible", bootstrap_source, fixed = TRUE),
  grepl("admissibility <- structural_canvas_fit_admissibility(fit)", bootstrap_source, fixed = TRUE)
)
saturated_bollen_eligibility <- structural_canvas_bollen_stine_eligibility(ml$fit)
stopifnot(
  !isTRUE(saturated_bollen_eligibility$available),
  grepl("df = 0", saturated_bollen_eligibility$reason, fixed = TRUE),
  inherits(try(structural_canvas_bollen_stine(ml$fit, reps = 2L), silent = TRUE), "try-error")
)
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
  grepl("추정량 권고", ui_source, fixed = TRUE),
  grepl("다변량 정규성 및 추정량 안내", ui_source, fixed = TRUE),
  grepl("MI 수정 기록", ui_source, fixed = TRUE),
  grepl("Heywood 제약 재분석", ui_source, fixed = TRUE),
  grepl("탐색적 수정 모형", ui_source, fixed = TRUE),
  grepl("MI 수정 이력", ui_source, fixed = TRUE),
  grepl("MI 홀드아웃 검증", ui_source, fixed = TRUE),
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

cat("CFA canvas validations passed.\n")
