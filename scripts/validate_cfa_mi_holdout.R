source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

set.seed(20260812)
n <- 400L
factor_score <- stats::rnorm(n)
continuous <- data.frame(
  x1 = .8 * factor_score + stats::rnorm(n, sd = .6),
  x2 = .7 * factor_score + stats::rnorm(n, sd = .7),
  x3 = .9 * factor_score + stats::rnorm(n, sd = .5)
)
snapshot <- list(
  nodes = c(
    list(list(id = "latent_1", role = "latent", name = "eta1", x = 200, y = 200)),
    lapply(seq_len(3L), function(index) {
      list(id = paste0("indicator_", index), role = "indicator", name = paste0("x", index), x = 400, y = index * 100)
    })
  ),
  edges = lapply(seq_len(3L), function(index) list(from = "latent_1", to = paste0("indicator_", index)))
)

ml <- run_structural_canvas_analysis(snapshot, continuous, "cfa")
holdout_data <- continuous
holdout_data$x4 <- .65 * factor_score + stats::rnorm(n, sd = .75)
seed_before_holdout_split <- .Random.seed
holdout_split <- structural_canvas_holdout_split(holdout_data, validation_fraction = .30, seed = 13579L)
holdout_split_repeat <- structural_canvas_holdout_split(holdout_data, validation_fraction = .30, seed = 13579L)
holdout_comparison <- structural_canvas_holdout_model_comparison(
  "eta1 =~ x1 + x2 + x3 + x4", "eta1 =~ x1 + x2 + x3 + x4\nx1 ~~ x2",
  holdout_split$validation, estimator = "MLR", missing = "fiml"
)
stopifnot(
  nrow(holdout_split$exploration) == 280L,
  nrow(holdout_split$validation) == 120L,
  identical(holdout_split$validation_rows, holdout_split_repeat$validation_rows),
  !length(intersect(holdout_split$exploration_rows, holdout_split$validation_rows)),
  identical(sort(c(holdout_split$exploration_rows, holdout_split$validation_rows)), seq_len(n)),
  identical(.Random.seed, seed_before_holdout_split),
  identical(holdout_comparison$table$Model, c("Original model", "Modified model")),
  all(holdout_comparison$table$Converged),
  all(holdout_comparison$table$Admissible),
  all(holdout_comparison$table[["Admissibility reasons"]] == "None"),
  all(vapply(holdout_comparison$fits, function(fit) structural_canvas_fit_admissibility(fit)$admissible, logical(1))),
  holdout_comparison$validation_n_raw == 120L,
  all(holdout_comparison$validation_n_used == 120L),
  all(holdout_comparison$table[["N used"]] == 120L),
  identical(holdout_comparison$changes$`Comparison status`[[1L]], "Both validation models admissible"),
  all(is.finite(unlist(holdout_comparison$changes[vapply(holdout_comparison$changes, is.numeric, logical(1))], use.names = FALSE))),
  inherits(try(structural_canvas_holdout_split(holdout_data[1:50, ], .30), silent = TRUE), "try-error"),
  isTRUE(structural_canvas_validate_holdout_options(TRUE, "cfa", "MLR")),
  inherits(try(structural_canvas_validate_holdout_options(TRUE, "cfa", "WLSMV", ordered = "x1"), silent = TRUE), "try-error"),
  inherits(try(structural_canvas_validate_holdout_options(TRUE, "cfa", "MLR", invariance_enabled = TRUE), silent = TRUE), "try-error"),
  inherits(try(structural_canvas_validate_holdout_options(TRUE, "cfa", "MLR", residual_variance_fixes = c(x1 = .001)), silent = TRUE), "try-error"),
  isTRUE(structural_canvas_validate_holdout_reuse(TRUE, FALSE)),
  inherits(try(structural_canvas_validate_holdout_reuse(TRUE, TRUE), silent = TRUE), "try-error"),
  isTRUE(structural_canvas_validate_holdout_reuse(FALSE, TRUE))
)

two_factor_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3\neta2 =~ x4 + x5 + x6",
  data = lavaan::HolzingerSwineford1939
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
information_criteria_different_observations <- structural_canvas_information_criteria(list(
  fit = perturbed_two_factor_fit, baseline_fit = two_factor_fit,
  modified_from_baseline = TRUE, comparison_label = "Different observations"
))
nonnested_eligibility <- structural_canvas_nested_comparison_eligibility(two_factor_fit, alternative_two_factor_fit)
nonnested_difference_report <- structural_canvas_model_difference_report(list(
  baseline_fit = two_factor_fit, fit = alternative_two_factor_fit, comparison_type = "mi"
))
stopifnot(
  !isTRUE(different_observation_eligibility$available),
  identical(different_observation_eligibility$reason, "Models do not use the same analyzed observations and values."),
  all(information_criteria_different_observations$`Comparison status` == "Not comparable; delta suppressed"),
  all(is.na(information_criteria_different_observations$`Delta AIC`)),
  !isTRUE(nonnested_eligibility$available),
  grepl("degrees of freedom", nonnested_eligibility$reason, fixed = TRUE) || grepl("nesting", nonnested_eligibility$reason, fixed = TRUE),
  is.null(structural_canvas_model_difference(two_factor_fit, alternative_two_factor_fit)),
  !isTRUE(nonnested_difference_report$Available[[1L]]),
  grepl("degrees of freedom", nonnested_difference_report$Reason[[1L]], fixed = TRUE) || grepl("nesting", nonnested_difference_report$Reason[[1L]], fixed = TRUE)
)

fixed_value <- stats::var(continuous$x1) * .001
constrained <- suppressWarnings(run_structural_canvas_analysis(
  snapshot, continuous, "cfa", residual_variance_fixes = c(x1 = fixed_value)
))
common_ml_measures <- structural_canvas_common_fit_measures(list(ml$fit, constrained$fit), "ML", .90)
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
nested_difference_eligibility <- structural_canvas_nested_comparison_eligibility(ml$fit, constrained$fit)
nested_difference_report <- structural_canvas_model_difference_report(list(
  baseline_fit = ml$fit, fit = constrained$fit, comparison_type = "mi"
))
stopifnot(
  length(common_ml_measures) == 2L,
  identical(common_ml_measures[[1L]]$keys, common_ml_measures[[2L]]$keys),
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
  max(abs(mi_significance_candidates$`MI p` - stats::pchisq(mi_significance_candidates$mi, 1L, lower.tail = FALSE))) < 1e-12,
  nrow(sequential_mi_candidates) > 0L,
  all(c("step", "skipped_inadmissible", "skipped_details") %in% names(sequential_mi_candidates)),
  identical(sequential_mi_candidates$step, seq_len(nrow(sequential_mi_candidates))),
  all(sequential_mi_candidates$skipped_inadmissible >= 0L),
  all(nzchar(sequential_mi_candidates$skipped_details) == (sequential_mi_candidates$skipped_inadmissible > 0L)),
  grepl("candidate_row$step <- step", evaluation_source, fixed = TRUE),
  grepl("candidate_row$skipped_inadmissible <- skipped_candidates", evaluation_source, fixed = TRUE),
  grepl("candidate_row$skipped_details <- paste(skipped_details, collapse = \" | \")", evaluation_source, fixed = TRUE),
  grepl("`Skipped unsafe` = skipped", ui_source, fixed = TRUE) && grepl('attr(table, "skipped_details"', ui_source, fixed = TRUE),
  grepl("structural-mi-skipped-details-table", ui_source, fixed = TRUE),
  grepl("Each Step is sequential", ui_source, fixed = TRUE),
  isTRUE(nested_difference_eligibility$available),
  !is.null(model_difference),
  is.finite(model_difference$df),
  isTRUE(nested_difference_report$Available[[1L]]),
  is.finite(nested_difference_report$`Delta chi-square`[[1L]]),
  identical(nested_difference_report$Context[[1L]], "Exploratory same-sample MI modification")
)

mi_fixture <- data.frame(
  lhs = c("x1", "x2"), op = c("~~", "~~"), rhs = c("x2", "x1"),
  mi = c(12, 12), epc = c(.20, .20), cfi_after = c(.95, .95),
  tli_after = c(.94, .94), rmsea_after = c(.05, .05), srmr_after = c(.04, .04),
  stringsAsFactors = FALSE
)
mi_history <- structural_canvas_mi_history_rows(mi_fixture, 1:2, justification = "Shared wording")
mi_history_again <- structural_canvas_mi_history_rows(mi_fixture, 1L, existing = mi_history, justification = "Duplicate")
stopifnot(
  nrow(mi_history) == 1L,
  identical(mi_history$Step, 1L),
  identical(mi_history$Justification, "Shared wording"),
  nrow(mi_history_again) == 1L
)

cat("CFA MI/holdout validations passed.\n")
