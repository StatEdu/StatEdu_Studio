source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

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

variable_table <- data.frame(
  name = c("x1", "x2", "x3"),
  measurement = c("ordered", "ordinal", "continuous"),
  stringsAsFactors = FALSE
)
ordered_names <- structural_canvas_ordered_indicators(snapshot, variable_table)

nominal_table <- data.frame(
  name = c("x1", "x2", "x3"),
  measurement = c("category", "nominal", "factor"),
  stringsAsFactors = FALSE
)
engine_nominal_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, continuous, "cfa", nominal = "x1")
  ""
}, error = conditionMessage)
stopifnot(
  identical(ordered_names, c("x1", "x2")),
  length(structural_canvas_ordered_indicators(snapshot, nominal_table)) == 0L,
  identical(structural_canvas_nominal_indicators(snapshot, nominal_table), c("x1", "x2", "x3")),
  grepl("Nominal indicators are not supported", engine_nominal_error, fixed = TRUE)
)

ordered_category_data <- data.frame(x1 = ordered(c("A", rep("B", 98), "C"), levels = c("A", "B", "C", "D")))
category_diagnostics <- structural_canvas_ordered_category_diagnostics(ordered_category_data, "x1")
ordinal <- as.data.frame(lapply(continuous, function(value) {
  as.integer(cut(value, breaks = stats::quantile(value, probs = seq(0, 1, .2)), include.lowest = TRUE))
}))
ordered_pair_diagnostics <- structural_canvas_ordered_pair_diagnostics(ordinal, ordered_names)
sparse_ordinal <- data.frame(
  a = factor(c(rep(1L, 99L), 2L), levels = 1:3),
  b = factor(c(rep(1L, 98L), 2L, 3L), levels = 1:3)
)
sparse_categories <- structural_canvas_ordered_category_diagnostics(sparse_ordinal, c("a", "b"))
sparse_pairs <- structural_canvas_ordered_pair_diagnostics(sparse_ordinal, c("a", "b"))
stopifnot(
  identical(as.character(category_diagnostics$Status), c("Sparse", "Dominant (>=95%)", "Sparse", "Empty")),
  nrow(ordered_pair_diagnostics) == choose(length(ordered_names), 2L),
  all(ordered_pair_diagnostics[["Cells"]] == 25L),
  all(ordered_pair_diagnostics[["Valid pairs"]] == n),
  any(sparse_categories$Status == "Empty"),
  any(sparse_categories$Status == "Sparse"),
  any(sparse_categories$Status == "Dominant (>=95%)"),
  sparse_pairs[["Empty cells"]] > 0L,
  sparse_pairs[["Sparse nonempty cells"]] > 0L,
  identical(sparse_pairs$Status, "Review")
)

wlsmv <- run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise", ordered = ordered_names)
wlsmv_bundle <- list(fit = wlsmv$fit, diagnostics = wlsmv, estimator = "WLSMV", parameterization = wlsmv$parameterization)
wlsmv_overview_en <- structural_canvas_result_table("overview", function() wlsmv_bundle, "cfa", function() character(0), function() "en")
wlsmv_overview_ko <- structural_canvas_result_table("overview", function() wlsmv_bundle, "cfa", function() character(0), function() "ko")
wlsmv_audit <- structural_canvas_audit_manifest(c(
  wlsmv_bundle,
  list(snapshot = snapshot, syntax = wlsmv$syntax, ordered = ordered_names, analysis_data = ordinal, missing = "pairwise")
), "cfa")
wlsmv_bollen_eligibility <- structural_canvas_bollen_stine_eligibility(wlsmv$fit)
wlsmv_measures <- structural_canvas_fit_measures(wlsmv$fit, "WLSMV", .90)
fixed_ordered_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise", ordered = ordered_names, residual_variance_fixes = c(x1 = .001))
  ""
}, error = conditionMessage)
missing_ordered_error <- tryCatch({
  run_structural_canvas_analysis(snapshot, ordinal, "cfa", estimator = "WLSMV", missing = "pairwise")
  ""
}, error = conditionMessage)
stopifnot(
  isTRUE(wlsmv$converged),
  identical(wlsmv$parameterization, "theta"),
  identical(tolower(lavaan::lavInspect(wlsmv$fit, "options")$parameterization), "theta"),
  identical(wlsmv_overview_en$Value[wlsmv_overview_en$Item == "Parameterization"], "Theta"),
  identical(wlsmv_overview_ko$값[wlsmv_overview_ko$항목 == "모수화"], "Theta"),
  identical(wlsmv_audit$analysis$parameterization, "theta"),
  !isTRUE(wlsmv_bollen_eligibility$available),
  grepl("ML estimation", wlsmv_bollen_eligibility$reason, fixed = TRUE),
  inherits(try(structural_canvas_bollen_stine(wlsmv$fit, reps = 2L), silent = TRUE), "try-error"),
  identical(sort(lavaan::lavNames(wlsmv$fit, "ov.ord")), sort(ordered_names)),
  isTRUE(wlsmv_measures$adjusted),
  grepl("supported only for continuous", fixed_ordered_error, fixed = TRUE),
  grepl("requires at least one", missing_ordered_error, fixed = TRUE)
)

cat("CFA ordinal validations passed.\n")
