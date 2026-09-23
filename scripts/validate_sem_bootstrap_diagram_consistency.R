options(warn = 1)
source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("lavaan is required for SEM bootstrap consistency validation.", call. = FALSE)
}

fixture_environment <- new.env(parent = emptyenv())
utils::data("PoliticalDemocracy", package = "lavaan", envir = fixture_environment)
fixture_data <- as.data.frame(
  fixture_environment$PoliticalDemocracy,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
model <- paste(
  "dem60 =~ x1 + x2 + x3",
  "dem65 =~ y1 + y2 + y3 + y4",
  "ind60 =~ y5 + y6 + y7 + y8",
  "dem65 ~ int_path*dem60",
  "ind60 ~ 0.5*dem65 + dem60",
  sep = "\n"
)
fit <- suppressWarnings(lavaan::sem(model, data = fixture_data))
if (!isTRUE(lavaan::lavInspect(fit, "converged")) ||
    !isTRUE(lavaan::lavInspect(fit, "post.check"))) {
  stop("Bootstrap consistency fixture must converge to an admissible solution.", call. = FALSE)
}

effect_definition <- list(
  type = "Indirect",
  paths = list(c("dem60", "dem65", "ind60")),
  path_labels = list(c("int_path", "0.5"))
)
moderation_definition <- list(
  predictor = "dem60", outcome = "dem65", moderator = "demW",
  interaction_label = "int_path"
)
analysis_result <- list(
  fit = fit, converged = TRUE, admissible = TRUE,
  effect_definitions = list(effect_definition),
  moderation_definitions = list(moderation_definition),
  df = unname(lavaan::fitMeasures(fit, "df"))
)

# A fixed numeric coefficient is a valid product token. It must contribute to
# both the original moderated-mediation index and every prepared bootstrap draw.
coefficient <- unname(lavaan::coef(fit)[["int_path"]])
expected_index <- coefficient * 0.5
indices <- structural_canvas_moderated_mediation_indices(analysis_result)
if (nrow(indices) != 1L || !isTRUE(all.equal(indices$est[[1L]], expected_index, tolerance = 1e-10))) {
  stop("The original moderated-mediation index dropped its fixed numeric token.", call. = FALSE)
}
specifications <- structural_canvas_moderated_mediation_bootstrap_specs(analysis_result)
if (length(specifications) != 1L ||
    !identical(as.character(specifications[[1L]]$required_tokens), c("int_path", "0.5"))) {
  stop("Prepared moderated-mediation specifications did not preserve the numeric token.", call. = FALSE)
}
raw <- lavaan::parameterEstimates(fit)
raw <- raw[raw$op %in% c("~", ":="), c("lhs", "op", "rhs", "est"), drop = FALSE]
raw <- rbind(raw, indices)
raw_keys <- paste(raw$lhs, raw$op, raw$rhs, sep = "\r")
extracted <- structural_canvas_effect_bootstrap_extract_fit(
  fit, raw_keys, specifications, unname(lavaan::fitMeasures(fit, "df"))
)
index_key <- paste(indices$lhs[[1L]], "modmed", indices$rhs[[1L]], sep = "\r")
index_position <- match(index_key, raw_keys)
if (!isTRUE(extracted$valid) || is.na(index_position) ||
    !isTRUE(all.equal(extracted$raw[[index_position]], expected_index, tolerance = 1e-10))) {
  stop("The prepared bootstrap extractor dropped its fixed numeric token.", call. = FALSE)
}

# A moderated-mediation index containing a fixed zero token is an algebraic
# constant, not a bootstrap test with p = 1 and CI [0, 0]. Keep its point
# estimate, but suppress every inferential field just like other fixed effects.
zero_effect_definition <- effect_definition
zero_effect_definition$path_labels <- list(c("int_path", "0"))
zero_result <- analysis_result
zero_result$effect_definitions <- list(zero_effect_definition)
zero_indices <- structural_canvas_moderated_mediation_indices(zero_result)
zero_sources <- structural_canvas_effect_bootstrap_fixed_sources(
  fit, zero_indices, zero_result$effect_definitions, zero_result$moderation_definitions
)
zero_diagnostics <- data.frame(
  lhs = zero_indices$lhs, op = zero_indices$op, rhs = zero_indices$rhs,
  estimate = zero_indices$est, se = 0, lower = 0, upper = 0, p = 1,
  beta_estimate = NA_real_, beta_se = NA_real_, beta_lower = NA_real_,
  beta_upper = NA_real_, beta_p = NA_real_, beta_valid = 100L,
  beta_status = "Not reported: product-indicator index is scale-dependent",
  valid = 100L, requested = 100L, valid_percent = 100,
  ci_method = "percentile", quantile_type = 6L, status = "Adequate",
  stringsAsFactors = FALSE
)
zero_diagnostics <- structural_canvas_suppress_fixed_bootstrap_inference(
  zero_diagnostics, zero_sources
)
if (nrow(zero_indices) != 1L || zero_indices$est[[1L]] != 0 ||
    !identical(zero_sources, "Fixed effect - no inferential test") ||
    !all(is.na(unlist(zero_diagnostics[c("se", "lower", "upper", "p")], use.names = FALSE))) ||
    !identical(zero_diagnostics$inference_source[[1L]], "Fixed effect - no inferential test")) {
  stop("Fixed-zero moderated mediation retained bootstrap inference.", call. = FALSE)
}

# Bootstrap preparation and synchronous execution must stop before resampling
# if the original fit is not an admissible converged solution.
inadmissible_result <- analysis_result
inadmissible_result$admissible <- FALSE
inadmissible_result$admissibility_reasons <- "injected inadmissible solution"
gate <- structural_canvas_effect_bootstrap_original_fit_gate(inadmissible_result)
if (isTRUE(gate$eligible) || !identical(gate$state, "original_fit_inadmissible") ||
    !grepl("inadmissible", gate$reason, fixed = TRUE)) {
  stop("The original-fit admissibility gate did not return an explicit blocked state.", call. = FALSE)
}
prepare_error <- tryCatch({
  structural_canvas_prepare_effect_bootstrap(
    list(), fixture_data, "sem", "ML", "listwise", FALSE,
    character(0), character(0), numeric(0), original_result = inadmissible_result
  )
  ""
}, error = function(error) conditionMessage(error))
if (!grepl("inadmissible", prepare_error, fixed = TRUE)) {
  stop("Prepared bootstrap did not stop on an inadmissible original fit.", call. = FALSE)
}
prepared_error <- tryCatch({
  structural_canvas_effect_bootstrap_prepared(
    list(
      data = fixture_data, fit_template = fit,
      original_fit_gate = gate
    ),
    reps = 2L, workers = 1L
  )
  ""
}, error = function(error) conditionMessage(error))
if (!grepl("inadmissible", prepared_error, fixed = TRUE)) {
  stop("Prepared bootstrap execution ignored the stored original-fit gate.", call. = FALSE)
}
synchronous_error <- local({
  original_runner <- run_structural_canvas_analysis
  on.exit(assign("run_structural_canvas_analysis", original_runner, envir = .GlobalEnv), add = TRUE)
  assign(
    "run_structural_canvas_analysis",
    function(...) inadmissible_result,
    envir = .GlobalEnv
  )
  tryCatch({
    structural_canvas_effect_bootstrap(
      list(), fixture_data, "sem", "ML", "listwise", FALSE,
      character(0), character(0), numeric(0), reps = 2L
    )
    ""
  }, error = function(error) conditionMessage(error))
})
if (!grepl("inadmissible", synchronous_error, fixed = TRUE)) {
  stop("Synchronous bootstrap execution ignored the original-fit gate.", call. = FALSE)
}
blocked_bundle <- list(
  effect_bootstrap = 100L,
  effect_bootstrap_pending = FALSE,
  effect_bootstrap_canceled = FALSE,
  effect_bootstrap_error = "",
  effect_bootstrap_blocked_reason = gate$reason,
  effect_bootstrap_result = NULL
)
blocked_bundle_state <- structural_canvas_effect_bootstrap_snapshot_state_from_bundle(blocked_bundle)
if (!identical(blocked_bundle_state$state, "blocked") || isTRUE(blocked_bundle_state$complete) ||
    !identical(blocked_bundle_state$source, "Bootstrap blocked - original model ineligible") ||
    !identical(blocked_bundle_state$reason, gate$reason) ||
    !identical(blocked_bundle_state$blocked_reason, gate$reason)) {
  stop("Bundle snapshot state did not preserve the explicit bootstrap block reason.", call. = FALSE)
}
blocked_reporting_state <- structural_canvas_effect_bootstrap_bundle_state(blocked_bundle)
if (!identical(blocked_reporting_state$state, "blocked") || isTRUE(blocked_reporting_state$complete) ||
    !identical(blocked_reporting_state$source, "Bootstrap blocked - original model ineligible") ||
    !identical(blocked_reporting_state$reason, gate$reason) ||
    !identical(blocked_reporting_state$note, gate$reason)) {
  stop("Bundle reporting state did not preserve the explicit bootstrap block reason.", call. = FALSE)
}

node <- function(id, name) list(id = id, role = "latent", name = name, canvasLabel = name, x = 100, y = 100)
snapshot <- list(
  nodes = list(node("lx", "dem60"), node("lm", "dem65"), node("ly", "ind60")),
  edges = list(
    list(id = "p1", from = "lx", to = "lm"),
    list(id = "p2", from = "lm", to = "ly"),
    list(id = "p3", from = "lx", to = "ly")
  )
)
edge_by_id <- function(result, id) {
  matches <- Filter(function(edge) identical(as.character(edge$id %||% ""), id), result$edges %||% list())
  if (length(matches)) matches[[1L]] else NULL
}

pending_snapshot <- structural_canvas_result_snapshot(
  snapshot, fit, "beta_p",
  effect_bootstrap_state = list(requested = 100L, pending = TRUE)
)
pending_edge <- edge_by_id(pending_snapshot, "p1")
if (is.null(pending_edge) || is.finite(pending_edge$p) || isTRUE(pending_edge$dashEligible) ||
    grepl("\\(", pending_edge$label) ||
    !identical(pending_edge$inferenceSource, "Bootstrap pending - inference suppressed")) {
  stop("Pending bootstrap canvas exposed normal-theory structural-path inference.", call. = FALSE)
}

failed_snapshot <- structural_canvas_result_snapshot(
  snapshot, fit, "beta_p",
  effect_bootstrap_state = list(requested = 100L, error = "injected failure")
)
failed_edge <- edge_by_id(failed_snapshot, "p1")
if (is.null(failed_edge) || is.finite(failed_edge$p) || isTRUE(failed_edge$dashEligible) ||
    grepl("\\(", failed_edge$label) ||
    !identical(failed_edge$inferenceSource, "Bootstrap failed - inference suppressed")) {
  stop("Failed bootstrap canvas exposed normal-theory structural-path inference.", call. = FALSE)
}

base_parameters <- lavaan::parameterEstimates(fit, standardized = TRUE)
base_path <- base_parameters[
  base_parameters$lhs == "dem65" & base_parameters$op == "~" & base_parameters$rhs == "dem60",
  , drop = FALSE
]
bootstrap_p <- 0.234
bootstrap_result <- data.frame(
  lhs = "dem65", op = "~", rhs = "dem60",
  estimate = base_path$est[[1L]], se = 0.05, lower = 0.10, upper = 0.40, p = bootstrap_p,
  beta_estimate = base_path$std.all[[1L]], beta_se = 0.05,
  beta_lower = 0.10, beta_upper = 0.40, beta_p = bootstrap_p,
  beta_valid = 100L, beta_status = "Estimated", valid = 100L,
  requested = 100L, valid_percent = 100, ci_method = "percentile",
  quantile_type = 6L, status = "Adequate",
  inference_source = "Bootstrap (empirical two-sided p)",
  stringsAsFactors = FALSE
)
complete_state <- list(requested = 100L, result = bootstrap_result)
complete_snapshot <- structural_canvas_result_snapshot(
  snapshot, fit, "beta_p", effect_bootstrap_state = complete_state
)
complete_edge <- edge_by_id(complete_snapshot, "p1")
if (is.null(complete_edge) ||
    !isTRUE(all.equal(complete_edge$p, bootstrap_p, tolerance = 0)) ||
    !grepl(paste0("(", format_p(bootstrap_p), ")"), complete_edge$label, fixed = TRUE) ||
    !isTRUE(complete_edge$dashEligible) ||
    !identical(complete_edge$inferenceSource, "Bootstrap (empirical two-sided p)")) {
  stop("Completed bootstrap canvas did not use the empirical bootstrap p/source.", call. = FALSE)
}

table_state <- structural_canvas_effect_bootstrap_reporting_state(
  requested = 100L, result = bootstrap_result
)
fmt_vector <- function(values) vapply(values, format_decimal3, character(1))
table <- structural_canvas_lavaan_structural_result_table(
  "structural", fit, FALSE, fmt_vector, identity,
  bootstrap = bootstrap_result, bootstrap_state = table_state
)
table_row <- table[table$Outcome == "dem65" & table$Predictor == "dem60", , drop = FALSE]
if (nrow(table_row) != 1L ||
    !isTRUE(all.equal(table_row$p_numeric[[1L]], complete_edge$p, tolerance = 0)) ||
    !identical(table_row[["Inference source"]][[1L]], complete_edge$inferenceSource)) {
  stop("Completed canvas and structural table used different bootstrap inference.", call. = FALSE)
}

fixed_bootstrap <- bootstrap_result
fixed_bootstrap$p <- NA_real_
fixed_bootstrap$inference_source <- "Fixed parameter - no inferential test"
fixed_snapshot <- structural_canvas_result_snapshot(
  snapshot, fit, "beta_p",
  effect_bootstrap_state = list(requested = 100L, result = fixed_bootstrap)
)
fixed_edge <- edge_by_id(fixed_snapshot, "p1")
if (is.null(fixed_edge) || is.finite(fixed_edge$p) || isTRUE(fixed_edge$dashEligible) ||
    grepl("\\(", fixed_edge$label) ||
    !identical(fixed_edge$inferenceSource, "Fixed parameter - no inferential test")) {
  stop("Fixed bootstrap path should show a point estimate without p/significance styling.", call. = FALSE)
}
unusable_bootstrap <- bootstrap_result
unusable_bootstrap$valid <- unusable_bootstrap$beta_valid <- 40L
unusable_snapshot <- structural_canvas_result_snapshot(
  snapshot, fit, "beta_p",
  effect_bootstrap_state = list(requested = 100L, result = unusable_bootstrap)
)
unusable_edge <- edge_by_id(unusable_snapshot, "p1")
if (is.null(unusable_edge) || is.finite(unusable_edge$p) || isTRUE(unusable_edge$dashEligible) ||
    grepl("\\(", unusable_edge$label) ||
    !identical(unusable_edge$inferenceSource, "Bootstrap requested - inference suppressed")) {
  stop("An unusable completed bootstrap path must suppress p/significance styling.", call. = FALSE)
}

# The overall snapshot may use its base-fit bootstrap. Group panels come from
# the free multigroup fit, so they must retain that fit's own group-specific p.
group_data <- rbind(transform(fixture_data, group_id = "A"), transform(fixture_data, group_id = "B"))
group_fit <- suppressWarnings(lavaan::sem(model, data = group_data, group = "group_id"))
if (!isTRUE(lavaan::lavInspect(group_fit, "converged"))) {
  stop("Group snapshot fixture did not converge.", call. = FALSE)
}
result_snapshots <- structural_canvas_group_result_snapshots(
  snapshot, fit, "beta_p", invariance_result = list(
    type = "structural_path_comparison",
    fits = list("Free structural paths" = group_fit)
  ), effect_bootstrap_state = complete_state
)
if (length(result_snapshots) != 3L) {
  stop("Expected overall plus two group result snapshots.", call. = FALSE)
}
overall_edge <- edge_by_id(result_snapshots[[1L]]$result, "p1")
group_edge <- edge_by_id(result_snapshots[[2L]]$result, "p1")
group_parameters <- lavaan::parameterEstimates(group_fit, standardized = TRUE)
group_path <- group_parameters[
  group_parameters$group == 1L & group_parameters$lhs == "dem65" &
    group_parameters$op == "~" & group_parameters$rhs == "dem60",
  , drop = FALSE
]
if (!isTRUE(all.equal(overall_edge$p, bootstrap_p, tolerance = 0)) ||
    !isTRUE(all.equal(group_edge$p, group_path$pvalue[[1L]], tolerance = 1e-12)) ||
    !identical(group_edge$inferenceSource, "Model-based normal-theory") ||
    identical(group_edge$p, bootstrap_p)) {
  stop("Group snapshot incorrectly inherited pooled/base-fit bootstrap inference.", call. = FALSE)
}

cat("SEM bootstrap diagram consistency validation passed.\n")
