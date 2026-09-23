#!/usr/bin/env Rscript

source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

assert_close <- function(actual, expected, tolerance = 1e-10, label = "values") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance, check.attributes = FALSE))) {
    stop(paste0(label, " differ. actual=", paste(signif(actual, 12), collapse = ", "),
      "; expected=", paste(signif(expected, 12), collapse = ", ")), call. = FALSE)
  }
}

node <- function(id, name, role, ...) c(
  list(id = id, name = name, canvasLabel = name, role = role), list(...)
)

make_fixture <- function(method = "two_stage", n = 420L, seed = 2026082501L) {
  set.seed(seed)
  x <- rnorm(n)
  w <- rnorm(n)
  y <- .34 * x + .22 * w + .48 * x * w + rnorm(n, sd = .55)
  make_block <- function(score, prefix) {
    loading <- .88
    data.frame(
      stats::setNames(lapply(seq_len(3L), function(index) {
        loading * score + rnorm(n, sd = sqrt(1 - loading^2))
      }), paste0(prefix, seq_len(3L))),
      check.names = FALSE
    )
  }
  data <- cbind(make_block(x, "x"), make_block(w, "w"), make_block(y, "y"))
  latents <- list(
    node("lx", "X", "latent", constructType = "commonFactor", measurementMode = "reflective", weightingMode = "auto"),
    node("lw", "W", "latent", constructType = "commonFactor", measurementMode = "reflective", weightingMode = "auto"),
    node("ly", "Y", "latent", constructType = "commonFactor", measurementMode = "reflective", weightingMode = "auto")
  )
  indicators <- list()
  measurement_edges <- list()
  edge_index <- 0L
  construct_ids <- c(X = "lx", W = "lw", Y = "ly")
  for (construct_name in names(construct_ids)) {
    construct_id <- unname(construct_ids[[construct_name]])
    for (indicator in paste0(tolower(construct_name), seq_len(3L))) {
      indicators[[length(indicators) + 1L]] <- node(indicator, indicator, "indicator", variableId = indicator)
      edge_index <- edge_index + 1L
      measurement_edges[[edge_index]] <- list(
        id = paste0("m", edge_index), from = construct_id, to = indicator, kind = "directed"
      )
    }
  }
  structural_edge <- list(id = "xy", from = "lx", to = "ly", kind = "directed", pathType = "regression")
  snapshot <- list(
    modelSchemaVersion = 7L,
    analysisType = "plssem",
    nodes = c(latents, indicators),
    edges = c(measurement_edges, list(structural_edge)),
    moderations = list(list(id = "x_by_w", from = "lw", toEdge = "xy")),
    moderationMethod = method,
    covariates = list()
  )
  list(snapshot = snapshot, data = data, latents = latents)
}

fit_fixture <- function(method = "two_stage", estimator = "PLS") {
  fixture <- make_fixture(method)
  result <- structural_canvas_run_pls_analysis(
    fixture$snapshot, fixture$data, fixture$latents, fixture$snapshot$edges, estimator
  )
  list(fixture = fixture, result = result)
}

# The production PLS result must equal a direct seminr numerical oracle using
# the same declared two-stage interaction and strong-hierarchy paths.
two_stage <- fit_fixture("two_stage", "PLS")
oracle_mm <- seminr::constructs(
  seminr::composite("X", paste0("x", 1:3), weights = seminr::mode_A),
  seminr::composite("W", paste0("w", 1:3), weights = seminr::mode_A),
  seminr::composite("Y", paste0("y", 1:3), weights = seminr::mode_A),
  seminr::interaction_term(iv = "X", moderator = "W", method = seminr::two_stage, weights = seminr::mode_A)
)
oracle_sm <- seminr::relationships(seminr::paths(from = c("X", "W", "X*W"), to = "Y"))
oracle_fit <- suppressMessages(seminr::estimate_pls(
  data = two_stage$fixture$data,
  measurement_model = oracle_mm,
  structural_model = oracle_sm,
  missing = seminr::mean_replacement,
  maxIt = structural_canvas_pls_max_iterations(),
  stopCriterion = structural_canvas_pls_stop_criterion(),
  assess_syntax = FALSE
))
assert_close(
  two_stage$result$fit$path_coef[c("X", "W", "X*W"), "Y"],
  oracle_fit$path_coef[c("X", "W", "X*W"), "Y"],
  tolerance = 1e-12,
  label = "PLS two-stage oracle paths"
)
stopifnot(
  identical(two_stage$result$moderation_definitions[[1L]]$method, "two_stage"),
  isTRUE(two_stage$result$moderation_definitions[[1L]]$moderator_main_effect_auto_added),
  all(c("X", "W", "X*W") %in% rownames(two_stage$result$fit$path_coef)),
  identical(two_stage$result$interaction_constructs, "X*W"),
  nrow(two_stage$result$moderation_effects) == 1L,
  nrow(two_stage$result$moderation_simple_slopes) == 3L
)
direct <- two_stage$result$fit$path_coef["X", "Y"]
interaction <- two_stage$result$fit$path_coef["X*W", "Y"]
assert_close(
  two_stage$result$moderation_simple_slopes[["Simple slope"]],
  direct + c(-1, 0, 1) * interaction,
  label = "simple-slope oracle"
)

# Saved CB-SEM methods normalize safely to two-stage, while every supported
# PLS selector builds the requested seminr interaction class.
legacy <- fit_fixture("all_pairs_dmc", "PLS")
stopifnot(
  identical(legacy$result$moderation_definitions[[1L]]$method, "two_stage"),
  isTRUE(legacy$result$moderation_definitions[[1L]]$method_normalized)
)
method_classes <- c(
  two_stage = "two_stage_interaction",
  product_indicator = "scaled_interaction",
  orthogonal = "orthogonal_interaction"
)
for (method in names(method_classes)) {
  fitted <- fit_fixture(method, "PLS")$result
  interaction_terms <- Filter(function(value) inherits(value, "interaction"), fitted$fit$measurement_model)
  stopifnot(
    length(interaction_terms) == 1L,
    inherits(interaction_terms[[1L]], method_classes[[method]]),
    identical(fitted$moderation_definitions[[1L]]$method, method)
  )
}

# PLSc retains selective correction for common factors but explicitly leaves
# the generated interaction as an uncorrected composite (rho_A fixed to one).
plsc <- fit_fixture("two_stage", "PLSc")
oracle_plsc <- oracle_fit
oracle_plsc$statedu_common_factor_constructs <- c("X", "W", "Y")
oracle_plsc$statedu_composite_constructs <- character(0)
oracle_plsc <- structural_canvas_apply_plsc(oracle_plsc, c("X", "W", "Y"))
assert_close(
  plsc$result$fit$path_coef[c("X", "W", "X*W"), "Y"],
  oracle_plsc$path_coef[c("X", "W", "X*W"), "Y"],
  tolerance = 1e-12,
  label = "PLSc two-stage oracle paths"
)
stopifnot(
  identical(plsc$result$plsc_uncorrected_interactions, "X*W"),
  identical(plsc$result$fit$statedu_plsc_rho_a["X*W", 1L], 1),
  grepl("Uncorrected composite", plsc$result$moderation_effects[["PLSc interaction correction"]][[1L]], fixed = TRUE)
)

# The whole-draw 80% gate applies identically to interaction and simple-slope
# inference. Seven of ten draws suppress CI/p; eight of ten release them.
point_paths <- two_stage$result$fit$path_coef
make_draws <- function(count) {
  output <- array(
    rep(as.numeric(point_paths), count),
    dim = c(nrow(point_paths), ncol(point_paths), count),
    dimnames = list(rownames(point_paths), colnames(point_paths), as.character(seq_len(count)))
  )
  for (index in seq_len(count)) {
    output["X", "Y", index] <- point_paths["X", "Y"] + (index - (count + 1) / 2) * .005
    output["X*W", "Y", index] <- point_paths["X*W", "Y"] + (index - (count + 1) / 2) * .008
  }
  output
}
gate_70 <- structural_canvas_pls_moderation_bootstrap_tables(
  point_paths, make_draws(7L), two_stage$result$moderation_definitions, 10L, "PLS"
)
gate_80 <- structural_canvas_pls_moderation_bootstrap_tables(
  point_paths, make_draws(8L), two_stage$result$moderation_definitions, 10L, "PLS"
)
stopifnot(
  !isTRUE(gate_70$validity$adequate),
  all(!gate_70$effects[["Inference available"]]),
  all(is.na(gate_70$effects$p)),
  all(is.na(gate_70$simple_slopes[["95% CI lower"]])),
  isTRUE(gate_80$validity$adequate),
  all(gate_80$effects[["Inference available"]]),
  all(is.finite(gate_80$effects$p)),
  all(is.finite(gate_80$simple_slopes[["95% CI lower"]]))
)

# Full-draw bootstrap reproducibility and method fidelity. The measurement
# model retained in each fit contains the interaction closure, so every sampled
# dataset re-estimates both stages before accepted path statistics are stored.
bootstrap_seed <- 2026082523L
boot_a <- suppressWarnings(structural_canvas_run_plsc_bootstrap(
  two_stage$result$fit, nboot = 24L, seed = bootstrap_seed, apply_plsc = FALSE
))
boot_b <- suppressWarnings(structural_canvas_run_plsc_bootstrap(
  two_stage$result$fit, nboot = 24L, seed = bootstrap_seed, apply_plsc = FALSE
))
message("PLS moderation reproducibility bootstrap completed.")
stopifnot(
  isTRUE(boot_a$inference_available),
  identical(boot_a$valid_positions, boot_b$valid_positions),
  isTRUE(all.equal(boot_a$statedu_boot_paths, boot_b$statedu_boot_paths, tolerance = 0)),
  identical(boot_a$bootstrapped_moderation_effects, boot_b$bootstrapped_moderation_effects),
  isTRUE(boot_a$statedu_moderation_bootstrap_contract$interaction_reestimated_each_draw),
  identical(boot_a$statedu_moderation_bootstrap_contract$methods, "two_stage"),
  identical(dimnames(boot_a$statedu_boot_paths)[[3L]], as.character(boot_a$valid_positions))
)
# The PLS bootstrap moderation table intentionally uses lowercase `p`.  The
# result snapshot must display coefficient(p) and expose the exact
# significance/dashing contract consumed by the canvas renderer.
stopifnot("p" %in% names(boot_a$bootstrapped_moderation_effects))
result_snapshot <- structural_canvas_result_snapshot(
  two_stage$fixture$snapshot, two_stage$result$fit,
  coefficient = "pls_p", bootstrap = boot_a,
  measurement_coefficient = "measurement_p"
)
snapshot_moderation <- result_snapshot$moderations[[1L]]
expected_interaction_p <- boot_a$bootstrapped_moderation_effects$p[[1L]]
stopifnot(
  length(result_snapshot$moderations) == 1L,
  isTRUE(snapshot_moderation$resultMatched),
  isTRUE(all.equal(snapshot_moderation$p, expected_interaction_p, tolerance = 0)),
  grepl("(", snapshot_moderation$label, fixed = TRUE),
  identical(snapshot_moderation$significant, expected_interaction_p < .05),
  isTRUE(snapshot_moderation$dashEligible),
  isTRUE(result_snapshot$dashNonsignificant),
  identical(
    snapshot_moderation$inferenceSource,
    boot_a$bootstrapped_moderation_effects[["Inference Source"]][[1L]]
  )
)

nonsignificant_boot <- boot_a
nonsignificant_boot$bootstrapped_moderation_effects$p[] <- .25
nonsignificant_snapshot <- structural_canvas_result_snapshot(
  two_stage$fixture$snapshot, two_stage$result$fit,
  coefficient = "pls_p", bootstrap = nonsignificant_boot
)
nonsignificant_moderation <- nonsignificant_snapshot$moderations[[1L]]
stopifnot(
  identical(nonsignificant_moderation$p, .25),
  identical(nonsignificant_moderation$significant, FALSE),
  isTRUE(nonsignificant_moderation$dashEligible),
  isTRUE(nonsignificant_snapshot$dashNonsignificant)
)

point_snapshot <- structural_canvas_result_snapshot(
  two_stage$fixture$snapshot, two_stage$result$fit,
  coefficient = "pls_value", bootstrap = boot_a
)
stopifnot(
  !"significant" %in% names(point_snapshot$moderations[[1L]]),
  !isTRUE(point_snapshot$moderations[[1L]]$dashEligible),
  !grepl("(", point_snapshot$moderations[[1L]]$label, fixed = TRUE)
)

old_pls_bootstrap_workers <- getOption("statedu.pls.bootstrap.workers")
options(statedu.pls.bootstrap.workers = 1L)
plsc_boot <- suppressWarnings(structural_canvas_run_plsc_bootstrap(
  plsc$result$fit, nboot = 20L, seed = bootstrap_seed, apply_plsc = TRUE
))
options(statedu.pls.bootstrap.workers = old_pls_bootstrap_workers)
message(
  "PLSc moderation bootstrap completed: ", plsc_boot$nboot, "/",
  plsc_boot$requested_nboot, " valid (", signif(plsc_boot$valid_ratio, 4), "); failures=",
  paste(names(plsc_boot$failure_counts), unlist(plsc_boot$failure_counts), sep = ":", collapse = ", "), "."
)
stopifnot(
  plsc_boot$nboot > 0L,
  identical(
    isTRUE(plsc_boot$inference_available),
    isTRUE(plsc_boot$valid_ratio >= structural_canvas_pls_bootstrap_min_valid_ratio())
  ),
  nrow(plsc_boot$bootstrapped_moderation_effects) == 1L,
  nrow(plsc_boot$bootstrapped_moderation_simple_slopes) == 3L,
  all(c("BH-adjusted p", "Bootstrap Status", "Inference Source") %in%
    names(plsc_boot$bootstrapped_moderation_effects)),
  all(c("BH-adjusted p", "Bootstrap Status", "Inference Source") %in%
    names(plsc_boot$bootstrapped_moderation_simple_slopes)),
  all(
    plsc_boot$bootstrapped_moderation_effects[["Inference available"]] ==
      isTRUE(plsc_boot$inference_available)
  ),
  grepl("Uncorrected composite", plsc_boot$bootstrapped_moderation_effects[["PLSc interaction correction"]][[1L]], fixed = TRUE)
)
if (isTRUE(plsc_boot$inference_available)) {
  stopifnot(
    all(is.finite(plsc_boot$bootstrapped_moderation_effects$p)),
    all(is.finite(plsc_boot$bootstrapped_moderation_effects[["BH-adjusted p"]])),
    all(is.finite(plsc_boot$bootstrapped_moderation_simple_slopes[["BH-adjusted p"]])),
    all(plsc_boot$bootstrapped_moderation_simple_slopes[["Bootstrap Status"]] == "Adequate"),
    all(nzchar(plsc_boot$bootstrapped_moderation_simple_slopes[["Inference Source"]]))
  )
} else {
  stopifnot(
    all(is.na(plsc_boot$bootstrapped_moderation_effects$p)),
    all(is.na(plsc_boot$bootstrapped_moderation_effects[["BH-adjusted p"]])),
    all(is.na(plsc_boot$bootstrapped_moderation_simple_slopes[["BH-adjusted p"]])),
    all(is.na(plsc_boot$bootstrapped_moderation_simple_slopes[["95% CI lower"]]))
  )
}

# Point estimates and audit counts remain available when the 80% whole-draw
# gate suppresses all interval/p-value inference.
point_paths <- as.matrix(two_stage$result$fit$path_coef)
suppressed_paths <- array(
  rep(point_paths, 4L),
  dim = c(nrow(point_paths), ncol(point_paths), 4L),
  dimnames = list(rownames(point_paths), colnames(point_paths), as.character(seq_len(4L)))
)
suppressed <- structural_canvas_pls_moderation_bootstrap_tables(
  point_paths, suppressed_paths,
  two_stage$result$moderation_definitions,
  requested_nboot = 10L, estimator = "PLS"
)
stopifnot(
  !isTRUE(suppressed$validity$adequate),
  all(is.finite(suppressed$effects$Estimate)),
  all(is.finite(suppressed$simple_slopes[["Simple slope"]])),
  all(is.na(suppressed$effects$p)),
  all(is.na(suppressed$simple_slopes[["BH-adjusted p"]])),
  all(suppressed$simple_slopes[["Valid replicates"]] == 4L),
  all(suppressed$simple_slopes[["Requested replicates"]] == 10L),
  all(abs(suppressed$simple_slopes[["Valid ratio"]] - .4) < 1e-12),
  all(!suppressed$simple_slopes[["Inference available"]]),
  all(suppressed$simple_slopes[["Bootstrap Status"]] == "Insufficient"),
  all(grepl("suppressed", suppressed$simple_slopes[["Inference Source"]], fixed = TRUE))
)

message("PLS/PLSc latent-moderation core validation passed.")
