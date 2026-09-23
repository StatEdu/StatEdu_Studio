#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x
default_seed <- function() 24680L

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(value, expected, message) {
  if (!identical(value, expected)) {
    stop(paste0(message, " (expected ", deparse(expected), ", got ", deparse(value), ")"), call. = FALSE)
  }
}

if (!requireNamespace("seminr", quietly = TRUE)) {
  stop("seminr is required for the PLS fail-closed validation.", call. = FALSE)
}

source("R/setup_custom_model_canvas_structural_pls_engine.R", local = globalenv(), encoding = "UTF-8")
source("scripts/pls_validation_fixture.R", local = environment(), encoding = "UTF-8")

set.seed(1977L)
fixture_rng_before <- .Random.seed
fixture_kind_before <- RNGkind()
mobi <- statedu_pls_validation_fixture()
assert_equal(.Random.seed, fixture_rng_before, "PLS validation fixture changed the caller's RNG state")
assert_equal(RNGkind(), fixture_kind_before, "PLS validation fixture changed the caller's RNG kind")
assert_true(identical(mobi, statedu_pls_validation_fixture()), "PLS validation fixture is not deterministic.")
measurement_model <- seminr::constructs(
  seminr::composite("Image", seminr::multi_items("IMAG", 1:5), weights = seminr::mode_A),
  seminr::composite("Expect", seminr::multi_items("CUEX", 1:3), weights = seminr::mode_A)
)
structural_model <- seminr::relationships(seminr::paths(from = "Image", to = "Expect"))
fit <- seminr::estimate_pls(
  data = mobi,
  measurement_model = measurement_model,
  structural_model = structural_model,
  missing = seminr::mean_replacement,
  maxIt = structural_canvas_pls_max_iterations(),
  stopCriterion = structural_canvas_pls_stop_criterion(),
  assess_syntax = FALSE
)

diagnostics <- structural_canvas_pls_fit_diagnostics(fit, "PLS")
assert_true(diagnostics$converged, "A regular PLS solution must satisfy the 1e-7 convergence threshold.")
assert_true(diagnostics$admissible, "A regular PLS solution must pass the numerical admissibility gate.")
assert_equal(diagnostics$max_iterations, 300L, "PLS maxIt contract changed")
assert_equal(diagnostics$stop_criterion, 7L, "PLS stopCriterion contract changed")

nonconverged <- fit
nonconverged$iterations <- structural_canvas_pls_max_iterations()
nonconverged$weightDiff <- 1e-4
nonconvergence_gate <- structural_canvas_pls_bootstrap_draw_gate(nonconverged, "PLS")
assert_true(!nonconvergence_gate$valid, "A nonconverged PLS bootstrap draw must be rejected.")
assert_equal(nonconvergence_gate$reason, "nonconvergence", "Nonconvergence must have a distinct bootstrap failure reason")
nonconvergence_error <- tryCatch(
  {
    structural_canvas_pls_assert_fit(nonconverged, "PLS", stage = "validation solution")
    ""
  },
  error = conditionMessage
)
assert_true(grepl("did not converge", nonconvergence_error, fixed = TRUE), "Base PLS nonconvergence must fail closed with a clear error.")

invalid_r2 <- fit
invalid_r2$rSquared[1L, 1L] <- 1.25
inadmissible_gate <- structural_canvas_pls_bootstrap_draw_gate(invalid_r2, "PLS")
assert_true(!inadmissible_gate$valid, "A PLS bootstrap draw with impossible R-squared must be rejected.")
assert_equal(inadmissible_gate$reason, "inadmissible", "Numerical inadmissibility must have a distinct bootstrap failure reason")

plsc_fit <- structural_canvas_apply_plsc(fit, c("Image", "Expect"))
plsc_diagnostics <- structural_canvas_pls_fit_diagnostics(plsc_fit, "PLSC", c("Image", "Expect"))
assert_true(plsc_diagnostics$converged, "PLSc must preserve the converged PLS outer-weight solution.")
assert_true(plsc_diagnostics$admissible, "A regular PLSc solution must pass rho_A/correlation/PD/path/R-squared checks.")
assert_true(is.finite(plsc_diagnostics$minimum_adjusted_correlation_eigenvalue) &&
  plsc_diagnostics$minimum_adjusted_correlation_eigenvalue > 0,
"A regular PLSc disattenuated correlation matrix must be positive definite.")

invalid_rho <- plsc_fit
invalid_rho$statedu_plsc_rho_a["Image", 1L] <- 1.1
invalid_rho_gate <- structural_canvas_pls_bootstrap_draw_gate(invalid_rho, "PLSC", c("Image", "Expect"))
assert_true(!invalid_rho_gate$valid && identical(invalid_rho_gate$reason, "inadmissible"),
  "PLSc rho_A outside (0, 1] must be rejected.")

invalid_pd <- plsc_fit
invalid_pd$statedu_plsc_adjusted_correlations[1L, 2L] <- 1
invalid_pd$statedu_plsc_adjusted_correlations[2L, 1L] <- 1
invalid_pd_gate <- structural_canvas_pls_bootstrap_draw_gate(invalid_pd, "PLSC", c("Image", "Expect"))
assert_true(!invalid_pd_gate$valid && identical(invalid_pd_gate$reason, "inadmissible"),
  "A non-positive-definite PLSc disattenuated correlation matrix must be rejected.")

# A bootstrap resample may retain a globally non-positive-definite corrected
# correlation matrix only as a diagnostic warning. The local equations and all
# downstream finite/range contracts are still enforced before acceptance.
bootstrap_pd <- invalid_pd
bootstrap_pd$statedu_plsc_bootstrap_resample <- TRUE
bootstrap_pd_gate <- structural_canvas_pls_bootstrap_draw_gate(bootstrap_pd, "PLSC", c("Image", "Expect"))
assert_true(bootstrap_pd_gate$valid,
  "A PLSc bootstrap resample with solvable local equations must not fail only because the global corrected matrix is non-positive-definite.")
assert_true(any(grepl("non-positive-definite global", bootstrap_pd_gate$diagnostics$diagnostic_warnings, fixed = TRUE)),
  "Retained non-positive-definite PLSc bootstrap resamples must carry an explicit diagnostic warning.")

metadata <- structural_canvas_pls_bootstrap_contract_metadata(
  list(),
  structural_canvas_pls_bootstrap_validity(8L, 10L),
  c("nonconvergence", "inadmissible"),
  valid_positions = 3:10,
  seed = 24680L
)
assert_equal(metadata$nonconvergence_failures, 1L, "Bootstrap metadata must count nonconverged draws")
assert_equal(metadata$inadmissible_failures, 1L, "Bootstrap metadata must count inadmissible draws")
assert_true(isTRUE(metadata$inference_available), "The existing 80% valid-draw inference contract must remain intact.")

engine_text <- paste(readLines("R/setup_custom_model_canvas_structural_pls_engine.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
explicit_missing_calls <- gregexpr("missing = seminr::mean_replacement", engine_text, fixed = TRUE)[[1L]]
assert_true(all(explicit_missing_calls > 0L) && length(explicit_missing_calls) >= 2L,
  "Both the base and bootstrap estimate_pls calls must explicitly use seminr mean replacement.")

bootstrap <- structural_canvas_run_plsc_bootstrap(
  plsc_fit, nboot = 12L, seed = 24680L, apply_plsc = TRUE
)
assert_equal(bootstrap$requested_nboot, 12L, "PLSc bootstrap must preserve the requested draw count")
assert_true(bootstrap$nboot >= 2L, "The integration fixture must retain enough converged and admissible PLSc draws.")
assert_true(bootstrap$execution_failures == 0L,
  "Parallel PLSc bootstrap workers must receive the fail-closed gate helpers.")
assert_true(is.numeric(bootstrap$retained_nonpositive_definite_plsc_draws) &&
  bootstrap$retained_nonpositive_definite_plsc_draws >= 0L &&
  bootstrap$retained_nonpositive_definite_plsc_draws <= bootstrap$nboot,
  "PLSc bootstrap metadata must audit retained globally non-positive-definite resamples.")
assert_true(
  bootstrap$nboot + sum(unlist(bootstrap$failure_counts), na.rm = TRUE) == bootstrap$requested_nboot,
  "Every requested bootstrap position must be valid or assigned one exclusive failure reason."
)

cat("PLS fail-closed core validation PASS\n")
