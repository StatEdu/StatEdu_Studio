#!/usr/bin/env Rscript

if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("PLSc f-squared validation requires a Windows UTF-8 locale.", call. = FALSE)
  }
}

source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages()
source_app_modules()
source("scripts/pls_validation_fixture.R", local = environment(), encoding = "UTF-8")

assert_close <- function(actual, expected, tolerance = 1e-8, message = "values differ") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance))) {
    stop(sprintf("%s (actual=%s, expected=%s)", message, format(actual, digits = 15), format(expected, digits = 15)), call. = FALSE)
  }
}

node <- function(id, name, role, ...) c(list(id = id, name = name, canvasLabel = name, role = role), list(...))

# Independent formula oracle. The stub supplies reduced-model R-squared values;
# the production helper must remove the correct edge and apply
# (R2 included - R2 excluded) / (1 - R2 included) without consulting seminr's
# native fSquare table.
oracle_latents <- list(
  node("lx", "X", "latent", constructType = "commonFactor", measurementMode = "reflective"),
  node("lm", "M", "latent", constructType = "commonFactor", measurementMode = "reflective"),
  node("ly", "Y", "latent", constructType = "commonFactor", measurementMode = "reflective")
)
oracle_edges <- list(
  list(id = "xy", from = "lx", to = "ly", kind = "directed"),
  list(id = "my", from = "lm", to = "ly", kind = "directed")
)
oracle_snapshot <- list(nodes = oracle_latents, edges = oracle_edges)
oracle_summary <- list(fSquare = matrix(0, 3L, 3L, dimnames = list(c("X", "M", "Y"), c("X", "M", "Y"))))
oracle_bundle <- list(
  fit = list(
    rSquared = matrix(c(.60, .55), 2L, 1L, dimnames = list(c("Rsq", "AdjRsq"), "Y")),
    rawdata = data.frame(dummy = 1:4),
    statedu_common_factor_constructs = c("X", "M", "Y")
  ),
  snapshot = oracle_snapshot,
  estimator = "PLSc",
  diagnostics = list(estimator = "PLSc", plsc_corrected_constructs = c("X", "M", "Y"))
)
oracle_runner <- function(snapshot, data, latents, edges, estimator) {
  remaining <- structural_canvas_pls_f_square_edge_rows(snapshot)
  excluded_r2 <- if (any(remaining$predictor == "M" & remaining$outcome == "Y")) .50 else .30
  list(
    fit = list(rSquared = matrix(c(excluded_r2, excluded_r2), 2L, 1L, dimnames = list(c("Rsq", "AdjRsq"), "Y"))),
    estimator = "PLSc",
    plsc_corrected_constructs = c("X", "M", "Y"),
    converged = TRUE,
    admissible = TRUE
  )
}
oracle <- structural_canvas_pls_f_square_for_reporting(
  oracle_bundle, oracle_summary, runner = oracle_runner, force_refit = TRUE, use_cache = FALSE
)
stopifnot(isTRUE(oracle$complete), !nrow(oracle$failures))
assert_close(oracle$values["X", "Y"], (.60 - .50) / (1 - .60), message = "X -> Y f-squared formula oracle failed")
assert_close(oracle$values["M", "Y"], (.60 - .30) / (1 - .60), message = "M -> Y f-squared formula oracle failed")

make_mobi_snapshot <- function(include_expect_path = TRUE) {
  latents <- list(
    node("l_image", "Image", "latent", constructType = "commonFactor", measurementMode = "reflective"),
    node("l_expect", "Expect", "latent", constructType = "commonFactor", measurementMode = "reflective"),
    node("l_quality", "Quality", "latent", constructType = "commonFactor", measurementMode = "reflective")
  )
  indicators <- list()
  edges <- list()
  add_block <- function(latent, prefix, count) {
    for (index in seq_len(count)) {
      indicator_id <- paste0("i_", prefix, index)
      indicators[[length(indicators) + 1L]] <<- node(indicator_id, paste0(prefix, index), "indicator")
      edges[[length(edges) + 1L]] <<- list(id = paste0("m_", indicator_id), from = latent$id, to = indicator_id, kind = "directed")
    }
  }
  add_block(latents[[1L]], "IMAG", 5L)
  add_block(latents[[2L]], "CUEX", 3L)
  add_block(latents[[3L]], "PERQ", 7L)
  structural_edges <- list(
    list(id = "image_quality", from = "l_image", to = "l_quality", kind = "directed"),
    list(id = "expect_quality", from = "l_expect", to = "l_quality", kind = "directed")
  )
  if (isTRUE(include_expect_path)) {
    structural_edges <- c(list(list(id = "image_expect", from = "l_image", to = "l_expect", kind = "directed")), structural_edges)
  }
  list(snapshot = list(nodes = c(latents, indicators), edges = c(edges, structural_edges)), latents = latents)
}

mobi <- statedu_pls_validation_fixture()
fixture <- make_mobi_snapshot(TRUE)

# Ordinary PLS remains numerically compatible with seminr because both full
# and reduced models use the same PLS estimator.
pls <- structural_canvas_run_pls_analysis(fixture$snapshot, mobi, fixture$latents, fixture$snapshot$edges, "PLS")
pls_summary <- summary(pls$fit)
pls_summary_without_native_f2 <- structural_canvas_pls_summary(pls$fit)
for (name in setdiff(names(pls_summary), "fSquare")) {
  stopifnot(isTRUE(all.equal(pls_summary_without_native_f2[[name]], pls_summary[[name]], tolerance = 1e-12)))
}
pls_bundle <- list(fit = pls$fit, snapshot = fixture$snapshot, diagnostics = pls, estimator = "PLS", analysis_data = mobi)
pls_refit <- structural_canvas_pls_f_square_for_reporting(
  pls_bundle, pls_summary, force_refit = TRUE, use_cache = FALSE
)
stopifnot(isTRUE(pls_refit$complete))
pls_paths <- structural_canvas_pls_f_square_edge_rows(fixture$snapshot)
for (index in seq_len(nrow(pls_paths))) {
  predictor <- pls_paths$predictor[[index]]
  outcome <- pls_paths$outcome[[index]]
  assert_close(
    pls_refit$values[predictor, outcome],
    pls_summary$fSquare[predictor, outcome],
    tolerance = 1e-7,
    message = paste0("plain PLS f-squared regression compatibility failed for ", predictor, " -> ", outcome)
  )
}

# PLSc must not reuse seminr's plain-PLS reduced model. This deterministic
# fixture exposes the estimator mismatch without relying on package example
# data, which is intentionally omitted from the bundled runtime.
plsc <- structural_canvas_run_pls_analysis(fixture$snapshot, mobi, fixture$latents, fixture$snapshot$edges, "PLSc")
plsc_summary <- summary(plsc$fit)
plsc_bundle <- list(
  fit = plsc$fit, snapshot = fixture$snapshot, diagnostics = plsc, estimator = "PLSc",
  analysis_data = mobi, analysis_run_id = "validate-plsc-f2"
)
plsc_refit <- structural_canvas_pls_f_square_for_reporting(plsc_bundle, plsc_summary, use_cache = FALSE)
stopifnot(isTRUE(plsc_refit$complete))
assert_close(plsc$fit$rSquared["Rsq", "Quality"], .5403516, tolerance = 1e-6, message = "PLSc fixture full R-squared changed")
assert_close(plsc_summary$fSquare["Image", "Quality"], .4113365, tolerance = 1e-6, message = "PLSc fixture no longer exposes the native plain-PLS mismatch")
assert_close(plsc_refit$values["Image", "Quality"], .1097537, tolerance = 1e-6, message = "PLSc estimator-consistent f-squared changed")
stopifnot(abs(plsc_refit$values["Image", "Quality"] - plsc_summary$fSquare["Image", "Quality"]) > .25)

# The public result table must use the estimator-consistent value.
fit_table <- structural_canvas_pls_fit_result_table(
  plsc_summary, plsc, identity, bootstrap = NULL, f_square_result = plsc_refit
)
image_quality <- fit_table$Predictor == "Image" & fit_table$Outcome == "Quality" & fit_table$Effect == "Direct"
stopifnot(sum(image_quality) == 1L)
stopifnot(
  abs(as.numeric(fit_table$f2[image_quality]) - plsc_refit$values["Image", "Quality"]) <= .005
)
stopifnot(grepl("PLSc estimator-consistent", fit_table$`f2 status`[image_quality], fixed = TRUE))

# The quality panel must also use the corrected matrix. In this two-path
# fixture the formerly inflated native maximum is replaced by the corrected
# maximum.
quality_fixture <- make_mobi_snapshot(FALSE)
quality_fit <- structural_canvas_run_pls_analysis(quality_fixture$snapshot, mobi, quality_fixture$latents, quality_fixture$snapshot$edges, "PLSc")
quality_bundle <- list(
  fit = quality_fit$fit, snapshot = quality_fixture$snapshot, diagnostics = quality_fit,
  estimator = "PLSc", analysis_data = mobi, analysis_run_id = "validate-plsc-f2-quality"
)
quality_f2 <- structural_canvas_pls_f_square_for_reporting(quality_bundle, summary(quality_fit$fit), use_cache = FALSE)
quality_rows <- structural_canvas_pls_quality_rows(quality_bundle)
quality_max <- quality_rows$Value[quality_rows$Item == "Max f2"]
stopifnot(length(quality_max) == 1L)
stopifnot(identical(quality_max, format_decimal3(max(quality_f2$values, na.rm = TRUE))))
assert_close(max(quality_f2$values, na.rm = TRUE), .5396748, tolerance = 1e-6, message = "PLSc corrected quality-panel maximum changed")
assert_close(max(summary(quality_fit$fit)$fSquare, na.rm = TRUE), .6556073, tolerance = 1e-6, message = "PLSc native quality-panel mismatch changed")
stopifnot(abs(as.numeric(quality_max) - max(summary(quality_fit$fit)$fSquare, na.rm = TRUE)) > .1)

# A nonconverged reduced model is fail-closed: the affected value is NA and a
# reason is retained for the guide table instead of falling back to native PLS.
invalid_runner <- function(snapshot, data, latents, edges, estimator) {
  list(
    fit = list(rSquared = matrix(c(.4, .4), 2L, 1L, dimnames = list(c("Rsq", "AdjRsq"), "Y"))),
    estimator = "PLSc", plsc_corrected_constructs = c("X", "M", "Y"),
    converged = FALSE, admissible = TRUE
  )
}
invalid <- structural_canvas_pls_f_square_for_reporting(
  oracle_bundle, oracle_summary, runner = invalid_runner, force_refit = TRUE, use_cache = FALSE
)
stopifnot(!isTRUE(invalid$complete), nrow(invalid$failures) == 2L)
stopifnot(all(is.na(invalid$values[c("X", "M"), "Y"])))
stopifnot(all(grepl("did not converge", invalid$failures$Reason, fixed = TRUE)))
stopifnot(all(grepl("Not reported", invalid$status[c("X", "M"), "Y"], fixed = TRUE)))

inadmissible_runner <- function(snapshot, data, latents, edges, estimator) {
  list(
    fit = list(rSquared = matrix(c(.4, .4), 2L, 1L, dimnames = list(c("Rsq", "AdjRsq"), "Y"))),
    estimator = "PLSc", plsc_corrected_constructs = c("X", "M", "Y"),
    converged = TRUE, admissible = FALSE
  )
}
inadmissible <- structural_canvas_pls_f_square_for_reporting(
  oracle_bundle, oracle_summary, runner = inadmissible_runner, force_refit = TRUE, use_cache = FALSE
)
stopifnot(!isTRUE(inadmissible$complete), nrow(inadmissible$failures) == 2L)
stopifnot(all(is.na(inadmissible$values[c("X", "M"), "Y"])))
stopifnot(all(grepl("numerically inadmissible", inadmissible$failures$Reason, fixed = TRUE)))

cat("PLSc estimator-consistent f-squared validation PASS\n")
