source(file.path("scripts", "smartpls_tam_reference_common.R"), encoding = "UTF-8")

pls <- run_structural_canvas_analysis(snapshot, tam_data[indicators], "plssem", estimator = "PLS")
plsc <- run_structural_canvas_analysis(snapshot, tam_data[indicators], "plssem", estimator = "PLSc")
stopifnot(
  !length(seminr:::all_constructs_of_mode(pls$fit$mmMatrix, "C")),
  any(abs(pls$fit$path_coef - plsc$fit$path_coef) > 1e-8)
)

fit_values <- function(result, estimator) {
  bundle <- list(
    fit = result$fit, diagnostics = result, estimator = estimator,
    snapshot = snapshot, analysis_data = tam_data[indicators]
  )
  structural_canvas_pls_approximate_fit_indices(bundle, summary(result$fit))
}
pls_fit <- fit_values(pls, "PLS")
plsc_fit <- fit_values(plsc, "PLSC")

rounded_reference <- rbind(
  pls = c(srmr = .077, d_g = .882, d_uls = 1.514),
  plsc = c(srmr = .080, d_g = NA_real_, d_uls = 1.601)
)
actual <- rbind(pls = pls_fit[c("srmr", "d_g", "d_uls")], plsc = plsc_fit[c("srmr", "d_g", "d_uls")])
stopifnot(
  abs(actual["pls", "srmr"] - rounded_reference["pls", "srmr"]) <= .0005,
  abs(actual["pls", "d_g"] - rounded_reference["pls", "d_g"]) <= .0005,
  abs(actual["pls", "d_uls"] - rounded_reference["pls", "d_uls"]) <= .0005,
  abs(actual["plsc", "srmr"] - rounded_reference["plsc", "srmr"]) <= .0005,
  is.na(actual["plsc", "d_g"]),
  abs(actual["plsc", "d_uls"] - rounded_reference["plsc", "d_uls"]) <= .0005
)

path_values <- function(model) vapply(path_pairs, function(pair) model$path_coef[pair[[1L]], pair[[2L]]], numeric(1))
pls_paths <- path_values(pls$fit)
plsc_paths <- path_values(plsc$fit)
stopifnot(
  max(abs(pls_paths - c(.361, .264, .332, .227, .339, .203, .193))) <= .0005,
  max(abs(plsc_paths - c(.388, .306, .352, .229, .380, .200, .253))) <= .0005
)

options(digits = 17)
print(actual)
message("SmartPLS TAM 100-row PLS/PLSc reference validation passed.")
