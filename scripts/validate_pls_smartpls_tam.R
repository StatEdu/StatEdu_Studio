source(file.path("scripts", "smartpls_tam_reference_common.R"), encoding = "UTF-8")

reference_path <- file.path(
  "docs", "evidence", "release_1_2_4", "pls",
  "smartpls_4_1_1_8_tam100_supplement", "external_reference.csv"
)
stopifnot(file.exists(reference_path))
external_reference <- utils::read.csv(reference_path, check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(
  identical(names(external_reference), c("Type", "Estimator", "Key", "SmartPLS")),
  nrow(external_reference) == 20L,
  setequal(tolower(external_reference$Estimator), c("pls", "plsc"))
)
reference_value <- function(type, estimator, key) {
  match <- external_reference[
    external_reference$Type == type & external_reference$Estimator == estimator & external_reference$Key == key,
    "SmartPLS"
  ]
  stopifnot(length(match) == 1L)
  as.numeric(match)
}

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
  pls = vapply(c("srmr", "d_g", "d_uls"), function(key) reference_value("fit", "pls", key), numeric(1)),
  plsc = vapply(c("srmr", "d_g", "d_uls"), function(key) reference_value("fit", "plsc", key), numeric(1))
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
path_reference <- function(estimator) vapply(path_pairs, function(pair) {
  reference_value("path", estimator, paste0(pair[[1L]], "->", pair[[2L]]))
}, numeric(1))
pls_paths <- path_values(pls$fit)
plsc_paths <- path_values(plsc$fit)
stopifnot(
  max(abs(pls_paths - path_reference("pls"))) <= .0005,
  max(abs(plsc_paths - path_reference("plsc"))) <= .0005
)

options(digits = 17)
print(actual)
message("SmartPLS TAM 100-row PLS/PLSc reference validation passed.")
