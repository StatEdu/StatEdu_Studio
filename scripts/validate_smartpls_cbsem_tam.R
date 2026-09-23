source(file.path("scripts", "smartpls_tam_reference_common.R"), encoding = "UTF-8")

fixture_path <- file.path("sample", "smartpls4118_cbsem_tam100_results.csv")
fixture <- utils::read.csv(fixture_path, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(
  nrow(fixture) == 25L,
  identical(names(fixture), c("category", "key", "smartpls_value", "reported_decimals")),
  all(fixture$reported_decimals == 3L)
)

fit_cbsem <- function(likelihood) {
  run_structural_canvas_analysis(
    snapshot,
    tam_data[indicators],
    "cbsem",
    estimator = "ML",
    missing = "listwise",
    ml_likelihood = likelihood
  )
}

normal <- fit_cbsem("normal")
wishart <- fit_cbsem("wishart")
fit_measure_names <- c(
  "chisq", "npar", "df", "pvalue", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
  "gfi_lisrel", "agfi", "pgfi", "srmr", "nfi", "tli", "cfi"
)
extract_fit_measures <- function(fit) {
  setNames(vapply(fit_measure_names, function(name) {
    values <- lavaan::fitMeasures(fit, fit.measures = name)
    unname(values[which(names(values) == name)[1L]])
  }, numeric(1)), fit_measure_names)
}
normal_fit <- extract_fit_measures(normal$fit)
wishart_fit <- extract_fit_measures(wishart$fit)
normal_options <- lavaan::lavInspect(normal$fit, "options")
wishart_options <- lavaan::lavInspect(wishart$fit, "options")
stopifnot(
  identical(normal$ml_likelihood, "normal"),
  identical(wishart$ml_likelihood, "wishart"),
  identical(normal_options$likelihood, "normal"),
  identical(wishart_options$likelihood, "wishart")
)

fit_reference <- fixture[fixture$category == "fit", , drop = FALSE]
fit_actual <- c(
  chisq = normal_fit[["chisq"]],
  npar = normal_fit[["npar"]],
  nobs = lavaan::lavInspect(normal$fit, "nobs"),
  df = normal_fit[["df"]],
  pvalue = normal_fit[["pvalue"]],
  chisq_df = normal_fit[["chisq"]] / normal_fit[["df"]],
  rmsea = normal_fit[["rmsea"]],
  rmsea_ci_lower = normal_fit[["rmsea.ci.lower"]],
  rmsea_ci_upper = normal_fit[["rmsea.ci.upper"]],
  gfi = normal_fit[["gfi_lisrel"]],
  agfi = normal_fit[["agfi"]],
  pgfi = normal_fit[["pgfi"]],
  srmr = normal_fit[["srmr"]],
  nfi = normal_fit[["nfi"]],
  tli = normal_fit[["tli"]],
  cfi = normal_fit[["cfi"]],
  aic_discrepancy = normal_fit[["chisq"]] + 2 * normal_fit[["npar"]],
  bic_discrepancy = normal_fit[["chisq"]] + log(lavaan::lavInspect(normal$fit, "nobs")) * normal_fit[["npar"]]
)
stopifnot(identical(names(fit_actual), fit_reference$key))
fit_tolerance <- 0.5 * 10^(-fit_reference$reported_decimals)
fit_difference <- abs(unname(fit_actual) - fit_reference$smartpls_value)
if (any(fit_difference > fit_tolerance)) {
  failed <- which(fit_difference > fit_tolerance)
  stop(
    "SmartPLS fit comparison failed: ",
    paste(
      paste0(
        fit_reference$key[failed], " actual=", format(unname(fit_actual)[failed], digits = 16),
        " reference=", fit_reference$smartpls_value[failed]
      ),
      collapse = "; "
    ),
    call. = FALSE
  )
}

standardized <- lavaan::standardizedSolution(normal$fit)
path_lookup <- setNames(
  standardized$est.std[standardized$op == "~"],
  paste0(standardized$rhs[standardized$op == "~"], "->", standardized$lhs[standardized$op == "~"])
)
path_reference <- fixture[fixture$category == "path", , drop = FALSE]
stopifnot(all(path_reference$key %in% names(path_lookup)))
path_tolerance <- 0.5 * 10^(-path_reference$reported_decimals)
stopifnot(all(abs(path_lookup[path_reference$key] - path_reference$smartpls_value) <= path_tolerance))

# The same sample covariance and standardized solution are retained, while the
# Wishart N-1 multiplier changes the raw chi-square and its derived indices.
standardized_difference <- max(abs(
  lavaan::standardizedSolution(normal$fit)$est.std -
    lavaan::standardizedSolution(wishart$fit)$est.std
), na.rm = TRUE)
stopifnot(
  abs(wishart_fit[["chisq"]] - normal_fit[["chisq"]]) > 4,
  standardized_difference < 1e-5
)

cat(
  "SmartPLS 4.1.1.8 TAM 100-row CB-SEM validation passed: ",
  "Normal ML matches all 25 displayed fit/path values; Wishart ML is correctly distinct.\n",
  sep = ""
)
