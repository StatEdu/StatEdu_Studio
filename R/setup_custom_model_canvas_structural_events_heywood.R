structural_canvas_heywood_constraint_settings <- function(bundle, data, percent) {
  if (!toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") || length(bundle$ordered %||% character(0))) {
    stop("Heywood-constrained reanalysis is available only for continuous indicators estimated with ML or MLR.")
  }

  variables <- as.character((bundle$baseline_diagnostics %||% bundle$diagnostics)$negative_residuals %||% character(0))
  if (!length(variables)) stop("No negative residual variances were found in the original model.")

  if (!is.finite(percent) || percent < 0.01 || percent > 5) stop("Enter a percentage between 0.01 and 5.")

  observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
  if (any(!is.finite(observed_variances) | observed_variances <= 0)) {
    stop("A positive observed variance is required for every Heywood indicator.")
  }

  fixes <- observed_variances * percent / 100
  names(fixes) <- variables

  settings <- bundle
  settings$residual_variance_fixes <- fixes
  settings$comparison_label <- "Heywood-constrained model"
  settings$comparison_type <- "heywood"

  list(settings = settings, variables = variables, percent = percent)
}
