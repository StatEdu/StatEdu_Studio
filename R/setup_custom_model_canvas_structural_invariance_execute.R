# Structural equation canvas measurement-invariance execution helpers.

structural_canvas_run_measurement_invariance <- function(analysis_type, invariance_enabled, result, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered) {
  invariance_result <- NULL
  if (identical(analysis_type, "cfa") && invariance_enabled) {
    if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop("Continuous-indicator measurement invariance requires ML or MLR.")
    if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop("Ordered-indicator measurement invariance requires WLSMV or DWLS.")
    if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop("Select a valid grouping variable for measurement invariance analysis.")
    if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop("The grouping variable cannot also be an indicator in the CFA model.")
    group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
    if (group_count < 2L || group_count > 20L) stop("The grouping variable must contain between 2 and 20 non-empty groups.")
    invariance_result <- structural_canvas_with_progress(message = "Estimating measurement-invariance models", value = 0, {
      structural_canvas_inc_progress(.15, detail = "Configural, metric, scalar, and strict models")
      value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered)
      structural_canvas_inc_progress(.85, detail = "Preparing robust comparisons")
      value
    })
  }
  invariance_result
}
