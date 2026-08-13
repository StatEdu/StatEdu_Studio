# Structural equation canvas reliability bootstrap execution helpers.

structural_canvas_run_reliability_bootstrap <- function(analysis_type, reliability_bootstrap, result, data, reliability_seed, estimator, missing, std_lv, ordered, validity_formula, reliability_ci_method) {
  reliability_bootstrap_result <- NULL
  if (identical(analysis_type, "cfa") && reliability_bootstrap > 0L) {
    structural_canvas_validate_model_based_bootstrap(result$fit, "AVE/reliability bootstrap")
    reliability_bootstrap_result <- structural_canvas_with_progress(message = "Estimating AVE and reliability confidence intervals", value = 0, {
      structural_canvas_inc_progress(.05, detail = paste0(reliability_bootstrap, " case-resampling replicates"))
      value <- structural_canvas_reliability_bootstrap(
        result$syntax, data, reps = reliability_bootstrap, confidence = .95, seed = reliability_seed,
        estimator = estimator, missing = missing, std_lv = std_lv, ordered = ordered, formula_mode = validity_formula,
        original_fit = result$fit,
        ci_method = reliability_ci_method,
        progress = function(done, total, valid) {
          structural_canvas_set_progress(
            value = .05 + .90 * (as.numeric(done) / max(1, as.numeric(total))),
            detail = sprintf("Reliability bootstrap %s/%s; valid replicates %s", done, total, valid)
          )
        }
      )
      if (nrow(value)) {
        point <- structural_canvas_reliability_estimates(result$fit, validity_formula)
        statistic_column <- c(AVE = "AVE", CR = "CR", Alpha = "Alpha", Omega = "Omega")
        value$Estimate <- vapply(seq_len(nrow(value)), function(index) {
          row <- point[point$Factor == value$Factor[[index]], , drop = FALSE]
          column <- statistic_column[[value$Statistic[[index]]]]
          if (nrow(row) && column %in% names(row)) as.numeric(row[[column]][[1L]]) else NA_real_
        }, numeric(1))
        value$`Valid %` <- 100 * value[["Valid replicates"]] / value[["Requested replicates"]]
        value <- value[, c("Factor", "Statistic", "Estimate", "Lower", "Upper", "CI method", "Valid replicates", "Requested replicates", "Valid %", "Status"), drop = FALSE]
      }
      structural_canvas_inc_progress(.95, detail = paste0("Preparing ", if (identical(reliability_ci_method, "bca")) "BCa" else "percentile", " intervals"))
      value
    })
  }
  reliability_bootstrap_result
}
