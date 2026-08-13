# Structural equation canvas HTMT bootstrap execution helpers.

structural_canvas_run_htmt_bootstrap <- function(analysis_type, htmt_bootstrap, result, data, htmt_seed, ordered, htmt_threshold, htmt_ci_method) {
  htmt_bootstrap_result <- NULL
  if (analysis_type %in% c("cfa", "cbsem") && htmt_bootstrap > 0L) {
    standardized_for_htmt <- lavaan::standardizedSolution(result$fit)
    observed_for_htmt <- lavaan::lavNames(result$fit, "ov")
    loadings_for_htmt <- standardized_for_htmt[
      standardized_for_htmt$op == "=~" & standardized_for_htmt$rhs %in% observed_for_htmt,
      c("lhs", "rhs"), drop = FALSE
    ]
    factor_names_for_htmt <- unique(loadings_for_htmt$lhs)
    if (length(factor_names_for_htmt) >= 2L) {
      indicators_for_htmt <- stats::setNames(lapply(factor_names_for_htmt, function(name) {
        unique(loadings_for_htmt$rhs[loadings_for_htmt$lhs == name])
      }), factor_names_for_htmt)
      htmt_bootstrap_result <- shiny::withProgress(
        message = "Estimating HTMT bootstrap confidence intervals",
        value = 0,
        {
          shiny::incProgress(.1, detail = paste0(htmt_bootstrap, " case-resampling replicates"))
          value <- structural_canvas_htmt_bootstrap(
            data, indicators_for_htmt, reps = htmt_bootstrap, confidence = .95,
            seed = htmt_seed, ordered = ordered, threshold = htmt_threshold,
            ci_method = htmt_ci_method,
            progress = function(done, total, valid) {
              shiny::setProgress(
                value = .10 + .80 * (as.numeric(done) / max(1, as.numeric(total))),
                detail = sprintf("HTMT bootstrap %s/%s; valid replicates %s", done, total, valid)
              )
            }
          )
          shiny::incProgress(.9, detail = "Preparing interval estimates")
          value
        }
      )
    }
  }
  htmt_bootstrap_result
}
