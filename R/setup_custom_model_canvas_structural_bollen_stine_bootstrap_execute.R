# Structural equation canvas Bollen-Stine bootstrap execution helpers.

structural_canvas_run_bollen_stine_bootstrap <- function(analysis_type, bollen_stine_bootstrap, result, bollen_stine_seed, language = "en") {
  tr <- function(text) structural_canvas_reporting_text(text, language)
  bollen_stine_result <- NULL
  if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
    bollen_stine_result <- structural_canvas_with_progress(message = tr("Estimating Bollen-Stine global-fit p value"), value = 0, {
      structural_canvas_inc_progress(.05, detail = sprintf(tr("%s transformed-data bootstrap replicates"), bollen_stine_bootstrap))
      value <- structural_canvas_bollen_stine(
        result$fit, bollen_stine_bootstrap, bollen_stine_seed,
        progress = function(done, total, valid) {
          structural_canvas_set_progress(
            value = .05 + .90 * (as.numeric(done) / max(1, as.numeric(total))),
            detail = sprintf(tr("Bollen-Stine bootstrap %s/%s; valid replicates %s"), done, total, valid)
          )
        }
      )
      structural_canvas_inc_progress(.95, detail = tr("Preparing bootstrap goodness-of-fit result"))
      value
    })
  }
  bollen_stine_result
}
