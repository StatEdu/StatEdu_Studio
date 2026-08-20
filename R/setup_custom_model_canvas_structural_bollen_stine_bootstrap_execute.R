# Structural equation canvas Bollen-Stine bootstrap execution helpers.

structural_canvas_run_bollen_stine_bootstrap <- function(analysis_type, bollen_stine_bootstrap, result, bollen_stine_seed) {
  bollen_stine_result <- NULL
  if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
    bollen_stine_result <- structural_canvas_with_progress(message = "Estimating Bollen-Stine global-fit p value", value = 0, {
      structural_canvas_inc_progress(.05, detail = paste0(bollen_stine_bootstrap, " transformed-data bootstrap replicates"))
      value <- structural_canvas_bollen_stine(
        result$fit, bollen_stine_bootstrap, bollen_stine_seed,
        progress = function(done, total, valid) {
          structural_canvas_set_progress(
            value = .05 + .90 * (as.numeric(done) / max(1, as.numeric(total))),
            detail = sprintf("Bollen-Stine bootstrap %s/%s; valid replicates %s", done, total, valid)
          )
        }
      )
      structural_canvas_inc_progress(.95, detail = "Preparing bootstrap goodness-of-fit result")
      value
    })
  }
  bollen_stine_result
}
