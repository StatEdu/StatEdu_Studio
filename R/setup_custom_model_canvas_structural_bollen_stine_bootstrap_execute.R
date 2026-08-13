# Structural equation canvas Bollen-Stine bootstrap execution helpers.

structural_canvas_run_bollen_stine_bootstrap <- function(analysis_type, bollen_stine_bootstrap, result, bollen_stine_seed) {
  bollen_stine_result <- NULL
  if (identical(analysis_type, "cfa") && bollen_stine_bootstrap > 0L) {
    bollen_stine_result <- shiny::withProgress(message = "Estimating Bollen-Stine global-fit p value", value = 0, {
      shiny::incProgress(.05, detail = paste0(bollen_stine_bootstrap, " transformed-data bootstrap replicates"))
      value <- structural_canvas_bollen_stine(result$fit, bollen_stine_bootstrap, bollen_stine_seed)
      shiny::incProgress(.95, detail = "Preparing bootstrap goodness-of-fit result")
      value
    })
  }
  bollen_stine_result
}
