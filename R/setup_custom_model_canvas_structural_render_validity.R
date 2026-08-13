structural_canvas_register_validity_outputs <- function(output, prefix, analysis_type, fit_result, result_table, app_language_fn = NULL) {
  if (identical(analysis_type, "plssem")) {
    output[[paste0(prefix, "_result_validity_note")]] <- renderUI({
      bundle <- fit_result()
      tagList(
        tags$p(class = "structural-result-note", "PLS-SEM validity output is based on seminr reliability summaries. Latent covariance, factor-score, HTMT, and lavaan delta-method diagnostics are not displayed for PLS models in this view."),
        if (as.integer(bundle$pls_bootstrap %||% 0L) > 0L) tags$p(class = "structural-result-note", "PLS bootstrap columns are added for direct paths, total and indirect effects, HTMT, outer loadings, and outer weights when seminr returns the corresponding bootstrap summaries.")
      )
    })
    return(invisible(TRUE))
  }
  structural_canvas_register_latent_correlation_outputs(output, prefix, fit_result)
  structural_canvas_register_validity_note_outputs(output, prefix, fit_result, result_table)
  structural_canvas_register_factor_score_outputs(output, prefix, fit_result)
  structural_canvas_register_reliability_bootstrap_outputs(output, prefix, fit_result, app_language_fn)
  structural_canvas_register_htmt_outputs(output, prefix, fit_result, app_language_fn)
  invisible(TRUE)
}
