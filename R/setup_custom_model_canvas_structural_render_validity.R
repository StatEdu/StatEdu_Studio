structural_canvas_register_validity_outputs <- function(output, prefix, analysis_type, fit_result, result_table, app_language_fn = NULL) {
  structural_canvas_register_latent_correlation_outputs(output, prefix, fit_result)
  structural_canvas_register_validity_note_outputs(output, prefix, fit_result, result_table)
  structural_canvas_register_factor_score_outputs(output, prefix, fit_result)
  structural_canvas_register_reliability_bootstrap_outputs(output, prefix, fit_result, app_language_fn)
  structural_canvas_register_htmt_outputs(output, prefix, fit_result, app_language_fn)
  invisible(TRUE)
}
