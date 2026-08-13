# Structural equation canvas factor-score render outputs.

structural_canvas_register_factor_score_outputs <- function(output, prefix, fit_result) {
output[[paste0(prefix, "_result_factor_scores")]] <- renderUI({
  bundle <- fit_result()
  values <- structural_canvas_factor_score_quality(bundle$fit)
  if (!nrow(values)) return(NULL)
  values$Determinacy <- vapply(values$Determinacy, format_decimal3, character(1))
  values[["Score reliability"]] <- vapply(values[["Score reliability"]], format_decimal3, character(1))
  tagList(
    tags$h5("Factor-score quality"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Determinacy is the correlation between regression factor scores and the latent factor; score reliability is its square. Descriptive guidance uses .90 as strong and .80 as a cautious-use threshold. These indices concern estimated factor scores for downstream or individual-level use and are not substitutes for CR, omega, validity evidence, or model admissibility."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", "For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.")
  )
})
  invisible(TRUE)
}
