# Structural equation canvas latent correlation render outputs.

structural_canvas_register_latent_correlation_outputs <- function(output, prefix, fit_result) {
output[[paste0(prefix, "_result_latent_correlation_ci")]] <- renderUI({
  bundle <- fit_result()
  values <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
  if (!nrow(values)) return(NULL)
  values$r <- vapply(values$r, format_decimal3, character(1))
  values[["CI lower"]] <- vapply(values[["CI lower"]], format_decimal3, character(1))
  values[["CI upper"]] <- vapply(values[["CI upper"]], format_decimal3, character(1))
  values$p <- vapply(values$p, format_p, character(1))
  values$p[values$Type == "Fixed"] <- "—"
  tags$div(
    class = "structural-latent-correlation-ci",
    tags$h5("Latent correlation confidence intervals"),
    tags$div(class = "table-responsive", tags$table(
      class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Intervals are 95% delta-method intervals for explicitly estimated or fixed latent covariance paths. 'CI reaches |1|' flags an interval touching an inadmissible correlation boundary; implied correlations without an explicit covariance parameter are not assigned a delta-method interval here.")
  )
})
  invisible(TRUE)
}
