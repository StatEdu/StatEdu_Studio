# Structural equation canvas latent correlation render outputs.

structural_canvas_register_latent_correlation_outputs <- function(output, prefix, fit_result, app_language_fn = NULL,
                                                                  display_name_for = function(bundle) identity) {
output[[paste0(prefix, "_result_latent_correlation_ci")]] <- renderUI({
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  values <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
  if (!nrow(values)) return(NULL)
  display_name <- display_name_for(bundle)
  values <- structural_canvas_display_identifier_table(values, display_name)
  values$r <- vapply(values$r, format_decimal3, character(1))
  values[["CI lower"]] <- vapply(values[["CI lower"]], format_decimal3, character(1))
  values[["CI upper"]] <- vapply(values[["CI upper"]], format_decimal3, character(1))
  values$p <- vapply(values$p, format_p, character(1))
  values$p[values$Type == "Fixed"] <- "—"
  tags$div(
    class = "structural-latent-correlation-ci",
    tags$h5(if (ko) "잠재상관 신뢰구간" else "Latent correlation confidence intervals"),
    tags$div(class = "table-responsive", tags$table(
      class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "구간은 명시적으로 추정되었거나 고정된 잠재 공분산 경로에 대한 95% delta-method 구간입니다. 'CI reaches |1|'은 허용 불가능한 상관 경계에 닿는 구간을 표시합니다. 명시적 공분산 모수가 없는 암묵적 상관에는 여기서 delta-method 구간을 부여하지 않습니다." else "Intervals are 95% delta-method intervals for explicitly estimated or fixed latent covariance paths. 'CI reaches |1|' flags an interval touching an inadmissible correlation boundary; implied correlations without an explicit covariance parameter are not assigned a delta-method interval here.")
  )
})
  invisible(TRUE)
}
