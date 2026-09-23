# Structural equation canvas latent correlation render outputs.

structural_canvas_latent_correlation_display <- function(values, language) {
  labels <- c("Factor 1" = "요인 1", "Factor 2" = "요인 2", "CI lower" = "CI 하한",
    "CI upper" = "CI 상한", "Type" = "유형", "CI reaches |1|" = "CI가 |1|에 도달",
    "Estimated" = "추정", "Fixed" = "고정", "Yes" = "예", "No" = "아니요", "Not assessed" = "평가하지 않음")
  label <- function(value) if (value %in% names(labels)) statedu_localized_text(language, value, unname(labels[[value]])) else value
  for (column in c("Type", "CI reaches |1|")) values[[column]] <- vapply(values[[column]], label, character(1), USE.NAMES = FALSE)
  names(values) <- vapply(names(values), label, character(1), USE.NAMES = FALSE)
  attr(values, "result_user_columns") <- names(values)
  values
}

structural_canvas_register_latent_correlation_outputs <- function(output, prefix, fit_result, app_language_fn = NULL,
                                                                  display_name_for = function(bundle) identity) {
output[[paste0(prefix, "_result_latent_correlation_ci")]] <- renderUI({
  bundle <- fit_result()
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  values <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
  if (!nrow(values)) return(NULL)
  display_name <- display_name_for(bundle)
  values <- structural_canvas_display_identifier_table(values, display_name)
  values$r <- vapply(values$r, format_decimal3, character(1))
  values[["CI lower"]] <- vapply(values[["CI lower"]], format_decimal3, character(1))
  values[["CI upper"]] <- vapply(values[["CI upper"]], format_decimal3, character(1))
  values$p <- vapply(values$p, format_p, character(1))
  values$p[values$Type == "Fixed"] <- "—"
  values <- structural_canvas_latent_correlation_display(values, language)
  tags$div(
    class = "structural-latent-correlation-ci",
    tags$h5(tr("Latent correlation confidence intervals", "잠재상관 신뢰구간")),
    structural_canvas_basic_html_table(values, role = "appendix", orientation = "auto", language = statedu_current_language(app_language_fn)),
    result_note_paragraph(class = "structural-result-note", tr("Intervals are 95% delta-method intervals for explicitly estimated or fixed latent covariance paths. 'CI reaches |1|' flags an interval touching an inadmissible correlation boundary; implied correlations without an explicit covariance parameter are not assigned a delta-method interval here.", "구간은 명시적으로 추정되었거나 고정된 잠재 공분산 경로에 대한 95% delta-method 구간입니다. 'CI가 |1|에 도달'은 허용 불가능한 상관 경계에 닿는 구간을 표시합니다. 명시적 공분산 모수가 없는 암묵적 상관에는 여기서 delta-method 구간을 부여하지 않습니다."))
  )
})
  invisible(TRUE)
}
