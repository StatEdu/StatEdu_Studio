structural_canvas_factor_score_display <- function(values, language) {
  labels <- c("Factor" = "요인", "Determinacy" = "결정성", "Score reliability" = "점수 신뢰도", "Guidance" = "해석 안내",
    "Not assessed" = "평가하지 않음", "At/above descriptive .90 reference" = "기술적 참고값 .90 이상",
    "Between descriptive .80 and .90 references" = "기술적 참고값 .80 이상 .90 미만",
    "Below descriptive .80; review score use" = "기술적 참고값 .80 미만; 점수 활용 검토")
  label <- function(value) if (value %in% names(labels)) statedu_localized_text(language, value, unname(labels[[value]])) else value
  values$Guidance <- vapply(values$Guidance, label, character(1), USE.NAMES = FALSE)
  names(values) <- vapply(names(values), label, character(1), USE.NAMES = FALSE)
  attr(values, "result_user_columns") <- names(values)
  values
}

# Structural equation canvas factor-score render outputs.

structural_canvas_register_factor_score_outputs <- function(output, prefix, fit_result, app_language_fn = NULL,
                                                             display_name_for = function(bundle) identity) {
output[[paste0(prefix, "_result_factor_scores")]] <- renderUI({
  bundle <- fit_result()
  language <- statedu_current_language(app_language_fn)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  values <- structural_canvas_factor_score_quality(bundle$fit)
  if (!nrow(values)) return(NULL)
  display_name <- display_name_for(bundle)
  values <- structural_canvas_display_identifier_table(values, display_name)
  values$Determinacy <- vapply(values$Determinacy, format_decimal3, character(1))
  values[["Score reliability"]] <- vapply(values[["Score reliability"]], format_decimal3, character(1))
  values <- structural_canvas_factor_score_display(values, language)
  tagList(
    tags$h5(tr("Factor-score quality", "요인점수 품질")),
    structural_canvas_basic_html_table(values, role = "appendix", orientation = "auto", language = statedu_current_language(app_language_fn)),
    result_note_paragraph(class = "structural-result-note", tr("Determinacy is the estimated correlation between regression factor scores and the latent factor under the fitted model; score reliability is its square. The .80 and .90 values are descriptive references, not pass thresholds for individual use. These indices do not replace CR, omega, content/criterion validity, model admissibility, or independent replication.", "Determinacy는 적합된 모형 아래에서 회귀 요인점수와 잠재요인의 추정 상관이고, score reliability는 그 제곱입니다. .80과 .90은 기술적 참고값일 뿐 개인 수준 사용의 합격선이 아닙니다. 이 지수는 CR, omega, 내용·준거타당도, 모형 허용성과 독립표본 재현성을 대체하지 않습니다.")),
    result_note_paragraph(class = "structural-result-note", tr("Factor scores are indeterminate: values and rankings can vary by scoring method, and regression scores are shrunken toward the mean. Treating them as error-free observed variables in ordinary downstream regression can understate uncertainty; prefer analysis within the latent-variable model or methods that propagate factor-score error.", "요인점수는 불확정적이며 산출법에 따라 값과 순위가 달라질 수 있고 회귀점수는 평균 쪽으로 수축됩니다. 점수를 일반 회귀의 오류 없는 관측변수처럼 사용하면 불확실성을 과소평가할 수 있으므로, 가능하면 잠재변수 모형 안에서 분석하거나 요인점수 오차를 전파하는 방법을 사용하십시오.")),
    result_note_paragraph(class = "structural-result-note", tr("Determinacy alone is insufficient for individual decisions such as diagnosis, selection, or feedback. Such use requires separate evidence on classification accuracy, fairness, temporal stability, external criteria, and independent validation.", "진단·선발·개인 피드백 같은 개인 수준 의사결정에는 결정성만으로 충분하지 않습니다. 별도의 분류정확도, 공정성, 검사-재검사 안정성, 외부 준거 및 독립 검증이 필요합니다.")),
    if (length(bundle$ordered %||% character(0))) result_note_paragraph(class = "structural-result-note", tr("For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.", "순서형 지표의 요인점수 품질은 적합된 잠재반응 WLSMV 모형과 범주 thresholds에 조건부입니다."))
  )
})
  invisible(TRUE)
}
