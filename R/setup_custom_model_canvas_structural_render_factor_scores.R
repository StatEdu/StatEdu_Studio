# Structural equation canvas factor-score render outputs.

structural_canvas_register_factor_score_outputs <- function(output, prefix, fit_result, app_language_fn = NULL) {
output[[paste0(prefix, "_result_factor_scores")]] <- renderUI({
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  values <- structural_canvas_factor_score_quality(bundle$fit)
  if (!nrow(values)) return(NULL)
  values$Determinacy <- vapply(values$Determinacy, format_decimal3, character(1))
  values[["Score reliability"]] <- vapply(values[["Score reliability"]], format_decimal3, character(1))
  tagList(
    tags$h5(if (ko) "요인점수 품질" else "Factor-score quality"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "Determinacy는 적합된 모형 아래에서 회귀 요인점수와 잠재요인의 추정 상관이고, score reliability는 그 제곱입니다. .80과 .90은 기술적 참고값일 뿐 개인 수준 사용의 합격선이 아닙니다. 이 지수는 CR, omega, 내용·준거타당도, 모형 허용성과 독립표본 재현성을 대체하지 않습니다." else "Determinacy is the estimated correlation between regression factor scores and the latent factor under the fitted model; score reliability is its square. The .80 and .90 values are descriptive references, not pass thresholds for individual use. These indices do not replace CR, omega, content/criterion validity, model admissibility, or independent replication."),
    tags$p(class = "structural-result-note", if (ko) "요인점수는 불확정적이며 산출법에 따라 값과 순위가 달라질 수 있고 회귀점수는 평균 쪽으로 수축됩니다. 점수를 일반 회귀의 오류 없는 관측변수처럼 사용하면 불확실성을 과소평가할 수 있으므로, 가능하면 잠재변수 모형 안에서 분석하거나 요인점수 오차를 전파하는 방법을 사용하십시오." else "Factor scores are indeterminate: values and rankings can vary by scoring method, and regression scores are shrunken toward the mean. Treating them as error-free observed variables in ordinary downstream regression can understate uncertainty; prefer analysis within the latent-variable model or methods that propagate factor-score error."),
    tags$p(class = "structural-result-note", if (ko) "진단·선발·개인 피드백 같은 개인 수준 의사결정에는 결정성만으로 충분하지 않습니다. 별도의 분류정확도, 공정성, 검사-재검사 안정성, 외부 준거 및 독립 검증이 필요합니다." else "Determinacy alone is insufficient for individual decisions such as diagnosis, selection, or feedback. Such use requires separate evidence on classification accuracy, fairness, temporal stability, external criteria, and independent validation."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", if (ko) "순서형 지표의 요인점수 품질은 적합된 잠재반응 WLSMV 모형과 범주 thresholds에 조건부입니다." else "For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.")
  )
})
  invisible(TRUE)
}
