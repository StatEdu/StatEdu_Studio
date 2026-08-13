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
    tags$p(class = "structural-result-note", if (ko) "Determinacy는 회귀 요인점수와 잠재요인의 상관이고, score reliability는 그 제곱입니다. 기술적 안내는 .90을 강한 수준, .80을 신중 사용 기준으로 봅니다. 이 지수는 후속 분석 또는 개인 수준 사용을 위한 추정 요인점수의 품질을 다루며 CR, omega, 타당도 근거, 모형 허용성을 대체하지 않습니다." else "Determinacy is the correlation between regression factor scores and the latent factor; score reliability is its square. Descriptive guidance uses .90 as strong and .80 as a cautious-use threshold. These indices concern estimated factor scores for downstream or individual-level use and are not substitutes for CR, omega, validity evidence, or model admissibility."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", if (ko) "순서형 지표의 요인점수 품질은 적합된 잠재반응 WLSMV 모형과 범주 thresholds에 조건부입니다." else "For ordered indicators, factor-score quality is conditional on the fitted latent-response WLSMV model and category thresholds.")
  )
})
  invisible(TRUE)
}
