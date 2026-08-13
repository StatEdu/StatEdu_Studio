structural_canvas_estimator_recommendation_modal <- function(recommendation, analysis_type, prefix, bollen_requested, language) {
  diagnosis <- recommendation$diagnosis
  ko <- identical(normalize_app_language(language), "ko")

  modalDialog(
    title = if (ko) "추정량 권고" else "Estimator recommendation",
    tags$p(if (ko) "Mardia 진단에서 연속형 지표의 비정규성이 확인되었습니다. 이 모형을 적합하기 전에 강건 MLR 사용을 권장합니다." else "Mardia diagnostics flagged nonnormal continuous indicators. Robust MLR is recommended before fitting this model."),
    tags$p(paste0(
      if (ko) "Mardia 왜도 p = " else "Mardia skewness p = ", format_p(diagnosis$skew_p),
      if (ko) "; 첨도 p = " else "; kurtosis p = ", format_p(diagnosis$kurtosis_p),
      if (ko) "; 완전 사례 = " else "; complete cases = ", diagnosis$n, if (ko) " / " else " of ", diagnosis$original_n, "."
    )),
    if (bollen_requested) tags$p(class = "structural-result-note", if (ko) "Bollen-Stine 부트스트랩은 ML에서만 사용할 수 있습니다. MLR을 선택하면 Bollen-Stine 부트스트랩 없이 모형을 실행합니다." else "Bollen-Stine bootstrap is available only for ML; choosing MLR will run the model without Bollen-Stine bootstrap."),
    footer = tagList(
      modalButton(if (ko) "취소" else "Cancel"),
      actionButton(paste0(prefix, "_run_with_ml"), if (ko) "ML로 실행" else "Run with ML", class = "btn-default"),
      actionButton(paste0(prefix, "_run_with_mlr"), if (ko) "MLR로 실행" else "Run with MLR", class = "btn-primary")
    ),
    easyClose = TRUE
  )
}
