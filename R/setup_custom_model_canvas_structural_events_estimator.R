structural_canvas_method_recommendation <- function(snapshot, variable_table, objective = "confirmatory") {
  objective <- match.arg(as.character(objective %||% "confirmatory"), c("confirmatory", "explanatory", "predictive", "scores"))
  specification <- structural_canvas_construct_specification(snapshot)
  ordered <- structural_canvas_ordered_indicators(snapshot, variable_table)
  nominal <- structural_canvas_nominal_indicators(snapshot, variable_table)
  add_candidate <- function(method, role, reason, limitation = "") data.frame(
    Method = method, Role = role, Reason = reason, Limitation = limitation,
    stringsAsFactors = FALSE
  )
  if (length(nominal)) return(list(status = "Blocked", primary = NA_character_, objective = objective, candidates = add_candidate(
    "No standard candidate", "Blocked", "Nominal indicators require a different measurement model.", paste(nominal, collapse = ", ")
  )))
  if (!nrow(specification) || any(specification$construct_type == "unspecified")) return(list(status = "Review", primary = NA_character_, objective = objective, candidates = add_candidate(
    "Specification required", "Review", "Specify every construct as a common factor or composite before selecting an estimator.", "The method is not inferred from sample size, nonnormality, or empirical fit."
  )))
  has_factor <- any(specification$construct_type == "commonFactor")
  has_composite <- any(specification$construct_type == "composite")
  if (length(ordered) && has_composite) return(list(status = "Blocked", primary = NA_character_, objective = objective, candidates = add_candidate(
    "No single current engine", "Blocked", "The model combines ordered indicators with composite constructs.", "Current WLSMV handles common factors; the current PLS engine does not provide an ordinal composite estimator."
  )))
  if (length(ordered)) return(list(status = "Ready", primary = "CB-SEM (WLSMV)", objective = objective, candidates = add_candidate(
    "CB-SEM (WLSMV)", "Primary", "Common-factor constructs use ordered indicators.", "Use thresholds and an ordinal identification/invariance workflow."
  )))
  if (has_composite) {
    limitation <- if (has_factor) "Mixed factor-composite model: standard PLS treats the common-factor blocks as composites; do not claim construct-specific consistency correction unless the engine supports it." else "Evaluate weights, collinearity, and redundancy/convergent validity for formative composites."
    return(list(status = "Ready", primary = "PLS-SEM", objective = objective, candidates = add_candidate(
      "PLS-SEM", "Primary", if (has_factor) "The model contains both common-factor and composite specifications." else "The model contains composite constructs.", limitation
    )))
  }
  if (objective %in% c("predictive", "scores")) {
    candidates <- rbind(
      add_candidate("PLSc-SEM", "Primary", "All constructs are reflective common factors and a PLS-based prediction/score workflow was requested.", "Verify that the selected PLSc implementation supports every structural feature."),
      add_candidate("CB-SEM", "Alternative", "Retains covariance-based common-factor estimation and confirmatory global-fit assessment.", "Prediction and score use require separate validation.")
    )
    return(list(status = "Ready", primary = "PLSc-SEM", objective = objective, candidates = candidates))
  }
  candidates <- rbind(
    add_candidate("CB-SEM", "Primary", "All constructs are reflective common factors and the objective is confirmatory or explanatory.", "Choose ML/MLR/WLSMV from indicator scale and estimator assumptions."),
    add_candidate("PLSc-SEM", "Alternative", "A consistent PLS factor-estimation workflow may be used when substantively required.", "Do not choose it solely because empirical fit is better.")
  )
  list(status = "Ready", primary = "CB-SEM", objective = objective, candidates = candidates)
}

structural_canvas_selected_method_label <- function(analysis_type, estimator) {
  estimator <- toupper(as.character(estimator %||% ""))
  if (analysis_type %in% c("cfa", "cbsem", "sem")) return(if (identical(estimator, "WLSMV")) "CB-SEM (WLSMV)" else "CB-SEM")
  if (identical(estimator, "PLSC")) "PLSc-SEM" else "PLS-SEM"
}

structural_canvas_method_recommendation_ui <- function(recommendation, selected_method, language = statedu_initial_language()) {
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  candidates <- recommendation$candidates %||% data.frame()
  aligned <- isTRUE(nzchar(recommendation$primary %||% "")) && identical(selected_method, recommendation$primary)
  translate_candidate <- function(value) {
    translations <- c(
      "No standard candidate" = "표준 후보 없음",
      "Specification required" = "명세 필요",
      "No single current engine" = "현재 단일 엔진 없음",
      "Nominal indicators require a different measurement model." = "명목형 지표에는 다른 측정모형이 필요합니다.",
      "Specify every construct as a common factor or composite before selecting an estimator." = "추정법을 선택하기 전에 각 구성개념을 공통요인 또는 합성변수로 지정하십시오.",
      "The method is not inferred from sample size, nonnormality, or empirical fit." = "표본크기, 비정규성 또는 경험적 적합도만으로 추정법을 결정하지 않습니다.",
      "The model combines ordered indicators with composite constructs." = "모형에 순서형 지표와 합성변수 구성개념이 함께 있습니다.",
      "Current WLSMV handles common factors; the current PLS engine does not provide an ordinal composite estimator." = "현재 WLSMV는 공통요인을 처리하며, 현재 PLS 엔진은 순서형 합성변수 추정법을 제공하지 않습니다.",
      "Mixed factor-composite model: standard PLS treats the common-factor blocks as composites; do not claim construct-specific consistency correction unless the engine supports it." = "요인·합성변수 혼합 모형에서 표준 PLS는 공통요인 블록도 합성변수로 처리합니다. 엔진이 지원하지 않는 한 구성개념별 일관성 보정을 적용했다고 해석하지 마십시오.",
      "Evaluate weights, collinearity, and redundancy/convergent validity for formative composites." = "형성형 합성변수의 가중치, 공선성 및 중복성·수렴타당도를 평가하십시오.",
      Primary = "우선 권고", Alternative = "대안", Review = "검토 필요", Blocked = "실행 불가",
      "All constructs are reflective common factors and the objective is confirmatory or explanatory." = "모든 구성개념이 반영형 공통요인이며 분석 목적이 확인적 또는 설명적입니다.",
      "Choose ML/MLR/WLSMV from indicator scale and estimator assumptions." = "지표의 측정수준과 추정 가정에 따라 ML, MLR 또는 WLSMV를 선택합니다.",
      "A consistent PLS factor-estimation workflow may be used when substantively required." = "실질적 필요가 있을 때 일관성 보정 PLS 요인추정 절차를 대안으로 사용할 수 있습니다.",
      "Do not choose it solely because empirical fit is better." = "경험적 적합도가 더 좋다는 이유만으로 선택하지 마십시오.",
      "All constructs are reflective common factors and a PLS-based prediction/score workflow was requested." = "모든 구성개념이 반영형 공통요인이며 PLS 기반 예측 또는 점수 활용이 목적입니다.",
      "Verify that the selected PLSc implementation supports every structural feature." = "선택한 PLSc 구현이 모형의 모든 구조적 기능을 지원하는지 확인해야 합니다.",
      "Retains covariance-based common-factor estimation and confirmatory global-fit assessment." = "공분산 기반 공통요인 추정과 확인적 전역 적합도 평가를 유지합니다.",
      "Prediction and score use require separate validation." = "예측과 점수 활용에는 별도의 검증이 필요합니다.",
      "Common-factor constructs use ordered indicators." = "공통요인 구성개념에 순서형 지표가 사용되었습니다.",
      "Use thresholds and an ordinal identification/invariance workflow." = "임계값과 순서형 식별·불변성 절차를 사용합니다.",
      "The model contains both common-factor and composite specifications." = "모형에 공통요인과 합성변수 명세가 함께 포함되어 있습니다.",
      "The model contains composite constructs." = "모형에 합성변수 구성개념이 포함되어 있습니다."
    )
    key <- as.character(value %||% "")
    if (key %in% names(translations)) tr(key, unname(translations[[key]])) else key
  }
  tags$div(
    class = paste("structural-method-recommendation", if (aligned) "is-aligned" else "needs-review"),
    tags$strong(tr("Method guidance: ", "추정법 안내: ")),
    if (is.na(recommendation$primary %||% NA_character_)) {
      tr("Review construct specification or engine compatibility first.", "구성개념 명세 또는 엔진 호환성을 먼저 검토하세요.")
    } else if (aligned) {
      sprintf(tr("The current selection matches the primary candidate: %s", "현재 선택이 1순위 후보와 일치합니다: %s"), recommendation$primary)
    } else {
      sprintf(tr("Primary candidate: %s. Current selection: %s.", "1순위 후보는 %s입니다. 현재 선택: %s."), recommendation$primary, selected_method)
    },
    if (nrow(candidates)) tags$details(
      class = "structural-method-guidance-details",
      tags$summary(tr("Show rationale and alternatives", "선정 근거와 대안 보기")),
      tags$ul(lapply(seq_len(nrow(candidates)), function(index) tags$li(
        tags$b(paste0(translate_candidate(candidates$Method[[index]]), " — ", translate_candidate(candidates$Role[[index]]), ": ")),
        translate_candidate(candidates$Reason[[index]]),
        if (nzchar(candidates$Limitation[[index]])) paste0(" ", if (identical(candidates$Reason[[index]], "Nominal indicators require a different measurement model.")) candidates$Limitation[[index]] else translate_candidate(candidates$Limitation[[index]]))
      ))),
      tags$p(class = "structural-option-note", tr("Small samples, nonnormality, or better empirical fit alone do not select PLS/PLSc. The user retains the final choice, which is stored in the analysis record.", "작은 표본, 비정규성 또는 더 좋은 적합도만으로 PLS/PLSc를 선택하지 않습니다. 최종 선택은 사용자가 확정하며 분석 기록에 저장됩니다."))
    )
  )
}

structural_canvas_estimator_recommendation_modal <- function(recommendation, analysis_type, prefix, bollen_requested, language) {
  diagnosis <- recommendation$diagnosis
  tr <- function(en, ko) statedu_localized_text(language, en, ko)

  modalDialog(
    title = tr("Estimator recommendation", "추정량 권고"),
    tags$p(tr("The sample-size-sensitive Mardia screen flagged departure from multivariate normality in the continuous indicators. Consider MLR for robust standard errors and a scaled test. This screen alone does not establish the substantive magnitude of departure or its impact on estimates.", "표본크기에 민감한 Mardia 선별검사에서 연속형 지표의 다변량 정규성 이탈이 표시되었습니다. 강건 표준오차와 보정 검정을 제공하는 MLR을 우선 검토하십시오. 이 검정만으로 분포 이탈의 실질적 크기나 추정치 영향을 확정할 수는 없습니다.")),
    tags$p(sprintf(tr("Mardia skewness p = %s; kurtosis p = %s; complete cases = %s of %s.",
      "Mardia 왜도 p = %s; 첨도 p = %s; 완전 사례 = %s / %s."),
      format_p(diagnosis$skew_p), format_p(diagnosis$kurtosis_p), diagnosis$n, diagnosis$original_n)),
    if (bollen_requested) result_note_paragraph(class = "structural-result-note", tr("Bollen-Stine bootstrap is available only for ML; choosing MLR will run the model without Bollen-Stine bootstrap.", "Bollen-Stine 부트스트랩은 ML에서만 사용할 수 있습니다. MLR을 선택하면 Bollen-Stine 부트스트랩 없이 모형을 실행합니다.")),
    footer = tagList(
      modalButton(tr("Cancel", "취소")),
      actionButton(paste0(prefix, "_run_with_ml"), tr("Run with ML", "ML로 실행"), class = "btn-default"),
      actionButton(paste0(prefix, "_run_with_mlr"), tr("Run with MLR", "MLR로 실행"), class = "btn-primary")
    ),
    easyClose = TRUE
  )
}
