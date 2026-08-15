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
  ko <- identical(normalize_app_language(language), "ko")
  candidates <- recommendation$candidates %||% data.frame()
  aligned <- isTRUE(nzchar(recommendation$primary %||% "")) && identical(selected_method, recommendation$primary)
  tags$div(
    class = paste("structural-method-recommendation", if (aligned) "is-aligned" else "needs-review"),
    tags$strong(if (ko) "추정법 안내: " else "Method guidance: "),
    if (is.na(recommendation$primary %||% NA_character_)) {
      if (ko) "구성개념 명세 또는 엔진 호환성을 먼저 검토하세요." else "Review construct specification or engine compatibility first."
    } else if (aligned) {
      paste0(if (ko) "현재 선택이 1순위 후보와 일치합니다: " else "The current selection matches the primary candidate: ", recommendation$primary)
    } else {
      paste0(if (ko) "1순위 후보는 " else "Primary candidate: ", recommendation$primary, if (ko) "입니다. 현재 선택: " else ". Current selection: ", selected_method, ".")
    },
    if (nrow(candidates)) tags$ul(lapply(seq_len(nrow(candidates)), function(index) tags$li(
      tags$b(paste0(candidates$Method[[index]], " — ", candidates$Role[[index]], ": ")),
      candidates$Reason[[index]], if (nzchar(candidates$Limitation[[index]])) paste0(" ", candidates$Limitation[[index]])
    ))),
    tags$p(class = "structural-option-note", if (ko) "작은 표본, 비정규성 또는 더 좋은 적합도만으로 PLS/PLSc를 선택하지 않습니다. 최종 선택은 사용자가 확정하며 분석 기록에 저장됩니다." else "Small samples, nonnormality, or better empirical fit alone do not select PLS/PLSc. The user retains the final choice, which is stored in the analysis record.")
  )
}

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
