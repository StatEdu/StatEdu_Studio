# Structural equation canvas analysis option controls.

structural_canvas_result_coefficient_choices <- function(language = statedu_initial_language(), analysis_type = "cbsem") {
  if (identical(analysis_type, "plssem")) {
    return(stats::setNames(c("pls_value", "pls_p"), c("β", "β(p)")))
  }
  stats::setNames(c("b_p", "b_t", "beta_t", "beta_p", "b_beta"), c("B(p)", "B(t)", "beta(t)", "beta(p)", "B(Beta)"))
}

structural_canvas_measurement_coefficient_choices <- function(language = statedu_initial_language()) {
  stats::setNames(c("measurement_value", "measurement_p"), c("loading / weight", "loading(p) / weight(p)"))
}

structural_canvas_bootstrap_replicate_values <- function(include_disabled = TRUE) {
  values <- c(1000L, 5000L, 10000L, 20000L, 50000L)
  if (isTRUE(include_disabled)) c(0L, values) else values
}

structural_analysis_options_panel <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  prefix <- structural_analysis_prefix(analysis_type)
  input_id <- function(suffix) paste0(prefix, suffix)
  bootstrap_choices <- function(values) {
    values <- as.integer(values)
    labels <- if (ko) {
      c("계산하지 않음", paste0(format(values[-1L], big.mark = ",", scientific = FALSE, trim = TRUE), "회"))
    } else {
      c("Do not compute", paste0(format(values[-1L], big.mark = ",", scientific = FALSE, trim = TRUE), " resamples"))
    }
    stats::setNames(as.character(values), labels)
  }
  bootstrap_select <- function(suffix, label_ko, label_en, values, selected = "0") {
    values <- as.integer(values)
    selected <- as.character(selected)
    if (!selected %in% as.character(values)) selected <- as.character(values[[1L]])
    selectInput(input_id(suffix), if (ko) label_ko else label_en, choices = bootstrap_choices(values), selected = selected)
  }
  bootstrap_details <- function(suffix, ...) {
    conditionalPanel(sprintf("input['%s'] != '0'", input_id(suffix)), ...)
  }

  estimation_tab <- tabPanel(
    if (ko) "추정" else "Estimation",
    selectInput(
      input_id("_estimator"), if (ko) "추정 방법" else "Estimator",
      choices = if (identical(analysis_type, "plssem")) {
        stats::setNames(
          c("AUTO", "PLS", "PLSC"),
          c(if (ko) "규칙 기반 추천(확인 필요)" else "Rule-based recommendation (confirmation required)", "PLS", "PLSc")
        )
      } else {
        c("ML" = "ML", "MLR" = "MLR", "WLSMV" = "WLSMV")
      },
      selected = if (identical(analysis_type, "plssem")) "AUTO" else "ML"
    ),
    if (identical(analysis_type, "plssem")) checkboxInput(
      input_id("_estimator_recommendation_confirmed"),
      if (ko) "구성개념 명세에 따른 PLS/PLSc 추천을 검토하고 적용합니다." else "I reviewed and accept the PLS/PLSc recommendation based on the construct specification.",
      value = FALSE
    ),
    selectInput(
      input_id("_objective"), if (ko) "주요 분석 목적" else "Primary analysis objective",
      choices = stats::setNames(
        c("confirmatory", "explanatory", "predictive", "scores"),
        c(if (ko) "이론 검증" else "Confirmatory theory testing", if (ko) "구조 설명" else "Structural explanation", if (ko) "표본외 예측" else "Out-of-sample prediction", if (ko) "구성개념 점수 활용" else "Construct-score use")
      ), selected = "confirmatory"
    ),
    selectInput(
      input_id("_analysis_plan_status"), if (ko) "분석계획 상태" else "Analysis-plan status",
      choices = stats::setNames(
        c("not_recorded", "preregistered", "protocol_defined", "exploratory"),
        c(if (ko) "기록되지 않음" else "Not recorded", if (ko) "사전등록됨" else "Preregistered", if (ko) "등록 전 프로토콜에 정의됨" else "Defined in an a-priori protocol", if (ko) "탐색적 분석" else "Exploratory analysis")
      ), selected = "not_recorded"
    ),
    textInput(input_id("_analysis_plan_reference"), if (ko) "사전등록·프로토콜 참조" else "Preregistration or protocol reference", value = "", placeholder = if (ko) "등록 URL/DOI, 날짜, 버전 또는 프로토콜 식별자" else "Registration URL/DOI, date, version, or protocol identifier"),
    selectInput(
      input_id("_sampling_design"), if (ko) "관측치·표본설계 구조" else "Observation and sampling structure",
      choices = stats::setNames(
        c("not_declared", "independent_cross_sectional", "clustered", "complex_survey", "longitudinal_repeated"),
        c(
          if (ko) "선택 필요" else "Select before analysis",
          if (ko) "독립 관측 횡단자료" else "Independent cross-sectional observations",
          if (ko) "군집·다층자료" else "Clustered or multilevel data",
          if (ko) "가중치·층화·PSU가 있는 복합표본" else "Complex survey (weights/strata/PSU)",
          if (ko) "종단·반복측정자료" else "Longitudinal or repeated measures"
        )
      ), selected = "independent_cross_sectional"
    ),
    tags$p(class = "structural-option-note", if (ko) "분석 전에 표집구조를 명시적으로 확인하십시오. 현재 캔버스 엔진은 군집, 복합표본 또는 종단·반복측정의 의존구조를 무시한 분석을 실행하지 않습니다." else "Confirm the sampling structure explicitly before analysis. The current canvas engine will not fit a model that ignores clustered, complex-survey, longitudinal, or repeated-measures dependence."),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_missing"), if (ko) "결측치 처리" else "Missing data", choices = stats::setNames(c("fiml", "listwise"), c("FIML", if (ko) "목록 삭제" else "Listwise deletion"))),
    if (!identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", if (ko) "순서형 지표 또는 WLSMV 추정량을 사용하면 lavaan 제약에 따라 FIML 대신 pairwise 결측 처리가 적용됩니다." else "When ordered indicators or the WLSMV estimator are used, lavaan uses pairwise missing-data handling instead of FIML."),
    uiOutput(input_id("_method_recommendation"))
  )

  bootstrap_tab <- tabPanel(
    if (ko) "부트스트랩" else "Bootstrap",
    tags$p(
      class = "structural-option-note",
      if (analysis_type %in% c("cbsem", "sem")) {
        if (ko) "기본 모형을 먼저 적합하며, 경로·간접·총효과 bootstrap은 기본 5,000회입니다. 그 밖의 재표집 분석은 선택한 경우에만 추가로 실행합니다." else "The base model is fitted first. Path, indirect, and total-effect bootstrap defaults to 5,000 resamples; other resampling analyses run only when selected."
      } else {
        if (ko) "기본 모형을 먼저 적합하고, 선택한 재표집 분석만 추가로 실행합니다. 모든 부트스트랩의 기본값은 '계산하지 않음'입니다." else "The base model is fitted first, and only selected resampling analyses are added. All bootstrap procedures default to 'Do not compute'."
      }
    ),
    if (analysis_type %in% c("cbsem", "sem")) tagList(
      tags$h5(if (ko) "경로·간접·총효과" else "Path, indirect, and total effects"),
      bootstrap_select("_effect_bootstrap", "경로·간접·총효과 bootstrap CI/p", "Path, indirect, and total-effect bootstrap CI/p", structural_canvas_bootstrap_replicate_values(), selected = "5000"),
      bootstrap_details(
        "_effect_bootstrap",
        numericInput(input_id("_effect_bootstrap_seed"), if (ko) "경로·간접·총효과 seed" else "Path/indirect/total-effect seed", value = default_seed(), min = 1L, step = 1L),
        selectInput(input_id("_effect_bootstrap_ci_method"), if (ko) "경로·간접·총효과 CI 방법" else "Path/indirect/total-effect CI method", choices = c("Bias-corrected (BC)" = "bias_corrected", "Percentile" = "percentile"), selected = "bias_corrected"),
        tags$p(class = "structural-option-note", if (ko) "직접경로, 특정·총 간접효과, 총효과와 조절된 매개효과 index를 매 반복에서 다시 계산합니다." else "Direct paths, specific and total indirect effects, total effects, and moderated-mediation indices are recomputed in every replicate.")
      )
    ),
    if (identical(analysis_type, "cfa")) tagList(
      tags$h5(if (ko) "측정모형 신뢰도" else "Measurement-model reliability"),
      bootstrap_select("_reliability_bootstrap", "AVE·신뢰도 bootstrap CI", "AVE/reliability bootstrap CI", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_reliability_bootstrap",
        numericInput(input_id("_reliability_seed"), if (ko) "AVE·신뢰도 seed" else "AVE/reliability seed", value = default_seed(), min = 1L, step = 1L),
        selectInput(input_id("_reliability_ci_method"), if (ko) "AVE·신뢰도 CI 방법" else "AVE/reliability CI method", choices = c("Bias-corrected (BC)" = "bias_corrected", "Percentile" = "percentile", "BCa (slower)" = "bca"), selected = "bias_corrected")
      ),
      tags$h5(if (ko) "전체 모형 적합도" else "Global model fit"),
      bootstrap_select("_bollen_stine_bootstrap", "Bollen-Stine 전체 적합도 bootstrap", "Bollen-Stine global-fit bootstrap", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_bollen_stine_bootstrap",
        numericInput(input_id("_bollen_stine_seed"), "Bollen-Stine seed", value = default_seed(), min = 1L, step = 1L),
        tags$p(class = "structural-option-note", if (ko) "결측이 없는 연속형 단일집단 ML CFA에서만 실행됩니다." else "Available only for complete continuous single-group CFA estimated with ML.")
      )
    ),
    if (!identical(analysis_type, "plssem")) tagList(
      tags$h5(if (ko) "판별타당도" else "Discriminant validity"),
      bootstrap_select("_htmt_bootstrap", "HTMT bootstrap CI", "HTMT bootstrap CI", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_htmt_bootstrap",
        numericInput(input_id("_htmt_seed"), "HTMT seed", value = default_seed(), min = 1L, step = 1L),
        selectInput(input_id("_htmt_ci_method"), if (ko) "HTMT CI 방법" else "HTMT CI method", choices = c("Bias-corrected (BC)" = "bias_corrected", "Percentile" = "percentile"), selected = "bias_corrected")
      )
    ),
    if (identical(analysis_type, "plssem")) tagList(
      tags$h5(if (ko) "PLS 모수 및 구조효과" else "PLS parameters and structural effects"),
      bootstrap_select("_pls_bootstrap", "PLS 경로·loading·weight·간접·총효과 bootstrap CI/p", "PLS path/loading/weight/indirect/total-effect bootstrap CI/p", structural_canvas_bootstrap_replicate_values()),
      bootstrap_details(
        "_pls_bootstrap",
        numericInput(input_id("_pls_seed"), "PLS bootstrap seed", value = default_seed(), min = 1L, step = 1L),
        tags$p(class = "structural-option-note", if (ko) "실행하지 않으면 점추정값은 표시하지만 bootstrap CI와 p 값은 표시하지 않습니다." else "When disabled, point estimates remain available but bootstrap CIs and p values are not reported.")
      )
    )
  )

  advanced_tab <- tabPanel(
    if (ko) "고급 옵션" else "Advanced Options",
    tags$p(class = "structural-option-note", if (ko) "보고·민감도·추가 모형 설정입니다. 기본 모형 적합의 필수 항목은 아닙니다." else "Reporting, sensitivity, and supplementary-model settings. These are not required to fit the base model."),
    selectInput(
      input_id("_power_basis"), if (ko) "사전 표본크기·검정력 근거" else "A-priori sample-size/power basis",
      choices = stats::setNames(
        c("not_recorded", "rmsea_power", "model_monte_carlo", "target_effect_precision", "prior_evidence", "other_documented"),
        c(if (ko) "기록되지 않음" else "Not recorded", if (ko) "RMSEA 적합도 검정력" else "RMSEA fit-test power", if (ko) "모형별 Monte Carlo" else "Model-specific Monte Carlo", if (ko) "목표 효과·정밀도" else "Target effect/precision", if (ko) "선행연구 근거" else "Prior evidence", if (ko) "기타 문서화된 근거" else "Other documented basis")
      ), selected = "not_recorded"
    ),
    textAreaInput(input_id("_power_details"), if (ko) "검정력 근거 상세" else "Power-basis details", rows = 2, placeholder = if (ko) "가정한 효과크기·모수, 목표 검정력, alpha, 결측/탈락, 사용 도구 또는 문헌을 기록하십시오." else "Record assumed effects/parameters, target power, alpha, attrition/missingness allowance, and software or source."),
    tags$p(class = "structural-option-note", if (ko) "N/모수 비율이나 PLS 10배 규칙은 사전 검정력 근거가 아닙니다." else "N-to-parameter ratios and the PLS 10-times rule are not a-priori power evidence."),
    if (!identical(analysis_type, "plssem")) conditionalPanel(
      sprintf("input['%s'] == 'ML'", input_id("_estimator")),
      selectInput(
        input_id("_ml_likelihood"), if (ko) "ML likelihood 관례" else "ML likelihood convention",
        choices = stats::setNames(
          c("normal", "wishart"),
          c(
            if (ko) "Normal ML (lavaan 기본; N 배율)" else "Normal ML (lavaan default; N multiplier)",
            if (ko) "Wishart ML (AMOS/LISREL/EQS 호환; N-1 배율)" else "Wishart ML (AMOS/LISREL/EQS compatible; N-1 multiplier)"
          )
        ),
        selected = "normal"
      ),
      tags$p(
        class = "structural-option-note",
        if (ko) "기본값은 lavaan의 Normal ML입니다. AMOS 등과 수치 교차검증할 때만 동일 모형·자료·결측 처리와 함께 Wishart ML을 선택하십시오. 카이제곱만 사후 보정하지 않고 전체 모형을 해당 관례로 다시 적합합니다."
        else "Normal ML is the lavaan default. Select Wishart ML only for a matched numerical comparison with AMOS or similar software, while also matching the model, data, and missing-data handling. The full model is refitted under that convention; chi-square is not adjusted post hoc."
      )
    ),
    if (!identical(analysis_type, "plssem")) selectInput(
      input_id("_missing_sensitivity_method"), if (ko) "결측 민감도 검토" else "Missing-data sensitivity assessment",
      choices = stats::setNames(
        c("not_assessed", "complete_case_comparison", "multiple_imputation", "delta_pattern_mixture", "external_analysis", "other_documented"),
        c(if (ko) "평가하지 않음" else "Not assessed", if (ko) "완전사례 비교" else "Complete-case comparison", if (ko) "다중대치 비교" else "Multiple-imputation comparison", "Delta/pattern-mixture", if (ko) "외부 민감도 분석" else "External sensitivity analysis", if (ko) "기타 문서화된 검토" else "Other documented assessment")
      ), selected = "not_assessed"
    ),
    if (!identical(analysis_type, "plssem")) textAreaInput(input_id("_missing_sensitivity_details"), if (ko) "민감도 가정·결과·결론" else "Sensitivity assumptions, results, and conclusion", rows = 3, placeholder = if (ko) "비교 방법, MNAR 이탈 가정, 주요 모수 변화와 결론의 강건성을 기록하십시오." else "Record the comparison method, MNAR-departure assumptions, changes in key parameters, and robustness of conclusions."),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_scale"), if (ko) "잠재변수 스케일" else "Latent scale", choices = stats::setNames(c("marker", "variance"), c(if (ko) "첫 지표 부하량 = 1" else "Marker loading = 1", if (ko) "잠재변수 분산 = 1" else "Latent variance = 1"))),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_rmsea_ci"), if (ko) "RMSEA 신뢰수준" else "RMSEA confidence level", choices = c("90% CI" = "0.90", "95% CI" = "0.95", "99% CI" = "0.99"), selected = "0.90"),
    if (analysis_type %in% c("cbsem", "sem")) selectInput(input_id("_moderation_method"), if (ko) "잠재조절 product-indicator 방법" else "Latent-moderation product-indicator method", choices = c("All-pairs + double-mean-centering" = "all_pairs_dmc", "Matched-pair + double-mean-centering" = "matched_pair_dmc", "All-pairs mean-centering (legacy)" = "all_pairs_mean_centered"), selected = "all_pairs_dmc"),
    if (analysis_type %in% c("cfa", "cbsem", "plssem")) checkboxInput(
      input_id("_invariance_enabled"),
      if (identical(analysis_type, "cfa")) {
        if (ko) "측정불변성 분석" else "Measurement invariance analysis"
      } else if (identical(analysis_type, "cbsem")) {
        if (ko) "구조경로 집단비교" else "Structural path group comparison"
      } else {
        if (ko) "PLS MICOM 측정불변성" else "PLS MICOM measurement invariance"
      }, value = FALSE
    ),
    if (analysis_type %in% c("cfa", "cbsem", "plssem")) selectInput(input_id("_invariance_group"), if (ko) "집단변수" else "Grouping variable", choices = character(0)),
    if (identical(analysis_type, "plssem")) selectInput(input_id("_micom_permutations"), if (ko) "MICOM permutation" else "MICOM permutations", choices = c("500" = "500", "1,000" = "1000", "5,000" = "5000"), selected = "500"),
    if (identical(analysis_type, "plssem")) numericInput(input_id("_micom_seed"), "MICOM seed", value = default_seed(), min = 1L, step = 1L)
  )

  validity_tab <- tabPanel(
    if (ko) "타당도" else "Validity",
    if (!identical(analysis_type, "plssem")) radioButtons(input_id("_validity_formula"), if (ko) "AVE·CR 계산 방식" else "AVE/CR formula", choices = stats::setNames(c("standardized", "model_implied"), c(if (ko) "표준화 부하량(Fornell-Larcker)" else "Standardized loadings (Fornell-Larcker)", if (ko) "모형모수 방식(Raykov 계열)" else "Model-implied parameters (Raykov)")), selected = "standardized", inline = TRUE),
    if (identical(analysis_type, "cfa")) checkboxInput(input_id("_mi_holdout_enabled"), if (ko) "MI 탐색·검증 표본분할" else "MI exploration/validation split", value = FALSE),
    if (identical(analysis_type, "cfa")) selectInput(input_id("_mi_holdout_fraction"), if (ko) "검증표본 비율" else "Validation-sample fraction", choices = c("20%" = "0.20", "30%" = "0.30", "40%" = "0.40"), selected = "0.30"),
    if (identical(analysis_type, "cfa")) numericInput(input_id("_mi_holdout_seed"), if (ko) "표본분할 seed" else "Sample-split seed", value = 13579L, min = 1L, step = 1L),
    if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "MI 표본분할은 연속형 ML/MLR CFA 전용이며 측정불변성 또는 Heywood 제약 재분석과 동시에 사용할 수 없습니다." else "MI splitting is for continuous ML/MLR CFA and cannot be combined with measurement invariance or Heywood-constrained reanalysis."),
    if (identical(analysis_type, "cfa")) checkboxInput(input_id("_parcel_enabled"), if (ko) "Parcel item-level 모형 생성" else "Create parcel item-level model", value = FALSE),
    if (identical(analysis_type, "cfa")) selectInput(input_id("_parcel_construct"), if (ko) "대상 공통요인" else "Target common factor", choices = character(0)),
    if (identical(analysis_type, "cfa")) selectInput(input_id("_parcel_count"), if (ko) "Parcel 수" else "Number of parcels", choices = c("3" = "3", "4" = "4"), selected = "3"),
    if (identical(analysis_type, "cfa")) textAreaInput(input_id("_parcel_purpose"), if (ko) "적용 목적과 이론적 근거" else "Purpose and substantive justification", rows = 3, placeholder = if (ko) "문항 수준 분석 대신 parcel을 고려하는 이유를 기록하십시오." else "Document why parceling is being considered instead of retaining item-level analysis."),
    if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "기본값은 비활성화입니다. 문항 수준 모형을 먼저 적합한 후 parcel 하위요인 모형을 생성합니다." else "Disabled by default. The item-level model is fitted before creating the parcel-factor model."),
    if (identical(analysis_type, "plssem")) selectInput(input_id("_redundancy_construct"), if (ko) "형성형 합성변수" else "Formative composite", choices = character(0)),
    if (identical(analysis_type, "plssem")) selectInput(input_id("_redundancy_criterion"), if (ko) "전역 기준변수" else "Global criterion variable", choices = stats::setNames("", if (ko) "선택하지 않음" else "Not selected")),
    if (identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", if (ko) "Redundancy analysis는 형성형 합성변수 점수와 동일한 개념을 측정하는 별도 전역 기준변수의 관계를 평가합니다." else "Redundancy analysis relates the formative-composite score to a separate global criterion measuring the same concept.")
  )

  diagnostics_tab <- tabPanel(
    if (ko) "진단" else "Diagnostics",
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_htmt_threshold"), if (ko) "HTMT 기준" else "HTMT threshold", choices = c("Strict (.85)" = "0.85", "Lenient (.90)" = "0.90"), selected = "0.85"),
    if (!identical(analysis_type, "plssem")) selectInput(input_id("_mi_mode"), if (ko) "MI 출력 기준" else "MI output method", choices = stats::setNames(c("theory", "conventional"), c(if (ko) "이론적 허용 MI + 누적 적합도" else "Theory-allowed MI with cumulative fit", if (ko) "일반 프로그램 방식(전체 MI)" else "Conventional output (all MI)")), selected = "theory"),
    if (identical(analysis_type, "plssem")) tagList(
      tags$p(class = "structural-option-note", if (ko) "PLS 진단은 반복 PLSpredict 표본외 예측, 잔차 기반 근사 적합도, 공선성과 구성개념별 품질지표를 구분해 검토합니다." else "PLS diagnostics distinguish repeated PLSpredict out-of-sample assessment while reviewing residual-based approximate fit, collinearity, and construct-level quality indices."),
      selectInput(input_id("_pls_predict_folds"), if (ko) "PLSpredict 교차검증" else "PLSpredict cross-validation", choices = c("Do not compute" = "0", "5-fold" = "5", "10-fold" = "10"), selected = "0"),
      selectInput(input_id("_pls_predict_reps"), if (ko) "PLSpredict 반복" else "PLSpredict repetitions", choices = c("5" = "5", "10" = "10", "20" = "20"), selected = "10"),
      numericInput(input_id("_pls_predict_seed"), "PLSpredict seed", value = default_seed(), min = 1L, step = 1L),
      tags$p(class = "structural-option-note", if (ko) "Direct Antecedents 방식으로 지표별 표본 밖 RMSE/MAE를 PLS와 선형모형 기준값에 비교합니다." else "The Direct Antecedents scheme compares indicator-level out-of-sample RMSE/MAE against PLS and linear-model benchmarks.")
    )
  )

  common_method_tab <- if (!identical(analysis_type, "plssem")) tabPanel(
    if (ko) "동일방법편의" else "Common Method",
    checkboxInput(input_id("_common_method_enabled"), if (ko) "동일방법편의 진단 실행" else "Run common method bias diagnostics", value = FALSE),
    checkboxGroupInput(
      input_id("_common_method_methods"), if (ko) "진단 방법" else "Common method diagnostics",
      choices = stats::setNames(
        c("harman", "single_factor_cfa", "common_latent_factor"),
        c(if (ko) "Harman 단일요인 점검" else "Harman single-factor screen", if (ko) "단일요인 CFA 비교" else "Single-factor CFA comparison", if (ko) "공통잠재요인 점검" else "Common latent factor screen")
      ), selected = c("harman", "single_factor_cfa")
    ),
    textAreaInput(input_id("_common_method_procedural_controls"), if (ko) "설계단계 절차적 통제" else "Design-stage procedural controls", value = "", rows = 3, placeholder = if (ko) "예: 응답원·측정시점 분리, 익명성, 문항 순서·척도 형식 설계" else "e.g., source/time separation, anonymity, item ordering, scale-format design"),
    textInput(input_id("_common_method_marker_variable"), if (ko) "Marker variable(기록용)" else "Marker variable (record only)", value = ""),
    textAreaInput(input_id("_common_method_marker_rationale"), if (ko) "Marker 선정·타당화 근거" else "Marker selection and validity rationale", value = "", rows = 2),
    tags$p(class = "structural-option-note", if (ko) "동일방법편의 진단은 증거 점검용이며, 편의가 없다는 증명이 아닙니다. Marker 정보는 감사기록용입니다." else "Common method diagnostics are evidence screens, not proof that bias is absent. Marker information is recorded for audit only.")
  )

  div(
    class = "custom-model-analysis-options structural-run-options-tabs analysis-tabbed-options",
    tabsetPanel(type = "tabs", estimation_tab, bootstrap_tab, advanced_tab, validity_tab, diagnostics_tab, common_method_tab)
  )
}
