# Structural equation canvas analysis option controls.

structural_canvas_result_coefficient_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("b_p", "b_t", "beta_t", "beta_p", "b_beta"),
    c("B(p)", "B(t)", "beta(t)", "beta(p)", "B(Beta)")
  )
}

structural_analysis_options_panel <- function(analysis_type = "cbsem", language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
        div(
          class = "custom-model-analysis-options structural-run-options-tabs analysis-tabbed-options",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              if (ko) "추정" else "Estimation",
          selectInput(paste0(structural_analysis_prefix(analysis_type), "_estimator"), if (ko) "추정 방법" else "Estimator", choices = if (analysis_type == "plssem") c("PLS" = "PLS", "PLSc" = "PLSc") else c("ML" = "ML", "MLR" = "MLR", "WLSMV" = "WLSMV")),
          selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_objective"),
            if (ko) "주요 분석 목적" else "Primary analysis objective",
            choices = stats::setNames(c("confirmatory", "explanatory", "predictive", "scores"), c(if (ko) "이론 검증" else "Confirmatory theory testing", if (ko) "구조 설명" else "Structural explanation", if (ko) "표본외 예측" else "Out-of-sample prediction", if (ko) "구성개념 점수 활용" else "Construct-score use")),
            selected = "confirmatory"
          ),
          uiOutput(paste0(structural_analysis_prefix(analysis_type), "_method_recommendation")),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_missing"), if (ko) "결측치 처리" else "Missing data", choices = stats::setNames(c("fiml", "listwise"), c("FIML", if (ko) "목록 삭제" else "Listwise deletion"))),
          if (analysis_type != "plssem") tags$p(class = "structural-option-note", if (ko) "순서형 지표 또는 WLSMV 추정량을 사용하면 lavaan 제약에 따라 FIML 대신 pairwise 결측 처리가 적용됩니다." else "When ordered indicators or the WLSMV estimator are used, lavaan uses pairwise missing-data handling instead of FIML."),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_scale"), if (ko) "잠재변수 스케일" else "Latent scale", choices = stats::setNames(c("marker", "variance"), c(if (ko) "첫 지표 부하량 = 1" else "Marker loading = 1", if (ko) "잠재변수 분산 = 1" else "Latent variance = 1"))),
          if (analysis_type != "plssem") selectInput(paste0(structural_analysis_prefix(analysis_type), "_rmsea_ci"), if (ko) "RMSEA 신뢰수준" else "RMSEA confidence level", choices = c("90% CI" = "0.90", "95% CI" = "0.95", "99% CI" = "0.99"), selected = "0.90"),
          if (analysis_type %in% c("cfa", "cbsem")) checkboxInput(
            paste0(structural_analysis_prefix(analysis_type), "_invariance_enabled"),
            if (identical(analysis_type, "cfa")) {
              if (ko) "측정불변성 분석" else "Measurement invariance analysis"
            } else {
              if (ko) "구조경로 집단비교" else "Structural path group comparison"
            },
            value = FALSE
          ),
          if (analysis_type %in% c("cfa", "cbsem")) selectInput(paste0(structural_analysis_prefix(analysis_type), "_invariance_group"), if (ko) "집단변수" else "Grouping variable", choices = character(0)),
          if (identical(analysis_type, "plssem")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_pls_predict_folds"),
            if (ko) "PLSpredict 교차검증" else "PLSpredict cross-validation",
            choices = c("Do not compute" = "0", "5-fold" = "5", "10-fold" = "10"),
            selected = "0"
          ),
          if (identical(analysis_type, "plssem")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_pls_predict_reps"),
            if (ko) "PLSpredict 반복" else "PLSpredict repetitions",
            choices = c("1" = "1", "3" = "3", "5" = "5"),
            selected = "1"
          ),
          if (identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", if (ko) "PLSpredict는 Direct Antecedents 방식으로 indicator별 out-of-sample RMSE/MAE를 PLS와 선형모형 기준값으로 비교합니다. fold와 반복 수가 커질수록 실행 시간이 늘어납니다." else "PLSpredict uses the Direct Antecedents scheme and compares indicator-level out-of-sample RMSE/MAE against a linear-model benchmark. More folds and repetitions increase run time.")
            ),
            tabPanel(
              if (ko) "타당도" else "Validity",
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_validity_formula"),
            if (ko) "AVE·CR 계산 방식" else "AVE/CR formula",
            choices = stats::setNames(c("standardized", "model_implied"), c(if (ko) "표준화 부하량(Fornell-Larcker)" else "Standardized loadings (Fornell-Larcker)", if (ko) "모형모수 방식(Raykov 계열)" else "Model-implied parameters (Raykov)")),
            selected = "standardized"
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_reliability_bootstrap"),
            if (ko) "AVE·신뢰도 bootstrap CI" else "AVE/reliability bootstrap CI",
            choices = c("Do not compute" = "0", "500 resamples" = "500", "1,000 resamples" = "1000", "2,000 resamples" = "2000"), selected = "0"
          ),
          if (identical(analysis_type, "cfa")) numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_reliability_seed"),
            if (ko) "AVE·신뢰도 bootstrap seed" else "AVE/reliability bootstrap seed",
            value = default_seed(), min = 1L, step = 1L
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_reliability_ci_method"),
            if (ko) "AVE/reliability CI method" else "AVE/reliability CI method",
            choices = c("Percentile" = "percentile", "BCa (slower)" = "bca"),
            selected = "percentile"
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_bollen_stine_bootstrap"),
            if (ko) "Bollen-Stine 전체 적합도 bootstrap" else "Bollen-Stine global-fit bootstrap",
            choices = c("Do not compute" = "0", "500 resamples" = "500", "1,000 resamples" = "1000", "2,000 resamples" = "2000"), selected = "0"
          ),
          if (identical(analysis_type, "cfa")) numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_bollen_stine_seed"),
            if (ko) "Bollen-Stine bootstrap seed" else "Bollen-Stine bootstrap seed",
            value = default_seed(), min = 1L, step = 1L
          ),
          if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "Bollen-Stine 검정은 결측이 없는 연속형 단일집단 ML CFA에서만 실행됩니다." else "Bollen-Stine is available only for complete continuous single-group CFA estimated with ML."),
          if (identical(analysis_type, "cfa")) checkboxInput(paste0(structural_analysis_prefix(analysis_type), "_mi_holdout_enabled"), if (ko) "MI 탐색·검증 표본분할" else "MI exploration/validation split", value = FALSE),
          if (identical(analysis_type, "cfa")) selectInput(paste0(structural_analysis_prefix(analysis_type), "_mi_holdout_fraction"), if (ko) "검증표본 비율" else "Validation-sample fraction", choices = c("20%" = "0.20", "30%" = "0.30", "40%" = "0.40"), selected = "0.30"),
          if (identical(analysis_type, "cfa")) numericInput(paste0(structural_analysis_prefix(analysis_type), "_mi_holdout_seed"), if (ko) "표본분할 seed" else "Sample-split seed", value = 13579L, min = 1L, step = 1L),
          if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "MI 표본분할은 연속형 ML/MLR CFA 전용이며 측정불변성 또는 Heywood 제약 재분석과 동시에 사용할 수 없습니다." else "MI splitting is for continuous ML/MLR CFA and cannot be combined with measurement invariance or Heywood-constrained reanalysis."),
          if (identical(analysis_type, "cfa")) checkboxInput(
            paste0(structural_analysis_prefix(analysis_type), "_parcel_enabled"),
            if (ko) "Parcel 계획 미리보기" else "Preview a parceling plan",
            value = FALSE
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_parcel_construct"),
            if (ko) "대상 공통요인" else "Target common factor",
            choices = character(0)
          ),
          if (identical(analysis_type, "cfa")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_parcel_count"),
            if (ko) "Parcel 수" else "Number of parcels",
            choices = c("3" = "3", "4" = "4"), selected = "3"
          ),
          if (identical(analysis_type, "cfa")) textAreaInput(
            paste0(structural_analysis_prefix(analysis_type), "_parcel_purpose"),
            if (ko) "적용 목적과 이론적 근거" else "Purpose and substantive justification",
            rows = 3,
            placeholder = if (ko) "문항 수준 분석 대신 parcel을 고려하는 이유를 기록하십시오." else "Document why parceling is being considered instead of retaining item-level analysis."
          ),
          if (identical(analysis_type, "cfa")) tags$p(class = "structural-option-note", if (ko) "기본값은 비활성화입니다. 미리보기는 데이터에 변수를 만들지 않으며 item-level CFA를 대체하지 않습니다." else "Disabled by default. The preview does not create variables and does not replace the item-level CFA."),
          if (identical(analysis_type, "plssem")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_pls_bootstrap"),
            if (ko) "PLS bootstrap CI/p" else "PLS bootstrap CI/p",
            choices = c("1,000 resamples" = "1000", "5,000 resamples" = "5000", "10,000 resamples" = "10000", "50,000 resamples" = "50000"),
            selected = "5000"
          ),
          if (identical(analysis_type, "plssem")) numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_pls_seed"),
            if (ko) "PLS bootstrap seed" else "PLS bootstrap seed",
            value = default_seed(), min = 1L, step = 1L
          ),
          if (identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", if (ko) "PLS bootstrap은 경로계수, outer loading, outer weight의 percentile CI와 p 값을 표시합니다. 반복 수가 클수록 시간이 늘어납니다." else "PLS bootstrap reports percentile CIs and p values for paths, outer loadings, and outer weights. Larger resample counts take longer."),
          if (identical(analysis_type, "plssem")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_redundancy_construct"),
            if (ko) "형성형 합성변수" else "Formative composite",
            choices = character(0)
          ),
          if (identical(analysis_type, "plssem")) selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_redundancy_criterion"),
            if (ko) "전역 기준변수" else "Global criterion variable",
            choices = stats::setNames("", if (ko) "선택하지 않음" else "Not selected")
          ),
          if (identical(analysis_type, "plssem")) tags$p(class = "structural-option-note", if (ko) "Redundancy analysis는 형성형 합성변수 점수와 이론적으로 동일한 개념을 측정하는 별도 전역 기준변수의 관계를 평가합니다. 단순히 상관이 높은 임의 변수를 선택하지 마십시오." else "Redundancy analysis relates the formative-composite score to a separate global criterion measuring the same concept. Do not choose an arbitrary variable merely because it correlates highly."),
            ),
            tabPanel(
              if (ko) "진단" else "Diagnostics",
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_threshold"),
            if (ko) "HTMT 기준" else "HTMT threshold",
            choices = c("Strict (.85)" = "0.85", "Lenient (.90)" = "0.90"),
            selected = "0.85"
          ),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_bootstrap"),
            if (ko) "HTMT 부트스트랩 CI" else "HTMT bootstrap CI",
            choices = c("Do not compute" = "0", "500 resamples" = "500", "1,000 resamples" = "1000", "2,000 resamples" = "2000"),
            selected = "0"
          ),
          if (analysis_type != "plssem") numericInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_seed"),
            if (ko) "HTMT 부트스트랩 seed" else "HTMT bootstrap seed",
            value = default_seed(), min = 1L, step = 1L
          ),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_htmt_ci_method"),
            if (ko) "HTMT CI method" else "HTMT CI method",
            choices = c("Percentile" = "percentile", "BCa (slower)" = "bca"),
            selected = "percentile"
          ),
          if (analysis_type != "plssem") selectInput(
            paste0(structural_analysis_prefix(analysis_type), "_mi_mode"),
            if (ko) "MI 출력 기준" else "MI output method",
            choices = stats::setNames(
              c("theory", "conventional"),
              c(if (ko) "이론적 허용 MI + 누적 적합도" else "Theory-allowed MI with cumulative fit", if (ko) "일반 프로그램 방식(전체 MI)" else "Conventional output (all MI)")
            ),
            selected = "theory"
          )
            ),
            if (analysis_type != "plssem") tabPanel(
              if (ko) "동일방법편의" else "Common Method",
              checkboxInput(
                paste0(structural_analysis_prefix(analysis_type), "_common_method_enabled"),
                if (ko) "동일방법편의 진단 실행" else "Run common method bias diagnostics",
                value = FALSE
              ),
              checkboxGroupInput(
                paste0(structural_analysis_prefix(analysis_type), "_common_method_methods"),
                if (ko) "진단 방법" else "Common method diagnostics",
                choices = stats::setNames(
                  c("harman", "single_factor_cfa", "common_latent_factor"),
                  c(
                    if (ko) "Harman 단일요인 점검" else "Harman single-factor screen",
                    if (ko) "단일요인 CFA 비교" else "Single-factor CFA comparison",
                    if (ko) "공통잠재요인 점검" else "Common latent factor screen"
                  )
                ),
                selected = c("harman", "single_factor_cfa")
              ),
              tags$p(
                class = "structural-option-note",
                if (ko) {
                  "동일방법편의 진단은 증거 점검용이며, 편의가 없다는 증명이 아닙니다."
                } else {
                  "Common method diagnostics are reported as evidence screens, not as proof that common method bias is absent."
                }
              )
            )
          )
        )
}
