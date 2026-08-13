# Structural fit summary result rendering.

structural_canvas_fit_guidance_result_ui <- function(bundle, language) {
  ko <- identical(normalize_app_language(language), "ko")
  comparison_fits <- if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) list(bundle$baseline_fit, bundle$fit) else list(bundle$fit)
  selections <- structural_canvas_common_fit_measures(comparison_fits, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
  labels <- if (length(selections) > 1L) c(if (ko) "기존 모형" else "Original model", if (ko) "탐색적 수정 모형" else bundle$comparison_label %||% "Modified model") else if (ko) "기존 모형" else "Original model"
  tables <- lapply(seq_along(selections), function(index) {
    guidance <- structural_canvas_fit_guidance(selections[[index]]$values)
    guidance$Model <- labels[[index]]
    guidance[, c("Model", "Metric", "Value", "Guidance", "Reference"), drop = FALSE]
  })
  table <- do.call(rbind, tables)
  table$Value <- vapply(table$Value, format_decimal3, character(1))
  severity <- c("Good" = 1L, "Marginal" = 2L, "Review" = 3L, "Not assessed" = 4L)
  summaries <- vapply(split(table$Guidance, table$Model), function(values) {
    if (all(values == "Not assessed")) return("Not assessed")
    score <- max(vapply(values[values != "Not assessed"], function(value) severity[[value]], integer(1)))
    names(severity)[match(score, severity)]
  }, character(1))
  div(class = "structural-fit-guidance-result",
    tags$h5(if (ko) "기준 기반 적합도 안내" else "Reference-based fit guidance"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
    ),
    tags$p(paste(vapply(names(summaries), function(name) paste0(name, ": ", summaries[[name]]), character(1)), collapse = " | ")),
    tags$p(class = "structural-result-note", if (ko) "Good/Marginal/Review 표시는 흔히 쓰는 근사 기준에 따른 설명용 안내입니다. 보편적 수용 규칙이 아니며 모형 식별, 잔차 진단, 모수 타당성, 이론, 표본 특성, 대안 모형 비교를 대체하지 않습니다." else "Good/Marginal/Review labels are descriptive reference guidance based on commonly used approximate cutoffs. They are not universal acceptance rules and do not replace model identification, residual diagnostics, parameter plausibility, theory, sample characteristics, or comparison with plausible alternatives."),
    tags$p(class = "structural-result-note", if (ko) "증분 적합도 안내: CFI/TLI >= .95 Good, >= .90 Marginal. 절대 적합도 안내: RMSEA <= .06 Good, <= .08 Marginal; SRMR <= .08 Good, <= .10 Marginal. 이 범위 밖 값은 Review로 표시합니다." else "Incremental-fit guidance: CFI/TLI >= .95 Good, >= .90 Marginal. Absolute-fit guidance: RMSEA <= .06 Good, <= .08 Marginal; SRMR <= .08 Good, <= .10 Marginal. Values outside these ranges are marked Review."),
    if (any(table$Guidance == "Not assessed")) tags$p(class = "structural-result-note", if (ko) "포화모형(df = 0)이거나 적합도 지수가 없으면 적합도 안내를 평가하지 않습니다." else "Fit guidance is not assessed for saturated models (df = 0) or unavailable fit indices.")
  )

}

structural_canvas_rmsea_tests_result_ui <- function(bundle, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  table <- structural_canvas_rmsea_hypothesis_tests(bundle)
  display <- table
  for (column in c("RMSEA", "Close-fit H0", "Not-close H0")) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  for (column in c("Close-fit p", "Not-close p")) display[[column]] <- vapply(display[[column]], format_p, character(1))
  tagList(
    tags$h5(if (ko) "RMSEA 가설검정" else "RMSEA hypothesis tests"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "Close-fit 검정의 귀무가설은 모집단 RMSEA <= .05입니다. 작은 p값은 close fit을 기각합니다. Not-close 검정의 귀무가설은 모집단 RMSEA >= .08입니다. 작은 p값은 poor approximate fit을 기각합니다. 두 검정은 RMSEA 추정값 및 신뢰구간과 함께 해석하십시오." else "Close-fit tests H0: population RMSEA <= .05; a small p value rejects close fit. Not-close tests H0: population RMSEA >= .08; a small p value rejects poor approximate fit. Interpret both with the RMSEA estimate and confidence interval."),
    tags$p(class = "structural-result-note", if (ko) "가능한 경우 보고된 RMSEA와 맞는 robust 또는 scaled p값을 선택합니다. 이 가설검정은 표본크기에 민감하며 독립적인 모형 채택 규칙이 아닙니다." else "Robust or scaled p values are selected to match the reported RMSEA when available. These hypothesis tests are sample-size sensitive and are not standalone model-acceptance rules.")
  )

}

structural_canvas_information_criteria_result_ui <- function(bundle, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  table <- structural_canvas_information_criteria(bundle)
  if (!any(is.finite(table$AIC)) && !any(is.finite(table$BIC))) return(NULL)
  display <- table
  numeric_columns <- names(display)[vapply(display, is.numeric, logical(1))]
  for (column in numeric_columns) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  tagList(
    tags$h5(if (ko) "우도 기반 정보기준" else "Likelihood-based information criteria"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "AIC, BIC, adjusted BIC가 낮을수록 복잡도 벌점을 반영한 상대적 기대 적합이 더 좋다는 뜻입니다. Delta 값은 이 표에 표시된 모형 중 가장 작은 기준값을 기준으로 계산합니다." else "Lower AIC, BIC, and adjusted BIC indicate better relative expected fit after penalizing complexity; delta values are relative to the smallest criterion in this displayed set."),
    if (any(grepl("^Not comparable", table$`Comparison status`))) tags$p(class = "structural-result-note", if (ko) "분석 관측치, 관측변수, 추정량 계열 또는 모형 허용성이 모형 간에 달라 delta 값을 숨겼습니다." else "Delta values were suppressed because analyzed observations, observed variables, estimator family, or model admissibility differed across models."),
    tags$p(class = "structural-result-note", if (ko) "정보기준은 같은 관측치와 관측변수, 같은 likelihood 및 추정량 계열로 적합한 모형끼리만 비교하십시오. 이는 절대적 적합도 검정이 아니며, 비교 가능한 likelihood가 없는 WLSMV 모형에는 보고하지 않습니다." else "Compare information criteria only for models fitted to the same observations and observed variables with the same likelihood and estimator family. They are not absolute goodness-of-fit tests and are not reported for WLSMV models without a comparable likelihood.")
  )

}

structural_canvas_bollen_stine_result_ui <- function(bundle, language) {
  result <- bundle$bollen_stine_result %||% NULL
  if (is.null(result) || !nrow(result)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  display <- result
  display[["Observed chi-square"]] <- vapply(display[["Observed chi-square"]], format_decimal3, character(1))
  display[["Bootstrap p"]] <- vapply(display[["Bootstrap p"]], format_p, character(1))
  for (column in c("Monte Carlo SE", "Monte Carlo 95% lower", "Monte Carlo 95% upper")) {
    display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  }
  display[["Valid %"]] <- paste0(vapply(display[["Valid %"]], format_decimal3, character(1)), "%")
  tagList(
    tags$h5(if (ko) "Bollen-Stine 부트스트랩 전반적 적합도 검정" else "Bollen-Stine bootstrap global-fit test"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", if (ko) "부트스트랩 p값은 plus-one 보정식 (1 + 관측값 이상인 부트스트랩 χ² 수) / (1 + 유효 반복 수)을 사용합니다. 유효 반복은 주 CFA와 같은 수렴, 분산, 공분산행렬, 자유도, 잠재상관 허용성 점검을 통과한 반복입니다. 작은 p값은 exact-fit 귀무가설에서 전반적 모형 부적합을 의미합니다." else "The bootstrap p value uses the plus-one correction: (1 + bootstrap chi-square values at least as large as observed) / (1 + valid replicates). Valid replicates pass the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA. A small p value indicates global model misfit under the exact-fit null."),
    tags$p(class = "structural-result-note", if (ko) "Monte Carlo SE와 95% Wilson 구간은 유효 부트스트랩 반복 수가 유한하기 때문에 생기는 시뮬레이션 오차를 나타냅니다. 모집단 모형 모수에 대한 신뢰구간이 아닙니다." else "Monte Carlo SE and the 95% Wilson interval quantify simulation error from the finite number of valid bootstrap replicates; they are not a confidence interval for a population model parameter."),
    if (any(result$Status != "Adequate")) tags$p(class = "structural-result-note", if (ko) "요청한 재표집의 80% 미만만 수렴하고 허용 가능한 통계량을 만들었습니다. 부트스트랩 p값과 Monte Carlo 구간을 불안정한 값으로 보고, 보고 전에 수렴 또는 허용성 문제를 해결하십시오." else "Fewer than 80% of requested resamples produced a converged admissible statistic. Treat the bootstrap p value and Monte Carlo interval as unstable; resolve convergence or admissibility problems before reporting."),
    if (isTRUE(bundle$modified_from_baseline)) tags$p(class = "structural-result-note", if (ko) "이 모형은 분석 자료를 보고 수정한 탐색적 수정 모형입니다. Bollen-Stine 결과는 탐색적 참고값이며 자료 기반 수정에 대한 확인적 근거를 제공하지 않습니다." else "This model was modified using the analyzed data. Its Bollen-Stine result is exploratory and does not provide confirmatory evidence for the data-driven modification."),
    tags$p(class = "structural-result-note", if (ko) "이 transformed-data 검정은 결측이 없는 연속형 단일집단 ML CFA에서만 사용할 수 있으며, 근사 적합도 지수, 잔차 진단, 실질적 모형 평가를 대체하지 않습니다." else "This transformed-data test is available only for complete continuous single-group ML CFA and does not replace approximate fit indices, residual diagnostics, or substantive model evaluation.")
  )

}
