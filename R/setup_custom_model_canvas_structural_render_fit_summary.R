# Structural fit summary result rendering.

structural_canvas_lavaan_quality_number <- function(values, direction = "single") {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return("")
  value <- switch(
    direction,
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    mean = mean(values, na.rm = TRUE),
    values[[1L]]
  )
  format_decimal3(value)
}

structural_canvas_lavaan_quality_loadings <- function(fit) {
  standardized <- tryCatch(lavaan::standardizedSolution(fit), error = function(error) data.frame())
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(error) character(0))
  loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs", "est.std"), drop = FALSE]
  loadings$est.std <- suppressWarnings(as.numeric(loadings$est.std))
  loadings
}

structural_canvas_lavaan_quality_validity <- function(fit) {
  loadings <- structural_canvas_lavaan_quality_loadings(fit)
  if (!nrow(loadings)) return(list(ave = numeric(0), cr = numeric(0)))
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(error) character(0))
  standardized <- tryCatch(lavaan::standardizedSolution(fit), error = function(error) data.frame())
  theta <- matrix(0, nrow = length(observed), ncol = length(observed), dimnames = list(observed, observed))
  theta_rows <- standardized$op == "~~" & standardized$lhs %in% observed & standardized$rhs %in% observed
  for (index in which(theta_rows)) {
    lhs <- standardized$lhs[[index]]
    rhs <- standardized$rhs[[index]]
    theta[lhs, rhs] <- standardized$est.std[[index]]
    theta[rhs, lhs] <- standardized$est.std[[index]]
  }
  latent_names <- unique(loadings$lhs)
  ave <- stats::setNames(vapply(latent_names, function(name) {
    lambda <- loadings$est.std[loadings$lhs == name]
    mean(lambda^2, na.rm = TRUE)
  }, numeric(1)), latent_names)
  cr <- stats::setNames(vapply(latent_names, function(name) {
    lambda <- loadings$est.std[loadings$lhs == name]
    indicators <- loadings$rhs[loadings$lhs == name]
    common <- sum(lambda, na.rm = TRUE)^2
    residual <- sum(theta[indicators, indicators, drop = FALSE], na.rm = TRUE)
    denominator <- common + residual
    if (is.finite(denominator) && denominator > 0) common / denominator else NA_real_
  }, numeric(1)), latent_names)
  list(ave = ave, cr = cr)
}

structural_canvas_lavaan_quality_max_latent_correlation <- function(fit) {
  correlations <- tryCatch(as.matrix(lavaan::lavInspect(fit, "cor.lv")), error = function(error) matrix(numeric(0), 0L, 0L))
  if (!length(correlations) || nrow(correlations) < 2L || ncol(correlations) < 2L) return(NA_real_)
  correlations[upper.tri(correlations, diag = TRUE)] <- NA_real_
  values <- suppressWarnings(abs(as.numeric(correlations)))
  if (!any(is.finite(values))) NA_real_ else max(values, na.rm = TRUE)
}

structural_canvas_lavaan_quality_structural <- function(fit) {
  standardized <- tryCatch(lavaan::standardizedSolution(fit), error = function(error) data.frame())
  structural <- standardized[standardized$op == "~", , drop = FALSE]
  beta <- suppressWarnings(abs(as.numeric(structural$est.std %||% numeric(0))))
  r2_values <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(error) numeric(0))
  outcomes <- unique(as.character(structural$lhs %||% character(0)))
  r2 <- suppressWarnings(as.numeric(r2_values[outcomes]))
  list(path_count = nrow(structural), max_beta = beta, r2 = r2)
}

structural_canvas_lavaan_quality_status <- function(item, value, analysis_type = "cfa") {
  numeric_value <- suppressWarnings(as.numeric(value))
  unavailable <- !nzchar(as.character(value %||% "")) || identical(value, "not recorded")
  if (unavailable) return("Not assessed")
  if (identical(item, "Converged")) return(if (identical(value, "TRUE")) "OK" else "Review")
  if (identical(item, "Admissible solution")) return(if (identical(value, "TRUE")) "OK" else "Review")
  if (identical(item, "Model df")) return(if (is.finite(numeric_value) && numeric_value > 0) "OK" else "Review")
  if (identical(item, "CFI")) return(if (is.finite(numeric_value) && numeric_value >= .90) "OK" else "Review")
  if (identical(item, "TLI")) return(if (is.finite(numeric_value) && numeric_value >= .90) "OK" else "Review")
  if (identical(item, "RMSEA")) return(if (is.finite(numeric_value) && numeric_value <= .08) "OK" else "Review")
  if (identical(item, "SRMR")) return(if (is.finite(numeric_value) && numeric_value <= .10) "OK" else "Review")
  if (identical(item, "Min standardized loading")) return(if (is.finite(numeric_value) && numeric_value >= .40) "OK" else "Review")
  if (identical(item, "Min CR")) return(if (is.finite(numeric_value) && numeric_value >= .70) "OK" else "Review")
  if (identical(item, "Min AVE")) return(if (is.finite(numeric_value) && numeric_value >= .50) "OK" else "Review")
  if (identical(item, "Max latent correlation")) return(if (is.finite(numeric_value) && numeric_value < .85) "OK" else "Review")
  if (identical(item, "Structural path count")) {
    if (!analysis_type %in% c("sem", "cbsem")) return("Not assessed")
    return(if (is.finite(numeric_value) && numeric_value > 0) "OK" else "Review")
  }
  if (identical(item, "Max structural beta")) {
    if (!analysis_type %in% c("sem", "cbsem")) return("Not assessed")
    return(if (is.finite(numeric_value) && numeric_value < 1) "OK" else "Review")
  }
  if (identical(item, "Min endogenous R2")) {
    if (!analysis_type %in% c("sem", "cbsem")) return("Not assessed")
    return(if (is.finite(numeric_value)) "OK" else "Review")
  }
  if (identical(item, "Model status")) return(if (identical(value, "Original/prespecified model")) "OK" else "Review")
  "Not assessed"
}

structural_canvas_lavaan_quality_rows <- function(bundle, analysis_type = "cfa") {
  if (is.null(bundle) || is.null(bundle$fit) || !inherits(bundle$fit, "lavaan")) {
    return(data.frame(Item = character(0), Value = character(0), Status = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  fit <- bundle$fit
  selection <- structural_canvas_fit_measures(fit, bundle$estimator %||% "ML", bundle$rmsea_ci %||% .90)
  values <- selection$values
  loadings <- structural_canvas_lavaan_quality_loadings(fit)
  validity <- structural_canvas_lavaan_quality_validity(fit)
  structural <- structural_canvas_lavaan_quality_structural(fit)
  converged <- bundle$converged %||% bundle$diagnostics$converged %||% tryCatch(lavaan::lavInspect(fit, "converged"), error = function(error) NA)
  admissible <- bundle$admissible %||% bundle$diagnostics$admissible %||% tryCatch(structural_canvas_fit_admissibility(fit)$admissible, error = function(error) NA)
  modified <- if (isTRUE(bundle$modified_model) || isTRUE(bundle$modified_from_baseline) || length(bundle$mi_history %||% list())) "Exploratory modified model" else "Original/prespecified model"
  items <- c(
    "Converged",
    "Admissible solution",
    "Model df",
    "CFI",
    "TLI",
    "RMSEA",
    "SRMR",
    "Min standardized loading",
    "Min CR",
    "Min AVE",
    "Max latent correlation",
    "Structural path count",
    "Max structural beta",
    "Min endogenous R2",
    "Model status"
  )
  displayed_values <- c(
    if (is.na(converged)) "not recorded" else as.character(isTRUE(converged)),
    if (is.na(admissible)) "not recorded" else as.character(isTRUE(admissible)),
    structural_canvas_lavaan_quality_number(values[[2L]]),
    structural_canvas_lavaan_quality_number(values[[5L]]),
    structural_canvas_lavaan_quality_number(values[[6L]]),
    structural_canvas_lavaan_quality_number(values[[8L]]),
    structural_canvas_lavaan_quality_number(values[[7L]]),
    structural_canvas_lavaan_quality_number(abs(loadings$est.std), "min"),
    structural_canvas_lavaan_quality_number(validity$cr, "min"),
    structural_canvas_lavaan_quality_number(validity$ave, "min"),
    structural_canvas_lavaan_quality_number(structural_canvas_lavaan_quality_max_latent_correlation(fit)),
    as.character(structural$path_count),
    structural_canvas_lavaan_quality_number(structural$max_beta, "max"),
    structural_canvas_lavaan_quality_number(structural$r2, "min"),
    modified
  )
  data.frame(
    Item = items,
    Value = displayed_values,
    Status = mapply(structural_canvas_lavaan_quality_status, items, displayed_values, MoreArgs = list(analysis_type = analysis_type), USE.NAMES = FALSE),
    Guidance = c(
      "Must be TRUE before interpreting estimates.",
      "Must be TRUE before reporting fit, reliability, validity, or structural paths as final.",
      "df = 0 indicates a saturated model; approximate fit is not substantively diagnostic.",
      "Review below .90; .95 is a common descriptive target.",
      "Review below .90; .95 is a common descriptive target.",
      "Review above .08; .06 is a common descriptive target.",
      "Review above .10; .08 is a common descriptive target.",
      "Review indicators below .40/.70 depending on purpose and theory.",
      "Review construct reliability below .70.",
      "Review convergent validity below .50.",
      "Review discriminant validity near or above .85/.90.",
      "Report whether the model includes structural regressions.",
      "Inspect coefficients near or above |1| for inadmissibility or suppression.",
      "Report explanatory power for endogenous latent variables when structural paths exist.",
      "MI-based modifications are exploratory unless validated in independent data."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

structural_canvas_lavaan_quality_status_summary <- function(rows) {
  if (!nrow(rows) || !"Status" %in% names(rows)) return("Quality status: not assessed.")
  counts <- table(factor(rows$Status, levels = c("OK", "Review", "Not assessed")))
  paste0(
    "Quality status: OK=", counts[["OK"]],
    "; Review=", counts[["Review"]],
    "; Not assessed=", counts[["Not assessed"]],
    "."
  )
}

structural_canvas_lavaan_quality_review_rows <- function(rows) {
  if (!nrow(rows) || !"Status" %in% names(rows)) {
    return(data.frame(Item = character(0), Value = character(0), Guidance = character(0), stringsAsFactors = FALSE))
  }
  review <- rows[rows$Status == "Review", c("Item", "Value", "Guidance"), drop = FALSE]
  rownames(review) <- NULL
  review
}

structural_canvas_lavaan_quality_result_ui <- function(bundle, analysis_type = "cfa", language = statedu_initial_language()) {
  rows <- structural_canvas_lavaan_quality_rows(bundle, analysis_type)
  if (!nrow(rows)) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  summary <- structural_canvas_lavaan_quality_status_summary(rows)
  review_rows <- structural_canvas_lavaan_quality_review_rows(rows)
  div(
    class = "result-section regression-result-panel structural-lavaan-quality-result",
    h4(if (ko) "SEM quality checklist" else "SEM quality checklist"),
    tags$p(class = "structural-result-note structural-quality-status-summary", summary),
    tags$h5(if (ko) "Review focus" else "Review focus"),
    if (nrow(review_rows)) structural_canvas_basic_html_table(review_rows) else tags$p(class = "structural-result-note", "No Review rows in the quality checklist."),
    structural_canvas_basic_html_table(rows),
    if (any(rows$Status == "Review")) tags$p(
      class = "structural-result-note",
      "Rows marked Review should be resolved or explicitly justified before confirmatory reporting."
    ),
    tags$p(
      class = "structural-result-note",
      if (ko) {
        "This checklist summarizes lavaan SEM/CFA convergence, admissibility, global fit, measurement quality, discriminant-validity risk, and structural explanatory-power conditions for reporting and review."
      } else {
        "This checklist summarizes lavaan SEM/CFA convergence, admissibility, global fit, measurement quality, discriminant-validity risk, and structural explanatory-power conditions for reporting and review."
      }
    )
  )
}

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
