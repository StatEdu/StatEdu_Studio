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

structural_canvas_rmsea_tests_result_ui <- function(bundle) {
  table <- structural_canvas_rmsea_hypothesis_tests(bundle)
  display <- table
  for (column in c("RMSEA", "Close-fit H0", "Not-close H0")) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  for (column in c("Close-fit p", "Not-close p")) display[[column]] <- vapply(display[[column]], format_p, character(1))
  tagList(
    tags$h5("RMSEA hypothesis tests"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Close-fit tests H0: population RMSEA <= .05; a small p value rejects close fit. Not-close tests H0: population RMSEA >= .08; a small p value rejects poor approximate fit. Interpret both with the RMSEA estimate and confidence interval."),
    tags$p(class = "structural-result-note", "Robust or scaled p values are selected to match the reported RMSEA when available. These hypothesis tests are sample-size sensitive and are not standalone model-acceptance rules.")
  )

}

structural_canvas_information_criteria_result_ui <- function(bundle) {
  table <- structural_canvas_information_criteria(bundle)
  if (!any(is.finite(table$AIC)) && !any(is.finite(table$BIC))) return(NULL)
  display <- table
  numeric_columns <- names(display)[vapply(display, is.numeric, logical(1))]
  for (column in numeric_columns) display[[column]] <- vapply(display[[column]], format_decimal3, character(1))
  tagList(
    tags$h5("Likelihood-based information criteria"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "Lower AIC, BIC, and adjusted BIC indicate better relative expected fit after penalizing complexity; delta values are relative to the smallest criterion in this displayed set."),
    if (any(grepl("^Not comparable", table$`Comparison status`))) tags$p(class = "structural-result-note", "Delta values were suppressed because analyzed observations, observed variables, estimator family, or model admissibility differed across models."),
    tags$p(class = "structural-result-note", "Compare information criteria only for models fitted to the same observations and observed variables with the same likelihood and estimator family. They are not absolute goodness-of-fit tests and are not reported for WLSMV models without a comparable likelihood.")
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
    tags$h5("Bollen-Stine bootstrap global-fit test"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    tags$p(class = "structural-result-note", "The bootstrap p value uses the plus-one correction: (1 + bootstrap chi-square values at least as large as observed) / (1 + valid replicates). Valid replicates pass the same convergence, variance, covariance-matrix, df, and latent-correlation admissibility checks as the main CFA. A small p value indicates global model misfit under the exact-fit null."),
    tags$p(class = "structural-result-note", "Monte Carlo SE and the 95% Wilson interval quantify simulation error from the finite number of valid bootstrap replicates; they are not a confidence interval for a population model parameter."),
    if (any(result$Status != "Adequate")) tags$p(class = "structural-result-note", "Fewer than 80% of requested resamples produced a converged admissible statistic. Treat the bootstrap p value and Monte Carlo interval as unstable; resolve convergence or admissibility problems before reporting."),
    if (isTRUE(bundle$modified_from_baseline)) tags$p(class = "structural-result-note", if (ko) "이 모형은 분석 자료를 보고 수정한 탐색적 수정 모형입니다. Bollen-Stine 결과는 탐색적 참고값이며 자료 기반 수정에 대한 확인적 근거를 제공하지 않습니다." else "This model was modified using the analyzed data. Its Bollen-Stine result is exploratory and does not provide confirmatory evidence for the data-driven modification."),
    tags$p(class = "structural-result-note", "This transformed-data test is available only for complete continuous single-group ML CFA and does not replace approximate fit indices, residual diagnostics, or substantive model evaluation.")
  )

}
