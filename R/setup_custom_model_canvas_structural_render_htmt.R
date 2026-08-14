# Structural equation canvas HTMT render outputs.

structural_canvas_register_htmt_outputs <- function(output, prefix, fit_result, app_language_fn = NULL) {
render_htmt_tables <- function(include_details = FALSE) {
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  fit <- bundle$fit
  standardized <- lavaan::standardizedSolution(fit)
  loadings <- standardized[standardized$op == "=~", c("lhs", "rhs"), drop = FALSE]
  loadings <- loadings[loadings$rhs %in% lavaan::lavNames(fit, "ov"), , drop = FALSE]
  factor_names <- unique(loadings$lhs)
  if (length(factor_names) < 2L) return(NULL)
  indicators_by_factor <- stats::setNames(lapply(factor_names, function(name) unique(loadings$rhs[loadings$lhs == name])), factor_names)
  sample_statistics <- lavaan::lavInspect(fit, "sampstat")
  sample_covariance <- sample_statistics$cov %||% NULL
  if (is.null(sample_covariance)) return(tags$p(class = "structural-result-note", if (ko) "다집단 표본통계에 대한 HTMT는 현재 결과 화면에 표시하지 않습니다." else "HTMT is not currently displayed for multigroup sample statistics."))
  sample_correlations <- stats::cov2cor(as.matrix(sample_covariance))
  threshold <- as.numeric(bundle$htmt_threshold %||% .85)
  htmt <- structural_canvas_htmt(sample_correlations, indicators_by_factor, threshold)
  matrix_values <- matrix("", nrow = length(factor_names), ncol = length(factor_names) + 1L)
  colnames(matrix_values) <- c("Factor", factor_names)
  for (row in seq_along(factor_names)) {
    matrix_values[row, 1L] <- factor_names[[row]]
    for (column in seq_along(factor_names)) {
      if (row == column) matrix_values[row, column + 1L] <- "—"
      else if (row > column) matrix_values[row, column + 1L] <- format_decimal3(htmt$matrix[row, column])
    }
  }
  pair_table <- htmt$pairs
  if ("HTMT" %in% names(pair_table)) pair_table$HTMT <- vapply(pair_table$HTMT, format_decimal3, character(1))
  if ("Reason" %in% names(pair_table)) pair_table$Reason[!nzchar(pair_table$Reason)] <- "—"
  pair_columns <- intersect(c("Factor 1", "Factor 2", "HTMT", "Criterion", "Reason"), names(pair_table))
  bootstrap_reps <- as.integer(bundle$htmt_bootstrap %||% 0L)
  bootstrap_seed <- as.integer(bundle$htmt_seed %||% 12345L)
  htmt_ci_method <- structural_canvas_bootstrap_ci_method(bundle$htmt_ci_method %||% "percentile")
  htmt_ci_label <- if (identical(htmt_ci_method, "bca")) "BCa" else "percentile"
  bootstrap_table <- NULL
  bootstrap_incomplete <- FALSE
  bootstrap_caution <- FALSE
  bootstrap_unreliable <- FALSE
  bootstrap_bca_unavailable <- FALSE
  if (bootstrap_reps > 0L) {
    bootstrap_table <- bundle$htmt_bootstrap_result %||% NULL
    if (!is.null(bootstrap_table)) {
      bootstrap_incomplete <- any(bootstrap_table[["Valid replicates"]] < bootstrap_reps)
      bootstrap_caution <- any(bootstrap_table$Status == "Caution")
      bootstrap_unreliable <- any(bootstrap_table$Status == "Unreliable")
      bootstrap_bca_unavailable <- "CI method" %in% names(bootstrap_table) && any(bootstrap_table[["CI method"]] == "BCa unavailable")
      names(bootstrap_table)[names(bootstrap_table) == "Lower"] <- "95% CI lower"
      names(bootstrap_table)[names(bootstrap_table) == "Upper"] <- "95% CI upper"
      names(bootstrap_table)[names(bootstrap_table) == "One-sided upper"] <- "One-sided 95% upper"
      bootstrap_table[["95% CI lower"]] <- vapply(bootstrap_table[["95% CI lower"]], format_decimal3, character(1))
      bootstrap_table[["95% CI upper"]] <- vapply(bootstrap_table[["95% CI upper"]], format_decimal3, character(1))
      bootstrap_table[["One-sided 95% upper"]] <- vapply(bootstrap_table[["One-sided 95% upper"]], format_decimal3, character(1))
      bootstrap_table[["Valid %"]] <- paste0(vapply(bootstrap_table[["Valid %"]], format_decimal3, character(1)), "%")
    }
  }
  if (!isTRUE(include_details)) {
    return(tagList(
      tags$h5(paste0("HTMT (threshold = ", format(threshold, nsmall = 2L), ")")),
      structural_canvas_basic_html_table(as.data.frame(matrix_values, check.names = FALSE), class = "table table-striped table-bordered structural-htmt-matrix")
    ))
  }
  tagList(
    tags$h5(if (ko) "HTMT 세부 판단" else "HTMT detailed criteria"),
    if (length(pair_columns)) structural_canvas_basic_html_table(pair_table[, pair_columns, drop = FALSE], class = "table table-striped table-bordered structural-htmt-criterion"),
    if (!is.null(bootstrap_table)) tagList(
      tags$h5(if (ko) paste0("HTMT ", htmt_ci_label, " 부트스트랩 신뢰구간 (", bootstrap_reps, "회 재표집; seed = ", bootstrap_seed, ")") else paste0("HTMT ", htmt_ci_label, " bootstrap confidence intervals (", bootstrap_reps, " resamples; seed = ", bootstrap_seed, ")")),
      structural_canvas_basic_html_table(bootstrap_table, class = "table table-striped table-bordered structural-htmt-bootstrap")
    ),
    tags$p(class = "structural-result-note", if (ko) "HTMT는 문항 상관의 절댓값을 사용합니다. 선택 기준보다 낮은 값은 'Criterion met'으로 표시됩니다." else "HTMT uses absolute item correlations. Values below the selected threshold are marked 'Criterion met'."),
    if (length(bundle$ordered %||% character(0))) tags$p(class = "structural-result-note", if (ko) "순서형 지표에서는 HTMT가 lavaan의 polychoric latent-response 상관을 사용합니다." else "For ordered indicators, HTMT uses lavaan's polychoric latent-response correlations."),
    if (bootstrap_reps <= 0L) tags$p(class = "structural-result-note", if (ko) "HTMT 점추정값은 기술적 지표입니다. 구간추정이 필요하면 분석 옵션에서 HTMT 부트스트랩 CI를 선택하십시오." else "HTMT point estimates are descriptive. Select HTMT bootstrap CI in the analysis options when interval estimates are required."),
    if (bootstrap_reps > 0L && is.null(bootstrap_table)) tags$p(class = "structural-result-note", if (ko) "선택된 지표로부터 HTMT 부트스트랩 신뢰구간을 계산하지 못했습니다." else "HTMT bootstrap confidence intervals could not be estimated from the selected indicators."),
    if (bootstrap_incomplete) tags$p(class = "structural-result-note", if (ko) "일부 부트스트랩 재표집은 허용 가능한 상관행렬을 만들지 못했습니다. 순서형 범주가 없거나 희소한 경우가 흔한 원인입니다. 유효 반복 수가 줄어든 구간은 신중하게 해석하십시오." else "Some bootstrap resamples could not produce an admissible correlation matrix, commonly because an ordered category was absent or sparse. Interpret intervals with reduced valid-replicate counts cautiously."),
    if (bootstrap_bca_unavailable) tags$p(class = "structural-result-note", if (ko) "BCa unavailable은 해당 쌍의 편향보정 또는 잭나이프 가속도를 계산하지 못했다는 뜻입니다. 유효 반복 수를 늘리거나 보고에는 percentile CI 사용을 검토하십시오." else "BCa unavailable means the bias-correction or jackknife acceleration could not be computed for that pair; increase valid replicates or use percentile CI for reporting."),
    if (bootstrap_caution) tags$p(class = "structural-result-note", if (ko) "HTMT 부트스트랩 상태 Caution은 요청 재표집의 50% 이상 80% 미만만 유효했다는 뜻입니다. 구간은 불안정할 수 있으므로 유효 반복 수를 함께 보고하십시오." else "HTMT bootstrap status Caution means that 50% to less than 80% of requested resamples were valid. Treat the interval as unstable and report the valid-replicate count."),
    if (bootstrap_unreliable) tags$p(class = "structural-result-note", if (ko) "HTMT 부트스트랩 상태 Unreliable은 요청 재표집의 50% 미만만 유효했다는 뜻입니다. 신뢰한계와 기준 판단은 평가하지 않으며, 변별타당도 근거로 사용하면 안 됩니다." else "HTMT bootstrap status Unreliable means that fewer than 50% of requested resamples were valid. Confidence limits and threshold decisions are not assessed and should not be used as discriminant-validity evidence."),
    if (!is.null(bootstrap_table)) tags$p(class = "structural-result-note", paste0(
      if (ko) paste0(htmt_ci_label, " 구간은 사례 재표집", if (identical(htmt_ci_method, "bca")) "과 leave-one-out 잭나이프 가속도" else "", if (length(bundle$ordered %||% character(0))) " 및 각 재표집에서 재추정된 polychoric 상관" else "", "에 기반합니다. 'Upper < threshold'는 선택된 .85/.90 기준에 대한 단측 95% 상한을 사용합니다. 'Upper < 1'은 양측 95% 구간이 1을 제외하는지 나타냅니다. 이 판단은 점추정 기준과 분리해 보고합니다.") else paste0("The ", htmt_ci_label, " interval is based on case resampling", if (identical(htmt_ci_method, "bca")) " with leave-one-out jackknife acceleration" else "", if (length(bundle$ordered %||% character(0))) " with polychoric correlations re-estimated in each resample" else "",
      ". 'Upper < threshold' uses the one-sided 95% upper confidence limit for the selected .85/.90 criterion. 'Upper < 1' indicates whether the two-sided 95% interval excludes 1. These are intentionally reported separately from the point-estimate criterion.")
    ))
  )
}
output[[paste0(prefix, "_result_htmt")]] <- renderUI({
  render_htmt_tables(include_details = FALSE)
})
output[[paste0(prefix, "_result_htmt_details")]] <- renderUI({
  render_htmt_tables(include_details = TRUE)
})
  invisible(TRUE)
}
