# Structural data diagnostic result rendering.

structural_canvas_normality_result_ui <- function(bundle, dataset, analysis_type, language) {
  if (analysis_type == "plssem" || length(bundle$ordered %||% character(0))) return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  diagnosis <- bundle$normality_diagnostics %||% structural_canvas_mardia(dataset, indicators)
  if (!isTRUE(diagnosis$available)) {
    reason <- if (ko && identical(diagnosis$reason, "Mardia diagnostics require at least two complete cases and two indicators.")) {
      "Mardia 진단에는 완전 사례 2개 이상과 지표 2개 이상이 필요합니다."
    } else {
      diagnosis$reason
    }
    return(div(class = "result-section regression-result-panel structural-normality-result",
      h4(if (ko) "다변량 정규성" else "Multivariate normality"),
      tags$p(class = "structural-result-note", reason)
    ))
  }
  table <- data.frame(
    Test = c("Mardia skewness", "Mardia kurtosis"),
    Estimate = c(format_decimal3(diagnosis$skewness), format_decimal3(diagnosis$kurtosis)),
    Statistic = c(format_decimal3(diagnosis$skew_statistic), format_decimal3(diagnosis$kurtosis_z)),
    df = c(format_decimal3(diagnosis$skew_df), "—"),
    p = c(format_p(diagnosis$skew_p), format_p(diagnosis$kurtosis_p)),
    check.names = FALSE
  )
  recommendation_label <- if (ko && identical(diagnosis$recommendation, "MLR recommended")) {
    "MLR 권장"
  } else if (ko && identical(diagnosis$recommendation, "No Mardia test flag; normality not established")) {
    "Mardia 검정 표시는 없으나 정규성이 입증된 것은 아님"
  } else {
    diagnosis$recommendation
  }
  div(class = "result-section regression-result-panel structural-normality-result",
    h4(if (ko) "다변량 정규성 및 추정량 안내" else "Multivariate normality and estimator guidance"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", if (ko) paste0("안내: ", recommendation_label, ". 이 권고는 진단용이며 추정량을 자동으로 변경하지 않습니다.") else paste0("Guidance: ", recommendation_label, ". This recommendation is diagnostic and does not automatically change the estimator.")),
    tags$p(class = "structural-result-note", if (ko) paste0("사용된 완전 사례: ", diagnosis$n, " / ", diagnosis$original_n, "; 지표 수: ", diagnosis$p, ".") else paste0("Complete cases used: ", diagnosis$n, " of ", diagnosis$original_n, "; indicators: ", diagnosis$p, ".")),
    if (isTRUE(diagnosis$sampled)) tags$p(class = "structural-result-note", if (ko) "계산 안정성을 위해 Mardia 통계량은 완전 사례 중 균등 간격 결정 표본 2,000건으로 계산했습니다." else "For computational stability, Mardia statistics used an evenly spaced deterministic subsample of 2,000 complete cases."),
    tags$p(class = "structural-result-note", if (ko) "Mardia 검정은 표본 수에 민감한 전체분포 선별검사입니다. 유의하지 않은 결과는 다변량 정규성을 입증하지 않으며, 유의한 결과도 왜도·첨도의 실질적 크기나 추정치에 대한 영향 정도를 단독으로 보여주지 않습니다. 분포 형태, 이상치, 측정 가정과 민감도 분석을 함께 검토하십시오." else "Mardia tests are sample-size-sensitive omnibus screens. Nonsignificance does not establish multivariate normality, while significance alone does not quantify the substantive size of skew/kurtosis or its impact on estimates. Consider distribution shape, outliers, measurement assumptions, and sensitivity analyses."),
  )
}

structural_canvas_risk_diagnostics_result_ui <- function(bundle, dataset, analysis_type, language = statedu_initial_language()) {
  if (analysis_type == "plssem") return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  factor_correlations <- structural_canvas_factor_correlation_diagnostics(bundle$fit)
  flagged_correlations <- factor_correlations[factor_correlations$Severity != "Below correlation review reference", , drop = FALSE]
  if (nrow(flagged_correlations)) {
    flagged_correlations$Correlation <- vapply(flagged_correlations$Correlation, format_decimal3, character(1))
    flagged_correlations[["Absolute correlation"]] <- vapply(flagged_correlations[["Absolute correlation"]], format_decimal3, character(1))
  }
  categories <- structural_canvas_ordered_category_diagnostics(dataset, bundle$ordered %||% character(0))
  flagged_categories <- if (nrow(categories)) categories[categories$Status != "No sparsity flag", , drop = FALSE] else categories
  if (nrow(flagged_categories)) flagged_categories$Percent <- paste0(vapply(flagged_categories$Percent, format_decimal3, character(1)), "%")
  ordered_pairs <- structural_canvas_ordered_pair_diagnostics(dataset, bundle$ordered %||% character(0))
  flagged_pairs <- if (nrow(ordered_pairs)) ordered_pairs[ordered_pairs$Status != "No sparsity flag", , drop = FALSE] else ordered_pairs
  if (nrow(flagged_pairs)) flagged_pairs[["Empty %"]] <- paste0(vapply(flagged_pairs[["Empty %"]], format_decimal3, character(1)), "%")
  error_covariances <- structural_canvas_error_covariance_diagnostics(bundle$snapshot %||% list())
  if (!nrow(flagged_correlations) && !nrow(flagged_categories) && !nrow(flagged_pairs) && error_covariances$count == 0L) return(NULL)
  render_data_table <- function(table) tags$table(class = "table table-striped table-bordered",
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
  )
  div(class = "result-section regression-result-panel structural-risk-result",
    h4(if (ko) "데이터 및 모형 위험 진단" else "Data and model risk diagnostics"),
    if (nrow(flagged_correlations)) tagList(tags$h5(if (ko) "높은 잠재변수 상관" else "High latent correlations"), render_data_table(flagged_correlations)),
    if (nrow(flagged_categories)) tagList(tags$h5(if (ko) "희소한 순서형 범주" else "Sparse ordered categories"), render_data_table(flagged_categories)),
    if (nrow(flagged_pairs)) tagList(tags$h5(if (ko) "희소한 순서형 지표 교차표" else "Sparse ordered-indicator cross-tabulations"), render_data_table(flagged_pairs)),
    if (error_covariances$count > 0L) tagList(
      tags$h5(if (ko) "상관된 측정오차" else "Correlated measurement errors"),
      tags$p(paste0(error_covariances$count, " of ", error_covariances$possible, " possible indicator pairs (", format_decimal3(100 * error_covariances$ratio), "%): ", error_covariances$status, "."))
    ),
    tags$p(class = "structural-result-note", if (ko) "잠재변수 상관이 .85 이상이면 판별타당도 검토가 필요합니다. .90 이상은 높은 상관, .95 이상은 심각한 구성개념 중복 가능성을 뜻합니다." else "Latent correlations of .85 or greater warrant discriminant-validity review; .90 or greater are high, and .95 or greater indicate severe construct overlap."),
    if (nrow(flagged_categories)) tags$p(class = "structural-result-note", if (ko) "범주가 비어 있거나, 빈도가 max(5, 유효응답의 1%) 이하이거나, 유효응답의 95% 이상을 차지하면 표시합니다. 희소하거나 지나치게 지배적인 범주는 thresholds와 polychoric 상관을 불안정하게 만들 수 있습니다." else "A category is flagged when empty, when its count is no greater than max(5, 1% of valid responses), or when it contains at least 95% of valid responses. Sparse or extremely dominant categories can destabilize thresholds and polychoric correlations."),
    if (nrow(flagged_pairs)) tags$p(class = "structural-result-note", if (ko) "각 순서형 지표 쌍은 관측되었거나 선언된 모든 범주에 대해 교차표를 계산합니다. 빈 셀 또는 빈도가 max(5, pairwise 유효응답의 1%) 이하인 비빈 셀은 polychoric 상관과 WLSMV 가중행렬을 불안정하게 할 수 있어 표시합니다. 범주 병합은 실질적 근거가 있어야 하며 순서를 보존해야 합니다." else "Each ordered-indicator pair is cross-tabulated over all observed or declared categories. Empty cells or nonempty cells with counts no greater than max(5, 1% of pairwise-valid responses) are flagged because they can destabilize polychoric correlations and the WLSMV weight matrix. Category collapsing requires substantive justification and must preserve order."),
    if (error_covariances$count > 0L) tags$p(class = "structural-result-note", if (ko) "여러 상관오차는 문항 중복 또는 자료 기반 과적합을 시사할 수 있습니다. 각 공분산에는 실질적 정당화가 필요합니다." else "Several correlated errors can indicate item redundancy or data-driven overfitting. Each covariance requires substantive justification.")
  )
}

structural_canvas_missing_outliers_result_ui <- function(bundle, dataset, analysis_type, language = statedu_initial_language()) {
  if (analysis_type == "plssem") return(NULL)
  ko <- identical(normalize_app_language(language), "ko")
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  missing <- bundle$missing_diagnostics %||% structural_canvas_missing_diagnostics(dataset, indicators)
  outliers <- if (!length(bundle$ordered %||% character(0))) structural_canvas_mahalanobis_diagnostics(dataset, indicators) else list(available = FALSE, reason = "Mahalanobis diagnostics are not reported for ordered indicators.")
  if (isTRUE(missing$available)) {
    missing_variables <- missing$variables[missing$variables$Missing > 0L, , drop = FALSE]
    if (nrow(missing_variables)) missing_variables$Percent <- paste0(vapply(missing_variables$Percent, format_decimal3, character(1)), "%")
  } else missing_variables <- data.frame()
  sensitivity <- structural_canvas_missing_sensitivity_rows(bundle)
  render_table <- function(table) tags$table(class = "table table-striped table-bordered",
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
  )
  div(class = "result-section regression-result-panel structural-missing-outlier-result",
    h4(if (ko) "결측 자료 및 다변량 이상치" else "Missing data and multivariate outliers"),
    if (nrow(missing_variables)) tagList(tags$h5(if (ko) "변수별 결측" else "Variable-level missingness"), render_table(missing_variables)) else tags$p(if (ko) "지표 변수에서 결측값이 발견되지 않았습니다." else "No missing indicator values were detected."),
    if (isTRUE(missing$available)) tags$p(if (ko) paste0("완전 사례: ", missing$complete_n, " / ", missing$n, "; 불완전 사례: ", missing$incomplete_n, "; 서로 다른 결측 패턴: ", missing$pattern_count, ".") else paste0("Complete cases: ", missing$complete_n, " of ", missing$n, "; incomplete cases: ", missing$incomplete_n, "; distinct missingness patterns: ", missing$pattern_count, ".")),
    if (isTRUE(missing$available) && identical(bundle$missing %||% "", "listwise") && missing$incomplete_n > 0L) tags$p(class = "structural-result-note", if (ko) paste0("목록 삭제로 지표 중 하나라도 결측인 ", missing$incomplete_n, "개 사례(", format_decimal3(missing$incomplete_percent), "%)가 모형 적합에서 제외됩니다. 완전사례와 제외사례의 차이 및 선택편향 가능성을 검토하십시오.") else paste0("Listwise deletion excludes ", missing$incomplete_n, " cases (", format_decimal3(missing$incomplete_percent), "%) with any missing indicator from model estimation. Examine differences between retained and excluded cases and the potential for selection bias.")),
    if (isTRUE(missing$available) && identical(bundle$missing %||% "", "pairwise")) tags$p(class = "structural-result-note", if (ko) paste0("지표 쌍별 최소 가용 N = ", missing$minimum_pairwise_n, ". 서로 다른 공분산이 서로 다른 사례 집합에 근거하므로 쌍별 포함 N의 불균형과 양의 정부호성을 점검하십시오.") else paste0("Minimum indicator-pair available N = ", missing$minimum_pairwise_n, ". Because covariances can use different case sets, inspect imbalance in pairwise N and positive definiteness.")),
    tags$h5(if (ko) "결측 가정 민감도 기록" else "Missing-assumption sensitivity record"),
    render_table(sensitivity),
    if (identical(sensitivity$Status[[1L]], "Review")) tags$p(class = "structural-result-note", if (ko) "FIML의 MAR 가정은 관측자료만으로 검증되지 않습니다. 가능한 MNAR 이탈 또는 대안 결측처리에서 주요 결론이 유지되는지 평가·기록하십시오." else "The MAR assumption underlying FIML is not verified by observed data alone. Assess and document whether key conclusions persist under plausible MNAR departures or alternative missing-data handling."),
    if (isTRUE(missing$available) && missing$pattern_count > 1L) tagList(tags$h5(if (ko) "결측 패턴" else "Missingness patterns"), render_table(utils::head(missing$patterns, 20L))),
    tags$p(class = "structural-result-note", if (length(bundle$ordered %||% character(0))) {
      if (ko) paste0("순서형 지표 추정은 ", bundle$missing %||% "pairwise", " 결측 처리를 사용합니다. 범주 빈도와 함께 pairwise 포함 범위의 희소성을 검토하십시오.") else paste0("Ordered-indicator estimation uses ", bundle$missing %||% "pairwise", " missing-data handling. Review sparse pairwise coverage alongside category frequencies.")
    } else if (identical(bundle$missing %||% "", "fiml")) {
      if (ko) "FIML은 모형에 포함된 변수들을 조건으로 한 missing-at-random(MAR) 가정하에 사용 가능한 모든 관측치를 사용합니다. MAR은 관측자료만으로 확정할 수 없으므로 결측 원인, 보조변수 포함 여부와 민감도 분석을 보고해야 합니다. 위의 완전 사례 수는 Mardia 및 Mahalanobis 거리 같은 진단에만 적용됩니다." else "FIML uses all available observations under a missing-at-random (MAR) assumption conditional on variables in the model. MAR cannot be established from the observed data alone; report likely causes of missingness, use of auxiliary variables, and sensitivity analyses. The complete-case count above applies only to diagnostics such as Mardia and Mahalanobis distance."
    } else {
      if (ko) paste0("적합 모형의 결측 처리 옵션: ", bundle$missing %||% "unspecified", ".") else paste0("The fitted model used missing-data option: ", bundle$missing %||% "unspecified", ".")
    }),
    tags$h5(if (ko) "Mahalanobis 이상치 후보" else "Mahalanobis outlier candidates"),
    if (!isTRUE(outliers$available)) tags$p(class = "structural-result-note", if (ko && identical(outliers$reason, "Mahalanobis diagnostics are not reported for ordered indicators.")) "순서형 지표에는 Mahalanobis 진단을 보고하지 않습니다." else outliers$reason) else if (!nrow(outliers$table)) tags$p(if (ko) paste0("p < ", outliers$alpha, "에서 표시된 완전 사례가 없습니다.") else paste0("No complete cases were flagged at p < ", outliers$alpha, ".")) else {
      outlier_table <- outliers$table
      outlier_table$Mahalanobis <- vapply(outlier_table$Mahalanobis, format_decimal3, character(1))
      outlier_table$p <- vapply(outlier_table$p, format_p, character(1))
      tagList(render_table(outlier_table), tags$p(if (ko) paste0(outliers$n, "개 완전 사례 중 ", outliers$flagged_n, "개가 p < ", outliers$alpha, "에서 표시되었습니다.") else paste0(outliers$flagged_n, " of ", outliers$n, " complete cases flagged at p < ", outliers$alpha, ".")))
    },
    tags$p(class = "structural-result-note", if (ko) "Mahalanobis 후보는 자료 오류, 특이하지만 유효한 사례, 영향점을 검토해야 합니다. 자동으로 제거하지 않으며, 필요하면 robust 추정 또는 문서화된 민감도 분석을 사용하십시오." else "Mahalanobis candidates should be investigated for data errors, unusual but valid cases, and influence. They are not removed automatically; use robust estimation or a documented sensitivity analysis when appropriate.")
  )
}
