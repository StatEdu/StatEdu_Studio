# Structural data diagnostic result rendering.

structural_canvas_normality_result_ui <- function(bundle, dataset, analysis_type, language) {
  if (analysis_type == "plssem" || length(bundle$ordered %||% character(0))) return(NULL)
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  diagnosis <- structural_canvas_mardia(dataset, indicators)
  if (!isTRUE(diagnosis$available)) {
    return(div(class = "result-section regression-result-panel structural-normality-result",
      h4("Multivariate normality"), tags$p(class = "structural-result-note", diagnosis$reason)))
  }
  table <- data.frame(
    Test = c("Mardia skewness", "Mardia kurtosis"),
    Estimate = c(format_decimal3(diagnosis$skewness), format_decimal3(diagnosis$kurtosis)),
    Statistic = c(format_decimal3(diagnosis$skew_statistic), format_decimal3(diagnosis$kurtosis_z)),
    df = c(format_decimal3(diagnosis$skew_df), "—"),
    p = c(format_p(diagnosis$skew_p), format_p(diagnosis$kurtosis_p)),
    check.names = FALSE
  )
  ko <- identical(normalize_app_language(language), "ko")
  recommendation_label <- if (ko && identical(diagnosis$recommendation, "MLR recommended")) {
    "MLR 권장"
  } else if (ko && identical(diagnosis$recommendation, "ML is acceptable")) {
    "ML 사용 가능"
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
    tags$p(class = "structural-result-note", if (ko) "Mardia 검정은 표본 수에 민감합니다. p 값과 함께 분포 형태, 이상치, 측정 가정을 함께 검토하십시오." else "Mardia tests are sample-size sensitive. Consider distribution shape, outliers, and substantive measurement assumptions alongside p-values.")
  )

}

structural_canvas_risk_diagnostics_result_ui <- function(bundle, dataset, analysis_type) {
  if (analysis_type == "plssem") return(NULL)
  factor_correlations <- structural_canvas_factor_correlation_diagnostics(bundle$fit)
  flagged_correlations <- factor_correlations[factor_correlations$Severity != "Acceptable", , drop = FALSE]
  if (nrow(flagged_correlations)) {
    flagged_correlations$Correlation <- vapply(flagged_correlations$Correlation, format_decimal3, character(1))
    flagged_correlations[["Absolute correlation"]] <- vapply(flagged_correlations[["Absolute correlation"]], format_decimal3, character(1))
  }
  categories <- structural_canvas_ordered_category_diagnostics(dataset, bundle$ordered %||% character(0))
  flagged_categories <- if (nrow(categories)) categories[categories$Status != "Adequate", , drop = FALSE] else categories
  if (nrow(flagged_categories)) flagged_categories$Percent <- paste0(vapply(flagged_categories$Percent, format_decimal3, character(1)), "%")
  ordered_pairs <- structural_canvas_ordered_pair_diagnostics(dataset, bundle$ordered %||% character(0))
  flagged_pairs <- if (nrow(ordered_pairs)) ordered_pairs[ordered_pairs$Status != "Adequate", , drop = FALSE] else ordered_pairs
  if (nrow(flagged_pairs)) flagged_pairs[["Empty %"]] <- paste0(vapply(flagged_pairs[["Empty %"]], format_decimal3, character(1)), "%")
  error_covariances <- structural_canvas_error_covariance_diagnostics(bundle$snapshot %||% list())
  if (!nrow(flagged_correlations) && !nrow(flagged_categories) && !nrow(flagged_pairs) && error_covariances$count == 0L) return(NULL)
  render_data_table <- function(table) tags$table(class = "table table-striped table-bordered",
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
  )
  div(class = "result-section regression-result-panel structural-risk-result",
    h4("Data and model risk diagnostics"),
    if (nrow(flagged_correlations)) tagList(tags$h5("High latent correlations"), render_data_table(flagged_correlations)),
    if (nrow(flagged_categories)) tagList(tags$h5("Sparse ordered categories"), render_data_table(flagged_categories)),
    if (nrow(flagged_pairs)) tagList(tags$h5("Sparse ordered-indicator cross-tabulations"), render_data_table(flagged_pairs)),
    if (error_covariances$count > 0L) tagList(
      tags$h5("Correlated measurement errors"),
      tags$p(paste0(error_covariances$count, " of ", error_covariances$possible, " possible indicator pairs (", format_decimal3(100 * error_covariances$ratio), "%): ", error_covariances$status, "."))
    ),
    tags$p(class = "structural-result-note", "Latent correlations of .85 or greater warrant discriminant-validity review; .90 or greater are high, and .95 or greater indicate severe construct overlap."),
    if (nrow(flagged_categories)) tags$p(class = "structural-result-note", "A category is flagged when empty, when its count is no greater than max(5, 1% of valid responses), or when it contains at least 95% of valid responses. Sparse or extremely dominant categories can destabilize thresholds and polychoric correlations."),
    if (nrow(flagged_pairs)) tags$p(class = "structural-result-note", "Each ordered-indicator pair is cross-tabulated over all observed or declared categories. Empty cells or nonempty cells with counts no greater than max(5, 1% of pairwise-valid responses) are flagged because they can destabilize polychoric correlations and the WLSMV weight matrix. Category collapsing requires substantive justification and must preserve order."),
    if (error_covariances$count > 0L) tags$p(class = "structural-result-note", "Several correlated errors can indicate item redundancy or data-driven overfitting. Each covariance requires substantive justification.")
  )

}

structural_canvas_missing_outliers_result_ui <- function(bundle, dataset, analysis_type) {
  if (analysis_type == "plssem") return(NULL)
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  missing <- structural_canvas_missing_diagnostics(dataset, indicators)
  outliers <- if (!length(bundle$ordered %||% character(0))) structural_canvas_mahalanobis_diagnostics(dataset, indicators) else list(available = FALSE, reason = "Mahalanobis diagnostics are not reported for ordered indicators.")
  if (isTRUE(missing$available)) {
    missing_variables <- missing$variables[missing$variables$Missing > 0L, , drop = FALSE]
    if (nrow(missing_variables)) missing_variables$Percent <- paste0(vapply(missing_variables$Percent, format_decimal3, character(1)), "%")
  } else missing_variables <- data.frame()
  render_table <- function(table) tags$table(class = "table table-striped table-bordered",
    tags$thead(tags$tr(lapply(names(table), tags$th))),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
  )
  div(class = "result-section regression-result-panel structural-missing-outlier-result",
    h4("Missing data and multivariate outliers"),
    if (nrow(missing_variables)) tagList(tags$h5("Variable-level missingness"), render_table(missing_variables)) else tags$p("No missing indicator values were detected."),
    if (isTRUE(missing$available)) tags$p(paste0("Complete cases: ", missing$complete_n, " of ", missing$n, "; incomplete cases: ", missing$incomplete_n, "; distinct missingness patterns: ", missing$pattern_count, ".")),
    if (isTRUE(missing$available) && missing$pattern_count > 1L) tagList(tags$h5("Missingness patterns"), render_table(utils::head(missing$patterns, 20L))),
    tags$p(class = "structural-result-note", if (length(bundle$ordered %||% character(0))) {
      paste0("Ordered-indicator estimation uses ", bundle$missing %||% "pairwise", " missing-data handling. Review sparse pairwise coverage alongside category frequencies.")
    } else if (identical(bundle$missing %||% "", "fiml")) {
      "FIML uses all available observations under the missing-at-random assumption; the complete-case count above applies only to diagnostics such as Mardia and Mahalanobis distance."
    } else {
      paste0("The fitted model used missing-data option: ", bundle$missing %||% "unspecified", ".")
    }),
    tags$h5("Mahalanobis outlier candidates"),
    if (!isTRUE(outliers$available)) tags$p(class = "structural-result-note", outliers$reason) else if (!nrow(outliers$table)) tags$p(paste0("No complete cases were flagged at p < ", outliers$alpha, ".")) else {
      outlier_table <- outliers$table
      outlier_table$Mahalanobis <- vapply(outlier_table$Mahalanobis, format_decimal3, character(1))
      outlier_table$p <- vapply(outlier_table$p, format_p, character(1))
      tagList(render_table(outlier_table), tags$p(paste0(outliers$flagged_n, " of ", outliers$n, " complete cases flagged at p < ", outliers$alpha, ".")))
    },
    tags$p(class = "structural-result-note", "Mahalanobis candidates should be investigated for data errors, unusual but valid cases, and influence. They are not removed automatically; use robust estimation or a documented sensitivity analysis when appropriate.")
  )

}
