structural_canvas_register_fit_diagnostic_outputs <- function(output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn) {
output[[paste0(prefix, "_result_fit")]] <- renderUI({
  values <- result_table("fit")
  shiny::req(nrow(values) > 0)
  ci_percent <- round(100 * as.numeric(fit_result()$rmsea_ci %||% .90))
  comparison_fits <- if (isTRUE(fit_result()$modified_from_baseline) && !is.null(fit_result()$baseline_fit)) list(fit_result()$baseline_fit, fit_result()$fit) else list(fit_result()$fit)
  fit_selections <- structural_canvas_common_fit_measures(comparison_fits, fit_result()$estimator %||% "ML", fit_result()$rmsea_ci %||% .90)
  selection <- fit_selections[[length(fit_selections)]]
  fit_labels <- selection$labels
  baseline_selection <- if (length(fit_selections) > 1L) fit_selections[[1L]] else NULL
  same_measure_keys <- is.null(baseline_selection) || identical(selection$keys, baseline_selection$keys)
  if (!same_measure_keys) fit_labels[c(5L, 6L, 8L)] <- c("Adjusted CFI", "Adjusted TLI", "Adjusted RMSEA")
  tagList(tags$table(
    class = "table table-striped table-bordered structural-fit-table",
    tags$thead(
      tags$tr(
        tags$th(rowspan = "2", "Model"),
        tags$th(rowspan = "2", if (!same_measure_keys) HTML("Adjusted &chi;<sup>2</sup>*") else if (grepl("Scaled", fit_labels[[1L]], fixed = TRUE)) HTML("Scaled &chi;<sup>2</sup>*") else HTML("&chi;<sup>2</sup>")), tags$th(rowspan = "2", "df"), tags$th(rowspan = "2", "p"), tags$th(rowspan = "2", "Q"),
        tags$th(rowspan = "2", paste0(fit_labels[[5L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", paste0(fit_labels[[6L]], if (selection$adjusted) "*" else "")), tags$th(rowspan = "2", "SRMR"), tags$th(rowspan = "2", paste0(fit_labels[[8L]], if (selection$adjusted) "*" else "")),
        tags$th(colspan = "2", paste0(ci_percent, "% CI"))
      ),
      tags$tr(tags$th("LLCI"), tags$th("ULCI"))
    ),
    tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, , drop = TRUE]), tags$td))))
  ), if (selection$adjusted)
    tags$p(class = "structural-result-note", if (is.null(baseline_selection) || same_measure_keys) {
      paste0("* Reported lavaan measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    } else {
      paste0("* Original-model measures: ", paste(unname(baseline_selection$keys), collapse = ", "), "; modified-model measures: ", paste(unname(selection$keys), collapse = ", "), ". SRMR has no separate robust correction.")
    }),
    if ((!is.null(baseline_selection) && baseline_selection$values[[2L]] == 0) || selection$values[[2L]] == 0)
      tags$p(class = "structural-result-note", "Q (chi-square/df) and some fit indices are not interpretable for a saturated model with df = 0.")
  )
})
output[[paste0(prefix, "_result_identification")]] <- renderUI({
  bundle <- fit_result()
  issues <- bundle$identification %||% data.frame()
  if (!nrow(issues)) return(tags$p(class = "structural-result-note", "Pre-fit structural identification check: no rule-based issues detected."))
  tags$div(class = "structural-identification-result",
    tags$h5("Pre-fit identification diagnostics"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(issues), tags$th))),
      tags$tbody(lapply(seq_len(nrow(issues)), function(index) tags$tr(lapply(as.character(issues[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", "This rule-based screen does not prove mathematical identification; lavaan estimation, degrees of freedom, information-matrix checks, and solution admissibility remain decisive.")
  )
})
output[[paste0(prefix, "_result_normality")]] <- renderUI({
  bundle <- fit_result()
  if (analysis_type == "plssem" || length(bundle$ordered %||% character(0))) return(NULL)
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  diagnosis <- structural_canvas_mardia(dataset_fn(), indicators)
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
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
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
})
output[[paste0(prefix, "_result_risk_diagnostics")]] <- renderUI({
  bundle <- fit_result()
  if (analysis_type == "plssem") return(NULL)
  factor_correlations <- structural_canvas_factor_correlation_diagnostics(bundle$fit)
  flagged_correlations <- factor_correlations[factor_correlations$Severity != "Acceptable", , drop = FALSE]
  if (nrow(flagged_correlations)) {
    flagged_correlations$Correlation <- vapply(flagged_correlations$Correlation, format_decimal3, character(1))
    flagged_correlations[["Absolute correlation"]] <- vapply(flagged_correlations[["Absolute correlation"]], format_decimal3, character(1))
  }
  categories <- structural_canvas_ordered_category_diagnostics(dataset_fn(), bundle$ordered %||% character(0))
  flagged_categories <- if (nrow(categories)) categories[categories$Status != "Adequate", , drop = FALSE] else categories
  if (nrow(flagged_categories)) flagged_categories$Percent <- paste0(vapply(flagged_categories$Percent, format_decimal3, character(1)), "%")
  ordered_pairs <- structural_canvas_ordered_pair_diagnostics(dataset_fn(), bundle$ordered %||% character(0))
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
})
output[[paste0(prefix, "_result_missing_outliers")]] <- renderUI({
  bundle <- fit_result()
  if (analysis_type == "plssem") return(NULL)
  indicators <- lavaan::lavNames(bundle$fit, "ov")
  missing <- structural_canvas_missing_diagnostics(dataset_fn(), indicators)
  outliers <- if (!length(bundle$ordered %||% character(0))) structural_canvas_mahalanobis_diagnostics(dataset_fn(), indicators) else list(available = FALSE, reason = "Mahalanobis diagnostics are not reported for ordered indicators.")
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
})
output[[paste0(prefix, "_result_fit_difference")]] <- renderUI({
  bundle <- fit_result()
  if (!identical(bundle$comparison_type %||% "", "mi") || is.null(bundle$baseline_fit)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  report <- structural_canvas_model_difference_report(bundle)
  if (!nrow(report) || !isTRUE(report$Available[[1L]])) {
    reason <- if (nrow(report)) as.character(report$Reason[[1L]]) else "eligibility was not established"
    return(tags$p(class = "structural-result-note", if (ko) paste0("공식 모형 차이 검정을 표시하지 않았습니다: ", reason) else paste0("A formal model-difference test was suppressed: ", reason)))
  }
  tags$div(
    class = "structural-fit-difference",
    tags$h5(if (ko) "기존 모형 vs 탐색적 수정 모형" else "Original vs exploratory modified model"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th("Δχ²"), tags$th("Δdf"), tags$th("p"))),
      tags$tbody(tags$tr(
        tags$td(format_decimal3(report$`Delta chi-square`[[1L]])),
        tags$td(format_decimal3(report$`Delta df`[[1L]])),
        tags$td(format_p(report$p[[1L]]))
      ))
    ),
    tags$p(class = "structural-result-note", report$Method[[1L]]),
    tags$p(class = "structural-result-note", if (ko) "이 수정은 동일 자료의 MI를 보고 선택했으므로, 이 차이 검정은 탐색적 결과이며 확인적 근거로 해석하면 안 됩니다." else "Because the modification was selected using MI from the same data, this difference test is exploratory and should not be treated as confirmatory evidence.")
  )
})
output[[paste0(prefix, "_result_invariance")]] <- renderUI({
  structural_canvas_invariance_result_ui(fit_result())
})
output[[paste0(prefix, "_result_fit_guidance")]] <- renderUI({
  structural_canvas_fit_guidance_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_rmsea_tests")]] <- renderUI({
  structural_canvas_rmsea_tests_result_ui(fit_result())
})
output[[paste0(prefix, "_result_information_criteria")]] <- renderUI({
  structural_canvas_information_criteria_result_ui(fit_result())
})
output[[paste0(prefix, "_result_bollen_stine")]] <- renderUI({
  structural_canvas_bollen_stine_result_ui(fit_result(), statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_result_heywood")]] <- renderUI({
  structural_canvas_heywood_result_ui(fit_result(), dataset_fn(), prefix, analysis_type)
})
  invisible(TRUE)
}
