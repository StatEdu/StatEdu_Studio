structural_canvas_register_result_outputs <- function(input, output, prefix, canvas_output, analysis_type, selected_names_fn, variable_table_fn, labels_fn, app_language_fn, fit_result, result_table) {
output[[canvas_output]] <- renderUI({
  structural_equation_workspace(selected_names_fn(), variable_table_fn(), labels_fn(), analysis_type, statedu_current_language(app_language_fn))
})
output[[paste0(prefix, "_results")]] <- renderUI({
  shiny::req(!is.null(fit_result()))
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  div(
    class = "structural-analysis-results regression-results",
    h3(if (ko) "분석 결과" else "Analysis Results"),
    if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_reproducibility"), if (ko) "분석 기록 다운로드" else "Download analysis record", class = "btn btn-default btn-sm"),
    if (analysis_type == "cfa") downloadButton(paste0(prefix, "_download_tables"), if (ko) "결과표 Excel 다운로드" else "Download result tables", class = "btn btn-default btn-sm"),
    div(class = "result-section regression-result-panel", h4(if (ko) "1. 모형 개요" else "1. Model overview"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_overview")))),
    uiOutput(paste0(prefix, "_result_identification")),
    uiOutput(paste0(prefix, "_result_normality")),
    uiOutput(paste0(prefix, "_result_missing_outliers")),
    uiOutput(paste0(prefix, "_result_risk_diagnostics")),
    uiOutput(paste0(prefix, "_result_heywood")),
    div(class = "result-section regression-result-panel", h4(if (ko) "2. 모형 적합도" else "2. Model fit"), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit"))), uiOutput(paste0(prefix, "_result_fit_guidance")), uiOutput(paste0(prefix, "_result_rmsea_tests")), uiOutput(paste0(prefix, "_result_information_criteria")), uiOutput(paste0(prefix, "_result_bollen_stine")), div(class = "table-responsive", uiOutput(paste0(prefix, "_result_fit_difference")))),
    uiOutput(paste0(prefix, "_result_invariance")),
    div(class = "result-section regression-result-panel", h4(if (ko) "3. 잠재 구성개념 상관, 신뢰도 및 수렴·판별타당도" else "3. Latent construct correlations, reliability, and convergent/discriminant validity"), div(class = "table-responsive", tableOutput(paste0(prefix, "_result_validity"))), uiOutput(paste0(prefix, "_result_latent_correlation_ci")), uiOutput(paste0(prefix, "_result_validity_note")), uiOutput(paste0(prefix, "_result_reliability_bootstrap")), uiOutput(paste0(prefix, "_result_factor_scores")), uiOutput(paste0(prefix, "_result_htmt"))),
    div(
      class = "result-section regression-result-panel structural-measurement-result",
      h4(if (ko) "4. 측정모형" else "4. Measurement model"),
      div(class = "table-responsive", tableOutput(paste0(prefix, "_result_measurement"))),
      tags$p(class = "structural-result-note", "* Fixed reference loading; its unstandardized SE, z, and p are not estimated. The standardized loading remains a derived estimate and therefore has a confidence interval."),
      tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized loadings. A fixed reference loading has a degenerate B interval at its fixed value."),
      tags$p(class = "structural-result-note", "Standardized-loading confidence intervals are 95% delta-method intervals from lavaan; robust fitted models use the fitted model's robust covariance information."),
      tags$p(class = "structural-result-note", "R² confidence intervals are obtained by complementing the standardized residual-variance interval (lower = 1 − residual upper; upper = 1 − residual lower)."),
      tags$p(class = "structural-result-note", "Std. residual variance is the standardized diagonal residual variance for each indicator. † marks an unavailable residual value, a residual outside [0, 1], or an R² interval extending beyond [0, 1], including a negative-residual Heywood case."),
      tags$p(class = "structural-result-note", "Guidance prioritizes inadmissible residual variance, then cross-loading, a standardized-loading CI containing 0, and |β| < .40. The .40 value is a descriptive review guideline rather than a universal item-retention rule."),
      tags$p(class = "structural-result-note", "Cross-loaded indicators require theory-based interpretation; simple-structure reliability and discriminant-validity summaries, especially HTMT, may be unavailable or require caution.")
    ),
    uiOutput(paste0(prefix, "_result_higher_order")),
    uiOutput(paste0(prefix, "_result_residuals")),
    uiOutput(paste0(prefix, "_result_mi_holdout")),
    uiOutput(paste0(prefix, "_result_mi_history")),
    if (analysis_type != "plssem") div(class = "result-section regression-result-panel structural-mi-result", h4(if (ko) "6. 수정지수(MI)" else "6. Modification indices (MI)"), uiOutput(paste0(prefix, "_result_mi")))
  )
})
  structural_canvas_register_fit_diagnostic_outputs(
    output, prefix, analysis_type, fit_result, result_table, dataset_fn, app_language_fn
  )
  structural_canvas_register_validity_outputs(
    output, prefix, analysis_type, fit_result, result_table, app_language_fn
  )
  structural_canvas_register_local_fit_outputs(
    output, prefix, fit_result, app_language_fn
  )
for (kind in c("overview", "validity", "measurement")) local({
  result_kind <- kind
  output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
})
  structural_canvas_register_mi_render_outputs(
    output, prefix, fit_result, result_table, app_language_fn
  )
  invisible(TRUE)
}
