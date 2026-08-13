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
    output, prefix, analysis_type, fit_result, result_table
  )
output[[paste0(prefix, "_result_residuals")]] <- renderUI({
  bundle <- fit_result()
  diagnostics <- structural_canvas_residual_diagnostics(bundle$fit)
  if (!isTRUE(diagnostics$available)) return(NULL)
  matrix_table <- function(matrix_value, title) {
    values <- matrix("", nrow(matrix_value), ncol(matrix_value) + 1L)
    colnames(values) <- c("Indicator", colnames(matrix_value))
    for (row_index in seq_len(nrow(matrix_value))) {
      values[row_index, 1L] <- rownames(matrix_value)[[row_index]]
      for (column_index in seq_len(ncol(matrix_value))) {
        value <- matrix_value[row_index, column_index]
        if (is.finite(value)) values[row_index, column_index + 1L] <- format_decimal3(value)
      }
    }
    tagList(tags$h5(title), tags$table(class = "table table-striped table-bordered structural-residual-matrix",
      tags$thead(tags$tr(lapply(colnames(values), tags$th))),
      tags$tbody(lapply(seq_len(nrow(values)), function(index) tags$tr(lapply(as.character(values[index, ]), tags$td))))
    ))
  }
  largest <- diagnostics$largest
  if (nrow(largest)) {
    largest[["Standardized residual"]] <- vapply(largest[["Standardized residual"]], format_decimal3, character(1))
    largest[["Correlation residual"]] <- vapply(largest[["Correlation residual"]], format_decimal3, character(1))
  }
  div(class = "result-section regression-result-panel structural-residual-result",
    h4("5. Local fit diagnostics"),
    matrix_table(diagnostics$standardized, "Standardized residual matrix"),
    matrix_table(diagnostics$correlation, "Correlation residual matrix"),
    tags$h5(paste0("Large standardized residuals (|z| >= ", diagnostics$cutoff, ")")),
    if (!nrow(largest)) tags$p("No residuals exceeded the cutoff.") else tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(largest), tags$th))),
      tags$tbody(lapply(seq_len(nrow(largest)), function(index) tags$tr(lapply(as.character(largest[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", "Large standardized residuals identify local areas of model misfit and should be interpreted with theory rather than used as automatic modification instructions.")
  )
})
output[[paste0(prefix, "_result_higher_order")]] <- renderUI({
  bundle <- fit_result()
  higher <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
  if (!isTRUE(higher$available)) return(NULL)
  table <- higher$table
  fixed <- !is.na(table$SE) & table$SE == 0 & is.na(table$z) & is.na(table$p)
  residual_abnormal <- !is.finite(table$ResidualVariance) | table$ResidualVariance < 0 | table$ResidualVariance > 1
  residual_display <- paste0(vapply(table$ResidualVariance, format_decimal3, character(1)), ifelse(residual_abnormal, "†", ""))
  r2_interval_abnormal <- !is.finite(table$R2CILower) | !is.finite(table$R2CIUpper) | table$R2CILower < 0 | table$R2CIUpper > 1
  loading_guidance <- vapply(table$Beta, structural_canvas_higher_order_loading_guidance, character(1))
  display <- data.frame(
    `Higher-order factor` = table$HigherOrderFactor,
    `Lower-order factor` = table$LowerOrderFactor,
    B = vapply(table$B, format_decimal3, character(1)),
    `B 95% CI lower` = vapply(table$BCILower, format_decimal3, character(1)),
    `B 95% CI upper` = vapply(table$BCIUpper, format_decimal3, character(1)),
    SE = vapply(table$SE, format_decimal3, character(1)),
    Beta = vapply(table$Beta, format_decimal3, character(1)),
    `β 95% CI lower` = vapply(table$BetaCILower, format_decimal3, character(1)),
    `β 95% CI upper` = vapply(table$BetaCIUpper, format_decimal3, character(1)),
    `R²` = vapply(table$R2, format_decimal3, character(1)),
    `R² 95% CI lower` = paste0(vapply(table$R2CILower, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `R² 95% CI upper` = paste0(vapply(table$R2CIUpper, format_decimal3, character(1)), ifelse(r2_interval_abnormal, "†", "")),
    `Residual variance` = residual_display,
    Guidance = ifelse(residual_abnormal | r2_interval_abnormal, "Review residual/R² interval", loading_guidance),
    z = vapply(table$z, format_decimal3, character(1)),
    p = vapply(table$p, format_p, character(1)),
    check.names = FALSE
  )
  display$SE[fixed] <- "Fixed*"
  display$z[fixed] <- "—"
  display$p[fixed] <- "—"
  omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
  div(class = "result-section regression-result-panel structural-higher-order-result",
    h4("Higher-order CFA results"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    )),
    if (isTRUE(omega_h$available)) tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th("Higher-order factor"), tags$th("Indicators"), tags$th("Hierarchical omega (ωh)"), tags$th("Guidance"))),
      tags$tbody(tags$tr(
        tags$td(omega_h$higher_order_factor), tags$td(omega_h$indicators),
        tags$td(paste0(format_decimal3(omega_h$omega_h), if (!is.finite(omega_h$omega_h) || omega_h$omega_h < 0 || omega_h$omega_h > 1) "†" else "")),
        tags$td(structural_canvas_omega_h_guidance(omega_h$omega_h))
      ))
    )) else tags$p(class = "structural-result-note", paste0("Hierarchical omega was not reported: ", omega_h$reason)),
    tags$p(class = "structural-result-note", "Lower-order R² is the variance explained by the higher-order factor. Residual variance is reported on the standardized latent-variable scale."),
    tags$p(class = "structural-result-note", "Lower-order R² intervals complement the standardized residual-variance intervals. † also marks an R² interval extending beyond [0, 1]."),
    tags$p(class = "structural-result-note", "Higher-order standardized-loading confidence intervals are 95% delta-method intervals from lavaan."),
    tags$p(class = "structural-result-note", "B confidence intervals are 95% intervals for unstandardized higher-order loadings; a fixed reference loading has a degenerate interval at its fixed value."),
    tags$p(class = "structural-result-note", "ωh estimates the proportion of unit-weighted total-score variance attributable to one higher-order general factor under the fitted higher-order CFA model."),
    tags$p(class = "structural-result-note", "The .40 loading and .70 ωh values are descriptive review guidelines, not universal pass/fail rules. † marks an unavailable value or a coefficient/residual variance outside [0, 1]."),
    tags$p(class = "structural-result-note", "* Fixed reference loading; SE, z, and p are not estimated.")
  )
})
for (kind in c("overview", "validity", "measurement")) local({
  result_kind <- kind
  output[[paste0(prefix, "_result_", result_kind)]] <- renderTable(result_table(result_kind), striped = TRUE, bordered = TRUE, sanitize.text.function = identity)
})
output[[paste0(prefix, "_result_mi")]] <- renderUI({
  table <- result_table("mi")
  if (!nrow(table)) return(NULL)
  theory_mi <- identical(fit_result()$mi_mode %||% "theory", "theory")
  tagList(tags$table(
    class = "table table-striped table-bordered structural-mi-table",
    tags$thead(tags$tr(
      lapply(names(table), tags$th),
      if (theory_mi) tags$th("Select")
    )),
    tags$tbody(lapply(seq_len(nrow(table)), function(index) {
      tags$tr(
        lapply(as.character(table[index, , drop = TRUE]), tags$td),
        if (theory_mi) tags$td(actionButton(
          paste0(prefix, "_mi_select_", index),
          "Select",
          class = "btn-sm structural-mi-select-button"
        ))
      )
    }))
  ),
  tags$p(class = "structural-result-note", "MI p treats each modification index as an unscaled asymptotic 1-df chi-square test. BH-adjusted p controls the false-discovery rate across all finite lavaan candidate modifications before the displayed MI and theory filters; MI tests reports that multiplicity-family size."),
  tags$p(class = "structural-result-note", "For MLR or WLSMV, these derived p values are not a separate robust/scaled score-test correction and should be treated as exploratory reference values."),
  tags$p(class = "structural-result-note", "EPC is the expected unstandardized parameter change if the fixed parameter is freed; Std. EPC is lavaan's fully standardized expected change (sepc.all). Consider direction and magnitude rather than MI rank alone."),
  if (theory_mi) tags$p(class = "structural-result-note", "Each Step is sequential: the displayed path is added, the model is refitted, and MI, multiplicity family, EPC, and cumulative fit for the next row are recomputed from that updated model. Rows are not simultaneous candidates from one unchanged model."),
  if (theory_mi) tags$p(class = "structural-result-note", "Skipped unsafe counts higher-ranked candidates rejected for nonconvergence, post.check failure, negative variance, a non-positive-definite or boundary residual/latent/parameter covariance matrix, invalid df, or |latent correlation| >= 1. Skipped details records each rejected path and diagnostic reason; a skipped candidate is not offered for automatic application."),
  tags$p(class = "structural-result-note", "Neither an unadjusted nor adjusted p value justifies a modification. Use effect size (EPC/standardized EPC), residual diagnostics, admissibility, theory, and preferably independent validation."))
})
output[[paste0(prefix, "_result_mi_history")]] <- renderUI({
  bundle <- fit_result()
  history <- bundle$mi_history %||% data.frame()
  if (!nrow(history)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  display <- history[, setdiff(names(history), "Signature"), drop = FALSE]
  for (name in intersect(c("MI", "EPC", "CFI", "TLI", "RMSEA", "SRMR"), names(display))) {
    display[[name]] <- vapply(display[[name]], format_decimal3, character(1))
  }
  display$Justification[!nzchar(display$Justification)] <- if (ko) "제공되지 않음" else "Not provided"
  div(class = "result-section regression-result-panel structural-mi-history-result",
    h4(if (ko) "MI 수정 이력" else "MI modification history"),
    tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(index) tags$tr(lapply(as.character(display[index, ]), tags$td))))
    ),
    tags$p(class = "structural-result-note", if (ko) "MI, EPC, 누적 적합도 값은 해당 경로를 선택한 시점의 값입니다. 근거란에는 각 모수를 자유화한 실질적 이유를 기록해야 합니다." else "MI, EPC, and cumulative fit values are those available when the path was selected. The justification should document the substantive reason for freeing each parameter."),
    tags$p(class = "structural-result-note", if (ko) "MI 기반 수정은 탐색적 수정 모형이며 독립 표본에서 교차검증해야 합니다." else "MI-driven modifications are exploratory and should be cross-validated in an independent sample.")
  )
})
output[[paste0(prefix, "_result_mi_holdout")]] <- renderUI({
  bundle <- fit_result()
  if (!isTRUE(bundle$mi_holdout_enabled)) return(NULL)
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  comparison <- bundle$holdout_comparison %||% NULL
  if (is.null(comparison)) return(tags$div(class = "result-section regression-result-panel",
    tags$h4(if (ko) "MI 홀드아웃 검증" else "MI holdout validation"),
    tags$p(paste0(if (ko) "탐색 표본 N = " else "Exploration N = ", nrow(bundle$analysis_data), if (ko) "; 예약 검증 표본 N = " else "; reserved validation N = ", nrow(bundle$validation_data), ".")),
    tags$p(class = "structural-result-note", if (ko) "MI 후보와 현재 표시된 CFA 추정값은 탐색 표본만 사용한 결과입니다. MI 경로를 적용한 뒤 검증 결과가 표시됩니다." else "MI candidates and all currently displayed CFA estimates are based only on the exploration sample. Validation results will appear after an MI path is applied.")
  ))
  table <- comparison$table
  for (name in c("Chisq", "df", "CFI", "TLI", "SRMR", "RMSEA")) table[[name]] <- vapply(table[[name]], format_decimal3, character(1))
  table$p <- vapply(table$p, format_p, character(1))
  changes <- comparison$changes
  for (name in names(changes)[vapply(changes, is.numeric, logical(1)) & names(changes) != "DeltaP"]) changes[[name]] <- vapply(changes[[name]], format_decimal3, character(1))
  changes$DeltaP <- vapply(changes$DeltaP, format_p, character(1))
  div(class = "result-section regression-result-panel structural-mi-holdout-result",
    h4(if (ko) "MI 홀드아웃 검증" else "MI holdout validation"),
    tags$p(paste0(if (ko) "탐색 행 수 = " else "Exploration rows = ", nrow(bundle$analysis_data), if (ko) "; 예약 검증 행 수 = " else "; reserved validation rows = ", comparison$validation_n_raw, if (ko) "; 사용된 검증 N = " else "; validation N used = ", paste(unique(comparison$validation_n_used), collapse = ", "), if (ko) "; 분할 seed = " else "; split seed = ", bundle$mi_holdout_seed, ".")),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(table), tags$th))),
      tags$tbody(lapply(seq_len(nrow(table)), function(index) tags$tr(lapply(as.character(table[index, ]), tags$td))))
    )),
    tags$h5(if (ko) "검증표본 변화: 수정 모형 - 기존 모형" else "Validation-sample change: modified minus original"),
    tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(lapply(names(changes), tags$th))),
      tags$tbody(tags$tr(lapply(as.character(changes[1L, ]), tags$td)))
    )),
    if (any(!comparison$table$Admissible)) tags$p(class = "structural-result-note", if (ko) "검증표본의 한 모형 또는 두 모형이 주 CFA와 동일한 전체 admissibility 점검을 통과하지 못했습니다. 검증표본 변화 통계와 공식 차이 검정은 표시하지 않으며, 이 수정은 반복검증된 것으로 해석하면 안 됩니다." else "One or both validation-sample models failed the same full admissibility checks as the main CFA. Validation-sample change statistics and the formal difference test are suppressed; the modification must not be treated as replicated."),
    tags$p(class = "structural-result-note", if (ko) "MI 경로는 탐색 표본에서만 선택되었습니다. 위 표는 예약된 검증 표본에서 두 모형을 독립적으로 다시 적합한 결과입니다. 적합도 개선의 반복은 안정성을 뒷받침하지만 실질적 근거를 대체하지 않으며, 반복되지 않으면 표본 특이적 수정일 가능성이 큽니다." else "The MI path was selected only in the exploration sample. The table above refits both models independently in the reserved validation sample. Replication of improved fit supports stability but does not replace substantive justification; failure to replicate indicates likely sample-specific modification."),
    tags$p(class = "structural-result-note", if (ko) "검증 표본은 이제 공개되어 잠겼습니다. 이 분할에서는 추가 MI 변경을 비활성화합니다. 다른 수정 모형을 평가하려면 새 분할 seed로 새 분석을 시작하십시오." else "The validation sample is now unblinded and locked. Further MI changes are disabled for this split; start a new analysis with a newly chosen split seed to evaluate a different modified model.")
  )
})
  invisible(TRUE)
}
