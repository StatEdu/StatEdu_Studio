structural_canvas_register_local_fit_outputs <- function(output, prefix, fit_result, app_language_fn = NULL,
                                                         variable_table_fn = function() NULL,
                                                         labels_fn = function() character(0)) {
  display_name_for <- function(bundle) structural_canvas_display_name_resolver(
    snapshot = bundle$snapshot %||% list(),
    variable_table = if (is.function(variable_table_fn)) variable_table_fn() else variable_table_fn,
    labels = if (is.function(labels_fn)) labels_fn() %||% character(0) else labels_fn %||% character(0),
    moderation_definitions = bundle$diagnostics$moderation_definitions %||% bundle$moderation_definitions %||% list(),
    language = statedu_current_language(app_language_fn)
  )
output[[paste0(prefix, "_result_residuals")]] <- renderUI({
  bundle <- fit_result()
  diagnostics <- structural_canvas_display_residual_diagnostics(
    structural_canvas_residual_diagnostics(bundle$fit),
    display_name_for(bundle)
  )
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
    tagList(
      tags$h5(title),
      structural_canvas_basic_html_table(as.data.frame(values, check.names = FALSE), class = "table table-striped table-bordered structural-residual-matrix")
    )
  }
  largest <- diagnostics$largest
  if (nrow(largest)) {
    largest[["Standardized residual"]] <- vapply(largest[["Standardized residual"]], format_decimal3, character(1))
    largest[["Correlation residual"]] <- vapply(largest[["Correlation residual"]], format_decimal3, character(1))
    if ("Screening p" %in% names(largest)) largest[["Screening p"]] <- vapply(largest[["Screening p"]], format_p, character(1))
    if ("BH-adjusted screening p" %in% names(largest)) largest[["BH-adjusted screening p"]] <- vapply(largest[["BH-adjusted screening p"]], format_p, character(1))
  }
  div(class = "result-section regression-result-panel landscape-table-panel structural-residual-result",
    h4("5. Local fit diagnostics"),
    matrix_table(diagnostics$standardized, "Standardized residual matrix"),
    matrix_table(diagnostics$correlation, "Correlation residual matrix"),
    tags$h5(paste0("Large standardized residuals (descriptive |z| >= ", diagnostics$cutoff, ")")),
    if (!isTRUE(diagnostics$standardized_available)) tags$p(class = "structural-result-note", "Standardized residuals were unavailable. Correlation residuals are displayed without z cutoffs or screening p values because they are not on the same reference scale."),
    if (isTRUE(diagnostics$standardized_available) && !nrow(largest)) tags$p("No standardized residuals exceeded the descriptive cutoff."),
    if (nrow(largest)) structural_canvas_basic_html_table(largest),
    tags$p(class = "structural-result-note", "The |2.58| marker and BH-adjusted two-sided normal-reference screening p values are descriptive localization aids across unique indicator pairs. Residuals are dependent and their reference distribution can vary by estimator; neither an exceedance nor a small adjusted p value is an automatic modification instruction, and no flagged residuals does not establish local fit.")
  )
})
output[[paste0(prefix, "_result_higher_order")]] <- renderUI({
  bundle <- fit_result()
  ko <- identical(normalize_app_language(statedu_current_language(app_language_fn)), "ko")
  higher <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
  if (!isTRUE(higher$available)) return(NULL)
  table <- higher$table
  display_name <- display_name_for(bundle)
  fixed <- !is.na(table$SE) & table$SE == 0 & is.na(table$z) & is.na(table$p)
  residual_abnormal <- !is.finite(table$ResidualVariance) | table$ResidualVariance < 0 | table$ResidualVariance > 1
  residual_display <- paste0(vapply(table$ResidualVariance, format_decimal3, character(1)), ifelse(residual_abnormal, "†", ""))
  r2_interval_abnormal <- !is.finite(table$R2CILower) | !is.finite(table$R2CIUpper) | table$R2CILower < 0 | table$R2CIUpper > 1
  loading_guidance <- vapply(table$Beta, structural_canvas_higher_order_loading_guidance, character(1))
  display <- data.frame(
    `Higher-order factor` = display_name(table$HigherOrderFactor),
    `Lower-order factor` = display_name(table$LowerOrderFactor),
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
  display$SE[fixed] <- "—"
  display$z[fixed] <- "—"
  display$p[fixed] <- "—"
  if (ko) {
    display$Guidance <- vapply(display$Guidance, function(value) {
      switch(
        value,
        "Review residual/R² interval" = "잔차/R2 구간 점검",
        "Not assessed" = "평가 안 됨",
        "Weak loading review" = "약한 적재량 점검",
        "At/above descriptive .40 reference" = "기술적 .40 참고값 이상",
        value
      )
    }, character(1))
    display$SE[fixed] <- "—"
    names(display) <- c(
      "고차요인", "저차요인", "B", "B 95% CI 하한", "B 95% CI 상한", "SE", "Beta",
      "Beta 95% CI 하한", "Beta 95% CI 상한", "R2", "R2 95% CI 하한", "R2 95% CI 상한",
      "잔차분산", "해석", "z", "p"
    )
  }
  omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
  omega_h_guidance <- if (isTRUE(omega_h$available)) structural_canvas_omega_h_guidance(omega_h$omega_h) else ""
  if (ko) {
    omega_h_guidance <- switch(
      omega_h_guidance,
      "Review inadmissible coefficient" = "허용 범위 밖 계수 점검",
      "Below common .70 guideline" = "일반적 .70 기준 미만",
      "At/above descriptive .70 reference" = "기술적 .70 참고값 이상",
      omega_h_guidance
    )
  }
  omega_h_reason <- omega_h$reason
  if (ko) {
    omega_h_reason <- switch(
      omega_h_reason,
      "No higher-order loading paths are specified." = "고차요인 적재 경로가 지정되지 않았습니다.",
      "Omega-h requires exactly one higher-order general factor." = "omega-h는 정확히 하나의 고차 일반요인이 필요합니다.",
      "Omega-h is not reported when a lower-order factor has multiple higher-order loadings." = "하나의 저차요인이 여러 고차요인에 적재되면 omega-h를 보고하지 않습니다.",
      "No observed indicators were found under the lower-order factors." = "저차요인 아래에서 관측 지표를 찾지 못했습니다.",
      "Omega-h is not reported with cross-loaded observed indicators." = "교차적재 관측 지표가 있으면 omega-h를 보고하지 않습니다.",
      "The model-implied indicator covariance matrix is unavailable." = "모형-함의 지표 공분산행렬을 사용할 수 없습니다.",
      "Omega-h denominator is not positive and finite." = "omega-h 분모가 양의 유한값이 아닙니다.",
      omega_h_reason
    )
  }
  div(class = "result-section regression-result-panel structural-higher-order-result",
    h4(if (ko) "고차요인 CFA 결과" else "Higher-order CFA results"),
    structural_canvas_basic_html_table(display),
    if (isTRUE(omega_h$available)) tags$div(class = "table-responsive", tags$table(class = "table table-striped table-bordered",
      tags$thead(tags$tr(tags$th(if (ko) "고차요인" else "Higher-order factor"), tags$th(if (ko) "지표 수" else "Indicators"), tags$th(if (ko) "위계적 omega (omega-h)" else "Hierarchical omega (ωh)"), tags$th(if (ko) "해석" else "Guidance"))),
      tags$tbody(tags$tr(
        tags$td(display_name(omega_h$higher_order_factor)), tags$td(omega_h$indicators),
        tags$td(paste0(format_decimal3(omega_h$omega_h), if (!is.finite(omega_h$omega_h) || omega_h$omega_h < 0 || omega_h$omega_h > 1) "†" else "")),
        tags$td(omega_h_guidance)
      ))
    )) else tags$p(class = "structural-result-note", paste0(if (ko) "위계적 omega를 보고하지 않았습니다: " else "Hierarchical omega was not reported: ", omega_h_reason)),
    tags$p(class = "structural-result-note", if (ko) "저차요인 R2는 고차요인이 설명하는 분산입니다. 잔차분산은 표준화된 잠재변수 척도로 보고됩니다." else "Lower-order R² is the variance explained by the higher-order factor. Residual variance is reported on the standardized latent-variable scale."),
    tags$p(class = "structural-result-note", if (ko) "저차요인 R2 구간은 표준화 잔차분산 구간의 보수(complement)입니다. † 표시는 R2 구간이 [0, 1] 범위를 벗어난 경우도 나타냅니다." else "Lower-order R² intervals complement the standardized residual-variance intervals. † also marks an R² interval extending beyond [0, 1]."),
    tags$p(class = "structural-result-note", if (ko) "고차요인 표준화 적재량 신뢰구간은 lavaan의 95% delta-method 구간입니다." else "Higher-order standardized-loading confidence intervals are 95% delta-method intervals from lavaan."),
    tags$p(class = "structural-result-note", if (ko) "B 신뢰구간은 비표준화 고차요인 적재량의 95% 구간입니다. 고정된 기준 적재량은 고정값에서 퇴화된 구간을 갖습니다." else "B confidence intervals are 95% intervals for unstandardized higher-order loadings; a fixed reference loading has a degenerate interval at its fixed value."),
    tags$p(class = "structural-result-note", if (ko) "omega-h는 적합된 고차요인 CFA 모형에서 단위가중 총점 분산 중 하나의 고차 일반요인에 귀속되는 비율을 추정합니다." else "ωh estimates the proportion of unit-weighted total-score variance attributable to one higher-order general factor under the fitted higher-order CFA model."),
    tags$p(class = "structural-result-note", if (ko) "고차요인 모형에서는 일반요인이 문항에 직접 적재하지 않고 저차요인을 통해 간접적으로 영향을 줍니다. 이는 일반요인과 집단요인이 문항에 직접 적재하는 bifactor 모형과 동일하지 않으며, 고차요인 적합만으로 단일차원성·일반점수 우월성 또는 bifactor 구조를 입증하지 않습니다." else "In a higher-order model, the general factor affects indicators indirectly through lower-order factors. This is not equivalent to a bifactor model, where general and specific factors load directly on indicators; fitting a higher-order factor alone does not establish unidimensionality, superiority of a general score, or a bifactor structure."),
    tags$p(class = "structural-result-note", if (ko) "ωh는 지정된 단위가중 총점과 적합된 고차요인 모형에 조건부인 기술량입니다. 척도 점수 사용을 정당화하려면 내용타당도, 대안모형, 요인점수 결정성 및 독립표본 재현성을 함께 검토하십시오." else "ωh is conditional on the specified unit-weighted total score and fitted higher-order model. Justifying score use also requires content validity, plausible alternative models, factor-score determinacy, and independent replication."),
    tags$p(class = "structural-result-note", if (ko) ".40 적재량과 .70 omega-h 값은 기술적 검토 기준이며, 보편적 통과/탈락 규칙이 아닙니다. † 표시는 사용할 수 없는 값 또는 [0, 1] 범위를 벗어난 계수/잔차분산을 나타냅니다." else "The .40 loading and .70 ωh values are descriptive review guidelines, not universal pass/fail rules. † marks an unavailable value or a coefficient/residual variance outside [0, 1]."),
    tags$p(class = "structural-result-note", if (ko) "고정된 기준 적재량은 SE, z, p를 추정하지 않습니다." else "Fixed reference loadings have no estimated SE, z, or p value.")
  )
})
  invisible(TRUE)
}
