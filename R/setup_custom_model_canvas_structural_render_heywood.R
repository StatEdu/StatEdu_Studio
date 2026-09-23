# Structural Heywood diagnostic result rendering.

structural_canvas_heywood_result_ui <- function(bundle, dataset, prefix, analysis_type, language = statedu_initial_language()) {
  diagnostics <- bundle$baseline_diagnostics %||% bundle$diagnostics %||% list()
  variables <- as.character(diagnostics$negative_residuals %||% character(0))
  latent_variables <- as.character(diagnostics$negative_latent_variances %||% character(0))
  theta_matrix_issue <- isTRUE(diagnostics$non_psd_theta) || isTRUE(diagnostics$near_singular_theta) || isTRUE(diagnostics$ill_conditioned_theta)
  latent_matrix_issue <- isTRUE(diagnostics$non_psd_latent_covariance) || isTRUE(diagnostics$near_singular_latent_covariance) || isTRUE(diagnostics$ill_conditioned_latent_covariance)
  parameter_matrix_issue <- isTRUE(diagnostics$non_psd_parameter_covariance) || isTRUE(diagnostics$near_singular_parameter_covariance) || isTRUE(diagnostics$ill_conditioned_parameter_covariance)
  matrix_issue <- theta_matrix_issue || latent_matrix_issue || parameter_matrix_issue
  if ((!length(variables) && !length(latent_variables) && !matrix_issue) || analysis_type == "plssem") return(NULL)
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  diagnostic_fit <- bundle$baseline_fit %||% bundle$fit
  theta <- as.matrix(lavaan::lavInspect(diagnostic_fit, "theta"))
  standardized <- lavaan::standardizedSolution(diagnostic_fit)
  r2 <- lavaan::lavInspect(diagnostic_fit, "r2")
  loadings <- standardized[standardized$op == "=~", c("lhs", "rhs", "est.std"), drop = FALSE]
  residual_rows <- standardized$op == "~~" & standardized$lhs == standardized$rhs
  standardized_residuals <- stats::setNames(standardized$est.std[residual_rows], standardized$lhs[residual_rows])
  data <- dataset
  observed_variances <- vapply(variables, function(name) stats::var(data[[name]], na.rm = TRUE), numeric(1))
  fixed_values <- as.numeric((bundle$residual_variance_fixes %||% numeric(0))[variables])
  applied_percent <- 100 * fixed_values / observed_variances
  diagnostic_table <- data.frame(
    Variable = variables,
    Factor = vapply(variables, function(name) paste(unique(loadings$lhs[loadings$rhs == name]), collapse = ", "), character(1)),
    `Residual variance` = vapply(variables, function(name) theta[name, name], numeric(1)),
    `Standardized residual` = as.numeric(standardized_residuals[variables]),
    R2 = as.numeric(r2[variables]),
    `Observed variance` = observed_variances,
    `Applied %` = applied_percent,
    `Fixed value` = fixed_values,
    Status = rep("Heywood", length(variables)),
    check.names = FALSE
  )
  names(diagnostic_table)[names(diagnostic_table) == "R2"] <- "R²"
  latent_covariance <- as.matrix(lavaan::lavInspect(diagnostic_fit, "cov.lv"))
  latent_table <- if (length(latent_variables)) data.frame(
    `Latent factor` = latent_variables,
    Variance = vapply(latent_variables, function(name) latent_covariance[name, name], numeric(1)),
    Status = "Latent Heywood", check.names = FALSE
  ) else data.frame()
  matrix_table <- data.frame(
    Matrix = c(if (theta_matrix_issue) "Residual covariance (theta)", if (latent_matrix_issue) "Latent covariance", if (parameter_matrix_issue) "Parameter-estimate covariance (vcov)"),
    `Minimum eigenvalue` = c(if (theta_matrix_issue) diagnostics$theta_min_eigenvalue, if (latent_matrix_issue) diagnostics$latent_min_eigenvalue, if (parameter_matrix_issue) diagnostics$parameter_min_eigenvalue),
    `Condition number` = c(if (theta_matrix_issue) diagnostics$theta_condition_number, if (latent_matrix_issue) diagnostics$latent_condition_number, if (parameter_matrix_issue) diagnostics$parameter_condition_number),
    Status = c(
      if (theta_matrix_issue) if (isTRUE(diagnostics$non_psd_theta)) "Not positive semidefinite" else if (isTRUE(diagnostics$near_singular_theta)) "Near singular / boundary" else "Ill-conditioned",
      if (latent_matrix_issue) if (isTRUE(diagnostics$non_psd_latent_covariance)) "Not positive semidefinite" else if (isTRUE(diagnostics$near_singular_latent_covariance)) "Near singular / boundary" else "Ill-conditioned",
      if (parameter_matrix_issue) if (isTRUE(diagnostics$non_psd_parameter_covariance)) "Unreliable standard errors" else if (isTRUE(diagnostics$near_singular_parameter_covariance)) "Empirical identification boundary" else "Ill-conditioned standard errors"
    ), check.names = FALSE
  )
  can_refit <- length(variables) > 0L && toupper(as.character(bundle$estimator %||% "ML")) %in% c("ML", "MLR") && !length(bundle$ordered %||% character(0))
  diagnostic_display <- diagnostic_table
  if (nrow(diagnostic_display)) {
    for (name in intersect(c("Residual variance", "Standardized residual", "R²", "Observed variance"), names(diagnostic_display))) {
      diagnostic_display[[name]] <- vapply(diagnostic_display[[name]], format_decimal3, character(1))
    }
    diagnostic_display[["Applied %"]] <- ifelse(is.finite(diagnostic_table[["Applied %"]]), paste0(vapply(diagnostic_table[["Applied %"]], format_decimal3, character(1)), "%"), "—")
    diagnostic_display[["Fixed value"]] <- ifelse(is.finite(diagnostic_table[["Fixed value"]]), vapply(diagnostic_table[["Fixed value"]], format_decimal3, character(1)), "—")
  }
  latent_display <- latent_table
  if (nrow(latent_display)) latent_display$Variance <- vapply(latent_display$Variance, format_decimal3, character(1))
  matrix_display <- matrix_table
  if (nrow(matrix_display)) {
    matrix_display[["Minimum eigenvalue"]] <- vapply(matrix_display[["Minimum eigenvalue"]], format_decimal3, character(1))
    matrix_display[["Condition number"]] <- ifelse(
      is.finite(matrix_table[["Condition number"]]),
      format(matrix_table[["Condition number"]], scientific = TRUE, digits = 3),
      "Inf"
    )
  }
  labels <- c("Residual variance" = "잔차분산", "Standardized residual" = "표준화 잔차", "Observed variance" = "관측분산",
    "Applied %" = "적용 비율(%)", "Fixed value" = "고정값", "Latent factor" = "잠재요인",
    "Minimum eigenvalue" = "최소 고유값", "Condition number" = "조건수",
    "Residual covariance (theta)" = "잔차 공분산(theta)", "Latent covariance" = "잠재 공분산",
    "Parameter-estimate covariance (vcov)" = "모수추정 공분산(vcov)", "Latent Heywood" = "잠재변수 Heywood",
    "Not positive semidefinite" = "양의 준정부호 아님", "Near singular / boundary" = "특이에 가까움 / 경계",
    "Ill-conditioned" = "수치적으로 불안정", "Unreliable standard errors" = "신뢰하기 어려운 표준오차",
    "Empirical identification boundary" = "경험적 식별 경계", "Ill-conditioned standard errors" = "수치적으로 불안정한 표준오차")
  label <- function(value) if (value %in% names(labels)) tr(value, unname(labels[[value]])) else structural_canvas_reporting_text(value, language)
  localize <- function(display) {
    for (column in intersect(c("Status", "Matrix"), names(display))) display[[column]] <- vapply(display[[column]], label, character(1), USE.NAMES = FALSE)
    names(display) <- vapply(names(display), label, character(1), USE.NAMES = FALSE)
    attr(display, "result_user_columns") <- names(display)
    display
  }
  diagnostic_display <- localize(diagnostic_display)
  latent_display <- localize(latent_display)
  matrix_display <- localize(matrix_display)
  tagList(
    div(class = "result-section regression-result-panel structural-heywood-result",
      h4(tr("Heywood case diagnostics", "Heywood 사례 진단")),
      if (nrow(diagnostic_display)) structural_canvas_basic_html_table(diagnostic_display, role = "appendix", orientation = "auto", language = language),
      if (nrow(latent_table)) tagList(
        tags$h5(tr("Negative latent variances", "음의 잠재변수 분산")),
        structural_canvas_basic_html_table(latent_display, role = "appendix", orientation = "portrait", language = language),
        result_note_paragraph(class = "structural-result-note", tr("A negative latent variance is a latent-variable Heywood case. The indicator residual-variance sensitivity button does not correct it; review factor specification, scaling, higher-order structure, correlations, and identification constraints.", "음의 잠재변수 분산은 잠재변수 Heywood 사례입니다. 지표 오차분산 제약 재분석 버튼으로 해결되는 문제가 아니므로 요인 지정, 척도화, 고차요인 구조, 상관, 식별 제약을 검토하십시오."))
      ),
      if (nrow(matrix_table)) tagList(
        tags$h5(tr("Covariance-matrix definiteness diagnostics", "공분산행렬 양정성 진단")),
        structural_canvas_basic_html_table(matrix_display, role = "appendix", orientation = "auto", language = language),
        result_note_paragraph(class = "structural-result-note", tr("A negative minimum eigenvalue means the covariance matrix is not positive semidefinite. A value near zero indicates a singular boundary. For vcov, these findings mean standard errors, confidence intervals, z tests, and p values may be unreliable and can indicate empirical underidentification. A condition number above 1e8 flags severe numerical sensitivity; it is a warning rather than, by itself, proof of inadmissibility. Review excessive covariance paths, near-collinear factors, correlations, constraints, and identification.", "음의 최소 고유값은 공분산행렬이 양의 준정부호가 아님을 뜻합니다. 0에 가까운 값은 특이 경계를 의미합니다. vcov에서 이런 결과가 나오면 표준오차, 신뢰구간, z 검정, p 값이 신뢰롭지 않을 수 있고 경험적 과소식별을 시사할 수 있습니다. 조건수가 1e8을 넘으면 심각한 수치 민감성을 표시하지만, 그 자체만으로 허용 불가능한 해를 입증하지는 않습니다. 과도한 공분산 경로, 거의 공선적인 요인, 상관, 제약, 식별을 검토하십시오."))
      ),
      if (can_refit) actionButton(paste0(prefix, "_heywood_refit"), tr("Constrained reanalysis", "제약 재분석"), class = "btn-warning btn-sm"),
      if (length(variables) && !can_refit) result_note_paragraph(class = "structural-result-note", tr("Constrained residual-variance reanalysis is available only for continuous indicators estimated with ML or MLR.", "제약 오차분산 재분석은 ML 또는 MLR로 추정한 연속형 지표에서만 사용할 수 있습니다.")),
      result_note_paragraph(class = "structural-result-note", tr("A constrained reanalysis is a sensitivity analysis and does not resolve the source of the Heywood case.", "제약 재분석은 민감도 분석이며 Heywood 사례의 원인을 해결하지는 않습니다."))
    )
  )
}
