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
  ko <- identical(normalize_app_language(language), "ko")
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
    Status = "Heywood",
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
  tagList(
    div(class = "result-section regression-result-panel structural-heywood-result",
      h4(if (ko) "Heywood 사례 진단" else "Heywood case diagnostics"),
      if (nrow(diagnostic_table)) tags$table(class = "table table-striped table-bordered",
        tags$thead(tags$tr(lapply(names(diagnostic_table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(diagnostic_table)), function(index) tags$tr(lapply(c(
          diagnostic_table$Variable[[index]], diagnostic_table$Factor[[index]],
          format_decimal3(diagnostic_table[["Residual variance"]][[index]]),
          format_decimal3(diagnostic_table[["Standardized residual"]][[index]]),
          format_decimal3(diagnostic_table[["R²"]][[index]]),
          format_decimal3(diagnostic_table[["Observed variance"]][[index]]),
          if (is.finite(diagnostic_table[["Applied %"]][[index]])) paste0(format_decimal3(diagnostic_table[["Applied %"]][[index]]), "%") else "—",
          if (is.finite(diagnostic_table[["Fixed value"]][[index]])) format_decimal3(diagnostic_table[["Fixed value"]][[index]]) else "—",
          diagnostic_table$Status[[index]]
        ), tags$td))))
      ),
      if (nrow(latent_table)) tagList(
        tags$h5(if (ko) "음의 잠재변수 분산" else "Negative latent variances"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(latent_table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(latent_table)), function(index) tags$tr(
            tags$td(latent_table[["Latent factor"]][[index]]),
            tags$td(format_decimal3(latent_table$Variance[[index]])),
            tags$td(latent_table$Status[[index]])
          )))
        ),
        tags$p(class = "structural-result-note", if (ko) "음의 잠재변수 분산은 잠재변수 Heywood 사례입니다. 지표 오차분산 제약 재분석 버튼으로 해결되는 문제가 아니므로 요인 지정, 척도화, 고차요인 구조, 상관, 식별 제약을 검토하십시오." else "A negative latent variance is a latent-variable Heywood case. The indicator residual-variance sensitivity button does not correct it; review factor specification, scaling, higher-order structure, correlations, and identification constraints.")
      ),
      if (nrow(matrix_table)) tagList(
        tags$h5(if (ko) "공분산행렬 양정성 진단" else "Covariance-matrix definiteness diagnostics"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(tags$tr(lapply(names(matrix_table), tags$th))),
          tags$tbody(lapply(seq_len(nrow(matrix_table)), function(index) tags$tr(
            tags$td(matrix_table$Matrix[[index]]),
            tags$td(format_decimal3(matrix_table[["Minimum eigenvalue"]][[index]])),
            tags$td(if (is.finite(matrix_table[["Condition number"]][[index]])) format(matrix_table[["Condition number"]][[index]], scientific = TRUE, digits = 3) else "Inf"),
            tags$td(matrix_table$Status[[index]])
          )))
        ),
        tags$p(class = "structural-result-note", if (ko) "음의 최소 고유값은 공분산행렬이 양의 준정부호가 아님을 뜻합니다. 0에 가까운 값은 특이 경계를 의미합니다. vcov에서 이런 결과가 나오면 표준오차, 신뢰구간, z 검정, p 값이 신뢰롭지 않을 수 있고 경험적 과소식별을 시사할 수 있습니다. 조건수가 1e8을 넘으면 심각한 수치 민감성을 표시하지만, 그 자체만으로 허용 불가능한 해를 입증하지는 않습니다. 과도한 공분산 경로, 거의 공선적인 요인, 상관, 제약, 식별을 검토하십시오." else "A negative minimum eigenvalue means the covariance matrix is not positive semidefinite. A value near zero indicates a singular boundary. For vcov, these findings mean standard errors, confidence intervals, z tests, and p values may be unreliable and can indicate empirical underidentification. A condition number above 1e8 flags severe numerical sensitivity; it is a warning rather than, by itself, proof of inadmissibility. Review excessive covariance paths, near-collinear factors, correlations, constraints, and identification.")
      ),
      if (can_refit) actionButton(paste0(prefix, "_heywood_refit"), if (ko) "제약 재분석" else "Constrained reanalysis", class = "btn-warning btn-sm"),
      if (length(variables) && !can_refit) tags$p(class = "structural-result-note", if (ko) "제약 오차분산 재분석은 ML 또는 MLR로 추정한 연속형 지표에서만 사용할 수 있습니다." else "Constrained residual-variance reanalysis is available only for continuous indicators estimated with ML or MLR."),
      tags$p(class = "structural-result-note", if (ko) "제약 재분석은 민감도 분석이며 Heywood 사례의 원인을 해결하지는 않습니다." else "A constrained reanalysis is a sensitivity analysis and does not resolve the source of the Heywood case.")
    )
  )
}
