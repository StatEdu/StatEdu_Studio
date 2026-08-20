structural_canvas_show_notification <- function(message, type = "message", duration = 5, id = NULL) {
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain) || !is.function(domain$sendNotification)) return(invisible(FALSE))
  showNotification(message, type = type, duration = duration, id = id)
  invisible(TRUE)
}

structural_canvas_error_message <- function(error, language = NULL) {
  message <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error %||% "")
  ko <- identical(normalize_app_language(language), "ko")
  if (!ko) return(message)
  if (grepl("Sampling-design gate blocked estimation", message, fixed = TRUE)) {
    if (grepl("Observation independence and sampling structure must be declared", message, fixed = TRUE)) {
      return("분석을 실행하려면 관측치와 표본설계 구조를 먼저 선택해야 합니다. 분석 옵션의 ‘추정’ 탭에서 독립 횡단자료, 군집·다층자료, 복합표본 또는 종단·반복측정자료 중 해당 항목을 선택하십시오.")
    }
    if (grepl("Cluster-robust or multilevel SEM is required", message, fixed = TRUE)) {
      return("현재 캔버스 엔진은 군집·다층자료를 분석할 수 없습니다. 군집 의존성을 반영하는 다층 SEM 또는 군집 강건 추정 절차가 필요합니다.")
    }
    if (grepl("Survey weights, strata, and primary sampling units", message, fixed = TRUE)) {
      return("현재 캔버스 엔진은 복합표본 자료를 분석할 수 없습니다. 표본가중치, 층화 및 PSU를 반영하는 복합표본 SEM 절차가 필요합니다.")
    }
    if (grepl("Within-person dependence and longitudinal measurement structure", message, fixed = TRUE)) {
      return("현재 캔버스 엔진은 종단·반복측정자료를 분석할 수 없습니다. 개인 내 의존성과 종단 측정구조를 반영하는 종단 SEM 절차가 필요합니다.")
    }
    return("선택한 표본설계는 현재 캔버스 추정 엔진에서 지원되지 않아 분석을 실행하지 않았습니다.")
  }
  if (grepl("modindices", message, fixed = TRUE) || grepl("modification indices", message, ignore.case = TRUE)) {
    if (grepl("information matrix is singular", message, ignore.case = TRUE)) {
      return("수정지수(MI)를 계산할 수 없습니다. 정보행렬이 특이(singular)하여 모형이 식별 경계에 있거나 추정이 불안정합니다. MI 후보는 표시하지 않습니다.")
    }
    return("수정지수(MI)를 계산할 수 없습니다. 현재 적합 모형에서 MI 후보를 산출하지 못했습니다.")
  }
  if (grepl("information matrix is singular", message, ignore.case = TRUE)) {
    return("정보행렬이 특이(singular)하여 표준오차나 수정지수를 안정적으로 계산할 수 없습니다. 모형 식별성, 공분산, 고차요인 구조를 확인하십시오.")
  }
  if (grepl("lavaan", message, fixed = TRUE)) {
    return(paste0("lavaan 추정 오류: ", message))
  }
  message
}

structural_canvas_identification_issue_message <- function(code, message, language = NULL) {
  if (!identical(normalize_app_language(language), "ko")) return(as.character(message %||% ""))
  switch(
    as.character(code %||% ""),
    unmeasured_latent = "관측지표나 하위요인이 없는 잠재변수입니다.",
    single_indicator_auto_fixed = "단일지표 요인은 지표 오차분산을 0으로 자동 고정하여 식별했습니다. 이는 완전측정 가정이므로 해석에 주의하십시오.",
    single_indicator_constrained = "단일지표 요인은 고정 잔차분산으로 식별되었습니다. 이 제약의 외부 신뢰도 근거를 기록하십시오.",
    two_indicators = "두 지표 요인은 안정적인 식별을 위해 추가 제약이나 구조정보가 필요할 수 있습니다.",
    few_lower_order_factors = "현재 자동 식별 방식에서는 고차요인마다 하위요인이 최소 3개 필요합니다.",
    mixed_measurement_level = "이 요인은 관측지표와 하위요인을 모두 가지고 있습니다. 혼합 측정 지정이 의도한 것인지 확인하십시오.",
    multiple_higher_order_parents = "이 하위요인은 둘 이상의 고차요인에 적재되어 있습니다. 표준 고차요인 신뢰도 요약이 적용되지 않을 수 있습니다.",
    cross_loading = as.character(message %||% ""),
    invalid_fixed_residual = "고정 잔차분산은 유한한 0 이상의 값이어야 합니다.",
    negative_fixed_residual = "잔차분산은 음수로 고정할 수 없습니다.",
    boundary_fixed_residual = "0으로 고정한 잔차분산은 완전한 무잔차 측정을 뜻하는 경계 제약입니다. 강한 실질적 근거를 제시하십시오.",
    duplicate_path = "동일한 양끝점 사이에 중복된 방향 경로가 있습니다.",
    as.character(message %||% "")
  )
}

structural_canvas_identification_issue_text <- function(issues, language = NULL) {
  if (!nrow(issues)) return("")
  ko <- identical(normalize_app_language(language), "ko")
  prefix <- if (ko) "모형 식별성 점검 실패: " else "Model identification check failed: "
  paste0(
    prefix,
    paste(
      paste0(
        issues$Element,
        " — ",
        mapply(
          structural_canvas_identification_issue_message,
          issues$Code,
          issues$Message,
          MoreArgs = list(language = language),
          USE.NAMES = FALSE
        )
      ),
      collapse = "; "
    )
  )
}

structural_canvas_notify_identification_warnings <- function(identification_warnings, language = NULL) {
  if (nrow(identification_warnings)) {
    ko <- identical(normalize_app_language(language), "ko")
    structural_canvas_show_notification(
      paste0(
        if (ko) "식별성 경고: " else "Identification warning: ",
        paste(
          paste0(
            identification_warnings$Element,
            " - ",
            mapply(
              structural_canvas_identification_issue_message,
              identification_warnings$Code,
              identification_warnings$Message,
              MoreArgs = list(language = language),
              USE.NAMES = FALSE
            )
          ),
          collapse = "; "
        )
      ),
      type = "warning",
      duration = 12
    )
  }
  invisible(TRUE)
}

structural_canvas_notify_missing_covariances <- function(missing_covariances, analysis_type, language = NULL) {
  if (analysis_type %in% c("cfa", "cbsem", "sem") && length(missing_covariances)) {
    ko <- identical(normalize_app_language(language), "ko")
    message <- if (ko) {
      paste0("외생 잠재변수 사이의 공분산 경로가 없습니다: ", paste(missing_covariances, collapse = ", "), ". 해당 공분산은 0으로 고정됩니다.")
    } else {
      paste0("Missing covariance paths between exogenous latent variables: ", paste(missing_covariances, collapse = ", "), ". These covariances will be fixed to zero.")
    }
    structural_canvas_show_notification(message, type = "warning", duration = 10)
  }
  invisible(TRUE)
}

structural_canvas_notify_ignored_pls_covariances <- function(result, analysis_type, language = NULL) {
  ignored_covariances <- result$ignored_covariances %||% character(0)
  if (identical(analysis_type, "plssem") && length(ignored_covariances)) {
    ko <- identical(normalize_app_language(language), "ko")
    message <- if (ko) {
      paste0("PLS-SEM은 공분산 경로를 추정하지 않으므로 제외했습니다: ", paste(ignored_covariances, collapse = ", "), ". 외생 구성개념 간 관련성은 구조모형 추정 과정에서 간접적으로 반영됩니다.")
    } else {
      paste0("PLS-SEM does not estimate covariance paths; excluded: ", paste(ignored_covariances, collapse = ", "), ". Associations among exogenous constructs are handled indirectly during structural-model estimation.")
    }
    structural_canvas_show_notification(message, type = "warning", duration = 10)
  }
  invisible(TRUE)
}

structural_canvas_notify_solution_diagnostics <- function(result, language = NULL) {
  ko <- identical(normalize_app_language(language), "ko")
  if (!isTRUE(result$admissible)) {
    details <- if (ko) c(
      if (!isTRUE(result$converged)) "모형이 수렴하지 않음",
      if (!isTRUE(result$post_check)) "lavaan 사후 추정 점검 실패",
      if (!isTRUE(result$identified)) paste0("모형 자유도가 유효하지 않음(df = ", format_decimal3(result$df), ")"),
      if (length(result$negative_residuals)) paste0("음의 오차분산: ", paste(result$negative_residuals, collapse = ", ")),
      if (length(result$negative_latent_variances)) paste0("음의 잠재변수 분산: ", paste(result$negative_latent_variances, collapse = ", ")),
      if (isTRUE(result$non_psd_theta)) paste0("오차 공분산행렬이 양의 준정부호가 아님(최소 고유값 = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_latent_covariance)) paste0("잠재변수 공분산행렬이 양의 준정부호가 아님(최소 고유값 = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_theta)) paste0("오차 공분산행렬이 거의 특이하거나 경계에 있음(최소 고유값 = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_latent_covariance)) paste0("잠재변수 공분산행렬이 거의 특이하거나 경계에 있음(최소 고유값 = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_parameter_covariance)) paste0("모수추정 공분산행렬이 양의 준정부호가 아님(최소 고유값 = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$near_singular_parameter_covariance)) paste0("모수추정 공분산행렬이 거의 특이함(최소 고유값 = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$invalid_correlations)) "절대값 1 이상의 잠재변수 상관이 있음"
    ) else c(
      if (!isTRUE(result$converged)) "the model did not converge",
      if (!isTRUE(result$post_check)) "lavaan post-estimation checks failed",
      if (!isTRUE(result$identified)) paste0("model degrees of freedom are invalid (df = ", format_decimal3(result$df), ")"),
      if (length(result$negative_residuals)) paste0("negative residual variances: ", paste(result$negative_residuals, collapse = ", ")),
      if (length(result$negative_latent_variances)) paste0("negative latent variances: ", paste(result$negative_latent_variances, collapse = ", ")),
      if (isTRUE(result$non_psd_theta)) paste0("residual covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_latent_covariance)) paste0("latent covariance matrix is not positive semidefinite (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_theta)) paste0("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$theta_min_eigenvalue), ")"),
      if (isTRUE(result$near_singular_latent_covariance)) paste0("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = ", format_decimal3(result$latent_min_eigenvalue), ")"),
      if (isTRUE(result$non_psd_parameter_covariance)) paste0("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$near_singular_parameter_covariance)) paste0("parameter-estimate covariance matrix is near singular (minimum eigenvalue = ", format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3), ")"),
      if (isTRUE(result$invalid_correlations)) "one or more absolute latent correlations are at least 1"
    )
    message <- if (ko) {
      paste0("잠재적으로 허용 불가능한 해: ", paste(details, collapse = "; "), ". 적합도, AVE, CR, 타당도 결과를 주의해서 해석하십시오.")
    } else {
      paste0("Potentially inadmissible solution: ", paste(details, collapse = "; "), ". Interpret fit, AVE, CR, and validity results with caution.")
    }
    structural_canvas_show_notification(message, type = "error", duration = NULL)
  }

  conditioning_details <- c(
    if (isTRUE(result$ill_conditioned_theta)) paste0(if (ko) "오차 공분산행렬 조건수 = " else "residual covariance condition number = ", format(result$theta_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_latent_covariance)) paste0(if (ko) "잠재변수 공분산행렬 조건수 = " else "latent covariance condition number = ", format(result$latent_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_parameter_covariance)) paste0(if (ko) "모수추정 공분산행렬 조건수 = " else "parameter-estimate covariance condition number = ", format(result$parameter_condition_number, scientific = TRUE, digits = 3))
  )
  if (length(conditioning_details)) {
    message <- if (ko) {
      paste0("수치적으로 불안정한 해: ", paste(conditioning_details, collapse = "; "), ". 자료나 모형 지정이 조금만 바뀌어도 추정치가 불안정할 수 있습니다.")
    } else {
      paste0("Numerically ill-conditioned solution: ", paste(conditioning_details, collapse = "; "), ". Small data or specification changes may produce unstable estimates.")
    }
    structural_canvas_show_notification(message, type = "warning", duration = 12)
  }
  invisible(TRUE)
}
