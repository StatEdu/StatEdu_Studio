structural_canvas_show_notification <- function(message, type = "message", duration = 5, id = NULL) {
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain) || !is.function(domain$sendNotification)) return(invisible(FALSE))
  showNotification(message, type = type, duration = duration, id = id)
  invisible(TRUE)
}

statedu_bootstrap_stop_button <- function(input_id, label) {
  shiny::actionButton(
    input_id,
    label,
    class = "btn btn-sm btn-danger statedu-bootstrap-stop-button",
    onclick = sprintf(
      "if (window.Shiny) Shiny.setInputValue('%s', Date.now(), {priority: 'event'});",
      input_id
    )
  )
}

statedu_analysis_status_ui <- function(
  title,
  detail,
  percent = NA_real_,
  stop_input_id = NULL,
  stop_label = NULL,
  phase_label = NULL
) {
  percent <- suppressWarnings(as.numeric(percent %||% NA_real_))
  determinate <- is.finite(percent)
  if (determinate) percent <- max(0, min(100, round(percent)))
  bar_class <- paste(
    "progress-bar progress-bar-striped",
    if (!determinate) "active" else ""
  )
  stop_input_id <- as.character(stop_input_id %||% "")
  stop_label <- as.character(stop_label %||% "")
  shiny::tagList(
    shiny::tags$strong(class = "statedu-bootstrap-status-title", title),
    shiny::tags$div(class = "statedu-bootstrap-status-detail", detail),
    shiny::tags$div(
      class = "progress structural-bootstrap-progress statedu-bootstrap-progress",
      shiny::tags$div(
        class = bar_class,
        role = "progressbar",
        `aria-valuemin` = "0",
        `aria-valuemax` = "100",
        `aria-valuenow` = if (determinate) as.character(percent) else NULL,
        style = paste0("width: ", if (determinate) paste0(percent, "%") else "100%", ";"),
        if (determinate) paste0(percent, "%") else as.character(phase_label %||% "")
      )
    ),
    if (nzchar(stop_input_id) && nzchar(stop_label)) {
      statedu_bootstrap_stop_button(stop_input_id, stop_label)
    }
  )
}

# Backward-compatible name used by the asynchronous bootstrap jobs.  Other
# analysis phases use the same card so progress presentation stays consistent.
statedu_bootstrap_status_ui <- function(...) statedu_analysis_status_ui(...)

structural_canvas_error_message <- function(error, language = NULL) {
  message <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error %||% "")
  ko <- identical(normalize_app_language(language), "ko")
  if (!normalize_app_language(language) %in% c("en", "ko") && grepl("Sampling-design gate blocked estimation", message, fixed = TRUE)) {
    source <- if (grepl("Observation independence and sampling structure must be declared", message, fixed = TRUE)) {
      "Declare observation independence and sampling structure in the Estimation options before analysis: independent cross-sectional, clustered/multilevel, complex survey, or longitudinal/repeated measures."
    } else if (grepl("Cluster-robust or multilevel SEM is required", message, fixed = TRUE)) {
      "This canvas engine cannot analyze clustered or multilevel data. Use multilevel SEM or cluster-robust estimation that accounts for cluster dependence."
    } else if (grepl("Survey weights, strata, and primary sampling units", message, fixed = TRUE)) {
      "This canvas engine cannot analyze complex survey data. Use survey SEM that accounts for sampling weights, strata, and primary sampling units."
    } else if (grepl("Within-person dependence and longitudinal measurement structure", message, fixed = TRUE)) {
      "This canvas engine cannot analyze longitudinal or repeated-measures data. Use longitudinal SEM that accounts for within-person dependence and longitudinal measurement structure."
    } else {
      "Analysis was not run because the selected sampling design is unsupported by the current canvas estimation engine."
    }
    return(statedu_localized_text(language, source))
  }
  if (!normalize_app_language(language) %in% c("en", "ko")) {
    tr <- function(text) statedu_localized_text(language, text)
    if (grepl("modindices", message, fixed = TRUE) || grepl("modification indices", message, ignore.case = TRUE)) {
      return(tr(if (grepl("information matrix is singular", message, ignore.case = TRUE))
        "Modification indices cannot be computed because the information matrix is singular. Identification may be borderline or estimation unstable; MI candidates are not displayed."
        else "Modification indices cannot be computed for the current fitted model; no MI candidates are available."))
    }
    if (grepl("information matrix is singular", message, ignore.case = TRUE)) return(tr("The information matrix is singular, so standard errors or modification indices cannot be computed reliably. Check model identification, covariances, and higher-order factor structure."))
    if (grepl("computationally singular", message, ignore.case = TRUE)) {
      marker <- regexpr("reciprocal condition number\\s*=\\s*", message, ignore.case = TRUE, perl = TRUE)
      detail <- if (marker[[1L]] > 0L) trimws(substring(message, marker[[1L]] + attr(marker, "match.length"))) else ""
      return(paste0(tr("The matrix is nearly singular and cannot be inverted reliably. Check zero within-group variances, nearly duplicate variables, extreme multicollinearity, model identification, and excessive equality constraints."),
        if (nzchar(detail)) paste0(" ", sprintf(tr("Reciprocal condition number: %s"), detail)) else ""))
    }
    if (grepl("Multi-group structural-path scope must be either|PLS-MGA path scope must be either", message)) return(tr("Choose either all structural paths or selected structural paths as the comparison scope."))
    if (grepl("Select at least one structural path|selected-path scope requires at least one selected structural path|Selected-path inference requires at least one resolved latent regression path", message, ignore.case = TRUE)) return(tr("Select at least one structural path to compare in selected-path mode."))
    if (grepl("has no latent-to-latent structural path", message, fixed = TRUE)) return(tr("The current model has no latent-to-latent structural path available for multi-group comparison."))
    if (grepl("selected structural paths are missing or no longer valid", message, fixed = TRUE)) {
      marker <- "current canvas: "
      position <- regexpr(marker, message, fixed = TRUE)[[1L]]
      details <- if (position > 0L) substring(message, position + nchar(marker)) else ""
      details <- sub("\\. Re-select the paths before analysis\\.$", "", details)
      return(paste0(tr("Selected structural paths are missing or no longer valid in the current canvas. Select the paths again."), if (nzchar(details)) paste0(" ", tr("Details"), ": ", details) else ""))
    }
    if (grepl("duplicate latent regression paths|duplicate structural paths|requires unique, non-empty edge IDs|requires unique structural edge IDs", message)) return(tr("Duplicate structural paths or path IDs prevent resolving the selected paths. Remove duplicates and select the paths again."))
    if (grepl("Selected-path equality constraints did not produce the expected model degrees-of-freedom change", message, fixed = TRUE)) return(tr("The group-difference test was stopped because selected-path equality constraints changed model degrees of freedom by an unexpected amount. Check the selected paths and model constraints."))
    if (grepl("PLS model contract blocked estimation:", message, fixed = TRUE)) {
      rules <- list(
        list("observed covariates/control variables and covariateTargets", "Covariates: ", "PLS/PLSc does not support observed covariates or their targets. Estimation was stopped to avoid omitting control effects."),
        list("fixed/free constraints, fixed values, start values, parameter names, and equality labels", "Modified elements: ", "PLS/PLSc does not support fixed/free constraints, fixed or start values, parameter names, or equality labels. Estimation was stopped to avoid ignoring these settings."),
        list("directed structural paths must be acyclic", "", "PLS/PLSc structural paths must be acyclic. Remove reciprocal paths or feedback loops before estimation."),
        list("each indicator may belong to only one construct", "Duplicate indicator ownership: ", "Each PLS/PLSc indicator must belong to one construct only. Remove duplicate indicator assignments or measurement paths.")
      )
      for (rule in rules) if (grepl(rule[[1L]], message, fixed = TRUE)) {
        marker <- rule[[2L]]
        position <- if (nzchar(marker)) regexpr(marker, message, fixed = TRUE)[[1L]] else -1L
        details <- if (position > 0L) substring(message, position + nchar(marker)) else ""
        return(paste0(tr(rule[[3L]]), if (nzchar(details)) paste0(" ", tr("Details"), ": ", details) else ""))
      }
      return(tr("PLS/PLSc estimation was stopped because the model contains unsupported settings."))
    }
    if (grepl("PLS model indicators missing from the current data: ", message, fixed = TRUE)) {
      missing <- sub("^.*PLS model indicators missing from the current data: ", "", message)
      missing <- sub("\\. Reassign the highlighted measurement variables to columns in the current data\\.$", "", missing)
      return(sprintf(tr("PLS indicators are missing from the current data: %s. Reassign the highlighted indicators to columns in the current data."), missing))
    }
    if (grepl("undefined columns selected", message, fixed = TRUE)) return(tr("PLS indicators do not match the current data columns. Reassign the highlighted indicators to columns in the current data."))
  }
  if (!identical(normalize_app_language(language), "en")) {
    selected_path_errors <- c(
      "The selected direct-path family could not be resolved exactly for MICOM/PLS-MGA.",
      "One or more selected paths were not available in the fitted multi-group model.",
      "Selected-path structural comparison requires a non-empty resolved path registry.",
      "PLS-MGA cannot resolve selected paths because the canvas contains duplicate node IDs.",
      "PLS-MGA cannot resolve selected paths because the canvas structural-edge registry is invalid.",
      "PLS-MGA selected-path inference requires a non-empty canonical direct-effect registry.",
      "PLS-MGA direct effects do not satisfy the canonical selected-path registry contract.",
      "PLS-MGA selected-path input must provide Predictor and Outcome together.",
      "PLS-MGA selected-path input requires Predictor/Outcome, Edge ID, or Path Key.",
      "PLS-MGA selected paths are not represented exactly once in every required group contrast.",
      "A selected PLS-MGA Edge ID is missing or duplicated in the current canvas.",
      "Every selected PLS-MGA row must identify one structural path.",
      "A selected PLS-MGA path is missing, stale, inconsistent, or ambiguous in the fitted direct-effect registry."
    )
    if (message %in% selected_path_errors) return(statedu_localized_text(language,
      "The selected structural paths do not match the current model or fitted multi-group paths exactly. Check the model and select the comparison paths again.",
      "선택한 구조경로가 현재 모형 또는 추정된 다집단 경로와 정확히 일치하지 않습니다. 모형을 확인하고 비교할 경로를 다시 선택하십시오."))
    templates <- list(
      c("PLS-MGA selected-path input has ambiguous columns: ", ".",
        "PLS-MGA selected-path input has ambiguous columns: %s.", "PLS-MGA 선택 경로 입력에 모호한 열이 있습니다: %s."),
      c("One or more selected canvas paths were not estimable structural regressions in the multi-group SEM: ", ".",
        "Selected canvas paths could not be estimated as structural regressions in the multi-group SEM: %s.", "다집단 SEM에서 선택한 캔버스 경로를 구조회귀로 추정할 수 없습니다: %s."),
      c("Selected path '", "' was not represented by exactly one free regression coefficient in every group.",
        "Selected path '%s' must have exactly one freely estimated regression coefficient in each group.", "선택 경로 '%s'에는 각 집단에서 자유롭게 추정되는 회귀계수가 정확히 하나씩 있어야 합니다.")
    )
    for (template in templates) {
      if (startsWith(message, template[[1L]]) && endsWith(message, template[[2L]])) {
        detail <- substr(message, nchar(template[[1L]]) + 1L, nchar(message) - nchar(template[[2L]]))
        return(sprintf(statedu_localized_text(language, template[[3L]], template[[4L]]), detail))
      }
    }
  }
  if (!ko) return(message)
  if (grepl("PLS model contract blocked estimation:", message, fixed = TRUE)) {
    if (grepl("observed covariates/control variables and covariateTargets", message, fixed = TRUE)) {
      details <- sub("^.* Covariates: ", "", message)
      details <- sub("\\.$", "", details)
      suffix <- if (!identical(details, message) && nzchar(details)) paste0(" 지정된 통제변수: ", details, ".") else ""
      return(paste0(
        "현재 PLS/PLSc 엔진은 관측 통제변수와 통제변수 대상(covariateTargets)을 추정하지 않습니다. 통제효과가 조용히 제외되는 것을 막기 위해 분석을 실행하지 않았습니다.",
        suffix
      ))
    }
    if (grepl("fixed/free constraints, fixed values, start values, parameter names, and equality labels", message, fixed = TRUE)) {
      details <- sub("^.* Modified elements: ", "", message)
      details <- sub("\\.$", "", details)
      return(paste0(
        "현재 PLS/PLSc 엔진은 고정·자유 모수 제약, 고정값, 시작값, 모수명 및 동일성 라벨을 지원하지 않습니다. 해당 지정이 조용히 무시되는 것을 막기 위해 분석을 실행하지 않았습니다. 수정된 요소: ",
        details, "."
      ))
    }
    if (grepl("directed structural paths must be acyclic", message, fixed = TRUE)) {
      return("PLS/PLSc 구조경로는 순환이 없는 방향모형이어야 합니다. 상호회귀경로 또는 피드백 순환이 있어 분석을 실행하지 않았습니다.")
    }
    if (grepl("each indicator may belong to only one construct", message, fixed = TRUE)) {
      details <- sub("^.* Duplicate indicator ownership: ", "", message)
      details <- sub("\\.$", "", details)
      return(paste0(
        "PLS/PLSc에서 각 측정지표는 하나의 구성개념에만 소속되어야 하고 동일한 측정경로를 중복 배치할 수 없습니다. 중복 지표 소속: ",
        details, "."
      ))
    }
    return("현재 PLS/PLSc 엔진이 지원하지 않는 모형 지정이 있어, 해당 지정이 조용히 무시되는 것을 막기 위해 분석을 실행하지 않았습니다.")
  }
  if (grepl("PLS model indicators missing from the current data:", message, fixed = TRUE)) {
    missing <- sub("^.*PLS model indicators missing from the current data: ", "", message)
    missing <- sub("\\. Reassign the highlighted measurement variables to columns in the current data\\.$", "", missing)
    return(paste0(
      "현재 데이터에 없는 PLS 측정변수가 있습니다: ", missing,
      ". 빨간색으로 표시된 측정변수를 현재 데이터의 열로 다시 지정하십시오."
    ))
  }
  if (grepl("undefined columns selected", message, fixed = TRUE)) {
    return("PLS 모형의 측정변수와 현재 데이터 열이 일치하지 않습니다. 빨간색으로 표시된 측정변수를 현재 데이터의 열로 다시 지정하십시오.")
  }
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
  if (
    grepl("Multi-group structural-path scope must be either", message, fixed = TRUE) ||
      grepl("PLS-MGA path scope must be either", message, fixed = TRUE)
  ) {
    return("구조경로 비교 범위 설정이 올바르지 않습니다. ‘모든 구조경로’ 또는 ‘선택한 구조경로’를 선택하십시오.")
  }
  if (
    grepl("Select at least one structural path", message, fixed = TRUE) ||
      grepl("selected-path scope requires at least one selected structural path", message, ignore.case = TRUE) ||
      grepl("Selected-path inference requires at least one resolved latent regression path", message, fixed = TRUE)
  ) {
    return("‘선택한 구조경로’ 모드에서는 비교할 구조경로를 하나 이상 선택해야 합니다.")
  }
  if (grepl("has no latent-to-latent structural path", message, fixed = TRUE)) {
    return("현재 모형에 다집단 비교 대상으로 선택할 수 있는 잠재변수 간 구조경로가 없습니다.")
  }
  if (grepl("selected structural paths are missing or no longer valid", message, fixed = TRUE)) {
    details <- sub("^.*current canvas: ", "", message)
    details <- sub("\\. Re-select the paths before analysis\\.$", "", details)
    suffix <- if (!identical(details, message) && nzchar(details)) paste0(" 해당 경로 ID: ", details, ".") else ""
    return(paste0("선택한 구조경로가 현재 캔버스에 없거나 더 이상 유효하지 않습니다. 경로를 다시 선택하십시오.", suffix))
  }
  if (
    grepl("duplicate latent regression paths", message, fixed = TRUE) ||
      grepl("duplicate structural paths", message, fixed = TRUE) ||
      grepl("requires unique, non-empty edge IDs", message, fixed = TRUE) ||
      grepl("requires unique structural edge IDs", message, fixed = TRUE)
  ) {
    return("같은 구조경로 또는 경로 ID가 중복되어 선택 경로를 확정할 수 없습니다. 모형의 중복 경로를 정리한 뒤 다시 선택하십시오.")
  }
  if (grepl("Selected-path equality constraints did not produce the expected model degrees-of-freedom change", message, fixed = TRUE)) {
    return("선택 경로 동일화 제약으로 증가한 모형 자유도가 예상값과 일치하지 않아 집단차이 검정을 중단했습니다. 선택 경로와 모형 제약을 확인하십시오.")
  }
  if (
    grepl("selected path", message, ignore.case = TRUE) ||
      grepl("selected-path", message, ignore.case = TRUE) ||
      grepl("selected direct-path", message, ignore.case = TRUE)
  ) {
    return("선택한 구조경로가 현재 모형 또는 추정된 다집단 경로와 정확히 일치하지 않습니다. 모형을 확인하고 비교할 경로를 다시 선택하십시오.")
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
  if (grepl("computationally singular", message, ignore.case = TRUE)) {
    reciprocal <- sub(
      "^.*reciprocal condition number\\s*=\\s*",
      "",
      gsub("[\r\n]+", " ", message),
      ignore.case = TRUE
    )
    reciprocal <- trimws(reciprocal)
    condition_note <- if (nzchar(reciprocal) && !identical(reciprocal, message)) {
      paste0(" 역조건수는 ", reciprocal, "입니다.")
    } else {
      ""
    }
    return(paste0(
      "계산에 사용된 행렬이 거의 특이행렬이어서 안정적인 역행렬을 구할 수 없습니다.",
      condition_note,
      " 집단별 분산이 0인 변수, 거의 중복된 변수, 극단적인 다중공선성, 모형 식별 또는 과도한 동일화 제약을 확인하십시오."
    ))
  }
  if (grepl("lavaan", message, fixed = TRUE)) {
    return(paste0("lavaan 추정 오류: ", message))
  }
  message
}

structural_canvas_identification_issue_message <- function(code, message, language = NULL) {
  if (identical(as.character(code), "cross_loading") && !identical(normalize_app_language(language), "en")) {
    text <- as.character(message %||% "")
    prefix <- "The indicator loads on multiple factors: "
    suffix <- ". Review simple-structure assumptions and reliability/validity summaries."
    if (length(text) == 1L && startsWith(text, prefix) && endsWith(text, suffix)) {
      factors <- substr(text, nchar(prefix) + 1L, nchar(text) - nchar(suffix))
      return(sprintf(statedu_localized_text(language,
        "The indicator loads on multiple factors: %s. Review simple-structure assumptions and reliability/validity summaries.",
        "이 지표는 여러 요인에 적재됩니다: %s. 단순구조 가정과 신뢰도·타당도 요약을 검토하십시오."), factors))
    }
  }
  if (!normalize_app_language(language) %in% c("en", "ko")) {
    known <- c("unmeasured_latent", "single_indicator_auto_fixed", "single_indicator_constrained", "two_indicators",
      "few_lower_order_factors", "mixed_measurement_level", "multiple_higher_order_parents", "invalid_fixed_residual",
      "negative_fixed_residual", "boundary_fixed_residual", "duplicate_path", "duplicate_covariance", "structural_cycle")
    if (as.character(code %||% "") %in% known) return(statedu_localized_text(language, as.character(message %||% "")))
    return(as.character(message %||% ""))
  }
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
    duplicate_covariance = "동일한 양끝점 사이에 중복된 공분산 경로가 있습니다.",
    structural_cycle = "현재 자동 식별 방식에서는 상호회귀 또는 순환 구조경로를 지원하지 않습니다.",
    as.character(message %||% "")
  )
}

structural_canvas_identification_issue_text <- function(issues, language = NULL) {
  if (!nrow(issues)) return("")
  ko <- identical(normalize_app_language(language), "ko")
  prefix <- paste0(statedu_localized_text(language, "Model identification check failed", "모형 식별성 점검 실패"), ": ")
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
        paste0(statedu_localized_text(language, "Identification warning", "식별성 경고"), ": "),
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
    message <- sprintf(statedu_localized_text(language,
      "Missing covariance paths between exogenous latent variables: %s. These covariances will be fixed to zero.",
      "외생 잠재변수 사이의 공분산 경로가 없습니다: %s. 해당 공분산은 0으로 고정됩니다."), paste(missing_covariances, collapse = ", "))
    structural_canvas_show_notification(message, type = "warning", duration = 10)
  }
  invisible(TRUE)
}

structural_canvas_notify_ignored_pls_covariances <- function(result, analysis_type, language = NULL) {
  ignored_covariances <- result$ignored_covariances %||% character(0)
  if (identical(analysis_type, "plssem") && length(ignored_covariances)) {
    ko <- identical(normalize_app_language(language), "ko")
    message <- sprintf(statedu_localized_text(language,
      "PLS-SEM does not estimate covariance paths; excluded: %s. Associations among exogenous constructs are handled indirectly during structural-model estimation.",
      "PLS-SEM은 공분산 경로를 추정하지 않으므로 제외했습니다: %s. 외생 구성개념 간 관련성은 구조모형 추정 과정에서 간접적으로 반영됩니다."), paste(ignored_covariances, collapse = ", "))
    structural_canvas_show_notification(message, type = "warning", duration = 10)
  }
  invisible(TRUE)
}

structural_canvas_notify_solution_diagnostics <- function(result, language = NULL) {
  tr <- function(en, ko) statedu_localized_text(language, en, ko)
  if (!isTRUE(result$admissible)) {
    details <- c(
      if (!isTRUE(result$converged)) tr("the model did not converge", "모형이 수렴하지 않음"),
      if (!isTRUE(result$post_check)) tr("lavaan post-estimation checks failed", "lavaan 사후 추정 점검 실패"),
      if (!isTRUE(result$identified)) sprintf(tr("model degrees of freedom are invalid (df = %s)", "모형 자유도가 유효하지 않음(df = %s)"), format_decimal3(result$df)),
      if (length(result$negative_residuals)) sprintf(tr("negative residual variances: %s", "음의 오차분산: %s"), paste(result$negative_residuals, collapse = ", ")),
      if (length(result$negative_latent_variances)) sprintf(tr("negative latent variances: %s", "음의 잠재변수 분산: %s"), paste(result$negative_latent_variances, collapse = ", ")),
      if (isTRUE(result$non_psd_theta)) sprintf(tr("residual covariance matrix is not positive semidefinite (minimum eigenvalue = %s)", "오차 공분산행렬이 양의 준정부호가 아님(최소 고유값 = %s)"), format_decimal3(result$theta_min_eigenvalue)),
      if (isTRUE(result$non_psd_latent_covariance)) sprintf(tr("latent covariance matrix is not positive semidefinite (minimum eigenvalue = %s)", "잠재변수 공분산행렬이 양의 준정부호가 아님(최소 고유값 = %s)"), format_decimal3(result$latent_min_eigenvalue)),
      if (isTRUE(result$near_singular_theta)) sprintf(tr("residual covariance matrix is near singular or on the boundary (minimum eigenvalue = %s)", "오차 공분산행렬이 거의 특이하거나 경계에 있음(최소 고유값 = %s)"), format_decimal3(result$theta_min_eigenvalue)),
      if (isTRUE(result$near_singular_latent_covariance)) sprintf(tr("latent covariance matrix is near singular or on the boundary (minimum eigenvalue = %s)", "잠재변수 공분산행렬이 거의 특이하거나 경계에 있음(최소 고유값 = %s)"), format_decimal3(result$latent_min_eigenvalue)),
      if (isTRUE(result$non_psd_parameter_covariance)) sprintf(tr("parameter-estimate covariance matrix is not positive semidefinite (minimum eigenvalue = %s)", "모수추정 공분산행렬이 양의 준정부호가 아님(최소 고유값 = %s)"), format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3)),
      if (isTRUE(result$near_singular_parameter_covariance)) sprintf(tr("parameter-estimate covariance matrix is near singular (minimum eigenvalue = %s)", "모수추정 공분산행렬이 거의 특이함(최소 고유값 = %s)"), format(result$parameter_min_eigenvalue, scientific = TRUE, digits = 3)),
      if (isTRUE(result$invalid_correlations)) tr("one or more absolute latent correlations are at least 1", "절대값 1 이상의 잠재변수 상관이 있음")
    )
    message <- sprintf(tr(
      "Potentially inadmissible solution: %s. Interpret fit, AVE, CR, and validity results with caution.",
      "잠재적으로 허용 불가능한 해: %s. 적합도, AVE, CR, 타당도 결과를 주의해서 해석하십시오."), paste(details, collapse = "; "))
    structural_canvas_show_notification(message, type = "error", duration = NULL)
  }

  conditioning_details <- c(
    if (isTRUE(result$ill_conditioned_theta)) sprintf(tr("residual covariance condition number = %s", "오차 공분산행렬 조건수 = %s"), format(result$theta_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_latent_covariance)) sprintf(tr("latent covariance condition number = %s", "잠재변수 공분산행렬 조건수 = %s"), format(result$latent_condition_number, scientific = TRUE, digits = 3)),
    if (isTRUE(result$ill_conditioned_parameter_covariance)) sprintf(tr("parameter-estimate covariance condition number = %s", "모수추정 공분산행렬 조건수 = %s"), format(result$parameter_condition_number, scientific = TRUE, digits = 3))
  )
  if (length(conditioning_details)) {
    message <- sprintf(tr(
      "Numerically ill-conditioned solution: %s. Small data or specification changes may produce unstable estimates.",
      "수치적으로 불안정한 해: %s. 자료나 모형 지정이 조금만 바뀌어도 추정치가 불안정할 수 있습니다."), paste(conditioning_details, collapse = "; "))
    structural_canvas_show_notification(message, type = "warning", duration = 12)
  }
  invisible(TRUE)
}
