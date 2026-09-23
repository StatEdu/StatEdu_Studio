# Structural equation canvas measurement-invariance execution helpers.

structural_canvas_invariance_text <- function(language, en, ko) {
  normalized <- if (exists("normalize_app_language", mode = "function")) {
    normalize_app_language(language)
  } else {
    tolower(trimws(as.character(language %||% "en")))
  }
  if (identical(normalized, "ko")) ko else en
}

structural_canvas_measurement_only_syntax <- function(syntax, language = NULL) {
  raw_syntax <- as.character(syntax %||% "")
  raw_lines <- trimws(strsplit(raw_syntax, "\n", fixed = TRUE)[[1L]])
  if (!any(grepl("=~", raw_lines, fixed = TRUE))) stop(structural_canvas_invariance_text(
    language,
    "A measurement model is required before structural path group comparison.",
    "집단 간 구조경로를 비교하려면 먼저 측정모형이 필요합니다."
  ))
  context <- "Multi-group measurement-invariance analysis"
  constraint_audit <- structural_canvas_multigroup_constraint_audit(
    raw_syntax,
    context = context
  )
  sanitized_syntax <- structural_canvas_sanitize_multigroup_syntax(
    raw_syntax,
    constraint_audit = constraint_audit,
    context = context
  )
  lines <- trimws(strsplit(sanitized_syntax, "\n", fixed = TRUE)[[1L]])
  measurement <- lines[grepl("=~", lines, fixed = TRUE)]
  if (!length(measurement)) stop(structural_canvas_invariance_text(
    language,
    "A measurement model is required before structural path group comparison.",
    "집단 간 구조경로를 비교하려면 먼저 측정모형이 필요합니다."
  ))
  parsed <- lavaan::lavaanify(sanitized_syntax, auto = FALSE)
  explicit <- parsed[parsed$user == 1L, , drop = FALSE]
  measurement_parameters <- explicit[explicit$op == "=~", , drop = FALSE]
  factors <- unique(as.character(measurement_parameters$lhs))
  structural_endogenous_factors <- intersect(
    unique(as.character(explicit$lhs[explicit$op == "~"])),
    factors
  )
  lower_order_factors <- intersect(
    unique(as.character(measurement_parameters$rhs)),
    factors
  )
  # Only factors that are not themselves indicators of a higher-order factor
  # are exogenous in the measurement-only representation. Covarying every
  # factor would add residual covariances between a higher-order factor and
  # its lower-order indicators and can make an otherwise valid model
  # unidentified or inadmissible.
  top_level_factors <- setdiff(factors, lower_order_factors)
  measurement_variables <- unique(c(factors, as.character(measurement_parameters$rhs)))

  explicit_covariance <- explicit[
    explicit$op == "~~" &
      explicit$lhs %in% measurement_variables &
      explicit$rhs %in% measurement_variables &
      !explicit$lhs %in% structural_endogenous_factors &
      !explicit$rhs %in% structural_endogenous_factors,
    , drop = FALSE
  ]
  covariance_key <- function(lhs, rhs) {
    vapply(seq_along(lhs), function(index) {
      paste(sort(c(as.character(lhs[[index]]), as.character(rhs[[index]]))), collapse = "\r")
    }, character(1))
  }
  explicit_covariance_keys <- if (nrow(explicit_covariance)) {
    covariance_key(explicit_covariance$lhs, explicit_covariance$rhs)
  } else {
    character(0)
  }
  format_start_value <- function(value) {
    format(as.numeric(value), digits = 15L, scientific = FALSE, trim = TRUE)
  }
  covariance_syntax <- if (nrow(explicit_covariance)) {
    vapply(seq_len(nrow(explicit_covariance)), function(index) {
      row <- explicit_covariance[index, , drop = FALSE]
      free <- suppressWarnings(as.integer(row$free[[1L]]))
      start <- suppressWarnings(as.numeric(row$ustart[[1L]]))
      modifier <- if (is.finite(free) && free == 0L && is.finite(start)) {
        paste0(format_start_value(start), "*")
      } else if (is.finite(free) && free > 0L && is.finite(start)) {
        paste0("start(", format_start_value(start), ")*")
      } else {
        ""
      }
      paste0(row$lhs[[1L]], " ~~ ", modifier, row$rhs[[1L]])
    }, character(1))
  } else {
    character(0)
  }

  automatic_covariance <- if (length(top_level_factors) > 1L) {
    pairs <- utils::combn(top_level_factors, 2L, simplify = FALSE)
    pairs <- Filter(function(pair) {
      !covariance_key(pair[[1L]], pair[[2L]]) %in% explicit_covariance_keys
    }, pairs)
    vapply(pairs, function(pair) paste(pair, collapse = " ~~ "), character(1))
  } else {
    character(0)
  }
  result <- paste(c(measurement, covariance_syntax, automatic_covariance), collapse = "\n")
  attr(result, "constraint_audit") <- constraint_audit
  result
}

structural_canvas_metric_invariance_gate_reason <- function(gate, language = NULL) {
  gate <- gate %||% list()
  ko <- identical(structural_canvas_invariance_text(language, "en", "ko"), "ko")
  code <- as.character(gate$reason_code %||% "")
  code <- if (length(code)) code[[1L]] else ""
  metrics <- gate$metrics %||% list()
  metric_value <- function(value) {
    value <- suppressWarnings(as.numeric(value %||% NA_real_))
    if (length(value) && is.finite(value[[1L]])) format_decimal3(value[[1L]]) else if (ko) "계산 불가" else "not available"
  }
  logical_value <- function(value) {
    flag <- length(value) && isTRUE(value[[1L]])
    if (ko) if (flag) "예" else "아니요" else toupper(as.character(flag))
  }
  if (identical(code, "metric_model_missing")) {
    return(if (ko) "측정단위 불변성 모형이 추정되지 않았습니다." else "Metric invariance was not estimated.")
  }
  if (identical(code, "passed")) {
    return(if (ko) "측정단위 불변성 기준을 통과했습니다." else "Metric invariance gate passed.")
  }
  if (identical(code, "fit_changes_unavailable")) {
    reason <- if (ko) {
      "적합도 변화량을 계산할 수 없어 측정단위 불변성 통과 여부를 판정하지 못했습니다"
    } else {
      "Metric invariance could not be evaluated because one or more fit-index changes were not available"
    }
  } else if (identical(code, "failed")) {
    reason <- if (ko) "측정단위 불변성 기준을 통과하지 못했습니다" else "Metric invariance gate failed"
  } else {
    legacy_reason <- as.character(gate$reason %||% "")
    legacy_reason <- if (length(legacy_reason)) trimws(legacy_reason[[1L]]) else ""
    return(if (nzchar(legacy_reason)) legacy_reason else if (ko) "측정불변성 기준이 기록되지 않았습니다." else "Measurement-invariance gate was not recorded.")
  }
  paste0(
    reason, " (ΔCFI=", metric_value(metrics$delta_cfi),
    ", ΔRMSEA=", metric_value(metrics$delta_rmsea),
    ", ΔSRMR=", metric_value(metrics$delta_srmr),
    if (ko) ", 수렴=" else ", converged=", logical_value(metrics$converged),
    if (ko) ", 허용 가능한 해=" else ", admissible=", logical_value(metrics$admissible), ")."
  )
}

structural_canvas_metric_invariance_gate <- function(invariance, language = NULL) {
  table <- invariance$table %||% data.frame()
  row <- table[table$Model == "Metric", , drop = FALSE]
  criteria <- list(
    delta_cfi_min = -.010,
    delta_rmsea_max = .015,
    delta_srmr_max = .030,
    require_converged = TRUE,
    require_admissible = TRUE
  )
  if (!nrow(row)) {
    gate <- list(
      passed = FALSE,
      reason_code = "metric_model_missing",
      metrics = list(
        delta_cfi = NA_real_, delta_rmsea = NA_real_, delta_srmr = NA_real_,
        converged = FALSE, admissible = FALSE
      ),
      criteria = criteria
    )
    gate$reason <- structural_canvas_metric_invariance_gate_reason(gate, "en")
    return(gate)
  }
  metrics <- list(
    delta_cfi = suppressWarnings(as.numeric(row$DeltaCFI[[1L]])),
    delta_rmsea = suppressWarnings(as.numeric(row$DeltaRMSEA[[1L]])),
    delta_srmr = suppressWarnings(as.numeric(row$DeltaSRMR[[1L]])),
    converged = isTRUE(row$Converged[[1L]]),
    admissible = isTRUE(row$Admissible[[1L]])
  )
  passed <- isTRUE(metrics$converged) && isTRUE(metrics$admissible) &&
    is.finite(metrics$delta_cfi) && metrics$delta_cfi >= criteria$delta_cfi_min &&
    is.finite(metrics$delta_rmsea) && metrics$delta_rmsea <= criteria$delta_rmsea_max &&
    is.finite(metrics$delta_srmr) && metrics$delta_srmr <= criteria$delta_srmr_max
  reason_code <- if (passed) {
    "passed"
  } else if (!all(is.finite(c(metrics$delta_cfi, metrics$delta_rmsea, metrics$delta_srmr)))) {
    "fit_changes_unavailable"
  } else {
    "failed"
  }
  gate <- list(
    passed = FALSE,
    reason_code = reason_code,
    metrics = metrics,
    criteria = criteria
  )
  gate$passed <- passed
  # Retain a stable English compatibility field for older consumers. UI and
  # exports localize from reason_code/metrics instead of persisting UI text.
  gate$reason <- structural_canvas_metric_invariance_gate_reason(gate, "en")
  gate
}

structural_canvas_normalize_metric_invariance_gate <- function(gate = NULL, invariance = NULL) {
  gate <- gate %||% list()
  code <- as.character(gate$reason_code %||% "")
  code <- if (length(code)) trimws(code[[1L]]) else ""
  if (nzchar(code) && is.list(gate$metrics) && is.list(gate$criteria)) return(gate)
  if (is.list(invariance) && is.data.frame(invariance$table %||% NULL)) {
    return(structural_canvas_metric_invariance_gate(invariance))
  }
  gate
}

structural_canvas_run_measurement_invariance <- function(
  analysis_type, invariance_enabled, result, data, invariance_group,
  estimator, missing, std_lv, rmsea_ci, ordered, snapshot = NULL,
  micom_permutations = 5000L, micom_seed = 20260816L,
  ml_likelihood = "normal", language = NULL,
  mga_bootstrap_reps = 5000L, mga_seed = 20260816L,
  result_coefficient = "pls_p", result_measurement_coefficient = "measurement_p",
  invariance_path_scope = "all", invariance_selected_path_ids = character(0)
) {
  text <- function(en, ko) structural_canvas_invariance_text(language, en, ko)
  invariance_result <- NULL
  if (identical(analysis_type, "cfa") && invariance_enabled) {
    if (!length(ordered) && !toupper(estimator) %in% c("ML", "MLR")) stop(text("Continuous-indicator measurement invariance requires ML or MLR.", "연속형 지표의 측정불변성 분석에는 ML 또는 MLR 추정이 필요합니다."))
    if (length(ordered) && !toupper(estimator) %in% c("WLSMV", "DWLS")) stop(text("Ordered-indicator measurement invariance requires WLSMV or DWLS.", "순서형 지표의 측정불변성 분석에는 WLSMV 또는 DWLS 추정이 필요합니다."))
    if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop(text("Select a valid grouping variable for measurement invariance analysis.", "측정불변성 분석에 사용할 올바른 집단변수를 선택하십시오."))
    if (invariance_group %in% lavaan::lavNames(result$fit, "ov")) stop(text("The grouping variable cannot also be an indicator in the CFA model.", "집단변수는 CFA 모형의 측정지표로 동시에 사용할 수 없습니다."))
    group_count <- length(unique(data[[invariance_group]][!is.na(data[[invariance_group]])]))
    if (group_count < 2L || group_count > 20L) stop(text("The grouping variable must contain between 2 and 20 non-empty groups.", "집단변수에는 관측치가 있는 집단이 2개 이상 20개 이하여야 합니다."))
    invariance_result <- structural_canvas_with_progress(message = text("Estimating measurement-invariance models", "측정불변성 모형 추정 중"), value = 0, {
      structural_canvas_inc_progress(.15, detail = text("Configural, metric, scalar, and strict models", "구성형태·측정단위·절편·엄격 불변성 모형"))
      value <- structural_canvas_measurement_invariance(result$syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered, ml_likelihood)
      structural_canvas_inc_progress(.85, detail = text("Preparing robust comparisons", "강건 비교 결과 준비 중"))
      value
    })
  }
  if (analysis_type %in% c("cbsem", "sem") && invariance_enabled) {
    if (length(ordered) || !toupper(estimator) %in% c("ML", "MLR")) stop(text("Structural path group comparison requires continuous ML or MLR SEM/CB-SEM.", "집단 간 구조경로 비교에는 연속형 지표와 ML 또는 MLR 추정의 SEM/CB-SEM이 필요합니다."))
    if (!nzchar(invariance_group) || !invariance_group %in% names(data)) stop(text("Select a valid grouping variable for structural path group comparison.", "집단 간 구조경로 비교에 사용할 올바른 집단변수를 선택하십시오."))
    path_selection <- structural_canvas_resolve_multigroup_path_selection(
      snapshot %||% list(), invariance_path_scope, invariance_selected_path_ids
    )
    moderation_definitions <- result$moderation_definitions %||% list()
    product_preparation <- if (length(moderation_definitions)) {
      structural_canvas_prepare_group_product_indicators(data, invariance_group, moderation_definitions)
    } else {
      list(data = data, audit = data.frame(), policy = list())
    }
    invariance_data <- product_preparation$data
    invariance_result <- structural_canvas_with_progress(message = text("Testing measurement invariance before structural path comparison", "집단 간 구조경로 비교 전 측정불변성 검정 중"), value = 0, {
      structural_canvas_inc_progress(.10, detail = text("Configural and metric measurement models", "구성형태 및 측정단위 불변성 모형"))
      # Apply the Chen-style configural/metric gate to the substantive factors
      # only.  Product indicators are artificial nonlinear terms and can make
      # change-index cutoffs answer a different question.  After this gate,
      # the joint structural models separately constrain the original and
      # interaction-factor loadings equal and suppress inference unless those
      # product-factor models converge and are admissible.
      measurement_syntax <- structural_canvas_base_measurement_syntax(
        result$syntax, moderation_definitions, language
      )
      measurement <- structural_canvas_measurement_invariance(measurement_syntax, data, invariance_group, estimator, missing, std_lv, rmsea_ci, ordered, ml_likelihood)
      gate <- structural_canvas_metric_invariance_gate(measurement, language)
      if (!isTRUE(gate$passed)) stop(paste0(structural_canvas_metric_invariance_gate_reason(gate, language), text(
        " Structural path equality tests were not run. Automatic or user-specified partial-invariance refitting is not implemented in this release; score/EPC diagnostics do not override the gate.",
        " 집단 간 구조경로 동일성 검정은 실행하지 않았습니다. 이 버전은 자동 또는 사용자 지정 부분불변성 재적합을 지원하지 않으며, 점수/EPC 진단만으로 이 기준을 통과한 것으로 처리하지 않습니다."
      )))
      structural_canvas_inc_progress(.45, detail = text("Metric gate passed; fitting free and equal structural paths", "측정단위 불변성 통과: 자유모형과 구조경로 동일화 모형 추정 중"))
      value <- structural_canvas_structural_path_group_comparison(
        result$syntax, invariance_data, invariance_group, estimator, missing,
        std_lv, rmsea_ci, ordered, ml_likelihood,
        effect_definitions = result$effect_definitions %||% list(),
        moderation_definitions = moderation_definitions,
        product_indicator_audit = product_preparation$audit,
        product_indicator_policy = product_preparation$policy,
        path_scope = path_selection$scope,
        selected_paths = path_selection$selected_paths
      )
      value$requested_path_ids <- path_selection$requested_path_ids
      value$measurement_invariance <- measurement
      value$measurement_gate <- gate
      structural_canvas_inc_progress(.45, detail = text("Preparing group-specific path comparisons", "집단별 구조경로 비교 결과 준비 중"))
      value
    })
  }
  if (identical(analysis_type, "plssem") && invariance_enabled) {
    if (!identical(toupper(as.character(estimator)), "PLS")) stop(text(
      "MICOM in this release is restricted to PLS composite-score models. PLSc common-factor measurement invariance is not established by the MICOM procedure and is therefore blocked.",
      "이 버전의 MICOM은 PLS 합성점수 모형에만 적용됩니다. MICOM으로는 PLSc 공통요인의 측정불변성을 확립할 수 없으므로 PLSc 다집단 추론은 차단됩니다."
    ))
    path_selection <- structural_canvas_resolve_multigroup_path_selection(
      snapshot %||% list(), invariance_path_scope, invariance_selected_path_ids
    )
    invariance_result <- structural_canvas_with_progress(message = text("Running MICOM and PLS multi-group analysis", "MICOM 및 PLS 다집단분석 실행 중"), value = 0, {
      structural_canvas_inc_progress(.10, detail = text("Checking configural invariance and group sizes", "구성불변성과 집단 크기 확인 중"))
      value <- structural_canvas_micom(
        snapshot %||% list(), data, invariance_group, estimator, micom_permutations, micom_seed,
        path_scope = path_selection$scope, selected_paths = path_selection$selected_paths
      )
      structural_canvas_inc_progress(.35, detail = text("Evaluating compositional, mean, and variance invariance", "합성점수·평균·분산 불변성 평가 중"))
      # Keep MICOM (measurement comparability) and PLS-MGA (structural-effect
      # inference) as separate, auditable procedures.  The former gates the
      # latter, while the group bootstraps are rerun independently inside each
      # group for every canonical effect family.
      legacy_permutation_paths <- value$mga_table %||% data.frame()
      mga <- structural_canvas_pls_mga_effects(
        snapshot %||% list(), data, invariance_group,
        estimator = estimator,
        bootstrap_reps = mga_bootstrap_reps,
        seed = mga_seed,
        micom_result = value,
        result_coefficient = result_coefficient,
        measurement_coefficient = result_measurement_coefficient,
        path_scope = path_selection$scope,
        selected_paths = path_selection$selected_paths
      )
      value$permutation_path_sensitivity <- legacy_permutation_paths
      value$pls_mga <- mga
      value$pls_modmed_mga <- mga$pls_modmed_mga %||% NULL
      value$group_effects <- mga$group_effects %||% data.frame()
      value$pairwise_effect_differences <- mga$pairwise_differences %||% data.frame()
      value$mga_table <- mga$mga_table %||% data.frame()
      value$mga_status <- paste(
        as.character(mga$status %||% "Not recorded"),
        as.character(mga$reason %||% "")
      )
      structural_canvas_inc_progress(.55, detail = text("Preparing MICOM-gated group effects and pairwise differences", "MICOM을 통과한 집단별 효과와 집단 쌍 차이 준비 중"))
      value
    })
  }
  invariance_result
}
