# Survival analysis result UI.

survival_cell_note_marker <- function(table, row_index, column) {
  markers <- attr(table, "note_markers", exact = TRUE)
  if (!is.data.frame(markers) || nrow(markers) == 0) return("")
  matched <- markers[markers$row == row_index & markers$column == column, , drop = FALSE]
  if (nrow(matched) == 0) "" else as.character(matched$marker[[1]])
}

survival_cell_content <- function(value, marker = "") {
  value <- as.character(value %||% "")
  marker <- as.character(marker %||% "")
  if (!nzchar(value) || !nzchar(marker)) return(value)
  tagList(
    value,
    tags$sup(style = "margin-left:2px;font-size:75%;vertical-align:super;", marker)
  )
}

survival_header_content <- function(name) {
  name <- as.character(name)
  if (identical(name, "Chi-square")) {
    return(tagList(HTML("&chi;"), tags$sup("2")))
  }
  if (identical(name, "Chi-square (df)")) {
    return(tagList(HTML("&chi;"), tags$sup("2"), "(df)"))
  }
  if (identical(name, "Median (95% CI)")) {
    return(tagList("Median", tags$br(), "(95% CI)"))
  }
  name
}

survival_column_class <- function(name) {
  key <- tolower(gsub("[^A-Za-z0-9]+", "-", as.character(name %||% "")))
  key <- gsub("(^-+|-+$)", "", key)
  paste("survival-col", paste0("survival-col-", key))
}

survival_simple_table <- function(table, class = "survival-result-table") {
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  tags$table(
    class = paste("coefficient-table", class),
    style = result_table_style(font_size = 12, min_width = 0),
    tags$thead(tags$tr(lapply(seq_along(table), function(col_index) {
      name <- names(table)[[col_index]]
      tags$th(
        class = survival_column_class(name),
        style = paste0(result_header_cell_style(col_index == 1L), if (col_index == 1L) "" else "text-align:center !important;"),
        survival_header_content(name)
      )
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        column <- names(table)[[col_index]]
        value <- table[[col_index]][[row_index]]
        if (is.numeric(value)) value <- survival_format_number(value)
        marker <- survival_cell_note_marker(table, row_index, column)
        tags$td(
          class = survival_column_class(column),
          style = paste0(result_body_cell_style(col_index == 1L, row_index == nrow(table)), if (col_index == 1L) "" else "text-align:center !important;"),
          survival_cell_content(value, marker)
        )
      }))
    }))
  )
}

survival_table_note <- function(text) {
  text <- as.character(text %||% "")
  if (!nzchar(text)) return(NULL)
  div(class = "survival-table-note", text)
}

survival_km_summary_note <- function() {
  survival_table_note("M = restricted mean over the default fitted follow-up horizon; use the RMST table for a prespecified tau comparison. SE = standard error; CI = confidence interval; NE = not estimable; df = degrees of freedom; p = p value.")
}

survival_rate_note <- function() {
  survival_table_note("CI = confidence interval.")
}

survival_cox_coef_note <- function() {
  survival_table_note("HR = hazard ratio; CI = confidence interval; p = p value. Categorical predictors list every level, and the reference level is shown with HR = 1.")
}

survival_cox_statistic_note <- function() {
  survival_table_note("B = log hazard coefficient; SE = standard error; z = Wald z statistic; p = p value. This supplementary table provides model statistics underlying the hazard-ratio table.")
}

survival_method_sentence <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  origin <- as.character(result$time_origin %||% "")[[1]]
  unit <- as.character(result$time_unit %||% "")[[1]]
  origin <- if (nzchar(origin)) origin else if (ko) "미지정" else "not specified"
  unit <- if (nzchar(unit)) unit else if (ko) "미지정" else "not specified"
  method <- if (identical(result$type, "cox")) {
    base_method <- if (nzchar(as.character(result$start %||% ""))) if (ko) "대상자 군집-강건 표준오차를 적용한 시간의존 Cox 비례위험모형" else "a time-dependent Cox proportional hazards model with subject-cluster robust standard errors" else if (nzchar(as.character(result$entry %||% ""))) if (ko) "지연 진입을 반영한 Cox 비례위험모형" else "a Cox proportional hazards model with delayed entry" else if (ko) "Cox 비례위험모형" else "a Cox proportional hazards model"
    if (nzchar(as.character(result$strata %||% ""))) base_method <- paste0(base_method, if (ko) sprintf(" (층화 변수: %s; 층별 기저위험 허용)", result$strata) else sprintf(" stratified by %s with stratum-specific baseline hazards", result$strata))
    if (!nzchar(as.character(result$start %||% "")) && nzchar(as.character(result$cluster %||% ""))) base_method <- paste0(base_method, if (ko) sprintf(" (군집 ID: %s; sandwich 강건 표준오차)", result$cluster) else sprintf(" with sandwich robust standard errors clustered by %s", result$cluster))
    if (nzchar(as.character(result$spline_covariate %||% ""))) base_method <- paste0(base_method, if (ko) sprintf(" (%s는 자유도 %d의 natural cubic spline)", result$spline_covariate, result$spline_df) else sprintf(" with %s modeled using a natural cubic spline with %d degrees of freedom", result$spline_covariate, result$spline_df))
    if (nzchar(as.character(result$time_varying_covariate %||% ""))) base_method <- paste0(base_method, if (ko) sprintf(" (%s의 계수는 log(1 + time)에 따라 변화)", result$time_varying_covariate) else sprintf(" with the coefficient for %s varying over log(1 + time)", result$time_varying_covariate))
    base_method <- paste0(base_method, if (ko) sprintf(" (동률 사건: %s 방식)", result$ties %||% "Efron") else sprintf(" using the %s method for tied event times", result$ties %||% "Efron"))
    base_method
  } else if (identical(result$type, "competing_risk")) {
    has_cs <- is.list(result$cause_specific)
    has_fg <- is.list(result$fine_gray)
    has_group <- nzchar(as.character(result$group %||% ""))
    censoring_text <- if (has_fg && nzchar(as.character(result$censoring_group %||% ""))) if (ko) paste0(" (검열분포 층화: ", result$censoring_group, ")") else paste0(" with censoring distributions estimated separately within ", result$censoring_group) else ""
    paste0(if (ko) paste0("누적발생함수", if (has_group) "와 Gray 검정" else "", if (has_cs) ", 원인별 Cox 회귀" else "", if (has_fg) ", Fine–Gray 회귀" else "") else paste0("cumulative incidence functions", if (has_group) " and Gray's test" else "", if (has_cs) ", cause-specific Cox regression" else "", if (has_fg) ", and Fine–Gray regression" else ""), censoring_text)
  } else {
    if (identical(result$analysis_method, "life_table")) if (ko) "생명표 방법" else "the actuarial life-table method" else if (nzchar(as.character(result$entry %||% ""))) if (ko) "지연 진입을 반영한 Kaplan–Meier 방법" else "the Kaplan–Meier method with delayed entry" else if (ko) "Kaplan–Meier 방법" else "the Kaplan–Meier method"
  }
  if (ko) sprintf("시간 원점은 '%s', 시간 단위는 '%s'로 정의하였으며, %s을 사용하였다.", origin, unit, method) else sprintf("Time was measured from '%s' in units of '%s', and %s was used.", origin, unit, method)
}

survival_reporting_preflights <- function(result) {
  if (identical(result$type, "km_multi")) return(lapply(result$analyses %||% list(), function(item) item$preflight))
  list(result$preflight)
}

survival_reporting_event_map <- function(result) {
  maps <- lapply(survival_reporting_preflights(result), function(preflight) preflight$transformations$event_map %||% data.frame())
  maps <- Filter(function(map) is.data.frame(map) && nrow(map), maps)
  if (!length(maps)) return(data.frame())
  map <- unique(do.call(rbind, maps))
  data.frame(`Raw value` = map$raw_value, Role = map$role, Label = map$label, check.names = FALSE, stringsAsFactors = FALSE)
}

survival_reporting_exclusions <- function(result) {
  audits <- lapply(survival_reporting_preflights(result), function(preflight) preflight$row_audit %||% data.frame())
  reasons <- unlist(lapply(audits, function(audit) {
    if (!is.data.frame(audit) || !"exclusion_reasons" %in% names(audit)) return(character(0))
    strsplit(audit$exclusion_reasons[!audit$included & nzchar(audit$exclusion_reasons)], ";", fixed = TRUE)
  }), recursive = TRUE, use.names = FALSE)
  reasons <- reasons[nzchar(reasons)]
  if (!length(reasons)) return(data.frame())
  counts <- sort(table(reasons), decreasing = TRUE)
  data.frame(`Exclusion reason` = names(counts), N = as.integer(counts), check.names = FALSE, stringsAsFactors = FALSE)
}

survival_reporting_data_flow <- function(result) {
  rows <- lapply(survival_reporting_preflights(result), function(preflight) {
    counts <- preflight$counts %||% list()
    data.frame(`Source rows` = counts$source_rows %||% NA_integer_, `Analysis rows` = counts$analysis_rows %||% NA_integer_, `Source subjects` = counts$source_subjects %||% NA_integer_, `Analysis subjects` = counts$analysis_subjects %||% NA_integer_, Events = counts$events %||% NA_integer_, `Competing events` = counts$competing_events %||% NA_integer_, Censored = counts$censored %||% NA_integer_, check.names = FALSE)
  })
  unique(do.call(rbind, rows))
}

survival_reporting_bundle <- function(result, language = statedu_initial_language()) {
  cox_diagnostics <- if (identical(result$type, "cox")) list(
    `Cox_PH_tests` = result$ph_table %||% data.frame(),
    `Cox_functional_form_data` = result$functional_form_data %||% data.frame(),
    `Cox_influence` = result$influence_table %||% data.frame(),
    `Cox_collinearity` = result$collinearity_table %||% data.frame(),
    `Cox_strata_event_counts` = result$strata_table %||% data.frame(),
    `Cox_cluster_summary` = result$cluster_summary %||% data.frame(),
    `Cox_spline_nonlinearity_test` = result$spline_test_table %||% data.frame(),
    `Cox_spline_HR_curve` = result$spline_curve %||% data.frame(),
    `Cox_time_varying_coefficient_test` = result$time_varying_test_table %||% data.frame(),
    `Cox_time_specific_HR` = result$time_varying_at_times %||% data.frame(),
    `Cox_time_varying_HR_curve` = result$time_varying_curve %||% data.frame(),
    `Cox_tied_event_summary` = result$ties_summary %||% data.frame(),
    `Cox_categorical_reference_levels` = result$categorical_reference_table %||% data.frame(),
    `Cox_categorical_joint_tests` = result$categorical_joint_tests %||% data.frame(),
    `Adjusted_survival_curve` = result$adjusted_survival$curve %||% data.frame(),
    `Adjusted_survival_time_points` = result$adjusted_survival$time_point_estimates %||% data.frame(),
    `Adjusted_survival_contrasts` = result$adjusted_survival$time_point_contrasts %||% data.frame()
  ) else list()
  km_diagnostics <- if (result$type %in% c("km", "km_multi")) list(
    `KM_curve_crossing_screen` = survival_km_crossing_summary_table(result, formatted = FALSE)
  ) else list()
  competing_diagnostics <- if (identical(result$type, "competing_risk")) list(
    `Competing_estimand_contract` = result$estimand_table %||% data.frame(),
    `Fine_Gray_censoring_distribution_strata` = result$censoring_group_table %||% data.frame(),
    `CIF_integrity_checks` = result$cif_integrity %||% data.frame(),
    `Competing_group_cause_event_counts` = result$event_count_table %||% data.frame(),
    `Gray_tests` = result$gray_tests %||% data.frame(),
    `Competing_regression_categorical_reference_levels` = result$categorical_reference_table %||% data.frame(),
    `Competing_regression_coefficient_labels` = result$coefficient_label_table %||% data.frame(),
    `Cause_specific_Cox_coefficients` = result$cause_specific$coef_table %||% data.frame(),
    `Cause_specific_model_tests` = result$cause_specific$model_tests %||% data.frame(),
    `Cause_specific_categorical_joint_tests` = result$cause_specific$categorical_joint_tests %||% data.frame(),
    `Cause_specific_PH_tests` = result$cause_specific$ph_table %||% data.frame(),
    `Cause_specific_residual_summary` = result$cause_specific$residual_table %||% data.frame(),
    `Cause_specific_functional_form_data` = result$cause_specific$functional_form_data %||% data.frame(),
    `Cause_specific_influence` = result$cause_specific$influence_table %||% data.frame(),
    `Cause_specific_collinearity` = result$cause_specific$collinearity_table %||% data.frame(),
    `Fine_Gray_coefficients` = result$fine_gray$coef_table %||% data.frame(),
    `Fine_Gray_model_tests` = result$fine_gray$model_tests %||% data.frame(),
    `Fine_Gray_categorical_joint_tests` = result$fine_gray$categorical_joint_tests %||% data.frame(),
    `Fine_Gray_optimizer_diagnostics` = result$fine_gray$optimizer_table %||% data.frame(),
    `Fine_Gray_collinearity` = result$fine_gray$collinearity_table %||% data.frame(),
    `Fine_Gray_proportionality_screen` = result$fine_gray$residual_review %||% data.frame(),
    `Fine_Gray_Schoenfeld_like_residuals` = result$fine_gray$residual_data %||% data.frame()
  ) else list()
  list(method = survival_method_sentence(result, language), data_flow = survival_reporting_data_flow(result), event_map = survival_reporting_event_map(result), exclusions = survival_reporting_exclusions(result), followup = survival_followup_diagnostics(result), stability = survival_stability_review(result, language), checklist = survival_reporting_checklist(result, language), interpretation = survival_interpretation_guide(result, language), diagnostics = c(cox_diagnostics, km_diagnostics, competing_diagnostics))
}

survival_followup_diagnostics <- function(result) {
  base <- if (identical(result$type, "km_multi")) (result$analyses %||% list())[[1]] else result
  data <- base$data
  preflight <- base$preflight
  if (!is.data.frame(data) || !nrow(data)) return(data.frame())
  time_name <- if (nzchar(as.character(base$stop %||% ""))) base$stop else base$time
  if (!nzchar(as.character(time_name %||% "")) || !time_name %in% names(data)) return(data.frame())
  raw_name <- preflight$transformations$raw_event_column %||% ""
  map <- preflight$transformations$event_map %||% data.frame()
  if (!nzchar(raw_name) || !raw_name %in% names(data) || !is.data.frame(map)) return(data.frame())
  role_lookup <- stats::setNames(as.character(map$role), as.character(map$raw_value))
  censor <- unname(role_lookup[trimws(as.character(data[[raw_name]]))]) == "censored"
  entry_name <- as.character(base$entry %||% "")
  entry_values <- if (nzchar(entry_name) && entry_name %in% names(data)) as.numeric(data[[entry_name]]) else rep(0, nrow(data))
  working <- data.frame(Entry = entry_values, Time = as.numeric(data[[time_name]]), Censored = censor, stringsAsFactors = FALSE)
  subject_id <- as.character(base$subject_id %||% "")
  if (nzchar(subject_id) && subject_id %in% names(data)) {
    working$Subject <- as.character(data[[subject_id]])
    ordered <- order(working$Subject, working$Time)
    working <- working[ordered, , drop = FALSE]
    working <- working[!duplicated(working$Subject, fromLast = TRUE), , drop = FALSE]
  }
  working <- working[is.finite(working$Entry) & is.finite(working$Time) & working$Entry < working$Time & !is.na(working$Censored), , drop = FALSE]
  if (!nrow(working)) return(data.frame())
  reverse_fit <- if (any(working$Entry > 0)) survival::survfit(survival::Surv(Entry, Time, Censored) ~ 1, data = working) else survival::survfit(survival::Surv(Time, Censored) ~ 1, data = working)
  median_followup <- as.numeric(summary(reverse_fit)$table[["median"]] %||% NA_real_)
  horizon <- max(working$Time)
  tail_time <- as.numeric(stats::quantile(working$Time, .9, names = FALSE, type = 1))
  tail_summary <- summary(reverse_fit, times = tail_time, extend = TRUE)
  data.frame(
    `Analysis subjects/records` = nrow(working),
    `Censored records` = sum(working$Censored),
    `Censoring proportion` = mean(working$Censored),
    `Median potential follow-up (reverse KM)` = median_followup,
    `Maximum observed time` = horizon,
    `90th percentile time` = tail_time,
    `At risk at 90th percentile time` = as.integer(tail_summary$n.risk[[1]] %||% NA_integer_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_stability_review <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  rows <- list()
  add <- function(level, code, evidence, guidance) rows[[length(rows) + 1L]] <<- data.frame(Level = level, Code = code, Evidence = evidence, Guidance = guidance, stringsAsFactors = FALSE)
  if (identical(result$type, "cox")) {
    epp <- as.numeric(result$events_per_parameter %||% NA_real_)
    if (is.finite(epp) && epp < 10) add(if (epp < 5) "high" else "review", "low_events_per_parameter", sprintf("Events/parameter = %s", survival_format_number(epp)), if (ko) "계수의 불안정성과 과적합 가능성을 검토하고 모형 단순화 또는 penalized 분석을 고려하세요." else "Review coefficient instability and overfitting; consider model simplification or penalized analysis.")
    if (nzchar(as.character(result$cluster %||% "")) && result$cluster %in% names(result$data)) {
      clusters <- length(unique(result$data[[result$cluster]]))
      if (clusters < 30L) add(if (clusters < 10L) "high" else "review", "few_robust_variance_clusters", sprintf("Robust-variance clusters = %d", clusters), if (ko) "소수 군집에서는 sandwich 표준오차의 유한표본 편향 가능성이 있으므로 small-sample 보정 또는 별도 군집 분석을 검토하세요." else "With few clusters, review finite-sample bias of sandwich standard errors and consider a small-sample correction or dedicated clustered analysis.")
    }
    if (any(!is.finite(result$coef_table$B %||% numeric(0)))) add("high", "nonfinite_coefficient", "One or more coefficients are non-finite.", if (ko) "희소성, 완전분리 또는 모형 식별 문제를 검토하세요." else "Review sparsity, separation, or model-identification problems.")
    vif <- as.numeric(result$collinearity_table$VIF %||% numeric(0))
    if (any(!is.finite(vif))) add("high", "nonfinite_vif", "At least one design-column VIF is not finite.", if (ko) "완전 공선성 또는 비추정 설계열을 검토하세요." else "Review exact collinearity or non-estimable design columns.") else if (any(vif >= 10, na.rm = TRUE)) add("high", "high_vif", sprintf("Maximum design-column VIF = %s", survival_format_number(max(vif, na.rm = TRUE))), if (ko) "계수와 표준오차의 불안정성을 검토하세요." else "Review instability in coefficients and standard errors.") else if (any(vif >= 5, na.rm = TRUE)) add("review", "elevated_vif", sprintf("Maximum design-column VIF = %s", survival_format_number(max(vif, na.rm = TRUE))), if (ko) "공변량 중복성과 모형 명세를 검토하세요." else "Review covariate redundancy and model specification.")
    condition_number <- as.numeric(result$condition_number %||% NA_real_)
    if (is.finite(condition_number) && condition_number >= 30) add("review", "high_condition_number", sprintf("Design condition number = %s", survival_format_number(condition_number)), if (ko) "설계행렬의 수치적 불안정성을 검토하세요." else "Review numerical instability in the design matrix.")
    influence <- result$influence_table
    if (is.data.frame(influence) && "Review signal" %in% names(influence) && any(influence$`Review signal` %in% TRUE)) add("review", "dfbetas_influence_signal", sprintf("Influential coefficient rows = %d", sum(influence$`Review signal` %in% TRUE)), if (ko) "표준화 DFBETAS가 경험적 선별기준을 넘은 행을 민감도 분석에서 검토하세요." else "Review rows exceeding the heuristic standardized DFBETAS threshold in sensitivity analyses.")
    strata_table <- result$strata_table
    if (is.data.frame(strata_table) && nrow(strata_table)) {
      zero_event <- strata_table$Stratum[strata_table$Events == 0]
      sparse_event <- strata_table$Stratum[strata_table$Events > 0 & strata_table$Events < 5]
      if (length(zero_event)) add("high", "zero_event_stratum", paste("No events in strata:", paste(zero_event, collapse = ", ")), if (ko) "사건이 없는 층의 기저위험과 해당 층이 기여하는 부분우도를 검토하고 결과 해석을 제한하세요." else "Review baseline-hazard estimation and partial-likelihood contribution for event-free strata and limit interpretation.")
      if (length(sparse_event)) add("review", "sparse_event_stratum", paste("Fewer than 5 events in strata:", paste(sparse_event, collapse = ", ")), if (ko) "희소 사건 층에서 계수와 층별 기저위험의 불안정성을 검토하세요." else "Review coefficient and stratum-specific baseline-hazard instability in sparse-event strata.")
    }
    adjusted <- result$adjusted_survival
    if (is.list(adjusted) && (adjusted$bootstrap_reps %||% 0L) > 0L) {
      ratio <- as.numeric(adjusted$bootstrap_effective_ratio %||% NA_real_)
      if (!isTRUE(adjusted$ci_available)) add("high", "adjusted_survival_bootstrap_insufficient", sprintf("Valid adjusted-survival bootstrap replicates = %d/%d", adjusted$bootstrap_successful %||% 0L, adjusted$bootstrap_reps %||% 0L), if (ko) "유효 반복이 요청 반복의 80% 미만이므로 보정 생존곡선의 bootstrap 신뢰구간을 보고하지 마세요." else "Do not report the adjusted-survival bootstrap intervals because fewer than 80% of requested replicates were valid.") else if (is.finite(ratio) && ratio < .9) add("review", "adjusted_survival_bootstrap_attrition", sprintf("Valid adjusted-survival bootstrap ratio = %.1f%%", 100 * ratio), if (ko) "유효 반복 손실의 원인(희소 집단, 수렴 실패)을 검토하고 반복 수와 유효률을 함께 보고하세요." else "Review causes of replicate loss (sparse groups or convergence failures) and report both requested and valid replicates.")
    }
    ties_summary <- result$ties_summary
    if (identical(result$ties_method %||% "efron", "breslow") && is.data.frame(ties_summary) && nrow(ties_summary)) {
      tied_proportion <- as.numeric(ties_summary$`Proportion of events at tied times`[[1]])
      if (is.finite(tied_proportion) && tied_proportion >= .1) add("review", "breslow_with_substantial_ties", sprintf("Events at tied times = %.1f%%", 100 * tied_proportion), if (ko) "동률 사건 비중이 큰 경우 Breslow 근사 대신 Efron 또는 연구설계에 맞는 exact 방법과의 민감도 비교를 검토하세요." else "With substantial tied events, consider sensitivity comparison with Efron or an exact method appropriate to the study design.")
    }
    joint_tests <- result$categorical_joint_tests
    if (is.data.frame(joint_tests) && nrow(joint_tests) && any(!joint_tests$Estimable)) add("high", "categorical_joint_test_not_estimable", paste("Non-estimable categorical joint tests:", paste(joint_tests$Variable[!joint_tests$Estimable], collapse = ", ")), if (ko) "범주형 변수의 희소 수준·분리·특이 공분산행렬을 검토하고 전체 효과 검정을 보고하지 마세요." else "Review sparse levels, separation, or a singular covariance matrix and do not report the affected omnibus test.")
  } else if (identical(result$type, "competing_risk")) {
    counts <- result$preflight$counts %||% list()
    cif_integrity <- result$cif_integrity
    if (is.data.frame(cif_integrity) && nrow(cif_integrity) && any(!cif_integrity$`Integrity passed`)) add("high", "cif_integrity_failure", paste("CIF integrity failed for groups:", paste(cif_integrity$Group[!cif_integrity$`Integrity passed`], collapse = ", ")), if (ko) "CIF 범위·유한성 또는 원인별 합 검사를 통과하지 못했으므로 곡선과 추정값을 보고하지 말고 사건 코딩과 엔진 결과를 검토하세요." else "Do not report the CIF curves or estimates because a finiteness, bounds, or cause-sum integrity check failed; review event coding and engine output.")
    if ((counts$events %||% 0L) < 10L) add("review", "few_interest_events", sprintf("Interest events = %d", counts$events %||% 0L), if (ko) "관심 사건 추정치와 회귀계수의 불확실성을 강조하세요." else "Emphasize uncertainty in interest-event estimates and regression coefficients.")
    if ((counts$competing_events %||% 0L) < 10L) add("review", "few_competing_events", sprintf("Competing events = %d", counts$competing_events %||% 0L), if (ko) "경쟁사건 CIF와 원인별 비교의 희소성을 검토하세요." else "Review sparsity of competing-event CIFs and cause-specific comparisons.")
    parameter_count <- max(c(
      if (is.list(result$cause_specific) && is.data.frame(result$cause_specific$coef_table)) nrow(result$cause_specific$coef_table) else 0L,
      if (is.list(result$fine_gray) && is.data.frame(result$fine_gray$coef_table)) nrow(result$fine_gray$coef_table) else 0L
    ))
    if (parameter_count > 0L && (counts$events %||% 0L) / parameter_count < 10) add("review", "low_interest_events_per_covariate", sprintf("Interest events/covariate = %s", survival_format_number((counts$events %||% 0L) / parameter_count)), if (ko) "경쟁위험 회귀모형의 복잡도를 재검토하세요." else "Reconsider competing-risk regression complexity.")
    cell_counts <- result$event_count_table
    if (is.data.frame(cell_counts) && nrow(cell_counts)) {
      zero_cells <- cell_counts[cell_counts$Events == 0L, , drop = FALSE]
      sparse_cells <- cell_counts[cell_counts$Events > 0L & cell_counts$Events < 5L, , drop = FALSE]
      if (nrow(zero_cells)) add("review", "zero_group_cause_events", paste("Zero-event group-cause cells:", paste(paste(zero_cells$Group, zero_cells$`Raw event value`, sep = "/"), collapse = ", ")), if (ko) "해당 집단·원인 CIF와 집단비교의 정보가 제한되므로 사건 수와 신뢰구간을 함께 제시하고 회귀 추정을 신중히 해석하세요." else "Information for the affected group-cause CIF and group comparison is limited; report event counts and confidence intervals and interpret regression estimates cautiously.")
      if (nrow(sparse_cells)) add("review", "sparse_group_cause_events", paste("Group-cause cells with fewer than 5 events:", paste(paste(sparse_cells$Group, sparse_cells$`Raw event value`, sep = "/"), collapse = ", ")), if (ko) "희소 집단·원인 셀의 CIF, Gray 검정 및 회귀계수 불확실성을 강조하세요." else "Emphasize uncertainty in CIFs, Gray tests, and regression coefficients for sparse group-cause cells.")
    }
    gray <- result$gray_tests
    if (is.data.frame(gray) && nrow(gray) && "Estimable" %in% names(gray) && any(!gray$Estimable)) add("high", "gray_test_not_estimable", paste("Non-estimable Gray tests for cause codes:", paste(gray$CauseCode[!gray$Estimable], collapse = ", ")), if (ko) "비추정 Gray 검정의 통계량과 p값을 보고하지 말고 집단별 사건 수와 자료 희소성을 검토하세요." else "Do not report statistics or p-values for non-estimable Gray tests; review group-specific event counts and data sparsity.")
    if (is.list(result$cause_specific) && !isTRUE(result$cause_specific$estimable)) add("high", "cause_specific_cox_not_estimable", "At least one cause-specific Cox coefficient or uncertainty estimate is non-finite.", if (ko) "비추정 원인별 Cox 계수를 보고하지 말고 희소성, 분리 및 모형 명세를 검토하세요." else "Do not report non-estimable cause-specific Cox coefficients; review sparsity, separation, and model specification.")
    if (is.list(result$cause_specific)) {
      cs_model_tests <- result$cause_specific$model_tests
      if (is.data.frame(cs_model_tests) && nrow(cs_model_tests) && any(!is.finite(cs_model_tests$Statistic) | !is.finite(cs_model_tests$df) | !is.finite(cs_model_tests$p))) add("high", "cause_specific_model_test_not_estimable", "At least one cause-specific Cox omnibus test is non-finite.", if (ko) "원인별 Cox 전체 모형 검정이 비추정이므로 해당 통계량을 보고하지 말고 희소성·분리·모형 식별을 검토하세요." else "Do not report non-finite cause-specific Cox omnibus tests; review sparsity, separation, and model identification.")
      cs_joint <- result$cause_specific$categorical_joint_tests
      if (is.data.frame(cs_joint) && nrow(cs_joint) && any(!cs_joint$Estimable)) add("high", "cause_specific_categorical_joint_test_not_estimable", paste("Non-estimable cause-specific categorical joint tests:", paste(cs_joint$Variable[!cs_joint$Estimable], collapse = ", ")), if (ko) "원인별 Cox 범주형 변수의 희소 수준·분리·특이 공분산행렬을 검토하고 해당 전체 효과 검정을 보고하지 마세요." else "Review sparse levels, separation, or a singular covariance matrix and do not report the affected cause-specific categorical joint test.")
      cs_ph <- result$cause_specific$ph_table
      if (is.data.frame(cs_ph) && "p" %in% names(cs_ph) && any(is.finite(cs_ph$p) & cs_ph$p < .05)) add("review", "cause_specific_ph_signal", paste("Cause-specific Schoenfeld review signals:", paste(cs_ph$Term[is.finite(cs_ph$p) & cs_ph$p < .05], collapse = ", ")), if (ko) "Schoenfeld 잔차도와 사전 지정 시간상호작용을 검토하세요. 작은 p값만으로 비례위험 가정을 자동 기각하지 마세요." else "Review Schoenfeld residual plots and prespecified time interactions. Do not reject proportional hazards automatically from a small p-value alone.")
      cs_influence <- result$cause_specific$influence_table
      if (is.data.frame(cs_influence) && "Review signal" %in% names(cs_influence) && any(cs_influence$`Review signal` %in% TRUE)) add("review", "cause_specific_dfbetas_signal", sprintf("Cause-specific influential coefficient rows = %d", sum(cs_influence$`Review signal` %in% TRUE)), if (ko) "경험적 DFBETAS 선별기준을 넘은 관측치를 제외·포함한 민감도 분석에서 검토하세요." else "Review observations exceeding the heuristic DFBETAS threshold in inclusion/exclusion sensitivity analyses.")
      cs_vif <- as.numeric(result$cause_specific$collinearity_table$VIF %||% numeric(0))
      if (any(!is.finite(cs_vif))) add("high", "cause_specific_nonfinite_vif", "At least one cause-specific Cox design-column VIF is not finite.", if (ko) "원인별 Cox 설계행렬의 완전 공선성 또는 비추정 열을 검토하세요." else "Review exact collinearity or non-estimable columns in the cause-specific Cox design matrix.") else if (any(cs_vif >= 10, na.rm = TRUE)) add("high", "cause_specific_high_vif", sprintf("Maximum cause-specific Cox VIF = %s", survival_format_number(max(cs_vif, na.rm = TRUE))), if (ko) "원인별 Cox 계수와 표준오차의 불안정성을 검토하세요." else "Review instability in cause-specific Cox coefficients and standard errors.") else if (any(cs_vif >= 5, na.rm = TRUE)) add("review", "cause_specific_elevated_vif", sprintf("Maximum cause-specific Cox VIF = %s", survival_format_number(max(cs_vif, na.rm = TRUE))), if (ko) "원인별 Cox 공변량 중복성과 모형 명세를 검토하세요." else "Review cause-specific Cox covariate redundancy and model specification.")
      cs_condition <- as.numeric(result$cause_specific$condition_number %||% NA_real_)
      if (is.finite(cs_condition) && cs_condition >= 30) add("review", "cause_specific_high_condition_number", sprintf("Cause-specific Cox design condition number = %s", survival_format_number(cs_condition)), if (ko) "원인별 Cox 설계행렬의 수치적 불안정성을 검토하세요." else "Review numerical instability in the cause-specific Cox design matrix.")
    }
    if (is.list(result$fine_gray) && !isTRUE(result$fine_gray$converged)) add("high", "fine_gray_not_converged", "Fine-Gray convergence flag is false.", if (ko) "Fine–Gray 추정 결과를 보고하지 말고 모형과 자료 희소성을 검토하세요." else "Do not report the Fine-Gray estimates before reviewing the model and data sparsity.")
    if (is.list(result$fine_gray) && !isTRUE(result$fine_gray$estimable)) add("high", "fine_gray_not_estimable", "At least one Fine-Gray coefficient or uncertainty estimate is non-finite.", if (ko) "비추정 Fine–Gray 계수를 보고하지 말고 희소성, 분리 및 설계행렬을 검토하세요." else "Do not report non-estimable Fine-Gray coefficients; review sparsity, separation, and the design matrix.")
    if (is.list(result$fine_gray)) {
      fg_model_tests <- result$fine_gray$model_tests
      if (is.data.frame(fg_model_tests) && nrow(fg_model_tests) && any(!is.finite(fg_model_tests$Statistic) | !is.finite(fg_model_tests$df) | !is.finite(fg_model_tests$p))) add("high", "fine_gray_model_test_not_estimable", "The Fine-Gray pseudo likelihood-ratio test is non-finite.", if (ko) "Fine–Gray pseudo likelihood-ratio 검정이 비추정이므로 해당 통계량을 보고하지 말고 수렴·희소성·정보행렬을 검토하세요." else "Do not report a non-finite Fine-Gray pseudo likelihood-ratio test; review convergence, sparsity, and the information matrix.")
      fg_joint <- result$fine_gray$categorical_joint_tests
      if (is.data.frame(fg_joint) && nrow(fg_joint) && any(!fg_joint$Estimable)) add("high", "fine_gray_categorical_joint_test_not_estimable", paste("Non-estimable Fine-Gray categorical joint tests:", paste(fg_joint$Variable[!fg_joint$Estimable], collapse = ", ")), if (ko) "Fine–Gray 범주형 변수의 희소 수준·분리·특이 공분산행렬을 검토하고 해당 전체 효과 검정을 보고하지 마세요." else "Review sparse levels, separation, or a singular covariance matrix and do not report the affected Fine-Gray categorical joint test.")
      fg_vif <- as.numeric(result$fine_gray$collinearity_table$VIF %||% numeric(0))
      if (any(!is.finite(fg_vif))) add("high", "fine_gray_nonfinite_vif", "At least one Fine-Gray design-column VIF is not finite.", if (ko) "Fine–Gray 설계행렬의 완전 공선성 또는 비추정 열을 검토하세요." else "Review exact collinearity or non-estimable columns in the Fine-Gray design matrix.") else if (any(fg_vif >= 10, na.rm = TRUE)) add("high", "fine_gray_high_vif", sprintf("Maximum Fine-Gray VIF = %s", survival_format_number(max(fg_vif, na.rm = TRUE))), if (ko) "Fine–Gray 계수와 표준오차의 불안정성을 검토하세요." else "Review instability in Fine-Gray coefficients and standard errors.") else if (any(fg_vif >= 5, na.rm = TRUE)) add("review", "fine_gray_elevated_vif", sprintf("Maximum Fine-Gray VIF = %s", survival_format_number(max(fg_vif, na.rm = TRUE))), if (ko) "Fine–Gray 공변량 중복성과 모형 명세를 검토하세요." else "Review Fine-Gray covariate redundancy and model specification.")
      fg_condition <- as.numeric(result$fine_gray$condition_number %||% NA_real_)
      if (is.finite(fg_condition) && fg_condition >= 30) add("review", "fine_gray_high_condition_number", sprintf("Fine-Gray design condition number = %s", survival_format_number(fg_condition)), if (ko) "Fine–Gray 설계행렬의 수치적 불안정성을 검토하세요." else "Review numerical instability in the Fine-Gray design matrix.")
      fg_optimizer <- result$fine_gray$optimizer_table
      if (is.data.frame(fg_optimizer) && nrow(fg_optimizer)) {
        relative_score <- as.numeric(fg_optimizer$`Relative score criterion`[[1]])
        tolerance <- as.numeric(fg_optimizer$`Convergence tolerance`[[1]])
        information_rank <- as.integer(fg_optimizer$`Information rank`[[1]])
        parameters <- as.integer(fg_optimizer$Parameters[[1]])
        information_condition <- as.numeric(fg_optimizer$`Standardized information condition number`[[1]])
        if (is.finite(relative_score) && is.finite(tolerance) && relative_score >= tolerance) add("high", "fine_gray_score_criterion_not_met", sprintf("Fine-Gray relative score criterion = %s; tolerance = %s", survival_format_number(relative_score), survival_format_number(tolerance)), if (ko) "최적화 score 기준을 충족하지 못했으므로 추정값을 보고하지 말고 모형과 자료를 검토하세요." else "Do not report the estimates before reviewing the model and data because the optimizer score criterion was not met.")
        if (is.finite(information_rank) && is.finite(parameters) && information_rank < parameters) add("high", "fine_gray_rank_deficient_information", sprintf("Fine-Gray information rank = %d/%d", information_rank, parameters), if (ko) "정보행렬의 계수 부족과 모형 식별 문제를 검토하세요." else "Review information-matrix rank deficiency and model-identification problems.")
        if (is.finite(information_condition) && information_condition >= 30) add("review", "fine_gray_ill_conditioned_information", sprintf("Standardized Fine-Gray information condition number = %s", survival_format_number(information_condition)), if (ko) "표준화 정보행렬의 불안정성과 계수 민감도를 검토하세요." else "Review standardized information-matrix instability and coefficient sensitivity.")
        if (!isTRUE(fg_optimizer$`Finite covariance matrix`[[1]]) || !isTRUE(fg_optimizer$`Positive standard errors`[[1]])) add("high", "fine_gray_invalid_covariance", "The Fine-Gray covariance matrix is non-finite or has a non-positive diagonal.", if (ko) "유효하지 않은 공분산행렬 또는 표준오차가 있으므로 추론 결과를 보고하지 마세요." else "Do not report inferential results with a non-finite covariance matrix or non-positive standard errors.")
      }
      fg_residual_data <- result$fine_gray$residual_data
      fg_residual_review <- result$fine_gray$residual_review
      unique_times <- if (is.data.frame(fg_residual_data) && nrow(fg_residual_data)) length(unique(fg_residual_data$`Event time`)) else 0L
      if (unique_times < 5L) add("review", "fine_gray_residual_diagnostic_limited", sprintf("Unique target-failure times available for residual review = %d", unique_times), if (ko) "관심 사건시점이 적어 시간패턴 진단력이 제한되므로 비례 부분분포 위험 가정의 충족을 주장하지 마세요." else "With few target-failure times, time-pattern diagnostics have limited power; do not claim that proportional subdistribution hazards has been established.")
      if (is.data.frame(fg_residual_review) && nrow(fg_residual_review) && "Review signal" %in% names(fg_residual_review) && any(fg_residual_review$`Review signal` %in% TRUE)) add("review", "fine_gray_residual_time_pattern", paste("Schoenfeld-like residual time-pattern signals:", paste(fg_residual_review$Term[fg_residual_review$`Review signal` %in% TRUE], collapse = ", ")), if (ko) "잔차 도표와 사전 지정 시간상호작용 모형을 검토하세요. 탐색적 상관 p값만으로 가정을 자동 기각하지 마세요." else "Review the residual plots and a prespecified time-interaction model. Do not reject the assumption automatically from the exploratory correlation p-value alone.")
      censoring_table <- result$censoring_group_table
      if (is.data.frame(censoring_table) && nrow(censoring_table) && any(censoring_table$`Review signal` %in% TRUE)) add("review", "fine_gray_sparse_censoring_stratum", paste("Sparse censoring-distribution strata:", paste(censoring_table$`Censoring stratum`[censoring_table$`Review signal` %in% TRUE], collapse = ", ")), if (ko) "층별 검열분포 추정의 불안정성을 검토하고 층 수·검열 수·연구설계 근거를 함께 보고하세요." else "Review instability in stratum-specific censoring-distribution estimation and report stratum sizes, censoring counts, and the design rationale.")
    }
  } else {
    items <- survival_km_result_items(result)
    for (item in items) {
      fit_table <- summary(item$fit)$table
      if (is.null(dim(fit_table))) fit_table <- t(as.matrix(fit_table))
      events <- as.numeric(fit_table[, "events", drop = TRUE] %||% numeric(0))
      strata <- rownames(fit_table) %||% rep("All", length(events))
      sparse <- which(is.finite(events) & events < 5)
      for (index in sparse) add("review", "few_events_in_stratum", sprintf("%s: events = %d", strata[[index]], events[[index]]), if (ko) "집단별 곡선과 비교검정의 불확실성을 강조하세요." else "Emphasize uncertainty in stratum curves and comparison tests.")
      crossing <- item$crossing_table
      if (is.data.frame(crossing) && nrow(crossing) && any(crossing$`Review signal` %in% TRUE)) {
        comparisons <- crossing$Comparison[crossing$`Review signal` %in% TRUE]
        add("review", "crossing_survival_curves", paste("Crossing survival curves:", paste(comparisons, collapse = "; ")), if (ko) "교차 곡선에서는 단일 log-rank p값만으로 차이를 요약하지 말고 사전 지정한 RMST 또는 시간대별 효과와 함께 검토하세요." else "For crossing curves, do not summarize the comparison with a single log-rank p-value alone; review a prespecified RMST or time-specific effects as well.")
      }
    }
  }
  followup <- survival_followup_diagnostics(result)
  if (nrow(followup)) {
    tail_risk <- as.integer(followup$`At risk at 90th percentile time`[[1]])
    if (is.finite(tail_risk) && tail_risk < 10L) add("review", "sparse_tail_risk_set", sprintf("At risk near follow-up tail = %d", tail_risk), if (ko) "말단 시점의 곡선·CIF·위험 추정은 불안정할 수 있으므로 위험집단 수와 신뢰구간을 함께 보고하세요." else "Tail curve, CIF, or hazard estimates may be unstable; report risk-set sizes and confidence intervals.")
    censor_rate <- as.numeric(followup$`Censoring proportion`[[1]])
    if (is.finite(censor_rate) && censor_rate > .8) add("review", "high_censoring_proportion", sprintf("Censoring proportion = %s", survival_format_number(censor_rate)), if (ko) "높은 검열 비율에서 추적관찰 과정과 검열 독립성 가정을 검토하세요." else "Review follow-up processes and the independent-censoring assumption under high censoring.")
  }
  if (!length(rows)) return(data.frame(Level = "info", Code = "no_major_stability_signal", Evidence = if (ko) "선별 규칙에서 주요 신호 없음" else "No major signal from the screening rules", Guidance = if (ko) "위험집단 수, 신뢰구간 및 연구 맥락을 계속 검토하세요." else "Continue reviewing risk-set sizes, confidence intervals, and study context.", stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

survival_reporting_checklist <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  reported <- if (ko) "보고됨" else "Reported"
  review <- if (ko) "검토 필요" else "Review"
  origin_ok <- nzchar(as.character(result$time_origin %||% ""))
  unit_ok <- nzchar(as.character(result$time_unit %||% ""))
  map <- survival_reporting_event_map(result)
  map_ok <- nrow(map) > 0 && !any(map$Role %in% c("unknown", "other_state"))
  rows <- data.frame(
    Item = if (ko) c("시간 원점", "시간 단위", "사건 코드 정의", "분석 대상과 제외", "효과 추정치와 95% 신뢰구간", "소프트웨어 버전") else c("Time origin", "Time unit", "Event-code definitions", "Analysis sample and exclusions", "Effect estimates and 95% CI", "Software version"),
    Status = c(if (origin_ok) reported else review, if (unit_ok) reported else review, if (map_ok) reported else review, reported, reported, reported),
    Evidence = c(result$time_origin %||% "", result$time_unit %||% "", if (map_ok) paste(map$`Raw value`, map$Role, sep = "=", collapse = "; ") else if (ko) "미확인 역할 확인" else "Check unresolved roles", sprintf("N=%s; excluded=%s", result$n %||% NA, sum(survival_reporting_exclusions(result)$N %||% 0)), if (identical(result$type, "cox")) "HR" else if (identical(result$type, "competing_risk")) "CIF / HR / sHR" else "Survival / RMST", result$packages %||% ""),
    stringsAsFactors = FALSE
  )
  if (identical(result$type, "cox")) {
    ph_review <- is.data.frame(result$ph_table) && "p" %in% names(result$ph_table) && any(is.finite(result$ph_table$p) & result$ph_table$p < .05)
    rows <- rbind(rows, data.frame(Item = if (ko) "비례위험 진단" else "Proportional-hazards diagnostics", Status = review, Evidence = if (ph_review) if (ko) "시간가변 효과 신호 있음" else "Possible time-varying effect signal" else if (ko) "Schoenfeld 결과를 맥락과 함께 검토" else "Review Schoenfeld results in context"))
  }
  if (identical(result$type, "competing_risk")) rows <- rbind(rows, data.frame(Item = if (ko) "경쟁위험 목표량" else "Competing-risk estimand", Status = review, Evidence = if (ko) "HR과 sHR을 구분하여 해석" else "Interpret HR and sHR as distinct estimands"))
  if (identical(result$type, "competing_risk") && is.list(result$fine_gray)) rows <- rbind(rows, data.frame(Item = if (ko) "Fine–Gray 검열분포" else "Fine-Gray censoring distribution", Status = review, Evidence = if (nzchar(as.character(result$censoring_group %||% ""))) paste("Separate censoring distributions within", result$censoring_group) else if (ko) "전체 분석표본에서 하나의 검열분포 추정; 독립검열 가정 검토" else "One pooled censoring distribution; review the independent-censoring assumption"))
  rows
}

survival_interpretation_guide <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  if (identical(result$type, "cox")) {
    topics <- if (ko) c("HR", "비례위험", "인과 해석") else c("HR", "Proportional hazards", "Causal interpretation")
    guidance <- if (ko) c("HR은 순간 위험의 상대적 비율이며 누적위험이나 확률비가 아닙니다.", "Schoenfeld 검정은 검토 신호이며 자동 합격·불합격 기준이 아닙니다.", "관찰자료의 보정 HR을 처치의 인과효과로 자동 해석하지 않습니다.") else c("An HR is a relative instantaneous hazard, not a cumulative risk or probability ratio.", "Schoenfeld tests are review signals, not automatic pass/fail rules.", "An adjusted HR from observational data is not automatically a causal treatment effect.")
  } else if (identical(result$type, "competing_risk")) {
    topics <- if (ko) c("CIF", "원인별 HR", "sHR") else c("CIF", "Cause-specific HR", "sHR")
    guidance <- if (ko) c("CIF는 경쟁사건이 존재할 때 관심 사건의 실제 누적발생을 추정합니다.", "원인별 HR은 아직 어떤 사건도 경험하지 않은 대상자의 순간 위험을 비교합니다.", "sHR은 CIF와 연결된 부분분포 위험비이며 개인 위험비로 해석하지 않습니다.") else c("The CIF estimates actual cumulative incidence in the presence of competing events.", "A cause-specific HR compares instantaneous hazards among those still event-free.", "An sHR is a subdistribution hazard ratio linked to the CIF, not an individual risk ratio.")
  } else {
    topics <- if (ko) c("생존확률", "Log-rank", "RMST") else c("Survival probability", "Log-rank", "RMST")
    guidance <- if (ko) c("생존확률은 지정 시점까지 관심 사건이 발생하지 않을 확률입니다.", "Log-rank 검정은 곡선 차이를 검정하지만 차이의 크기를 나타내지 않습니다.", "RMST 차이는 지정한 tau까지의 평균 사건 없는 시간 차이입니다.") else c("Survival probability is the probability of remaining free of the event through a specified time.", "The log-rank test assesses curve differences but is not an effect-size estimate.", "An RMST difference is the difference in mean event-free time through the prespecified tau.")
  }
  data.frame(Topic = topics, Guidance = guidance, stringsAsFactors = FALSE)
}

survival_reporting_guidance_panel <- function(result, language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  div(class = "result-section regression-result-panel survival-result-panel",
    h3(if (ko) "보고 체크리스트와 해석 가이드" else "Reporting checklist and interpretation guide"),
    h4(if (ko) "분석 안정성 검토" else "Analysis stability review"),
    survival_simple_table(survival_stability_review(result, language)),
    div(class = "result-note", if (ko) "임계값은 선별용 경험 규칙이며 분석 품질의 자동 합격·불합격 기준이 아닙니다." else "Thresholds are heuristic screening rules, not automatic analysis-quality pass/fail criteria."),
    h4(if (ko) "검열 및 추적관찰 진단" else "Censoring and follow-up diagnostics"),
    survival_simple_table(survival_followup_diagnostics(result)),
    div(class = "result-note", if (ko) "잠재 추적기간 중앙값은 검열을 사건으로 둔 역 Kaplan–Meier 방법으로 추정합니다. 높은 검열률 자체는 편향의 증거가 아니며 검열기전의 타당성을 별도로 검토해야 합니다." else "Median potential follow-up uses reverse Kaplan–Meier with censoring as the event. A high censoring proportion is not itself evidence of bias; the censoring mechanism requires separate review."),
    survival_simple_table(survival_reporting_checklist(result, language)),
    h4(if (ko) "해석 가이드" else "Interpretation guide"),
    survival_simple_table(survival_interpretation_guide(result, language)),
    div(class = "result-note", if (ko) "체크리스트는 보고 누락을 줄이기 위한 보조도구이며 연구 설계의 타당성이나 인과성을 자동 판정하지 않습니다." else "This checklist helps prevent reporting omissions; it does not automatically establish design validity or causality.")
  )
}

survival_ph_guide_title <- function(table_number = 2L, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (identical(language, "ko")) {
    paste0("표 ", table_number, " 가이드: 비례위험 가정 진단")
  } else {
    paste0("Guide for Table ", table_number, ": proportional hazards assumption")
  }
}

survival_ph_guide_note <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  text <- if (identical(language, "ko")) {
    "비례위험 가정은 Schoenfeld 잔차로 평가합니다. 작은 p값은 시간에 따라 효과가 변할 가능성을 시사합니다."
  } else {
    "The proportional hazards assumption is assessed with Schoenfeld residuals. A small p-value suggests time-varying effects."
  }
  div(text, class = "result-note")
}

survival_km_test_supplement_title <- function(language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (identical(language, "ko")) "보조표: 집단 비교 검정" else "Supplementary table: group comparison test"
}

survival_km_overview_table <- function(result) {
  group_variables <- if (identical(result$type, "km_multi")) {
    labels <- vapply(result$analyses %||% list(), survival_km_group_label, character(1))
    paste(labels[nzchar(labels)], collapse = ", ")
  } else {
    survival_km_group_label(result)
  }
  group_variables <- if (nzchar(group_variables) && !identical(group_variables, "All")) group_variables else "None"
  data.frame(
    Item = c("Method", "Time origin", "Time unit", "Entry variable", "Time variable", "Event variable", "Event value", "Group variables", "N", "Events", "Censored", "Package"),
    Value = c(
      if (identical(result$analysis_method, "life_table")) "Life table" else "Kaplan-Meier",
      result$time_origin %||% "",
      result$time_unit %||% "",
      result$entry %||% "None",
      result$time %||% "",
      result$event %||% "",
      result$event_value %||% "",
      group_variables,
      result$n,
      result$events,
      result$censored,
      result$packages
    ),
    stringsAsFactors = FALSE
  )
}

survival_km_data_flow_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    preflight <- item$preflight
    if (!is.list(preflight) || !is.list(preflight$counts)) return(NULL)
    counts <- preflight$counts
    data.frame(
      `Group variable` = survival_km_group_label(item),
      `Source rows` = as.integer(counts$source_rows %||% NA_integer_),
      `Analysis rows` = as.integer(counts$analysis_rows %||% NA_integer_),
      Excluded = as.integer((counts$source_rows %||% 0L) - (counts$analysis_rows %||% 0L)),
      Events = as.integer(counts$events %||% 0L),
      Censored = as.integer(counts$censored %||% 0L),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_exclusion_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    audit <- item$preflight$row_audit
    if (!is.data.frame(audit) || nrow(audit) == 0 || !"exclusion_reasons" %in% names(audit)) return(NULL)
    reasons <- unlist(strsplit(audit$exclusion_reasons[!audit$included & nzchar(audit$exclusion_reasons)], ";", fixed = TRUE))
    reasons <- reasons[nzchar(reasons)]
    if (length(reasons) == 0) return(NULL)
    counts <- sort(table(reasons), decreasing = TRUE)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      `Exclusion reason` = names(counts),
      N = as.integer(counts),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_issue_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    issues <- item$preflight$issues
    if (!is.data.frame(issues) || nrow(issues) == 0) return(NULL)
    issues <- issues[issues$severity %in% c("warning", "info"), , drop = FALSE]
    if (nrow(issues) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      Severity = issues$severity,
      Code = issues$code,
      Message = issues$message,
      Action = issues$action,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_group_label <- function(result) {
  group <- as.character(result$group %||% "")
  if (nzchar(group)) group else "All"
}

survival_km_level_label <- function(value, group = "") {
  value <- trimws(as.character(value %||% ""))
  group <- trimws(as.character(group %||% ""))
  if (!nzchar(value) || !nzchar(group) || identical(group, "All")) {
    return(value)
  }
  prefix <- paste0(group, "=")
  if (startsWith(value, prefix)) {
    return(substr(value, nchar(prefix) + 1L, nchar(value)))
  }
  value
}

survival_median_ci_text <- function(median, lower, upper) {
  median_num <- suppressWarnings(as.numeric(median))
  median_text <- if (length(median_num) > 0 && is.finite(median_num)) survival_format_number(median_num) else "NR"
  ci_text <- survival_format_ci(lower, upper)
  if (!nzchar(ci_text)) {
    return(median_text)
  }
  if (identical(median_text, "NR") && identical(ci_text, "(NE-NE)")) {
    return("NR")
  }
  paste0(median_text, "\n", ci_text)
}

survival_km_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  row_offset <- 0L
  marker_rows <- list()
  rows <- lapply(items, function(item) {
    table <- item$median_table
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    group_label <- survival_km_group_label(item)
    item_rows <- data.frame(
      Variables = group_label,
      Level = vapply(table$Strata, survival_km_level_label, character(1), group = group_label),
      N = table$Records,
      Events = table$Events,
      Censored = table$Records - table$Events,
      MeanSE = sprintf("%s \u00B1 %s", vapply(table$Mean, survival_format_number, character(1)), vapply(table$`Mean SE`, survival_format_number, character(1))),
      Q1 = vapply(table$Q1, survival_format_number, character(1)),
      `Median (95% CI)` = mapply(function(median, lower, upper) {
        survival_median_ci_text(median, lower, upper)
      }, table$Median, table$`Median 95% LCL`, table$`Median 95% UCL`),
      Q3 = vapply(table$Q3, survival_format_number, character(1)),
      `Chi-square (df)` = "",
      p = "",
      `post-hoc` = "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(item_rows)[names(item_rows) == "MeanSE"] <- "M \u00B1 SE"
    logrank <- item$logrank
    if (!is.null(logrank) && nrow(item_rows) > 0) {
      item_rows$`Chi-square (df)`[[1]] <- sprintf("%s (%s)", survival_format_number(logrank$chisq), as.character(logrank$df))
      item_rows$p[[1]] <- survival_p(logrank$p)
    }
    item_rows <- survival_km_apply_ordered_posthoc(item_rows, item)
    if (nrow(item_rows) > 1L) {
      item_rows$Variables[-1L] <- ""
      item_rows$`Chi-square (df)`[-1L] <- ""
      item_rows$p[-1L] <- ""
    }
    markers <- attr(item_rows, "note_markers", exact = TRUE)
    if (is.data.frame(markers) && nrow(markers) > 0) {
      markers$row <- as.integer(markers$row) + row_offset
      marker_rows[[length(marker_rows) + 1L]] <<- markers
    }
    row_offset <<- row_offset + nrow(item_rows)
    item_rows
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  rows_without_attrs <- lapply(rows, function(row) {
    attr(row, "note_markers") <- NULL
    row
  })
  out <- do.call(rbind, rows_without_attrs)
  if (length(marker_rows) > 0) {
    attr(out, "note_markers") <- do.call(rbind, marker_rows)
  }
  out
}

survival_km_posthoc_p_matrix <- function(item) {
  posthoc <- item$posthoc
  group <- survival_km_group_label(item)
  strata <- vapply(as.character(item$median_table$Strata %||% character(0)), survival_km_level_label, character(1), group = group)
  strata <- strata[nzchar(strata)]
  if (!is.data.frame(posthoc) || nrow(posthoc) == 0 || length(strata) < 2L) {
    return(NULL)
  }
  p_matrix <- matrix(NA_real_, nrow = length(strata), ncol = length(strata), dimnames = list(strata, strata))
  diag(p_matrix) <- 1
  display_level <- function(level) {
    level <- trimws(as.character(level %||% ""))
    if (!nzchar(level)) return(level)
    survival_km_level_label(level, group)
  }
  has_direct_groups <- all(c("Group1", "Group2") %in% names(posthoc))
  for (row_index in seq_len(nrow(posthoc))) {
    first <- if (has_direct_groups) display_level(posthoc$Group1[[row_index]]) else character(0)
    second <- if (has_direct_groups) display_level(posthoc$Group2[[row_index]]) else character(0)
    if (!nzchar(first) || !nzchar(second)) {
      parts <- trimws(strsplit(as.character(posthoc$Comparison[[row_index]] %||% ""), "\\s+vs\\s+", perl = TRUE)[[1]])
      if (length(parts) != 2L) next
      first <- display_level(parts[[1]])
      second <- display_level(parts[[2]])
    }
    if (!all(c(first, second) %in% strata)) next
    p_value <- suppressWarnings(as.numeric(posthoc$p_adjusted[[row_index]] %||% posthoc$p[[row_index]]))
    if (!is.finite(p_value)) next
    p_matrix[first, second] <- p_value
    p_matrix[second, first] <- p_value
  }
  p_matrix
}

survival_km_apply_ordered_posthoc <- function(rows, item, alpha = .05) {
  if (!is.data.frame(rows) || nrow(rows) == 0 || !"post-hoc" %in% names(rows)) {
    return(rows)
  }
  p_matrix <- survival_km_posthoc_p_matrix(item)
  if (is.null(p_matrix)) return(rows)
  estimates <- suppressWarnings(as.numeric(item$median_table$Median))
  fallback <- suppressWarnings(as.numeric(item$median_table$Mean))
  estimates[!is.finite(estimates)] <- fallback[!is.finite(estimates)]
  if (exists("analysis_apply_ordered_posthoc_markers", mode = "function")) {
    return(analysis_apply_ordered_posthoc_markers(
      rows,
      estimates = estimates,
      levels = rows$Level,
      p_matrix = p_matrix,
      label_column = "Level",
      alpha = alpha
    ))
  }
  rows
}

survival_km_test_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    logrank <- item$logrank
    if (is.null(logrank)) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      Test = logrank$label %||% item$test_label %||% "Log-rank",
      Chisq = survival_format_number(logrank$chisq),
      df = logrank$df,
      p = survival_p(logrank$p),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_posthoc_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- survival_km_posthoc_display_table(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_rate_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- survival_km_rate_table(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_number_at_risk_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    summary_at <- item$summary_at
    if (is.null(summary_at) || length(summary_at$time) == 0) return(NULL)
    strata <- as.character(summary_at$strata %||% "")
    if (length(strata) == 0) strata <- rep("All", length(summary_at$time))
    group_label <- survival_km_group_label(item)
    data.frame(
      `Group variable` = group_label,
      Level = vapply(strata, survival_km_level_label, character(1), group = group_label),
      Time = as.numeric(summary_at$time),
      `Number at risk` = as.integer(summary_at$n.risk),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_rmst_estimate_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    estimates <- item$rmst$estimates
    if (!is.data.frame(estimates) || nrow(estimates) == 0) return(NULL)
    group_label <- survival_km_group_label(item)
    data.frame(
      `Group variable` = group_label,
      Level = vapply(estimates$Strata, survival_km_level_label, character(1), group = group_label),
      Tau = vapply(estimates$Tau, survival_format_number, character(1)),
      RMST = vapply(estimates$RMST, survival_format_number, character(1)),
      SE = vapply(estimates$SE, survival_format_number, character(1)),
      `95% CI` = mapply(survival_format_ci, estimates$LLCI, estimates$ULCI),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_km_rmst_contrast_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    contrasts <- item$rmst$contrasts
    if (!is.data.frame(contrasts) || nrow(contrasts) == 0) return(NULL)
    group_label <- survival_km_group_label(item)
    comparison <- as.character(contrasts$Comparison)
    if (nzchar(group_label) && !identical(group_label, "All")) comparison <- gsub(paste0(group_label, "="), "", comparison, fixed = TRUE)
    data.frame(
      `Group variable` = group_label,
      Comparison = comparison,
      Tau = vapply(contrasts$Tau, survival_format_number, character(1)),
      `RMST difference` = vapply(contrasts$Difference, survival_format_number, character(1)),
      `Difference 95% CI` = mapply(survival_format_ci, contrasts$LLCI, contrasts$ULCI),
      `RMST ratio` = vapply(contrasts$Ratio, survival_format_number, character(1)),
      `Ratio 95% CI` = mapply(survival_format_ci, contrasts$RatioLLCI, contrasts$RatioULCI),
      p = vapply(contrasts$p, survival_p, character(1)),
      `p adjusted` = vapply(contrasts$p_adjusted, survival_p, character(1)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_life_summary_table <- function(result) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- survival_life_table_display(item)
    if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
    data.frame(
      `Group variable` = survival_km_group_label(item),
      table,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

survival_logrank_table <- function(result) {
  logrank <- result$logrank
  if (is.null(logrank)) return(data.frame())
  data.frame(
    Test = logrank$label %||% result$test_label %||% "Log-rank",
    Chisq = survival_format_number(logrank$chisq),
    df = logrank$df,
    p = survival_p(logrank$p),
    stringsAsFactors = FALSE
  )
}

survival_km_rate_table <- function(result) {
  summary_at <- result$summary_at
  if (is.null(summary_at)) return(data.frame())
  strata <- as.character(summary_at$strata %||% "")
  if (length(strata) == 0) strata <- rep("All", length(summary_at$time))
  data.frame(
    Strata = strata,
    Time = summary_at$time,
    `Number at risk` = summary_at$n.risk,
    Survival = survival_format_number(summary_at$surv),
    `95% CI` = mapply(survival_format_ci, summary_at$lower, summary_at$upper),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_life_table_display <- function(result) {
  table <- result$life_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  for (name in names(table)) {
    if (is.numeric(table[[name]])) {
      table[[name]] <- vapply(table[[name]], survival_format_number, character(1))
    }
  }
  table
}

survival_km_posthoc_display_table <- function(result) {
  table <- result$posthoc
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Comparison = table$Comparison,
    Chisq = vapply(table$Chisq, survival_format_number, character(1)),
    df = table$df,
    p = vapply(table$p, survival_p, character(1)),
    `p adjusted` = vapply(table$p_adjusted, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_km_crossing_summary_table <- function(result, formatted = TRUE) {
  items <- survival_km_result_items(result)
  rows <- lapply(items, function(item) {
    table <- item$crossing_table
    if (!is.data.frame(table) || !nrow(table)) return(NULL)
    if (isTRUE(formatted)) {
      table$`First crossing time` <- vapply(table$`First crossing time`, survival_format_number, character(1))
      table$`Common follow-up end` <- vapply(table$`Common follow-up end`, survival_format_number, character(1))
      table$Review <- ifelse(table$`Review signal` %in% TRUE, "Crossing detected", "No crossing detected")
      table$`Review signal` <- NULL
    }
    if (identical(result$type, "km_multi")) {
      table <- data.frame(`Group variable` = survival_km_group_label(item), table, check.names = FALSE, stringsAsFactors = FALSE)
    }
    table
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

survival_km_crossing_note <- function(language = statedu_initial_language()) {
  ko <- identical(normalize_app_language(language), "ko")
  survival_table_note(if (ko) {
    "이 표는 Kaplan–Meier 계단함수의 상대적 순서가 바뀌는지를 기술적으로 선별합니다. 곡선 교차가 있으면 단일 log-rank p값만으로 차이를 요약하지 말고 사전 지정한 RMST 또는 시간대별 효과를 함께 검토해야 합니다. 가장 작은 p값을 얻기 위해 가중 검정을 사후 선택하지 마세요."
  } else {
    "This descriptive screen detects changes in the relative ordering of Kaplan–Meier step functions. When curves cross, do not summarize the comparison with a single log-rank p-value alone; also review a prespecified RMST or time-specific effects. Do not select a weighted test post hoc to obtain the smallest p-value."
  })
}

survival_plot_type_label <- function(type, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  switch(as.character(type %||% "survival")[[1]],
    event = survival_ui_text("1 - Survival function", language),
    cumhaz = survival_ui_text("Cumulative hazard", language),
    log_survival = survival_ui_text("Log survival", language),
    survival_ui_text("Survival function", language)
  )
}

survival_plot_version_label <- function(version, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  switch(as.character(version %||% "color")[[1]],
    bw = survival_ui_text("Black and white", language),
    survival_ui_text("Color", language)
  )
}

survival_km_result_items <- function(result) {
  if (identical(result$type, "km_multi")) {
    return(result$analyses %||% list())
  }
  list(result)
}

survival_km_result_panel <- function(result, plot_output_ids = "survival_km_plot", title = NULL, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  output_tables <- as.character(result$output_tables %||% c("survival_table", "survival_time"))
  plot_types <- as.character(result$plot_types %||% "survival")
  plot_versions <- as.character(result$plot_versions %||% "color")
  plot_specs <- expand.grid(
    plot_type = plot_types,
    plot_version = plot_versions,
    stringsAsFactors = FALSE
  )
  if (length(plot_output_ids) < nrow(plot_specs)) {
    plot_output_ids <- paste0("survival_km_plot_", seq_len(nrow(plot_specs)))
  }
  data_flow <- survival_km_data_flow_table(result)
  exclusions <- survival_km_exclusion_table(result)
  issues <- survival_km_issue_table(result)
  risk_table <- survival_km_number_at_risk_table(result)
  rmst_estimates <- survival_km_rmst_estimate_table(result)
  rmst_contrasts <- survival_km_rmst_contrast_table(result)
  crossing_summary <- survival_km_crossing_summary_table(result)
  div(
    class = "survival-km-result-block",
    if (nzchar(as.character(title %||% ""))) h2(title),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("1. Analysis overview"),
      survival_simple_table(survival_km_overview_table(result)),
      div(class = "result-note", survival_method_sentence(result, language)),
      h4("Event-code mapping"), survival_simple_table(survival_reporting_event_map(result)),
      if (nrow(data_flow) > 0) tagList(h4("Data inclusion audit"), survival_simple_table(data_flow)),
      if (nrow(exclusions) > 0) tagList(h4("Excluded rows by reason"), survival_simple_table(exclusions)),
      if (nrow(issues) > 0) tagList(h4("Data review messages"), survival_simple_table(issues))
    ),
    if ("survival_time" %in% output_tables) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3("2. Kaplan-Meier survival time summary"),
        survival_simple_table(survival_km_summary_table(result)),
        survival_km_summary_note(),
        if (nrow(rmst_estimates) > 0) tagList(h4("Restricted mean survival time (RMST)"), survival_simple_table(rmst_estimates)),
        if (nrow(rmst_contrasts) > 0) tagList(h4("RMST contrasts"), survival_simple_table(rmst_contrasts), survival_table_note("Difference is second level minus first level; tau is the prespecified restriction time."))
      )
    },
    if ("survival_table" %in% output_tables) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3("3. Survival probabilities at selected time points"),
        survival_simple_table(survival_km_rate_table(result)),
        survival_rate_note(),
        if (nrow(risk_table) > 0) tagList(h4("Number at risk"), survival_simple_table(risk_table))
      )
    },
    if (identical(result$analysis_method, "life_table") && "survival_table" %in% output_tables) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3("4. Life table"),
        survival_simple_table(survival_life_table_display(result))
      )
    },
    if (!is.null(result$logrank)) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_km_test_supplement_title(language)),
        survival_simple_table(survival_logrank_table(result)),
        if (nrow(crossing_summary) > 0) tagList(
          h4(if (identical(language, "ko")) "생존곡선 교차 선별" else "Survival-curve crossing screen"),
          survival_simple_table(crossing_summary),
          survival_km_crossing_note(language)
        )
      )
    },
    if (is.data.frame(result$posthoc) && nrow(result$posthoc) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(survival_ui_text("Post-hoc pairwise comparison", language)),
        survival_simple_table(survival_km_posthoc_display_table(result))
      )
    },
    survival_reporting_guidance_panel(result, language),
    lapply(seq_len(nrow(plot_specs)), function(index) {
      plot_type <- plot_specs$plot_type[[index]]
      plot_version <- plot_specs$plot_version[[index]]
      div(class = "result-section regression-result-panel survival-result-panel",
        h3(paste(survival_plot_type_label(plot_type, language), survival_plot_version_label(plot_version, language), sep = " - ")),
        plotOutput(plot_output_ids[[index]], height = "620px")
      )
    })
  )
}

survival_km_results_panel <- function(result, plot_output_ids = "survival_km_plot", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  items <- survival_km_result_items(result)
  if (length(items) == 0) return(NULL)
  if (length(plot_output_ids) < length(items)) {
    plot_output_ids <- paste0("survival_km_plot_", seq_along(items))
  }
  output_tables <- as.character(result$output_tables %||% c("survival_table", "survival_time"))
  km_summary <- survival_km_summary_table(result)
  rate_summary <- survival_km_rate_summary_table(result)
  risk_summary <- survival_km_number_at_risk_table(result)
  life_summary <- survival_life_summary_table(result)
  data_flow <- survival_km_data_flow_table(result)
  exclusions <- survival_km_exclusion_table(result)
  issues <- survival_km_issue_table(result)
  rmst_estimates <- survival_km_rmst_estimate_table(result)
  rmst_contrasts <- survival_km_rmst_contrast_table(result)
  crossing_summary <- survival_km_crossing_summary_table(result)
  test_summary <- survival_km_test_summary_table(result)
  posthoc_summary <- survival_km_posthoc_summary_table(result)
  div(
    class = "survival-results",
    div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
      h3("1. Analysis overview"),
      survival_simple_table(survival_km_overview_table(result)),
      div(class = "result-note", survival_method_sentence(result, language)),
      h4("Event-code mapping"), survival_simple_table(survival_reporting_event_map(result)),
      if (nrow(data_flow) > 0) tagList(h4("Data inclusion audit"), survival_simple_table(data_flow)),
      if (nrow(exclusions) > 0) tagList(h4("Excluded rows by reason"), survival_simple_table(exclusions)),
      if (nrow(issues) > 0) tagList(h4("Data review messages"), survival_simple_table(issues))
    ),
    div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
      h3("2. Kaplan-Meier survival time summary"),
      survival_simple_table(km_summary),
      survival_km_summary_note(),
      if (nrow(rmst_estimates) > 0) tagList(h4("Restricted mean survival time (RMST)"), survival_simple_table(rmst_estimates)),
      if (nrow(rmst_contrasts) > 0) tagList(h4("RMST contrasts"), survival_simple_table(rmst_contrasts), survival_table_note("Difference is second level minus first level; tau is the prespecified restriction time."))
    ),
    if ("survival_table" %in% output_tables && nrow(rate_summary) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
        h3("3. Survival probabilities at selected time points"),
        survival_simple_table(rate_summary),
        survival_rate_note(),
        if (nrow(risk_summary) > 0) tagList(h4("Number at risk"), survival_simple_table(risk_summary))
      )
    },
    if (identical(result$analysis_method, "life_table") && "survival_table" %in% output_tables && nrow(life_summary) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
        h3("4. Life table"),
        survival_simple_table(life_summary)
      )
    },
    if (nrow(test_summary) > 0) {
      div(class = "result-section regression-result-panel survival-result-panel survival-wide-result-panel",
        h3(survival_km_test_supplement_title(language)),
        survival_simple_table(test_summary),
        if (nrow(crossing_summary) > 0) tagList(
          h4(if (identical(language, "ko")) "생존곡선 교차 선별" else "Survival-curve crossing screen"),
          survival_simple_table(crossing_summary),
          survival_km_crossing_note(language)
        ),
        if (nrow(posthoc_summary) > 0) tagList(
          h4(survival_ui_text("Post-hoc pairwise comparison", language)),
          survival_simple_table(posthoc_summary)
        )
      )
    },
    survival_reporting_guidance_panel(result, language),
    div(class = "survival-plot-results",
      lapply(seq_along(items), function(index) {
        item <- items[[index]]
        item_plot_ids <- plot_output_ids[[index]]
        plot_types <- as.character(item$plot_types %||% "survival")
        plot_versions <- as.character(item$plot_versions %||% "color")
        plot_specs <- expand.grid(
          plot_type = plot_types,
          plot_version = plot_versions,
          stringsAsFactors = FALSE
        )
        if (is.null(item_plot_ids) || length(item_plot_ids) < nrow(plot_specs)) {
          item_plot_ids <- paste0("survival_km_plot_", index, "_", seq_len(nrow(plot_specs)))
        }
        lapply(seq_len(nrow(plot_specs)), function(plot_index) {
          plot_type <- plot_specs$plot_type[[plot_index]]
          plot_version <- plot_specs$plot_version[[plot_index]]
          title <- paste(survival_plot_type_label(plot_type, language), survival_plot_version_label(plot_version, language), sep = " - ")
          if (identical(result$type, "km_multi")) {
            title <- paste0(survival_km_group_label(item), ": ", title)
          }
          div(class = "result-section regression-result-panel survival-result-panel survival-plot-result-panel",
            h3(title),
            plotOutput(item_plot_ids[[plot_index]], height = "650px")
          )
        })
      })
    )
  )
}

survival_cox_overview_table <- function(result) {
  concordance <- result$concordance
  c_value <- if (length(concordance) >= 1L) suppressWarnings(as.numeric(concordance[[1]])) else NA_real_
  c_se <- if (length(concordance) >= 2L) suppressWarnings(as.numeric(concordance[[2]])) else NA_real_
  c_index <- if (is.finite(c_value) && is.finite(c_se)) {
    lower <- max(0, c_value - 1.96 * c_se)
    upper <- min(1, c_value + 1.96 * c_se)
    sprintf("%s %s", survival_format_number(c_value), survival_format_ci(lower, upper))
  } else if (is.finite(c_value)) {
    survival_format_number(c_value)
  } else {
    ""
  }
  model_tests <- result$model_tests
  likelihood <- if (is.data.frame(model_tests) && nrow(model_tests) > 0) {
    model_tests[model_tests$Test == "Likelihood-ratio", , drop = FALSE]
  } else {
    data.frame()
  }
  lr_chisq <- if (nrow(likelihood) > 0) sprintf("%s (%s)", survival_format_number(likelihood$Statistic[[1]]), as.character(likelihood$df[[1]])) else ""
  lr_p <- if (nrow(likelihood) > 0) survival_p(likelihood$p[[1]]) else ""
  data.frame(
    Item = c("Time origin", "Time unit", "Entry variable", "Interval start", "Interval stop", "Subject ID", "Stratification variable", "Cluster variable", "Clusters", "Variance", "Source rows", "Analysis rows", "Excluded rows", "Events", "Parameters", "Events / parameter", "Ties", "LR chi-square (df)", "LR p", "Concordance (95% CI)", "Package"),
    Value = c(
      result$time_origin %||% "",
      result$time_unit %||% "",
      result$entry %||% "None",
      result$start %||% "None",
      result$stop %||% "None",
      result$subject_id %||% "None",
      if (nzchar(as.character(result$strata %||% ""))) result$strata else "None",
      if (nzchar(as.character(result$cluster %||% ""))) result$cluster else "None",
      if (is.data.frame(result$cluster_summary) && nrow(result$cluster_summary)) result$cluster_summary$Clusters[[1]] else "N/A",
      result$variance %||% if (!is.null(result$fit$naive.var)) "Subject-cluster robust" else "Model-based",
      result$preflight$counts$source_rows %||% result$n,
      result$n,
      (result$preflight$counts$source_rows %||% result$n) - result$n,
      result$events,
      result$parameter_count %||% NA_integer_,
      survival_format_number(result$events_per_parameter),
      result$ties %||% "",
      lr_chisq,
      lr_p,
      c_index,
      result$packages
    ),
    stringsAsFactors = FALSE
  )
}

survival_cox_exclusion_table <- function(result) {
  audit <- result$preflight$row_audit
  if (!is.data.frame(audit) || nrow(audit) == 0 || !"exclusion_reasons" %in% names(audit)) return(data.frame())
  reasons <- unlist(strsplit(audit$exclusion_reasons[!audit$included & nzchar(audit$exclusion_reasons)], ";", fixed = TRUE))
  reasons <- reasons[nzchar(reasons)]
  if (length(reasons) == 0) return(data.frame())
  counts <- sort(table(reasons), decreasing = TRUE)
  data.frame(`Exclusion reason` = names(counts), N = as.integer(counts), check.names = FALSE, stringsAsFactors = FALSE)
}

survival_cox_model_test_table <- function(result) {
  table <- result$model_tests
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Test = table$Test,
    Statistic = vapply(table$Statistic, survival_format_number, character(1)),
    df = vapply(table$df, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    stringsAsFactors = FALSE
  )
}

survival_cox_display_parameter_table <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || !nrow(table)) return(table)
  replace_effect <- function(current, rows, variable, label) {
    if (!length(rows)) return(current)
    placeholder <- current[rows[[1]], , drop = FALSE]
    for (name in c("B", "SE", "HR", "LLCI", "ULCI", "z", "p")) placeholder[[name]] <- NA_real_
    placeholder$Term <- variable
    placeholder$DisplayTerm <- label
    placeholder$Variable <- variable
    placeholder$Reference <- FALSE
    if ("SplineBasis" %in% names(placeholder)) placeholder$SplineBasis <- FALSE
    if ("TimeVaryingEffect" %in% names(placeholder)) placeholder$TimeVaryingEffect <- FALSE
    rbind(current[-rows, , drop = FALSE], placeholder)
  }
  spline_rows <- if ("SplineBasis" %in% names(table)) which(table$SplineBasis %in% TRUE) else integer(0)
  table <- replace_effect(table, spline_rows, result$spline_covariate %||% "", paste0(result$spline_covariate %||% "", " (natural cubic spline; see curve)"))
  time_rows <- if ("TimeVaryingEffect" %in% names(table)) which(table$TimeVaryingEffect %in% TRUE) else integer(0)
  replace_effect(table, time_rows, result$time_varying_covariate %||% "", paste0(result$time_varying_covariate %||% "", " (time-varying HR; see curve)"))
}

survival_cox_coef_table <- function(result) {
  table <- survival_cox_display_parameter_table(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  is_reference <- as.logical(table$Reference %||% rep(FALSE, nrow(table)))
  terms <- as.character(table$DisplayTerm %||% table$Term)
  data.frame(
    Term = ifelse(is_reference, paste0(terms, " (reference)"), terms),
    HR = ifelse(is_reference, "1.000", vapply(table$HR, survival_format_number, character(1))),
    `95% CI` = ifelse(is_reference, "reference", mapply(survival_format_ci, table$LLCI, table$ULCI)),
    p = ifelse(is_reference, "", vapply(table$p, survival_p, character(1))),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_statistic_table <- function(result) {
  table <- survival_cox_display_parameter_table(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  is_reference <- as.logical(table$Reference %||% rep(FALSE, nrow(table)))
  terms <- as.character(table$DisplayTerm %||% table$Term)
  data.frame(
    Term = ifelse(is_reference, paste0(terms, " (reference)"), terms),
    B = ifelse(is_reference, "", vapply(table$B, survival_format_number, character(1))),
    SE = ifelse(is_reference, "", vapply(table$SE, survival_format_number, character(1))),
    z = ifelse(is_reference, "", vapply(table$z, survival_format_number, character(1))),
    p = ifelse(is_reference, "", vapply(table$p, survival_p, character(1))),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_table_header_style <- function(first = FALSE, bottom = TRUE, center = FALSE) {
  paste0(
    result_header_cell_style(first, compact = FALSE, compact_width = 56, compact_first_width = 112),
    "width:auto;min-width:0;max-width:none;",
    if (!isTRUE(bottom)) "border-bottom:0;" else "",
    if (isTRUE(center)) "text-align:center;" else ""
  )
}

survival_cox_table_body_style <- function(first = FALSE, last = FALSE, center = FALSE) {
  paste0(
    result_body_cell_style(first, last, compact = FALSE, compact_width = 56, compact_first_width = 112),
    "width:auto;min-width:0;max-width:none;",
    if (isTRUE(center)) "text-align:center;" else ""
  )
}

survival_cox_result_html_table <- function(result, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  table <- survival_cox_display_parameter_table(result)
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  is_reference <- as.logical(table$Reference %||% rep(FALSE, nrow(table)))
  variables <- as.character(table$Variable %||% table$Term)
  display_terms <- as.character(table$DisplayTerm %||% table$Term)
  variables[!nzchar(variables)] <- display_terms[!nzchar(variables)]
  levels <- vapply(seq_len(nrow(table)), function(index) {
    prefix <- paste0(variables[[index]], "=")
    if (startsWith(display_terms[[index]], prefix)) substring(display_terms[[index]], nchar(prefix) + 1L) else ""
  }, character(1))
  variable_labels <- variables
  duplicated_variable <- duplicated(variables)
  variable_labels[duplicated_variable] <- ""
  rows <- lapply(seq_len(nrow(table)), function(index) {
    list(
      variable = variable_labels[[index]],
      level = levels[[index]],
      hr = if (is_reference[[index]]) "1.00" else survival_format_number(table$HR[[index]]),
      llci = if (is_reference[[index]]) "" else survival_format_number(table$LLCI[[index]]),
      ulci = if (is_reference[[index]]) "" else survival_format_number(table$ULCI[[index]]),
      p = if (is_reference[[index]]) "" else survival_p(table$p[[index]])
    )
  })
  headers <- c("", "", "HR", "LLCI", "ULCI", "p")
  widths <- c(22, 18, 15, 15, 15, 15)
  model_tests <- result$model_tests
  if (is.data.frame(model_tests) && "Test" %in% names(model_tests)) {
    model_tests <- model_tests[tolower(model_tests$Test) == "likelihood-ratio", , drop = FALSE]
  }
  model_test_rows <- if (is.data.frame(model_tests) && nrow(model_tests) > 0) {
    lapply(seq_len(nrow(model_tests)), function(index) {
      paste0(
        model_tests$Test[[index]], " χ²(p) = ",
        survival_format_number(model_tests$Statistic[[index]]), " (",
        survival_p(model_tests$p[[index]]), ")"
      )
    })
  } else list()
  ph <- result$ph_table
  ph_rows <- if (is.data.frame(ph) && nrow(ph) > 0) {
    ph_term <- if ("Term" %in% names(ph)) as.character(ph[["Term"]]) else rownames(ph)
    ph_chisq <- if ("chisq" %in% names(ph)) as.numeric(ph[["chisq"]]) else rep(NA_real_, nrow(ph))
    ph_p <- if ("p" %in% names(ph)) as.numeric(ph[["p"]]) else rep(NA_real_, nrow(ph))
    global_index <- which(toupper(ph_term) == "GLOBAL")
    lapply(global_index, function(index) {
      paste0(
        if (nzchar(as.character(result$time_varying_covariate %||% ""))) "PH GLOBAL (companion PH model) χ²(p) = " else paste0("PH ", ph_term[[index]], " χ²(p) = "),
        survival_format_number(ph_chisq[[index]]), " (",
        survival_p(ph_p[[index]]), ")"
      )
    })
  } else list()
  summary_rows <- c(model_test_rows, ph_rows)
  tags$table(
    class = "coefficient-table logistic-result-table survival-cox-result-table output-table-style-standard",
    style = paste0(result_table_style(font_size = 12, min_width = 0), "width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;"),
    tags$colgroup(lapply(widths, function(width) tags$col(style = sprintf("width:%d%%;", width)))),
    tags$thead(
      tags$tr(
        tags$th(style = survival_cox_table_header_style(TRUE, bottom = FALSE), ""),
        tags$th(style = survival_cox_table_header_style(FALSE, bottom = FALSE), ""),
        tags$th(style = survival_cox_table_header_style(FALSE, bottom = FALSE), ""),
        tags$th(class = "coefficient-ci-group-header", style = survival_cox_table_header_style(FALSE, bottom = TRUE, center = TRUE), colspan = 2, span(class = "coefficient-ci-group-label", "95% CI")),
        tags$th(style = survival_cox_table_header_style(FALSE, bottom = FALSE), "")
      ),
      tags$tr(lapply(seq_along(headers), function(index) {
        tags$th(style = survival_cox_table_header_style(index %in% c(1L, 2L), center = index > 2L), headers[[index]])
      }))
    ),
    tags$tbody(
      lapply(seq_along(rows), function(index) {
        values <- unname(unlist(rows[[index]], use.names = FALSE))
        tags$tr(lapply(seq_along(values), function(column) {
          tags$td(
            style = survival_cox_table_body_style(column %in% c(1L, 2L), index == length(rows) && length(summary_rows) == 0L, center = column > 2L),
            values[[column]]
          )
        }))
      }),
      lapply(seq_along(summary_rows), function(index) {
        tags$tr(
          class = paste("survival-cox-summary-row", if (index == 1L) "survival-cox-summary-row-first" else ""),
          tags$td(style = "text-align:center;", colspan = 6, summary_rows[[index]])
        )
      })
    )
  )
}

survival_ph_table <- function(result, language = statedu_initial_language()) {
  table <- result$ph_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  names(table) <- sub("^p$", "p", names(table))
  for (name in names(table)) {
    if (is.numeric(table[[name]])) {
      table[[name]] <- if (identical(name, "p")) {
        vapply(table[[name]], survival_p, character(1))
      } else {
        vapply(table[[name]], survival_format_number, character(1))
      }
    }
  }
  if (identical(normalize_app_language(language), "ko")) {
    names(table)[names(table) == "Term"] <- "항목"
    names(table)[names(table) == "chisq"] <- "Chi-square"
    names(table)[names(table) == "p"] <- "p"
  }
  table
}

survival_ph_review_table <- function(result, language = statedu_initial_language()) {
  table <- result$ph_table
  if (!is.data.frame(table) || nrow(table) == 0 || !"p" %in% names(table)) return(data.frame())
  potential <- is.finite(table$p) & table$p < .05
  status <- if (identical(normalize_app_language(language), "ko")) {
    ifelse(potential, "시간가변 효과 가능성 검토", "이 검정에서 강한 신호 없음")
  } else {
    ifelse(potential, "Review possible time-varying effect", "No strong signal in this test")
  }
  data.frame(
    Term = table$Term,
    `Review status` = status,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

survival_cox_residual_table <- function(result) {
  table <- result$residual_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  for (name in setdiff(names(table), "Type")) table[[name]] <- vapply(table[[name]], survival_format_number, character(1))
  table
}

survival_cox_influence_table <- function(result) {
  table <- result$influence_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = table$Term,
    `Maximum absolute DFBETA` = vapply(table$`Maximum absolute DFBETA`, survival_format_number, character(1)),
    `Maximum absolute DFBETAS` = vapply(table$`Maximum absolute DFBETAS`, survival_format_number, character(1)),
    `Screening threshold` = vapply(table$`Screening threshold`, survival_format_number, character(1)),
    `Review signal` = ifelse(table$`Review signal` %in% TRUE, "Review", "No strong signal"),
    `Analysis row` = table$`Source row in analysis data`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_strata_table <- function(result) {
  table <- result$strata_table %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Stratum = table$Stratum,
    Records = table$Records,
    Events = table$Events,
    `Event proportion` = vapply(table$Events / table$Records, survival_format_number, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_cluster_summary_table <- function(result) {
  table <- result$cluster_summary %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  table
}

survival_cox_ties_summary_table <- function(result) {
  table <- result$ties_summary %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  display <- table
  display$`Proportion of events at tied times` <- if (is.finite(display$`Proportion of events at tied times`[[1]])) sprintf("%.1f%%", 100 * display$`Proportion of events at tied times`[[1]]) else "N/A"
  display
}

survival_cox_categorical_joint_test_table <- function(result) {
  table <- result$categorical_joint_tests %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Variable = table$Variable,
    Levels = table$Levels,
    Parameters = table$Parameters,
    `Wald chi-square` = vapply(table$`Wald chi-square`, survival_format_number, character(1)),
    df = table$df,
    p = vapply(table$p, survival_p, character(1)),
    Variance = table$Variance,
    Status = ifelse(table$Estimable, "Estimable", "Not estimable"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_spline_test_display_table <- function(result) {
  table <- result$spline_test_table %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Variable = table$Variable,
    `Spline df` = table$`Spline df`,
    `Reference value` = vapply(table$`Reference value`, survival_format_number, character(1)),
    `Nonlinearity LR chi-square` = vapply(table$`Nonlinearity LR chi-square`, survival_format_number, character(1)),
    df = table$df,
    p = vapply(table$p, survival_p, character(1)),
    `Linear-model AIC` = vapply(table$`Linear-model AIC`, survival_format_number, character(1)),
    `Spline-model AIC` = vapply(table$`Spline-model AIC`, survival_format_number, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_spline_ggplot <- function(result) {
  curve <- result$spline_curve %||% data.frame()
  if (!is.data.frame(curve) || !nrow(curve)) return(NULL)
  reference <- unique(curve$Reference)[[1]]
  ggplot2::ggplot(curve, ggplot2::aes(x = Value, y = HR)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper), fill = "#4C78A8", alpha = .18) +
    ggplot2::geom_line(color = "#1F4E79", linewidth = .95) +
    ggplot2::geom_hline(yintercept = 1, color = "grey45", linetype = 2, linewidth = .5) +
    ggplot2::geom_vline(xintercept = reference, color = "#C44E52", linetype = 3, linewidth = .55) +
    ggplot2::labs(x = result$spline_covariate, y = sprintf("Hazard ratio (reference = %s)", survival_format_number(reference))) +
    ggplot2::theme_classic(base_size = 12)
}

survival_cox_time_varying_test_display_table <- function(result) {
  table <- result$time_varying_test_table %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Variable = table$Variable,
    `Time function` = table$`Time function`,
    `Interaction B` = vapply(table$`Interaction B`, survival_format_number, character(1)),
    SE = vapply(table$SE, survival_format_number, character(1)),
    z = vapply(table$z, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_time_specific_hr_table <- function(result) {
  table <- result$time_varying_at_times %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Variable = table$Variable,
    Time = vapply(table$Time, survival_format_number, character(1)),
    HR = vapply(table$HR, survival_format_number, character(1)),
    `95% CI` = mapply(survival_format_ci, table$Lower, table$Upper),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_time_varying_ggplot <- function(result) {
  curve <- result$time_varying_curve %||% data.frame()
  if (!is.data.frame(curve) || !nrow(curve)) return(NULL)
  ggplot2::ggplot(curve, ggplot2::aes(x = Time, y = HR)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper), fill = "#59A14F", alpha = .18) +
    ggplot2::geom_line(color = "#2F6B2F", linewidth = .95) +
    ggplot2::geom_hline(yintercept = 1, color = "grey45", linetype = 2, linewidth = .5) +
    ggplot2::labs(x = result$time, y = sprintf("Time-specific HR for %s", result$time_varying_covariate)) +
    ggplot2::theme_classic(base_size = 12)
}

survival_cox_collinearity_table <- function(result) {
  table <- result$collinearity_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    `Design column` = table$`Design column`,
    VIF = vapply(table$VIF, survival_format_number, character(1)),
    Tolerance = vapply(table$Tolerance, survival_format_number, character(1)),
    Review = table$Review,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cox_functional_form_ggplot <- function(result) {
  data <- result$functional_form_data
  if (!is.data.frame(data) || nrow(data) == 0) return(NULL)
  ggplot2::ggplot(data, ggplot2::aes(x = Value, y = `Martingale residual`)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = .45, linetype = 2) +
    ggplot2::geom_point(alpha = .55, size = 1.5, color = "#1F4E79") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#C44E52", fill = "#C44E52", alpha = .15, linewidth = .8) +
    ggplot2::facet_wrap(~Covariate, scales = "free_x") +
    ggplot2::labs(x = "Covariate value", y = "Martingale residual") +
    ggplot2::theme_classic(base_size = 11)
}

survival_cox_ph_plot <- function(result) {
  ph <- result$ph
  if (is.null(ph) || is.null(ph$y) || !ncol(as.matrix(ph$y))) return(invisible(NULL))
  term_count <- ncol(as.matrix(ph$y))
  columns <- if (term_count == 1L) 1L else 2L
  rows <- ceiling(term_count / columns)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(previous), add = TRUE)
  graphics::par(mfrow = c(rows, columns), mar = c(4, 4, 2.5, 1))
  for (index in seq_len(term_count)) {
    plot(ph, var = index, resid = TRUE, se = TRUE, xlab = "Time", ylab = "Time-varying coefficient")
  }
  invisible(TRUE)
}

survival_adjusted_survival_overview_table <- function(result) {
  adjusted <- result$adjusted_survival
  if (!is.list(adjusted)) return(data.frame())
  data.frame(
    Item = c("Method", "Group variable", "Standardization population", "CI method", "Bootstrap requested", "Bootstrap successful", "Effective bootstrap ratio", "CI available", "Bootstrap seed"),
    Value = c(adjusted$method, adjusted$group, "Complete-case Cox analysis sample", adjusted$ci_method, adjusted$bootstrap_reps, adjusted$bootstrap_successful, if (is.finite(adjusted$bootstrap_effective_ratio)) sprintf("%.1f%%", 100 * adjusted$bootstrap_effective_ratio) else "N/A", if (isTRUE(adjusted$ci_available)) "Yes" else "No", adjusted$seed),
    stringsAsFactors = FALSE
  )
}

survival_adjusted_survival_time_table <- function(result) {
  table <- result$adjusted_survival$time_point_estimates %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Group = table$Level,
    Time = vapply(table$Time, survival_format_number, character(1)),
    `Adjusted survival` = vapply(table$Survival, survival_format_number, character(1)),
    `Pointwise 95% CI` = mapply(survival_format_ci, table$Lower, table$Upper),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_adjusted_survival_contrast_table <- function(result) {
  table <- result$adjusted_survival$time_point_contrasts %||% data.frame()
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Time = vapply(table$Time, survival_format_number, character(1)),
    Comparison = paste(table$`Second level`, "vs", table$`First level`),
    Difference = vapply(table$Difference, survival_format_number, character(1)),
    `Difference 95% CI` = mapply(survival_format_ci, table$`Difference lower`, table$`Difference upper`),
    Ratio = vapply(table$Ratio, survival_format_number, character(1)),
    `Ratio 95% CI` = mapply(survival_format_ci, table$`Ratio lower`, table$`Ratio upper`),
    `Effective bootstrap draws` = table$`Effective bootstrap draws`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_adjusted_survival_ggplot <- function(result) {
  adjusted <- result$adjusted_survival
  if (!is.list(adjusted) || !is.data.frame(adjusted$curve) || nrow(adjusted$curve) == 0) return(NULL)
  curve <- adjusted$curve
  palette <- survival_publication_palette(length(unique(curve$Level)))
  plot <- ggplot2::ggplot(curve, ggplot2::aes(x = Time, y = Survival, color = Level, fill = Level, group = Level)) +
    ggplot2::geom_step(linewidth = .95) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = result$time, y = "Marginal adjusted survival probability", color = adjusted$group, fill = adjusted$group) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  if (all(is.finite(curve$Lower)) && all(is.finite(curve$Upper))) {
    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower, ymax = Upper), alpha = .15, color = NA)
  }
  plot
}

survival_cox_results_panel <- function(result, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  exclusions <- survival_cox_exclusion_table(result)
  reporting <- survival_reporting_bundle(result, language)
  div(
    class = "survival-results",
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("1. Analysis overview"),
      survival_simple_table(survival_cox_overview_table(result)),
      div(class = "result-note", survival_method_sentence(result, language)),
      h4("Data inclusion audit"), survival_simple_table(reporting$data_flow),
      h4("Event-code mapping"), survival_simple_table(reporting$event_map),
      if (nrow(result$categorical_reference_table %||% data.frame()) > 0) tagList(
        h4("Categorical reference levels and contrast coding"),
        survival_simple_table(result$categorical_reference_table),
        survival_table_note("Treatment contrasts are fixed inside the analysis. The stated reference level remains the denominator regardless of the session-wide R contrasts option.")
      ),
      if (nrow(survival_cox_strata_table(result)) > 0) tagList(
        h4("Stratum event counts"),
        survival_simple_table(survival_cox_strata_table(result)),
        survival_table_note("The stratification variable defines separate baseline hazards and therefore has no estimated hazard ratio. Sparse or event-free strata require cautious interpretation.")
      ),
      if (nrow(survival_cox_cluster_summary_table(result)) > 0) tagList(
        h4("Robust-variance cluster summary"),
        survival_simple_table(survival_cox_cluster_summary_table(result)),
        survival_table_note("Sandwich standard errors account for within-cluster correlation. The coefficient estimates are unchanged from the corresponding partial-likelihood model, but inference depends on the number and independence of clusters.")
      ),
      h4("Tied-event summary"),
      survival_simple_table(survival_cox_ties_summary_table(result)),
      survival_table_note("The ties method is prespecified and reported with the observed extent of tied failures. It is not selected by searching for the smallest p-value."),
      if (nrow(exclusions) > 0) tagList(h4("Excluded rows by reason"), survival_simple_table(exclusions))
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(if (nzchar(as.character(result$time_varying_covariate %||% ""))) "2. Cox model with a time-varying coefficient" else "2. Cox proportional hazards model"),
      survival_cox_result_html_table(result, language),
      survival_cox_coef_note(),
      if (nrow(survival_cox_categorical_joint_test_table(result)) > 0) tagList(
        h4("Categorical-predictor omnibus tests"),
        survival_simple_table(survival_cox_categorical_joint_test_table(result)),
        survival_table_note("Each Wald chi-square jointly tests all non-reference coefficients for one categorical predictor. Use the omnibus test for the predictor's overall association and the level-specific HRs for direction and magnitude; do not select levels solely from individual p-values.")
      ),
      survival_table_note(if (identical(language, "ko")) "주. 괄호 안은 p값입니다. Likelihood-ratio, Wald 및 Score는 Cox 모형의 전체 효과를 검정합니다. PH는 Schoenfeld 잔차에 근거한 비례위험 가정 검정이며, GLOBAL은 모든 공변량에 대한 전체 검정입니다. 전체 모형 검정은 자동 합격·불합격 기준이 아니며, PH의 작은 p값은 시간가변 효과 가능성을 추가로 검토하라는 신호입니다." else "Note. Values in parentheses are p-values. Likelihood-ratio, Wald, and Score test the overall Cox model effect. PH denotes the Schoenfeld-residual test of the proportional-hazards assumption, and GLOBAL is its omnibus test across covariates. Overall model tests are not automatic pass/fail criteria; a small PH p-value signals review of a possible time-varying effect.")
    ),
    if (!is.null(survival_cox_ggplot(result, "color"))) div(
      class = "result-section regression-result-panel survival-result-panel survival-plot-result-panel",
      h3("Hazard ratio forest plot"),
      plotOutput("survival_cox_forest_plot", height = "420px")
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("3. Supplementary statistics and diagnostics"),
      survival_simple_table(survival_cox_statistic_table(result), class = "survival-result-table logistic-result-table"),
      survival_cox_statistic_note(),
      h4(if (identical(language, "ko")) "비례위험 가정 상세 진단" else "Detailed proportional-hazards diagnostics"),
      survival_simple_table(survival_ph_table(result, language), class = "survival-result-table logistic-result-table"),
      survival_table_note(if (identical(language, "ko")) "변수별 Schoenfeld 잔차 검정은 시간가변 효과 가능성을 찾기 위한 보조 진단입니다. 본표에는 전체 모형의 Likelihood-ratio 검정과 PH GLOBAL 검정만 제시합니다." else "Covariate-specific Schoenfeld-residual tests are supplementary diagnostics for possible time-varying effects. The main table reports only the overall likelihood-ratio test and the PH GLOBAL test."),
      if (nzchar(as.character(result$time_varying_covariate %||% ""))) survival_table_note("For this time-varying-coefficient analysis, the Schoenfeld tests and plots come from the companion proportional-hazards model fitted before adding tt(). They document the diagnostic motivation; they are not post-extension PH tests."),
      if (!is.null(result$ph) && !is.null(result$ph$y)) tagList(
        plotOutput("survival_cox_ph_plot", height = "620px"),
        survival_table_note("Smoothed scaled Schoenfeld-residual plots support assessment of time-varying coefficients. Formal p-values and plots should be interpreted together; neither is an automatic pass/fail rule.")
      ),
      h4("Design-matrix collinearity review"),
      survival_simple_table(survival_cox_collinearity_table(result)),
      survival_table_note(sprintf("VIF is computed for each Cox design-matrix column; thresholds are heuristic review signals. Condition number = %s.", survival_format_number(result$condition_number))),
      if (nrow(survival_cox_spline_test_display_table(result)) > 0) tagList(
        h4("Natural cubic spline functional-form analysis"),
        survival_simple_table(survival_cox_spline_test_display_table(result)),
        plotOutput("survival_cox_spline_plot", height = "500px"),
        survival_table_note("The likelihood-ratio test compares the prespecified natural-cubic-spline model with the corresponding linear-term model. The HR curve is relative to the sample median and spans the 5th to 95th percentiles. Spline basis coefficients are intentionally omitted because their individual HRs are not substantively interpretable; evaluate the joint test and curve together.")
      ),
      if (nrow(survival_cox_time_varying_test_display_table(result)) > 0) tagList(
        h4("Time-varying coefficient analysis"),
        survival_simple_table(survival_cox_time_varying_test_display_table(result)),
        h4("Time-specific hazard ratios"),
        survival_simple_table(survival_cox_time_specific_hr_table(result)),
        plotOutput("survival_cox_time_varying_plot", height = "500px"),
        survival_table_note("The model is β(t) = β + γ log(1 + time). The interaction test evaluates γ = 0; report the time-specific HR curve and confidence intervals rather than a single constant HR. The plotted range uses the 5th to 95th percentiles of observed event times, plus requested reporting times.")
      )
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("4. Residual distribution review"),
      survival_simple_table(survival_cox_residual_table(result)),
      survival_table_note("Martingale residuals support functional-form review; deviance residuals support unusual-observation review. These summaries do not establish model adequacy by themselves."),
      if (is.data.frame(result$functional_form_data) && nrow(result$functional_form_data) > 0) tagList(
        plotOutput("survival_cox_functional_plot", height = "500px"),
        survival_table_note("The loess smooth of Martingale residuals against each continuous covariate is a functional-form screening plot. Systematic curvature suggests considering a prespecified transformation or spline and comparing substantive conclusions.")
      )
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("5. Influence review"),
      survival_simple_table(survival_cox_influence_table(result)),
      survival_table_note("Standardized DFBETAS are compared with the heuristic 2/sqrt(N) screening threshold. Signals identify observations for sensitivity review; observations are not deleted automatically.")
    ),
    if (is.list(result$adjusted_survival)) div(class = "result-section regression-result-panel survival-result-panel",
      h3("6. Marginal adjusted survival"),
      survival_simple_table(survival_adjusted_survival_overview_table(result)),
      survival_table_note("Each group curve averages model-based survival over the complete-case analysis sample's observed covariate distribution. It is not a curve for one mean-covariate subject."),
      if (nrow(survival_adjusted_survival_time_table(result)) > 0) tagList(
        h4("Adjusted survival at specified times"),
        survival_simple_table(survival_adjusted_survival_time_table(result))
      ),
      if (nrow(survival_adjusted_survival_contrast_table(result)) > 0) tagList(
        h4("Pairwise adjusted-survival contrasts"),
        survival_simple_table(survival_adjusted_survival_contrast_table(result)),
        survival_table_note("Difference is second level minus first level; ratio is second level divided by first level. Confidence intervals are pointwise percentile-bootstrap intervals and are not simultaneous bands.")
      ),
      plotOutput("survival_cox_adjusted_plot", height = "520px")
    ),
    survival_reporting_guidance_panel(result, language)
  )
}

survival_competing_cause_label <- function(cause_code, result) {
  cause_code <- as.integer(cause_code)
  if (is.na(cause_code)) return(as.character(cause_code))
  if (cause_code == 1L) {
    raw <- result$event_map$raw_value[result$event_map$role == "event_of_interest"]
  } else {
    competing <- result$event_map$raw_value[result$event_map$role == "competing_event"]
    raw <- competing[cause_code - 1L]
  }
  if (length(raw) == 0 || is.na(raw[[1]])) paste("Cause", cause_code) else as.character(raw[[1]])
}

survival_competing_overview_table <- function(result) {
  counts <- result$preflight$counts
  data.frame(
    Item = c("Time origin", "Time unit", "N", "Event of interest", "Interest events", "Competing events", "Censored", "Group variable", "Fine-Gray censoring strata", "Method", "Package"),
    Value = c(result$time_origin %||% "", result$time_unit %||% "", result$n, result$event_of_interest, counts$events, counts$competing_events, counts$censored, if (nzchar(result$group)) result$group else "None", if (nzchar(as.character(result$censoring_group %||% ""))) result$censoring_group else "Pooled", if (nzchar(result$group)) "Cumulative incidence function / Gray test" else "Cumulative incidence function", result$packages),
    stringsAsFactors = FALSE
  )
}

survival_competing_event_map_table <- function(result) {
  data.frame(`Raw value` = result$event_map$raw_value, Role = result$event_map$role, Label = result$event_map$label, check.names = FALSE, stringsAsFactors = FALSE)
}

survival_censoring_group_display_table <- function(result) {
  table <- result$censoring_group_table
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  table$`Censoring proportion` <- vapply(table$`Censoring proportion`, survival_format_number, character(1))
  table
}

survival_competing_event_count_display_table <- function(result) {
  table <- result$event_count_table
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  data.frame(
    Group = table$Group,
    `Event value` = table$`Raw event value`,
    Role = table$`Event role`,
    Label = table$Label,
    N = table$`Analysis N`,
    Events = table$Events,
    `Event proportion` = vapply(table$`Event proportion`, survival_format_number, character(1)),
    Review = ifelse(table$`Review signal` %in% TRUE, table$`Review reason`, ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_cif_integrity_display_table <- function(result) {
  table <- result$cif_integrity
  if (!is.data.frame(table) || !nrow(table)) return(data.frame())
  for (name in c("Minimum CIF", "Maximum CIF", "Maximum sum of cause-specific CIFs")) {
    table[[name]] <- vapply(table[[name]], survival_format_number, character(1))
  }
  table
}

survival_competing_rate_table <- function(result) {
  table <- result$cif_at_times
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Group = table$Group,
    Cause = vapply(table$CauseCode, survival_competing_cause_label, character(1), result = result),
    Time = vapply(table$Time, survival_format_number, character(1)),
    CIF = vapply(table$CIF, survival_format_number, character(1)),
    `95% CI` = mapply(survival_format_ci, table$Lower, table$Upper),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_gray_display_table <- function(result) {
  table <- result$gray_tests
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  estimable <- if ("Estimable" %in% names(table)) table$Estimable %in% TRUE else is.finite(table$Statistic) & is.finite(table$p)
  data.frame(
    Cause = vapply(table$CauseCode, survival_competing_cause_label, character(1), result = result),
    Statistic = vapply(table$Statistic, survival_format_number, character(1)),
    df = vapply(table$df, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    Status = ifelse(estimable, "Estimable", "Not estimable"),
    stringsAsFactors = FALSE
  )
}

survival_competing_regression_coef_display <- function(result, model_name, effect_name) {
  model <- result[[model_name]]
  table <- model$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  labels <- result$coefficient_label_table
  if (!is.data.frame(labels) || !nrow(labels)) {
    labels <- data.frame(
      Term = table$Term,
      Variable = table$Term,
      Level = "",
      Reference = FALSE,
      stringsAsFactors = FALSE
    )
  }
  effect_values <- table[[effect_name]]
  rows <- lapply(seq_len(nrow(labels)), function(index) {
    label <- labels[index, , drop = FALSE]
    is_reference <- isTRUE(label$Reference[[1]])
    raw_index <- if (is_reference) NA_integer_ else match(label$Term[[1]], table$Term)
    if (is_reference) {
      values <- c(label$Variable[[1]], paste0(label$Level[[1]], " (reference)"), "", "", "1.000", "Reference", "", "")
    } else if (!is.na(raw_index)) {
      values <- c(
        label$Variable[[1]], label$Level[[1]],
        survival_format_number(table$B[[raw_index]]),
        survival_format_number(table$SE[[raw_index]]),
        survival_format_number(effect_values[[raw_index]]),
        survival_format_ci(table$LLCI[[raw_index]], table$ULCI[[raw_index]]),
        survival_format_number(table$z[[raw_index]]),
        survival_p(table$p[[raw_index]])
      )
    } else {
      return(NULL)
    }
    data.frame(
      Variable = values[[1]], Level = values[[2]], B = values[[3]], SE = values[[4]],
      Effect = values[[5]], `95% CI` = values[[6]], z = values[[7]], p = values[[8]],
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  matched_terms <- labels$Term[!labels$Reference & nzchar(labels$Term)]
  unmatched <- which(!table$Term %in% matched_terms)
  if (length(unmatched)) {
    rows <- c(rows, lapply(unmatched, function(index) data.frame(
      Variable = table$Term[[index]], Level = "",
      B = survival_format_number(table$B[[index]]),
      SE = survival_format_number(table$SE[[index]]),
      Effect = survival_format_number(effect_values[[index]]),
      `95% CI` = survival_format_ci(table$LLCI[[index]], table$ULCI[[index]]),
      z = survival_format_number(table$z[[index]]),
      p = survival_p(table$p[[index]]),
      check.names = FALSE, stringsAsFactors = FALSE
    )))
  }
  display <- do.call(rbind, rows)
  names(display)[names(display) == "Effect"] <- effect_name
  rownames(display) <- NULL
  display
}

survival_cause_specific_coef_table <- function(result) {
  survival_competing_regression_coef_display(result, "cause_specific", "HR")
}

survival_cause_specific_ph_table <- function(result) {
  table <- result$cause_specific$ph_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  for (name in setdiff(names(table), "Term")) {
    table[[name]] <- if (identical(name, "p")) vapply(table[[name]], survival_p, character(1)) else vapply(table[[name]], survival_format_number, character(1))
  }
  table
}

survival_fine_gray_coef_table <- function(result) {
  survival_competing_regression_coef_display(result, "fine_gray", "sHR")
}

survival_fine_gray_optimizer_table <- function(result) {
  table <- result$fine_gray$optimizer_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  display <- table
  numeric_columns <- c("Log likelihood", "Maximum absolute score", "Relative score criterion", "Convergence tolerance", "Standardized information condition number")
  for (name in intersect(numeric_columns, names(display))) display[[name]] <- vapply(display[[name]], survival_format_number, character(1))
  logical_columns <- c("Converged", "Finite covariance matrix", "Positive standard errors")
  for (name in intersect(logical_columns, names(display))) display[[name]] <- ifelse(display[[name]] %in% TRUE, "Yes", "No")
  display
}

survival_fine_gray_residual_table <- function(result) {
  table <- result$fine_gray$residual_review
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = table$Term,
    `Unique target-failure times` = table$`Unique target-failure times`,
    `Spearman rho` = vapply(table$`Spearman rho`, survival_format_number, character(1)),
    `Exploratory p` = vapply(table$`Exploratory p`, survival_p, character(1)),
    `Holm-adjusted p` = vapply(table$`Holm-adjusted p`, survival_p, character(1)),
    `Review status` = ifelse(table$`Review signal` %in% TRUE, "Review possible time-varying subdistribution effect", "No monotonic time-pattern signal in this exploratory screen"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_fine_gray_residual_ggplot <- function(result) {
  data <- result$fine_gray$residual_data
  if (!is.data.frame(data) || !nrow(data)) return(NULL)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = `Event time`, y = Residual)) +
    ggplot2::geom_hline(yintercept = 0, color = "#6b7280", linewidth = .35, linetype = "22") +
    ggplot2::geom_point(color = "#1f4e79", alpha = .65, size = 1.45) +
    ggplot2::facet_wrap(~Term, scales = "free_y") +
    ggplot2::labs(x = "Unique target-failure time", y = "Schoenfeld-like residual", caption = "Residual plots are descriptive lack-of-fit screens; a flat pattern supports, but does not prove, proportional subdistribution hazards.") +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "#eef3f8", color = "#cbd5e1"), legend.position = "none")
  if (length(unique(data$`Event time`)) >= 5L) {
    plot <- plot + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#b45309", fill = "#f59e0b", alpha = .16, linewidth = .75)
  }
  plot
}

survival_competing_ggplot <- function(result) {
  curve <- result$curve
  if (!is.data.frame(curve) || nrow(curve) == 0) return(NULL)
  curve$Cause <- vapply(curve$CauseCode, survival_competing_cause_label, character(1), result = result)
  curve$Curve <- paste(curve$Group, curve$Cause, sep = " / ")
  palette <- survival_publication_palette(length(unique(curve$Curve)))
  caption <- survival_plot_tail_caption(result)
  ggplot2::ggplot(curve, ggplot2::aes(x = Time, y = CIF, color = Curve, group = Curve)) +
    ggplot2::geom_step(linewidth = .95) +
    ggplot2::geom_step(ggplot2::aes(y = Lower), linewidth = .28, linetype = "22", alpha = .45) +
    ggplot2::geom_step(ggplot2::aes(y = Upper), linewidth = .28, linetype = "22", alpha = .45) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = result$time, y = "Cumulative incidence", color = NULL, caption = caption) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

survival_plot_tail_caption <- function(result) {
  followup <- survival_followup_diagnostics(result)
  if (!nrow(followup)) return(NULL)
  n <- as.integer(followup$`At risk at 90th percentile time`[[1]])
  if (is.finite(n) && n < 10L) sprintf("Caution: only %d remain at risk near the follow-up tail; interpret tail estimates with their confidence intervals.", n) else NULL
}

survival_risk_table_plot <- function(table, x_label, palette = NULL) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  table$Strata <- factor(as.character(table$Strata), levels = rev(unique(as.character(table$Strata))))
  plot <- ggplot2::ggplot(table, ggplot2::aes(x = Time, y = Strata, label = N, color = Strata)) +
    ggplot2::geom_text(size = 3.2, show.legend = FALSE) +
    ggplot2::labs(x = x_label, y = NULL, title = "Number at risk") +
    ggplot2::theme_classic(base_size = 10) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0, margin = ggplot2::margin(b = 4)),
      plot.margin = ggplot2::margin(0, 10, 5, 8)
    )
  if (!is.null(palette)) plot <- plot + ggplot2::scale_color_manual(values = palette, drop = FALSE)
  plot
}

survival_km_risk_table_plot <- function(result, figure_version = "color") {
  summary_at <- result$summary_at
  if (is.null(summary_at) || !length(summary_at$time)) return(NULL)
  strata <- as.character(summary_at$strata %||% rep("All", length(summary_at$time)))
  if (!length(strata)) strata <- rep("All", length(summary_at$time))
  table <- data.frame(Time = as.numeric(summary_at$time), Strata = strata, N = as.integer(summary_at$n.risk), stringsAsFactors = FALSE)
  n <- length(unique(strata))
  palette <- if (identical(figure_version, "bw")) survival_bw_palette(n) else survival_publication_palette(n)
  survival_risk_table_plot(table, result$time, stats::setNames(palette, unique(strata)))
}

survival_competing_risk_table_plot <- function(result) {
  times <- as.numeric(result$rate_times %||% numeric(0))
  if (!length(times)) return(NULL)
  data <- result$data
  groups <- if (nzchar(as.character(result$group %||% ""))) unique(as.character(data[[result$group]])) else "All"
  rows <- do.call(rbind, lapply(groups, function(group) {
    subset <- if (identical(group, "All")) data else data[as.character(data[[result$group]]) == group, , drop = FALSE]
    data.frame(Time = times, Strata = group, N = vapply(times, function(value) sum(subset[[result$time]] >= value, na.rm = TRUE), integer(1)), stringsAsFactors = FALSE)
  }))
  palette <- survival_publication_palette(length(groups))
  survival_risk_table_plot(rows, result$time, stats::setNames(palette, groups))
}

survival_draw_plot_with_risk_table <- function(main_plot, risk_plot) {
  if (is.null(risk_plot)) return(print(main_plot))
  caption <- as.character(main_plot$labels$caption %||% "")
  caption <- caption[!is.na(caption) & nzchar(caption)]
  main_plot <- main_plot +
    ggplot2::labs(x = NULL, caption = NULL) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "top",
      legend.margin = ggplot2::margin(0, 0, 2, 0),
      plot.margin = ggplot2::margin(6, 10, 0, 8)
    )
  risk_plot <- risk_plot +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 10, 3, 8))
  main_grob <- ggplot2::ggplotGrob(main_plot)
  risk_grob <- ggplot2::ggplotGrob(risk_plot)
  if (length(main_grob$widths) == length(risk_grob$widths)) {
    aligned_widths <- grid::unit.pmax(main_grob$widths, risk_grob$widths)
    main_grob$widths <- aligned_widths
    risk_grob$widths <- aligned_widths
  }
  grid::grid.newpage()
  has_caption <- length(caption) > 0L
  heights <- if (has_caption) c(4.08, 1.12, 0.34) else c(4.08, 1.12)
  layout <- grid::grid.layout(length(heights), 1, heights = grid::unit(heights, "null"))
  grid::pushViewport(grid::viewport(layout = layout))
  grid::pushViewport(grid::viewport(layout.pos.row = 1L, layout.pos.col = 1L))
  grid::grid.draw(main_grob)
  grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row = 2L, layout.pos.col = 1L))
  grid::grid.draw(risk_grob)
  grid::popViewport()
  if (has_caption) {
    grid::pushViewport(grid::viewport(layout.pos.row = 3L, layout.pos.col = 1L))
    grid::grid.text(
      caption[[1]],
      x = grid::unit(8, "pt"),
      y = grid::unit(0.88, "npc"),
      just = c("left", "top"),
      gp = grid::gpar(fontsize = 8.5, col = "#4b5563")
    )
    grid::popViewport()
  }
  grid::popViewport()
  invisible(list(main = main_plot, risk = risk_plot, main_grob = main_grob, risk_grob = risk_grob, caption = caption))
}

survival_competing_results_panel <- function(result, plot_output_id = "survival_competing_plot", language = statedu_initial_language()) {
  curve_number <- 4L + as.integer(is.list(result$cause_specific)) + as.integer(is.list(result$fine_gray))
  reporting <- survival_reporting_bundle(result, language)
  div(
    class = "survival-results",
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("1. Analysis overview"),
      survival_simple_table(survival_competing_overview_table(result)),
      div(class = "result-note", survival_method_sentence(result, language)),
      h4("Data inclusion audit"), survival_simple_table(reporting$data_flow),
      h4("Event-code mapping"),
      survival_simple_table(reporting$event_map),
      if (nrow(result$categorical_reference_table %||% data.frame()) > 0) tagList(
        h4("Categorical reference levels and contrast coding"),
        survival_simple_table(result$categorical_reference_table),
        survival_table_note("Treatment contrasts are fixed inside cause-specific Cox and Fine-Gray regression. The stated reference level remains the denominator regardless of the session-wide R contrasts option.")
      ),
      h4("Estimand contract"),
      survival_simple_table(result$estimand_table %||% data.frame()),
      h4("Group-by-cause event counts"),
      survival_simple_table(survival_competing_event_count_display_table(result)),
      h4("CIF integrity checks"),
      survival_simple_table(survival_cif_integrity_display_table(result)),
      if (nrow(reporting$exclusions)) tagList(h4("Excluded rows by reason"), survival_simple_table(reporting$exclusions))
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("2. Cumulative incidence at selected time points"),
      survival_simple_table(survival_competing_rate_table(result)),
      survival_table_note("CIF estimates the actual cause-specific cumulative incidence in the presence of competing events; 1-KM is not used.")
    ),
    if (nrow(survival_gray_display_table(result)) > 0) div(class = "result-section regression-result-panel survival-result-panel",
      h3("3. Gray test"),
      survival_simple_table(survival_gray_display_table(result)),
      survival_table_note("Gray's test compares cumulative incidence functions across groups; it is not an effect-size estimate.")
    ),
    if (is.list(result$cause_specific)) div(class = "result-section regression-result-panel survival-result-panel",
      h3("4. Cause-specific Cox regression"),
      survival_simple_table(survival_cause_specific_coef_table(result)),
      survival_table_note("HR is the cause-specific hazard ratio. Competing events leave the risk set at their occurrence time as part of this estimand. Categorical reference rows are definitions with HR = 1, not estimated coefficients; the raw engine coefficient table is retained in the audit export."),
      h4("Cause-specific overall model tests"),
      survival_simple_table(survival_cox_model_test_table(result$cause_specific)),
      survival_table_note("Likelihood-ratio, Wald, and Score tests evaluate the joint null that all displayed cause-specific Cox coefficients are zero. They are not model-fit indices or evidence of causality."),
      if (nrow(survival_cox_categorical_joint_test_table(result$cause_specific)) > 0) tagList(
        h4("Cause-specific categorical-predictor omnibus tests"),
        survival_simple_table(survival_cox_categorical_joint_test_table(result$cause_specific)),
        survival_table_note("Each Wald test jointly evaluates all non-reference coefficients for one categorical predictor. Report this overall association with level-specific HRs; do not select levels solely from individual p-values.")
      ),
      h4("Cause-specific proportional hazards review"),
      survival_simple_table(survival_cause_specific_ph_table(result)),
      if (!is.null(result$cause_specific$ph)) plotOutput("survival_cause_specific_ph_plot", height = "620px"),
      survival_table_note("Schoenfeld results and residual plots are review signals, not an automatic proportional-hazards pass/fail decision."),
      h4("Cause-specific collinearity review"),
      survival_simple_table(survival_cox_collinearity_table(result$cause_specific)),
      survival_table_note(sprintf("Design-matrix condition number: %s. VIF and condition-number thresholds are heuristic screening rules.", survival_format_number(result$cause_specific$condition_number %||% NA_real_))),
      h4("Cause-specific residual distribution"),
      survival_simple_table(survival_cox_residual_table(result$cause_specific)),
      if (is.data.frame(result$cause_specific$functional_form_data) && nrow(result$cause_specific$functional_form_data)) tagList(
        h4("Cause-specific continuous-covariate functional-form review"),
        plotOutput("survival_cause_specific_functional_plot", height = "500px"),
        survival_table_note("The Martingale-residual smoother is a descriptive functional-form diagnostic; it does not select a transformation automatically.")
      ),
      h4("Cause-specific influence review"),
      survival_simple_table(survival_cox_influence_table(result$cause_specific)),
      survival_table_note("Standardized DFBETAS above 2/sqrt(n) are observations for sensitivity review, not automatic deletion rules.")
    ),
    if (is.list(result$fine_gray)) div(class = "result-section regression-result-panel survival-result-panel",
      h3("5. Fine-Gray regression"),
      survival_simple_table(survival_fine_gray_coef_table(result)),
      survival_table_note("sHR is a subdistribution hazard ratio linked to the CIF. It is not labeled HR or interpreted as an individual risk ratio. Categorical reference rows are definitions with sHR = 1, not estimated coefficients; the raw engine coefficient table is retained in the audit export."),
      h4("Fine-Gray overall model test"),
      survival_simple_table(survival_cox_model_test_table(result$fine_gray)),
      survival_table_note("The pseudo likelihood-ratio test is the cmprsk::crr omnibus test of the joint coefficient null. It is not a conventional likelihood fit index and does not establish predictive adequacy or causality."),
      if (nrow(survival_cox_categorical_joint_test_table(result$fine_gray)) > 0) tagList(
        h4("Fine-Gray categorical-predictor omnibus tests"),
        survival_simple_table(survival_cox_categorical_joint_test_table(result$fine_gray)),
        survival_table_note("Each model-based Wald test jointly evaluates all non-reference coefficients for one categorical predictor. Report it with level-specific sHRs; do not select levels solely from individual p-values.")
      ),
      h4("Censoring-distribution estimation"),
      survival_simple_table(survival_censoring_group_display_table(result)),
      survival_table_note(if (nzchar(as.character(result$censoring_group %||% ""))) paste("The censoring distribution was estimated separately within", result$censoring_group, "using crr(cengroup=...). This variable should be prespecified from the design, not selected from observed p-values.") else "One censoring distribution was estimated across the analysis sample. Fine-Gray validity requires independent censoring conditional on the modeled structure; consider prespecified cengroup strata when censoring distributions differ across known groups."),
      h4("Fine-Gray numerical stability"),
      survival_simple_table(survival_fine_gray_optimizer_table(result)),
      h4("Fine-Gray design-matrix collinearity review"),
      survival_simple_table(survival_cox_collinearity_table(result$fine_gray)),
      survival_table_note(sprintf("Design-matrix condition number: %s. VIF and condition-number thresholds are heuristic screening rules; they do not select or delete covariates automatically.", survival_format_number(result$fine_gray$condition_number %||% NA_real_))),
      h4("Proportional subdistribution hazards review"),
      survival_simple_table(survival_fine_gray_residual_table(result)),
      if (is.data.frame(result$fine_gray$residual_data) && nrow(result$fine_gray$residual_data)) plotOutput("survival_fine_gray_residual_plot", height = "520px"),
      survival_table_note("The crr Schoenfeld-like residual plot is the primary descriptive lack-of-fit review. Spearman correlations and Holm-adjusted p-values are exploratory monotonic-trend screens, not a formal cox.zph-equivalent test and not an automatic assumption pass/fail decision.")
    ),
    survival_reporting_guidance_panel(result, language),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(paste0(curve_number, ". Cumulative incidence curves")),
      plotOutput(plot_output_id, height = "650px")
    )
  )
}

survival_saved_result_title <- function(result) {
  switch(
    as.character(result$type %||% ""),
    km = "StatEdu Studio Kaplan-Meier Results",
    km_multi = "StatEdu Studio Kaplan-Meier Results",
    cox = "StatEdu Studio Cox Regression Results",
    competing_risk = "StatEdu Studio Competing-Risks Results",
    "StatEdu Studio Survival Results"
  )
}

survival_saved_km_plot_ids <- function(result) {
  items <- survival_km_result_items(result)
  lapply(seq_along(items), function(index) {
    count <- length(items[[index]]$plot_types %||% "survival") * length(items[[index]]$plot_versions %||% "color")
    paste0("survival_saved_km_plot_", index, "_", seq_len(count))
  })
}

survival_saved_plot_specs <- function(result, km_plot_ids = NULL, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  specs <- list()
  add <- function(id, title, draw, width = 1000, height = 720) {
    specs[[length(specs) + 1L]] <<- list(id = id, title = title, draw = draw, width = width, height = height)
  }
  if (result$type %in% c("km", "km_multi")) {
    items <- survival_km_result_items(result)
    if (is.null(km_plot_ids)) km_plot_ids <- survival_saved_km_plot_ids(result)
    for (item_index in seq_along(items)) {
      item <- items[[item_index]]
      plot_specs <- expand.grid(
        plot_type = as.character(item$plot_types %||% "survival"),
        plot_version = as.character(item$plot_versions %||% "color"),
        stringsAsFactors = FALSE
      )
      for (plot_index in seq_len(nrow(plot_specs))) {
        local({
          current_item <- item
          current_type <- plot_specs$plot_type[[plot_index]]
          current_version <- plot_specs$plot_version[[plot_index]]
          current_id <- km_plot_ids[[item_index]][[plot_index]]
          current_title <- paste(survival_plot_type_label(current_type, language), survival_plot_version_label(current_version, language), sep = " - ")
          if (identical(result$type, "km_multi")) current_title <- paste0(survival_km_group_label(current_item), ": ", current_title)
          add(current_id, current_title, function() {
            survival_draw_plot_with_risk_table(
              survival_km_ggplot(current_item, current_type, current_version),
              survival_km_risk_table_plot(current_item, current_version)
            )
          })
        })
      }
    }
  } else if (identical(result$type, "cox")) {
    forest_plot <- survival_cox_ggplot(result, "color")
    if (!is.null(forest_plot)) add(
      "survival_cox_forest_plot",
      "Hazard ratio forest plot",
      function() print(survival_cox_ggplot(result, "color")),
      width = 1000,
      height = 680
    )
    if (!is.null(result$ph) && !is.null(result$ph$y)) add("survival_cox_ph_plot", "Scaled Schoenfeld-residual plots", function() survival_cox_ph_plot(result), height = 760)
    if (nrow(result$spline_curve %||% data.frame())) add("survival_cox_spline_plot", "Natural cubic spline hazard-ratio curve", function() print(survival_cox_spline_ggplot(result)))
    if (nrow(result$time_varying_curve %||% data.frame())) add("survival_cox_time_varying_plot", "Time-specific hazard-ratio curve", function() print(survival_cox_time_varying_ggplot(result)))
    if (nrow(result$functional_form_data %||% data.frame())) add("survival_cox_functional_plot", "Martingale-residual functional-form review", function() print(survival_cox_functional_form_ggplot(result)))
    if (is.list(result$adjusted_survival)) add("survival_cox_adjusted_plot", "Marginal adjusted survival curves", function() print(survival_adjusted_survival_ggplot(result)))
  } else if (identical(result$type, "competing_risk")) {
    add("survival_competing_plot", "Cumulative incidence curves", function() {
      survival_draw_plot_with_risk_table(survival_competing_ggplot(result), survival_competing_risk_table_plot(result))
    })
    if (is.list(result$cause_specific) && !is.null(result$cause_specific$ph)) add("survival_cause_specific_ph_plot", "Cause-specific scaled Schoenfeld-residual plots", function() survival_cox_ph_plot(result$cause_specific), height = 760)
    if (is.list(result$cause_specific) && nrow(result$cause_specific$functional_form_data %||% data.frame())) add("survival_cause_specific_functional_plot", "Cause-specific Martingale-residual functional-form review", function() print(survival_cox_functional_form_ggplot(result$cause_specific)))
    if (is.list(result$fine_gray) && nrow(result$fine_gray$residual_data %||% data.frame())) add("survival_fine_gray_residual_plot", "Fine-Gray Schoenfeld-like residual review", function() print(survival_fine_gray_residual_ggplot(result)))
  }
  specs
}

survival_inline_saved_plots <- function(panel, plot_specs) {
  rendered <- htmltools::renderTags(panel)$html
  if (!length(plot_specs)) return(rendered)
  document <- xml2::read_html(paste0("<html><body>", rendered, "</body></html>"), options = c("RECOVER", "NOERROR", "NOWARNING"))
  for (spec in plot_specs) {
    node <- xml2::xml_find_first(document, sprintf("//*[@id='%s']", spec$id))
    if (length(node) == 0 || is.na(xml2::xml_name(node))) next
    uri <- plot_data_uri(function(ignored) spec$draw(), NULL, width = spec$width, height = spec$height, res = 110)
    children <- xml2::xml_children(node)
    if (length(children)) xml2::xml_remove(children)
    xml2::xml_attr(node, "id") <- NULL
    xml2::xml_attr(node, "style") <- NULL
    xml2::xml_set_attr(node, "class", "residual-plot-card survival-export-plot-card")
    xml2::xml_add_child(node, "img", src = uri, width = as.character(spec$width), height = as.character(spec$height), alt = spec$title)
  }
  body <- xml2::xml_find_first(document, ".//body")
  paste(vapply(xml2::xml_children(body), as.character, character(1)), collapse = "\n")
}

survival_saved_results_content <- function(result, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  if (result$type %in% c("km", "km_multi")) {
    plot_ids <- survival_saved_km_plot_ids(result)
    panel <- survival_km_results_panel(result, plot_ids, language)
    specs <- survival_saved_plot_specs(result, plot_ids, language)
  } else if (identical(result$type, "cox")) {
    panel <- survival_cox_results_panel(result, language)
    specs <- survival_saved_plot_specs(result, language = language)
  } else if (identical(result$type, "competing_risk")) {
    panel <- survival_competing_results_panel(result, language = language)
    specs <- survival_saved_plot_specs(result, language = language)
  } else {
    stop("Unsupported survival result type for export.")
  }
  survival_inline_saved_plots(panel, specs)
}

saved_survival_results_html <- function(result, language = statedu_initial_language(), css_path = file.path("www", "style.css"), report_mode = FALSE) {
  saved_results_document(
    survival_saved_result_title(result),
    htmltools::HTML(survival_saved_results_content(result, language)),
    max_width = 1500,
    css_path = css_path,
    print_landscape = TRUE,
    report_mode = report_mode
  )
}

write_survival_results_html <- function(result, file, language = statedu_initial_language()) {
  writeLines(saved_survival_results_html(result, language), file, useBytes = TRUE)
  invisible(file)
}

write_survival_results_pdf <- function(result, file, language = statedu_initial_language()) {
  write_pdf_from_html(saved_survival_results_html(result, language, report_mode = TRUE), file)
}

survival_excel_result_tables <- function(result, language = statedu_initial_language()) {
  entry <- list(
    title = survival_saved_result_title(result),
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    html = saved_survival_results_html(result, language)
  )
  result_entry_tables(entry, 1L)
}

save_survival_excel_file <- function(result, file, language = statedu_initial_language()) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Excel export requires the openxlsx package.")
  workbook <- openxlsx::createWorkbook()
  used_sheets <- character(0)
  metadata <- data.frame(
    Item = c("Analysis", "Method", "Generated by"),
    Value = c(survival_saved_result_title(result), survival_method_sentence(result, language), "StatEdu Studio"),
    stringsAsFactors = FALSE
  )
  used_sheets <- add_excel_table_sheet(workbook, "Report metadata", metadata, used_sheets, title = "Report metadata")
  tables <- survival_excel_result_tables(result, language)
  for (table_info in tables) {
    used_sheets <- add_excel_table_sheet(workbook, table_info$title, table_info$table, used_sheets, title = table_info$title)
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}
