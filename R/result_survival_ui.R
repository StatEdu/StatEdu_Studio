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

survival_simple_table <- function(table, class = "survival-result-table") {
  if (!is.data.frame(table) || nrow(table) == 0) return(NULL)
  tags$table(
    class = paste("coefficient-table", class),
    style = result_table_style(font_size = 12, min_width = 0),
    tags$thead(tags$tr(lapply(seq_along(table), function(col_index) {
      tags$th(
        style = paste0(result_header_cell_style(col_index == 1L), if (col_index == 1L) "" else "text-align:center !important;"),
        survival_header_content(names(table)[[col_index]])
      )
    }))),
    tags$tbody(lapply(seq_len(nrow(table)), function(row_index) {
      tags$tr(lapply(seq_along(table), function(col_index) {
        column <- names(table)[[col_index]]
        value <- table[[col_index]][[row_index]]
        if (is.numeric(value)) value <- survival_format_number(value)
        marker <- survival_cell_note_marker(table, row_index, column)
        tags$td(
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
    if (nzchar(as.character(result$start %||% ""))) if (ko) "대상자 군집-강건 표준오차를 적용한 시간의존 Cox 비례위험모형" else "a time-dependent Cox proportional hazards model with subject-cluster robust standard errors" else if (nzchar(as.character(result$entry %||% ""))) if (ko) "지연 진입을 반영한 Cox 비례위험모형" else "a Cox proportional hazards model with delayed entry" else if (ko) "Cox 비례위험모형" else "a Cox proportional hazards model"
  } else if (identical(result$type, "competing_risk")) {
    has_cs <- is.list(result$cause_specific)
    has_fg <- is.list(result$fine_gray)
    if (ko) paste0("누적발생함수와 Gray 검정", if (has_cs) ", 원인별 Cox 회귀" else "", if (has_fg) ", Fine–Gray 회귀" else "") else paste0("cumulative incidence functions and Gray's test", if (has_cs) ", cause-specific Cox regression" else "", if (has_fg) ", and Fine–Gray regression" else "")
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
  list(method = survival_method_sentence(result, language), data_flow = survival_reporting_data_flow(result), event_map = survival_reporting_event_map(result), exclusions = survival_reporting_exclusions(result), followup = survival_followup_diagnostics(result), stability = survival_stability_review(result, language), checklist = survival_reporting_checklist(result, language), interpretation = survival_interpretation_guide(result, language))
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
    if (nzchar(as.character(result$subject_id %||% "")) && result$subject_id %in% names(result$data)) {
      clusters <- length(unique(result$data[[result$subject_id]]))
      if (clusters < 30L) add("review", "few_subject_clusters", sprintf("Subject clusters = %d", clusters), if (ko) "소수 군집에서 강건 표준오차의 유한표본 성능을 검토하세요." else "Review finite-sample performance of cluster-robust standard errors.")
    }
    if (any(!is.finite(result$coef_table$B %||% numeric(0)))) add("high", "nonfinite_coefficient", "One or more coefficients are non-finite.", if (ko) "희소성, 완전분리 또는 모형 식별 문제를 검토하세요." else "Review sparsity, separation, or model-identification problems.")
  } else if (identical(result$type, "competing_risk")) {
    counts <- result$preflight$counts %||% list()
    if ((counts$events %||% 0L) < 10L) add("review", "few_interest_events", sprintf("Interest events = %d", counts$events %||% 0L), if (ko) "관심 사건 추정치와 회귀계수의 불확실성을 강조하세요." else "Emphasize uncertainty in interest-event estimates and regression coefficients.")
    if ((counts$competing_events %||% 0L) < 10L) add("review", "few_competing_events", sprintf("Competing events = %d", counts$competing_events %||% 0L), if (ko) "경쟁사건 CIF와 원인별 비교의 희소성을 검토하세요." else "Review sparsity of competing-event CIFs and cause-specific comparisons.")
    parameter_count <- max(c(
      if (is.list(result$cause_specific) && is.data.frame(result$cause_specific$coef_table)) nrow(result$cause_specific$coef_table) else 0L,
      if (is.list(result$fine_gray) && is.data.frame(result$fine_gray$coef_table)) nrow(result$fine_gray$coef_table) else 0L
    ))
    if (parameter_count > 0L && (counts$events %||% 0L) / parameter_count < 10) add("review", "low_interest_events_per_covariate", sprintf("Interest events/covariate = %s", survival_format_number((counts$events %||% 0L) / parameter_count)), if (ko) "경쟁위험 회귀모형의 복잡도를 재검토하세요." else "Reconsider competing-risk regression complexity.")
    if (is.list(result$fine_gray) && !isTRUE(result$fine_gray$converged)) add("high", "fine_gray_not_converged", "Fine-Gray convergence flag is false.", if (ko) "Fine–Gray 추정 결과를 보고하지 말고 모형과 자료 희소성을 검토하세요." else "Do not report the Fine-Gray estimates before reviewing the model and data sparsity.")
  } else {
    items <- survival_km_result_items(result)
    for (item in items) {
      fit_table <- summary(item$fit)$table
      if (is.null(dim(fit_table))) fit_table <- t(as.matrix(fit_table))
      events <- as.numeric(fit_table[, "events", drop = TRUE] %||% numeric(0))
      strata <- rownames(fit_table) %||% rep("All", length(events))
      sparse <- which(is.finite(events) & events < 5)
      for (index in sparse) add("review", "few_events_in_stratum", sprintf("%s: events = %d", strata[[index]], events[[index]]), if (ko) "집단별 곡선과 비교검정의 불확실성을 강조하세요." else "Emphasize uncertainty in stratum curves and comparison tests.")
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
  median_text <- survival_format_number(median)
  ci_text <- survival_format_ci(lower, upper)
  if (!nzchar(ci_text)) {
    return(median_text)
  }
  paste0(median_text, "\u00A0", ci_text)
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
  for (row_index in seq_len(nrow(posthoc))) {
    parts <- trimws(strsplit(as.character(posthoc$Comparison[[row_index]] %||% ""), "\\s+vs\\s+", perl = TRUE)[[1]])
    if (length(parts) != 2L) next
    first <- display_level(parts[[1]])
    second <- display_level(parts[[2]])
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
        survival_simple_table(survival_logrank_table(result))
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
  c_index <- if (length(concordance) >= 1L) survival_format_number(concordance[[1]]) else ""
  data.frame(
    Item = c("Time origin", "Time unit", "Entry variable", "Interval start", "Interval stop", "Subject ID", "Variance", "Source rows", "Analysis rows", "Excluded rows", "Events", "Parameters", "Events / parameter", "Ties", "Concordance", "Package"),
    Value = c(
      result$time_origin %||% "",
      result$time_unit %||% "",
      result$entry %||% "None",
      result$start %||% "None",
      result$stop %||% "None",
      result$subject_id %||% "None",
      if (!is.null(result$fit$naive.var)) "Subject-cluster robust" else "Model-based",
      result$preflight$counts$source_rows %||% result$n,
      result$n,
      (result$preflight$counts$source_rows %||% result$n) - result$n,
      result$events,
      result$parameter_count %||% NA_integer_,
      survival_format_number(result$events_per_parameter),
      result$ties %||% "",
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

survival_cox_coef_table <- function(result) {
  table <- result$coef_table
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
  table <- result$coef_table
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
  table <- result$coef_table
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
        "PH ", ph_term[[index]], " χ²(p) = ",
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
    `Analysis row` = table$`Source row in analysis data`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_adjusted_survival_overview_table <- function(result) {
  adjusted <- result$adjusted_survival
  if (!is.list(adjusted)) return(data.frame())
  data.frame(
    Item = c("Method", "Group variable", "Standardization population", "Bootstrap requested", "Bootstrap successful", "Bootstrap seed"),
    Value = c(adjusted$method, adjusted$group, "Complete-case Cox analysis sample", adjusted$bootstrap_reps, adjusted$bootstrap_successful, adjusted$seed),
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
      if (nrow(exclusions) > 0) tagList(h4("Excluded rows by reason"), survival_simple_table(exclusions))
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("2. Cox proportional hazards model"),
      survival_cox_result_html_table(result, language),
      survival_cox_coef_note(),
      survival_table_note(if (identical(language, "ko")) "주. 괄호 안은 p값입니다. Likelihood-ratio, Wald 및 Score는 Cox 모형의 전체 효과를 검정합니다. PH는 Schoenfeld 잔차에 근거한 비례위험 가정 검정이며, GLOBAL은 모든 공변량에 대한 전체 검정입니다. 전체 모형 검정은 자동 합격·불합격 기준이 아니며, PH의 작은 p값은 시간가변 효과 가능성을 추가로 검토하라는 신호입니다." else "Note. Values in parentheses are p-values. Likelihood-ratio, Wald, and Score test the overall Cox model effect. PH denotes the Schoenfeld-residual test of the proportional-hazards assumption, and GLOBAL is its omnibus test across covariates. Overall model tests are not automatic pass/fail criteria; a small PH p-value signals review of a possible time-varying effect.")
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("3. Supplementary statistics and diagnostics"),
      survival_simple_table(survival_cox_statistic_table(result), class = "survival-result-table logistic-result-table"),
      survival_cox_statistic_note(),
      h4(if (identical(language, "ko")) "비례위험 가정 상세 진단" else "Detailed proportional-hazards diagnostics"),
      survival_simple_table(survival_ph_table(result, language), class = "survival-result-table logistic-result-table"),
      survival_table_note(if (identical(language, "ko")) "변수별 Schoenfeld 잔차 검정은 시간가변 효과 가능성을 찾기 위한 보조 진단입니다. 본표에는 전체 모형의 Likelihood-ratio 검정과 PH GLOBAL 검정만 제시합니다." else "Covariate-specific Schoenfeld-residual tests are supplementary diagnostics for possible time-varying effects. The main table reports only the overall likelihood-ratio test and the PH GLOBAL test.")
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("4. Residual distribution review"),
      survival_simple_table(survival_cox_residual_table(result)),
      survival_table_note("Martingale residuals support functional-form review; deviance residuals support unusual-observation review. These summaries do not establish model adequacy by themselves.")
    ),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3("5. Influence review"),
      survival_simple_table(survival_cox_influence_table(result)),
      survival_table_note("Large absolute DFBETA values identify observations for review; observations are not deleted automatically.")
    ),
    if (is.list(result$adjusted_survival)) div(class = "result-section regression-result-panel survival-result-panel",
      h3("6. Marginal adjusted survival"),
      survival_simple_table(survival_adjusted_survival_overview_table(result)),
      survival_table_note("Each group curve averages model-based survival over the complete-case analysis sample's observed covariate distribution. It is not a curve for one mean-covariate subject."),
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
    Item = c("Time origin", "Time unit", "N", "Event of interest", "Interest events", "Competing events", "Censored", "Group variable", "Method", "Package"),
    Value = c(result$time_origin %||% "", result$time_unit %||% "", result$n, result$event_of_interest, counts$events, counts$competing_events, counts$censored, if (nzchar(result$group)) result$group else "None", "Cumulative incidence function / Gray test", result$packages),
    stringsAsFactors = FALSE
  )
}

survival_competing_event_map_table <- function(result) {
  data.frame(`Raw value` = result$event_map$raw_value, Role = result$event_map$role, Label = result$event_map$label, check.names = FALSE, stringsAsFactors = FALSE)
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
  data.frame(
    Cause = vapply(table$CauseCode, survival_competing_cause_label, character(1), result = result),
    Statistic = vapply(table$Statistic, survival_format_number, character(1)),
    df = vapply(table$df, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    stringsAsFactors = FALSE
  )
}

survival_cause_specific_coef_table <- function(result) {
  table <- result$cause_specific$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = table$Term,
    B = vapply(table$B, survival_format_number, character(1)),
    SE = vapply(table$SE, survival_format_number, character(1)),
    HR = vapply(table$HR, survival_format_number, character(1)),
    `95% CI` = mapply(survival_format_ci, table$LLCI, table$ULCI),
    z = vapply(table$z, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
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
  table <- result$fine_gray$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = table$Term,
    B = vapply(table$B, survival_format_number, character(1)),
    SE = vapply(table$SE, survival_format_number, character(1)),
    sHR = vapply(table$sHR, survival_format_number, character(1)),
    `95% CI` = mapply(survival_format_ci, table$LLCI, table$ULCI),
    z = vapply(table$z, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_fine_gray_residual_table <- function(result) {
  table <- result$fine_gray$residual_review
  if (!is.data.frame(table) || nrow(table) == 0) return(data.frame())
  data.frame(
    Term = table$Term,
    `Residual-time correlation` = vapply(table$`Residual-time correlation`, survival_format_number, character(1)),
    p = vapply(table$p, survival_p, character(1)),
    `Review status` = ifelse(is.finite(table$p) & table$p < .05, "Review possible time-varying subdistribution effect", "No strong signal in this residual screen"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
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
      survival_table_note("HR is the cause-specific hazard ratio. Competing events leave the risk set at their occurrence time as part of this estimand."),
      h4("Cause-specific proportional hazards review"),
      survival_simple_table(survival_cause_specific_ph_table(result)),
      survival_table_note("Schoenfeld results are review signals, not an automatic proportional-hazards pass/fail decision.")
    ),
    if (is.list(result$fine_gray)) div(class = "result-section regression-result-panel survival-result-panel",
      h3("5. Fine-Gray regression"),
      survival_simple_table(survival_fine_gray_coef_table(result)),
      survival_table_note("sHR is a subdistribution hazard ratio linked to the CIF. It is not labeled HR or interpreted as an individual risk ratio."),
      h4("Proportional subdistribution hazards review"),
      survival_simple_table(survival_fine_gray_residual_table(result)),
      survival_table_note("Residual-time correlation is a screening diagnostic. Review residual plots and substantive context; it is not an automatic assumption pass.")
    ),
    survival_reporting_guidance_panel(result, language),
    div(class = "result-section regression-result-panel survival-result-panel",
      h3(paste0(curve_number, ". Cumulative incidence curves")),
      plotOutput(plot_output_id, height = "650px")
    )
  )
}
