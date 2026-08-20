# Survival analysis core functions.

survival_ui_text <- function(text, language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  key <- tolower(trimws(as.character(text %||% "")))
  labels <- list(
    "survival analysis" = c(en = "Survival Analysis", ko = "생존분석"),
    "analysis setup" = c(en = "Survival Analysis Guide", ko = "분석 선택 도우미"),
    "kaplan-meier" = c(en = "Kaplan-Meier", ko = "Kaplan-Meier"),
    "cox regression" = c(en = "Cox Regression", ko = "Cox 회귀분석"),
    "competing risks" = c(en = "Competing-Event Analysis", ko = "경쟁사건 분석"),
    "censored value" = c(en = "Censored value", ko = "검열값"),
    "event of interest" = c(en = "Event of interest", ko = "관심 사건값"),
    "competing event values" = c(en = "Competing event values", ko = "경쟁사건값"),
    "run competing risks" = c(en = "Run competing-risks analysis", ko = "경쟁위험 분석 실행"),
    "time variable" = c(en = "Time variable", ko = "생존시간 변수"),
    "event variable" = c(en = "Event variable", ko = "사건 변수"),
    "event value" = c(en = "Event value", ko = "사건값"),
    "group variable" = c(en = "Group variable", ko = "집단 변수"),
    "no group" = c(en = "No group", ko = "집단 없음"),
    "covariates" = c(en = "Covariates", ko = "공변량"),
    "time-point rates" = c(en = "Time-point rates", ko = "시점별 생존율"),
    "rmst tau" = c(en = "RMST limit (tau)", ko = "RMST 제한시간 (tau)"),
    "show confidence interval" = c(en = "Show confidence interval", ko = "신뢰구간 표시"),
    "show censor marks" = c(en = "Show censor marks", ko = "검열 표시"),
    "run kaplan-meier" = c(en = "Run Kaplan-Meier", ko = "Kaplan-Meier 실행"),
    "run cox regression" = c(en = "Run Cox regression", ko = "Cox 회귀분석 실행"),
    "ph assumption" = c(en = "PH assumption", ko = "PH 가정"),
    "adjusted survival group" = c(en = "Adjusted survival group", ko = "보정 생존곡선 집단"),
    "bootstrap repetitions" = c(en = "Bootstrap repetitions", ko = "Bootstrap 반복수"),
    "number at risk" = c(en = "Number at risk", ko = "Number at risk"),
    "median survival" = c(en = "Median survival", ko = "중앙 생존시간"),
    "log-rank test" = c(en = "Log-rank test", ko = "Log-rank 검정"),
    "model overview" = c(en = "Model overview", ko = "모형 요약"),
    "analysis" = c(en = "Analysis", ko = "분석"),
    "tables" = c(en = "Tables", ko = "표"),
    "plots" = c(en = "Plots", ko = "도표"),
    "analysis method" = c(en = "Analysis method", ko = "분석 방법"),
    "life table" = c(en = "Life table", ko = "생명표분석"),
    "group comparison" = c(en = "Group comparison", ko = "집단 비교"),
    "survival table" = c(en = "Survival table", ko = "생존표"),
    "mean / median / quartiles" = c(en = "Mean / median / quartiles", ko = "평균 / 중위수 / 사분위수"),
    "plot type" = c(en = "Plot type", ko = "도표 종류"),
    "plot version" = c(en = "Plot version", ko = "도표 버전"),
    "display" = c(en = "Display", ko = "표시"),
    "color" = c(en = "Color", ko = "컬러"),
    "black and white" = c(en = "Black and white", ko = "흑백"),
    "survival function" = c(en = "Survival function", ko = "생존함수"),
    "1 - survival function" = c(en = "1 - Survival function", ko = "1 - 생존함수"),
    "cumulative hazard" = c(en = "Cumulative hazard", ko = "누적위험함수"),
    "log survival" = c(en = "Log survival", ko = "로그 생존함수"),
    "kaplan-meier survival time summary" = c(en = "Kaplan-Meier survival time summary", ko = "Kaplan-Meier 생존시간 요약"),
    "group comparison test" = c(en = "Group comparison test", ko = "집단 비교 검정"),
    "post-hoc pairwise comparison" = c(en = "Post-hoc pairwise comparison", ko = "사후 쌍별 비교"),
    "complete step 2 in the data tab before setting up survival analysis." = c(
      en = "Complete Step 2 in the Data tab before setting up survival analysis.",
      ko = "생존분석 설정 전에 데이터 탭의 Step 2를 완료하세요."
    )
  )
  value <- labels[[key]]
  if (is.null(value)) return(as.character(text))
  value[[language]] %||% value[["en"]]
}

survival_selected_names <- function(selected_names) {
  values <- as.character(selected_names %||% character(0))
  values[!is.na(values) & nzchar(values)]
}

survival_available_variable_names <- function(selected_names = character(0), data = NULL, variable_table = NULL) {
  data_names <- names(data %||% data.frame())
  selected <- survival_selected_names(selected_names)
  if (length(data_names)) {
    selected_in_data <- intersect(selected, data_names)
    return(if (length(selected_in_data)) selected_in_data else data_names)
  }
  if (length(selected)) return(selected)
  if (is.data.frame(variable_table) && "name" %in% names(variable_table)) {
    return(survival_selected_names(variable_table$name))
  }
  character(0)
}

survival_recommend <- function(settings) {
  value <- function(name, default = "") {
    item <- as.character(settings[[name]] %||% default)
    if (length(item) == 0 || is.na(item[[1]])) default else item[[1]]
  }
  objective <- value("objective", "group_comparison")
  data_shape <- value("data_shape", "single_record")
  event_structure <- value("event_structure", "single")
  estimand <- value("competing_estimand")
  time_dependent <- isTRUE(settings$time_dependent)
  result <- function(status, primary, rule_ids, reason_codes, target_tab = NULL,
                     alternatives = character(0), warnings = character(0),
                     confirmations = character(0), blocked_by = character(0)) {
    list(status = status, primary = primary, rule_ids = rule_ids,
         reason_codes = reason_codes, alternatives = alternatives,
         warnings = warnings, confirmations = confirmations,
         blocked_by = blocked_by, target_tab = target_tab)
  }

  if (identical(data_shape, "interval_censored")) {
    return(result("unsupported", "Interval-censored survival model", "U04", "interval_censoring_not_supported", blocked_by = "Interval-censored data are not supported in survival v1."))
  }
  if (objective == "prediction") {
    return(result("unsupported", "Survival prediction model", "U01", "prediction_not_supported", blocked_by = "Prediction modelling and validation are not supported in survival v1."))
  }
  if (objective == "recurrent" || event_structure == "recurrent") {
    return(result("unsupported", "Recurrent-event model", "U02", "recurrent_events_not_supported", blocked_by = "Recurrent-event models are not supported in survival v1."))
  }
  if (objective == "state_transition" || event_structure == "multistate") {
    return(result("unsupported", "Multi-state model", "U03", "multistate_not_supported", blocked_by = "Multi-state models are not supported in survival v1."))
  }
  if (data_shape == "start_stop" && objective == "association" && time_dependent) {
    return(result("ready", "Time-dependent Cox regression", c("S03", "A06"), "start_stop_time_dependent_association", "analysis_survival_cox", warnings = "Subject-level cluster-robust standard errors will be used."))
  }
  if (data_shape == "start_stop") {
    return(result("needs_confirmation", "Time-dependent Cox regression", "B11", "confirm_time_dependent_structure", confirmations = "Confirm that the start-stop rows represent time-dependent covariate intervals."))
  }
  if (time_dependent) {
    return(result("unsupported", "Time-dependent Cox model", "U05", "time_dependent_not_supported", blocked_by = "Start-stop and time-dependent covariates require a later survival module."))
  }
  if (data_shape == "entry_exit" && (event_structure == "competing" || objective == "competing")) {
    return(result("unsupported", "Delayed-entry competing-risk model", "U06", "delayed_entry_competing_not_supported", blocked_by = "The current CIF/Fine-Gray engine does not support delayed entry."))
  }
  if (event_structure == "competing" || objective == "competing") {
    if (objective == "group_comparison") {
      return(result("ready", "Cumulative incidence function and Gray test", "G02", "competing_group_comparison", "analysis_survival_competing", alternatives = "Cause-specific summaries"))
    }
    if (!estimand %in% c("cumulative_incidence", "cause_specific", "both")) {
      return(result("needs_confirmation", "Competing-risk regression", "C07", "estimand_required", confirmations = "Choose whether the target is cumulative incidence, cause-specific hazard, or both."))
    }
    if (estimand == "cause_specific") {
      return(result("ready", "Cause-specific Cox regression (HR)", "A03", "cause_specific_hazard", "analysis_survival_competing", alternatives = "Fine-Gray regression (sHR)"))
    }
    if (estimand == "cumulative_incidence") {
      return(result("ready", "Fine-Gray regression (sHR)", "A04", "cumulative_incidence_estimand", "analysis_survival_competing", alternatives = "Cause-specific Cox regression (HR)"))
    }
    return(result("ready", "Cause-specific Cox and Fine-Gray regression", "A05", "both_competing_estimands", "analysis_survival_competing"))
  }
  if (objective == "association") {
    return(result("ready", "Cox proportional hazards regression", "A01", "single_event_association", "analysis_survival_cox", alternatives = "Kaplan-Meier descriptive curves"))
  }
  result("ready", "Kaplan-Meier, log-rank test, and RMST", "G01", "single_event_group_comparison", "analysis_survival_km", alternatives = "Cox regression for adjusted association")
}

survival_contract_settings <- function(values) {
  scalar <- function(name, default = "") {
    item <- as.character(values[[name]] %||% default)
    if (length(item) == 0 || is.na(item[[1]])) default else trimws(item[[1]])
  }
  shape <- scalar("data_shape", "single_record")
  unit <- scalar("time_unit")
  custom_unit <- scalar("custom_time_unit")
  if (unit == "other") unit <- custom_unit
  supplied_map <- values$event_map
  if (is.data.frame(supplied_map) && all(c("raw_value", "role") %in% names(supplied_map))) {
    event_map <- supplied_map
    if (!"label" %in% names(event_map)) event_map$label <- as.character(event_map$raw_value)
    interest_values <- as.character(event_map$raw_value[event_map$role == "event_of_interest"])
    event_value <- if (length(interest_values)) interest_values[[1]] else ""
  } else {
    event_value <- scalar("event_value", "1")
    censored_value <- scalar("censored_value", "0")
    competing_values <- trimws(strsplit(scalar("competing_values"), ",", fixed = TRUE)[[1]])
    competing_values <- competing_values[nzchar(competing_values)]
    raw_values <- c(censored_value, event_value, competing_values)
    event_map <- data.frame(raw_value = raw_values, role = c("censored", "event_of_interest", rep("competing_event", length(competing_values))), label = c("Censored", "Event of interest", rep("Competing event", length(competing_values))), stringsAsFactors = FALSE)
  }
  list(
    schema_version = "survival-v1",
    objective = scalar("objective", "group_comparison"),
    event_structure = scalar("event_structure", "single"),
    data_shape = shape,
    time_origin = scalar("time_origin"),
    time_unit = unit,
    roles = list(
      time = scalar("time"), entry = scalar("entry"), start = scalar("start"),
      stop = scalar("stop"), event = scalar("event"), subject_id = scalar("subject_id"),
      cluster_id = NULL, group = scalar("group"), covariates = survival_selected_names(values$covariates)
    ),
    event_map = event_map,
    event_map_confirmed = isTRUE(values$event_map_confirmed %||% TRUE),
    event_of_interest = event_value,
    missing_strategy = "complete_case"
  )
}

survival_contract_transfer <- function(settings, recommendation) {
  roles <- settings$roles %||% list()
  event_map <- settings$event_map %||% data.frame()
  mapped <- function(role, default = "") {
    if (!is.data.frame(event_map) || !all(c("raw_value", "role") %in% names(event_map))) return(default)
    values <- as.character(event_map$raw_value[event_map$role == role])
    if (length(values)) values else default
  }
  time <- if (identical(settings$data_shape, "start_stop")) roles$stop %||% "" else roles$time %||% ""
  rule <- as.character(recommendation$rule_ids %||% "")
  regression <- if ("A03" %in% rule) "cause_specific" else if ("A04" %in% rule) "fine_gray" else if ("A05" %in% rule) "both" else "none"
  list(
    target_tab = recommendation$target_tab, data_shape = as.character(settings$data_shape %||% "single_record"), entry = as.character(roles$entry %||% ""),
    start = as.character(roles$start %||% ""), stop = as.character(roles$stop %||% ""),
    time = as.character(time %||% ""), event = as.character(roles$event %||% ""),
    subject_id = as.character(roles$subject_id %||% ""), group = as.character(roles$group %||% ""),
    covariates = survival_selected_names(roles$covariates),
    event_value = mapped("event_of_interest", "1")[[1]],
    censored_value = mapped("censored", "0")[[1]],
    competing_values = paste(mapped("competing_event", character(0)), collapse = ", "),
    regression = regression, time_origin = settings$time_origin, time_unit = settings$time_unit
  )
}

survival_contract_preflight <- function(data, settings) {
  checked <- survival_preflight(data, settings)
  metadata <- list()
  if (!nzchar(trimws(as.character(settings$time_origin %||% "")))) {
    metadata[[length(metadata) + 1L]] <- survival_issue("block", "missing_time_origin", message = "Specify the time origin.")
  }
  if (!nzchar(trimws(as.character(settings$time_unit %||% "")))) {
    metadata[[length(metadata) + 1L]] <- survival_issue("block", "missing_time_unit", message = "Select or enter a time unit.")
  }
  if (!isTRUE(settings$event_map_confirmed)) {
    metadata[[length(metadata) + 1L]] <- survival_issue("block", "event_map_not_confirmed", message = "Confirm the meaning of every observed event code.")
  }
  interest_count <- if (is.data.frame(settings$event_map) && "role" %in% names(settings$event_map)) sum(settings$event_map$role == "event_of_interest", na.rm = TRUE) else 0L
  if (interest_count != 1L) metadata[[length(metadata) + 1L]] <- survival_issue("block", "event_of_interest_count", n = interest_count, message = "Select exactly one event-of-interest code for survival v1.")
  competing_count <- if (is.data.frame(settings$event_map) && "role" %in% names(settings$event_map)) sum(settings$event_map$role == "competing_event", na.rm = TRUE) else 0L
  if ((identical(settings$event_structure, "competing") || identical(settings$objective, "competing")) && competing_count == 0L) metadata[[length(metadata) + 1L]] <- survival_issue("block", "missing_competing_event_code", message = "Assign at least one observed code as a competing event.")
  if (length(metadata)) checked$issues <- survival_bind_issues(c(list(checked$issues), metadata))
  checked$ok <- !any(checked$issues$severity %in% c("error", "block"))
  checked
}

survival_empty_issues <- function() {
  data.frame(
    severity = character(0),
    code = character(0),
    variable = character(0),
    n = integer(0),
    message = character(0),
    action = character(0),
    stringsAsFactors = FALSE
  )
}

survival_issue <- function(severity, code, variable = "", n = NA_integer_, message = "", action = "") {
  data.frame(
    severity = as.character(severity),
    code = as.character(code),
    variable = as.character(variable %||% ""),
    n = as.integer(n),
    message = as.character(message),
    action = as.character(action),
    stringsAsFactors = FALSE
  )
}

survival_bind_issues <- function(issues) {
  issues <- Filter(function(item) is.data.frame(item) && nrow(item) > 0, issues)
  if (length(issues) == 0) return(survival_empty_issues())
  do.call(rbind, issues)
}

survival_coerce_duration <- function(values) {
  text <- trimws(as.character(values))
  missing <- is.na(values) | !nzchar(text)
  unsupported_class <- inherits(values, c("Date", "POSIXct", "POSIXlt")) || is.logical(values) || is.list(values)
  numeric_values <- if (unsupported_class) {
    rep(NA_real_, length(values))
  } else if (is.factor(values) || is.character(values)) {
    suppressWarnings(as.numeric(text))
  } else {
    suppressWarnings(as.numeric(values))
  }
  invalid_encoding <- !missing & (unsupported_class | is.na(numeric_values))
  list(values = numeric_values, missing = missing, invalid_encoding = invalid_encoding)
}

survival_legacy_settings <- function(
  time,
  event,
  event_value = "1",
  group = "",
  covariates = character(0),
  variable_info = NULL
) {
  time <- survival_selected_names(time)
  event <- survival_selected_names(event)
  group <- survival_selected_names(group)
  covariates <- survival_selected_names(covariates)
  list(
    schema_version = "survival-v1",
    objective = if (length(covariates) > 0) "association" else "group_comparison",
    data_shape = "single_record",
    time_origin = "Not specified (legacy setup)",
    time_unit = "Not specified (legacy setup)",
    roles = list(
      time = if (length(time) > 0) time[[1]] else "",
      entry = NULL,
      start = NULL,
      stop = NULL,
      event = if (length(event) > 0) event[[1]] else "",
      subject_id = NULL,
      cluster_id = NULL,
      group = if (length(group) > 0) group[[1]] else NULL,
      covariates = covariates
    ),
    event_map = NULL,
    event_of_interest = trimws(as.character(event_value %||% "1")[[1]]),
    missing_strategy = "complete_case",
    legacy = TRUE,
    variable_info = variable_info
  )
}

survival_normalize_event_map <- function(values, event_map = NULL, event_of_interest = "1") {
  observed <- unique(trimws(as.character(values[!is.na(values)])))
  observed <- observed[nzchar(observed)]
  interest <- trimws(as.character(event_of_interest %||% "1")[[1]])
  if (is.data.frame(event_map) && all(c("raw_value", "role") %in% names(event_map))) {
    map <- event_map[, intersect(c("raw_value", "role", "label"), names(event_map)), drop = FALSE]
    map$raw_value <- trimws(as.character(map$raw_value))
    map$role <- trimws(tolower(as.character(map$role)))
    if (!"label" %in% names(map)) map$label <- map$raw_value
    map$label <- as.character(map$label)
    return(map)
  }
  data.frame(
    raw_value = observed,
    role = ifelse(observed == interest, "event_of_interest", "censored"),
    label = observed,
    stringsAsFactors = FALSE
  )
}

survival_preflight <- function(data, settings) {
  if (!is.data.frame(data)) stop("No data is loaded.")
  if (!is.list(settings)) stop("Survival settings must be a list.")
  roles <- settings$roles %||% list()
  shape <- as.character(settings$data_shape %||% "single_record")[[1]]
  time <- as.character(roles$time %||% "")[[1]]
  event <- as.character(roles$event %||% "")[[1]]
  entry <- as.character(roles$entry %||% "")[[1]]
  start <- as.character(roles$start %||% "")[[1]]
  stop <- as.character(roles$stop %||% "")[[1]]
  subject_id <- as.character(roles$subject_id %||% "")[[1]]
  group <- survival_selected_names(roles$group)
  group <- if (length(group) > 0) group[[1]] else ""
  covariates <- survival_selected_names(roles$covariates)
  role_variables <- unique(c(time, event, entry, start, stop, subject_id, group, covariates))
  role_variables <- role_variables[nzchar(role_variables)]
  issues <- list()
  required <- switch(shape,
    entry_exit = c(entry, time, event),
    start_stop = c(subject_id, start, stop, event),
    c(time, event)
  )
  required <- required[nzchar(required)]
  if ((identical(shape, "single_record") || identical(shape, "entry_exit")) && !nzchar(time)) {
    issues[[length(issues) + 1L]] <- survival_issue("error", "missing_time_role", message = "Select a time variable.")
  }
  if (!nzchar(event)) {
    issues[[length(issues) + 1L]] <- survival_issue("error", "missing_event_role", message = "Select an event variable.")
  }
  if (identical(shape, "entry_exit") && !nzchar(entry)) {
    issues[[length(issues) + 1L]] <- survival_issue("error", "missing_entry_role", message = "Select an entry-time variable.")
  }
  if (identical(shape, "start_stop")) {
    if (!nzchar(subject_id)) issues[[length(issues) + 1L]] <- survival_issue("block", "missing_subject_id", message = "Start-stop data require a subject ID.")
    if (!nzchar(start) || !nzchar(stop)) issues[[length(issues) + 1L]] <- survival_issue("error", "missing_interval_role", message = "Select start and stop variables.")
  }
  missing_columns <- setdiff(role_variables, names(data))
  if (length(missing_columns) > 0) {
    issues[[length(issues) + 1L]] <- survival_issue(
      "error", "missing_columns", paste(missing_columns, collapse = ", "), length(missing_columns),
      paste("Variables not found:", paste(missing_columns, collapse = ", ")),
      "Select variables that exist in the current data."
    )
  }
  duplicate_required <- unique(required[duplicated(required)])
  if (length(duplicate_required) > 0) {
    issues[[length(issues) + 1L]] <- survival_issue(
      "error", "conflicting_roles", paste(duplicate_required, collapse = ", "), length(duplicate_required),
      "The same variable is assigned to conflicting required survival roles.",
      "Assign a different variable to each required role."
    )
  }
  current_issues <- survival_bind_issues(issues)
  if (any(current_issues$severity %in% c("error", "block")) && length(missing_columns) > 0) {
    return(list(ok = FALSE, schema = shape, settings = settings, analysis_data = data.frame(), row_audit = data.frame(), counts = list(source_rows = nrow(data), source_subjects = nrow(data), analysis_rows = 0L, analysis_subjects = 0L, events = 0L, competing_events = 0L, censored = 0L), issues = current_issues, transformations = list()))
  }
  frame <- data[, role_variables, drop = FALSE]
  row_id <- seq_len(nrow(frame))
  reasons <- vector("list", nrow(frame))
  add_reason <- function(mask, code) {
    indexes <- which(!is.na(mask) & mask)
    for (index in indexes) reasons[[index]] <<- unique(c(reasons[[index]], code))
  }
  numeric_roles <- unique(c(time, entry, start, stop))
  numeric_roles <- numeric_roles[nzchar(numeric_roles) & numeric_roles %in% names(frame)]
  duration_status <- list()
  for (name in numeric_roles) {
    duration_status[[name]] <- survival_coerce_duration(frame[[name]])
    frame[[name]] <- duration_status[[name]]$values
    invalid_encoding <- duration_status[[name]]$invalid_encoding
    if (any(invalid_encoding)) {
      add_reason(invalid_encoding, "invalid_time_encoding")
      issues[[length(issues) + 1L]] <- survival_issue(
        "block", "invalid_time_encoding", name, sum(invalid_encoding),
        "Time-role values must be numeric durations; date/time fields must be converted to elapsed durations before analysis.",
        "Convert the selected time variable to a numeric elapsed duration and verify its unit."
      )
    }
  }
  if (nzchar(time) && time %in% names(frame)) {
    add_reason(duration_status[[time]]$missing, "missing_time")
    invalid_time <- !is.na(frame[[time]]) & (!is.finite(frame[[time]]) | frame[[time]] < 0)
    add_reason(invalid_time, "invalid_time")
    if (any(invalid_time)) issues[[length(issues) + 1L]] <- survival_issue("block", "invalid_time", time, sum(invalid_time), "Time values must be finite and nonnegative.", "Correct the time values.")
  }
  if (identical(shape, "entry_exit") && all(c(entry, time) %in% names(frame))) {
    add_reason(duration_status[[entry]]$missing, "missing_entry")
    invalid_order <- !is.na(frame[[entry]]) & !is.na(frame[[time]]) & frame[[entry]] >= frame[[time]]
    add_reason(invalid_order, "invalid_time_order")
    if (any(invalid_order)) issues[[length(issues) + 1L]] <- survival_issue("block", "entry_not_before_exit", paste(entry, time, sep = ", "), sum(invalid_order), "Entry time must be earlier than exit time.", "Correct entry and exit times.")
  }
  if (identical(shape, "start_stop") && all(c(start, stop) %in% names(frame))) {
    add_reason(duration_status[[start]]$missing | duration_status[[stop]]$missing, "missing_interval")
    invalid_interval <- !is.na(frame[[start]]) & (!is.finite(frame[[start]]) | frame[[start]] < 0) |
      !is.na(frame[[stop]]) & (!is.finite(frame[[stop]]) | frame[[stop]] < 0)
    add_reason(invalid_interval, "invalid_interval_time")
    if (any(invalid_interval)) issues[[length(issues) + 1L]] <- survival_issue("block", "invalid_interval_time", paste(start, stop, sep = ", "), sum(invalid_interval), "Interval times must be finite and nonnegative.", "Correct the start-stop values.")
    invalid_order <- !is.na(frame[[start]]) & !is.na(frame[[stop]]) & frame[[start]] >= frame[[stop]]
    add_reason(invalid_order, "invalid_time_order")
    if (any(invalid_order)) issues[[length(issues) + 1L]] <- survival_issue("block", "start_not_before_stop", paste(start, stop, sep = ", "), sum(invalid_order), "Start time must be earlier than stop time.", "Correct the risk intervals.")
    if (nzchar(subject_id) && subject_id %in% names(frame)) {
      missing_id <- is.na(frame[[subject_id]]) | !nzchar(trimws(as.character(frame[[subject_id]])))
      add_reason(missing_id, "missing_subject_id_value")
      if (any(missing_id)) issues[[length(issues) + 1L]] <- survival_issue("block", "missing_subject_id_value", subject_id, sum(missing_id), "Every start-stop row requires a subject ID.", "Complete the subject IDs.")
    }
  }
  if (nzchar(event) && event %in% names(frame)) {
    raw_event <- frame[[event]]
    add_reason(is.na(raw_event) | !nzchar(trimws(as.character(raw_event))), "missing_event")
    event_map <- survival_normalize_event_map(raw_event, settings$event_map, settings$event_of_interest)
    allowed_roles <- c("censored", "event_of_interest", "competing_event", "other_state", "exclude", "unknown")
    duplicate_map <- duplicated(event_map$raw_value) | duplicated(event_map$raw_value, fromLast = TRUE)
    invalid_map <- !event_map$role %in% allowed_roles
    observed <- unique(trimws(as.character(raw_event[!is.na(raw_event)])))
    unmapped <- setdiff(observed[nzchar(observed)], event_map$raw_value)
    if (any(duplicate_map) || any(invalid_map) || length(unmapped) > 0 || any(event_map$role == "unknown")) {
      issues[[length(issues) + 1L]] <- survival_issue("block", "invalid_event_map", event, length(unique(c(event_map$raw_value[duplicate_map | invalid_map | event_map$role == "unknown"], unmapped))), "Every observed event code must have one valid role.", "Review the event-code mapping.")
    }
    role_lookup <- stats::setNames(event_map$role, event_map$raw_value)
    event_roles <- unname(role_lookup[trimws(as.character(raw_event))])
    add_reason(event_roles == "exclude", "excluded_event_code")
    competing_event <- event_roles == "competing_event"
    competing_workflow <- identical(as.character(settings$objective %||% "")[[1]], "competing") ||
      identical(as.character(settings$event_structure %||% "")[[1]], "competing")
    if (any(competing_event, na.rm = TRUE) && !competing_workflow) {
      add_reason(competing_event, "competing_event_requires_competing_risk")
      issues[[length(issues) + 1L]] <- survival_issue(
        "block", "competing_event_requires_competing_risk", event, sum(competing_event, na.rm = TRUE),
        "Competing-event codes cannot be analyzed as ordinary censoring in the standard Kaplan-Meier or Cox workflow.",
        "Use the Competing Risks analysis for cumulative incidence, Gray's test, cause-specific Cox, or Fine-Gray regression."
      )
    }
    other_state <- event_roles == "other_state"
    add_reason(other_state, "unsupported_other_state")
    if (any(other_state, na.rm = TRUE)) {
      issues[[length(issues) + 1L]] <- survival_issue(
        "block", "unsupported_other_state", event, sum(other_state, na.rm = TRUE),
        "Event codes assigned as other_state require a multi-state survival model, which is not supported in survival v1.",
        "Use a supported terminal-event definition or a dedicated multi-state analysis."
      )
    }
    frame[[paste0(event, "__raw")]] <- raw_event
    frame[[event]] <- event_roles == "event_of_interest"
  } else {
    event_map <- data.frame(raw_value = character(), role = character(), label = character(), stringsAsFactors = FALSE)
    event_roles <- rep(NA_character_, nrow(frame))
  }
  if (identical(shape, "start_stop") && all(c(subject_id, start, stop, event) %in% names(frame))) {
    usable <- !is.na(frame[[subject_id]]) & is.finite(frame[[start]]) & is.finite(frame[[stop]]) & frame[[start]] < frame[[stop]]
    subject_rows <- split(which(usable), as.character(frame[[subject_id]][usable]), drop = TRUE)
    overlap_rows <- integer(0)
    post_event_rows <- integer(0)
    extra_event_rows <- integer(0)
    unordered_subjects <- character(0)
    for (id_value in names(subject_rows)) {
      indexes <- subject_rows[[id_value]]
      if (length(indexes) > 1L && is.unsorted(frame[[start]][indexes], strictly = FALSE)) unordered_subjects <- c(unordered_subjects, id_value)
      ordered <- indexes[order(frame[[start]][indexes], frame[[stop]][indexes], indexes)]
      if (length(ordered) > 1L) {
        prior_max_stop <- cummax(frame[[stop]][ordered])[-length(ordered)]
        overlapping <- frame[[start]][ordered[-1L]] < prior_max_stop
        overlap_rows <- c(overlap_rows, ordered[-1L][overlapping])
      }
      event_indexes <- ordered[frame[[event]][ordered] %in% TRUE]
      if (length(event_indexes) > 1L) extra_event_rows <- c(extra_event_rows, event_indexes[-1L])
      if (length(event_indexes) > 0L) {
        first_event <- event_indexes[[1]]
        later <- ordered[frame[[start]][ordered] >= frame[[stop]][first_event] & ordered != first_event]
        post_event_rows <- c(post_event_rows, later)
      }
    }
    overlap_rows <- unique(overlap_rows)
    post_event_rows <- unique(post_event_rows)
    extra_event_rows <- unique(extra_event_rows)
    if (length(overlap_rows)) {
      add_reason(seq_len(nrow(frame)) %in% overlap_rows, "overlapping_interval")
      issues[[length(issues) + 1L]] <- survival_issue("block", "overlapping_intervals", paste(start, stop, sep = ", "), length(overlap_rows), "Risk intervals overlap within a subject.", "Remove or reconcile overlapping intervals.")
    }
    if (length(post_event_rows)) {
      add_reason(seq_len(nrow(frame)) %in% post_event_rows, "interval_after_event")
      issues[[length(issues) + 1L]] <- survival_issue("block", "interval_after_event", subject_id, length(post_event_rows), "Risk intervals remain after a subject's event.", "End follow-up at the first terminal event or choose a recurrent-event model.")
    }
    if (length(extra_event_rows)) {
      add_reason(seq_len(nrow(frame)) %in% extra_event_rows, "multiple_subject_events")
      issues[[length(issues) + 1L]] <- survival_issue("block", "multiple_subject_events", event, length(extra_event_rows), "A subject has multiple event rows in a standard time-dependent Cox setup.", "Use the first terminal event or a recurrent-event model.")
    }
    if (length(unordered_subjects)) issues[[length(issues) + 1L]] <- survival_issue("warning", "intervals_not_source_order", subject_id, length(unique(unordered_subjects)), "Some subject intervals are not stored in chronological order; analysis uses start-time order.", "Review the source row order.")
  }
  for (name in c(group, covariates)) {
    if (!nzchar(name) || !name %in% names(frame)) next
    add_reason(is.na(frame[[name]]), if (identical(name, group)) "missing_group" else "missing_covariate")
  }
  excluded <- lengths(reasons) > 0
  row_audit <- data.frame(
    source_row = row_id,
    included = !excluded,
    exclusion_reasons = vapply(reasons, paste, collapse = ";", character(1)),
    stringsAsFactors = FALSE
  )
  analysis_data <- frame[!excluded, , drop = FALSE]
  if (identical(shape, "start_stop") && nrow(analysis_data) > 1L && all(c(subject_id, start, stop) %in% names(analysis_data))) {
    analysis_data <- analysis_data[order(as.character(analysis_data[[subject_id]]), analysis_data[[start]], analysis_data[[stop]]), , drop = FALSE]
  }
  if (nzchar(group) && group %in% names(analysis_data)) analysis_data[[group]] <- droplevels(as.factor(analysis_data[[group]]))
  measurements <- character(0)
  variable_info <- settings$variable_info
  if (is.data.frame(variable_info) && all(c("name", "measurement") %in% names(variable_info))) measurements <- stats::setNames(tolower(as.character(variable_info$measurement)), as.character(variable_info$name))
  for (name in covariates) {
    if (!name %in% names(analysis_data)) next
    measurement <- named_value(measurements, name, "")
    if (measurement %in% c("binary", "category", "ordered", "ordinal", "nominal")) analysis_data[[name]] <- as.factor(analysis_data[[name]])
  }
  interest_events <- if (nzchar(event) && event %in% names(analysis_data)) sum(analysis_data[[event]], na.rm = TRUE) else 0L
  competing_events <- sum(event_roles[!excluded] == "competing_event", na.rm = TRUE)
  censored <- sum(event_roles[!excluded] == "censored", na.rm = TRUE)
  if (nrow(analysis_data) == 0) issues[[length(issues) + 1L]] <- survival_issue("block", "no_analysis_rows", n = nrow(data), message = "No complete valid rows remain after applying survival variables.", action = "Review the exclusion reasons.")
  if (nrow(analysis_data) > 0 && interest_events == 0) issues[[length(issues) + 1L]] <- survival_issue("block", "no_events", event, 0L, "No events were found for the selected event of interest.", "Review the event-code mapping.")
  if (isTRUE(settings$legacy) && nrow(event_map) >= 3L) issues[[length(issues) + 1L]] <- survival_issue("warning", "potential_competing_events", event, nrow(event_map) - 1L, "Multiple non-interest event codes were treated as censoring by the legacy setup.", "Confirm whether any code represents a competing event.")
  issues_table <- survival_bind_issues(issues)
  source_subjects <- if (nzchar(subject_id) && subject_id %in% names(data)) length(unique(data[[subject_id]][!is.na(data[[subject_id]])])) else nrow(data)
  analysis_subjects <- if (nzchar(subject_id) && subject_id %in% names(analysis_data)) length(unique(analysis_data[[subject_id]][!is.na(analysis_data[[subject_id]])])) else nrow(analysis_data)
  list(
    ok = !any(issues_table$severity %in% c("error", "block")),
    schema = shape,
    settings = settings,
    analysis_data = analysis_data,
    row_audit = row_audit,
    counts = list(source_rows = nrow(data), source_subjects = source_subjects, analysis_rows = nrow(analysis_data), analysis_subjects = analysis_subjects, events = as.integer(interest_events), competing_events = as.integer(competing_events), censored = as.integer(censored)),
    issues = issues_table,
    transformations = list(event_map = event_map, raw_event_column = if (nzchar(event)) paste0(event, "__raw") else "", event_column = event)
  )
}

survival_preflight_stop <- function(preflight) {
  if (isTRUE(preflight$ok)) return(invisible(preflight))
  blocking <- preflight$issues[preflight$issues$severity %in% c("error", "block"), , drop = FALSE]
  message <- if (nrow(blocking) > 0) paste(unique(blocking$message), collapse = " ") else "Survival data validation failed."
  stop(message, call. = FALSE)
}

survival_variable_choices <- function(selected_names, include_none = FALSE, language = statedu_initial_language()) {
  names <- survival_selected_names(selected_names)
  choices <- stats::setNames(names, names)
  if (isTRUE(include_none)) {
    choices <- c(stats::setNames("", survival_ui_text("No group", language)), choices)
  }
  choices
}

survival_parse_event <- function(values, event_value = "1") {
  values_chr <- trimws(as.character(values))
  event_chr <- trimws(as.character(event_value %||% "1")[[1]])
  if (!nzchar(event_chr)) {
    numeric_values <- suppressWarnings(as.numeric(values_chr))
    return(!is.na(numeric_values) & numeric_values > 0)
  }
  values_chr == event_chr
}

survival_analysis_data <- function(data, time, event, event_value = "1", group = "", covariates = character(0), variable_info = NULL) {
  settings <- survival_legacy_settings(time, event, event_value, group, covariates, variable_info)
  preflight <- survival_preflight(data, settings)
  survival_preflight_stop(preflight)
  preflight$analysis_data
}

survival_display_name <- function(name, variable_table = NULL, labels = character(0)) {
  display_variable_name_static(name, variable_table, labels, label_only = TRUE)
}

survival_format_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0) return("")
  x <- x[[1]]
  if (is.na(x)) return("")
  if (!is.finite(x)) return(as.character(x))
  format_decimal3(x)
}

survival_format_ci <- function(lower, upper) {
  lower <- suppressWarnings(as.numeric(lower))
  upper <- suppressWarnings(as.numeric(upper))
  if ((length(lower) == 0 || is.na(lower)) && (length(upper) == 0 || is.na(upper))) return("")
  lower_text <- if (length(lower) == 0 || is.na(lower)) "NE" else survival_format_number(lower)
  upper_text <- if (length(upper) == 0 || is.na(upper)) "NE" else survival_format_number(upper)
  sprintf("(%s-%s)", lower_text, upper_text)
}

survival_p <- function(p) {
  if (length(p) == 0 || is.na(p)) "" else format_p(p)
}

survival_parse_times <- function(value) {
  raw <- unlist(strsplit(as.character(value %||% ""), "[,;[:space:]]+"))
  times <- suppressWarnings(as.numeric(raw))
  sort(unique(times[is.finite(times) & times >= 0]))
}

survival_parse_times_strict <- function(value, label = "Time points") {
  text <- trimws(as.character(value %||% "")[[1]])
  if (!nzchar(text)) return(numeric(0))
  tokens <- unlist(strsplit(text, "[,;[:space:]]+"))
  values <- suppressWarnings(as.numeric(tokens))
  if (!length(values) || any(!is.finite(values)) || any(values < 0)) {
    stop(sprintf("%s must contain only nonnegative finite numbers separated by commas, semicolons, or spaces.", label))
  }
  sort(unique(values))
}

survival_default_risk_times <- function(time) {
  max_time <- suppressWarnings(max(as.numeric(time), na.rm = TRUE))
  if (!is.finite(max_time) || max_time <= 0) return(numeric(0))
  pretty(c(0, max_time), n = 5)
}

survival_fit_label <- function(fit) {
  paste(capture.output(print(fit$call)), collapse = " ")
}

survival_km_test_label <- function(method) {
  method <- as.character(method %||% "logrank")[[1]]
  switch(method,
    breslow = "Breslow",
    tarone_ware = "Tarone-Ware",
    "Log-rank"
  )
}

survival_weighted_rank_test <- function(data, time, event, group, method = "logrank", entry = "") {
  if (!nzchar(as.character(group %||% ""))) return(NULL)
  group_values <- droplevels(as.factor(data[[group]]))
  levels <- levels(group_values)
  if (length(levels) < 2L) return(NULL)
  times <- sort(unique(data[[time]][isTRUE(data[[event]]) | data[[event]]]))
  if (length(times) == 0) return(NULL)
  k <- length(levels)
  observed_minus_expected <- numeric(k)
  variance <- matrix(0, nrow = k, ncol = k)
  for (event_time in times) {
    risk <- data[[time]] >= event_time
    if (nzchar(as.character(entry %||% ""))) risk <- risk & data[[entry]] < event_time
    event_at_time <- data[[time]] == event_time & data[[event]]
    n_total <- sum(risk)
    d_total <- sum(event_at_time)
    if (n_total <= 1 || d_total <= 0) next
    n_group <- as.numeric(table(factor(group_values[risk], levels = levels)))
    d_group <- as.numeric(table(factor(group_values[event_at_time], levels = levels)))
    expected <- d_total * n_group / n_total
    weight <- switch(as.character(method %||% "logrank")[[1]],
      breslow = n_total,
      tarone_ware = sqrt(n_total),
      1
    )
    observed_minus_expected <- observed_minus_expected + weight * (d_group - expected)
    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        value <- if (i == j) {
          d_total * (n_total - d_total) * n_group[[i]] * (n_total - n_group[[i]]) / (n_total^2 * (n_total - 1))
        } else {
          -d_total * (n_total - d_total) * n_group[[i]] * n_group[[j]] / (n_total^2 * (n_total - 1))
        }
        variance[i, j] <- variance[i, j] + weight^2 * value
      }
    }
  }
  keep <- seq_len(k - 1L)
  chisq <- tryCatch(
    as.numeric(t(observed_minus_expected[keep]) %*% qr.solve(variance[keep, keep, drop = FALSE], observed_minus_expected[keep])),
    error = function(e) NA_real_
  )
  if (!is.finite(chisq)) return(NULL)
  list(
    method = as.character(method %||% "logrank")[[1]],
    label = survival_km_test_label(method),
    chisq = chisq,
    df = k - 1L,
    p = stats::pchisq(chisq, df = k - 1L, lower.tail = FALSE)
  )
}

survival_life_table <- function(data, time, event, group = "", breaks = numeric(0)) {
  max_time <- suppressWarnings(max(data[[time]], na.rm = TRUE))
  if (!is.finite(max_time) || max_time <= 0) return(data.frame())
  breaks <- sort(unique(suppressWarnings(as.numeric(breaks))))
  breaks <- breaks[is.finite(breaks) & breaks > 0 & breaks < max_time]
  breaks <- unique(c(0, breaks, max_time))
  if (length(breaks) < 2L) breaks <- pretty(c(0, max_time), n = 5)
  groups <- if (nzchar(as.character(group %||% ""))) levels(droplevels(as.factor(data[[group]]))) else "All"
  rows <- list()
  for (group_value in groups) {
    group_data <- if (identical(group_value, "All")) data else data[as.factor(data[[group]]) == group_value, , drop = FALSE]
    survival_value <- 1
    for (index in seq_len(length(breaks) - 1L)) {
      start <- breaks[[index]]
      end <- breaks[[index + 1L]]
      in_interval <- group_data[[time]] > start & group_data[[time]] <= end
      at_risk <- sum(group_data[[time]] > start)
      events <- sum(in_interval & group_data[[event]], na.rm = TRUE)
      censored <- sum(in_interval & !group_data[[event]], na.rm = TRUE)
      effective <- at_risk - censored / 2
      q <- if (effective > 0) events / effective else NA_real_
      p <- if (is.finite(q)) max(0, 1 - q) else NA_real_
      if (is.finite(p)) survival_value <- survival_value * p
      rows[[length(rows) + 1L]] <- data.frame(
        Strata = group_value,
        Interval = sprintf("%s-%s", survival_format_number(start), survival_format_number(end)),
        `At risk` = at_risk,
        Events = events,
        Censored = censored,
        Survival = survival_value,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

survival_km_plot_transform <- function(values, plot_type = "survival", lower = NULL, upper = NULL) {
  plot_type <- as.character(plot_type %||% "survival")[[1]]
  eps <- .Machine$double.eps
  values <- pmin(pmax(as.numeric(values), eps), 1)
  if (identical(plot_type, "event")) {
    return(1 - values)
  }
  if (identical(plot_type, "cumhaz")) {
    return(-log(values))
  }
  if (identical(plot_type, "log_survival")) {
    return(log(values))
  }
  values
}

survival_km_plot_ci <- function(lower, upper, plot_type = "survival") {
  lower <- pmin(pmax(as.numeric(lower), .Machine$double.eps), 1)
  upper <- pmin(pmax(as.numeric(upper), .Machine$double.eps), 1)
  if (identical(plot_type, "event")) {
    return(list(lower = 1 - upper, upper = 1 - lower))
  }
  if (identical(plot_type, "cumhaz")) {
    return(list(lower = -log(upper), upper = -log(lower)))
  }
  if (identical(plot_type, "log_survival")) {
    return(list(lower = log(lower), upper = log(upper)))
  }
  list(lower = lower, upper = upper)
}

survival_km_plot_data <- function(fit, plot_type = "survival") {
  curve_summary <- summary(fit)
  strata <- as.character(curve_summary$strata %||% "All")
  if (length(strata) == 0) strata <- rep("All", length(curve_summary$time))
  curve <- data.frame(
    Time = as.numeric(curve_summary$time),
    Survival = as.numeric(curve_summary$surv),
    Lower = as.numeric(curve_summary$lower),
    Upper = as.numeric(curve_summary$upper),
    Strata = strata,
    stringsAsFactors = FALSE
  )
  if (nrow(curve) > 0) {
    zero_rows <- do.call(rbind, lapply(unique(curve$Strata), function(stratum) {
      data.frame(Time = 0, Survival = 1, Lower = 1, Upper = 1, Strata = stratum, stringsAsFactors = FALSE)
    }))
    curve <- rbind(zero_rows, curve)
  }
  ci <- survival_km_plot_ci(curve$Lower, curve$Upper, plot_type)
  curve$Value <- survival_km_plot_transform(curve$Survival, plot_type)
  curve$LowerValue <- ci$lower
  curve$UpperValue <- ci$upper

  censor_summary <- summary(fit, censored = TRUE)
  censor_strata <- as.character(censor_summary$strata %||% "All")
  if (length(censor_strata) == 0) censor_strata <- rep("All", length(censor_summary$time))
  censor <- data.frame(
    Time = as.numeric(censor_summary$time),
    Survival = as.numeric(censor_summary$surv),
    Censored = as.numeric(censor_summary$n.censor %||% rep(0, length(censor_summary$time))),
    Strata = censor_strata,
    stringsAsFactors = FALSE
  )
  censor <- censor[censor$Censored > 0, , drop = FALSE]
  if (nrow(censor) > 0) {
    censor$Value <- survival_km_plot_transform(censor$Survival, plot_type)
  }
  list(curve = curve, censor = censor)
}

survival_publication_palette <- function(n) {
  base <- c("#1F77B4", "#D62728", "#2CA02C", "#9467BD", "#FF7F0E", "#17BECF", "#8C564B", "#111111")
  rep(base, length.out = max(1L, n))
}

survival_bw_palette <- function(n) {
  if (n <= 1L) return("#111111")
  grDevices::gray.colors(n, start = 0.05, end = 0.65)
}

survival_bw_linetypes <- function(n) {
  base <- c("solid", "22", "42", "44", "13", "1343", "73", "2262")
  rep(base, length.out = max(1L, n))
}

survival_km_ggplot <- function(result, plot_type = "survival", figure_version = "color") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for survival plots.")
  }
  plot_type <- as.character(plot_type %||% "survival")[[1]]
  figure_version <- as.character(figure_version %||% "color")[[1]]
  if (!figure_version %in% c("color", "bw")) figure_version <- "color"
  plot_data <- survival_km_plot_data(result$fit, plot_type)
  curve <- plot_data$curve
  censor <- plot_data$censor
  strata_count <- length(unique(curve$Strata))
  palette <- if (identical(figure_version, "bw")) survival_bw_palette(strata_count) else survival_publication_palette(strata_count)
  y_label <- switch(plot_type,
    event = "1 - Survival probability",
    cumhaz = "Cumulative hazard",
    log_survival = "Log survival probability",
    "Survival probability"
  )
  caption <- if (exists("survival_plot_tail_caption", mode = "function")) survival_plot_tail_caption(result) else NULL
  p <- ggplot2::ggplot(curve, ggplot2::aes(x = Time, y = Value, color = Strata, group = Strata))
  if (identical(figure_version, "bw")) {
    p <- p +
      ggplot2::geom_step(ggplot2::aes(linetype = Strata), linewidth = 0.95) +
      ggplot2::scale_linetype_manual(values = survival_bw_linetypes(strata_count), drop = FALSE)
  } else {
    p <- p + ggplot2::geom_step(linewidth = 0.95)
  }
  p <- p +
    ggplot2::labs(x = result$time, y = y_label, color = NULL, linetype = NULL, shape = NULL, caption = caption) +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.45, colour = "#111111"),
      axis.ticks = ggplot2::element_line(linewidth = 0.45, colour = "#111111"),
      axis.ticks.length = grid::unit(3, "pt"),
      axis.title = ggplot2::element_text(size = 12, colour = "#111111"),
      axis.text = ggplot2::element_text(size = 10.5, colour = "#111111"),
      legend.position = if (strata_count > 1L) "bottom" else "none",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key.width = grid::unit(22, "pt"),
      legend.key.height = grid::unit(12, "pt"),
      legend.text = ggplot2::element_text(size = 10.5),
      legend.margin = ggplot2::margin(2, 0, 0, 0),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(linewidth = 1.2))
    )
  if (isTRUE(result$show_ci) && all(c("LowerValue", "UpperValue") %in% names(curve))) {
    p <- p +
      ggplot2::geom_step(ggplot2::aes(y = LowerValue), linewidth = 0.28, linetype = "22", alpha = 0.45) +
      ggplot2::geom_step(ggplot2::aes(y = UpperValue), linewidth = 0.28, linetype = "22", alpha = 0.45)
  }
  if (isTRUE(result$show_censor) && nrow(censor) > 0) {
    if (identical(figure_version, "bw")) {
      p <- p +
        ggplot2::geom_point(
          data = censor,
          ggplot2::aes(x = Time, y = Value, color = Strata, shape = Strata),
          size = 1.45,
          stroke = 0.55,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_shape_manual(values = rep(c(3, 4, 8, 1, 2, 5, 6, 7), length.out = strata_count), drop = FALSE)
    } else {
      p <- p +
        ggplot2::geom_point(
          data = censor,
          ggplot2::aes(x = Time, y = Value, color = Strata),
          shape = 3,
          size = 1.45,
          stroke = 0.55,
          inherit.aes = FALSE
        )
    }
  }
  if (plot_type %in% c("survival", "event")) {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
  }
  p
}

survival_km_posthoc_table <- function(data, time, event, group, formula, test_method = "logrank", entry = "") {
  if (!nzchar(as.character(group %||% ""))) return(data.frame())
  levels <- levels(droplevels(as.factor(data[[group]])))
  if (length(levels) < 3L) return(data.frame())
  pairs <- utils::combn(levels, 2, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    subset_data <- data[data[[group]] %in% pair, , drop = FALSE]
    subset_data[[group]] <- droplevels(as.factor(subset_data[[group]]))
    tryCatch(
      {
        test <- survival_weighted_rank_test(subset_data, time, event, group, test_method, entry)
        if (is.null(test)) return(NULL)
        data.frame(
          Comparison = paste(pair, collapse = " vs "),
          Chisq = unname(test$chisq),
          df = test$df,
          p = test$p,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) NULL
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  table <- do.call(rbind, rows)
  table$p_adjusted <- stats::p.adjust(table$p, method = "holm")
  table
}

survival_km_crossing_diagnostics <- function(fit, tolerance = 1e-10) {
  fit_summary <- summary(fit, censored = TRUE)
  strata <- as.character(fit_summary$strata %||% character(0))
  strata_levels <- unique(strata[nzchar(strata)])
  if (length(strata_levels) < 2L) return(data.frame())
  time <- as.numeric(fit_summary$time)
  surv <- as.numeric(fit_summary$surv)
  valid <- is.finite(time) & is.finite(surv) & nzchar(strata)
  if (!any(valid)) return(data.frame())
  time <- time[valid]
  surv <- surv[valid]
  strata <- strata[valid]
  grid <- sort(unique(time))
  if (!length(grid)) return(data.frame())
  step_survival <- lapply(strata_levels, function(level) {
    keep <- strata == level
    level_time <- time[keep]
    level_surv <- surv[keep]
    ordered <- order(level_time)
    level_time <- level_time[ordered]
    level_surv <- level_surv[ordered]
    duplicated_time <- duplicated(level_time, fromLast = TRUE)
    level_time <- level_time[!duplicated_time]
    level_surv <- level_surv[!duplicated_time]
    stats::approx(
      x = c(-Inf, level_time),
      y = c(1, level_surv),
      xout = grid,
      method = "constant",
      f = 0,
      rule = 2,
      ties = "ordered"
    )$y
  })
  names(step_survival) <- strata_levels
  pairs <- utils::combn(strata_levels, 2, simplify = FALSE)
  rows <- lapply(pairs, function(pair) {
    common_followup_end <- min(vapply(pair, function(level) max(time[strata == level]), numeric(1)))
    pair_grid_indices <- which(grid <= common_followup_end)
    pair_grid <- grid[pair_grid_indices]
    difference <- step_survival[[pair[[1]]]][pair_grid_indices] - step_survival[[pair[[2]]]][pair_grid_indices]
    non_tied <- which(is.finite(difference) & abs(difference) > tolerance)
    crossing_times <- numeric(0)
    if (length(non_tied) > 1L) {
      signs <- sign(difference[non_tied])
      changes <- which(signs[-1L] != signs[-length(signs)]) + 1L
      if (length(changes)) crossing_times <- pair_grid[non_tied[changes]]
    }
    data.frame(
      Comparison = paste(pair, collapse = " vs "),
      Crossings = length(crossing_times),
      `First crossing time` = if (length(crossing_times)) crossing_times[[1]] else NA_real_,
      `Common follow-up end` = common_followup_end,
      `Review signal` = length(crossing_times) > 0L,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

prepare_km_single_analysis_result <- function(
  data,
  time,
  event,
  group = "",
  event_value = "1",
  rate_times = NULL,
  analysis_method = "km",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event", "cumhaz", "log_survival"),
  plot_versions = "color",
  show_ci = TRUE,
  show_censor = TRUE,
  rmst_tau = NULL,
  entry = "",
  time_origin = "",
  time_unit = "",
  event_map = NULL
) {
  time <- as.character(time %||% character(0))
  event <- as.character(event %||% character(0))
  group <- as.character(group %||% character(0))
  entry <- survival_selected_names(entry)
  time <- time[!is.na(time) & nzchar(time)]
  event <- event[!is.na(event) & nzchar(event)]
  group <- group[!is.na(group) & nzchar(group)]
  if (length(time) == 0) stop("Select a time variable.")
  if (length(event) == 0) stop("Select an event variable.")
  time <- time[[1]]
  event <- event[[1]]
  group <- if (length(group) > 0) group[[1]] else ""
  entry <- if (length(entry) > 0) entry[[1]] else ""
  settings <- survival_legacy_settings(time, event, event_value, group)
  if (nzchar(entry)) {
    settings$data_shape <- "entry_exit"
    settings$roles$entry <- entry
  }
  if (is.data.frame(event_map)) {
    settings$legacy <- FALSE
    settings$event_map <- event_map
  }
  if (nzchar(entry) && identical(as.character(analysis_method %||% "km")[[1]], "life_table")) stop("The actuarial life-table option does not support delayed entry; use Kaplan-Meier.")
  preflight <- survival_preflight(data, settings)
  survival_preflight_stop(preflight)
  model_data <- preflight$analysis_data
  surv_term <- if (nzchar(entry)) sprintf("survival::Surv(`%s`, `%s`, `%s`)", entry, time, event) else sprintf("survival::Surv(`%s`, `%s`)", time, event)
  formula <- if (nzchar(as.character(group %||% ""))) {
    stats::as.formula(sprintf("%s ~ `%s`", surv_term, group))
  } else {
    stats::as.formula(sprintf("%s ~ 1", surv_term))
  }
  fit <- survival::survfit(formula, data = model_data)
  crossing_table <- survival_km_crossing_diagnostics(fit)
  logrank <- NULL
  if (nzchar(as.character(group %||% "")) && length(unique(model_data[[group]])) > 1L) {
    logrank <- survival_weighted_rank_test(model_data, time, event, group, test_method, entry)
  }
  times <- survival_parse_times(rate_times)
  if (length(times) == 0) times <- survival_default_risk_times(model_data[[time]])
  summary_at <- if (length(times) > 0) summary(fit, times = times) else NULL
  tau <- survival_parse_rmst_tau(rmst_tau)
  rmst <- NULL
  if (!is.null(tau)) {
    common_max <- survival_rmst_common_max(model_data, time, group)
    if (!is.finite(common_max) || tau > common_max) {
      stop("RMST limit (tau) must be within the observed follow-up range of every comparison group.")
    }
    estimates <- survival_rmst_estimates(fit, tau)
    rmst <- list(
      tau = tau,
      common_max = common_max,
      estimates = estimates,
      contrasts = survival_rmst_contrasts(estimates)
    )
  }
  output_tables <- intersect(as.character(output_tables %||% character(0)), c("survival_table", "survival_time"))
  plot_types <- intersect(as.character(plot_types %||% character(0)), c("survival", "event", "cumhaz", "log_survival"))
  plot_versions <- intersect(as.character(plot_versions %||% character(0)), c("color", "bw"))
  if (length(plot_versions) == 0) plot_versions <- "color"
  list(
    type = "km",
    analysis_method = as.character(analysis_method %||% "km")[[1]],
    test_method = as.character(test_method %||% "logrank")[[1]],
    test_label = survival_km_test_label(test_method),
    fit = fit,
    data = model_data,
    preflight = preflight,
    time = time,
    entry = entry,
    time_origin = as.character(time_origin %||% "")[[1]],
    time_unit = as.character(time_unit %||% "")[[1]],
    event = event,
    group = group,
    event_value = event_value,
    n = nrow(model_data),
    events = sum(model_data[[event]], na.rm = TRUE),
    censored = sum(!model_data[[event]], na.rm = TRUE),
    show_ci = isTRUE(show_ci),
    show_censor = isTRUE(show_censor),
    rate_times = times,
    summary_at = summary_at,
    rmst = rmst,
    median_table = survival_km_median_table(fit),
    life_table = survival_life_table(model_data, time, event, group, times),
    crossing_table = crossing_table,
    logrank = logrank,
    posthoc = survival_km_posthoc_table(model_data, time, event, group, formula, test_method, entry),
    output_tables = output_tables,
    plot_types = plot_types,
    plot_versions = plot_versions,
    packages = package_version_label("survival")
  )
}

survival_parse_rmst_tau <- function(value) {
  values <- survival_parse_times(value)
  if (length(values) == 0) return(NULL)
  if (length(values) != 1L || values[[1]] <= 0) stop("Enter one positive RMST limit (tau).")
  values[[1]]
}

survival_rmst_common_max <- function(data, time, group = "") {
  if (!nzchar(as.character(group %||% ""))) return(max(data[[time]], na.rm = TRUE))
  maxima <- tapply(data[[time]], data[[group]], max, na.rm = TRUE)
  min(as.numeric(maxima), na.rm = TRUE)
}

survival_rmst_estimates <- function(fit, tau) {
  tau <- as.numeric(tau)[[1]]
  table <- summary(fit, rmean = tau)$table
  if (is.null(dim(table))) table <- t(as.matrix(table))
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  strata <- rownames(table)
  if (is.null(strata) || any(!nzchar(strata))) strata <- rep("All", nrow(table))
  estimate <- as.numeric(table[["rmean"]])
  se <- as.numeric(table[["se(rmean)"]])
  data.frame(
    Strata = strata,
    Tau = tau,
    RMST = estimate,
    SE = se,
    LLCI = estimate - stats::qnorm(.975) * se,
    ULCI = estimate + stats::qnorm(.975) * se,
    stringsAsFactors = FALSE
  )
}

survival_rmst_contrasts <- function(estimates) {
  if (!is.data.frame(estimates) || nrow(estimates) < 2L) return(data.frame())
  pairs <- utils::combn(seq_len(nrow(estimates)), 2L, simplify = FALSE)
  rows <- lapply(pairs, function(index) {
    first <- estimates[index[[1]], , drop = FALSE]
    second <- estimates[index[[2]], , drop = FALSE]
    difference <- second$RMST[[1]] - first$RMST[[1]]
    se <- sqrt(first$SE[[1]]^2 + second$SE[[1]]^2)
    z <- if (is.finite(se) && se > 0) difference / se else NA_real_
    ratio <- if (first$RMST[[1]] > 0 && second$RMST[[1]] > 0) second$RMST[[1]] / first$RMST[[1]] else NA_real_
    ratio_se <- if (is.finite(ratio)) sqrt((first$SE[[1]] / first$RMST[[1]])^2 + (second$SE[[1]] / second$RMST[[1]])^2) else NA_real_
    data.frame(
      Comparison = paste(second$Strata[[1]], "-", first$Strata[[1]]),
      Tau = first$Tau[[1]],
      Difference = difference,
      SE = se,
      LLCI = difference - stats::qnorm(.975) * se,
      ULCI = difference + stats::qnorm(.975) * se,
      z = z,
      p = if (is.finite(z)) 2 * stats::pnorm(abs(z), lower.tail = FALSE) else NA_real_,
      Ratio = ratio,
      RatioLLCI = if (is.finite(ratio_se)) exp(log(ratio) - stats::qnorm(.975) * ratio_se) else NA_real_,
      RatioULCI = if (is.finite(ratio_se)) exp(log(ratio) + stats::qnorm(.975) * ratio_se) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$p_adjusted <- if (nrow(out) > 1L) stats::p.adjust(out$p, method = "holm") else out$p
  out
}

prepare_km_analysis_result <- function(
  data,
  time,
  event,
  group = "",
  event_value = "1",
  rate_times = NULL,
  analysis_method = "km",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event", "cumhaz", "log_survival"),
  plot_versions = "color",
  show_ci = TRUE,
  show_censor = TRUE,
  rmst_tau = NULL,
  entry = "",
  time_origin = "",
  time_unit = "",
  event_map = NULL
) {
  groups <- survival_selected_names(group)
  if (length(groups) <= 1L) {
    return(prepare_km_single_analysis_result(data, time, event, groups, event_value, rate_times, analysis_method, test_method, output_tables, plot_types, plot_versions, show_ci, show_censor, rmst_tau, entry, time_origin, time_unit, event_map))
  }
  analyses <- lapply(groups, function(group_name) {
    prepare_km_single_analysis_result(data, time, event, group_name, event_value, rate_times, analysis_method, test_method, output_tables, plot_types, plot_versions, show_ci, show_censor, rmst_tau, entry, time_origin, time_unit, event_map)
  })
  names(analyses) <- groups
  n_values <- vapply(analyses, function(item) item$n, numeric(1))
  event_values <- vapply(analyses, function(item) item$events, numeric(1))
  censored_values <- vapply(analyses, function(item) item$censored, numeric(1))
  list(
    type = "km_multi",
    analyses = analyses,
    time = analyses[[1]]$time,
    entry = analyses[[1]]$entry,
    time_origin = analyses[[1]]$time_origin,
    time_unit = analyses[[1]]$time_unit,
    event = analyses[[1]]$event,
    group = groups,
    event_value = event_value,
    analysis_method = as.character(analysis_method %||% "km")[[1]],
    test_method = as.character(test_method %||% "logrank")[[1]],
    test_label = survival_km_test_label(test_method),
    n = max(n_values),
    events = max(event_values),
    censored = max(censored_values),
    show_ci = isTRUE(show_ci),
    show_censor = isTRUE(show_censor),
    rmst_tau = survival_parse_rmst_tau(rmst_tau),
    output_tables = analyses[[1]]$output_tables,
    plot_types = analyses[[1]]$plot_types,
    plot_versions = analyses[[1]]$plot_versions,
    preflight = analyses[[1]]$preflight,
    packages = package_version_label("survival")
  )
}

survival_km_median_table <- function(fit) {
  summary_table <- summary(fit)$table
  table <- as.data.frame(summary_table, stringsAsFactors = FALSE)
  if (is.null(dim(summary_table))) {
    table <- as.data.frame(t(summary_table), stringsAsFactors = FALSE)
  }
  table$Strata <- rownames(table)
  rownames(table) <- NULL
  quantiles <- tryCatch(quantile(fit, probs = c(0.25, 0.5, 0.75)), error = function(e) NULL)
  add_quantile <- function(prob_label, column_name) {
    if (is.null(quantiles) || is.null(quantiles$quantile)) return(rep(NA_real_, nrow(table)))
    quantile_values <- quantiles$quantile
    if (is.null(dim(quantile_values))) {
      value <- quantile_values[[prob_label]] %||% NA_real_
      return(rep(as.numeric(value), nrow(table)))
    }
    values <- quantile_values[, prob_label]
    names(values) <- rownames(quantiles$quantile)
    unname(values[table$Strata])
  }
  result <- data.frame(
    Strata = table$Strata,
    Records = table[["records"]] %||% table[["n.max"]] %||% NA,
    Events = table[["events"]] %||% NA,
    Mean = table[["rmean"]] %||% NA,
    `Mean SE` = table[["se(rmean)"]] %||% NA,
    Q1 = add_quantile("25", "Q1"),
    Median = table[["median"]] %||% add_quantile("50", "Median"),
    Q3 = add_quantile("75", "Q3"),
    `Median 95% LCL` = table[["0.95LCL"]] %||% NA,
    `Median 95% UCL` = table[["0.95UCL"]] %||% NA,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  result
}

survival_cox_formula <- function(time, event, covariates, entry = "", start = "", stop = "", subject_id = "", strata_variable = "", cluster_variable = "", spline_covariate = "", spline_df = 4L, time_varying_covariate = "") {
  rhs_terms <- vapply(covariates, function(covariate) {
    if (nzchar(as.character(spline_covariate %||% "")) && identical(covariate, spline_covariate)) {
      sprintf("splines::ns(`%s`, df = %d)", covariate, as.integer(spline_df))
    } else {
      sprintf("`%s`", covariate)
    }
  }, character(1))
  if (nzchar(as.character(time_varying_covariate %||% ""))) rhs_terms <- c(rhs_terms, sprintf("tt(`%s`)", time_varying_covariate))
  if (nzchar(as.character(strata_variable %||% ""))) rhs_terms <- c(rhs_terms, sprintf("strata(`%s`)", strata_variable))
  cluster_name <- if (nzchar(as.character(subject_id %||% ""))) subject_id else cluster_variable
  if (nzchar(as.character(cluster_name %||% ""))) rhs_terms <- c(rhs_terms, sprintf("cluster(`%s`)", cluster_name))
  rhs <- paste(rhs_terms, collapse = " + ")
  lhs <- if (nzchar(as.character(start %||% "")) && nzchar(as.character(stop %||% ""))) {
    sprintf("survival::Surv(`%s`, `%s`, `%s`)", start, stop, event)
  } else if (nzchar(as.character(entry %||% ""))) sprintf("survival::Surv(`%s`, `%s`, `%s`)", entry, time, event) else sprintf("survival::Surv(`%s`, `%s`)", time, event)
  stats::as.formula(sprintf("%s ~ %s", lhs, rhs))
}

survival_cox_time_varying_curve <- function(fit, data, variable, time_name, event_name, selected_times = numeric(0), points = 101L) {
  coefficient_names <- names(stats::coef(fit))
  base_candidates <- c(variable, paste0("`", variable, "`"))
  base_index <- which(coefficient_names %in% base_candidates)
  time_index <- which(startsWith(coefficient_names, "tt(") & grepl(variable, coefficient_names, fixed = TRUE))
  if (length(base_index) != 1L || length(time_index) != 1L) stop("Could not identify the base and time-interaction coefficients for the time-varying Cox effect.")
  observed_time <- as.numeric(data[[time_name]])
  event_time <- observed_time[as.logical(data[[event_name]]) & is.finite(observed_time) & observed_time >= 0]
  if (length(event_time) < 2L) event_time <- observed_time[is.finite(observed_time) & observed_time >= 0]
  limits <- stats::quantile(event_time, probs = c(.05, .95), names = FALSE, type = 7)
  if (!all(is.finite(limits)) || limits[[1]] >= limits[[2]]) limits <- range(event_time)
  grid <- sort(unique(c(selected_times, seq(limits[[1]], limits[[2]], length.out = as.integer(points)))))
  grid <- grid[is.finite(grid) & grid >= 0]
  contrasts <- matrix(0, nrow = length(grid), ncol = length(coefficient_names), dimnames = list(NULL, coefficient_names))
  contrasts[, base_index] <- 1
  contrasts[, time_index] <- log1p(grid)
  coefficients <- stats::coef(fit)
  log_hr <- as.numeric(contrasts %*% coefficients)
  variance <- stats::vcov(fit)
  standard_error <- sqrt(pmax(0, rowSums((contrasts %*% variance) * contrasts)))
  critical <- stats::qnorm(.975)
  data.frame(
    Variable = variable,
    Time = grid,
    `Time function` = "log(1 + time)",
    HR = exp(log_hr),
    Lower = exp(log_hr - critical * standard_error),
    Upper = exp(log_hr + critical * standard_error),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

survival_cox_spline_curve <- function(fit, data, variable, points = 101L) {
  values <- as.numeric(data[[variable]])
  values <- values[is.finite(values)]
  if (length(values) < 2L) return(data.frame())
  limits <- stats::quantile(values, probs = c(.05, .95), names = FALSE, type = 7)
  if (!all(is.finite(limits)) || limits[[1]] >= limits[[2]]) limits <- range(values)
  reference <- stats::median(values)
  grid <- sort(unique(c(reference, seq(limits[[1]], limits[[2]], length.out = as.integer(points)))))
  newdata <- data[rep(1L, length(grid) + 1L), , drop = FALSE]
  newdata[[variable]] <- c(reference, grid)
  design <- stats::model.matrix(fit, data = newdata)
  coefficients <- stats::coef(fit)
  coefficient_names <- names(coefficients)
  if (!all(coefficient_names %in% colnames(design))) stop("Could not reconstruct the Cox spline design matrix for effect plotting.")
  design <- design[, coefficient_names, drop = FALSE]
  contrasts <- sweep(design[-1L, , drop = FALSE], 2L, design[1L, ], FUN = "-")
  log_hr <- as.numeric(contrasts %*% coefficients)
  variance <- stats::vcov(fit)
  standard_error <- sqrt(pmax(0, rowSums((contrasts %*% variance) * contrasts)))
  critical <- stats::qnorm(.975)
  data.frame(
    Variable = variable,
    Value = grid,
    Reference = reference,
    HR = exp(log_hr),
    Lower = exp(log_hr - critical * standard_error),
    Upper = exp(log_hr + critical * standard_error),
    stringsAsFactors = FALSE
  )
}

survival_cox_collinearity <- function(design) {
  design <- as.matrix(design)
  if (!nrow(design) || !ncol(design)) return(list(table = data.frame(), condition_number = NA_real_))
  storage.mode(design) <- "double"
  column_names <- colnames(design) %||% paste0("X", seq_len(ncol(design)))
  finite_rows <- apply(design, 1L, function(row) all(is.finite(row)))
  design <- design[finite_rows, , drop = FALSE]
  if (nrow(design) < 2L) return(list(table = data.frame(), condition_number = NA_real_))
  variances <- apply(design, 2L, stats::var)
  estimable <- is.finite(variances) & variances > 0
  vif <- rep(NA_real_, ncol(design))
  if (sum(estimable) == 1L) {
    vif[estimable] <- 1
  } else if (sum(estimable) > 1L) {
    estimable_design <- design[, estimable, drop = FALSE]
    estimable_indexes <- which(estimable)
    for (position in seq_along(estimable_indexes)) {
      target <- estimable_design[, position]
      others <- estimable_design[, -position, drop = FALSE]
      fit <- stats::lm.fit(cbind(1, others), target)
      total <- sum((target - mean(target))^2)
      residual <- sum(fit$residuals^2)
      r_squared <- if (total > 0) max(0, min(1, 1 - residual / total)) else NA_real_
      vif[estimable_indexes[[position]]] <- if (is.finite(r_squared) && r_squared < 1) 1 / (1 - r_squared) else Inf
    }
  }
  condition_number <- if (sum(estimable) > 1L) {
    scaled <- scale(design[, estimable, drop = FALSE])
    tryCatch(kappa(scaled, exact = TRUE), error = function(e) NA_real_)
  } else if (sum(estimable) == 1L) 1 else NA_real_
  review <- ifelse(!estimable, "Not estimable", ifelse(!is.finite(vif) | vif >= 10, "High", ifelse(vif >= 5, "Review", "No strong signal")))
  list(
    table = data.frame(
      `Design column` = column_names,
      VIF = vif,
      Tolerance = ifelse(is.finite(vif) & vif > 0, 1 / vif, 0),
      Review = review,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    condition_number = as.numeric(condition_number)
  )
}

survival_cox_categorical_joint_tests <- function(fit, coefficient_table, data, covariates, variance_label = "Model-based") {
  factor_covariates <- covariates[vapply(covariates, function(name) is.factor(data[[name]]) && nlevels(data[[name]]) >= 2L, logical(1))]
  if (!length(factor_covariates)) return(data.frame())
  coefficients <- stats::coef(fit)
  covariance <- stats::vcov(fit)
  rows <- lapply(factor_covariates, function(covariate) {
    terms <- coefficient_table$Term[
      coefficient_table$Variable == covariate &
        !(coefficient_table$Reference %in% TRUE) &
        is.finite(coefficient_table$B)
    ]
    terms <- intersect(terms, names(coefficients))
    if (!length(terms)) {
      return(data.frame(Variable = covariate, Levels = nlevels(data[[covariate]]), Parameters = 0L, `Wald chi-square` = NA_real_, df = NA_integer_, p = NA_real_, Estimable = FALSE, Variance = variance_label, check.names = FALSE, stringsAsFactors = FALSE))
    }
    beta <- coefficients[terms]
    variance <- covariance[terms, terms, drop = FALSE]
    rank <- qr(variance)$rank
    statistic <- if (rank == length(terms)) tryCatch(as.numeric(crossprod(beta, solve(variance, beta))), error = function(e) NA_real_) else NA_real_
    estimable <- is.finite(statistic) && rank == length(terms)
    data.frame(
      Variable = covariate,
      Levels = nlevels(data[[covariate]]),
      Parameters = length(terms),
      `Wald chi-square` = statistic,
      df = if (estimable) length(terms) else rank,
      p = if (estimable) stats::pchisq(statistic, df = length(terms), lower.tail = FALSE) else NA_real_,
      Estimable = estimable,
      Variance = variance_label,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

survival_categorical_joint_tests_from_design <- function(coefficients, covariance, data, covariates, variance_label) {
  factor_covariates <- covariates[vapply(covariates, function(name) is.factor(data[[name]]) && nlevels(data[[name]]) >= 2L, logical(1))]
  if (!length(factor_covariates)) return(data.frame())
  coefficient_names <- names(coefficients) %||% character(length(coefficients))
  coefficients <- as.numeric(coefficients)
  names(coefficients) <- coefficient_names
  covariance <- as.matrix(covariance)
  if (length(coefficient_names) != nrow(covariance)) return(data.frame())
  if (is.null(rownames(covariance)) || is.null(colnames(covariance))) dimnames(covariance) <- list(coefficient_names, coefficient_names)
  design_formula <- stats::as.formula(paste("~", paste(sprintf("`%s`", covariates), collapse = " + ")))
  full_design <- stats::model.matrix(design_formula, data = data)
  design_assign <- attr(full_design, "assign")
  term_labels <- gsub("`", "", attr(stats::terms(design_formula), "term.labels"), fixed = TRUE)
  rows <- lapply(factor_covariates, function(covariate) {
    term_indexes <- which(term_labels == covariate)
    design_terms <- colnames(full_design)[design_assign %in% term_indexes]
    coefficient_terms <- intersect(design_terms, coefficient_names)
    if (!length(coefficient_terms)) {
      return(data.frame(Variable = covariate, Levels = nlevels(data[[covariate]]), Parameters = 0L, `Wald chi-square` = NA_real_, df = NA_integer_, p = NA_real_, Estimable = FALSE, Variance = variance_label, check.names = FALSE, stringsAsFactors = FALSE))
    }
    beta <- coefficients[coefficient_terms]
    variance <- covariance[coefficient_terms, coefficient_terms, drop = FALSE]
    finite <- all(is.finite(beta)) && all(is.finite(variance))
    rank <- if (finite) qr(variance)$rank else NA_integer_
    statistic <- if (finite && rank == length(coefficient_terms)) tryCatch(as.numeric(crossprod(beta, solve(variance, beta))), error = function(e) NA_real_) else NA_real_
    estimable <- is.finite(statistic) && is.finite(rank) && rank == length(coefficient_terms)
    data.frame(
      Variable = covariate,
      Levels = nlevels(data[[covariate]]),
      Parameters = length(coefficient_terms),
      `Wald chi-square` = statistic,
      df = if (estimable) length(coefficient_terms) else rank,
      p = if (estimable) stats::pchisq(statistic, df = length(coefficient_terms), lower.tail = FALSE) else NA_real_,
      Estimable = estimable,
      Variance = variance_label,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

survival_apply_treatment_contrasts <- function(data, covariates) {
  for (covariate in covariates) {
    values <- data[[covariate]]
    if (!is.factor(values) || nlevels(values) < 2L) next
    values <- droplevels(values)
    attr(values, "contrasts") <- NULL
    data[[covariate]] <- values
  }
  data
}

survival_categorical_reference_table <- function(data, covariates) {
  factor_covariates <- covariates[vapply(covariates, function(name) is.factor(data[[name]]) && nlevels(data[[name]]) >= 2L, logical(1))]
  if (!length(factor_covariates)) return(data.frame())
  do.call(rbind, lapply(factor_covariates, function(covariate) {
    observed_levels <- levels(data[[covariate]])
    data.frame(
      Variable = covariate,
      `Reference level` = observed_levels[[1]],
      `Observed levels` = paste(observed_levels, collapse = ", "),
      Contrast = "Treatment; each non-reference level versus the reference",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

survival_coefficient_label_table <- function(data, covariates) {
  if (!length(covariates)) return(data.frame())
  design_formula <- stats::as.formula(paste("~", paste(sprintf("`%s`", covariates), collapse = " + ")))
  full_design <- stats::model.matrix(design_formula, data = data)
  design_assign <- attr(full_design, "assign")
  term_labels <- gsub("`", "", attr(stats::terms(design_formula), "term.labels"), fixed = TRUE)
  rows <- list()
  for (covariate in covariates) {
    term_indexes <- which(term_labels == covariate)
    design_terms <- colnames(full_design)[design_assign %in% term_indexes]
    values <- data[[covariate]]
    if (is.factor(values) && nlevels(values) >= 2L) {
      observed_levels <- levels(values)
      rows[[length(rows) + 1L]] <- data.frame(
        Term = "",
        Variable = covariate,
        Level = observed_levels[[1]],
        `Reference level` = observed_levels[[1]],
        Reference = TRUE,
        `Display term` = paste0(covariate, "=", observed_levels[[1]], " (reference)"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      nonreference_levels <- observed_levels[-1L]
      for (index in seq_along(design_terms)) {
        level <- if (index <= length(nonreference_levels)) nonreference_levels[[index]] else design_terms[[index]]
        rows[[length(rows) + 1L]] <- data.frame(
          Term = design_terms[[index]],
          Variable = covariate,
          Level = level,
          `Reference level` = observed_levels[[1]],
          Reference = FALSE,
          `Display term` = paste0(covariate, "=", level, " vs ", observed_levels[[1]]),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    } else {
      for (term in design_terms) {
        rows[[length(rows) + 1L]] <- data.frame(
          Term = term,
          Variable = covariate,
          Level = "",
          `Reference level` = "",
          Reference = FALSE,
          `Display term` = covariate,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

survival_cox_functional_form_data <- function(data, covariates, martingale) {
  martingale <- as.numeric(martingale)
  if (length(martingale) != nrow(data)) return(data.frame())
  continuous <- covariates[vapply(covariates, function(name) {
    values <- data[[name]]
    is.numeric(values) && !is.factor(values) && length(unique(values[is.finite(values)])) >= 4L
  }, logical(1))]
  if (!length(continuous)) return(data.frame())
  rows <- lapply(continuous, function(name) {
    values <- as.numeric(data[[name]])
    keep <- is.finite(values) & is.finite(martingale)
    data.frame(
      `Analysis row` = which(keep),
      Covariate = name,
      Value = values[keep],
      `Martingale residual` = martingale[keep],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

survival_cox_basic_diagnostics <- function(fit, data, covariates) {
  ph <- tryCatch(survival::cox.zph(fit), error = function(e) NULL)
  ph_table <- if (!is.null(ph)) {
    table <- as.data.frame(ph$table, stringsAsFactors = FALSE)
    table$Term <- rownames(table)
    rownames(table) <- NULL
    table[, c("Term", setdiff(names(table), "Term")), drop = FALSE]
  } else data.frame()
  martingale <- tryCatch(stats::residuals(fit, type = "martingale"), error = function(e) numeric(0))
  deviance <- tryCatch(stats::residuals(fit, type = "deviance"), error = function(e) numeric(0))
  functional_form_data <- survival_cox_functional_form_data(data, covariates, martingale)
  residual_summary <- function(values, type) {
    values <- as.numeric(values)
    values <- values[is.finite(values)]
    if (!length(values)) return(NULL)
    quantiles <- stats::quantile(values, probs = c(0, .25, .5, .75, 1), names = FALSE)
    data.frame(Type = type, Min = quantiles[[1]], Q1 = quantiles[[2]], Median = quantiles[[3]], Q3 = quantiles[[4]], Max = quantiles[[5]], stringsAsFactors = FALSE)
  }
  residual_rows <- Filter(Negate(is.null), list(residual_summary(martingale, "Martingale"), residual_summary(deviance, "Deviance")))
  residual_table <- if (length(residual_rows)) do.call(rbind, residual_rows) else data.frame()
  dfbeta <- tryCatch(stats::residuals(fit, type = "dfbeta"), error = function(e) NULL)
  dfbetas <- tryCatch(stats::residuals(fit, type = "dfbetas"), error = function(e) NULL)
  influence_table <- data.frame()
  if (!is.null(dfbeta)) {
    if (is.null(dim(dfbeta))) dfbeta <- matrix(dfbeta, ncol = 1L, dimnames = list(names(dfbeta), names(stats::coef(fit))[[1]]))
    if (is.null(colnames(dfbeta))) colnames(dfbeta) <- names(stats::coef(fit))
    if (!is.null(dfbetas) && is.null(dim(dfbetas))) dfbetas <- matrix(dfbetas, ncol = 1L, dimnames = list(names(dfbetas), names(stats::coef(fit))[[1]]))
    if (!is.null(dfbetas) && is.null(colnames(dfbetas))) colnames(dfbetas) <- names(stats::coef(fit))
    threshold <- 2 / sqrt(nrow(data))
    influence_table <- do.call(rbind, lapply(seq_len(ncol(dfbeta)), function(index) {
      absolute <- abs(dfbeta[, index])
      standardized <- if (!is.null(dfbetas) && ncol(dfbetas) >= index) abs(dfbetas[, index]) else rep(NA_real_, length(absolute))
      maximum <- if (any(is.finite(absolute))) max(absolute, na.rm = TRUE) else NA_real_
      standardized_maximum <- if (any(is.finite(standardized))) max(standardized, na.rm = TRUE) else NA_real_
      row_index <- if (is.finite(standardized_maximum)) which.max(standardized) else if (is.finite(maximum)) which.max(absolute) else NA_integer_
      data.frame(
        Term = colnames(dfbeta)[[index]],
        `Maximum absolute DFBETA` = maximum,
        `Maximum absolute DFBETAS` = standardized_maximum,
        `Screening threshold` = threshold,
        `Review signal` = is.finite(standardized_maximum) && standardized_maximum > threshold,
        `Source row in analysis data` = row_index,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  }
  collinearity <- survival_cox_collinearity(fit$x)
  list(
    ph = ph,
    ph_table = ph_table,
    residual_table = residual_table,
    functional_form_data = functional_form_data,
    influence_table = influence_table,
    collinearity_table = collinearity$table,
    condition_number = collinearity$condition_number
  )
}

survival_cox_omnibus_tests <- function(fit) {
  fit_summary <- summary(fit)
  model_test_row <- function(name, value) {
    value <- as.numeric(value %||% numeric(0))
    data.frame(
      Test = name,
      Statistic = if (length(value) >= 1L) value[[1]] else NA_real_,
      df = if (length(value) >= 2L) value[[2]] else NA_real_,
      p = if (length(value) >= 3L) value[[3]] else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, list(
    model_test_row("Likelihood-ratio", fit_summary$logtest),
    model_test_row("Wald", fit_summary$waldtest),
    model_test_row("Score", fit_summary$sctest)
  ))
}

survival_fine_gray_omnibus_test <- function(fit) {
  logtest <- as.numeric(summary(fit)$logtest %||% numeric(0))
  statistic <- if (length(logtest) >= 1L) logtest[[1]] else NA_real_
  degrees_freedom <- if (length(logtest) >= 2L) logtest[[2]] else NA_real_
  data.frame(
    Test = "Pseudo likelihood-ratio",
    Statistic = statistic,
    df = degrees_freedom,
    p = if (is.finite(statistic) && is.finite(degrees_freedom) && degrees_freedom > 0) stats::pchisq(statistic, df = degrees_freedom, lower.tail = FALSE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

survival_adjusted_curve_once <- function(fit, data, group, times) {
  baseline <- survival::basehaz(fit, centered = FALSE)
  baseline <- rbind(data.frame(hazard = 0, time = 0), baseline[, c("hazard", "time"), drop = FALSE])
  cumulative_hazard <- stats::approx(baseline$time, baseline$hazard, xout = times, method = "constant", f = 0, rule = 2, ties = "ordered")$y
  levels <- levels(data[[group]])
  rows <- lapply(levels, function(level) {
    newdata <- data
    newdata[[group]] <- factor(level, levels = levels)
    risk <- as.numeric(stats::predict(fit, newdata = newdata, type = "risk", reference = "zero"))
    survival <- vapply(cumulative_hazard, function(hazard) mean(exp(-hazard * risk), na.rm = TRUE), numeric(1))
    data.frame(Level = level, Time = times, Survival = survival, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

survival_adjusted_curve <- function(fit, data, group, bootstrap_reps = 2000L, seed = 20260815L, selected_times = numeric(0)) {
  group <- as.character(group %||% "")[[1]]
  if (!nzchar(group)) return(NULL)
  if (!group %in% names(data)) stop("Adjusted-survival group variable was not found in the analysis data.")
  if (!is.factor(data[[group]])) stop("Adjusted survival requires a categorical group variable included in the Cox model.")
  if (nlevels(data[[group]]) < 2L) stop("Adjusted survival requires at least two observed group levels.")
  selected_times <- sort(unique(as.numeric(selected_times)))
  selected_times <- selected_times[is.finite(selected_times) & selected_times >= 0]
  baseline <- survival::basehaz(fit, centered = FALSE)
  times <- sort(unique(c(0, baseline$time, selected_times)))
  estimate <- survival_adjusted_curve_once(fit, data, group, times)
  bootstrap_reps <- as.integer(bootstrap_reps %||% 0L)
  bootstrap_reps <- max(0L, bootstrap_reps)
  successful <- 0L
  boot_values <- NULL
  minimum_successful <- if (bootstrap_reps > 0L) max(20L, ceiling(bootstrap_reps * .8)) else 0L
  if (bootstrap_reps > 0L) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(as.integer(seed))
    levels <- levels(data[[group]])
    boot_values <- lapply(levels, function(level) matrix(NA_real_, nrow = bootstrap_reps, ncol = length(times)))
    names(boot_values) <- levels
    formula <- stats::formula(fit)
    for (iteration in seq_len(bootstrap_reps)) {
      indexes <- sample.int(nrow(data), nrow(data), replace = TRUE)
      sample_data <- data[indexes, , drop = FALSE]
      sample_data[[group]] <- factor(sample_data[[group]], levels = levels)
      if (length(unique(sample_data[[group]])) < length(levels)) next
      boot_fit <- tryCatch(survival::coxph(formula, data = sample_data, ties = fit$method, x = TRUE), error = function(e) NULL)
      if (is.null(boot_fit) || any(!is.finite(stats::coef(boot_fit)))) next
      boot_curve <- tryCatch(survival_adjusted_curve_once(boot_fit, sample_data, group, times), error = function(e) NULL)
      if (is.null(boot_curve)) next
      successful <- successful + 1L
      for (level in levels) boot_values[[level]][iteration, ] <- boot_curve$Survival[boot_curve$Level == level]
    }
    estimate$Lower <- NA_real_
    estimate$Upper <- NA_real_
    if (successful >= minimum_successful) {
      for (level in levels) {
        target <- estimate$Level == level
        values <- boot_values[[level]]
        estimate$Lower[target] <- apply(values, 2, stats::quantile, probs = .025, na.rm = TRUE, names = FALSE)
        estimate$Upper[target] <- apply(values, 2, stats::quantile, probs = .975, na.rm = TRUE, names = FALSE)
      }
    }
  } else {
    estimate$Lower <- NA_real_
    estimate$Upper <- NA_real_
  }
  effective_ratio <- if (bootstrap_reps > 0L) successful / bootstrap_reps else NA_real_
  ci_available <- bootstrap_reps > 0L && successful >= minimum_successful
  time_point_estimates <- if (length(selected_times)) {
    estimate[estimate$Time %in% selected_times, c("Level", "Time", "Survival", "Lower", "Upper"), drop = FALSE]
  } else {
    data.frame()
  }
  time_point_contrasts <- data.frame()
  levels <- levels(data[[group]])
  if (length(selected_times) && length(levels) >= 2L) {
    comparisons <- utils::combn(levels, 2L, simplify = FALSE)
    contrast_rows <- list()
    for (time_value in selected_times) {
      time_index <- match(time_value, times)
      for (comparison in comparisons) {
        first <- comparison[[1]]
        second <- comparison[[2]]
        first_estimate <- estimate$Survival[estimate$Level == first & estimate$Time == time_value][[1]]
        second_estimate <- estimate$Survival[estimate$Level == second & estimate$Time == time_value][[1]]
        difference_values <- ratio_values <- numeric(0)
        if (!is.null(boot_values) && !is.na(time_index)) {
          first_values <- boot_values[[first]][, time_index]
          second_values <- boot_values[[second]][, time_index]
          valid_difference <- is.finite(first_values) & is.finite(second_values)
          difference_values <- second_values[valid_difference] - first_values[valid_difference]
          valid_ratio <- valid_difference & first_values > 0
          ratio_values <- second_values[valid_ratio] / first_values[valid_ratio]
        }
        interval <- function(values) {
          if (!ci_available || length(values) < minimum_successful) return(c(NA_real_, NA_real_))
          stats::quantile(values, probs = c(.025, .975), na.rm = TRUE, names = FALSE)
        }
        difference_ci <- interval(difference_values)
        ratio_ci <- interval(ratio_values)
        contrast_rows[[length(contrast_rows) + 1L]] <- data.frame(
          Time = time_value,
          `First level` = first,
          `Second level` = second,
          Difference = second_estimate - first_estimate,
          `Difference lower` = difference_ci[[1]],
          `Difference upper` = difference_ci[[2]],
          Ratio = if (is.finite(first_estimate) && first_estimate > 0) second_estimate / first_estimate else NA_real_,
          `Ratio lower` = ratio_ci[[1]],
          `Ratio upper` = ratio_ci[[2]],
          `Effective bootstrap draws` = length(difference_values),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
    }
    time_point_contrasts <- do.call(rbind, contrast_rows)
  }
  list(
    method = "Marginal standardization",
    group = group,
    curve = estimate,
    selected_times = selected_times,
    time_point_estimates = time_point_estimates,
    time_point_contrasts = time_point_contrasts,
    bootstrap_reps = bootstrap_reps,
    bootstrap_successful = successful,
    bootstrap_effective_ratio = effective_ratio,
    minimum_successful = minimum_successful,
    ci_available = ci_available,
    ci_method = if (bootstrap_reps > 0L) "Pointwise percentile bootstrap 95% CI" else "Not requested",
    seed = as.integer(seed)
  )
}

prepare_cox_analysis_result <- function(data, time, event, covariates, event_value = "1", variable_info = NULL, reference_values = character(0), adjusted_group = "", adjusted_bootstrap_reps = 2000L, adjusted_times = "", entry = "", start = "", stop = "", subject_id = "", time_origin = "", time_unit = "", event_map = NULL, strata = "", cluster = "", spline_covariate = "", spline_df = 4L, time_varying_covariate = "", time_varying_times = "", ties_method = "efron") {
  previous_contrasts <- getOption("contrasts")
  options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
  on.exit(options(contrasts = previous_contrasts), add = TRUE)
  time <- as.character(time %||% character(0))
  event <- as.character(event %||% character(0))
  time <- time[!is.na(time) & nzchar(time)]
  event <- event[!is.na(event) & nzchar(event)]
  start_input <- as.character(start %||% "")[[1]]
  stop_input <- as.character(stop %||% "")[[1]]
  if (length(time) == 0 && !(nzchar(start_input) && nzchar(stop_input))) stop("Select a time variable, or both start and stop variables.")
  if (length(event) == 0) stop("Select an event variable.")
  time <- if (length(time) > 0) time[[1]] else stop_input
  event <- event[[1]]
  covariates <- survival_selected_names(covariates)
  entry <- survival_selected_names(entry)
  entry <- if (length(entry) > 0) entry[[1]] else ""
  start <- as.character(start %||% "")[[1]]
  stop <- as.character(stop %||% "")[[1]]
  subject_id <- as.character(subject_id %||% "")[[1]]
  strata <- as.character(strata %||% "")[[1]]
  cluster <- as.character(cluster %||% "")[[1]]
  spline_covariate <- as.character(spline_covariate %||% "")[[1]]
  spline_df <- as.integer(spline_df %||% 4L)
  time_varying_covariate <- as.character(time_varying_covariate %||% "")[[1]]
  ties_method <- tolower(as.character(ties_method %||% "efron")[[1]])
  if (!ties_method %in% c("efron", "breslow", "exact")) stop("Cox ties method must be Efron, Breslow, or exact.")
  if (length(covariates) == 0) stop("Select at least one covariate.")
  if (nzchar(strata) && strata %in% covariates) stop("The stratification variable must not also be entered as a Cox covariate because a stratified Cox model does not estimate its hazard ratio.")
  if (nzchar(cluster) && cluster %in% covariates) stop("The cluster variable must not also be entered as a Cox covariate because it identifies correlated observations rather than an estimated effect.")
  if (nzchar(cluster) && nzchar(start)) stop("A separate cluster variable is not available for start-stop Cox models. The required subject ID already defines the robust-variance cluster; multiway clustering is not implemented.")
  if (nzchar(cluster) && identical(cluster, strata)) stop("The same variable cannot be used simultaneously as both the stratification variable and the robust-variance cluster.")
  if (nzchar(spline_covariate) && !spline_covariate %in% covariates) stop("The spline variable must also be included among the Cox covariates.")
  if (nzchar(spline_covariate) && (!is.finite(spline_df) || !spline_df %in% 3:5)) stop("Spline degrees of freedom must be 3, 4, or 5.")
  if (nzchar(spline_covariate) && (nzchar(cluster) || nzchar(subject_id))) stop("Spline nonlinearity testing is not available with cluster-robust Cox variance in survival v1. Use an unclustered model for the partial-likelihood spline comparison or a dedicated robust nonlinear-effect analysis.")
  if (nzchar(time_varying_covariate) && !time_varying_covariate %in% covariates) stop("The time-varying coefficient variable must also be included among the Cox covariates.")
  if (nzchar(time_varying_covariate) && nzchar(spline_covariate)) stop("Spline and time-varying coefficient options cannot be applied in the same survival v1 Cox model.")
  if (nzchar(time_varying_covariate) && (nzchar(cluster) || nzchar(subject_id))) stop("Time-varying coefficient Cox is not available with cluster-robust variance in survival v1.")
  if (nzchar(time_varying_covariate) && nzchar(start)) stop("Time-varying coefficient Cox is not available for start-stop data in survival v1; the start-stop module is reserved for time-dependent covariate values.")
  if (identical(ties_method, "exact") && nzchar(start)) stop("Exact partial likelihood is not available for start-stop Cox models in survival v1.")
  if (identical(ties_method, "exact") && (nzchar(cluster) || nzchar(subject_id))) stop("Exact partial likelihood is not available with cluster-robust Cox variance in survival v1.")
  if (identical(ties_method, "exact") && nzchar(time_varying_covariate)) stop("Exact partial likelihood is not available with a time-varying coefficient in survival v1.")
  settings <- survival_legacy_settings(time, event, event_value, covariates = unique(c(covariates, strata, cluster)), variable_info = variable_info)
  if (nzchar(entry)) {
    settings$data_shape <- "entry_exit"
    settings$roles$entry <- entry
  }
  if (nzchar(start) || nzchar(stop) || nzchar(subject_id)) {
    settings$data_shape <- "start_stop"
    settings$roles$time <- ""
    settings$roles$start <- start
    settings$roles$stop <- stop
    settings$roles$subject_id <- subject_id
  }
  if (is.data.frame(event_map)) {
    settings$legacy <- FALSE
    settings$event_map <- event_map
  }
  preflight <- survival_preflight(data, settings)
  survival_preflight_stop(preflight)
  model_data <- preflight$analysis_data
  if (nzchar(strata)) {
    model_data[[strata]] <- droplevels(factor(model_data[[strata]]))
    if (nlevels(model_data[[strata]]) < 2L) stop("Stratified Cox regression requires at least two observed strata in the analysis sample.")
  }
  effective_cluster <- if (nzchar(subject_id)) subject_id else cluster
  if (nzchar(effective_cluster)) {
    cluster_values <- model_data[[effective_cluster]]
    observed_clusters <- unique(cluster_values[!is.na(cluster_values)])
    if (length(observed_clusters) < 2L) stop("Cluster-robust Cox regression requires at least two observed clusters in the analysis sample.")
  }
  if (nzchar(spline_covariate)) {
    spline_values <- model_data[[spline_covariate]]
    if (!is.numeric(spline_values) || is.factor(spline_values)) stop("Cox spline modeling requires a numeric continuous covariate.")
    if (length(unique(spline_values[is.finite(spline_values)])) < spline_df + 2L) stop("The spline covariate has too few distinct values for the requested degrees of freedom.")
  }
  if (nzchar(time_varying_covariate)) {
    time_varying_values <- model_data[[time_varying_covariate]]
    if (!is.numeric(time_varying_values) || is.factor(time_varying_values)) stop("Time-varying coefficient Cox requires a numeric continuous covariate.")
    if (length(unique(time_varying_values[is.finite(time_varying_values)])) < 3L) stop("The time-varying coefficient covariate has too few distinct values.")
  }
  for (covariate in covariates) {
    if (!is.factor(model_data[[covariate]]) || is.ordered(model_data[[covariate]])) next
    reference <- trimws(named_value(reference_values, covariate, ""))
    if (nzchar(reference) && reference %in% levels(model_data[[covariate]])) {
      model_data[[covariate]] <- stats::relevel(model_data[[covariate]], ref = reference)
    }
  }
  model_data <- survival_apply_treatment_contrasts(model_data, covariates)
  categorical_reference_table <- survival_categorical_reference_table(model_data, covariates)
  formula <- survival_cox_formula(time, event, covariates, entry, start, stop, subject_id, strata, cluster, spline_covariate, spline_df, time_varying_covariate)
  fit <- if (nzchar(time_varying_covariate)) {
    survival::coxph(formula, data = model_data, x = TRUE, y = TRUE, model = FALSE, ties = ties_method, tt = function(x, t, ...) x * log1p(t))
  } else {
    survival::coxph(formula, data = model_data, x = TRUE, y = TRUE, model = TRUE, ties = ties_method)
  }
  fit_summary <- summary(fit)
  coef_summary <- fit_summary$coefficients
  se_column <- if ("robust se" %in% colnames(coef_summary)) "robust se" else "se(coef)"
  coefficient <- coef_summary[, "coef"]
  standard_error <- coef_summary[, se_column]
  z_value <- coefficient / standard_error
  p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
  critical_value <- stats::qnorm(.975)
  coef_table <- data.frame(
    Term = rownames(coef_summary),
    B = coefficient,
    SE = standard_error,
    HR = exp(coefficient),
    LLCI = exp(coefficient - critical_value * standard_error),
    ULCI = exp(coefficient + critical_value * standard_error),
    z = z_value,
    p = p_value,
    row.names = NULL,
    check.names = FALSE
  )
  coef_table$SplineBasis <- if (nzchar(spline_covariate)) grepl("splines::ns(", coef_table$Term, fixed = TRUE) else FALSE
  coef_table$TimeVaryingEffect <- if (nzchar(time_varying_covariate)) coef_table$Term %in% c(time_varying_covariate, paste0("`", time_varying_covariate, "`")) | (startsWith(coef_table$Term, "tt(") & grepl(time_varying_covariate, coef_table$Term, fixed = TRUE)) else FALSE
  coef_table$DisplayTerm <- coef_table$Term
  coef_table$Reference <- FALSE
  reference_rows <- list()
  for (covariate in covariates) {
    values <- model_data[[covariate]]
    if (!is.factor(values) || nlevels(values) < 2L) next
    levels <- levels(values)
    reference_rows[[length(reference_rows) + 1L]] <- data.frame(
      Term = paste0(covariate, "=", levels[[1]]),
      B = NA_real_, SE = NA_real_, HR = 1, LLCI = NA_real_, ULCI = NA_real_, z = NA_real_, p = NA_real_,
      DisplayTerm = paste0(covariate, "=", levels[[1]]),
      Reference = TRUE,
      Variable = covariate,
      SplineBasis = FALSE,
      TimeVaryingEffect = FALSE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    for (level in levels[-1L]) {
      candidates <- c(paste0(covariate, level), paste0("`", covariate, "`", level))
      matched <- coef_table$Term %in% candidates
      coef_table$DisplayTerm[matched] <- paste0(covariate, "=", level)
    }
  }
  coef_table$Variable <- vapply(seq_len(nrow(coef_table)), function(index) {
    matched <- covariates[vapply(covariates, function(covariate) {
      identical(coef_table$Term[[index]], covariate) || startsWith(coef_table$Term[[index]], covariate) || startsWith(coef_table$Term[[index]], paste0("`", covariate, "`"))
    }, logical(1))]
    if (length(matched)) matched[[1]] else ""
  }, character(1))
  if (length(reference_rows)) {
    references <- do.call(rbind, reference_rows)
    ordered_rows <- list()
    used <- rep(FALSE, nrow(coef_table))
    for (covariate in covariates) {
      reference <- references[references$Variable == covariate, , drop = FALSE]
      if (nrow(reference)) ordered_rows[[length(ordered_rows) + 1L]] <- reference
      matched <- coef_table$Variable == covariate
      if (any(matched)) {
        ordered_rows[[length(ordered_rows) + 1L]] <- coef_table[matched, , drop = FALSE]
        used[matched] <- TRUE
      }
    }
    if (any(!used)) ordered_rows[[length(ordered_rows) + 1L]] <- coef_table[!used, , drop = FALSE]
    coef_table <- do.call(rbind, ordered_rows)
    rownames(coef_table) <- NULL
  }
  ph_companion_fit <- NULL
  if (nzchar(time_varying_covariate)) {
    ph_companion_formula <- survival_cox_formula(time, event, covariates, entry, start, stop, subject_id, strata, cluster)
    ph_companion_fit <- survival::coxph(ph_companion_formula, data = model_data, x = TRUE, y = TRUE, model = TRUE, ties = ties_method)
  }
  ph <- tryCatch(survival::cox.zph(ph_companion_fit %||% fit), error = function(e) NULL)
  ph_table <- if (!is.null(ph)) {
    table <- as.data.frame(ph$table, stringsAsFactors = FALSE)
    table$Term <- rownames(table)
    rownames(table) <- NULL
    table[, c("Term", setdiff(names(table), "Term")), drop = FALSE]
  } else {
    data.frame()
  }
  model_test_row <- function(name, value) {
    value <- as.numeric(value %||% numeric(0))
    data.frame(
      Test = name,
      Statistic = if (length(value) >= 1L) value[[1]] else NA_real_,
      df = if (length(value) >= 2L) value[[2]] else NA_real_,
      p = if (length(value) >= 3L) value[[3]] else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  model_tests <- do.call(rbind, list(
    model_test_row("Likelihood-ratio", fit_summary$logtest),
    model_test_row("Wald", fit_summary$waldtest),
    model_test_row("Score", fit_summary$sctest)
  ))
  martingale <- tryCatch(stats::residuals(fit, type = "martingale"), error = function(e) numeric(0))
  deviance <- tryCatch(stats::residuals(fit, type = "deviance"), error = function(e) numeric(0))
  functional_form_data <- survival_cox_functional_form_data(model_data, covariates, martingale)
  residual_summary <- function(values, type) {
    values <- as.numeric(values)
    values <- values[is.finite(values)]
    if (length(values) == 0) return(NULL)
    quantiles <- stats::quantile(values, probs = c(0, .25, .5, .75, 1), names = FALSE)
    data.frame(Type = type, Min = quantiles[[1]], Q1 = quantiles[[2]], Median = quantiles[[3]], Q3 = quantiles[[4]], Max = quantiles[[5]], stringsAsFactors = FALSE)
  }
  residual_rows <- Filter(Negate(is.null), list(residual_summary(martingale, "Martingale"), residual_summary(deviance, "Deviance")))
  residual_table <- if (length(residual_rows) > 0) do.call(rbind, residual_rows) else data.frame()
  dfbeta <- tryCatch(stats::residuals(fit, type = "dfbeta"), error = function(e) NULL)
  dfbetas <- tryCatch(stats::residuals(fit, type = "dfbetas"), error = function(e) NULL)
  if (!is.null(dfbeta)) {
    if (is.null(dim(dfbeta))) dfbeta <- matrix(dfbeta, ncol = 1L, dimnames = list(names(dfbeta), names(stats::coef(fit))[[1]]))
    if (is.null(colnames(dfbeta))) colnames(dfbeta) <- names(stats::coef(fit))
    if (!is.null(dfbetas) && is.null(dim(dfbetas))) dfbetas <- matrix(dfbetas, ncol = 1L, dimnames = list(names(dfbetas), names(stats::coef(fit))[[1]]))
    if (!is.null(dfbetas) && is.null(colnames(dfbetas))) colnames(dfbetas) <- names(stats::coef(fit))
    dfbetas_threshold <- 2 / sqrt(nrow(model_data))
    influence_table <- do.call(rbind, lapply(seq_len(ncol(dfbeta)), function(index) {
      values <- abs(dfbeta[, index])
      maximum <- if (any(is.finite(values))) max(values, na.rm = TRUE) else NA_real_
      standardized <- if (!is.null(dfbetas) && ncol(dfbetas) >= index) abs(dfbetas[, index]) else rep(NA_real_, length(values))
      standardized_maximum <- if (any(is.finite(standardized))) max(standardized, na.rm = TRUE) else NA_real_
      row_index <- if (is.finite(standardized_maximum)) which.max(standardized) else if (is.finite(maximum)) which.max(values) else NA_integer_
      data.frame(
        Term = colnames(dfbeta)[[index]],
        `Maximum absolute DFBETA` = maximum,
        `Maximum absolute DFBETAS` = standardized_maximum,
        `Screening threshold` = dfbetas_threshold,
        `Review signal` = is.finite(standardized_maximum) && standardized_maximum > dfbetas_threshold,
        `Source row in analysis data` = row_index,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    influence_table <- data.frame()
  }
  collinearity <- survival_cox_collinearity(fit$x)
  concordance <- fit_summary$concordance
  parameter_count <- length(stats::coef(fit))
  strata_table <- if (nzchar(strata)) {
    strata_levels <- levels(model_data[[strata]])
    do.call(rbind, lapply(strata_levels, function(level) {
      selected <- model_data[[strata]] == level
      data.frame(
        Stratum = level,
        Records = sum(selected),
        Events = sum(model_data[[event]][selected], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    data.frame()
  }
  cluster_summary <- if (nzchar(effective_cluster)) {
    cluster_values <- as.character(model_data[[effective_cluster]])
    records_by_cluster <- table(cluster_values)
    events_by_cluster <- tapply(model_data[[event]], cluster_values, sum, na.rm = TRUE)
    data.frame(
      `Cluster variable` = effective_cluster,
      Clusters = length(records_by_cluster),
      `Minimum records per cluster` = min(as.numeric(records_by_cluster)),
      `Median records per cluster` = stats::median(as.numeric(records_by_cluster)),
      `Maximum records per cluster` = max(as.numeric(records_by_cluster)),
      `Clusters with events` = sum(events_by_cluster > 0),
      `Clusters without events` = sum(events_by_cluster == 0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame()
  }
  spline_test_table <- spline_curve <- data.frame()
  if (nzchar(spline_covariate)) {
    linear_formula <- survival_cox_formula(time, event, covariates, entry, start, stop, subject_id, strata, cluster)
    linear_fit <- survival::coxph(linear_formula, data = model_data, x = TRUE, y = TRUE, model = TRUE, ties = ties_method)
    full_loglik <- stats::logLik(fit)
    linear_loglik <- stats::logLik(linear_fit)
    test_df <- as.integer(attr(full_loglik, "df") - attr(linear_loglik, "df"))
    statistic <- max(0, 2 * (as.numeric(full_loglik) - as.numeric(linear_loglik)))
    spline_reference <- stats::median(as.numeric(model_data[[spline_covariate]]), na.rm = TRUE)
    spline_test_table <- data.frame(
      Variable = spline_covariate,
      `Spline df` = spline_df,
      `Reference value` = spline_reference,
      `Nonlinearity LR chi-square` = statistic,
      df = test_df,
      p = if (test_df > 0L) stats::pchisq(statistic, df = test_df, lower.tail = FALSE) else NA_real_,
      `Linear-model AIC` = stats::AIC(linear_fit),
      `Spline-model AIC` = stats::AIC(fit),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    spline_curve <- survival_cox_spline_curve(fit, model_data, spline_covariate)
  }
  time_varying_test_table <- time_varying_curve <- time_varying_at_times <- data.frame()
  selected_time_varying_times <- survival_parse_times_strict(time_varying_times, "Time-varying HR reporting times")
  if (nzchar(time_varying_covariate)) {
    maximum_followup <- max(as.numeric(model_data[[time]]), na.rm = TRUE)
    if (length(selected_time_varying_times) && any(selected_time_varying_times > maximum_followup)) stop(sprintf("Time-varying HR reporting times must not exceed the maximum observed follow-up time (%s).", format(maximum_followup, trim = TRUE)))
    if (!length(selected_time_varying_times)) {
      event_times <- as.numeric(model_data[[time]][as.logical(model_data[[event]])])
      selected_time_varying_times <- sort(unique(as.numeric(stats::quantile(event_times, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE))))
    }
    time_varying_curve <- survival_cox_time_varying_curve(fit, model_data, time_varying_covariate, time, event, selected_time_varying_times)
    time_varying_at_times <- time_varying_curve[time_varying_curve$Time %in% selected_time_varying_times, , drop = FALSE]
    time_term <- which(coef_table$TimeVaryingEffect & startsWith(coef_table$Term, "tt("))
    time_varying_test_table <- data.frame(
      Variable = time_varying_covariate,
      `Time function` = "log(1 + time)",
      `Interaction B` = coef_table$B[time_term],
      SE = coef_table$SE[time_term],
      z = coef_table$z[time_term],
      p = coef_table$p[time_term],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  adjusted_group <- as.character(adjusted_group %||% "")[[1]]
  if (nzchar(start) && nzchar(adjusted_group)) stop("Marginal adjusted survival is not available for start-stop Cox models in survival v1.")
  if (nzchar(strata) && nzchar(adjusted_group)) stop("Marginal adjusted survival is not available with stratified Cox regression in survival v1. Report the stratified hazard-ratio model separately or remove stratification before requesting standardized curves.")
  if (nzchar(cluster) && nzchar(adjusted_group)) stop("Marginal adjusted survival is not available with user-specified clustered Cox regression in survival v1 because valid uncertainty estimation requires cluster-level resampling.")
  if (nzchar(time_varying_covariate) && nzchar(adjusted_group)) stop("Marginal adjusted survival is not available with a time-varying coefficient Cox model in survival v1.")
  selected_adjusted_times <- survival_parse_times_strict(adjusted_times, "Adjusted-survival time points")
  if (nzchar(adjusted_group) && length(selected_adjusted_times)) {
    followup_name <- if (nzchar(stop)) stop else time
    maximum_followup <- max(as.numeric(model_data[[followup_name]]), na.rm = TRUE)
    if (any(selected_adjusted_times > maximum_followup)) {
      stop(sprintf("Adjusted-survival time points must not exceed the maximum observed follow-up time (%s).", format(maximum_followup, trim = TRUE)))
    }
  }
  adjusted_survival <- if (nzchar(adjusted_group)) survival_adjusted_curve(fit, model_data, adjusted_group, adjusted_bootstrap_reps, selected_times = selected_adjusted_times) else NULL
  event_time_name <- if (nzchar(stop)) stop else time
  event_times <- as.numeric(model_data[[event_time_name]][as.logical(model_data[[event]])])
  event_time_counts <- table(event_times[is.finite(event_times)])
  tied_counts <- as.numeric(event_time_counts[event_time_counts > 1L])
  ties_summary <- data.frame(
    Method = switch(ties_method, efron = "Efron", breslow = "Breslow", exact = "Exact"),
    Events = length(event_times),
    `Distinct event times` = length(event_time_counts),
    `Tied event times` = length(tied_counts),
    `Events at tied times` = sum(tied_counts),
    `Maximum events at one time` = if (length(event_time_counts)) max(as.numeric(event_time_counts)) else 0L,
    `Proportion of events at tied times` = if (length(event_times)) sum(tied_counts) / length(event_times) else NA_real_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  variance_label <- if (identical(se_column, "robust se")) if (nzchar(subject_id)) "Subject-cluster robust" else sprintf("Cluster-robust (%s)", effective_cluster) else "Model-based"
  categorical_joint_tests <- survival_cox_categorical_joint_tests(fit, coef_table, model_data, covariates, variance_label)
  list(
    type = "cox",
    fit = fit,
    formula = formula,
    data = model_data,
    preflight = preflight,
    time = time,
    entry = entry,
    start = start,
    stop = stop,
    subject_id = subject_id,
    cluster = effective_cluster,
    cluster_summary = cluster_summary,
    spline_covariate = spline_covariate,
    spline_df = if (nzchar(spline_covariate)) spline_df else NA_integer_,
    spline_test_table = spline_test_table,
    spline_curve = spline_curve,
    time_varying_covariate = time_varying_covariate,
    time_varying_function = if (nzchar(time_varying_covariate)) "log(1 + time)" else "",
    time_varying_test_table = time_varying_test_table,
    time_varying_curve = time_varying_curve,
    time_varying_at_times = time_varying_at_times,
    ph_source = if (nzchar(time_varying_covariate)) "Companion proportional-hazards model before time-varying extension" else "Fitted Cox model",
    strata = strata,
    strata_table = strata_table,
    time_origin = as.character(time_origin %||% "")[[1]],
    time_unit = as.character(time_unit %||% "")[[1]],
    event = event,
    covariates = covariates,
    event_value = event_value,
    n = nrow(model_data),
    events = sum(model_data[[event]], na.rm = TRUE),
    coef_table = coef_table,
    categorical_reference_table = categorical_reference_table,
    categorical_joint_tests = categorical_joint_tests,
    ph = ph,
    ph_table = ph_table,
    model_tests = model_tests,
    residual_table = residual_table,
    functional_form_data = functional_form_data,
    influence_table = influence_table,
    collinearity_table = collinearity$table,
    condition_number = collinearity$condition_number,
    parameter_count = parameter_count,
    events_per_parameter = if (parameter_count > 0) sum(model_data[[event]], na.rm = TRUE) / parameter_count else NA_real_,
    ties = switch(ties_method, efron = "Efron", breslow = "Breslow", exact = "Exact"),
    ties_method = ties_method,
    ties_summary = ties_summary,
    variance = variance_label,
    adjusted_survival = adjusted_survival,
    concordance = concordance,
    packages = package_version_label("survival")
  )
}

survival_competing_event_map <- function(values, censored_value, event_of_interest, competing_values) {
  observed <- unique(trimws(as.character(values[!is.na(values)])))
  censored_value <- trimws(as.character(censored_value %||% "0")[[1]])
  event_of_interest <- trimws(as.character(event_of_interest %||% "1")[[1]])
  competing_values <- trimws(as.character(competing_values %||% character(0)))
  competing_values <- competing_values[nzchar(competing_values)]
  roles <- ifelse(observed == censored_value, "censored", ifelse(observed == event_of_interest, "event_of_interest", ifelse(observed %in% competing_values, "competing_event", "unknown")))
  data.frame(raw_value = observed, role = roles, label = observed, stringsAsFactors = FALSE)
}

survival_competing_status <- function(raw_values, event_map) {
  raw <- trimws(as.character(raw_values))
  role <- stats::setNames(event_map$role, event_map$raw_value)[raw]
  competing_codes <- event_map$raw_value[event_map$role == "competing_event"]
  competing_lookup <- stats::setNames(seq_along(competing_codes) + 1L, competing_codes)
  status <- rep(NA_integer_, length(raw))
  status[role == "censored"] <- 0L
  status[role == "event_of_interest"] <- 1L
  status[role == "competing_event"] <- unname(competing_lookup[raw[role == "competing_event"]])
  status
}

survival_cuminc_curve_table <- function(fit, group_present = TRUE) {
  component_names <- setdiff(names(fit), "Tests")
  rows <- lapply(component_names, function(name) {
    component <- fit[[name]]
    if (!is.list(component) || !all(c("time", "est", "var") %in% names(component))) return(NULL)
    parts <- strsplit(name, " ", fixed = TRUE)[[1]]
    cause <- tail(parts, 1L)
    group <- if (isTRUE(group_present)) paste(head(parts, -1L), collapse = " ") else "All"
    data.frame(Group = group, CauseCode = cause, Time = as.numeric(component$time), CIF = as.numeric(component$est), Variance = as.numeric(component$var), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  se <- sqrt(pmax(out$Variance, 0))
  out$Lower <- pmax(0, out$CIF - stats::qnorm(.975) * se)
  out$Upper <- pmin(1, out$CIF + stats::qnorm(.975) * se)
  out
}

survival_competing_event_count_table <- function(status, group_values, event_map) {
  cause_map <- rbind(
    event_map[event_map$role == "event_of_interest", c("raw_value", "role", "label"), drop = FALSE],
    event_map[event_map$role == "competing_event", c("raw_value", "role", "label"), drop = FALSE]
  )
  if (!nrow(cause_map)) return(data.frame())
  cause_map$CauseCode <- seq_len(nrow(cause_map))
  groups <- unique(as.character(group_values))
  rows <- lapply(groups, function(group) {
    in_group <- as.character(group_values) == group
    do.call(rbind, lapply(seq_len(nrow(cause_map)), function(index) {
      events <- sum(status[in_group] == cause_map$CauseCode[[index]], na.rm = TRUE)
      data.frame(
        Group = group,
        CauseCode = cause_map$CauseCode[[index]],
        `Raw event value` = as.character(cause_map$raw_value[[index]]),
        `Event role` = as.character(cause_map$role[[index]]),
        Label = as.character(cause_map$label[[index]]),
        `Analysis N` = sum(in_group),
        Events = events,
        `Event proportion` = if (sum(in_group) > 0L) events / sum(in_group) else NA_real_,
        `Review signal` = events < 5L,
        `Review reason` = if (events == 0L) "No events in this group-cause cell" else if (events < 5L) "Fewer than 5 events in this group-cause cell" else "",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

survival_competing_estimand_table <- function(event_map, regression = "none", group_present = FALSE) {
  interest <- event_map[event_map$role == "event_of_interest", , drop = FALSE]
  competing <- event_map[event_map$role == "competing_event", , drop = FALSE]
  if (nrow(interest) != 1L) return(data.frame())
  models <- "Cumulative incidence function"
  if (isTRUE(group_present)) models <- c(models, "Gray test")
  if (regression %in% c("cause_specific", "both")) models <- c(models, "Cause-specific Cox")
  if (regression %in% c("fine_gray", "both")) models <- c(models, "Fine-Gray")
  measure <- c(
    "CIF",
    if (isTRUE(group_present)) "Gray chi-square / p (no effect size)",
    if (regression %in% c("cause_specific", "both")) "Cause-specific HR",
    if (regression %in% c("fine_gray", "both")) "Subdistribution HR (sHR)"
  )
  handling <- c(
    "Retained as distinct failure causes in cumulative-incidence estimation",
    if (isTRUE(group_present)) "Retained as distinct failure causes in cumulative-incidence comparison",
    if (regression %in% c("cause_specific", "both")) "Removed from the event-free risk set at occurrence",
    if (regression %in% c("fine_gray", "both")) "Retained in the subdistribution risk set"
  )
  interpretation <- c(
    "Absolute cumulative incidence of the target cause",
    if (isTRUE(group_present)) "Equality of target-cause CIFs across groups",
    if (regression %in% c("cause_specific", "both")) "Instantaneous hazard of the target cause among event-free subjects",
    if (regression %in% c("fine_gray", "both")) "Subdistribution hazard associated with the target-cause CIF"
  )
  data.frame(
    Model = models,
    `Target event value` = as.character(interest$raw_value[[1]]),
    `Target event label` = as.character(interest$label[[1]]),
    `Competing event values` = paste(as.character(competing$raw_value), collapse = ", "),
    Measure = measure,
    `Competing-event handling` = handling,
    `Interpretation target` = interpretation,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

survival_censoring_group_table <- function(status, censoring_values = NULL) {
  strata <- if (is.null(censoring_values)) rep("All analysis records", length(status)) else as.character(censoring_values)
  groups <- unique(strata)
  rows <- lapply(groups, function(group) {
    keep <- strata == group
    n <- sum(keep)
    censored <- sum(status[keep] == 0L, na.rm = TRUE)
    interest <- sum(status[keep] == 1L, na.rm = TRUE)
    competing <- sum(status[keep] > 1L, na.rm = TRUE)
    reasons <- c(if (n < 10L) "Fewer than 10 analysis records", if (censored == 0L) "No censored observations" else if (censored < 5L) "Fewer than 5 censored observations")
    data.frame(
      `Censoring stratum` = group,
      N = n,
      Censored = censored,
      `Interest events` = interest,
      `Competing events` = competing,
      `Censoring proportion` = if (n > 0L) censored / n else NA_real_,
      `Review signal` = length(reasons) > 0L,
      `Review reason` = paste(reasons, collapse = "; "),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

survival_fine_gray_numerical_diagnostics <- function(fit, design, gtol = 1e-6, maxiter = 10L) {
  collinearity <- survival_cox_collinearity(design)
  coefficients <- as.numeric(fit$coef %||% numeric(0))
  score <- as.numeric(fit$score %||% numeric(0))
  log_likelihood <- as.numeric(fit$loglik %||% NA_real_)
  relative_score <- if (length(coefficients) && length(score) == length(coefficients) && is.finite(log_likelihood)) {
    max(abs(score) * pmax(abs(coefficients), 1), na.rm = TRUE) / max(abs(log_likelihood), 1)
  } else NA_real_
  information <- as.matrix(fit$inf %||% matrix(numeric(0), 0L, 0L))
  information_finite <- length(information) > 0L && all(is.finite(information))
  information_rank <- if (information_finite) qr(information)$rank else NA_integer_
  information_condition <- NA_real_
  if (information_finite && nrow(information) > 0L && nrow(information) == ncol(information)) {
    diagonal <- diag(information)
    if (all(is.finite(diagonal) & diagonal > 0)) {
      standardized <- information / sqrt(outer(diagonal, diagonal))
      information_condition <- tryCatch(kappa(standardized, exact = TRUE), error = function(e) NA_real_)
    }
  }
  covariance <- as.matrix(fit$var %||% matrix(numeric(0), 0L, 0L))
  covariance_finite <- length(covariance) > 0L && all(is.finite(covariance))
  positive_standard_errors <- covariance_finite && all(diag(covariance) > 0)
  optimizer_table <- data.frame(
    Converged = isTRUE(fit$converged),
    `Log likelihood` = log_likelihood,
    `Maximum absolute score` = if (length(score) && any(is.finite(score))) max(abs(score), na.rm = TRUE) else NA_real_,
    `Relative score criterion` = relative_score,
    `Convergence tolerance` = as.numeric(gtol),
    `Maximum iterations` = as.integer(maxiter),
    `Information rank` = information_rank,
    Parameters = length(coefficients),
    `Standardized information condition number` = as.numeric(information_condition),
    `Finite covariance matrix` = covariance_finite,
    `Positive standard errors` = positive_standard_errors,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(
    optimizer_table = optimizer_table,
    collinearity_table = collinearity$table,
    condition_number = collinearity$condition_number
  )
}

survival_gray_test_table <- function(fit, cause_codes = character(0)) {
  tests <- fit$Tests
  table <- if (is.null(tests) || length(tests) == 0) {
    data.frame(CauseCode = character(0), Statistic = numeric(0), p = numeric(0), df = numeric(0), stringsAsFactors = FALSE)
  } else {
    tests <- as.data.frame(tests, stringsAsFactors = FALSE)
    tests$CauseCode <- rownames(tests)
    rownames(tests) <- NULL
    data.frame(CauseCode = tests$CauseCode, Statistic = tests$stat, p = tests$pv, df = tests$df, stringsAsFactors = FALSE)
  }
  cause_codes <- unique(as.character(cause_codes))
  missing_codes <- setdiff(cause_codes, as.character(table$CauseCode))
  if (length(missing_codes)) {
    table <- rbind(table, data.frame(CauseCode = missing_codes, Statistic = NA_real_, p = NA_real_, df = NA_real_, stringsAsFactors = FALSE))
  }
  if (length(cause_codes) && nrow(table)) table <- table[match(cause_codes, as.character(table$CauseCode)), , drop = FALSE]
  table$Estimable <- is.finite(table$Statistic) & is.finite(table$p) & is.finite(table$df) & table$df > 0
  rownames(table) <- NULL
  table
}

survival_cif_at_times <- function(curve, times) {
  if (!is.data.frame(curve) || nrow(curve) == 0 || length(times) == 0) return(data.frame())
  keys <- unique(curve[, c("Group", "CauseCode"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(index) {
    subset <- curve[curve$Group == keys$Group[[index]] & curve$CauseCode == keys$CauseCode[[index]], , drop = FALSE]
    values <- function(column) stats::approx(subset$Time, subset[[column]], xout = times, method = "constant", f = 0, rule = 2, ties = "ordered")$y
    data.frame(Group = keys$Group[[index]], CauseCode = keys$CauseCode[[index]], Time = times, CIF = values("CIF"), Lower = values("Lower"), Upper = values("Upper"), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

survival_cif_integrity_table <- function(curve, tolerance = 1e-10) {
  if (!is.data.frame(curve) || !nrow(curve)) return(data.frame())
  groups <- unique(as.character(curve$Group))
  rows <- lapply(groups, function(group) {
    subset <- curve[as.character(curve$Group) == group, , drop = FALSE]
    times <- sort(unique(as.numeric(subset$Time)))
    evaluated <- survival_cif_at_times(subset, times)
    sums <- if (nrow(evaluated)) stats::aggregate(CIF ~ Time, data = evaluated, sum)$CIF else numeric(0)
    minimum <- suppressWarnings(min(evaluated$CIF, na.rm = TRUE))
    maximum <- suppressWarnings(max(evaluated$CIF, na.rm = TRUE))
    maximum_sum <- suppressWarnings(max(sums, na.rm = TRUE))
    finite <- length(evaluated$CIF) > 0L && all(is.finite(evaluated$CIF))
    within_bounds <- finite && minimum >= -tolerance && maximum <= 1 + tolerance
    sum_within_one <- length(sums) > 0L && all(is.finite(sums)) && maximum_sum <= 1 + tolerance
    data.frame(
      Group = group,
      `Minimum CIF` = if (is.finite(minimum)) minimum else NA_real_,
      `Maximum CIF` = if (is.finite(maximum)) maximum else NA_real_,
      `Maximum sum of cause-specific CIFs` = if (is.finite(maximum_sum)) maximum_sum else NA_real_,
      `All CIF values finite` = finite,
      `CIF values within [0,1]` = within_bounds,
      `Cause-specific CIF sum <= 1` = sum_within_one,
      `Integrity passed` = finite && within_bounds && sum_within_one,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

prepare_competing_risk_result <- function(
  data,
  time,
  event,
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "",
  rate_times = NULL,
  covariates = character(0),
  regression = "none",
  variable_info = NULL,
  time_origin = "",
  time_unit = "",
  event_map = NULL,
  censoring_group = ""
) {
  previous_contrasts <- getOption("contrasts")
  options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
  on.exit(options(contrasts = previous_contrasts), add = TRUE)
  if (!requireNamespace("cmprsk", quietly = TRUE)) stop("Package 'cmprsk' is required for cumulative incidence and Gray's test.")
  time <- survival_selected_names(time)
  event <- survival_selected_names(event)
  group <- survival_selected_names(group)
  if (length(time) == 0) stop("Select a time variable.")
  if (length(event) == 0) stop("Select an event variable.")
  time <- time[[1]]
  event <- event[[1]]
  group <- if (length(group) > 0) group[[1]] else ""
  covariates <- survival_selected_names(covariates)
  censoring_group <- survival_selected_names(censoring_group)
  censoring_group <- if (length(censoring_group)) censoring_group[[1]] else ""
  regression <- as.character(regression %||% "none")[[1]]
  if (!regression %in% c("none", "cause_specific", "fine_gray", "both")) regression <- "none"
  if (!identical(regression, "none") && length(covariates) == 0) stop("Select at least one covariate for competing-risks regression.")
  if (nzchar(censoring_group) && !regression %in% c("fine_gray", "both")) stop("Censoring-distribution stratification (cengroup) is available only with Fine-Gray regression.")
  if (nzchar(censoring_group) && !censoring_group %in% names(data)) stop("The selected censoring-distribution stratification variable was not found.")
  if (nzchar(censoring_group) && censoring_group %in% c(time, event)) stop("The censoring-distribution stratification variable cannot also be the time or event variable.")
  competing_values <- unlist(strsplit(paste(competing_values, collapse = ","), "[,;[:space:]]+"))
  competing_values <- competing_values[nzchar(competing_values)]
  event_map <- if (is.data.frame(event_map)) survival_normalize_event_map(data[[event]], event_map, event_of_interest) else survival_competing_event_map(data[[event]], censored_value, event_of_interest, competing_values)
  interest_values <- as.character(event_map$raw_value[event_map$role == "event_of_interest"])
  if (length(interest_values) != 1L) stop("Select exactly one event-of-interest code for competing-risks analysis.")
  event_of_interest <- interest_values[[1]]
  settings <- survival_legacy_settings(time, event, event_of_interest, group)
  settings$legacy <- FALSE
  settings$objective <- "competing"
  settings$event_map <- event_map
  settings$roles$covariates <- unique(c(covariates, censoring_group[nzchar(censoring_group)]))
  settings$variable_info <- variable_info
  preflight <- survival_preflight(data, settings)
  survival_preflight_stop(preflight)
  model_data <- preflight$analysis_data
  model_data <- survival_apply_treatment_contrasts(model_data, covariates)
  categorical_reference_table <- survival_categorical_reference_table(model_data, covariates)
  coefficient_label_table <- survival_coefficient_label_table(model_data, covariates)
  raw_event_column <- preflight$transformations$raw_event_column
  status <- survival_competing_status(model_data[[raw_event_column]], event_map)
  censoring_values <- if (nzchar(censoring_group)) droplevels(as.factor(model_data[[censoring_group]])) else NULL
  if (!is.null(censoring_values) && nlevels(censoring_values) < 2L) stop("Censoring-distribution stratification requires at least two observed levels.")
  if (!is.null(censoring_values) && nlevels(censoring_values) > 20L) stop("Censoring-distribution stratification must use a categorical variable with no more than 20 observed levels.")
  censoring_group_table <- survival_censoring_group_table(status, censoring_values)
  group_values <- if (nzchar(group)) droplevels(as.factor(model_data[[group]])) else rep("All", nrow(model_data))
  event_count_table <- survival_competing_event_count_table(status, group_values, event_map)
  estimand_table <- survival_competing_estimand_table(event_map, regression, group_present = nzchar(group))
  estimand_table$`Censoring distribution` <- if (nzchar(censoring_group)) paste("Estimated separately within", censoring_group) else "Pooled across the analysis sample"
  fit <- cmprsk::cuminc(ftime = model_data[[time]], fstatus = status, group = group_values, cencode = 0)
  curve <- survival_cuminc_curve_table(fit, group_present = nzchar(group))
  cif_integrity <- survival_cif_integrity_table(curve)
  expected_causes <- if (nzchar(group)) sort(unique(status[is.finite(status) & status > 0L])) else integer(0)
  tests <- survival_gray_test_table(fit, expected_causes)
  times <- survival_parse_times(rate_times)
  if (length(times) == 0) times <- survival_default_risk_times(model_data[[time]])
  cause_specific <- NULL
  if (regression %in% c("cause_specific", "both")) {
    cs_formula <- survival_cox_formula(time, event, covariates)
    cs_fit <- survival::coxph(cs_formula, data = model_data, x = TRUE, y = TRUE, model = TRUE, ties = "efron")
    cs_summary <- summary(cs_fit)
    cs_coef <- cs_summary$coefficients
    cs_ci <- cs_summary$conf.int
    cs_table <- data.frame(
      Term = rownames(cs_coef), B = cs_coef[, "coef"], SE = cs_coef[, "se(coef)"],
      HR = cs_ci[, "exp(coef)"], LLCI = cs_ci[, "lower .95"], ULCI = cs_ci[, "upper .95"],
      z = cs_coef[, "z"], p = cs_coef[, "Pr(>|z|)"], row.names = NULL, check.names = FALSE
    )
    cs_diagnostics <- survival_cox_basic_diagnostics(cs_fit, model_data, covariates)
    cs_model_tests <- survival_cox_omnibus_tests(cs_fit)
    cs_categorical_joint_tests <- survival_categorical_joint_tests_from_design(stats::coef(cs_fit), stats::vcov(cs_fit), model_data, covariates, "Cause-specific Cox model-based")
    cs_estimable <- nrow(cs_table) > 0L && all(is.finite(unlist(cs_table[, c("B", "SE", "HR", "LLCI", "ULCI", "z", "p"), drop = FALSE])))
    cause_specific <- c(list(fit = cs_fit, coef_table = cs_table, model_tests = cs_model_tests, categorical_joint_tests = cs_categorical_joint_tests, estimable = cs_estimable), cs_diagnostics)
  }
  fine_gray <- NULL
  if (regression %in% c("fine_gray", "both")) {
    design_formula <- stats::as.formula(paste("~", paste(sprintf("`%s`", covariates), collapse = " + ")))
    design <- stats::model.matrix(design_formula, data = model_data)
    design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
    if (ncol(design) == 0) stop("Fine-Gray regression requires at least one estimable covariate column.")
    fg_gtol <- 1e-6
    fg_maxiter <- 10L
    fg_fit <- if (is.null(censoring_values)) {
      cmprsk::crr(ftime = model_data[[time]], fstatus = status, cov1 = design, failcode = 1L, cencode = 0L, gtol = fg_gtol, maxiter = fg_maxiter)
    } else {
      cmprsk::crr(ftime = model_data[[time]], fstatus = status, cov1 = design, failcode = 1L, cencode = 0L, cengroup = censoring_values, gtol = fg_gtol, maxiter = fg_maxiter)
    }
    fg_coef <- as.numeric(fg_fit$coef)
    fg_se <- sqrt(diag(fg_fit$var))
    fg_z <- fg_coef / fg_se
    fg_table <- data.frame(
      Term = names(fg_fit$coef), B = fg_coef, SE = fg_se, sHR = exp(fg_coef),
      LLCI = exp(fg_coef - stats::qnorm(.975) * fg_se), ULCI = exp(fg_coef + stats::qnorm(.975) * fg_se),
      z = fg_z, p = 2 * stats::pnorm(abs(fg_z), lower.tail = FALSE), stringsAsFactors = FALSE
    )
    fg_residual_data <- if (!is.null(fg_fit$res) && length(fg_fit$uftime)) {
      residual_matrix <- as.matrix(fg_fit$res)
      if (is.null(colnames(residual_matrix))) colnames(residual_matrix) <- colnames(design)
      do.call(rbind, lapply(seq_len(ncol(residual_matrix)), function(index) data.frame(
        `Event time` = as.numeric(fg_fit$uftime),
        Term = colnames(residual_matrix)[[index]],
        Residual = as.numeric(residual_matrix[, index]),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )))
    } else data.frame()
    fg_residual_review <- if (is.data.frame(fg_residual_data) && nrow(fg_residual_data) && length(unique(fg_residual_data$`Event time`)) > 2L) {
      review <- do.call(rbind, lapply(unique(fg_residual_data$Term), function(term) {
        values <- fg_residual_data[fg_residual_data$Term == term, , drop = FALSE]
        test <- tryCatch(stats::cor.test(values$`Event time`, values$Residual, method = "spearman", exact = FALSE), error = function(e) NULL)
        data.frame(
          Term = term,
          `Unique target-failure times` = length(unique(values$`Event time`)),
          `Spearman rho` = if (is.null(test)) NA_real_ else unname(test$estimate),
          `Exploratory p` = if (is.null(test)) NA_real_ else test$p.value,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }))
      review$`Holm-adjusted p` <- stats::p.adjust(review$`Exploratory p`, method = "holm")
      review$`Review signal` <- is.finite(review$`Holm-adjusted p`) & review$`Holm-adjusted p` < .05
      review
    } else data.frame()
    fg_numerical <- survival_fine_gray_numerical_diagnostics(fg_fit, design, fg_gtol, fg_maxiter)
    fg_model_tests <- survival_fine_gray_omnibus_test(fg_fit)
    fg_categorical_joint_tests <- survival_categorical_joint_tests_from_design(fg_fit$coef, fg_fit$var, model_data, covariates, "Fine-Gray model-based")
    fg_estimable <- nrow(fg_table) > 0L && all(is.finite(unlist(fg_table[, c("B", "SE", "sHR", "LLCI", "ULCI", "z", "p"), drop = FALSE])))
    fine_gray <- c(list(fit = fg_fit, design_columns = colnames(design), coef_table = fg_table, model_tests = fg_model_tests, categorical_joint_tests = fg_categorical_joint_tests, residual_data = fg_residual_data, residual_review = fg_residual_review, converged = isTRUE(fg_fit$converged), estimable = fg_estimable, censoring_group = censoring_group), fg_numerical)
  }
  list(
    type = "competing_risk",
    fit = fit,
    data = model_data,
    preflight = preflight,
    time = time,
    time_origin = as.character(time_origin %||% "")[[1]],
    time_unit = as.character(time_unit %||% "")[[1]],
    event = event,
    group = group,
    event_map = event_map,
    event_of_interest = event_of_interest,
    status = status,
    n = nrow(model_data),
    curve = curve,
    cif_integrity = cif_integrity,
    event_count_table = event_count_table,
    estimand_table = estimand_table,
    censoring_group = censoring_group,
    censoring_group_table = censoring_group_table,
    gray_tests = tests,
    covariates = covariates,
    categorical_reference_table = categorical_reference_table,
    coefficient_label_table = coefficient_label_table,
    regression = regression,
    cause_specific = cause_specific,
    fine_gray = fine_gray,
    rate_times = times,
    cif_at_times = survival_cif_at_times(curve, times),
    packages = paste(package_version_label("survival"), package_version_label("cmprsk"), sep = "; ")
  )
}
