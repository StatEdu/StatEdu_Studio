script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_survival_preflight.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(htmltools))
source(file.path(repo_root, "R", "utils.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_model_summary.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_table_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "analysis_survival.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_survival_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "setup_survival_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_export_files.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_saved_ui.R"), encoding = "UTF-8")
source(file.path(repo_root, "R", "result_export.R"), encoding = "UTF-8")

message("Checking deterministic survival design recommendations...")
rec_group <- survival_recommend(list(objective = "group_comparison", data_shape = "single_record", event_structure = "single"))
stopifnot(identical(rec_group$status, "ready"), identical(rec_group$rule_ids, "G01"), identical(rec_group$target_tab, "analysis_survival_km"))
rec_assoc <- survival_recommend(list(objective = "association", data_shape = "single_record", event_structure = "single"))
stopifnot(identical(rec_assoc$rule_ids, "A01"), identical(rec_assoc$target_tab, "analysis_survival_cox"))
rec_competing <- survival_recommend(list(objective = "association", data_shape = "single_record", event_structure = "competing", competing_estimand = "both"))
stopifnot(identical(rec_competing$rule_ids, "A05"), identical(rec_competing$target_tab, "analysis_survival_competing"))
rec_confirm <- survival_recommend(list(objective = "association", data_shape = "single_record", event_structure = "competing"))
stopifnot(identical(rec_confirm$status, "needs_confirmation"), identical(rec_confirm$rule_ids, "C07"))
rec_unsupported <- survival_recommend(list(objective = "prediction", data_shape = "single_record", event_structure = "single"))
stopifnot(identical(rec_unsupported$status, "unsupported"), identical(rec_unsupported$rule_ids, "U01"))
recommendation_html <- htmltools::renderTags(survival_design_recommendation_panel(rec_group, "en"))$html
stopifnot(grepl("G01", recommendation_html, fixed = TRUE), grepl("Open recommended analysis", recommendation_html, fixed = TRUE))
recommendation_ko_html <- htmltools::renderTags(survival_design_recommendation_panel(rec_group, "ko"))$html
stopifnot(
  grepl("Kaplan–Meier 생존곡선", recommendation_ko_html, fixed = TRUE),
  grepl("추천 이유", recommendation_ko_html, fixed = TRUE),
  grepl("주요 결과", recommendation_ko_html, fixed = TRUE),
  grepl("대안 선택 기준", recommendation_ko_html, fixed = TRUE),
  grepl("집단별 Kaplan–Meier 생존곡선과 위험대상자 수", recommendation_ko_html, fixed = TRUE),
  !grepl("Kaplan-Meier, log-rank test, and RMST", recommendation_ko_html, fixed = TRUE)
)

blocked_recommendation <- list(
  status = "blocked",
  primary = "Complete the survival data contract",
  rule_ids = "U05",
  confirmations = character(0),
  blocked_by = c("missing_time_origin", "event_map_not_confirmed"),
  warnings = "Start-stop and time-dependent covariates require a later survival module.",
  alternatives = character(0),
  preflight = list(
    counts = list(source_rows = 12L, analysis_rows = 0L, events = 0L),
    issues = data.frame(
      code = c("missing_time_origin", "event_map_not_confirmed"),
      message = c("Specify the time origin.", "Confirm the meaning of every observed event code."),
      stringsAsFactors = FALSE
    )
  )
)
blocked_ko_html <- htmltools::renderTags(survival_design_recommendation_panel(blocked_recommendation, "ko"))$html
stopifnot(
  grepl("실행 전 확인 필요", blocked_ko_html, fixed = TRUE),
  grepl("시간 원점을 입력하세요", blocked_ko_html, fixed = TRUE),
  grepl("입력 확인 결과", blocked_ko_html, fixed = TRUE),
  !grepl("missing_time_origin", blocked_ko_html, fixed = TRUE),
  !grepl("규칙 ID", blocked_ko_html, fixed = TRUE),
  !grepl("Complete the survival data contract", blocked_ko_html, fixed = TRUE),
  !grepl("Specify the time origin", blocked_ko_html, fixed = TRUE)
)

contract_setup_html <- htmltools::renderTags(survival_contract_setup_panel(c("time", "status", "group"), language = "ko"))$html
stopifnot(
  grepl("survival-contract-grid-fragment", contract_setup_html, fixed = TRUE),
  grepl("2. 시간 정의", contract_setup_html, fixed = TRUE),
  grepl("3. 사건·분석 변수", contract_setup_html, fixed = TRUE)
)

message("Checking survival variable-scope fallback...")
scope_data <- data.frame(time = 1:3, status = c(0, 1, 0), age = c(40, 50, 60))
scope_meta <- data.frame(name = c("fallback_time", "fallback_event"), stringsAsFactors = FALSE)
stopifnot(identical(survival_available_variable_names(character(0), scope_data, NULL), names(scope_data)))
stopifnot(identical(survival_available_variable_names(c("age", "missing"), scope_data, NULL), "age"))
stopifnot(identical(survival_available_variable_names(character(0), NULL, scope_meta), scope_meta$name))

message("Checking survival data-contract builder and metadata validation...")
contract <- survival_contract_settings(list(objective = "group_comparison", data_shape = "entry_exit", time_origin = "Enrollment", time_unit = "day", entry = "entry", time = "time", event = "status", censored_value = "0", event_value = "1"))
stopifnot(identical(contract$roles$entry, "entry"), identical(contract$time_unit, "day"))
contract_data <- data.frame(entry = c(0, 2), time = c(5, 7), status = c(1, 0))
contract_check <- survival_contract_preflight(contract_data, contract)
stopifnot(isTRUE(contract_check$ok), contract_check$counts$analysis_rows == 2L)
contract$time_origin <- ""
contract_check_missing <- survival_contract_preflight(contract_data, contract)
stopifnot(!contract_check_missing$ok, "missing_time_origin" %in% contract_check_missing$issues$code)

message("Checking explicit observed-value event-map confirmation...")
explicit_map <- data.frame(raw_value = c("0", "1", "2"), role = c("censored", "event_of_interest", "competing_event"), label = c("Censored", "Target", "Other cause"), stringsAsFactors = FALSE)
mapped_contract <- survival_contract_settings(list(objective = "competing", data_shape = "single_record", time_origin = "Diagnosis", time_unit = "month", time = "time", event = "status", event_map = explicit_map, event_map_confirmed = FALSE))
mapped_event_data <- data.frame(time = c(2, 4, 6, 8, 10), status = c(0, 1, 2, 1, 2))
unconfirmed_map_check <- survival_contract_preflight(mapped_event_data, mapped_contract)
stopifnot(!unconfirmed_map_check$ok, "event_map_not_confirmed" %in% unconfirmed_map_check$issues$code)
mapped_contract$event_map_confirmed <- TRUE
confirmed_map_check <- survival_contract_preflight(mapped_event_data, mapped_contract)
stopifnot(isTRUE(confirmed_map_check$ok), identical(confirmed_map_check$transformations$event_map$role, explicit_map$role))
multiple_interest_contract <- mapped_contract
multiple_interest_contract$event_map$role[[3]] <- "event_of_interest"
multiple_interest_check <- survival_contract_preflight(mapped_event_data, multiple_interest_contract)
stopifnot(!multiple_interest_check$ok, "event_of_interest_count" %in% multiple_interest_check$issues$code)
map_html <- htmltools::renderTags(survival_event_map_panel(c(0, 1, 2), "en"))$html
stopifnot(grepl("Observed event-code mapping", map_html, fixed = TRUE), grepl("survival_event_role_3", map_html, fixed = TRUE))
stopifnot(!grepl('data-for="survival_event_role_3"', map_html, fixed = TRUE))

message("Checking explicit event-map preservation and competing-risk routing...")
mapped_exec <- data.frame(time = c(2, 4, 6, 8, 10), status = c(0, 1, 2, 9, 1), x = c(0, 1, 0, 1, 0))
exec_map <- data.frame(raw_value = c("0", "1", "2", "9"), role = c("censored", "event_of_interest", "competing_event", "exclude"), label = c("Censored", "Target", "Other cause", "Protocol exclusion"), stringsAsFactors = FALSE)
mapped_standard_settings <- survival_legacy_settings("time", "status", "1")
mapped_standard_settings$legacy <- FALSE
mapped_standard_settings$event_map <- exec_map
mapped_standard_check <- survival_preflight(mapped_exec, mapped_standard_settings)
stopifnot(
  !isTRUE(mapped_standard_check$ok),
  "competing_event_requires_competing_risk" %in% mapped_standard_check$issues$code,
  grepl("competing_event_requires_competing_risk", mapped_standard_check$row_audit$exclusion_reasons[[3]], fixed = TRUE),
  grepl("excluded_event_code", mapped_standard_check$row_audit$exclusion_reasons[[4]], fixed = TRUE)
)
mapped_km_error <- tryCatch(prepare_km_analysis_result(mapped_exec, "time", "status", event_map = exec_map), error = identity)
mapped_cox_error <- tryCatch(suppressWarnings(prepare_cox_analysis_result(mapped_exec, "time", "status", "x", event_map = exec_map)), error = identity)
stopifnot(
  inherits(mapped_km_error, "error"),
  inherits(mapped_cox_error, "error"),
  grepl("Competing-event codes", conditionMessage(mapped_km_error), fixed = TRUE),
  grepl("Competing-event codes", conditionMessage(mapped_cox_error), fixed = TRUE)
)
mapped_cr <- prepare_competing_risk_result(mapped_exec, "time", "status", event_map = exec_map)
stopifnot(mapped_cr$n == 4L, mapped_cr$preflight$counts$competing_events == 1L)
mapped_design_contract <- survival_contract_settings(list(objective = "group_comparison", event_structure = "competing", data_shape = "single_record", time_origin = "Diagnosis", time_unit = "month", time = "time", event = "status", event_map = exec_map, event_map_confirmed = TRUE))
mapped_design_check <- survival_contract_preflight(mapped_exec, mapped_design_contract)
stopifnot(isTRUE(mapped_design_check$ok), mapped_design_check$counts$competing_events == 1L)
unsupported_delayed_cr <- survival_recommend(list(objective = "competing", data_shape = "entry_exit", event_structure = "competing", competing_estimand = "both"))
stopifnot(identical(unsupported_delayed_cr$status, "unsupported"), identical(unsupported_delayed_cr$rule_ids, "U06"))

message("Checking contract transfer into recommended analyses...")
transfer_contract <- survival_contract_settings(list(objective = "association", data_shape = "single_record", time_origin = "Surgery", time_unit = "day", time = "time", event = "status", group = "arm", covariates = c("age", "stage"), censored_value = "0", event_value = "1", competing_values = "2, 3"))
transfer_cox <- survival_contract_transfer(transfer_contract, survival_recommend(list(objective = "association", data_shape = "single_record", event_structure = "single")))
stopifnot(identical(transfer_cox$target_tab, "analysis_survival_cox"), identical(transfer_cox$data_shape, "single_record"), identical(transfer_cox$time, "time"), identical(transfer_cox$event, "status"), identical(transfer_cox$covariates, c("age", "stage")))
transfer_fg <- survival_contract_transfer(transfer_contract, survival_recommend(list(objective = "association", data_shape = "single_record", event_structure = "competing", competing_estimand = "cumulative_incidence")))
stopifnot(identical(transfer_fg$target_tab, "analysis_survival_competing"), identical(transfer_fg$regression, "fine_gray"), identical(transfer_fg$competing_values, "2, 3"))
transfer_both <- survival_contract_transfer(transfer_contract, rec_competing)
competing_setup_html <- htmltools::renderTags(survival_competing_setup_panel(
  c("time", "status", "arm", "age", "stage", "site"),
  values = list(
    time = transfer_both$time,
    event = transfer_both$event,
    group = transfer_both$group,
    censored_value = transfer_both$censored_value,
    interest_value = transfer_both$event_value,
    competing_values = transfer_both$competing_values,
    regression = transfer_both$regression,
    covariates = transfer_both$covariates,
    censoring_group = "site"
  ),
  language = "en"
))$html
stopifnot(grepl('value="time" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="status" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="arm" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="both" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="age" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="stage" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="site" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl("Censoring-distribution strata", competing_setup_html, fixed = TRUE))
stopifnot(
  grepl("ttest-anova-setup-grid", competing_setup_html, fixed = TRUE),
  grepl("survival-competing-event-panel", competing_setup_html, fixed = TRUE),
  !grepl("ttest-anova-target-column survival-competing-event-panel", competing_setup_html, fixed = TRUE)
)

message("Checking delayed-entry Kaplan-Meier and Cox engines...")
delayed <- data.frame(entry = c(0, 2, 0, 4, 1, 3), exit = c(5, 8, 7, 10, 9, 12), status = c(1, 0, 1, 0, 1, 0), arm = factor(c("A", "A", "B", "B", "A", "B")), age = c(50, 50, 60, 60, 55, 55))
delayed_km <- prepare_km_analysis_result(delayed, "exit", "status", "arm", entry = "entry", time_origin = "Enrollment", time_unit = "day")
stopifnot(identical(delayed_km$entry, "entry"), identical(delayed_km$fit$type, "counting"), identical(delayed_km$time_origin, "Enrollment"), identical(delayed_km$time_unit, "day"))
stopifnot(grepl("Enrollment", survival_method_sentence(delayed_km, "en"), fixed = TRUE), grepl("지연 진입", survival_method_sentence(delayed_km, "ko"), fixed = TRUE))
delayed_cox <- prepare_cox_analysis_result(delayed, "exit", "status", "age", entry = "entry", time_origin = "Enrollment", time_unit = "day")
stopifnot(identical(delayed_cox$entry, "entry"), identical(attr(delayed_cox$fit$y, "type"), "counting"))
delayed_life_error <- try(prepare_km_analysis_result(delayed, "exit", "status", "arm", entry = "entry", analysis_method = "life_table"), silent = TRUE)
stopifnot(inherits(delayed_life_error, "try-error"), grepl("does not support delayed entry", as.character(delayed_life_error), fixed = TRUE))

message("Checking start-stop time-dependent Cox engine...")
td <- data.frame(id = rep(1:6, each = 2), start = rep(c(0, 5), 6), stop = rep(c(5, 10), 6), status = c(0,1, 0,0, 0,1, 0,0, 0,1, 0,0), exposure = c(0,1, 1,1, 0,0, 1,0, 0,1, 1,0))
td_rec <- survival_recommend(list(objective = "association", data_shape = "start_stop", event_structure = "single", time_dependent = TRUE))
stopifnot(identical(td_rec$status, "ready"), identical(td_rec$rule_ids, c("S03", "A06")))
td_contract <- survival_contract_settings(list(objective = "association", data_shape = "start_stop", time_origin = "Enrollment", time_unit = "day", start = "start", stop = "stop", subject_id = "id", event = "status", covariates = "exposure", censored_value = "0", event_value = "1"))
td_transfer <- survival_contract_transfer(td_contract, td_rec)
stopifnot(identical(td_transfer$data_shape, "start_stop"), identical(td_transfer$start, "start"), identical(td_transfer$stop, "stop"), identical(td_transfer$subject_id, "id"), identical(td_transfer$target_tab, "analysis_survival_cox"))
td_cox <- prepare_cox_analysis_result(td, "", "status", "exposure", start = "start", stop = "stop", subject_id = "id")
stopifnot(identical(td_cox$start, "start"), identical(td_cox$stop, "stop"), identical(td_cox$subject_id, "id"), identical(attr(td_cox$fit$y, "type"), "counting"), !is.null(td_cox$fit$naive.var))
td_summary <- summary(td_cox$fit)$coefficients
td_robust_se <- unname(td_summary[, "robust se"])
td_model_se <- unname(td_summary[, "se(coef)"])
stopifnot(
  identical(td_cox$variance, "Subject-cluster robust"),
  isTRUE(all.equal(td_cox$coef_table$SE, td_robust_se, tolerance = 1e-12)),
  !isTRUE(all.equal(td_cox$coef_table$SE, td_model_se, tolerance = 1e-12)),
  isTRUE(all.equal(td_cox$coef_table$z, td_cox$coef_table$B / td_robust_se, tolerance = 1e-12)),
  isTRUE(all.equal(td_cox$coef_table$LLCI, exp(td_cox$coef_table$B - stats::qnorm(.975) * td_robust_se), tolerance = 1e-12)),
  isTRUE(all.equal(td_cox$coef_table$ULCI, exp(td_cox$coef_table$B + stats::qnorm(.975) * td_robust_se), tolerance = 1e-12))
)
td_adjusted_error <- try(prepare_cox_analysis_result(td, "", "status", "exposure", start = "start", stop = "stop", subject_id = "id", adjusted_group = "exposure"), silent = TRUE)
stopifnot(inherits(td_adjusted_error, "try-error"), grepl("not available for start-stop", as.character(td_adjusted_error), fixed = TRUE))

message("Checking legacy single-record survival preflight...")
single <- data.frame(
  id = 1:6,
  time = c(5, 8, 12, 15, NA, 20),
  status = c(1, 0, 1, 0, 1, 0),
  group = c("A", "A", "B", "B", "A", "B"),
  age = c(50, 60, 55, NA, 70, 65)
)
legacy <- survival_legacy_settings("time", "status", "1", "group", "age")
legacy_check <- survival_preflight(single, legacy)
stopifnot(isTRUE(legacy_check$ok))
stopifnot(legacy_check$counts$source_rows == 6L)
stopifnot(legacy_check$counts$analysis_rows == 4L)
stopifnot(legacy_check$counts$events == 2L)
stopifnot(identical(which(!legacy_check$row_audit$included), c(4L, 5L)))
stopifnot(grepl("missing_covariate", legacy_check$row_audit$exclusion_reasons[[4]], fixed = TRUE))
stopifnot(grepl("missing_time", legacy_check$row_audit$exclusion_reasons[[5]], fixed = TRUE))
stopifnot("status__raw" %in% names(legacy_check$analysis_data))
stopifnot(is.logical(legacy_check$analysis_data$status))
stopifnot(identical(single$status, c(1, 0, 1, 0, 1, 0)))

message("Checking safe duration coercion and invalid time encodings...")
factor_time <- data.frame(time = factor(c("10", "20", "30")), status = c(1, 0, 1))
factor_time_check <- survival_preflight(factor_time, survival_legacy_settings("time", "status", "1"))
stopifnot(isTRUE(factor_time_check$ok), identical(factor_time_check$analysis_data$time, c(10, 20, 30)))
character_time <- data.frame(time = c("1.5", "2.5", "3.5"), status = c(1, 0, 1))
character_time_check <- survival_preflight(character_time, survival_legacy_settings("time", "status", "1"))
stopifnot(isTRUE(character_time_check$ok), identical(character_time_check$analysis_data$time, c(1.5, 2.5, 3.5)))
invalid_time <- data.frame(time = c("10", "not-a-duration", "30"), status = c(1, 0, 1))
invalid_time_check <- survival_preflight(invalid_time, survival_legacy_settings("time", "status", "1"))
stopifnot(
  !isTRUE(invalid_time_check$ok),
  "invalid_time_encoding" %in% invalid_time_check$issues$code,
  grepl("invalid_time_encoding", invalid_time_check$row_audit$exclusion_reasons[[2]], fixed = TRUE)
)
date_time <- data.frame(time = as.Date("2026-01-01") + 0:2, status = c(1, 0, 1))
date_time_check <- survival_preflight(date_time, survival_legacy_settings("time", "status", "1"))
stopifnot(!isTRUE(date_time_check$ok), "invalid_time_encoding" %in% date_time_check$issues$code)

message("Checking explicit competing-event mapping...")
competing <- data.frame(time = c(2, 4, 6, 8, 10), status = c(0, 1, 2, 1, 2))
competing_settings <- survival_legacy_settings("time", "status", "1")
competing_settings$legacy <- FALSE
competing_settings$objective <- "competing"
competing_settings$event_structure <- "competing"
competing_settings$event_map <- data.frame(
  raw_value = c("0", "1", "2"),
  role = c("censored", "event_of_interest", "competing_event"),
  label = c("Censored", "Target", "Competing"),
  stringsAsFactors = FALSE
)
competing_check <- survival_preflight(competing, competing_settings)
stopifnot(isTRUE(competing_check$ok))
stopifnot(competing_check$counts$events == 2L)
stopifnot(competing_check$counts$competing_events == 2L)
stopifnot(competing_check$counts$censored == 1L)

message("Checking potential competing-event warning for legacy setup...")
legacy_competing <- survival_preflight(competing, survival_legacy_settings("time", "status", "1"))
stopifnot(isTRUE(legacy_competing$ok))
stopifnot("potential_competing_events" %in% legacy_competing$issues$code)

message("Checking invalid event map block...")
bad_map_settings <- competing_settings
bad_map_settings$event_map$role[bad_map_settings$event_map$raw_value == "2"] <- "unknown"
bad_map <- survival_preflight(competing, bad_map_settings)
stopifnot(!isTRUE(bad_map$ok))
stopifnot("invalid_event_map" %in% bad_map$issues$code)

message("Checking unsupported multi-state event-code block...")
other_state_data <- data.frame(time = c(2, 4, 6), status = c(0, 1, 2))
other_state_map <- data.frame(
  raw_value = c("0", "1", "2"),
  role = c("censored", "event_of_interest", "other_state"),
  label = c("Censored", "Target event", "Intermediate state"),
  stringsAsFactors = FALSE
)
other_state_settings <- survival_legacy_settings("time", "status", "1")
other_state_settings$legacy <- FALSE
other_state_settings$event_map <- other_state_map
other_state_check <- survival_preflight(other_state_data, other_state_settings)
stopifnot(
  !isTRUE(other_state_check$ok),
  "unsupported_other_state" %in% other_state_check$issues$code,
  grepl("unsupported_other_state", other_state_check$row_audit$exclusion_reasons[[3]], fixed = TRUE)
)

message("Checking entry-exit time-order block...")
entry_data <- data.frame(entry = c(0, 5, 9), exit = c(4, 5, 12), status = c(1, 0, 1))
entry_settings <- survival_legacy_settings("exit", "status", "1")
entry_settings$legacy <- FALSE
entry_settings$data_shape <- "entry_exit"
entry_settings$roles$entry <- "entry"
entry_check <- survival_preflight(entry_data, entry_settings)
stopifnot(!isTRUE(entry_check$ok))
stopifnot("entry_not_before_exit" %in% entry_check$issues$code)
stopifnot(grepl("invalid_time_order", entry_check$row_audit$exclusion_reasons[[2]], fixed = TRUE))

message("Checking start-stop role and interval validation...")
start_stop <- data.frame(id = c(1, 1, 2), start = c(0, 5, 4), stop = c(5, 10, 3), status = c(0, 1, 1))
start_settings <- survival_legacy_settings("stop", "status", "1")
start_settings$legacy <- FALSE
start_settings$data_shape <- "start_stop"
start_settings$roles$time <- NULL
start_settings$roles$subject_id <- "id"
start_settings$roles$start <- "start"
start_settings$roles$stop <- "stop"
start_check <- survival_preflight(start_stop, start_settings)
stopifnot(!isTRUE(start_check$ok))
stopifnot("start_not_before_stop" %in% start_check$issues$code)

missing_id_settings <- start_settings
missing_id_settings$roles$subject_id <- NULL
missing_id_check <- survival_preflight(start_stop, missing_id_settings)
stopifnot(!isTRUE(missing_id_check$ok))
stopifnot("missing_subject_id" %in% missing_id_check$issues$code)

message("Checking subject-level start-stop integrity...")
integrity_settings <- start_settings
overlap_data <- data.frame(id = c(1, 1, 2), start = c(0, 4, 0), stop = c(5, 8, 6), status = c(0, 0, 1))
overlap_check <- survival_preflight(overlap_data, integrity_settings)
stopifnot(!overlap_check$ok, "overlapping_intervals" %in% overlap_check$issues$code, grepl("overlapping_interval", overlap_check$row_audit$exclusion_reasons[[2]], fixed = TRUE))
post_event_data <- data.frame(id = c(1, 1, 2), start = c(0, 5, 0), stop = c(5, 9, 7), status = c(1, 0, 1))
post_event_check <- survival_preflight(post_event_data, integrity_settings)
stopifnot(!post_event_check$ok, "interval_after_event" %in% post_event_check$issues$code)
multiple_event_data <- data.frame(id = c(1, 1, 2), start = c(0, 5, 0), stop = c(5, 9, 7), status = c(1, 1, 0))
multiple_event_check <- survival_preflight(multiple_event_data, integrity_settings)
stopifnot(!multiple_event_check$ok, "multiple_subject_events" %in% multiple_event_check$issues$code)
unordered_data <- data.frame(id = c(1, 1, 2), start = c(5, 0, 0), stop = c(9, 5, 7), status = c(0, 0, 1))
unordered_check <- survival_preflight(unordered_data, integrity_settings)
stopifnot(isTRUE(unordered_check$ok), "intervals_not_source_order" %in% unordered_check$issues$code, identical(unordered_check$analysis_data$start, c(0, 5, 0)))
bad_identity_data <- data.frame(id = c(1, NA), start = c(0, -1), stop = c(5, 4), status = c(1, 0))
bad_identity_check <- survival_preflight(bad_identity_data, integrity_settings)
stopifnot(!bad_identity_check$ok, all(c("missing_subject_id_value", "invalid_interval_time") %in% bad_identity_check$issues$code))

message("Checking legacy wrapper compatibility...")
legacy_data <- survival_analysis_data(single, "time", "status", "1", "group")
stopifnot(nrow(legacy_data) == 5L)
stopifnot(is.factor(legacy_data$group))
stopifnot(sum(legacy_data$status) == 2L)

message("Checking Kaplan-Meier and Cox compatibility through preflight...")
km_fit <- prepare_km_single_analysis_result(
  single,
  time = "time",
  event = "status",
  group = "group",
  event_value = "1",
  rate_times = "5,10,15"
)
stopifnot(identical(km_fit$type, "km"))
stopifnot(km_fit$n == 5L)
stopifnot(km_fit$events == 2L)
stopifnot(inherits(km_fit$fit, "survfit"))
stopifnot(is.list(km_fit$preflight))
stopifnot(km_fit$preflight$counts$source_rows == 6L)
stopifnot(km_fit$preflight$counts$analysis_rows == 5L)
data_flow <- survival_km_data_flow_table(km_fit)
stopifnot(nrow(data_flow) == 1L)
stopifnot(data_flow$Excluded[[1]] == 1L)
exclusions <- survival_km_exclusion_table(km_fit)
stopifnot(nrow(exclusions) == 1L)
stopifnot(exclusions$`Exclusion reason`[[1]] == "missing_time")
risk_table <- survival_km_number_at_risk_table(km_fit)
stopifnot(nrow(risk_table) > 0L)
stopifnot(all(c("Time", "Number at risk") %in% names(risk_table)))
stopifnot(all(risk_table$`Number at risk` >= 0L))
km_html <- htmltools::renderTags(survival_km_results_panel(km_fit, plot_output_ids = list(character(0)), language = "en"))$html
stopifnot(grepl("Event-code mapping", km_html, fixed = TRUE))
report_bundle <- survival_reporting_bundle(km_fit, "en")
stopifnot(is.data.frame(report_bundle$event_map), nrow(report_bundle$event_map) >= 2L, is.data.frame(report_bundle$data_flow))
audit_dir <- tempfile("survival-audit-")
dir.create(audit_dir)
audit_files <- save_survival_reporting_files(km_fit, audit_dir, "en")
stopifnot(any(grepl("Survival_methods.txt", audit_files, fixed = TRUE)), any(grepl("Event_code_mapping.csv", audit_files, fixed = TRUE)), all(file.exists(audit_files)))
stopifnot(any(grepl("Reporting_checklist.csv", audit_files, fixed = TRUE)), any(grepl("Interpretation_guide.csv", audit_files, fixed = TRUE)))
stopifnot(any(grepl("Analysis_stability_review.csv", audit_files, fixed = TRUE)))
stopifnot(any(grepl("Censoring_followup_diagnostics.csv", audit_files, fixed = TRUE)))
km_checklist <- survival_reporting_checklist(km_fit, "en")
stopifnot(all(c("Item", "Status", "Evidence") %in% names(km_checklist)), "Review" %in% km_checklist$Status)
km_guide <- survival_interpretation_guide(km_fit, "en")
stopifnot(identical(km_guide$Topic, c("Survival probability", "Log-rank", "RMST")))
km_stability <- survival_stability_review(km_fit, "en")
stopifnot(all(c("Level", "Code", "Evidence", "Guidance") %in% names(km_stability)))
km_followup <- survival_followup_diagnostics(km_fit)
stopifnot(nrow(km_followup) == 1L, all(c("Censoring proportion", "Median potential follow-up (reverse KM)", "At risk at 90th percentile time") %in% names(km_followup)))
delayed_followup <- survival_followup_diagnostics(delayed_km)
stopifnot(nrow(delayed_followup) == 1L, delayed_followup$`Analysis subjects/records`[[1]] == nrow(delayed))
high_censor_data <- data.frame(time = 1:10, status = c(1, rep(0, 9)))
high_censor_fit <- prepare_km_analysis_result(high_censor_data, "time", "status")
high_censor_codes <- survival_stability_review(high_censor_fit, "en")$Code
stopifnot("high_censoring_proportion" %in% high_censor_codes, "sparse_tail_risk_set" %in% high_censor_codes)
stopifnot(inherits(survival_km_risk_table_plot(high_censor_fit), "ggplot"), nzchar(survival_plot_tail_caption(high_censor_fit)))
stopifnot(grepl("Data inclusion audit", km_html, fixed = TRUE))
stopifnot(grepl("Excluded rows by reason", km_html, fixed = TRUE))
stopifnot(grepl("Number at risk", km_html, fixed = TRUE))

km_competing_legacy <- prepare_km_single_analysis_result(competing, "time", "status", event_value = "1")
review_messages <- survival_km_issue_table(km_competing_legacy)
stopifnot("potential_competing_events" %in% review_messages$Code)

message("Checking log-rank family and RMST output...")
rmst_data <- data.frame(
  time = c(5, 10, 15, 20, 5, 10, 15, 20),
  status = c(0, 0, 0, 0, 1, 1, 0, 0),
  group = rep(c("A", "B"), each = 4)
)
rmst_fit <- prepare_km_single_analysis_result(
  rmst_data,
  time = "time",
  event = "status",
  group = "group",
  event_value = "1",
  rate_times = "5,10,12",
  rmst_tau = "12"
)
stopifnot(is.list(rmst_fit$rmst))
stopifnot(rmst_fit$rmst$tau == 12)
stopifnot(nrow(rmst_fit$rmst$estimates) == 2L)
stopifnot(abs(rmst_fit$rmst$estimates$RMST[rmst_fit$rmst$estimates$Strata == "group=A"] - 12) < 1e-10)
stopifnot(nrow(rmst_fit$rmst$contrasts) == 1L)
stopifnot(rmst_fit$rmst$contrasts$Difference[[1]] < 0)
stopifnot(rmst_fit$rmst$contrasts$p[[1]] == rmst_fit$rmst$contrasts$p_adjusted[[1]])
stopifnot(nrow(survival_km_rmst_estimate_table(rmst_fit)) == 2L)
stopifnot(nrow(survival_km_rmst_contrast_table(rmst_fit)) == 1L)
rmst_html <- htmltools::renderTags(survival_km_results_panel(rmst_fit, plot_output_ids = list(character(0)), language = "en"))$html
stopifnot(grepl("Restricted mean survival time (RMST)", rmst_html, fixed = TRUE))
stopifnot(grepl("RMST contrasts", rmst_html, fixed = TRUE))

tau_error <- tryCatch({
  prepare_km_single_analysis_result(rmst_data, "time", "status", "group", rmst_tau = "25")
  NULL
}, error = function(e) e)
stopifnot(inherits(tau_error, "error"))
stopifnot(grepl("observed follow-up range", conditionMessage(tau_error), fixed = TRUE))

for (method in c("logrank", "breslow", "tarone_ware")) {
  method_fit <- prepare_km_single_analysis_result(rmst_data, "time", "status", "group", test_method = method)
  stopifnot(is.list(method_fit$logrank))
  stopifnot(is.finite(method_fit$logrank$chisq))
  stopifnot(method_fit$logrank$df == 1L)
  stopifnot(is.finite(method_fit$logrank$p))
}

message("Checking Kaplan-Meier curve-crossing diagnostics...")
crossing_data <- data.frame(
  time = c(1, 2, 10, 10, 5, 6, 7, 10),
  status = c(1, 1, 0, 0, 1, 1, 1, 0),
  group = rep(c("A", "B"), each = 4)
)
crossing_fit <- prepare_km_single_analysis_result(crossing_data, "time", "status", "group")
stopifnot(nrow(crossing_fit$crossing_table) == 1L)
stopifnot(crossing_fit$crossing_table$Crossings[[1]] == 1L)
stopifnot(crossing_fit$crossing_table$`First crossing time`[[1]] == 7)
stopifnot(crossing_fit$crossing_table$`Common follow-up end`[[1]] == 10)
stopifnot(isTRUE(crossing_fit$crossing_table$`Review signal`[[1]]))
stopifnot("crossing_survival_curves" %in% survival_stability_review(crossing_fit, "en")$Code)
crossing_html <- htmltools::renderTags(survival_km_results_panel(crossing_fit, plot_output_ids = list(character(0)), language = "en"))$html
stopifnot(grepl("Survival-curve crossing screen", crossing_html, fixed = TRUE))
stopifnot(grepl("Do not select a weighted test post hoc", crossing_html, fixed = TRUE))
crossing_audit_dir <- tempfile("survival-crossing-audit-")
dir.create(crossing_audit_dir)
crossing_audit_files <- save_survival_reporting_files(crossing_fit, crossing_audit_dir, "en")
stopifnot(any(grepl("KM_curve_crossing_screen.csv", crossing_audit_files, fixed = TRUE)))
crossing_export <- utils::read.csv(file.path(crossing_audit_dir, "KM_curve_crossing_screen.csv"), check.names = FALSE)
stopifnot(crossing_export$Crossings[[1]] == 1L, crossing_export$`First crossing time`[[1]] == 7)
stopifnot(rmst_fit$crossing_table$Crossings[[1]] == 0L, !isTRUE(rmst_fit$crossing_table$`Review signal`[[1]]))
unequal_followup_data <- data.frame(
  time = c(1, 2, 1, 3, 4),
  status = c(1, 0, 1, 1, 0),
  group = c("A", "A", "B", "B", "B")
)
unequal_followup_fit <- prepare_km_single_analysis_result(unequal_followup_data, "time", "status", "group")
stopifnot(unequal_followup_fit$crossing_table$`Common follow-up end`[[1]] == 2)
stopifnot(unequal_followup_fit$crossing_table$Crossings[[1]] == 0L)

set.seed(20260815)
cox_data <- data.frame(
  time = stats::rexp(60, rate = 0.08),
  status = stats::rbinom(60, size = 1, prob = 0.55),
  age = stats::rnorm(60, mean = 60, sd = 8),
  treatment = rep(c("A", "B"), 30)
)
cox_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = "age",
  event_value = "1"
)
stopifnot(identical(cox_fit$type, "cox"))
stopifnot(cox_fit$n == 60L)
stopifnot(cox_fit$events == sum(cox_data$status))
stopifnot(inherits(cox_fit$fit, "coxph"))
stopifnot(is.list(cox_fit$preflight))
stopifnot(identical(cox_fit$variance, "Model-based"))
stopifnot(isTRUE(all.equal(cox_fit$coef_table$SE, unname(summary(cox_fit$fit)$coefficients[, "se(coef)"]), tolerance = 1e-12)))
stopifnot(cox_fit$parameter_count == 1L)
stopifnot(is.finite(cox_fit$events_per_parameter))
stopifnot(nrow(cox_fit$model_tests) == 3L)
stopifnot(identical(cox_fit$model_tests$Test, c("Likelihood-ratio", "Wald", "Score")))
stopifnot(all(is.finite(cox_fit$model_tests$Statistic)))
stopifnot(nrow(cox_fit$residual_table) == 2L)
stopifnot(nrow(cox_fit$influence_table) == 1L)
stopifnot(
  nrow(cox_fit$functional_form_data) == cox_fit$n,
  identical(unique(cox_fit$functional_form_data$Covariate), "age"),
  nrow(cox_fit$collinearity_table) == 1L,
  isTRUE(all.equal(cox_fit$collinearity_table$VIF[[1]], 1, tolerance = 1e-12)),
  all(c("Maximum absolute DFBETAS", "Screening threshold", "Review signal") %in% names(cox_fit$influence_table))
)
exact_collinearity <- survival_cox_collinearity(cbind(x1 = 1:10, x2 = 2 * (1:10)))
stopifnot(all(!is.finite(exact_collinearity$table$VIF)), exact_collinearity$condition_number >= 30)
stopifnot(nrow(survival_cox_model_test_table(cox_fit)) == 3L)
stopifnot(nrow(survival_ph_review_table(cox_fit, "en")) > 0L)
stopifnot(nrow(survival_cox_residual_table(cox_fit)) == 2L)
stopifnot(nrow(survival_cox_influence_table(cox_fit)) == 1L)
stopifnot(nrow(survival_cox_collinearity_table(cox_fit)) == 1L)
stopifnot(inherits(survival_cox_functional_form_ggplot(cox_fit), "ggplot"))
ph_plot_file <- tempfile("cox-schoenfeld-", fileext = ".png")
grDevices::png(ph_plot_file, width = 900, height = 700, res = 120)
survival_cox_ph_plot(cox_fit)
grDevices::dev.off()
stopifnot(file.exists(ph_plot_file), file.info(ph_plot_file)$size > 0)
cox_html <- htmltools::renderTags(survival_cox_results_panel(cox_fit, language = "en"))$html
stopifnot(grepl("Reporting checklist and interpretation guide", cox_html, fixed = TRUE), grepl("not automatically a causal", cox_html, fixed = TRUE))
stopifnot(grepl("Analysis stability review", cox_html, fixed = TRUE), "low_events_per_parameter" %in% survival_stability_review(delayed_cox, "en")$Code)
stopifnot(grepl("3. Supplementary statistics and diagnostics", cox_html, fixed = TRUE))
stopifnot(grepl("Overall model tests", cox_html, fixed = TRUE))
stopifnot(grepl("proportional-hazards assumption", cox_html, fixed = TRUE))
stopifnot(grepl("survival_cox_ph_plot", cox_html, fixed = TRUE))
stopifnot(grepl("Design-matrix collinearity review", cox_html, fixed = TRUE))
stopifnot(grepl("survival_cox_functional_plot", cox_html, fixed = TRUE))
stopifnot(grepl("4. Residual distribution review", cox_html, fixed = TRUE))
stopifnot(grepl("5. Influence review", cox_html, fixed = TRUE))
stopifnot(grepl("not automatic pass/fail criteria", cox_html, fixed = TRUE))
stopifnot(grepl("survival-cox-summary-row", cox_html, fixed = TRUE))
stopifnot(!grepl("survival-cox-model-subsection", cox_html, fixed = TRUE))
cox_audit_dir <- tempfile("cox-audit-")
dir.create(cox_audit_dir)
cox_audit_files <- save_survival_reporting_files(cox_fit, cox_audit_dir, "en")
stopifnot(all(c("Cox_PH_tests.csv", "Cox_functional_form_data.csv", "Cox_influence.csv", "Cox_collinearity.csv") %in% basename(cox_audit_files)))

message("Checking stratified Cox regression...")
cox_data$site <- factor(rep(c("Site 1", "Site 2", "Site 3"), length.out = nrow(cox_data)))
stratified_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = "age",
  event_value = "1",
  strata = "site"
)
reference_stratified_fit <- survival::coxph(
  survival::Surv(time, status) ~ age + strata(site),
  data = cox_data,
  ties = "efron",
  x = TRUE,
  y = TRUE,
  model = TRUE
)
stopifnot(identical(stratified_fit$strata, "site"))
stopifnot(nrow(stratified_fit$strata_table) == 3L)
stopifnot(sum(stratified_fit$strata_table$Records) == stratified_fit$n)
stopifnot(sum(stratified_fit$strata_table$Events) == stratified_fit$events)
stopifnot(isTRUE(all.equal(unname(stats::coef(stratified_fit$fit)), unname(stats::coef(reference_stratified_fit)), tolerance = 1e-12)))
stopifnot(!any(grepl("site", stratified_fit$coef_table$Term, fixed = TRUE)))
stopifnot(grepl("strata", paste(deparse(stratified_fit$formula), collapse = ""), fixed = TRUE))
stopifnot(nrow(survival_cox_strata_table(stratified_fit)) == 3L)
stopifnot(grepl("stratum-specific baseline hazards", survival_method_sentence(stratified_fit, "en"), fixed = TRUE))
stratified_html <- htmltools::renderTags(survival_cox_results_panel(stratified_fit, language = "en"))$html
stopifnot(grepl("Stratum event counts", stratified_html, fixed = TRUE))
stopifnot(grepl("has no estimated hazard ratio", stratified_html, fixed = TRUE))
stratified_audit_dir <- tempfile("stratified-cox-audit-")
dir.create(stratified_audit_dir)
stratified_audit_files <- save_survival_reporting_files(stratified_fit, stratified_audit_dir, "en")
stopifnot("Cox_strata_event_counts.csv" %in% basename(stratified_audit_files))

strata_review_probe <- stratified_fit
strata_review_probe$strata_table$Events[[1]] <- 0L
stopifnot("zero_event_stratum" %in% survival_stability_review(strata_review_probe, "en")$Code)

strata_overlap_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("age", "site"), strata = "site"),
  error = identity
)
stopifnot(inherits(strata_overlap_error, "error"), grepl("must not also be entered", conditionMessage(strata_overlap_error), fixed = TRUE))

single_stratum_data <- cox_data
single_stratum_data$site <- "Only site"
single_stratum_error <- tryCatch(
  prepare_cox_analysis_result(single_stratum_data, "time", "status", "age", strata = "site"),
  error = identity
)
stopifnot(inherits(single_stratum_error, "error"), grepl("at least two observed strata", conditionMessage(single_stratum_error), fixed = TRUE))

stratified_adjusted_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("treatment", "age"), adjusted_group = "treatment", adjusted_bootstrap_reps = 40L, strata = "site"),
  error = identity
)
stopifnot(inherits(stratified_adjusted_error, "error"), grepl("not available with stratified Cox", conditionMessage(stratified_adjusted_error), fixed = TRUE))

stratified_setup_code <- paste(deparse(body(survival_cox_setup_panel)), collapse = "\n")
stopifnot(grepl("survival_cox_strata", stratified_setup_code, fixed = TRUE))
stopifnot(grepl("Stratification variable", stratified_setup_code, fixed = TRUE))

message("Checking user-specified cluster-robust Cox regression...")
cox_data$center <- factor(rep(seq_len(20L), each = 3L))
clustered_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = "age",
  event_value = "1",
  cluster = "center"
)
reference_clustered_fit <- survival::coxph(
  survival::Surv(time, status) ~ age + cluster(center),
  data = cox_data,
  ties = "efron",
  x = TRUE,
  y = TRUE,
  model = TRUE
)
clustered_summary <- summary(reference_clustered_fit)$coefficients
stopifnot(identical(clustered_fit$cluster, "center"))
stopifnot(identical(clustered_fit$variance, "Cluster-robust (center)"))
stopifnot(!is.null(clustered_fit$fit$naive.var))
stopifnot(isTRUE(all.equal(unname(stats::coef(clustered_fit$fit)), unname(stats::coef(reference_clustered_fit)), tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(clustered_fit$coef_table$SE, unname(clustered_summary[, "robust se"]), tolerance = 1e-12)))
stopifnot(nrow(clustered_fit$cluster_summary) == 1L)
stopifnot(clustered_fit$cluster_summary$Clusters[[1]] == 20L)
stopifnot(clustered_fit$cluster_summary$`Minimum records per cluster`[[1]] == 3L)
stopifnot(clustered_fit$cluster_summary$`Maximum records per cluster`[[1]] == 3L)
stopifnot(nrow(survival_cox_cluster_summary_table(clustered_fit)) == 1L)
stopifnot("few_robust_variance_clusters" %in% survival_stability_review(clustered_fit, "en")$Code)
stopifnot(grepl("sandwich robust standard errors clustered by center", survival_method_sentence(clustered_fit, "en"), fixed = TRUE))
clustered_html <- htmltools::renderTags(survival_cox_results_panel(clustered_fit, language = "en"))$html
stopifnot(grepl("Robust-variance cluster summary", clustered_html, fixed = TRUE))
clustered_audit_dir <- tempfile("clustered-cox-audit-")
dir.create(clustered_audit_dir)
clustered_audit_files <- save_survival_reporting_files(clustered_fit, clustered_audit_dir, "en")
stopifnot("Cox_cluster_summary.csv" %in% basename(clustered_audit_files))

cluster_overlap_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("age", "center"), cluster = "center"),
  error = identity
)
stopifnot(inherits(cluster_overlap_error, "error"), grepl("must not also be entered", conditionMessage(cluster_overlap_error), fixed = TRUE))

single_cluster_data <- cox_data
single_cluster_data$center <- "Only cluster"
single_cluster_error <- tryCatch(
  prepare_cox_analysis_result(single_cluster_data, "time", "status", "age", cluster = "center"),
  error = identity
)
stopifnot(inherits(single_cluster_error, "error"), grepl("at least two observed clusters", conditionMessage(single_cluster_error), fixed = TRUE))

start_stop_cluster_error <- tryCatch(
  prepare_cox_analysis_result(td, "", "status", "exposure", start = "start", stop = "stop", subject_id = "id", cluster = "id"),
  error = identity
)
stopifnot(inherits(start_stop_cluster_error, "error"), grepl("multiway clustering is not implemented", conditionMessage(start_stop_cluster_error), fixed = TRUE))

clustered_adjusted_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("treatment", "age"), adjusted_group = "treatment", adjusted_bootstrap_reps = 40L, cluster = "center"),
  error = identity
)
stopifnot(inherits(clustered_adjusted_error, "error"), grepl("requires cluster-level resampling", conditionMessage(clustered_adjusted_error), fixed = TRUE))

stopifnot(grepl("survival_cox_cluster", stratified_setup_code, fixed = TRUE))
stopifnot(grepl("Cluster ID variable", stratified_setup_code, fixed = TRUE))

message("Checking natural-cubic-spline Cox functional form...")
spline_variable_info <- data.frame(
  name = c("age", "treatment"),
  measurement = c("continuous", "category"),
  stringsAsFactors = FALSE
)
spline_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("age", "treatment"),
  event_value = "1",
  variable_info = spline_variable_info,
  spline_covariate = "age",
  spline_df = 4L
)
reference_spline_fit <- survival::coxph(
  survival::Surv(time, status) ~ splines::ns(age, df = 4) + treatment,
  data = transform(cox_data, treatment = factor(treatment)),
  ties = "efron",
  x = TRUE,
  y = TRUE,
  model = TRUE
)
stopifnot(identical(spline_fit$spline_covariate, "age"), spline_fit$spline_df == 4L)
stopifnot(isTRUE(all.equal(unname(stats::coef(spline_fit$fit)), unname(stats::coef(reference_spline_fit)), tolerance = 1e-12)))
stopifnot(nrow(spline_fit$spline_test_table) == 1L)
stopifnot(spline_fit$spline_test_table$df[[1]] == 3L)
stopifnot(is.finite(spline_fit$spline_test_table$`Nonlinearity LR chi-square`[[1]]))
stopifnot(is.finite(spline_fit$spline_test_table$p[[1]]))
stopifnot(nrow(spline_fit$spline_curve) >= 101L)
reference_curve_row <- which.min(abs(spline_fit$spline_curve$Value - spline_fit$spline_curve$Reference))
stopifnot(isTRUE(all.equal(spline_fit$spline_curve$HR[[reference_curve_row]], 1, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(spline_fit$spline_curve$Lower[[reference_curve_row]], 1, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(spline_fit$spline_curve$Upper[[reference_curve_row]], 1, tolerance = 1e-12)))
spline_display_parameters <- survival_cox_display_parameter_table(spline_fit)
stopifnot(!any(spline_display_parameters$SplineBasis %in% TRUE))
stopifnot(any(grepl("natural cubic spline; see curve", spline_display_parameters$DisplayTerm, fixed = TRUE)))
stopifnot(nrow(survival_cox_spline_test_display_table(spline_fit)) == 1L)
stopifnot(inherits(survival_cox_spline_ggplot(spline_fit), "ggplot"))
stopifnot(grepl("natural cubic spline with 4 degrees of freedom", survival_method_sentence(spline_fit, "en"), fixed = TRUE))
spline_html <- htmltools::renderTags(survival_cox_results_panel(spline_fit, language = "en"))$html
stopifnot(grepl("Natural cubic spline functional-form analysis", spline_html, fixed = TRUE))
stopifnot(grepl("Spline basis coefficients are intentionally omitted", spline_html, fixed = TRUE))
stopifnot(grepl("survival_cox_spline_plot", spline_html, fixed = TRUE))
spline_audit_dir <- tempfile("spline-cox-audit-")
dir.create(spline_audit_dir)
spline_audit_files <- save_survival_reporting_files(spline_fit, spline_audit_dir, "en")
stopifnot(all(c("Cox_spline_nonlinearity_test.csv", "Cox_spline_HR_curve.csv") %in% basename(spline_audit_files)))

categorical_spline_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("age", "treatment"), variable_info = spline_variable_info, spline_covariate = "treatment", spline_df = 4L),
  error = identity
)
stopifnot(inherits(categorical_spline_error, "error"), grepl("numeric continuous covariate", conditionMessage(categorical_spline_error), fixed = TRUE))

unselected_spline_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "treatment", variable_info = spline_variable_info, spline_covariate = "age", spline_df = 4L),
  error = identity
)
stopifnot(inherits(unselected_spline_error, "error"), grepl("must also be included", conditionMessage(unselected_spline_error), fixed = TRUE))

clustered_spline_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "age", spline_covariate = "age", spline_df = 4L, cluster = "center"),
  error = identity
)
stopifnot(inherits(clustered_spline_error, "error"), grepl("not available with cluster-robust", conditionMessage(clustered_spline_error), fixed = TRUE))

stopifnot(grepl("survival_cox_spline_covariate", stratified_setup_code, fixed = TRUE))
stopifnot(grepl("Spline degrees of freedom", stratified_setup_code, fixed = TRUE))

message("Checking Cox time-varying coefficient analysis...")
time_varying_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("age", "treatment"),
  event_value = "1",
  variable_info = spline_variable_info,
  time_varying_covariate = "age",
  time_varying_times = "2, 5, 10"
)
reference_time_varying_fit <- survival::coxph(
  survival::Surv(time, status) ~ age + treatment + tt(age),
  data = transform(cox_data, treatment = factor(treatment)),
  ties = "efron",
  x = TRUE,
  y = TRUE,
  model = FALSE,
  tt = function(x, t, ...) x * log1p(t)
)
stopifnot(identical(time_varying_fit$time_varying_covariate, "age"))
stopifnot(identical(time_varying_fit$time_varying_function, "log(1 + time)"))
stopifnot(isTRUE(all.equal(unname(stats::coef(time_varying_fit$fit)), unname(stats::coef(reference_time_varying_fit)), tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(unname(stats::vcov(time_varying_fit$fit)), unname(stats::vcov(reference_time_varying_fit)), tolerance = 1e-12)))
stopifnot(nrow(time_varying_fit$time_varying_test_table) == 1L)
stopifnot(all(c(2, 5, 10) %in% time_varying_fit$time_varying_at_times$Time))
stopifnot(all(time_varying_fit$time_varying_curve$HR > 0))
stopifnot(all(time_varying_fit$time_varying_curve$Lower > 0))
stopifnot(all(time_varying_fit$time_varying_curve$Upper > 0))
tv_coef <- stats::coef(time_varying_fit$fit)
manual_log_hr_5 <- tv_coef[["age"]] + tv_coef[["tt(age)"]] * log1p(5)
reported_hr_5 <- time_varying_fit$time_varying_at_times$HR[time_varying_fit$time_varying_at_times$Time == 5][[1]]
stopifnot(isTRUE(all.equal(reported_hr_5, exp(manual_log_hr_5), tolerance = 1e-12)))
stopifnot(identical(time_varying_fit$ph_source, "Companion proportional-hazards model before time-varying extension"))
stopifnot(is.data.frame(time_varying_fit$ph_table), nrow(time_varying_fit$ph_table) > 0L)
time_varying_display <- survival_cox_display_parameter_table(time_varying_fit)
stopifnot(!any(time_varying_display$TimeVaryingEffect %in% TRUE))
stopifnot(any(grepl("time-varying HR; see curve", time_varying_display$DisplayTerm, fixed = TRUE)))
stopifnot(nrow(survival_cox_time_varying_test_display_table(time_varying_fit)) == 1L)
stopifnot(nrow(survival_cox_time_specific_hr_table(time_varying_fit)) == 3L)
stopifnot(inherits(survival_cox_time_varying_ggplot(time_varying_fit), "ggplot"))
stopifnot(grepl("coefficient for age varying over log(1 + time)", survival_method_sentence(time_varying_fit, "en"), fixed = TRUE))
time_varying_html <- htmltools::renderTags(survival_cox_results_panel(time_varying_fit, language = "en"))$html
stopifnot(grepl("2. Cox model with a time-varying coefficient", time_varying_html, fixed = TRUE))
stopifnot(grepl("Time-varying coefficient analysis", time_varying_html, fixed = TRUE))
stopifnot(grepl("Time-specific hazard ratios", time_varying_html, fixed = TRUE))
stopifnot(grepl("companion proportional-hazards model", time_varying_html, fixed = TRUE))
stopifnot(grepl("survival_cox_time_varying_plot", time_varying_html, fixed = TRUE))
time_varying_audit_dir <- tempfile("time-varying-cox-audit-")
dir.create(time_varying_audit_dir)
time_varying_audit_files <- save_survival_reporting_files(time_varying_fit, time_varying_audit_dir, "en")
stopifnot(all(c("Cox_time_varying_coefficient_test.csv", "Cox_time_specific_HR.csv", "Cox_time_varying_HR_curve.csv") %in% basename(time_varying_audit_files)))

categorical_time_varying_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("age", "treatment"), variable_info = spline_variable_info, time_varying_covariate = "treatment"),
  error = identity
)
stopifnot(inherits(categorical_time_varying_error, "error"), grepl("numeric continuous covariate", conditionMessage(categorical_time_varying_error), fixed = TRUE))

combined_spline_time_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "age", spline_covariate = "age", time_varying_covariate = "age"),
  error = identity
)
stopifnot(inherits(combined_spline_time_error, "error"), grepl("cannot be applied in the same", conditionMessage(combined_spline_time_error), fixed = TRUE))

clustered_time_varying_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "age", time_varying_covariate = "age", cluster = "center"),
  error = identity
)
stopifnot(inherits(clustered_time_varying_error, "error"), grepl("not available with cluster-robust", conditionMessage(clustered_time_varying_error), fixed = TRUE))

time_varying_adjusted_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", c("age", "treatment"), variable_info = spline_variable_info, adjusted_group = "treatment", adjusted_bootstrap_reps = 40L, time_varying_covariate = "age"),
  error = identity
)
stopifnot(inherits(time_varying_adjusted_error, "error"), grepl("not available with a time-varying coefficient", conditionMessage(time_varying_adjusted_error), fixed = TRUE))

invalid_time_varying_report_time <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "age", time_varying_covariate = "age", time_varying_times = as.character(max(cox_data$time) + 1)),
  error = identity
)
stopifnot(inherits(invalid_time_varying_report_time, "error"), grepl("maximum observed follow-up", conditionMessage(invalid_time_varying_report_time), fixed = TRUE))

stopifnot(grepl("survival_cox_time_varying_covariate", stratified_setup_code, fixed = TRUE))
stopifnot(grepl("HR reporting times", stratified_setup_code, fixed = TRUE))

message("Checking Cox tied-event methods...")
tied_data <- cox_data[seq_len(36L), c("age"), drop = FALSE]
tied_data$time <- rep(seq_len(12L), each = 3L)
tied_data$status <- rep(c(1L, 1L, 0L), 12L)
for (ties_method in c("efron", "breslow", "exact")) {
  tied_fit <- prepare_cox_analysis_result(
    tied_data,
    time = "time",
    event = "status",
    covariates = "age",
    event_value = "1",
    ties_method = ties_method
  )
  reference_tied_fit <- survival::coxph(
    survival::Surv(time, status) ~ age,
    data = tied_data,
    ties = ties_method,
    x = TRUE,
    y = TRUE,
    model = TRUE
  )
  stopifnot(identical(tied_fit$ties_method, ties_method))
  stopifnot(isTRUE(all.equal(unname(stats::coef(tied_fit$fit)), unname(stats::coef(reference_tied_fit)), tolerance = 1e-12)))
  stopifnot(isTRUE(all.equal(tied_fit$coef_table$SE, unname(summary(reference_tied_fit)$coefficients[, "se(coef)"]), tolerance = 1e-12)))
  stopifnot(tied_fit$ties_summary$Events[[1]] == 24L)
  stopifnot(tied_fit$ties_summary$`Distinct event times`[[1]] == 12L)
  stopifnot(tied_fit$ties_summary$`Tied event times`[[1]] == 12L)
  stopifnot(tied_fit$ties_summary$`Events at tied times`[[1]] == 24L)
  stopifnot(tied_fit$ties_summary$`Maximum events at one time`[[1]] == 2L)
  stopifnot(isTRUE(all.equal(tied_fit$ties_summary$`Proportion of events at tied times`[[1]], 1, tolerance = 1e-12)))
}
breslow_tied_fit <- prepare_cox_analysis_result(tied_data, "time", "status", "age", ties_method = "breslow")
stopifnot("breslow_with_substantial_ties" %in% survival_stability_review(breslow_tied_fit, "en")$Code)
stopifnot(nrow(survival_cox_ties_summary_table(breslow_tied_fit)) == 1L)
stopifnot(grepl("using the Breslow method for tied event times", survival_method_sentence(breslow_tied_fit, "en"), fixed = TRUE))
tied_html <- htmltools::renderTags(survival_cox_results_panel(breslow_tied_fit, language = "en"))$html
stopifnot(grepl("Tied-event summary", tied_html, fixed = TRUE))
stopifnot(grepl("not selected by searching for the smallest p-value", tied_html, fixed = TRUE))
tied_audit_dir <- tempfile("tied-cox-audit-")
dir.create(tied_audit_dir)
tied_audit_files <- save_survival_reporting_files(breslow_tied_fit, tied_audit_dir, "en")
stopifnot("Cox_tied_event_summary.csv" %in% basename(tied_audit_files))

invalid_ties_error <- tryCatch(
  prepare_cox_analysis_result(tied_data, "time", "status", "age", ties_method = "unsupported"),
  error = identity
)
stopifnot(inherits(invalid_ties_error, "error"), grepl("must be Efron, Breslow, or exact", conditionMessage(invalid_ties_error), fixed = TRUE))

exact_cluster_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "age", cluster = "center", ties_method = "exact"),
  error = identity
)
stopifnot(inherits(exact_cluster_error, "error"), grepl("not available with cluster-robust", conditionMessage(exact_cluster_error), fixed = TRUE))

exact_start_stop_error <- tryCatch(
  prepare_cox_analysis_result(td, "", "status", "exposure", start = "start", stop = "stop", subject_id = "id", ties_method = "exact"),
  error = identity
)
stopifnot(inherits(exact_start_stop_error, "error"), grepl("not available for start-stop", conditionMessage(exact_start_stop_error), fixed = TRUE))

exact_time_varying_error <- tryCatch(
  prepare_cox_analysis_result(cox_data, "time", "status", "age", time_varying_covariate = "age", ties_method = "exact"),
  error = identity
)
stopifnot(inherits(exact_time_varying_error, "error"), grepl("not available with a time-varying coefficient", conditionMessage(exact_time_varying_error), fixed = TRUE))

stopifnot(grepl("survival_cox_ties_method", stratified_setup_code, fixed = TRUE))
stopifnot(grepl("Partial-likelihood method", stratified_setup_code, fixed = TRUE))

message("Checking categorical-predictor omnibus Cox tests...")
cox_data$stage <- factor(rep(c("I", "II", "III"), length.out = nrow(cox_data)))
categorical_variable_info <- data.frame(
  name = c("age", "stage"),
  measurement = c("continuous", "category"),
  stringsAsFactors = FALSE
)
categorical_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("age", "stage"),
  event_value = "1",
  variable_info = categorical_variable_info
)
categorical_joint <- categorical_fit$categorical_joint_tests
stopifnot(nrow(categorical_joint) == 1L)
stopifnot(identical(categorical_joint$Variable, "stage"))
stopifnot(categorical_joint$Levels[[1]] == 3L, categorical_joint$Parameters[[1]] == 2L, categorical_joint$df[[1]] == 2L)
stopifnot(isTRUE(categorical_joint$Estimable[[1]]), is.finite(categorical_joint$`Wald chi-square`[[1]]), is.finite(categorical_joint$p[[1]]))
stage_terms <- grep("^stage", names(stats::coef(categorical_fit$fit)), value = TRUE)
stage_beta <- stats::coef(categorical_fit$fit)[stage_terms]
stage_variance <- stats::vcov(categorical_fit$fit)[stage_terms, stage_terms, drop = FALSE]
manual_stage_wald <- as.numeric(crossprod(stage_beta, solve(stage_variance, stage_beta)))
stopifnot(isTRUE(all.equal(categorical_joint$`Wald chi-square`[[1]], manual_stage_wald, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(categorical_joint$p[[1]], stats::pchisq(manual_stage_wald, df = 2, lower.tail = FALSE), tolerance = 1e-12)))
stopifnot(nrow(survival_cox_categorical_joint_test_table(categorical_fit)) == 1L)
stopifnot(nrow(categorical_fit$categorical_reference_table) == 1L)
stopifnot(identical(categorical_fit$categorical_reference_table$Variable, "stage"))
stopifnot(identical(categorical_fit$categorical_reference_table$`Reference level`, "I"))
stopifnot(grepl("Treatment", categorical_fit$categorical_reference_table$Contrast, fixed = TRUE))
categorical_html <- htmltools::renderTags(survival_cox_results_panel(categorical_fit, language = "en"))$html
stopifnot(grepl("Categorical-predictor omnibus tests", categorical_html, fixed = TRUE))
stopifnot(grepl("jointly tests all non-reference coefficients", categorical_html, fixed = TRUE))
stopifnot(grepl("Categorical reference levels and contrast coding", categorical_html, fixed = TRUE))
categorical_audit_dir <- tempfile("categorical-cox-audit-")
dir.create(categorical_audit_dir)
categorical_audit_files <- save_survival_reporting_files(categorical_fit, categorical_audit_dir, "en")
stopifnot("Cox_categorical_joint_tests.csv" %in% basename(categorical_audit_files))
stopifnot("Cox_categorical_reference_levels.csv" %in% basename(categorical_audit_files))

releveled_categorical_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("age", "stage"),
  event_value = "1",
  variable_info = categorical_variable_info,
  reference_values = c(stage = "III")
)
stopifnot(identical(releveled_categorical_fit$categorical_reference_table$`Reference level`, "III"))
stopifnot(all(c("stageI", "stageII") %in% names(stats::coef(releveled_categorical_fit$fit))))

original_contrast_options <- getOption("contrasts")
options(contrasts = c("contr.sum", "contr.poly"))
sum_option_categorical_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("age", "stage"),
  event_value = "1",
  variable_info = categorical_variable_info
)
options(contrasts = original_contrast_options)
stopifnot(identical(names(stats::coef(sum_option_categorical_fit$fit)), names(stats::coef(categorical_fit$fit))))
stopifnot(isTRUE(all.equal(unname(stats::coef(sum_option_categorical_fit$fit)), unname(stats::coef(categorical_fit$fit)), tolerance = 1e-12)))
stopifnot(identical(sum_option_categorical_fit$categorical_reference_table, categorical_fit$categorical_reference_table))

clustered_categorical_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("age", "stage"),
  event_value = "1",
  variable_info = categorical_variable_info,
  cluster = "center"
)
clustered_joint <- clustered_categorical_fit$categorical_joint_tests
clustered_stage_terms <- grep("^stage", names(stats::coef(clustered_categorical_fit$fit)), value = TRUE)
clustered_stage_beta <- stats::coef(clustered_categorical_fit$fit)[clustered_stage_terms]
clustered_stage_variance <- stats::vcov(clustered_categorical_fit$fit)[clustered_stage_terms, clustered_stage_terms, drop = FALSE]
manual_clustered_stage_wald <- as.numeric(crossprod(clustered_stage_beta, solve(clustered_stage_variance, clustered_stage_beta)))
stopifnot(identical(clustered_joint$Variance[[1]], "Cluster-robust (center)"))
stopifnot(isTRUE(all.equal(clustered_joint$`Wald chi-square`[[1]], manual_clustered_stage_wald, tolerance = 1e-12)))

message("Checking marginal standardized survival curves...")
cox_variable_info <- data.frame(
  name = c("age", "treatment"),
  measurement = c("continuous", "category"),
  stringsAsFactors = FALSE
)
adjusted_fit <- prepare_cox_analysis_result(
  cox_data,
  time = "time",
  event = "status",
  covariates = c("treatment", "age"),
  event_value = "1",
  variable_info = cox_variable_info,
  adjusted_group = "treatment",
  adjusted_bootstrap_reps = 40L,
  adjusted_times = "2, 5, 10"
)
adjusted <- adjusted_fit$adjusted_survival
stopifnot(is.list(adjusted))
stopifnot(identical(adjusted$method, "Marginal standardization"))
stopifnot(identical(adjusted$ci_method, "Pointwise percentile bootstrap 95% CI"))
stopifnot(identical(sort(unique(adjusted$curve$Level)), c("A", "B")))
stopifnot(all(adjusted$curve$Survival >= 0 & adjusted$curve$Survival <= 1))
stopifnot(all(adjusted$curve$Survival[adjusted$curve$Time == 0] == 1))
stopifnot(adjusted$bootstrap_successful >= adjusted$minimum_successful)
stopifnot(isTRUE(adjusted$ci_available), adjusted$bootstrap_effective_ratio >= .8)
stopifnot(all(is.finite(adjusted$curve$Lower)))
stopifnot(all(is.finite(adjusted$curve$Upper)))
stopifnot(nrow(adjusted$time_point_estimates) == 6L)
stopifnot(nrow(adjusted$time_point_contrasts) == 3L)
stopifnot(all(adjusted$time_point_estimates$Time %in% c(2, 5, 10)))
stopifnot(all(is.finite(adjusted$time_point_contrasts$Difference)))
stopifnot(all(is.finite(adjusted$time_point_contrasts$`Difference lower`)))
stopifnot(all(is.finite(adjusted$time_point_contrasts$`Difference upper`)))
stopifnot(nrow(survival_adjusted_survival_time_table(adjusted_fit)) == 6L)
stopifnot(nrow(survival_adjusted_survival_contrast_table(adjusted_fit)) == 3L)
stopifnot(inherits(survival_adjusted_survival_ggplot(adjusted_fit), "ggplot"))
adjusted_html <- htmltools::renderTags(survival_cox_results_panel(adjusted_fit, language = "en"))$html
stopifnot(grepl("6. Marginal adjusted survival", adjusted_html, fixed = TRUE))
stopifnot(grepl("not a curve for one mean-covariate subject", adjusted_html, fixed = TRUE))
stopifnot(grepl("Adjusted survival at specified times", adjusted_html, fixed = TRUE))
stopifnot(grepl("Pairwise adjusted-survival contrasts", adjusted_html, fixed = TRUE))
adjusted_audit_dir <- tempfile("adjusted-survival-audit-")
dir.create(adjusted_audit_dir)
adjusted_audit_files <- save_survival_reporting_files(adjusted_fit, adjusted_audit_dir, "en")
stopifnot(all(c("Adjusted_survival_curve.csv", "Adjusted_survival_time_points.csv", "Adjusted_survival_contrasts.csv") %in% basename(adjusted_audit_files)))

invalid_adjusted_time <- tryCatch(
  prepare_cox_analysis_result(
    cox_data,
    time = "time",
    event = "status",
    covariates = c("treatment", "age"),
    event_value = "1",
    variable_info = cox_variable_info,
    adjusted_group = "treatment",
    adjusted_bootstrap_reps = 40L,
    adjusted_times = as.character(max(cox_data$time) + 1)
  ),
  error = identity
)
stopifnot(inherits(invalid_adjusted_time, "error"), grepl("maximum observed follow-up", conditionMessage(invalid_adjusted_time), fixed = TRUE))

adjusted_setup_code <- paste(deparse(body(survival_cox_setup_panel)), collapse = "\n")
stopifnot(identical(formals(survival_cox_setup_panel)$adjusted_bootstrap_reps, 2000L))
stopifnot(grepl("survival_cox_adjusted_times", adjusted_setup_code, fixed = TRUE))
stopifnot(grepl("Adjusted-survival reporting times", adjusted_setup_code, fixed = TRUE))
stopifnot(grepl("max = 5000", adjusted_setup_code, fixed = TRUE))

message("Checking cumulative incidence and Gray test...")
set.seed(20260816)
competing_data <- data.frame(
  time = stats::rexp(160, rate = .08),
  status = c(
    sample(c(0, 1, 2), 80, replace = TRUE, prob = c(.25, .55, .20)),
    sample(c(0, 1, 2), 80, replace = TRUE, prob = c(.25, .30, .45))
  ),
  group = rep(c("A", "B"), each = 80),
  age = stats::rnorm(160, 60, 8),
  stage = factor(sample(c("I", "II"), 160, replace = TRUE))
)
competing_result <- prepare_competing_risk_result(
  competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "group",
  rate_times = "0,5,10"
)
stopifnot(identical(competing_result$type, "competing_risk"))
stopifnot(inherits(competing_result$fit, "cuminc"))
stopifnot(competing_result$preflight$counts$events == sum(competing_data$status == 1))
stopifnot(competing_result$preflight$counts$competing_events == sum(competing_data$status == 2))
stopifnot(nrow(competing_result$curve) > 0L)
stopifnot(all(competing_result$curve$CIF >= 0 & competing_result$curve$CIF <= 1))
stopifnot(all(competing_result$curve$Lower >= 0 & competing_result$curve$Upper <= 1))
stopifnot(nrow(competing_result$cif_integrity) == 2L)
stopifnot(all(competing_result$cif_integrity$`Integrity passed`))
cif_integrity_probe <- competing_result
cif_integrity_probe$cif_integrity$`Integrity passed`[[1]] <- FALSE
stopifnot("cif_integrity_failure" %in% survival_stability_review(cif_integrity_probe, "en")$Code)
stopifnot(nrow(competing_result$gray_tests) == 2L)
stopifnot(all(is.finite(competing_result$gray_tests$Statistic)))
stopifnot(all(is.finite(competing_result$gray_tests$p)))
stopifnot(all(competing_result$gray_tests$Estimable))
stopifnot(nrow(competing_result$event_count_table) == 4L)
stopifnot(sum(competing_result$event_count_table$Events[competing_result$event_count_table$`Event role` == "event_of_interest"]) == sum(competing_data$status == 1))
stopifnot(sum(competing_result$event_count_table$Events[competing_result$event_count_table$`Event role` == "competing_event"]) == sum(competing_data$status == 2))
sum_cif <- stats::aggregate(CIF ~ Group + Time, data = competing_result$cif_at_times, sum)
stopifnot(all(sum_cif$CIF <= 1 + 1e-10))
stopifnot(all(competing_result$cif_at_times$CIF[competing_result$cif_at_times$Time == 0] == 0))
stopifnot(inherits(survival_competing_ggplot(competing_result), "ggplot"))
stopifnot(inherits(survival_competing_risk_table_plot(competing_result), "ggplot"))
combined_plot_file <- tempfile(fileext = ".pdf")
grDevices::pdf(combined_plot_file, width = 7, height = 7)
survival_draw_plot_with_risk_table(survival_competing_ggplot(competing_result), survival_competing_risk_table_plot(competing_result))
grDevices::dev.off()
stopifnot(file.exists(combined_plot_file), file.info(combined_plot_file)$size > 0)
combined_export_dir <- tempfile("survival-figure-")
dir.create(combined_export_dir)
combined_png <- save_survival_competing_figure_files(competing_result, combined_export_dir, dpi = 72)
stopifnot(file.exists(combined_png), file.info(combined_png)$size > 0)
competing_html <- htmltools::renderTags(survival_competing_results_panel(competing_result, language = "en"))$html
stopifnot(grepl("2. Cumulative incidence at selected time points", competing_html, fixed = TRUE))
stopifnot(grepl("3. Gray test", competing_html, fixed = TRUE))
stopifnot(grepl("1-KM is not used", competing_html, fixed = TRUE))
stopifnot(grepl("Group-by-cause event counts", competing_html, fixed = TRUE))
stopifnot(grepl("CIF integrity checks", competing_html, fixed = TRUE))
competing_audit_dir <- tempfile("competing-audit-")
dir.create(competing_audit_dir)
competing_audit_files <- save_survival_reporting_files(competing_result, competing_audit_dir, "en")
stopifnot(any(grepl("Competing_group_cause_event_counts.csv", competing_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Gray_tests.csv", competing_audit_files, fixed = TRUE)))
stopifnot(any(grepl("CIF_integrity_checks.csv", competing_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Competing_estimand_contract.csv", competing_audit_files, fixed = TRUE)))

message("Checking CIF reference values and event-code invariance...")
manual_cif_at_times <- function(time, status, cause, times) {
  event_times <- sort(unique(time[status > 0L]))
  survival_value <- 1
  cif_value <- 0
  cif_path <- numeric(length(event_times))
  for (index in seq_along(event_times)) {
    current <- event_times[[index]]
    risk <- sum(time >= current)
    all_events <- sum(time == current & status > 0L)
    cause_events <- sum(time == current & status == cause)
    cif_value <- cif_value + survival_value * cause_events / risk
    survival_value <- survival_value * (1 - all_events / risk)
    cif_path[[index]] <- cif_value
  }
  stats::approx(c(0, event_times), c(0, cif_path), xout = times, method = "constant", f = 0, rule = 2, ties = "ordered")$y
}
for (group_value in unique(competing_data$group)) {
  keep <- competing_data$group == group_value
  for (cause in c(1L, 2L)) {
    engine <- competing_result$cif_at_times[
      competing_result$cif_at_times$Group == group_value & as.integer(competing_result$cif_at_times$CauseCode) == cause,
      c("Time", "CIF"),
      drop = FALSE
    ]
    reference <- manual_cif_at_times(competing_data$time[keep], competing_data$status[keep], cause, engine$Time)
    stopifnot(isTRUE(all.equal(engine$CIF, reference, tolerance = 1e-10, check.attributes = FALSE)))
  }
}

single_cause_data <- data.frame(time = c(1, 2, 3, 4, 5, 6), status = c(1, 0, 1, 0, 1, 0))
single_cause_result <- prepare_competing_risk_result(
  single_cause_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = character(0),
  rate_times = single_cause_data$time
)
single_cause_km <- survival::survfit(survival::Surv(time, status) ~ 1, data = single_cause_data)
single_cause_km_cif <- 1 - summary(single_cause_km, times = single_cause_data$time, extend = TRUE)$surv
single_cause_engine_cif <- single_cause_result$cif_at_times$CIF[order(single_cause_result$cif_at_times$Time)]
stopifnot(isTRUE(all.equal(single_cause_engine_cif, single_cause_km_cif, tolerance = 1e-12, check.attributes = FALSE)))

multi_cause_data <- competing_data
second_competing_indexes <- which(multi_cause_data$status == 2L)
multi_cause_data$status[second_competing_indexes[seq(2L, length(second_competing_indexes), by = 2L)]] <- 3L
original_map <- data.frame(
  raw_value = c("0", "1", "2", "3"),
  role = c("censored", "event_of_interest", "competing_event", "competing_event"),
  label = c("Censored", "Interest", "Competing A", "Competing B"),
  stringsAsFactors = FALSE
)
original_coded_result <- prepare_competing_risk_result(
  multi_cause_data, "time", "status", group = "group", rate_times = "0,5,10", event_map = original_map
)
recoded_data <- multi_cause_data[nrow(multi_cause_data):1L, , drop = FALSE]
recoded_data$status <- c("CEN", "INTEREST", "COMP_A", "COMP_B")[match(recoded_data$status, 0:3)]
recoded_map <- data.frame(
  raw_value = c("COMP_B", "INTEREST", "CEN", "COMP_A"),
  role = c("competing_event", "event_of_interest", "censored", "competing_event"),
  label = c("Competing B", "Interest", "Censored", "Competing A"),
  stringsAsFactors = FALSE
)
recoded_result <- prepare_competing_risk_result(
  recoded_data, "time", "status", group = "group", rate_times = "0,5,10", event_map = recoded_map
)
stopifnot(identical(recoded_result$event_of_interest, "INTEREST"))
stopifnot(all(recoded_result$estimand_table$`Target event value` == "INTEREST"))
semantic_cause_labels <- function(result, cause_codes) {
  ordered_map <- rbind(
    result$event_map[result$event_map$role == "event_of_interest", , drop = FALSE],
    result$event_map[result$event_map$role == "competing_event", , drop = FALSE]
  )
  ordered_map$label[as.integer(cause_codes)]
}
normalized_cif <- function(result) {
  table <- result$cif_at_times
  table$Cause <- semantic_cause_labels(result, table$CauseCode)
  table <- table[order(table$Group, table$Cause, table$Time), c("Group", "Cause", "Time", "CIF", "Lower", "Upper"), drop = FALSE]
  rownames(table) <- NULL
  table
}
normalized_gray <- function(result) {
  table <- result$gray_tests
  table$Cause <- semantic_cause_labels(result, table$CauseCode)
  table <- table[order(table$Cause), c("Cause", "Statistic", "p", "df", "Estimable"), drop = FALSE]
  rownames(table) <- NULL
  table
}
normalized_counts <- function(result) {
  table <- result$event_count_table
  table <- table[order(table$Group, table$Label), c("Group", "Label", "Analysis N", "Events", "Event proportion"), drop = FALSE]
  rownames(table) <- NULL
  table
}
stopifnot(isTRUE(all.equal(normalized_cif(original_coded_result), normalized_cif(recoded_result), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(normalized_gray(original_coded_result), normalized_gray(recoded_result), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(normalized_counts(original_coded_result), normalized_counts(recoded_result), tolerance = 1e-12, check.attributes = FALSE)))

sparse_competing_data <- data.frame(
  time = 1:12,
  status = c(0, 2, 2, 0, 2, 0, 0, 1, 1, 0, 1, 0),
  group = rep(c("A", "B"), each = 6)
)
sparse_competing_result <- prepare_competing_risk_result(
  sparse_competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "group"
)
sparse_competing_codes <- survival_stability_review(sparse_competing_result, "en")$Code
stopifnot("zero_group_cause_events" %in% sparse_competing_codes)
stopifnot("sparse_group_cause_events" %in% sparse_competing_codes)
stopifnot(sum(sparse_competing_result$event_count_table$Events == 0L) == 2L)
gray_not_estimable_probe <- sparse_competing_result
gray_not_estimable_probe$gray_tests$Statistic[[1]] <- NA_real_
gray_not_estimable_probe$gray_tests$Estimable[[1]] <- FALSE
stopifnot("gray_test_not_estimable" %in% survival_stability_review(gray_not_estimable_probe, "en")$Code)

unknown_competing_error <- tryCatch({
  prepare_competing_risk_result(competing_data, "time", "status", event_of_interest = "1", censored_value = "0", competing_values = character(0), group = "group")
  NULL
}, error = function(e) e)
stopifnot(inherits(unknown_competing_error, "error"))
stopifnot(grepl("event code", conditionMessage(unknown_competing_error), ignore.case = TRUE))

message("Checking cause-specific Cox and Fine-Gray regression...")
competing_variable_info <- data.frame(
  name = c("age", "stage"),
  measurement = c("continuous", "category"),
  stringsAsFactors = FALSE
)
competing_regression <- prepare_competing_risk_result(
  competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "group",
  covariates = c("age", "stage"),
  regression = "both",
  variable_info = competing_variable_info
)
stopifnot(is.list(competing_regression$cause_specific))
stopifnot(inherits(competing_regression$cause_specific$fit, "coxph"))
stopifnot(isTRUE(competing_regression$cause_specific$estimable))
stopifnot(nrow(competing_regression$cause_specific$coef_table) == 2L)
stopifnot(all(competing_regression$cause_specific$coef_table$HR > 0))
stopifnot(nrow(competing_regression$cause_specific$ph_table) > 0L)
stopifnot(is.list(competing_regression$fine_gray))
stopifnot(inherits(competing_regression$fine_gray$fit, "crr"))
stopifnot(isTRUE(competing_regression$fine_gray$converged))
stopifnot(isTRUE(competing_regression$fine_gray$estimable))
stopifnot(nrow(competing_regression$fine_gray$coef_table) == 2L)
stopifnot(all(competing_regression$fine_gray$coef_table$sHR > 0))
stopifnot(nrow(competing_regression$fine_gray$residual_review) == 2L)
stopifnot(all(c("Unique target-failure times", "Spearman rho", "Exploratory p", "Holm-adjusted p", "Review signal") %in% names(competing_regression$fine_gray$residual_review)))
stopifnot(nrow(competing_regression$fine_gray$residual_data) == length(competing_regression$fine_gray$fit$uftime) * length(competing_regression$fine_gray$design_columns))
stopifnot(all(c("Event time", "Term", "Residual") %in% names(competing_regression$fine_gray$residual_data)))
stopifnot(inherits(survival_fine_gray_residual_ggplot(competing_regression), "ggplot"))
stopifnot(nrow(competing_regression$estimand_table) == 4L)
stopifnot(identical(competing_regression$estimand_table$Measure, c("CIF", "Gray chi-square / p (no effect size)", "Cause-specific HR", "Subdistribution HR (sHR)")))
cause_specific_display <- survival_cause_specific_coef_table(competing_regression)
fine_gray_display <- survival_fine_gray_coef_table(competing_regression)
stopifnot(all(c("Variable", "Level", "HR") %in% names(cause_specific_display)))
stopifnot(all(c("Variable", "Level", "sHR") %in% names(fine_gray_display)))
stopifnot(nrow(cause_specific_display) == 3L, nrow(fine_gray_display) == 3L)
stopifnot(identical(cause_specific_display$Variable, c("age", "stage", "stage")))
stopifnot(identical(fine_gray_display$Variable, c("age", "stage", "stage")))
stopifnot(identical(cause_specific_display$Level, c("", "I (reference)", "II")))
stopifnot(identical(fine_gray_display$Level, c("", "I (reference)", "II")))
stopifnot(identical(cause_specific_display$HR[[2]], "1.000"), identical(cause_specific_display$`95% CI`[[2]], "Reference"))
stopifnot(identical(fine_gray_display$sHR[[2]], "1.000"), identical(fine_gray_display$`95% CI`[[2]], "Reference"))
stopifnot(identical(cause_specific_display$B[[2]], ""), identical(cause_specific_display$p[[2]], ""))
stopifnot(identical(fine_gray_display$B[[2]], ""), identical(fine_gray_display$p[[2]], ""))

reference_design <- stats::model.matrix(~ age + stage, data = competing_data)
reference_design <- reference_design[, colnames(reference_design) != "(Intercept)", drop = FALSE]
stopifnot(identical(competing_regression$fine_gray$design_columns, colnames(reference_design)))
reference_cs <- survival::coxph(survival::Surv(time, status == 1L) ~ age + stage, data = competing_data, ties = "efron", x = TRUE, y = TRUE, model = TRUE)
reference_cs_summary <- summary(reference_cs)
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$coef_table$B, unname(reference_cs_summary$coefficients[, "coef"]), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$coef_table$SE, unname(reference_cs_summary$coefficients[, "se(coef)"]), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$coef_table$HR, unname(reference_cs_summary$conf.int[, "exp(coef)"]), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$coef_table$LLCI, unname(reference_cs_summary$conf.int[, "lower .95"]), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$coef_table$ULCI, unname(reference_cs_summary$conf.int[, "upper .95"]), tolerance = 1e-12, check.attributes = FALSE)))
engine_cs_joint <- competing_regression$cause_specific$categorical_joint_tests
reference_cs_stage_term <- grep("^stage", names(stats::coef(reference_cs)), value = TRUE)
reference_cs_stage_wald <- as.numeric(stats::coef(reference_cs)[reference_cs_stage_term]^2 / stats::vcov(reference_cs)[reference_cs_stage_term, reference_cs_stage_term])
stopifnot(nrow(engine_cs_joint) == 1L, identical(engine_cs_joint$Variable, "stage"), engine_cs_joint$Parameters[[1]] == 1L, isTRUE(engine_cs_joint$Estimable[[1]]))
stopifnot(isTRUE(all.equal(engine_cs_joint$`Wald chi-square`[[1]], reference_cs_stage_wald, tolerance = 1e-12)))
reference_cs_model_tests <- rbind(reference_cs_summary$logtest, reference_cs_summary$waldtest, reference_cs_summary$sctest)
engine_cs_model_tests <- competing_regression$cause_specific$model_tests
stopifnot(identical(engine_cs_model_tests$Test, c("Likelihood-ratio", "Wald", "Score")))
stopifnot(isTRUE(all.equal(as.matrix(engine_cs_model_tests[, c("Statistic", "df", "p"), drop = FALSE]), unname(reference_cs_model_tests[, 1:3, drop = FALSE]), tolerance = 1e-12, check.attributes = FALSE)))
reference_cs_ph <- survival::cox.zph(reference_cs)
engine_cs_ph <- competing_regression$cause_specific$ph_table
stopifnot(identical(engine_cs_ph$Term, rownames(reference_cs_ph$table)))
stopifnot(isTRUE(all.equal(as.matrix(engine_cs_ph[, c("chisq", "df", "p"), drop = FALSE]), unname(reference_cs_ph$table[, c("chisq", "df", "p"), drop = FALSE]), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(nrow(competing_regression$cause_specific$residual_table) == 2L)
stopifnot(identical(competing_regression$cause_specific$residual_table$Type, c("Martingale", "Deviance")))
reference_cs_martingale <- as.numeric(stats::residuals(reference_cs, type = "martingale"))
engine_cs_functional <- competing_regression$cause_specific$functional_form_data
stopifnot(nrow(engine_cs_functional) == nrow(competing_data))
stopifnot(all(engine_cs_functional$Covariate == "age"))
stopifnot(identical(engine_cs_functional$`Analysis row`, seq_len(nrow(competing_data))))
stopifnot(isTRUE(all.equal(engine_cs_functional$`Martingale residual`, reference_cs_martingale, tolerance = 1e-12, check.attributes = FALSE)))
reference_cs_dfbeta <- as.matrix(stats::residuals(reference_cs, type = "dfbeta"))
reference_cs_dfbetas <- as.matrix(stats::residuals(reference_cs, type = "dfbetas"))
engine_cs_influence <- competing_regression$cause_specific$influence_table
stopifnot(nrow(engine_cs_influence) == ncol(reference_cs_dfbeta))
stopifnot(identical(engine_cs_influence$Term, names(stats::coef(reference_cs))))
stopifnot(isTRUE(all.equal(engine_cs_influence$`Maximum absolute DFBETA`, apply(abs(reference_cs_dfbeta), 2L, max), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(engine_cs_influence$`Maximum absolute DFBETAS`, apply(abs(reference_cs_dfbetas), 2L, max), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(all(engine_cs_influence$`Screening threshold` == 2 / sqrt(nrow(competing_data))))
reference_cs_collinearity <- survival_cox_collinearity(reference_cs$x)
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$collinearity_table, reference_cs_collinearity$table, tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$cause_specific$condition_number, reference_cs_collinearity$condition_number, tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(inherits(survival_cox_functional_form_ggplot(competing_regression$cause_specific), "ggplot"))
cs_ph_plot_file <- tempfile(fileext = ".pdf")
grDevices::pdf(cs_ph_plot_file)
stopifnot(isTRUE(survival_cox_ph_plot(competing_regression$cause_specific)))
grDevices::dev.off()
stopifnot(file.exists(cs_ph_plot_file), file.info(cs_ph_plot_file)$size > 0)
reference_fg <- cmprsk::crr(ftime = competing_data$time, fstatus = competing_data$status, cov1 = reference_design, failcode = 1L, cencode = 0L)
reference_fg_se <- sqrt(diag(reference_fg$var))
stopifnot(isTRUE(all.equal(competing_regression$fine_gray$coef_table$B, unname(reference_fg$coef), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$fine_gray$coef_table$SE, unname(reference_fg_se), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$fine_gray$coef_table$sHR, unname(exp(reference_fg$coef)), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(nrow(competing_regression$categorical_reference_table) == 1L)
stopifnot(identical(competing_regression$categorical_reference_table$Variable, "stage"))
stopifnot(identical(competing_regression$categorical_reference_table$`Reference level`, "I"))
engine_fg_joint <- competing_regression$fine_gray$categorical_joint_tests
reference_fg_stage_term <- grep("^stage", names(reference_fg$coef), value = TRUE)
reference_fg_stage_wald <- as.numeric(reference_fg$coef[reference_fg_stage_term]^2 / reference_fg$var[which(names(reference_fg$coef) == reference_fg_stage_term), which(names(reference_fg$coef) == reference_fg_stage_term)])
stopifnot(nrow(engine_fg_joint) == 1L, identical(engine_fg_joint$Variable, "stage"), engine_fg_joint$Parameters[[1]] == 1L, isTRUE(engine_fg_joint$Estimable[[1]]))
stopifnot(isTRUE(all.equal(engine_fg_joint$`Wald chi-square`[[1]], reference_fg_stage_wald, tolerance = 1e-12)))
reference_fg_logtest <- as.numeric(summary(reference_fg)$logtest)
engine_fg_model_tests <- competing_regression$fine_gray$model_tests
stopifnot(identical(engine_fg_model_tests$Test, "Pseudo likelihood-ratio"))
stopifnot(isTRUE(all.equal(engine_fg_model_tests$Statistic[[1]], reference_fg_logtest[[1]], tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(engine_fg_model_tests$df[[1]], reference_fg_logtest[[2]], tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(engine_fg_model_tests$p[[1]], stats::pchisq(reference_fg_logtest[[1]], df = reference_fg_logtest[[2]], lower.tail = FALSE), tolerance = 1e-12)))
reference_fg_collinearity <- survival_cox_collinearity(reference_design)
stopifnot(isTRUE(all.equal(competing_regression$fine_gray$collinearity_table, reference_fg_collinearity$table, tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(competing_regression$fine_gray$condition_number, reference_fg_collinearity$condition_number, tolerance = 1e-12, check.attributes = FALSE)))
engine_fg_optimizer <- competing_regression$fine_gray$optimizer_table
reference_fg_relative_score <- max(abs(reference_fg$score) * pmax(abs(reference_fg$coef), 1)) / max(abs(reference_fg$loglik), 1)
reference_fg_information_diagonal <- diag(reference_fg$inf)
reference_fg_standardized_information <- reference_fg$inf / sqrt(outer(reference_fg_information_diagonal, reference_fg_information_diagonal))
stopifnot(nrow(engine_fg_optimizer) == 1L)
stopifnot(isTRUE(engine_fg_optimizer$Converged[[1]]))
stopifnot(isTRUE(all.equal(engine_fg_optimizer$`Relative score criterion`[[1]], reference_fg_relative_score, tolerance = 1e-12)))
stopifnot(engine_fg_optimizer$`Convergence tolerance`[[1]] == 1e-6)
stopifnot(engine_fg_optimizer$`Maximum iterations`[[1]] == 10L)
stopifnot(engine_fg_optimizer$`Information rank`[[1]] == qr(reference_fg$inf)$rank)
stopifnot(engine_fg_optimizer$Parameters[[1]] == length(reference_fg$coef))
stopifnot(isTRUE(all.equal(engine_fg_optimizer$`Standardized information condition number`[[1]], kappa(reference_fg_standardized_information, exact = TRUE), tolerance = 1e-12)))
stopifnot(isTRUE(engine_fg_optimizer$`Finite covariance matrix`[[1]]), isTRUE(engine_fg_optimizer$`Positive standard errors`[[1]]))
stopifnot(nrow(survival_fine_gray_optimizer_table(competing_regression)) == 1L)
reference_residual_data <- do.call(rbind, lapply(seq_len(ncol(reference_fg$res)), function(index) data.frame(
  `Event time` = as.numeric(reference_fg$uftime),
  Term = colnames(reference_design)[[index]],
  Residual = as.numeric(reference_fg$res[, index]),
  check.names = FALSE,
  stringsAsFactors = FALSE
)))
stopifnot(isTRUE(all.equal(competing_regression$fine_gray$residual_data, reference_residual_data, tolerance = 1e-12, check.attributes = FALSE)))

message("Checking multi-level categorical omnibus tests in competing-risk regression...")
multilevel_competing_data <- competing_data
multilevel_competing_data$stage3 <- factor(rep(c("I", "II", "III"), length.out = nrow(multilevel_competing_data)), levels = c("I", "II", "III"))
multilevel_competing_info <- data.frame(name = c("age", "stage3"), measurement = c("continuous", "category"), stringsAsFactors = FALSE)
multilevel_competing <- prepare_competing_risk_result(
  multilevel_competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  covariates = c("age", "stage3"),
  regression = "both",
  variable_info = multilevel_competing_info
)
reference_multilevel_design <- stats::model.matrix(~ age + stage3, data = multilevel_competing_data)
reference_multilevel_design <- reference_multilevel_design[, colnames(reference_multilevel_design) != "(Intercept)", drop = FALSE]
reference_multilevel_cs <- survival::coxph(survival::Surv(time, status == 1L) ~ age + stage3, data = multilevel_competing_data, ties = "efron", x = TRUE, y = TRUE, model = TRUE)
reference_multilevel_fg <- cmprsk::crr(ftime = multilevel_competing_data$time, fstatus = multilevel_competing_data$status, cov1 = reference_multilevel_design, failcode = 1L, cencode = 0L)
multilevel_cs_terms <- grep("^stage3", names(stats::coef(reference_multilevel_cs)), value = TRUE)
multilevel_cs_beta <- stats::coef(reference_multilevel_cs)[multilevel_cs_terms]
multilevel_cs_variance <- stats::vcov(reference_multilevel_cs)[multilevel_cs_terms, multilevel_cs_terms, drop = FALSE]
multilevel_cs_wald <- as.numeric(crossprod(multilevel_cs_beta, solve(multilevel_cs_variance, multilevel_cs_beta)))
multilevel_fg_terms <- grep("^stage3", names(reference_multilevel_fg$coef), value = TRUE)
multilevel_fg_index <- match(multilevel_fg_terms, names(reference_multilevel_fg$coef))
multilevel_fg_beta <- reference_multilevel_fg$coef[multilevel_fg_index]
multilevel_fg_variance <- reference_multilevel_fg$var[multilevel_fg_index, multilevel_fg_index, drop = FALSE]
multilevel_fg_wald <- as.numeric(crossprod(multilevel_fg_beta, solve(multilevel_fg_variance, multilevel_fg_beta)))
stopifnot(multilevel_competing$cause_specific$categorical_joint_tests$Parameters[[1]] == 2L, multilevel_competing$cause_specific$categorical_joint_tests$df[[1]] == 2L)
stopifnot(multilevel_competing$fine_gray$categorical_joint_tests$Parameters[[1]] == 2L, multilevel_competing$fine_gray$categorical_joint_tests$df[[1]] == 2L)
stopifnot(isTRUE(all.equal(multilevel_competing$cause_specific$categorical_joint_tests$`Wald chi-square`[[1]], multilevel_cs_wald, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(multilevel_competing$fine_gray$categorical_joint_tests$`Wald chi-square`[[1]], multilevel_fg_wald, tolerance = 1e-12)))
stopifnot(identical(multilevel_competing$categorical_reference_table$`Reference level`, "I"))
multilevel_cs_display <- survival_cause_specific_coef_table(multilevel_competing)
multilevel_fg_display <- survival_fine_gray_coef_table(multilevel_competing)
stopifnot(nrow(multilevel_cs_display) == 4L, nrow(multilevel_fg_display) == 4L)
stopifnot(identical(multilevel_cs_display$Variable, c("age", "stage3", "stage3", "stage3")))
stopifnot(identical(multilevel_fg_display$Variable, c("age", "stage3", "stage3", "stage3")))
stopifnot(identical(multilevel_cs_display$Level, c("", "I (reference)", "II", "III")))
stopifnot(identical(multilevel_fg_display$Level, c("", "I (reference)", "II", "III")))
stopifnot(identical(multilevel_cs_display$HR[[2]], "1.000"), identical(multilevel_fg_display$sHR[[2]], "1.000"))

original_competing_contrast_options <- getOption("contrasts")
options(contrasts = c("contr.sum", "contr.poly"))
sum_option_multilevel_competing <- prepare_competing_risk_result(
  multilevel_competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  covariates = c("age", "stage3"),
  regression = "both",
  variable_info = multilevel_competing_info
)
options(contrasts = original_competing_contrast_options)
stopifnot(identical(names(stats::coef(sum_option_multilevel_competing$cause_specific$fit)), names(stats::coef(multilevel_competing$cause_specific$fit))))
stopifnot(isTRUE(all.equal(unname(stats::coef(sum_option_multilevel_competing$cause_specific$fit)), unname(stats::coef(multilevel_competing$cause_specific$fit)), tolerance = 1e-12)))
stopifnot(identical(names(sum_option_multilevel_competing$fine_gray$fit$coef), names(multilevel_competing$fine_gray$fit$coef)))
stopifnot(isTRUE(all.equal(unname(sum_option_multilevel_competing$fine_gray$fit$coef), unname(multilevel_competing$fine_gray$fit$coef), tolerance = 1e-12)))
stopifnot(identical(sum_option_multilevel_competing$categorical_reference_table, multilevel_competing$categorical_reference_table))

message("Checking Fine-Gray censoring-distribution stratification...")
cengroup_data <- competing_data
cengroup_data$censor_stratum <- rep(c("Site A", "Site B"), length.out = nrow(cengroup_data))
cengroup_regression <- prepare_competing_risk_result(
  cengroup_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "group",
  covariates = c("age", "stage"),
  regression = "fine_gray",
  variable_info = competing_variable_info,
  censoring_group = "censor_stratum"
)
reference_cengroup <- droplevels(as.factor(cengroup_data$censor_stratum))
reference_cengroup_fg <- cmprsk::crr(
  ftime = cengroup_data$time,
  fstatus = cengroup_data$status,
  cov1 = reference_design,
  failcode = 1L,
  cencode = 0L,
  cengroup = reference_cengroup
)
stopifnot(identical(cengroup_regression$censoring_group, "censor_stratum"))
stopifnot(identical(cengroup_regression$fine_gray$censoring_group, "censor_stratum"))
stopifnot(nrow(cengroup_regression$censoring_group_table) == 2L)
stopifnot(sum(cengroup_regression$censoring_group_table$N) == nrow(cengroup_data))
stopifnot(sum(cengroup_regression$censoring_group_table$Censored) == sum(cengroup_data$status == 0L))
stopifnot(isTRUE(all.equal(cengroup_regression$fine_gray$coef_table$B, unname(reference_cengroup_fg$coef), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(cengroup_regression$fine_gray$coef_table$SE, unname(sqrt(diag(reference_cengroup_fg$var))), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(all(grepl("Estimated separately within censor_stratum", cengroup_regression$estimand_table$`Censoring distribution`, fixed = TRUE)))
stopifnot(grepl("censoring distributions estimated separately within censor_stratum", survival_method_sentence(cengroup_regression, "en"), fixed = TRUE))
cengroup_html <- htmltools::renderTags(survival_competing_results_panel(cengroup_regression, language = "en"))$html
stopifnot(grepl("Censoring-distribution estimation", cengroup_html, fixed = TRUE))
stopifnot(grepl("crr(cengroup=...)", cengroup_html, fixed = TRUE))
cengroup_audit_dir <- tempfile("fine-gray-cengroup-audit-")
dir.create(cengroup_audit_dir)
cengroup_audit_files <- save_survival_reporting_files(cengroup_regression, cengroup_audit_dir, "en")
stopifnot(any(grepl("Fine_Gray_censoring_distribution_strata.csv", cengroup_audit_files, fixed = TRUE)))
cengroup_sparse_probe <- cengroup_regression
cengroup_sparse_probe$censoring_group_table$`Review signal`[[1]] <- TRUE
stopifnot("fine_gray_sparse_censoring_stratum" %in% survival_stability_review(cengroup_sparse_probe, "en")$Code)

single_cengroup_error <- tryCatch(
  prepare_competing_risk_result(transform(cengroup_data, censor_stratum = "Only"), "time", "status", covariates = "age", regression = "fine_gray", censoring_group = "censor_stratum"),
  error = identity
)
stopifnot(inherits(single_cengroup_error, "error"), grepl("at least two observed levels", conditionMessage(single_cengroup_error), fixed = TRUE))
unused_cengroup_error <- tryCatch(
  prepare_competing_risk_result(cengroup_data, "time", "status", covariates = "age", regression = "cause_specific", censoring_group = "censor_stratum"),
  error = identity
)
stopifnot(inherits(unused_cengroup_error, "error"), grepl("only with Fine-Gray regression", conditionMessage(unused_cengroup_error), fixed = TRUE))
continuous_cengroup_error <- tryCatch(
  prepare_competing_risk_result(cengroup_data, "time", "status", covariates = "age", regression = "fine_gray", censoring_group = "time"),
  error = identity
)
stopifnot(inherits(continuous_cengroup_error, "error"), grepl("cannot also be the time or event variable", conditionMessage(continuous_cengroup_error), fixed = TRUE))

swapped_event_map <- data.frame(
  raw_value = c("0", "1", "2"),
  role = c("censored", "competing_event", "event_of_interest"),
  label = c("Censored", "Original interest as competing", "Original competing as interest"),
  stringsAsFactors = FALSE
)
swapped_regression <- prepare_competing_risk_result(
  competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  group = "group",
  rate_times = "0,5,10",
  covariates = c("age", "stage"),
  regression = "both",
  variable_info = competing_variable_info,
  event_map = swapped_event_map
)
stopifnot(identical(swapped_regression$event_of_interest, "2"))
stopifnot(all(swapped_regression$estimand_table$`Target event value` == "2"))
stopifnot(all(swapped_regression$estimand_table$`Competing event values` == "1"))
stopifnot(sum(swapped_regression$event_count_table$Events[swapped_regression$event_count_table$`Event role` == "event_of_interest"]) == sum(competing_data$status == 2L))
stopifnot(sum(swapped_regression$event_count_table$Events[swapped_regression$event_count_table$`Event role` == "competing_event"]) == sum(competing_data$status == 1L))
swapped_reference_cs <- survival::coxph(survival::Surv(time, status == 2L) ~ age + stage, data = competing_data, ties = "efron")
swapped_reference_cs_summary <- summary(swapped_reference_cs)
stopifnot(isTRUE(all.equal(swapped_regression$cause_specific$coef_table$B, unname(swapped_reference_cs_summary$coefficients[, "coef"]), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(swapped_regression$cause_specific$coef_table$SE, unname(swapped_reference_cs_summary$coefficients[, "se(coef)"]), tolerance = 1e-12, check.attributes = FALSE)))
swapped_reference_fg <- cmprsk::crr(ftime = competing_data$time, fstatus = competing_data$status, cov1 = reference_design, failcode = 2L, cencode = 0L)
stopifnot(isTRUE(all.equal(swapped_regression$fine_gray$coef_table$B, unname(swapped_reference_fg$coef), tolerance = 1e-12, check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(swapped_regression$fine_gray$coef_table$SE, unname(sqrt(diag(swapped_reference_fg$var))), tolerance = 1e-12, check.attributes = FALSE)))
swapped_reference_cuminc <- cmprsk::cuminc(ftime = competing_data$time, fstatus = competing_data$status, group = competing_data$group, cencode = 0L)
swapped_reference_curve <- survival_cuminc_curve_table(swapped_reference_cuminc, group_present = TRUE)
swapped_reference_at_times <- survival_cif_at_times(swapped_reference_curve, c(0, 5, 10))
engine_target_cif <- swapped_regression$cif_at_times[as.integer(swapped_regression$cif_at_times$CauseCode) == 1L, c("Group", "Time", "CIF"), drop = FALSE]
reference_target_cif <- swapped_reference_at_times[as.integer(swapped_reference_at_times$CauseCode) == 2L, c("Group", "Time", "CIF"), drop = FALSE]
engine_target_cif <- engine_target_cif[order(engine_target_cif$Group, engine_target_cif$Time), , drop = FALSE]
reference_target_cif <- reference_target_cif[order(reference_target_cif$Group, reference_target_cif$Time), , drop = FALSE]
stopifnot(isTRUE(all.equal(engine_target_cif$CIF, reference_target_cif$CIF, tolerance = 1e-12, check.attributes = FALSE)))

multiple_interest_error <- tryCatch(
  prepare_competing_risk_result(competing_data, "time", "status", event_map = data.frame(raw_value = c("0", "1", "2"), role = c("censored", "event_of_interest", "event_of_interest"), label = c("Censored", "First", "Second"))),
  error = identity
)
stopifnot(inherits(multiple_interest_error, "error"), grepl("exactly one event-of-interest", conditionMessage(multiple_interest_error), fixed = TRUE))
regression_html <- htmltools::renderTags(survival_competing_results_panel(competing_regression, language = "en"))$html
stopifnot(grepl("4. Cause-specific Cox regression", regression_html, fixed = TRUE))
stopifnot(grepl("5. Fine-Gray regression", regression_html, fixed = TRUE))
stopifnot(grepl("6. Cumulative incidence curves", regression_html, fixed = TRUE))
stopifnot(grepl("not labeled HR", regression_html, fixed = TRUE))
stopifnot(grepl("survival_cause_specific_ph_plot", regression_html, fixed = TRUE))
stopifnot(grepl("survival_cause_specific_functional_plot", regression_html, fixed = TRUE))
stopifnot(grepl("survival_fine_gray_residual_plot", regression_html, fixed = TRUE))
stopifnot(grepl("Cause-specific overall model tests", regression_html, fixed = TRUE))
stopifnot(grepl("Fine-Gray overall model test", regression_html, fixed = TRUE))
stopifnot(grepl("Cause-specific categorical-predictor omnibus tests", regression_html, fixed = TRUE))
stopifnot(grepl("Fine-Gray categorical-predictor omnibus tests", regression_html, fixed = TRUE))
stopifnot(grepl("Categorical reference levels and contrast coding", regression_html, fixed = TRUE))
stopifnot(grepl("regardless of the session-wide R contrasts option", regression_html, fixed = TRUE))
stopifnot(grepl("I (reference)", regression_html, fixed = TRUE))
stopifnot(grepl("raw engine coefficient table is retained in the audit export", regression_html, fixed = TRUE))
stopifnot(grepl("not model-fit indices", regression_html, fixed = TRUE))
stopifnot(grepl("not a conventional likelihood fit index", regression_html, fixed = TRUE))
stopifnot(grepl("Fine-Gray numerical stability", regression_html, fixed = TRUE))
stopifnot(grepl("Fine-Gray design-matrix collinearity review", regression_html, fixed = TRUE))
stopifnot(grepl("not a formal cox.zph-equivalent test", regression_html, fixed = TRUE))
competing_saved_html <- saved_survival_results_html(competing_regression, language = "en")
stopifnot(grepl("StatEdu Studio Competing-Risks Results", competing_saved_html, fixed = TRUE))
stopifnot(grepl("data:image/png;base64,", competing_saved_html, fixed = TRUE))
stopifnot(grepl("I (reference)", competing_saved_html, fixed = TRUE))
competing_saved_document <- xml2::read_html(competing_saved_html)
stopifnot(length(xml2::xml_find_all(competing_saved_document, "//*[contains(concat(' ', normalize-space(@class), ' '), ' shiny-plot-output ')]")) == 0L)
competing_saved_tables <- survival_excel_result_tables(competing_regression, language = "en")
stopifnot(length(competing_saved_tables) >= 10L)
stopifnot(any(vapply(competing_saved_tables, function(item) grepl("Cause-specific Cox regression", item$title, fixed = TRUE), logical(1))))
stopifnot(any(vapply(competing_saved_tables, function(item) grepl("Fine-Gray regression", item$title, fixed = TRUE), logical(1))))
fine_gray_audit_dir <- tempfile("fine-gray-audit-")
dir.create(fine_gray_audit_dir)
fine_gray_audit_files <- save_survival_reporting_files(competing_regression, fine_gray_audit_dir, "en")
stopifnot(any(grepl("Fine_Gray_Schoenfeld_like_residuals.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Fine_Gray_proportionality_screen.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Cause_specific_residual_summary.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Cause_specific_model_tests.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Cause_specific_categorical_joint_tests.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Cause_specific_functional_form_data.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Cause_specific_influence.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Cause_specific_collinearity.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Fine_Gray_optimizer_diagnostics.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Fine_Gray_collinearity.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Fine_Gray_model_tests.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Fine_Gray_categorical_joint_tests.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Competing_regression_categorical_reference_levels.csv", fine_gray_audit_files, fixed = TRUE)))
stopifnot(any(grepl("Competing_regression_coefficient_labels.csv", fine_gray_audit_files, fixed = TRUE)))
survival_server_source <- paste(readLines(file.path(repo_root, "R", "server_survival.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (prefix in c("km", "cox", "competing")) {
  stopifnot(grepl(paste0("save_survival_", prefix, "_html_dialog"), survival_server_source, fixed = TRUE))
  stopifnot(grepl(paste0("save_survival_", prefix, "_pdf_dialog"), survival_server_source, fixed = TRUE))
  stopifnot(grepl(paste0("save_survival_", prefix, "_excel_dialog"), survival_server_source, fixed = TRUE))
  stopifnot(grepl(paste0("add_survival_", prefix, "_result"), survival_server_source, fixed = TRUE))
}
fine_gray_figure_dir <- tempfile("fine-gray-figures-")
dir.create(fine_gray_figure_dir)
fine_gray_figure_files <- save_survival_competing_figure_files(competing_regression, fine_gray_figure_dir, dpi = 72)
stopifnot(length(fine_gray_figure_files) == 4L, all(file.exists(fine_gray_figure_files)), all(file.info(fine_gray_figure_files)$size > 0))
cause_specific_diagnostic_probe <- competing_regression
cause_specific_diagnostic_probe$cause_specific$ph_table$p[[1]] <- .01
cause_specific_diagnostic_probe$cause_specific$influence_table$`Review signal`[[1]] <- TRUE
cause_specific_diagnostic_probe$cause_specific$collinearity_table$VIF[[1]] <- 12
cause_specific_diagnostic_probe$cause_specific$condition_number <- 40
cause_specific_probe_codes <- survival_stability_review(cause_specific_diagnostic_probe, "en")$Code
stopifnot(all(c("cause_specific_ph_signal", "cause_specific_dfbetas_signal", "cause_specific_high_vif", "cause_specific_high_condition_number") %in% cause_specific_probe_codes))
cause_specific_elevated_vif_probe <- competing_regression
cause_specific_elevated_vif_probe$cause_specific$collinearity_table$VIF[] <- 6
stopifnot("cause_specific_elevated_vif" %in% survival_stability_review(cause_specific_elevated_vif_probe, "en")$Code)
cause_specific_nonfinite_vif_probe <- competing_regression
cause_specific_nonfinite_vif_probe$cause_specific$collinearity_table$VIF[[1]] <- Inf
stopifnot("cause_specific_nonfinite_vif" %in% survival_stability_review(cause_specific_nonfinite_vif_probe, "en")$Code)
fine_gray_numerical_probe <- competing_regression
fine_gray_numerical_probe$fine_gray$collinearity_table$VIF[[1]] <- 12
fine_gray_numerical_probe$fine_gray$condition_number <- 40
fine_gray_numerical_probe$fine_gray$optimizer_table$`Relative score criterion`[[1]] <- 2e-6
fine_gray_numerical_probe$fine_gray$optimizer_table$`Information rank`[[1]] <- 1L
fine_gray_numerical_probe$fine_gray$optimizer_table$`Standardized information condition number`[[1]] <- 40
fine_gray_numerical_probe$fine_gray$optimizer_table$`Finite covariance matrix`[[1]] <- FALSE
fine_gray_numerical_codes <- survival_stability_review(fine_gray_numerical_probe, "en")$Code
stopifnot(all(c("fine_gray_high_vif", "fine_gray_high_condition_number", "fine_gray_score_criterion_not_met", "fine_gray_rank_deficient_information", "fine_gray_ill_conditioned_information", "fine_gray_invalid_covariance") %in% fine_gray_numerical_codes))
fine_gray_elevated_vif_probe <- competing_regression
fine_gray_elevated_vif_probe$fine_gray$collinearity_table$VIF[] <- 6
stopifnot("fine_gray_elevated_vif" %in% survival_stability_review(fine_gray_elevated_vif_probe, "en")$Code)
fine_gray_nonfinite_vif_probe <- competing_regression
fine_gray_nonfinite_vif_probe$fine_gray$collinearity_table$VIF[[1]] <- Inf
stopifnot("fine_gray_nonfinite_vif" %in% survival_stability_review(fine_gray_nonfinite_vif_probe, "en")$Code)
competing_model_test_probe <- competing_regression
competing_model_test_probe$cause_specific$model_tests$p[[1]] <- NA_real_
competing_model_test_probe$fine_gray$model_tests$Statistic[[1]] <- NA_real_
competing_model_test_codes <- survival_stability_review(competing_model_test_probe, "en")$Code
stopifnot(all(c("cause_specific_model_test_not_estimable", "fine_gray_model_test_not_estimable") %in% competing_model_test_codes))
competing_joint_test_probe <- competing_regression
competing_joint_test_probe$cause_specific$categorical_joint_tests$Estimable[[1]] <- FALSE
competing_joint_test_probe$fine_gray$categorical_joint_tests$Estimable[[1]] <- FALSE
competing_joint_test_codes <- survival_stability_review(competing_joint_test_probe, "en")$Code
stopifnot(all(c("cause_specific_categorical_joint_test_not_estimable", "fine_gray_categorical_joint_test_not_estimable") %in% competing_joint_test_codes))
fine_gray_pattern_probe <- competing_regression
fine_gray_pattern_probe$fine_gray$residual_review$`Review signal`[[1]] <- TRUE
stopifnot("fine_gray_residual_time_pattern" %in% survival_stability_review(fine_gray_pattern_probe, "en")$Code)
fine_gray_limited_probe <- competing_regression
fine_gray_limited_probe$fine_gray$residual_data <- fine_gray_limited_probe$fine_gray$residual_data[fine_gray_limited_probe$fine_gray$residual_data$`Event time` %in% head(unique(fine_gray_limited_probe$fine_gray$residual_data$`Event time`), 3L), , drop = FALSE]
stopifnot("fine_gray_residual_diagnostic_limited" %in% survival_stability_review(fine_gray_limited_probe, "en")$Code)
regression_not_estimable_probe <- competing_regression
regression_not_estimable_probe$cause_specific$estimable <- FALSE
regression_not_estimable_probe$fine_gray$estimable <- FALSE
regression_probe_codes <- survival_stability_review(regression_not_estimable_probe, "en")$Code
stopifnot("cause_specific_cox_not_estimable" %in% regression_probe_codes)
stopifnot("fine_gray_not_estimable" %in% regression_probe_codes)

message("All survival preflight validations passed.")
