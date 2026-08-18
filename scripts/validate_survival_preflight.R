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

message("Checking explicit event-map preservation in all engines...")
mapped_exec <- data.frame(time = c(2, 4, 6, 8, 10), status = c(0, 1, 2, 9, 1), x = c(0, 1, 0, 1, 0))
exec_map <- data.frame(raw_value = c("0", "1", "2", "9"), role = c("censored", "event_of_interest", "competing_event", "exclude"), label = c("Censored", "Target", "Other cause", "Protocol exclusion"), stringsAsFactors = FALSE)
mapped_km <- prepare_km_analysis_result(mapped_exec, "time", "status", event_map = exec_map)
stopifnot(mapped_km$n == 4L, mapped_km$preflight$counts$competing_events == 1L, grepl("excluded_event_code", mapped_km$preflight$row_audit$exclusion_reasons[[4]], fixed = TRUE))
mapped_cox <- suppressWarnings(prepare_cox_analysis_result(mapped_exec, "time", "status", "x", event_map = exec_map))
stopifnot(mapped_cox$n == 4L, mapped_cox$preflight$counts$competing_events == 1L)
mapped_cr <- prepare_competing_risk_result(mapped_exec, "time", "status", event_map = exec_map)
stopifnot(mapped_cr$n == 4L, mapped_cr$preflight$counts$competing_events == 1L)
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
  c("time", "status", "arm", "age", "stage"),
  values = list(
    time = transfer_both$time,
    event = transfer_both$event,
    group = transfer_both$group,
    censored_value = transfer_both$censored_value,
    interest_value = transfer_both$event_value,
    competing_values = transfer_both$competing_values,
    regression = transfer_both$regression,
    covariates = transfer_both$covariates
  ),
  language = "en"
))$html
stopifnot(grepl('value="time" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="status" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="arm" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="both" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="age" selected', competing_setup_html, fixed = TRUE))
stopifnot(grepl('value="stage" selected', competing_setup_html, fixed = TRUE))
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

message("Checking explicit competing-event mapping...")
competing <- data.frame(time = c(2, 4, 6, 8, 10), status = c(0, 1, 2, 1, 2))
competing_settings <- survival_legacy_settings("time", "status", "1")
competing_settings$legacy <- FALSE
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
stopifnot(cox_fit$parameter_count == 1L)
stopifnot(is.finite(cox_fit$events_per_parameter))
stopifnot(nrow(cox_fit$model_tests) == 3L)
stopifnot(identical(cox_fit$model_tests$Test, c("Likelihood-ratio", "Wald", "Score")))
stopifnot(all(is.finite(cox_fit$model_tests$Statistic)))
stopifnot(nrow(cox_fit$residual_table) == 2L)
stopifnot(nrow(cox_fit$influence_table) == 1L)
stopifnot(nrow(survival_cox_model_test_table(cox_fit)) == 3L)
stopifnot(nrow(survival_ph_review_table(cox_fit, "en")) > 0L)
stopifnot(nrow(survival_cox_residual_table(cox_fit)) == 2L)
stopifnot(nrow(survival_cox_influence_table(cox_fit)) == 1L)
cox_html <- htmltools::renderTags(survival_cox_results_panel(cox_fit, language = "en"))$html
stopifnot(grepl("Reporting checklist and interpretation guide", cox_html, fixed = TRUE), grepl("not automatically a causal", cox_html, fixed = TRUE))
stopifnot(grepl("Analysis stability review", cox_html, fixed = TRUE), "low_events_per_parameter" %in% survival_stability_review(delayed_cox, "en")$Code)
stopifnot(grepl("3. Supplementary statistics and diagnostics", cox_html, fixed = TRUE))
stopifnot(grepl("Overall model tests", cox_html, fixed = TRUE))
stopifnot(grepl("proportional-hazards assumption", cox_html, fixed = TRUE))
stopifnot(grepl("4. Residual distribution review", cox_html, fixed = TRUE))
stopifnot(grepl("5. Influence review", cox_html, fixed = TRUE))
stopifnot(grepl("not automatic pass/fail criteria", cox_html, fixed = TRUE))
stopifnot(grepl("survival-cox-summary-row", cox_html, fixed = TRUE))
stopifnot(!grepl("survival-cox-model-subsection", cox_html, fixed = TRUE))

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
  adjusted_bootstrap_reps = 40L
)
adjusted <- adjusted_fit$adjusted_survival
stopifnot(is.list(adjusted))
stopifnot(identical(adjusted$method, "Marginal standardization"))
stopifnot(identical(sort(unique(adjusted$curve$Level)), c("A", "B")))
stopifnot(all(adjusted$curve$Survival >= 0 & adjusted$curve$Survival <= 1))
stopifnot(all(adjusted$curve$Survival[adjusted$curve$Time == 0] == 1))
stopifnot(adjusted$bootstrap_successful >= 20L)
stopifnot(all(is.finite(adjusted$curve$Lower)))
stopifnot(all(is.finite(adjusted$curve$Upper)))
stopifnot(inherits(survival_adjusted_survival_ggplot(adjusted_fit), "ggplot"))
adjusted_html <- htmltools::renderTags(survival_cox_results_panel(adjusted_fit, language = "en"))$html
stopifnot(grepl("6. Marginal adjusted survival", adjusted_html, fixed = TRUE))
stopifnot(grepl("not a curve for one mean-covariate subject", adjusted_html, fixed = TRUE))

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
stopifnot(nrow(competing_result$gray_tests) == 2L)
stopifnot(all(is.finite(competing_result$gray_tests$Statistic)))
stopifnot(all(is.finite(competing_result$gray_tests$p)))
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
stopifnot(nrow(competing_regression$cause_specific$coef_table) == 2L)
stopifnot(all(competing_regression$cause_specific$coef_table$HR > 0))
stopifnot(nrow(competing_regression$cause_specific$ph_table) > 0L)
stopifnot(is.list(competing_regression$fine_gray))
stopifnot(inherits(competing_regression$fine_gray$fit, "crr"))
stopifnot(isTRUE(competing_regression$fine_gray$converged))
stopifnot(nrow(competing_regression$fine_gray$coef_table) == 2L)
stopifnot(all(competing_regression$fine_gray$coef_table$sHR > 0))
stopifnot(nrow(competing_regression$fine_gray$residual_review) == 2L)
stopifnot("HR" %in% names(survival_cause_specific_coef_table(competing_regression)))
stopifnot("sHR" %in% names(survival_fine_gray_coef_table(competing_regression)))
regression_html <- htmltools::renderTags(survival_competing_results_panel(competing_regression, language = "en"))$html
stopifnot(grepl("4. Cause-specific Cox regression", regression_html, fixed = TRUE))
stopifnot(grepl("5. Fine-Gray regression", regression_html, fixed = TRUE))
stopifnot(grepl("6. Cumulative incidence curves", regression_html, fixed = TRUE))
stopifnot(grepl("not labeled HR", regression_html, fixed = TRUE))

message("All survival preflight validations passed.")
