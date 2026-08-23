source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

decision_rules <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
engine_audit <- paste(readLines(file.path("docs", "SEM_ENGINE_CODE_AUDIT_2026-08-18_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
installer_checklist <- paste(readLines(file.path("docs", "INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl("새 SEM/CB-SEM 분석의 경로·간접·총효과 bootstrap 기본값은 5,000회", decision_rules, fixed = TRUE),
  grepl("independent_cross_sectional", decision_rules, fixed = TRUE),
  grepl("복합표본·군집·종단 설계를 지원하거나 그 분산을 올바르게 추정했다는 주장이 아니다", decision_rules, fixed = TRUE),
  grepl("알 수 없는 설계 코드는 `not_declared`로 처리해 실행을 차단", decision_rules, fixed = TRUE),
  grepl("버전 간 bootstrap draw와 결과가 bitwise 동일하다고 보장하지 않는다", decision_rules, fixed = TRUE),
  grepl("`Pending`·`Failed`·`Canceled` 상태", decision_rules, fixed = TRUE),
  grepl("2026-08-23 릴리스 정책·Audit schema 후속", engine_audit, fixed = TRUE),
  grepl("Model_Syntax", installer_checklist, fixed = TRUE),
  grepl("Analysis_Record", installer_checklist, fixed = TRUE),
  grepl("R quantile type", installer_checklist, fixed = TRUE)
)

sem_defaults <- structural_canvas_execute_settings(settings = list(), input = list(), prefix = "structural_sem")
cfa_defaults <- structural_canvas_execute_settings(settings = list(), input = list(), prefix = "structural_cfa")
pls_defaults <- structural_canvas_execute_settings(settings = list(), input = list(), prefix = "structural_plssem")
invalid_sampling <- structural_canvas_execute_settings(
  settings = list(sampling_design = "unexpected_design"), input = list(), prefix = "structural_sem"
)
blank_sampling <- structural_canvas_execute_settings(
  settings = list(sampling_design = ""), input = list(), prefix = "structural_sem"
)
invalid_sampling_error <- tryCatch({
  structural_canvas_sampling_design_gate(invalid_sampling$sampling_design)
  ""
}, error = function(error) conditionMessage(error))
blank_sampling_error <- tryCatch({
  structural_canvas_sampling_design_gate(blank_sampling$sampling_design)
  ""
}, error = function(error) conditionMessage(error))
stopifnot(
  identical(sem_defaults$sampling_design, "independent_cross_sectional"),
  identical(cfa_defaults$sampling_design, "independent_cross_sectional"),
  identical(pls_defaults$sampling_design, "independent_cross_sectional"),
  identical(invalid_sampling$sampling_design, "not_declared"),
  grepl("Sampling-design gate blocked estimation", invalid_sampling_error, fixed = TRUE),
  identical(blank_sampling$sampling_design, "not_declared"),
  grepl("Sampling-design gate blocked estimation", blank_sampling_error, fixed = TRUE),
  identical(sem_defaults$effect_bootstrap, 5000L),
  identical(cfa_defaults$effect_bootstrap, 0L),
  identical(pls_defaults$effect_bootstrap, 0L)
)

null_pls_bundle <- list(
  fit = NULL, estimator = "PLS", diagnostics = list(estimator = "PLS"),
  pls_bootstrap = 100L, pls_seed = 24680L, pls_bootstrap_result = NULL
)
null_pls_reporting_label <- structural_canvas_reporting_bootstrap_label(null_pls_bundle, "plssem")
null_pls_fit_ui <- as.character(structural_canvas_fit_table_result_ui(
  null_pls_bundle,
  data.frame(Path = "A → B", B = ".250", `Boot SE` = "", z = "", p = "", check.names = FALSE)
))
no_bootstrap_bundle <- null_pls_bundle
no_bootstrap_bundle$pls_bootstrap <- 0L
no_bootstrap_fit_ui <- as.character(structural_canvas_fit_table_result_ui(
  no_bootstrap_bundle,
  data.frame(Path = "A → B", B = ".250", `Boot SE` = "", z = "", p = "", check.names = FALSE)
))
requested_fallback_pattern <- "bootstrap$requested_nboot %||% bundle$pls_bootstrap %||% 0L"
fit_render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
measurement_render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
validity_render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_validity.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("valid=0/100", null_pls_reporting_label, fixed = TRUE),
  grepl("status=Not recorded", null_pls_reporting_label, fixed = TRUE),
  grepl("RNG=L'Ecuyer-CMRG independent stream per requested position", null_pls_reporting_label, fixed = TRUE),
  grepl("structural-result-warning", null_pls_fit_ui, fixed = TRUE),
  grepl("Valid resamples: 0/100", null_pls_fit_ui, fixed = TRUE),
  grepl("Bootstrap SE, CI, t, and p values are suppressed", null_pls_fit_ui, fixed = TRUE),
  identical(structural_canvas_reporting_bootstrap_label(no_bootstrap_bundle, "plssem"), "Not requested"),
  !grepl("structural-result-warning", no_bootstrap_fit_ui, fixed = TRUE),
  grepl(requested_fallback_pattern, fit_render_source, fixed = TRUE),
  grepl(requested_fallback_pattern, measurement_render_source, fixed = TRUE),
  grepl(requested_fallback_pattern, validity_render_source, fixed = TRUE)
)

reference_values <- c(-1.2, -0.3, 0.1, 0.4, 0.9, 1.7, 2.4)
stopifnot(
  isTRUE(all.equal(
    bootstrap_percentile_ci(reference_values),
    as.numeric(stats::quantile(reference_values, c(.025, .975), type = 6, names = FALSE))
  )),
  identical(structural_canvas_bootstrap_quantile_type("bias_corrected", "structural_effects"), 6L),
  identical(structural_canvas_bootstrap_ci_method("Bias-corrected (BC)"), "bias_corrected"),
  identical(structural_canvas_bootstrap_quantile_type("percentile", "structural_effects"), 6L),
  identical(structural_canvas_bootstrap_quantile_type("percentile", "htmt"), 6L),
  identical(structural_canvas_bootstrap_quantile_type("percentile", "reliability"), 7L),
  identical(structural_canvas_bootstrap_quantile_type("bias_corrected", "reliability"), 6L),
  identical(structural_canvas_bootstrap_quantile_type("bca", "reliability"), 6L)
)

set.seed(20260823)
n <- 120L
eta <- stats::rnorm(n)
analysis_data <- data.frame(
  x1 = .8 * eta + stats::rnorm(n, sd = .5),
  x2 = .7 * eta + stats::rnorm(n, sd = .6),
  x3 = .9 * eta + stats::rnorm(n, sd = .4)
)
syntax <- "eta1 =~ x1 + x2 + x3"
fit <- lavaan::cfa(syntax, data = analysis_data, auto.cov.lv.x = FALSE)
bundle <- list(
  fit = fit,
  syntax = syntax,
  snapshot = list(nodes = list(), edges = list()),
  diagnostics = list(converged = TRUE, admissible = TRUE),
  analysis_type = "cbsem",
  analysis_data = analysis_data,
  estimator = "ML",
  missing = "listwise",
  sampling_design = "independent_cross_sectional",
  sampling_design_gate = structural_canvas_sampling_design_gate("independent_cross_sectional"),
  reliability_bootstrap = 1000L,
  reliability_seed = 111L,
  reliability_ci_method = "percentile",
  htmt_bootstrap = 1000L,
  htmt_seed = 222L,
  htmt_ci_method = "bias_corrected",
  effect_bootstrap = 5000L,
  effect_bootstrap_seed = 333L,
  effect_bootstrap_ci_method = "bias_corrected"
)

audit <- structural_canvas_audit_manifest(bundle, "cbsem", as.POSIXct("2026-08-23 12:00:00", tz = "Asia/Seoul"))
record <- structural_canvas_reproducibility_record(bundle, as.POSIXct("2026-08-23 12:00:00", tz = "Asia/Seoul"))
reporting_label <- structural_canvas_reporting_bootstrap_label(bundle, "cbsem")
stopifnot(
  identical(audit$schema$version, "1.7"),
  identical(audit$resampling$reliability$ci_method, "percentile"),
  identical(audit$resampling$reliability$quantile_type, 7L),
  identical(audit$resampling$htmt$quantile_type, 6L),
  identical(audit$resampling$structural_effects$ci_method, "bias_corrected"),
  identical(audit$resampling$structural_effects$quantile_type, 6L),
  grepl("R quantile type is recorded separately", audit$resampling$reproducibility_policy$quantile_definition, fixed = TRUE),
  grepl("AVE/reliability bootstrap: 1000; seed: 111; CI method: percentile; quantile type: R type 7", record, fixed = TRUE),
  grepl("HTMT bootstrap: 1000; seed: 222; CI method: bias_corrected; quantile type: R type 6", record, fixed = TRUE),
  grepl("Path/indirect/total-effect bootstrap: 5000; seed: 333; CI method: bias_corrected; quantile type: R type 6", record, fixed = TRUE),
  grepl("Reliability/AVE R=1000, CI=percentile, quantile=R type 7", reporting_label, fixed = TRUE),
  grepl("Path/indirect/total-effect R=5000, CI=bias_corrected, quantile=R type 6", reporting_label, fixed = TRUE)
)

pls_fingerprint_functions <- c(
  "structural_canvas_execute_settings", "structural_canvas_sampling_design_gate",
  "structural_canvas_bootstrap_quantile_type",
  "structural_canvas_rng_streams", "structural_canvas_run_pls_analysis", "structural_canvas_apply_plsc",
  "structural_canvas_run_pls_bootstrap",
  "structural_canvas_run_plsc_bootstrap", "structural_canvas_pls_bootstrap_min_valid_ratio",
  "structural_canvas_pls_bootstrap_validity", "structural_canvas_pls_bootstrap_required_masks",
  "structural_canvas_pls_bootstrap_components_contract", "structural_canvas_pls_bootstrap_select_draws",
  "structural_canvas_pls_bootstrap_suppress_inference", "structural_canvas_pls_bootstrap_contract_metadata",
  "structural_canvas_pls_bootstrap_unavailable_result"
)
pls_fingerprint_before <- structural_canvas_analysis_code_fingerprint()
original_min_valid_ratio <- structural_canvas_pls_bootstrap_min_valid_ratio
assign("structural_canvas_pls_bootstrap_min_valid_ratio", function() .81, envir = .GlobalEnv)
pls_fingerprint_after <- structural_canvas_analysis_code_fingerprint()
assign("structural_canvas_pls_bootstrap_min_valid_ratio", original_min_valid_ratio, envir = .GlobalEnv)
stopifnot(
  all(pls_fingerprint_functions %in% pls_fingerprint_before$included_functions),
  is.character(pls_fingerprint_before$sha256),
  nzchar(pls_fingerprint_before$sha256),
  !identical(pls_fingerprint_before$sha256, pls_fingerprint_after$sha256)
)

pls_audit_bundle <- bundle
pls_audit_bundle$fit <- NULL
pls_audit_bundle$analysis_type <- "plssem"
pls_audit_bundle$estimator <- "PLS"
pls_audit_bundle$pls_bootstrap <- 100L
pls_audit_bundle$pls_seed <- 24680L
pls_audit_bundle$pls_bootstrap_result <- list(
  nboot = 77L,
  requested_nboot = 100L,
  valid_ratio = .77,
  minimum_valid_ratio = .80,
  inference_available = FALSE,
  bootstrap_status = "Insufficient",
  timeout_failures = 3L,
  estimation_failures = 5L,
  invalid_statistic_failures = 15L,
  execution_failures = 0L,
  canceled_failures = 0L,
  valid_positions = c(1:40, 42:78),
  rng = "L'Ecuyer-CMRG independent stream per requested position",
  draw_order = "requested-position order; invalid draws removed without reordering valid draws",
  validity_contract = "all required PLS statistics finite with matching dimensions, names, and structural missingness"
)
pls_audit <- structural_canvas_audit_manifest(
  pls_audit_bundle, "plssem", as.POSIXct("2026-08-23 12:30:00", tz = "Asia/Seoul")
)
pls_resampling_warning <- pls_audit$warnings[
  pls_audit$warnings$Category == "Resampling" &
    grepl("PLS bootstrap inference is unavailable", pls_audit$warnings$Message, fixed = TRUE),
  , drop = FALSE
]
stopifnot(
  nrow(pls_resampling_warning) == 1L,
  identical(pls_resampling_warning$Severity[[1L]], "Critical"),
  grepl("valid 77/100", pls_resampling_warning$Message[[1L]], fixed = TRUE),
  grepl("timeout=3", pls_resampling_warning$Message[[1L]], fixed = TRUE),
  grepl("estimation=5", pls_resampling_warning$Message[[1L]], fixed = TRUE),
  grepl("invalid-statistics=15", pls_resampling_warning$Message[[1L]], fixed = TRUE),
  identical(pls_audit$resampling$pls$requested_replicates, 100L),
  identical(pls_audit$resampling$pls$valid_replicates, 77L),
  identical(pls_audit$resampling$pls$valid_positions, c(1:40, 42:78)),
  identical(pls_audit$resampling$pls$failure_counts$timeout, 3L),
  identical(pls_audit$resampling$pls$failure_counts$estimation, 5L),
  identical(pls_audit$resampling$pls$failure_counts$invalid_statistics, 15L),
  identical(pls_audit$resampling$pls$failure_counts$execution, 0L),
  identical(pls_audit$resampling$pls$failure_counts$canceled, 0L),
  identical(pls_audit$resampling$pls$rng, "L'Ecuyer-CMRG independent stream per requested position"),
  grepl("requested-position order", pls_audit$resampling$pls$draw_order, fixed = TRUE)
)

pending_pls_bundle <- pls_audit_bundle
pending_pls_bundle$pls_bootstrap_result <- structural_canvas_pls_bootstrap_unavailable_result(
  100L, 24680L, status = "Pending", failure_message = "Background bootstrap is running."
)
pending_pls_audit <- structural_canvas_audit_manifest(
  pending_pls_bundle, "plssem", as.POSIXct("2026-08-23 12:32:00", tz = "Asia/Seoul")
)
pending_pls_warning <- pending_pls_audit$warnings[
  pending_pls_audit$warnings$Category == "Resampling" &
    grepl("status Pending", pending_pls_audit$warnings$Message, fixed = TRUE),
  , drop = FALSE
]
stopifnot(
  nrow(pending_pls_warning) == 1L,
  identical(pending_pls_warning$Severity[[1L]], "Critical"),
  grepl("valid 0/100", pending_pls_warning$Message[[1L]], fixed = TRUE),
  identical(pending_pls_audit$resampling$pls$failure_counts$execution, 0L),
  identical(pending_pls_audit$resampling$pls$failure_counts$canceled, 0L)
)

failed_pls_bundle <- pls_audit_bundle
failed_pls_bundle$pls_bootstrap_result <- structural_canvas_pls_bootstrap_unavailable_result(
  100L, 24680L, status = "Failed", failure_message = "Worker process failed."
)
failed_pls_audit <- structural_canvas_audit_manifest(
  failed_pls_bundle, "plssem", as.POSIXct("2026-08-23 12:35:00", tz = "Asia/Seoul")
)
failed_pls_warning <- failed_pls_audit$warnings[
  failed_pls_audit$warnings$Category == "Resampling" &
    grepl("status Failed", failed_pls_audit$warnings$Message, fixed = TRUE),
  , drop = FALSE
]
stopifnot(
  nrow(failed_pls_warning) == 1L,
  identical(failed_pls_warning$Severity[[1L]], "Critical"),
  grepl("valid 0/100", failed_pls_warning$Message[[1L]], fixed = TRUE),
  grepl("execution=1", failed_pls_warning$Message[[1L]], fixed = TRUE),
  grepl("Worker process failed", failed_pls_warning$Message[[1L]], fixed = TRUE),
  identical(failed_pls_audit$resampling$pls$failure_counts$execution, 1L),
  identical(failed_pls_audit$resampling$pls$failure_counts$canceled, 0L)
)

canceled_pls_bundle <- pls_audit_bundle
canceled_pls_bundle$pls_bootstrap_result <- structural_canvas_pls_bootstrap_unavailable_result(
  100L, 24680L, status = "Canceled", failure_message = "Canceled by user"
)
canceled_pls_audit <- structural_canvas_audit_manifest(
  canceled_pls_bundle, "plssem", as.POSIXct("2026-08-23 12:37:00", tz = "Asia/Seoul")
)
canceled_pls_warning <- canceled_pls_audit$warnings[
  canceled_pls_audit$warnings$Category == "Resampling" &
    grepl("status Canceled", canceled_pls_audit$warnings$Message, fixed = TRUE),
  , drop = FALSE
]
stopifnot(
  nrow(canceled_pls_warning) == 1L,
  identical(canceled_pls_warning$Severity[[1L]], "Critical"),
  grepl("valid 0/100", canceled_pls_warning$Message[[1L]], fixed = TRUE),
  grepl("canceled=1", canceled_pls_warning$Message[[1L]], fixed = TRUE),
  grepl("Canceled by user", canceled_pls_warning$Message[[1L]], fixed = TRUE),
  identical(canceled_pls_audit$resampling$pls$failure_counts$execution, 0L),
  identical(canceled_pls_audit$resampling$pls$failure_counts$canceled, 1L)
)

missing_pls_bundle <- pls_audit_bundle
missing_pls_bundle$pls_bootstrap_result <- NULL
missing_pls_audit <- structural_canvas_audit_manifest(
  missing_pls_bundle, "plssem", as.POSIXct("2026-08-23 12:40:00", tz = "Asia/Seoul")
)
missing_pls_warning <- missing_pls_audit$warnings[
  missing_pls_audit$warnings$Category == "Resampling" &
    grepl("no bootstrap result contract", missing_pls_audit$warnings$Message, fixed = TRUE),
  , drop = FALSE
]
stopifnot(
  nrow(missing_pls_warning) == 1L,
  identical(missing_pls_warning$Severity[[1L]], "Critical"),
  grepl("valid 0/100", missing_pls_warning$Message[[1L]], fixed = TRUE),
  identical(missing_pls_audit$resampling$pls$rng, "L'Ecuyer-CMRG independent stream per requested position")
)

ci_metadata <- data.frame(ci_method = "bias_corrected", quantile_type = 6L, stringsAsFactors = FALSE)
stopifnot(identical(
  structural_canvas_effect_bootstrap_ci_label(ci_metadata),
  "Bootstrap bias-corrected (BC) 95% CI (R quantile type 6)"
))

cat("SEM policy and bootstrap quantile metadata validations passed.\n")
