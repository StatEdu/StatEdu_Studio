if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_installer_regression_gate.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

require_contains <- function(text, pattern, label) {
  if (!grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("Installer regression gate contract missing: %s", label), call. = FALSE)
  }
}

require_absent <- function(text, pattern, label) {
  if (grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("Installer regression gate bypass detected: %s", label), call. = FALSE)
  }
}

gate <- read_text("scripts/validate_installer_regressions.ps1")
data_upload_performance <- read_text("scripts/validate_data_upload_performance.R")
runtime_regression <- read_text("scripts/validate_mediation_moderation_runtime.R")
structural_bootstrap_regression <- read_text("scripts/validate_structural_bootstrap_performance.R")
pls_bootstrap_engine <- read_text("R/setup_custom_model_canvas_structural_pls_engine.R")
structural_core <- read_text("R/setup_custom_model_canvas_structural_core.R")
structural_settings <- read_text("R/setup_custom_model_canvas_structural_execute_settings.R")
structural_handlers <- read_text("R/setup_custom_model_canvas_structural_handlers.R")
audit_export <- read_text("R/setup_custom_model_canvas_export_report.R")
pls_bootstrap_contract <- read_text("scripts/validate_pls_bootstrap_contract.R")
cfa_ui_regression <- read_text("scripts/validate_cfa_ui.R")
sem_policy_metadata <- read_text("scripts/validate_sem_policy_metadata.R")
sem_canvas_regression <- read_text("scripts/validate_sem_canvas.R")
csem_regression <- read_text("scripts/validate_pls_fit_csem.R")
smartpls_private_evidence <- read_text("scripts/validate_pls_smartpls_private_evidence.R")
smartpls_evidence_finalizer <- read_text("scripts/finalize_pls_external_evidence.R")
stabilization <- read_text("scripts/validate_stabilization.ps1")
preflight <- read_text("scripts/release_preflight.ps1")
build <- read_text("scripts/build_electron_beta.ps1")
release_checklist <- read_text("docs/RELEASE_CHECKLIST.md")
manual_qa <- read_text("docs/RELEASE_MANUAL_QA.md")
promotion <- read_text("docs/RELEASE_1_2_3_PROMOTION_CHECKLIST.md")
packaged_notes <- read_text("docs/RELEASE_1_2_3_PACKAGED_VALIDATION_NOTES.md")
manual_record <- read_text("docs/RELEASE_1_2_3_MANUAL_QA_RECORD.md")
dev_packaged_notes <- read_text("docs/RELEASE_1_2_4_DEV_PACKAGED_VALIDATION_NOTES.md")
dev_manual_record <- read_text("docs/RELEASE_1_2_4_DEV_MANUAL_QA_RECORD.md")
regression_checklist <- read_text("docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md")

message("Checking the focused installer regression suite...")
required_validations <- c(
  "scripts\\validate_installer_regression_gate.R",
  "scripts\\validate_data_io.R",
  "scripts\\validate_data_upload_performance.R",
  "scripts\\validate_startup_performance_contract.R",
  "scripts\\validate_custom_model_canvas.R",
  "scripts\\validate_cfa_ui.R",
  "scripts\\validate_sem_policy_metadata.R",
  "scripts\\validate_sem_canvas.R",
  "scripts\\validate_pls_fit_csem.R",
  "scripts\\validate_pls_smartpls_private_evidence.R",
  "scripts\\validate_pls_bootstrap_contract.R",
  "scripts\\validate_structural_bootstrap_performance.R",
  "scripts\\validate_mediation_moderation.R",
  "scripts\\validate_mediation_moderation_runtime.R",
  "scripts\\validate_ui_layout_contract.R",
  "scripts\\validate_release_hygiene.R"
)
for (validation in required_validations) {
  require_contains(gate, validation, sprintf("focused validation %s", validation))
}
require_contains(gate, "git diff --check", "whitespace/error precheck")
require_contains(gate, "Installer regression gate passed.", "explicit success marker")
require_absent(gate, "SkipInstallerRegression", "focused gate skip switch")
for (pattern in c(
  'warmup <- run_temp_upload_resolution("Temp upload cold-control sample", timed = TRUE)',
  "timed_runs <- lapply(seq_len(9L)",
  "median_elapsed <- stats::median(timed_samples)",
  "all_samples <- c(warmup$elapsed, timed_samples)",
  "maximum_elapsed <- max(all_samples)",
  "diagnostic_tail_seconds <- 10",
  "operational_ceiling_seconds <- 30",
  "any(all_samples >= operational_ceiling_seconds)",
  "any(all_samples >= diagnostic_tail_seconds)",
  "DIAGNOSTIC WARNING:",
  "median_elapsed >= 2",
  "if (!identical(scans, 0L))"
)) {
  require_contains(
    data_upload_performance,
    pattern,
    sprintf("data-upload performance sampling contract: %s", pattern)
  )
}
for (contract in c(
  "cold-control 준비 표본 1회",
  "연속 timed 표본 9회",
  "중앙값 2초 미만",
  "각 실행 재귀 검색 0회",
  "10초 이상은 진단 경고",
  "30초 이상은 운영 실행시간 상한 위반으로 차단",
  "성능 SLA가 아니다"
)) {
  require_contains(
    regression_checklist,
    contract,
    sprintf("documented data-upload performance contract: %s", contract)
  )
}
require_absent(
  data_upload_performance,
  "warning(",
  "data-upload diagnostic message must remain non-blocking under options(warn = 2)"
)
for (pattern in c(
  'identical(audit$schema$version, "1.7")',
  'identical(audit$resampling$reliability$quantile_type, 7L)',
  'identical(audit$resampling$structural_effects$quantile_type, 6L)',
  'identical(sem_defaults$effect_bootstrap, 5000L)',
  'identical(sem_defaults$sampling_design, "independent_cross_sectional")'
)) {
  require_contains(sem_policy_metadata, pattern, sprintf("SEM policy/metadata regression: %s", pattern))
}
for (duplicate_validation in c(
  "scripts\\validate_complex_sample_custom_model.R",
  "scripts\\validate_cfa_canvas.R",
  "scripts\\validate_cfa_bootstrap.R",
  "scripts\\validate_cfa_reporting_exports.R"
)) {
  require_absent(
    gate,
    duplicate_validation,
    sprintf("full-suite-only validation duplicated in focused gate: %s", duplicate_validation)
  )
}

message("Checking fail-closed structural bootstrap release measurements...")
structural_contracts <- c(
  'Sys.getenv("STATEDU_STRUCTURAL_BOOTSTRAP_MODE", "core")' = "core/installer validation mode",
  "STATEDU_STRUCTURAL_BOOTSTRAP_REPORT is required in installer mode" = "required measured JSON artifact",
  "STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID is required in installer mode" = "fresh installer evidence run identifier",
  "The previous structural bootstrap report could not be invalidated before validation." = "direct-R stale report invalidation",
  'read_budget("STATEDU_STRUCTURAL_BOOTSTRAP_CORE_MAX_SECONDS", 120, 600)' = "bounded core runtime override",
  'read_budget("STATEDU_STRUCTURAL_SEM_MAX_SECONDS", 360, 900)' = "bounded SEM runtime override",
  'read_budget("STATEDU_STRUCTURAL_CFA_MAX_SECONDS", 180, 600)' = "bounded CFA runtime override",
  'read_budget("STATEDU_STRUCTURAL_PLS_MAX_SECONDS", 240, 600)' = "bounded PLS-SEM runtime override",
  'read_budget("STATEDU_STRUCTURAL_PREPARE_MAX_SECONDS", 3, 60)' = "bounded synchronous preparation override",
  'read_budget("STATEDU_STRUCTURAL_FIRST_COMPLETION_MAX_SECONDS", 15, 120)' = "bounded first-completion override",
  'read_budget("STATEDU_STRUCTURAL_SUMMARIZE_MAX_SECONDS", 2, 60)' = "bounded summarization override",
  "value <= 0 || value > maximum" = "invalid or excessive override rejection",
  'moderationMethod = "all_pairs_dmc"' = "representative all-pairs DMC SEM fixture",
  "product_indicator_count != 6L" = "six product-indicator assertion",
  "Prepared SEM raw estimates differ from a full canvas refit." = "raw estimate equivalence",
  "Prepared SEM standardized estimates differ from a full canvas refit." = "standardized estimate equivalence",
  "Prepared SEM strict admissibility disagrees" = "strict admissibility equivalence",
  "progress completed count regressed" = "monotonic completed progress",
  "progress phase regressed" = "monotonic bootstrap phases",
  "Repeated atomic SEM progress writes exposed a partial RDS snapshot." = "atomic progress snapshot readability",
  "A corrupt SEM progress RDS replaced the last valid cached snapshot." = "corrupt progress retention",
  "structural_canvas_effect_bootstrap_progress_merge(previous, corrupted_candidate)" = "cached progress merge after corrupt read",
  "structural_canvas_stop_effect_bootstrap_job(cancellation_job)" = "background bootstrap cancellation",
  "SEM bootstrap cancellation left its temporary job directory behind." = "canceled job cleanup",
  "is not seed-reproducible" = "deterministic seed regression",
  "exactness_run <- run_structural_exactness_gate()" = "mode-independent SEM/CFA exactness execution",
  "SEM two-stage versus unchanged full-SE bootstrap" = "SEM two-stage exact full-SE comparison",
  "sem_product_index_vs_legacy = isTRUE(sem_product_index$legacy_vs_fast)" = "latent-product index versus full-SE legacy evidence",
  "sem_product_index_fail_open = isTRUE(sem_product_index$fail_open)" = "latent-product whole-batch fail-open evidence",
  "sem_product_index_missing_guard = isTRUE(sem_product_index$actual_missing_guard)" = "latent-product actual-missing guard evidence",
  "sem_product_index_single_position = isTRUE(sem_product_index$singleton_full_exercised)" = "latent-product singleton/deferred full evidence",
  "sem_fixed_index_vs_legacy = isTRUE(sem_fixed_index$legacy_vs_fast)" = "nonproduct fixed-index legacy evidence",
  "sem_fixed_index_fail_open = isTRUE(sem_fixed_index$fail_open)" = "nonproduct fixed-index fail-open evidence",
  "sem_fixed_index_normal_default = isTRUE(sem_fixed_index$normal_default_activated)" = "nonproduct default-normal activation evidence",
  "sem_fixed_index_worker_payload_small = isTRUE(sem_fixed_index$worker_payload_small)" = "fixed-index transport-size evidence",
  "CFA legacy, fused serial, and reusable-PSOCK bootstrap paths are not numerically exact." = "CFA legacy/serial/PSOCK exact comparison",
  "metadata_restore = isTRUE(metadata_restore_passed)" = "metadata binding restoration evidence",
  "reliability_bootstrap = 1000L" = "actual CFA 1,000-repetition installer profile",
  "reps = 5000L" = "actual SEM 5,000-repetition installer profile",
  "structural_canvas_start_pls_bootstrap_job(pls_original, 1000L" = "actual PLS-SEM 1,000-repetition installer profile",
  "installer_pls_evidence_from_result <- function(value)" = "PLS product-result evidence mapping",
  "repetitions = scalar_integer(value$requested_nboot)" = "PLS requested-draw evidence mapping",
  "valid_repetitions = scalar_integer(value$nboot)" = "PLS valid-draw evidence mapping",
  "valid_ratio = scalar_number(value$valid_ratio)" = "PLS valid-ratio evidence mapping",
  "minimum_valid_repetitions = scalar_integer(value$minimum_valid_n)" = "PLS minimum-valid evidence mapping",
  "inference_available = isTRUE(value$inference_available)" = "PLS inference-availability evidence mapping",
  "bootstrap_status = scalar_text(value$bootstrap_status)" = "PLS status evidence mapping",
  "installer_pls_whole_draw_verified <- function(pls)" = "PLS whole-draw JSON verifier",
  "!isTRUE(inference_available_pls)" = "actual PLS inference-availability gate",
  "!identical(bootstrap_status_pls, \"Adequate\")" = "actual PLS adequate-status gate",
  "valid_pls < 800L" = "actual PLS 800-valid-draw gate",
  "valid_ratio_pls < .80" = "actual PLS 80-percent valid-ratio gate",
  "!installer_pls_whole_draw_verified(pls_installer_evidence)" = "actual PLS evidence consistency gate",
  "minimum_valid_repetitions = pls_installer_evidence$minimum_valid_repetitions" = "PLS minimum-valid JSON evidence",
  "inference_available = pls_installer_evidence$inference_available" = "PLS inference JSON evidence",
  "bootstrap_status = pls_installer_evidence$bootstrap_status" = "PLS status JSON evidence",
  "jsonlite::write_json(" = "staged measured timing report persistence",
  "file.rename(temporary_path, path)" = "atomic measured timing report publication",
  "jsonlite::read_json(path" = "published measured timing report verification",
  "schema_version = 2L" = "versioned fresh installer evidence",
  "contains a missing or invalid required measurement" = "unmeasured timing blocks release",
  "phase_transition_seconds = phase_transition_seconds" = "observed phase timing evidence"
)
for (pattern in names(structural_contracts)) {
  require_contains(
    structural_bootstrap_regression,
    pattern,
    structural_contracts[[pattern]]
  )
}

structural_expressions <- parse(text = structural_bootstrap_regression, keep.source = FALSE)
extract_assignment <- function(name) {
  matched <- Filter(function(expression) {
    is.call(expression) && identical(as.character(expression[[1L]]), "<-") &&
      identical(as.character(expression[[2L]]), name)
  }, as.list(structural_expressions))
  if (length(matched) != 1L) {
    stop(sprintf("Installer regression gate could not isolate structural helper '%s'.", name), call. = FALSE)
  }
  environment <- new.env(parent = baseenv())
  eval(matched[[1L]], envir = environment)
  get(name, envir = environment, inherits = FALSE)
}
map_pls_evidence <- extract_assignment("installer_pls_evidence_from_result")
verify_pls_evidence <- extract_assignment("installer_pls_whole_draw_verified")
synthetic_product_result <- list(
  requested_nboot = 1000L,
  nboot = 800L,
  valid_ratio = .80,
  minimum_valid_n = 800L,
  minimum_valid_ratio = .80,
  inference_available = TRUE,
  bootstrap_status = "Adequate"
)
synthetic_pls_evidence <- map_pls_evidence(synthetic_product_result)
expected_pls_fields <- c(
  "repetitions", "valid_repetitions", "valid_ratio",
  "minimum_valid_repetitions", "minimum_valid_ratio",
  "inference_available", "bootstrap_status"
)
if (!identical(names(synthetic_pls_evidence), expected_pls_fields) ||
    !isTRUE(verify_pls_evidence(synthetic_pls_evidence))) {
  stop("Installer PLS product fields did not map to adequate whole-draw JSON evidence.", call. = FALSE)
}
invalid_pls_evidence <- list(
  insufficient_n = within(synthetic_pls_evidence, valid_repetitions <- 799L),
  insufficient_ratio = within(synthetic_pls_evidence, valid_ratio <- .799),
  unavailable = within(synthetic_pls_evidence, inference_available <- FALSE),
  insufficient_status = within(synthetic_pls_evidence, bootstrap_status <- "Insufficient"),
  inconsistent_ratio = within(synthetic_pls_evidence, valid_ratio <- .81),
  string_count = within(synthetic_pls_evidence, valid_repetitions <- "800"),
  string_inference = within(synthetic_pls_evidence, inference_available <- "true"),
  missing_ratio = synthetic_pls_evidence[setdiff(names(synthetic_pls_evidence), "valid_ratio")]
)
if (any(vapply(invalid_pls_evidence, verify_pls_evidence, logical(1)))) {
  stop("Installer PLS whole-draw verifier accepted insufficient or malformed synthetic evidence.", call. = FALSE)
}
fractional_product_result <- synthetic_product_result
fractional_product_result$nboot <- 800.5
if (isTRUE(verify_pls_evidence(map_pls_evidence(fractional_product_result)))) {
  stop("Installer PLS product-field mapper truncated a fractional valid-draw count.", call. = FALSE)
}
require_contains(gate, '$env:STATEDU_STRUCTURAL_BOOTSTRAP_MODE = "installer"', "focused structural installer mode")
require_contains(gate, '$env:STATEDU_CSEM_VALIDATION_MODE = "required"', "focused required cSEM validation mode")
require_contains(gate, 'Remove-Item Env:\\STATEDU_CSEM_VALIDATION_MODE', "focused cSEM mode restoration")
require_contains(gate, '$env:STATEDU_SMARTPLS_EVIDENCE_MODE = "required"', "focused required SmartPLS evidence mode")
require_contains(gate, 'Remove-Item Env:\\STATEDU_SMARTPLS_EVIDENCE_MODE', "focused SmartPLS evidence mode restoration")
require_contains(gate, '$env:STATEDU_STRUCTURAL_BOOTSTRAP_REPORT', "focused structural timing report path")
require_contains(gate, '$env:STATEDU_STRUCTURAL_BOOTSTRAP_RUN_ID = $structuralBootstrapRunId', "fresh structural evidence run id")
require_contains(gate, 'Remove-Item -LiteralPath $structuralBootstrapReport -Force', "focused stale structural evidence invalidation")
require_contains(gate, '$structuralEvidence.run_id -ne $structuralBootstrapRunId', "focused structural run-id verification")
require_contains(gate, '$structuralExactness.sem_two_stage_vs_full_se', "focused SEM exactness evidence verification")
require_contains(gate, '$structuralExactness.sem_product_index_vs_legacy', "focused latent-product legacy evidence verification")
require_contains(gate, '$structuralExactness.sem_product_index_fail_open', "focused latent-product fail-open evidence verification")
require_contains(gate, '$structuralExactness.sem_product_index_missing_guard', "focused latent-product missing guard verification")
require_contains(gate, '$structuralExactness.sem_product_index_single_position', "focused latent-product singleton/deferred verification")
require_contains(gate, '$structuralExactness.sem_fixed_index_vs_legacy', "focused nonproduct fixed-index evidence verification")
require_contains(gate, '$structuralExactness.sem_fixed_index_fail_open', "focused nonproduct fail-open evidence verification")
require_contains(gate, '$structuralExactness.sem_fixed_index_normal_default', "focused nonproduct normal-default verification")
require_contains(gate, '$structuralExactness.sem_fixed_index_worker_payload_small', "focused transport-size evidence verification")
require_contains(gate, '$structuralExactness.cfa_legacy_vs_fast_serial', "focused CFA serial exactness evidence verification")
require_contains(gate, '$structuralExactness.cfa_fast_serial_vs_psock', "focused CFA PSOCK exactness evidence verification")
require_contains(gate, '$structuralExactness.metadata_restore', "focused metadata restoration evidence verification")
require_contains(gate, "CFA 1,000 / SEM 5,000 / PLS-SEM 1,000 actual repetitions", "exact measured repetition verification")
require_contains(gate, "function Assert-PlsWholeDrawEvidence", "external PLS whole-draw JSON verifier")
require_contains(gate, '$valid -lt 800', "external PLS 800-valid-draw gate")
require_contains(gate, '$ratio -lt .80', "external PLS 80-percent valid-ratio gate")
require_contains(gate, '$Evidence.inference_available -isnot [bool]', "external PLS inference boolean gate")
require_contains(gate, '[string]$Evidence.bootstrap_status -cne "Adequate"', "external PLS adequate-status gate")
require_contains(gate, "Assert-PlsWholeDrawEvidence -Evidence $structuralEvidence.metrics.installer.pls_sem", "external PLS JSON evidence invocation")

exactness_position <- regexpr(
  "exactness_run <- run_structural_exactness_gate()",
  structural_bootstrap_regression, fixed = TRUE
)[[1L]]
mode_timing_position <- regexpr(
  paste0('if (!installer_mode) {\n',
         '  message("Running fast core structural bootstrap progress and reproducibility checks...")'),
  structural_bootstrap_regression, fixed = TRUE
)[[1L]]
if (exactness_position < 1L || mode_timing_position < 1L || exactness_position > mode_timing_position) {
  stop("SEM/CFA exactness checks must execute before the core/installer timing branch.", call. = FALSE)
}
require_contains(
  stabilization,
  "scripts\\validate_structural_bootstrap_performance.R",
  "fast structural bootstrap validation in the core stabilization runner"
)

message("Checking the fail-closed cSEM numerical-validation contract...")
csem_contracts <- c(
  'Sys.getenv("STATEDU_CSEM_VALIDATION_MODE", "required")' = "required-by-default focused cSEM mode",
  'validation_mode %in% c("required", "optional")' = "closed cSEM mode vocabulary",
  'expected_csem_version <- "0.6.1"' = "pinned cSEM reference version",
  "RELEASE BLOCKER: cSEM " = "missing required cSEM blocks release",
  "required package-level PLS numerical validation cannot be skipped" = "no silent package-level equivalence skip",
  "SKIP (explicit optional contract)" = "explicit non-release optional behavior",
  "tolerance = 1e-12" = "strict cSEM numerical tolerance"
)
for (pattern in names(csem_contracts)) {
  require_contains(csem_regression, pattern, csem_contracts[[pattern]])
}
require_contains(stabilization, '$env:STATEDU_CSEM_VALIDATION_MODE = "optional"', "general full-suite explicit optional cSEM contract")

message("Checking the private SmartPLS evidence release contract...")
smartpls_contracts <- c(
  'Sys.getenv("STATEDU_SMARTPLS_EVIDENCE_MODE", "required")' = "required-by-default SmartPLS evidence mode",
  'STATEDU_SMARTPLS_EVIDENCE_ROOT' = "external private evidence root",
  'RELEASE BLOCKER:' = "missing private evidence blocks release",
  'must use a safe relative path below the private evidence root' = "absolute/traversal path rejection",
  'byte length does not match the recorded value' = "private artifact byte seal",
  'length(private_names) == 16L' = "complete sixteen-artifact private bundle",
  'length(citation_public_files) == 5L' = "five public SmartPLS citation surfaces",
  'Ringle, C. M., Wende, S., and Becker, J.-M. (2024). SmartPLS 4. Bönningstedt: SmartPLS GmbH. https://www.smartpls.com .' = "complete SmartPLS Terms section 8.1 citation",
  'SmartPLS Terms §3.4' = "private-artifact publication basis",
  'normalized-text SHA-256 does not match the recorded value' = "public normalized text seal",
  'nrow(validated$comparison) == 6L' = "six displayed fit comparisons",
  'all(validated$comparison$`Absolute error` <= half_unit)' = "display half-unit tolerance",
  'pls_fit_compare_external(regenerated$statedu_path' = "canonical StatEdu regeneration"
)
for (pattern in names(smartpls_contracts)) {
  require_contains(paste(smartpls_private_evidence, smartpls_evidence_finalizer, sep = "\n"), pattern, smartpls_contracts[[pattern]])
}
require_contains(stabilization, '$env:STATEDU_SMARTPLS_EVIDENCE_MODE = "optional"', "general full-suite explicit optional SmartPLS evidence contract")

message("Checking the PLS whole-draw validity and minimum-ratio contract...")
pls_bootstrap_contracts <- c(
  "structural_canvas_pls_bootstrap_min_valid_ratio <- function() .80" = "80% PLS minimum valid ratio",
  "structural_canvas_pls_bootstrap_required_masks" = "explicit reported-statistic mask",
  "identical(is.finite(value), required_mask)" = "complete required-statistic mask validation",
  "structural_canvas_pls_bootstrap_components_contract" = "PLS component dimension/name contract",
  "invalid_statistic_failures" = "PLS invalid-statistic diagnostics",
  "structural_canvas_pls_bootstrap_suppress_inference" = "insufficient-validity inference suppression",
  "structural_canvas_pls_bootstrap_unavailable_result" = "persistent pending/failed/canceled result contract",
  "apply_plsc = use_plsc" = "shared PLS/PLSc bootstrap engine"
)
for (pattern in names(pls_bootstrap_contracts)) {
  require_contains(pls_bootstrap_engine, pattern, pls_bootstrap_contracts[[pattern]])
}
for (pattern in c(
  "identical(selection$valid, c(TRUE, FALSE, TRUE))",
  "identical(masked_selection$valid, c(TRUE, TRUE))",
  "structural_canvas_pls_bootstrap_validity(79L, 100L)",
  "all(is.na(suppressed",
  "identical(streams_first, streams_second)",
  "snapshot_no_bootstrap",
  "snapshot_suppressed",
  "all(!vapply(suppressed_edges"
)) {
  require_contains(pls_bootstrap_contract, pattern, sprintf("executable PLS bootstrap regression: %s", pattern))
}
for (pattern in c(
  "bootstrap_inference_available <- is.list(bootstrap) && isTRUE(bootstrap$inference_available)",
  "edge$dashEligible <- !error_path && is.finite(edge$p)"
)) {
  require_contains(structural_core, pattern, sprintf("PLS snapshot inference gate: %s", pattern))
}
for (pattern in c(
  'status = "Pending"',
  'status = "Canceled"',
  'status = "Failed"',
  "result_contract_ok"
)) {
  require_contains(structural_handlers, pattern, sprintf("persistent PLS background failure contract: %s", pattern))
}
for (pattern in c(
  'else "not_declared"',
  '"independent_cross_sectional"'
)) {
  require_contains(structural_settings, pattern, sprintf("fail-closed sampling-design normalization: %s", pattern))
}
for (pattern in c(
  "structural_canvas_run_plsc_bootstrap",
  "structural_canvas_pls_bootstrap_components_contract",
  "PLS bootstrap inference is unavailable",
  "valid_positions",
  "failure_counts"
)) {
  require_contains(audit_export, pattern, sprintf("PLS bootstrap Audit contract: %s", pattern))
}
for (pattern in c(
  "!identical(pls_fingerprint_before$sha256, pls_fingerprint_after$sha256)",
  "grepl(\"valid 77/100\"",
  "identical(pls_audit$resampling$pls$failure_counts$invalid_statistics, 15L)"
)) {
  require_contains(sem_policy_metadata, pattern, sprintf("executable PLS Audit regression: %s", pattern))
}

message("Checking the shared CFA/SEM/PLS-SEM bootstrap choice contract...")
require_contains(
  cfa_ui_regression,
  "c(0L, 1000L, 5000L, 10000L, 20000L, 50000L)",
  "CFA shared bootstrap choices"
)
require_contains(
  sem_canvas_regression,
  "common_bootstrap_values <- c(0L, 1000L, 5000L, 10000L, 20000L, 50000L)",
  "SEM/PLS-SEM shared bootstrap choices"
)
require_contains(cfa_ui_regression, '!grepl(\'value="30000"\'', "CFA 30,000 exclusion")
require_contains(sem_canvas_regression, '!grepl(\'value="30000"\'', "SEM/PLS-SEM 30,000 exclusion")

message("Checking the tracked 10,000-sample runtime regression contract...")
runtime_contracts <- c(
  "n <- 75L" = "75-row fixture",
  "x = c(\"X1\", \"X2\")" = "two focal X variables",
  "mediators = c(\"M1\", \"M2\")" = "two mediator variables",
  "y = \"Y\"" = "one outcome variable",
  "covariates = \"C\"" = "one covariate",
  "boot_r = 5000L" = "5,000 samples per focal X",
  "job$requested_total == 10000L" = "10,000 requested samples",
  "STATEDU_RUNTIME_BOOTSTRAP_MAX_SECONDS" = "worker runtime budget",
  "STATEDU_RUNTIME_RESULT_READ_MAX_SECONDS" = "result-read runtime budget",
  "STATEDU_RUNTIME_RESULT_RENDER_MAX_SECONDS" = "result-render runtime budget",
  "STATEDU_RUNTIME_FINALIZE_MAX_SECONDS" = "finalization runtime budget",
  "runtime_finalize_elapsed <- function(phase_samples)" = "finalizing-to-complete interval helper",
  "complete_first_seen - finalizing_first_seen" = "complete progress boundary for finalization",
  "value >= 0 && value <= budget" = "finite nonnegative finalization budget contract",
  "synthetic_delayed_worker_exit - runtime_phase_first_seen(synthetic_delayed_exit_phases, \"complete\") > 5" = "post-complete exit-delay synthetic",
  "runtime_finalize_within_budget(synthetic_delayed_exit_phases, 5)" = "exit-delay exclusion synthetic pass",
  "stopifnot(!runtime_finalize_within_budget(synthetic_slow_finalize_phases, 5))" = "true slow-finalization synthetic fail",
  "worker_hard_max <- 25" = "non-overridable 25-second per-worker hard ceiling",
  "runtime_sample_count <- 3L" = "three independent worker samples",
  "run_runtime_sample(sample_index, reference_result)" = "fresh worker invocation for every repeat",
  "runtime_canonical_result <- function(value)" = "comparison-copy environment canonicalization",
  "if (is.environment(value)) return(emptyenv())" = "worker-local environment identity exclusion",
  "value[index] <- list(NULL)" = "NULL child preservation during canonicalization",
  "attributes(value) <- value_attributes" = "canonical comparison attribute preservation",
  "runtime_results_identical(result, reference_result)" = "exact repeat-result comparison",
  "ignore.environment = FALSE" = "strict post-canonicalization comparison",
  "parent_rng_after, parent_rng_before, num.eq = FALSE" = "exact parent RNG isolation comparison",
  "required_phases <- c(\"starting\", \"preparing\", \"resampling\", \"finalizing\", \"complete\")" = "phase metrics",
  "try(statedu_stop_background_process_tree(job$process)" = "recursive worker-process termination",
  "mediation_moderation_cleanup_bootstrap_job(job)" = "worker-directory cleanup",
  "cleanup_verified <- isTRUE(process_stopped) && !dir.exists(job_directory)" = "process and directory cleanup assertion",
  "length(worker_elapsed_values) != runtime_sample_count || any(!is.finite(worker_elapsed_values))" = "three finite runtime measurements",
  "for (sample_index in seq_len(runtime_sample_count))" = "three sequential fresh-worker invocations",
  "worker_elapsed > worker_hard_max" = "live individual hard-stop enforcement",
  "post_complete_exit_elapsed <- worker_elapsed - complete_started" = "post-complete delay retained in worker total",
  "worker_median <- stats::median(worker_elapsed_values)" = "unmodified median statistic",
  "worker_max <- max(worker_elapsed_values)" = "unmodified maximum statistic",
  "worker_median > worker_budget" = "historical 20-second median target",
  "worker_max > worker_hard_max" = "25-second individual hard maximum",
  "one uncached render preserves the user-visible result latency contract" = "single fresh HTML render measurement"
)
for (pattern in names(runtime_contracts)) {
  require_contains(runtime_regression, pattern, runtime_contracts[[pattern]])
}
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_BOOTSTRAP_MAX_SECONDS", 20)', "default 20-second worker budget")
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_RESULT_READ_MAX_SECONDS", 1)', "default 1-second result-read budget")
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_RESULT_RENDER_MAX_SECONDS", 5)', "default 5-second result-render budget")
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_FINALIZE_MAX_SECONDS", 5)', "default 5-second finalization budget")
require_contains(runtime_regression, "cannot exceed the historical 20-second target", "fail-closed 20-second override ceiling")
require_contains(gate, "Exact 10,000-sample custom bootstrap robust runtime", "robust runtime gate label")

message("Checking fail-closed release and build integration...")
require_contains(
  stabilization,
  "scripts\\validate_installer_regression_gate.R",
  "gate contract in the core stabilization runner"
)
require_contains(
  stabilization,
  "scripts\\validate_mediation_moderation_runtime.R",
  "tracked runtime regression in the core stabilization runner"
)
require_contains(
  preflight,
  "scripts\\validate_installer_regressions.ps1",
  "focused gate in release preflight"
)
require_contains(
  build,
  "scripts\\validate_installer_regressions.ps1",
  "focused gate before Electron packaging"
)
require_absent(build, "SkipInstallerRegression", "installer build gate skip switch")

gate_position <- regexpr("scripts\\validate_installer_regressions.ps1", build, fixed = TRUE)[[1]]
destructive_position <- regexpr("\nRemove-StaleElectronDistArtifacts\n", build, fixed = TRUE)[[1]]
if (gate_position < 1L || destructive_position < 1L || gate_position > destructive_position) {
  stop("Installer regression gate must run before Electron staging cleanup begins.", call. = FALSE)
}

message("Checking release documentation and packaged QA coverage...")
for (text in list(release_checklist, manual_qa, promotion, packaged_notes, manual_record)) {
  require_contains(
    text,
    "INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md",
    "regression checklist reference"
  )
  require_contains(
    text,
    "validate_installer_regressions.ps1",
    "focused gate command"
  )
}

for (text in list(packaged_notes, manual_record)) {
  require_contains(text, "within 15 seconds", "packaged first-screen timing budget")
  require_contains(text, "within 5 seconds", "packaged file/result timing budget")
  require_contains(text, "within 3 seconds", "packaged custom-canvas timing budget")
  require_contains(text, "within 20 seconds", "packaged bootstrap timing budget")
  require_contains(text, "Delta R-squared", "packaged conditional Delta row check")
}
require_contains(packaged_notes, "A failed or unmeasured required row blocks promotion.", "unmeasured packaged check blocks promotion")
require_contains(manual_record, "every required row is `Pass` or has a justified `NA`", "manual QA completion is fail-closed")

for (text in list(dev_packaged_notes, dev_manual_record)) {
  require_contains(text, "1.2.4-dev", "current development-package identity")
  require_contains(text, "INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md", "current regression checklist reference")
  require_contains(text, "Pending", "current development-package fail-closed status")
}
for (contract in c(
  "STATEDU_SMARTPLS_EVIDENCE_ROOT",
  "cSEM 0.6.1",
  "Audit schema 1.7",
  "Complex-sample",
  "L'Ecuyer"
)) {
  require_contains(
    dev_packaged_notes,
    contract,
    sprintf("1.2.4-dev packaged evidence contract %s", contract)
  )
}
for (contract in c(
  "scripts/validate_installer_regressions.ps1",
  "Private SmartPLS evidence gate",
  "Sampling-design gate",
  "At least 80% whole-draw-valid repetitions",
  "Residual processes"
)) {
  require_contains(
    dev_manual_record,
    contract,
    sprintf("1.2.4-dev manual QA contract %s", contract)
  )
}

for (contract in c(
  "구조모형 기본 설정",
  "1,000 / 5,000 / 10,000 / 20,000 / 50,000",
  "구조모형 결과 라벨",
  "statedu_pi_*",
  "구조모형 bootstrap 성능"
)) {
  require_contains(
    regression_checklist,
    contract,
    sprintf("current installer structural contract %s", contract)
  )
}

required_symptoms <- c(
  "작은 파일 열기",
  "캔버스 진입",
  "상태바 두 개",
  "결과 전환",
  "Delta R²",
  "독립 관측 횡단자료",
  "1,000 / 5,000 / 10,000 / 20,000 / 50,000",
  "라벨 우선",
  "CFA·SEM·PLS-SEM",
  "PLS bootstrap 유효 반복",
  "cSEM 0.6.1",
  "startup.log",
  "13px"
)
for (symptom in required_symptoms) {
  require_contains(
    regression_checklist,
    symptom,
    sprintf("documented regression symptom %s", symptom)
  )
}

required_budgets <- c(
  "15초 초과",
  "5초 초과",
  "3초 초과",
  "전체 20초 초과",
  "중앙값 20초 이내",
  "개별 25초 이내"
)
for (budget in required_budgets) {
  require_contains(
    regression_checklist,
    budget,
    sprintf("documented performance budget %s", budget)
  )
}

message("All installer regression gate contract validations passed.")
