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
runtime_regression <- read_text("scripts/validate_mediation_moderation_runtime.R")
structural_bootstrap_regression <- read_text("scripts/validate_structural_bootstrap_performance.R")
cfa_ui_regression <- read_text("scripts/validate_cfa_ui.R")
sem_canvas_regression <- read_text("scripts/validate_sem_canvas.R")
stabilization <- read_text("scripts/validate_stabilization.ps1")
preflight <- read_text("scripts/release_preflight.ps1")
build <- read_text("scripts/build_electron_beta.ps1")
release_checklist <- read_text("docs/RELEASE_CHECKLIST.md")
manual_qa <- read_text("docs/RELEASE_MANUAL_QA.md")
promotion <- read_text("docs/RELEASE_1_2_3_PROMOTION_CHECKLIST.md")
packaged_notes <- read_text("docs/RELEASE_1_2_3_PACKAGED_VALIDATION_NOTES.md")
manual_record <- read_text("docs/RELEASE_1_2_3_MANUAL_QA_RECORD.md")
regression_checklist <- read_text("docs/INSTALLER_REGRESSION_CHECKLIST_2026-08-22_KO.md")

message("Checking the focused installer regression suite...")
required_validations <- c(
  "scripts\\validate_installer_regression_gate.R",
  "scripts\\validate_data_io.R",
  "scripts\\validate_data_upload_performance.R",
  "scripts\\validate_startup_performance_contract.R",
  "scripts\\validate_custom_model_canvas.R",
  "scripts\\validate_cfa_ui.R",
  "scripts\\validate_sem_canvas.R",
  "scripts\\validate_pls_fit_csem.R",
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
require_contains(gate, '$env:STATEDU_STRUCTURAL_BOOTSTRAP_MODE = "installer"', "focused structural installer mode")
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
  "required_phases <- c(\"starting\", \"preparing\", \"resampling\", \"finalizing\", \"complete\")" = "phase metrics",
  "mediation_moderation_cleanup_bootstrap_job(job)" = "worker-directory cleanup",
  "cleanup_verified <- !dir.exists(job_directory)" = "cleanup assertion"
)
for (pattern in names(runtime_contracts)) {
  require_contains(runtime_regression, pattern, runtime_contracts[[pattern]])
}
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_BOOTSTRAP_MAX_SECONDS", 20)', "default 20-second worker budget")
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_RESULT_READ_MAX_SECONDS", 1)', "default 1-second result-read budget")
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_RESULT_RENDER_MAX_SECONDS", 5)', "default 5-second result-render budget")
require_contains(runtime_regression, 'runtime_budget("STATEDU_RUNTIME_FINALIZE_MAX_SECONDS", 5)', "default 5-second finalization budget")

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
  "전체 20초 초과"
)
for (budget in required_budgets) {
  require_contains(
    regression_checklist,
    budget,
    sprintf("documented performance budget %s", budget)
  )
}

message("All installer regression gate contract validations passed.")
