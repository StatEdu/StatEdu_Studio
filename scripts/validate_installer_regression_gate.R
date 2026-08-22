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
  "scripts\\validate_sem_canvas.R",
  "scripts\\validate_pls_fit_csem.R"
)) {
  require_absent(
    gate,
    duplicate_validation,
    sprintf("full-suite-only validation duplicated in focused gate: %s", duplicate_validation)
  )
}

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

required_symptoms <- c(
  "작은 파일 열기",
  "캔버스 진입",
  "상태바 두 개",
  "결과 전환",
  "Delta R²",
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
