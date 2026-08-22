script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_startup_performance_contract.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

require_contains <- function(text, pattern, label, fixed = TRUE) {
  if (!grepl(pattern, text, fixed = fixed, perl = !fixed)) {
    stop(sprintf("Startup performance contract missing: %s", label), call. = FALSE)
  }
}

require_absent <- function(text, pattern, label, fixed = TRUE) {
  if (grepl(pattern, text, fixed = fixed, perl = !fixed)) {
    stop(sprintf("Startup performance regression detected: %s", label), call. = FALSE)
  }
}

app_server <- read_text("R/app_server.R")
app_entry <- read_text("app.R")
server_analysis <- read_text("R/server_analysis.R")
server_state <- read_text("R/server_state.R")
ui_helpers <- read_text("R/ui_helpers.R")
latent_module <- read_text("R/latent_mplus_module.R")
easyflow <- read_text("www/easyflow.js")
launcher <- read_text("scripts/launch_statedu.ps1")
batch_launcher <- read_text("StatEdu_Studio.bat")
electron <- read_text("packaging/electron/main.js")

message("Checking that idle sessions do not poll bootstrap state...")
require_absent(server_state, "reactiveTimer(200", "global 200 ms bootstrap timer")
require_absent(server_analysis, "bootstrap_tick", "legacy bootstrap tick dependency")
require_contains(server_analysis, "req(!is.null(bootstrap_job()))", "job-scoped bootstrap polling")
require_contains(server_analysis, "invalidateLater(200, session)", "active-job bootstrap refresh")
require_contains(app_server, "bootstrap_manager$cancel()", "bootstrap cleanup")
require_contains(app_server, "priority = 1000", "file-open bootstrap cancellation priority")

message("Checking that latent analysis is absent from initial session assembly...")
require_contains(app_server, "register_on_first_menu_visit(\"latent_mixture\"", "deferred latent server registration")
require_contains(app_server, "lazy_ui(\"lazy_latent_mixture\"", "deferred latent UI registration")
require_contains(latent_module, "uiOutput(\"lazy_latent_mixture\")", "latent menu placeholder")
require_contains(latent_module, "latent_mplus_panel_content", "latent panel factory")
require_absent(ui_helpers, "if (latent_mplus_enabled()) latent_mplus_head_tags(version)", "eager latent head assets")

message("Checking that the custom-model canvas does not initialize every analysis module...")
require_contains(
  app_server,
  "register_on_first_menu_visit(\"analysis_custom_model_canvas\"",
  "dedicated custom-model canvas server registration"
)
bulk_analysis_start <- regexpr(
  "register_on_first_menu_visit(c(\n    \"Frequencies / Descriptives\"",
  app_server,
  fixed = TRUE
)[[1]]
bulk_analysis_end <- regexpr(
  "statedu_log_timing(\"deferred analysis modules\"",
  app_server,
  fixed = TRUE
)[[1]]
if (bulk_analysis_start < 1L || bulk_analysis_end <= bulk_analysis_start) {
  stop("Startup performance contract missing: bulk analysis registration block", call. = FALSE)
}
bulk_analysis_block <- substr(app_server, bulk_analysis_start, bulk_analysis_end)
require_absent(
  bulk_analysis_block,
  "analysis_custom_model_canvas",
  "custom-model menu in bulk analysis registration"
)
require_absent(
  bulk_analysis_block,
  "register_custom_model_canvas_handlers(",
  "custom-model handlers in bulk analysis registration"
)

message("Checking first-page asset deferral...")
require_contains(ui_helpers, "logo-final.png", "reduced-size header logo")
require_absent(ui_helpers, "logo-horizontal.png", "oversized 13k header logo")
require_contains(ui_helpers, "window.easyflowMathJaxSrc", "MathJax deferred source")
require_absent(ui_helpers, "id = \"MathJax-script\"", "eager MathJax script tag")
require_contains(easyflow, "window.easyflowEnsureMathJax", "on-demand MathJax loader")

message("Checking local backend reuse and bounded diagnostics...")
require_contains(batch_launcher, "launch_statedu.ps1", "safe launcher delegation")
require_contains(batch_launcher, "-RestartStaleBackend", "explicit stale backend reload request")
require_absent(batch_launcher, "taskkill", "unconditional backend termination", fixed = FALSE)
require_contains(launcher, "function Get-WorkspaceFingerprint", "workspace content fingerprint")
require_contains(launcher, "function Get-StatEduBackendInfo", "versioned backend health check")
require_contains(launcher, "statedu-health/build.json", "lightweight static backend health endpoint")
require_contains(launcher, "STATEDU_BUILD_FINGERPRINT", "backend build fingerprint propagation")
require_contains(
  launcher,
  'if ($backendInfo.Fingerprint -eq $workspaceFingerprint)',
  "matching-build backend reuse"
)
require_contains(launcher, "function Test-WorkspaceBackendProcess", "workspace backend ownership check")
require_contains(launcher, "verifiedByMetadata", "PID and process-start metadata verification")
require_contains(launcher, "Stop-VerifiedWorkspaceBackend", "verified-only stale backend restart")
require_absent(launcher, "function Test-StatEduBackend", "legacy content-only backend health check")
require_contains(launcher, "STATEDU_SINGLE_SESSION", "single desktop session mode")
require_contains(app_entry, 'addResourcePath("statedu-health"', "static backend health resource")
require_contains(app_entry, "fingerprint = statedu_build_fingerprint", "health fingerprint payload")
require_contains(
  ui_helpers,
  'tags$meta(name = "statedu-build-fingerprint"',
  "HTML build fingerprint marker"
)
require_contains(ui_helpers, "app_script_link(asset_version)", "fingerprinted browser asset URLs")
require_contains(electron, "STATEDU_RENDERER_DIAGNOSTICS", "opt-in renderer diagnostics")
require_contains(electron, "STARTUP_LOG_MAX_BYTES", "startup log rotation")
require_absent(electron, "fs.appendFileSync", "synchronous startup logging")

message("All startup performance contract validations passed.")
