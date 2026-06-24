script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_version_metadata.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

read_first_line <- function(path) {
  trimws(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8")[[1]])
}

extract_match <- function(text, pattern, label) {
  match <- regexec(pattern, text, perl = TRUE)
  parts <- regmatches(text, match)[[1]]
  if (length(parts) < 2) {
    stop(sprintf("Could not find %s", label), call. = FALSE)
  }
  parts[[2]]
}

assert_equal <- function(actual, expected, label) {
  if (is.null(actual) || length(actual) == 0) {
    stop(sprintf("%s missing", label), call. = FALSE)
  }
  if (!identical(actual, expected)) {
    stop(sprintf("%s mismatch: expected %s, found %s", label, expected, paste(actual, collapse = ", ")), call. = FALSE)
  }
}

assert_contains <- function(text, pattern, label) {
  if (!grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("%s missing", label), call. = FALSE)
  }
}

assert_not_contains <- function(text, pattern, label) {
  if (grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("%s should not be present", label), call. = FALSE)
  }
}

message("Checking version metadata...")

version <- read_first_line("VERSION")
if (!grepl("^\\d+\\.\\d+\\.\\d+$", version)) {
  stop(sprintf("VERSION must use semantic version format, found: %s", version), call. = FALSE)
}

read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required for version metadata validation.", call. = FALSE)
  }
  jsonlite::fromJSON(file.path(repo_root, path), simplifyVector = FALSE)
}

electron_package <- read_json("packaging/electron/package.json")
electron_lock <- read_json("packaging/electron/package-lock.json")

assert_equal(electron_package$version, version, "packaging/electron/package.json version")
assert_equal(electron_lock$version, version, "packaging/electron/package-lock.json root version")
root_package <- electron_lock$packages[[which(names(electron_lock$packages) == "")[[1]]]]
assert_equal(root_package$version, version, "packaging/electron/package-lock.json package version")

readme <- read_text("README.md")
readme_current <- extract_match(readme, "Current development version: `([^`]+)`", "README current development version")
assert_equal(readme_current, version, "README current development version")

readme_citation <- extract_match(readme, "\\(Version ([0-9]+\\.[0-9]+\\.[0-9]+)\\) \\[Computer software\\]", "README citation version")
assert_equal(readme_citation, version, "README citation version")
assert_contains(readme, "scripts\\validate_stabilization.ps1", "README core stabilization validation command")
assert_contains(readme, "scripts\\validate_stabilization.ps1 -Full", "README full stabilization validation command")
assert_contains(readme, "scripts\\release_preflight.ps1", "README release preflight command")
assert_contains(readme, "scripts\\release_preflight.ps1 -FullElectronSmoke", "README full Electron preflight command")
assert_contains(readme, "scripts\\smoke_shiny_app.ps1", "README Shiny smoke command")
assert_contains(readme, "scripts\\smoke_electron_release.ps1 -SkipUnpackedChecks", "README Electron smoke command")
assert_contains(readme, "docs/RELEASE_MANUAL_QA.md", "README manual QA protocol")
assert_contains(readme, "completed\nQA record with the release notes and validation artifacts", "README manual QA record")
assert_contains(readme, "Before a public\n1.0 release or public citation announcement", "README DOI public citation gate")

latent_readme <- read_text("modules/latent_mplus/app/README.md")
latent_readme_current <- extract_match(latent_readme, "Current development version: `([^`]+)`", "latent README current development version")
assert_equal(latent_readme_current, version, "latent README current development version")
assert_contains(latent_readme, paste0("StatEdu Studio ", version, " app shell structure"), "latent README app shell version")

changelog <- read_text("CHANGELOG.md")
changelog_current <- extract_match(changelog, "(?m)^## v([0-9]+\\.[0-9]+\\.[0-9]+) - ", "CHANGELOG current version")
assert_equal(changelog_current, version, "CHANGELOG current version")
assert_contains(changelog, "Added Shiny startup and Electron release smoke checks", "CHANGELOG smoke validation entry")
assert_contains(changelog, "Hardened release hygiene checks", "CHANGELOG release hygiene entry")

citation <- read_text("CITATION.cff")
citation_version <- extract_match(citation, '(?m)^version: "([^"]+)"', "CITATION.cff version")
assert_equal(citation_version, version, "CITATION.cff version")

release_checklist <- read_text("docs/RELEASE_CHECKLIST.md")
assert_contains(release_checklist, "0.9.42 stabilization phase", "release checklist stabilization phase")
assert_contains(release_checklist, "do not add new analysis features before 1.0", "release checklist feature freeze")
assert_contains(release_checklist, "validate_stabilization.ps1 -Full", "release checklist full validation command")
assert_contains(release_checklist, "release_preflight.ps1", "release checklist release preflight command")
assert_contains(release_checklist, "-FullElectronSmoke", "release checklist full Electron smoke option")
assert_contains(release_checklist, "smoke_shiny_app.ps1", "release checklist Shiny smoke test")
assert_contains(release_checklist, ".Rhistory", "release checklist local artifact hygiene")
assert_contains(release_checklist, "Electron staging directories are not tracked by git", "release checklist Electron staging hygiene")
assert_contains(release_checklist, "docs/RELEASE_READINESS_STATUS.md", "release checklist readiness status")
assert_contains(release_checklist, "docs/RELEASE_1_0_DECISION_LOG.md", "release checklist decision log")
assert_contains(release_checklist, "docs/RELEASE_MANUAL_QA.md", "release checklist manual QA")
assert_contains(release_checklist, "completed manual QA record", "release checklist completed manual QA record")
assert_contains(release_checklist, "docs/UI_LAYOUT_CONTRACT.md", "release checklist UI layout contract")
assert_contains(release_checklist, "footer button placement", "release checklist UI footer placement")
assert_contains(release_checklist, "resolves to `https://studio.statedu.com`", "release checklist DOI landing URL")
assert_contains(release_checklist, "replace 0.9.x beta packaging names with final release names", "release checklist beta package naming gate")
assert_contains(release_checklist, "do not carry those beta names into a public 1.0 installer", "release checklist public beta naming warning")

release_readiness <- read_text("docs/RELEASE_READINESS_STATUS.md")
assert_contains(release_readiness, paste0("Current version: ", version), "release readiness current version")
assert_contains(release_readiness, "scripts/validate_stabilization.ps1 -Full", "release readiness full validation")
assert_contains(release_readiness, "scripts/release_preflight.ps1", "release readiness release preflight")
assert_contains(release_readiness, "`scripts/release_preflight.ps1`: passed", "release readiness release preflight passed status")
assert_contains(release_readiness, "-FullElectronSmoke", "release readiness full Electron smoke option")
assert_contains(release_readiness, "scripts/smoke_shiny_app.ps1", "release readiness Shiny smoke")
assert_contains(release_readiness, "scripts/smoke_electron_release.ps1 -SkipUnpackedChecks", "release readiness Electron smoke")
assert_contains(release_readiness, "without `-SkipUnpackedChecks`", "release readiness full Electron smoke reminder")
assert_contains(release_readiness, "docs/RELEASE_MANUAL_QA.md", "release readiness manual QA")
assert_contains(release_readiness, "completed manual QA record", "release readiness completed manual QA record")
assert_contains(release_readiness, "HTTP 404", "release readiness DOI status")
assert_contains(release_readiness, "implemented for 1.0 or explicitly deferred", "release readiness implementation deferral decision")
assert_contains(release_readiness, "docs/RELEASE_1_0_DECISION_LOG.md", "release readiness decision log")
assert_contains(release_readiness, "Items Still Required Before Public 1.0", "release readiness public blockers")
assert_contains(release_readiness, "Build the Electron package", "release readiness package blocker")
assert_contains(release_readiness, "Confirm `studio.statedu.com` is live", "release readiness website blocker")
assert_contains(release_readiness, "Register and verify DOI", "release readiness DOI blocker")
assert_contains(release_readiness, "DOI landing URL resolves to `https://studio.statedu.com`", "release readiness DOI landing URL")
assert_contains(release_readiness, "README, `CITATION.cff`, and About metadata already contain the intended DOI", "release readiness intended DOI metadata warning")
assert_contains(release_readiness, "Replace 0.9.x beta packaging names with final 1.0 release names", "release readiness beta package naming gate")
assert_contains(release_readiness, "record an explicit decision to keep beta branding", "release readiness beta naming decision")

manual_qa <- read_text("docs/RELEASE_MANUAL_QA.md")
assert_contains(manual_qa, "Do not use this pass to add new analysis features.", "manual QA feature freeze")
assert_contains(manual_qa, "record `Pass`, `Fail`, or `NA`", "manual QA status recording")
assert_contains(manual_qa, "scripts/validate_stabilization.ps1 -Full", "manual QA full validation")
assert_contains(manual_qa, "docs/UI_LAYOUT_CONTRACT.md", "manual QA UI layout contract")
assert_contains(manual_qa, "settings save/load uses only the `.studio` file type", "manual QA settings file type")
assert_contains(manual_qa, "Wide to Long can save the reshaped CSV output", "manual QA wide-to-long CSV save")
assert_contains(manual_qa, "Longitudinal / Panel Models", "manual QA longitudinal workflow")
assert_contains(manual_qa, "About > Open Source Licenses", "manual QA open source licenses")
assert_contains(manual_qa, "DOI `10.22934/statedu.studio` resolves to `https://studio.statedu.com`", "manual QA DOI gate")
assert_contains(manual_qa, "do not claim gated editions, license activation, in-app updates, or public installer infrastructure", "manual QA public release non-claiming gate")
assert_contains(manual_qa, "QA Record Template", "manual QA record template")
assert_contains(manual_qa, "release notes and validation artifacts", "manual QA evidence archive")
assert_contains(manual_qa, "Fix commit:", "manual QA fix commit field")
assert_contains(manual_qa, "Re-run validation:", "manual QA rerun validation field")

release_decision_log <- read_text("docs/RELEASE_1_0_DECISION_LOG.md")
assert_contains(release_decision_log, paste0("Current version: ", version), "release decision log current version")
assert_contains(release_decision_log, "No new analysis features before 1.0", "release decision log feature freeze")
assert_contains(release_decision_log, "Must Decide Before Public 1.0", "release decision log pending decisions")
assert_contains(release_decision_log, "HTTP 404", "release decision log DOI status")
assert_contains(release_decision_log, "Intended DOI is present in README/CITATION/About metadata", "release decision log intended DOI metadata warning")
assert_contains(release_decision_log, "resolves to `https://studio.statedu.com`", "release decision log DOI landing URL")
assert_contains(release_decision_log, "Block public 1.0 installer", "release decision log package blocker")
assert_contains(release_decision_log, "Current 0.9.42 package metadata intentionally uses beta naming", "release decision log beta naming status")
assert_contains(release_decision_log, "Do not publish public 1.0 installer with beta naming unless explicitly approved", "release decision log beta naming default")
assert_contains(release_decision_log, "Block public citation claim", "release decision log citation blocker")
assert_contains(release_decision_log, "Block public website claim", "release decision log website blocker")
assert_contains(release_decision_log, "Do not claim gated editions", "release decision log edition default")
assert_contains(release_decision_log, "Do not claim license activation", "release decision log license default")
assert_contains(release_decision_log, "Do not claim in-app updates", "release decision log update default")
assert_contains(release_decision_log, "Block public release announcement", "release decision log release note blocker")

distribution_plan <- read_text("docs/RELEASE_1_0_DISTRIBUTION_LICENSE_PLAN_KO.md")
assert_contains(distribution_plan, "planning/reference document only", "distribution plan planning-only status")
assert_contains(distribution_plan, "Do not claim gated editions, license activation, in-app updates, or public installer infrastructure", "distribution plan non-claiming warning")

assert_contains(distribution_plan, "0.9.33 beta", "distribution plan initial baseline version")
assert_contains(distribution_plan, "0.9.42", "distribution plan current stabilization version")
assert_contains(distribution_plan, "statedu-release-plan-reviewed-0.9.42", "distribution plan current review tag")
assert_contains(distribution_plan, "docs/RELEASE_1_0_DECISION_LOG.md", "distribution plan decision log reference")

app_bootstrap <- read_text("R/app_bootstrap.R")
assert_contains(app_bootstrap, 'read_app_config <- function(version_file = "VERSION")', "main read_app_config VERSION default")
assert_contains(app_bootstrap, "version = trimws(readLines(version_file, warn = FALSE)[1])", "main read_app_config VERSION reader")

app_entry <- read_text("app.R")
assert_contains(app_entry, "app_config <- read_app_config()", "main app config read")
assert_contains(app_entry, "app_version <- app_config$version", "main app version assignment")
assert_contains(app_entry, "app_ui(app_version)", "main UI version propagation")
assert_contains(app_entry, "create_app_server(app_version)", "main server version propagation")

ui_helpers <- read_text("R/ui_helpers.R")
assert_contains(ui_helpers, 'span(class = "version", paste0("v", version))', "navbar version display")
assert_contains(ui_helpers, "about_tab_panel(version)", "About version propagation")
assert_contains(ui_helpers, "app_head_tags(version)", "head asset version propagation")

sample_size_ui <- read_text("R/sample_size_ui.R")
assert_not_contains(sample_size_ui, "will be added in the same order", "effect-size future placeholder subtitle")
assert_contains(sample_size_ui, "Effect-size calculator availability follows the selected method.", "effect-size neutral fallback subtitle")

result_saved_ui <- read_text("R/result_saved_ui.R")
assert_contains(result_saved_ui, 'saved_results_app_version <- function(version_file = "VERSION")', "saved results VERSION default")
assert_contains(result_saved_ui, 'Sys.getenv("EASYFLOW_VERSION", "")', "saved results environment version override")
assert_contains(result_saved_ui, 'readLines(version_file, warn = FALSE)[1]', "saved results VERSION fallback")
assert_contains(result_saved_ui, 'sprintf("StatEdu Studio v%s", app_version)', "saved results version label")

latent_app <- read_text("modules/latent_mplus/app/app.R")
assert_contains(latent_app, "app_config <- read_app_config()", "latent app config read")
assert_contains(latent_app, "app_version <- app_config$version", "latent app version assignment")
assert_contains(latent_app, "app_ui(app_version)", "latent UI version propagation")
assert_contains(latent_app, "create_app_server(app_version)", "latent server version propagation")

latent_app_server <- read_text("modules/latent_mplus/app/R/app_server.R")
assert_not_contains(latent_app_server, "builder placeholder", "latent user-facing placeholder builder message")
assert_contains(latent_app_server, "Dictionary builder is not enabled in this release", "latent dictionary unavailable message")
assert_contains(latent_app_server, "CFG builder is not enabled in this release", "latent CFG unavailable message")

latent_bootstrap <- read_text("modules/latent_mplus/app/R/app_bootstrap.R")
assert_contains(latent_bootstrap, 'read_app_config <- function(version_file = "VERSION")', "latent read_app_config VERSION default")
assert_contains(latent_bootstrap, "list(version = trimws(readLines(version_file, warn = FALSE)[1]))", "latent read_app_config VERSION reader")

cat(sprintf("Version metadata validation passed: %s\n", version))
