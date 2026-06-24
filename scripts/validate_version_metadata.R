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
assert_contains(readme, "scripts\\smoke_shiny_app.ps1", "README Shiny smoke command")
assert_contains(readme, "scripts\\smoke_electron_release.ps1 -SkipUnpackedChecks", "README Electron smoke command")

citation <- read_text("CITATION.cff")
citation_version <- extract_match(citation, '(?m)^version: "([^"]+)"', "CITATION.cff version")
assert_equal(citation_version, version, "CITATION.cff version")

release_checklist <- read_text("docs/RELEASE_CHECKLIST.md")
assert_contains(release_checklist, "0.9.42 stabilization phase", "release checklist stabilization phase")
assert_contains(release_checklist, "do not add new analysis features before 1.0", "release checklist feature freeze")
assert_contains(release_checklist, "validate_stabilization.ps1 -Full", "release checklist full validation command")
assert_contains(release_checklist, "smoke_shiny_app.ps1", "release checklist Shiny smoke test")
assert_contains(release_checklist, ".Rhistory", "release checklist local artifact hygiene")
assert_contains(release_checklist, "Electron staging directories are not tracked by git", "release checklist Electron staging hygiene")

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

latent_bootstrap <- read_text("modules/latent_mplus/app/R/app_bootstrap.R")
assert_contains(latent_bootstrap, 'read_app_config <- function(version_file = "VERSION")', "latent read_app_config VERSION default")
assert_contains(latent_bootstrap, "list(version = trimws(readLines(version_file, warn = FALSE)[1]))", "latent read_app_config VERSION reader")

cat(sprintf("Version metadata validation passed: %s\n", version))
