script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_settings_dialogs.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_text <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

read_lines <- function(path) {
  readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8")
}

extract_line_range <- function(lines, start_pattern, end_pattern = NULL) {
  start <- grep(start_pattern, lines, fixed = TRUE)[[1]]
  if (is.null(end_pattern)) {
    end <- length(lines)
  } else {
    end_matches <- grep(end_pattern, lines, fixed = TRUE)
    end_matches <- end_matches[end_matches > start]
    end <- if (length(end_matches) > 0) end_matches[[1]] - 1 else length(lines)
  }
  paste(lines[start:end], collapse = "\n")
}

assert_contains <- function(text, pattern, label, fixed = TRUE) {
  found <- if (fixed) {
    grepl(pattern, text, fixed = TRUE)
  } else {
    grepl(pattern, text, perl = TRUE)
  }
  if (!found) {
    stop(sprintf("%s missing", label), call. = FALSE)
  }
}

assert_not_contains <- function(text, pattern, label, fixed = TRUE) {
  found <- if (fixed) {
    grepl(pattern, text, fixed = TRUE)
  } else {
    grepl(pattern, text, perl = TRUE)
  }
  if (found) {
    stop(sprintf("%s found", label), call. = FALSE)
  }
}

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "settings_dialogs.R"))

message("Checking settings file dialog contract...")

settings_dialog_lines <- read_lines("R/settings_dialogs.R")
open_settings_body <- extract_line_range(
  settings_dialog_lines,
  "open_settings_file <- function() {",
  "open_data_file <- function() {"
)
save_settings_body <- extract_line_range(
  settings_dialog_lines,
  "save_settings_file <- function() {"
)

assert_contains(open_settings_body, "{{StatEdu Studio Settings} {.studio}}", "open settings .studio filter")
assert_contains(open_settings_body, '"StatEdu Studio Settings", "*.studio"', "Windows open settings .studio filter")
assert_not_contains(open_settings_body, "*.efs-settings", "legacy .efs-settings open filter")
assert_not_contains(open_settings_body, "*.json", "legacy JSON open filter")

assert_contains(save_settings_body, 'defaultextension = ".studio"', "save settings default extension")
assert_contains(save_settings_body, 'filetypes = "{{StatEdu Studio Settings} {.studio}}"', "save settings .studio filter")
assert_contains(save_settings_body, 'initialfile = ""', "blank save settings filename")
assert_contains(save_settings_body, 'utils::choose.files(', "Windows save settings fallback")
assert_contains(save_settings_body, 'default = ""', "blank fallback save settings filename")
assert_not_contains(save_settings_body, 'initialfile = default_name', "default save settings filename")
assert_not_contains(save_settings_body, 'default_settings_file_name', "legacy generated save settings filename")
assert_not_contains(save_settings_body, 'default_name', "legacy generated save settings filename variable")

message("Checking settings path normalization...")

stopifnot(identical(normalize_settings_save_path("analysis"), "analysis.studio"))
stopifnot(identical(normalize_settings_save_path("analysis.studio"), "analysis.studio"))
stopifnot(identical(normalize_settings_save_path("analysis.studio.studio"), "analysis.studio"))
stopifnot(identical(normalize_settings_save_path("analysis.efs-settings"), "analysis.studio"))
stopifnot(identical(normalize_settings_save_path("analysis.efs-settings.efs-settings"), "analysis.studio"))
stopifnot(identical(normalize_settings_save_path("analysis.json"), "analysis.studio"))
stopifnot(identical(normalize_settings_save_path("analysis.txt"), "analysis.studio"))

message("Checking latent settings file dialog contract...")

latent_settings_dialog_lines <- read_lines("modules/latent_mplus/app/R/settings_dialogs.R")
latent_open_settings_body <- extract_line_range(
  latent_settings_dialog_lines,
  "open_settings_file <- function() {",
  "open_data_file <- function() {"
)
latent_save_settings_body <- extract_line_range(
  latent_settings_dialog_lines,
  "save_settings_file <- function() {"
)

assert_contains(latent_open_settings_body, "{{StatEdu Studio Settings} {.studio}}", "latent open settings .studio filter")
assert_contains(latent_open_settings_body, '"StatEdu Studio Settings", "*.studio"', "latent Windows open settings .studio filter")
assert_not_contains(latent_open_settings_body, "*.efs-settings", "latent legacy .efs-settings open filter")
assert_not_contains(latent_open_settings_body, "*.json", "latent legacy JSON open filter")

assert_contains(latent_save_settings_body, 'defaultextension = ".studio"', "latent save settings default extension")
assert_contains(latent_save_settings_body, 'filetypes = "{{StatEdu Studio Settings} {.studio}}"', "latent save settings .studio filter")
assert_contains(latent_save_settings_body, 'initialfile = ""', "latent blank save settings filename")
assert_contains(latent_save_settings_body, 'utils::choose.files(', "latent Windows save settings fallback")
assert_contains(latent_save_settings_body, 'default = ""', "latent blank fallback save settings filename")
assert_not_contains(latent_save_settings_body, 'StatEdu_Studio_settings.efs-settings', "latent legacy save settings filename")
assert_not_contains(latent_save_settings_body, 'JSON settings', "latent legacy JSON save filter")

latent_env <- new.env(parent = globalenv())
source(file.path(repo_root, "R", "utils.R"), local = latent_env)
source(file.path(repo_root, "modules", "latent_mplus", "app", "R", "settings_dialogs.R"), local = latent_env)

stopifnot(identical(latent_env$normalize_settings_save_path("latent"), "latent.studio"))
stopifnot(identical(latent_env$normalize_settings_save_path("latent.studio"), "latent.studio"))
stopifnot(identical(latent_env$normalize_settings_save_path("latent.studio.studio"), "latent.studio"))
stopifnot(identical(latent_env$normalize_settings_save_path("latent.efs-settings"), "latent.studio"))
stopifnot(identical(latent_env$normalize_settings_save_path("latent.efs-settings.efs-settings"), "latent.studio"))
stopifnot(identical(latent_env$normalize_settings_save_path("latent.json"), "latent.studio"))
stopifnot(identical(latent_env$normalize_settings_save_path("latent.txt"), "latent.studio"))

cat("Settings dialog validation passed.\n")
