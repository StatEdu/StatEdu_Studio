# App bootstrap helpers for EasyFlow Statistics.

required_packages <- c(
  "shiny",
  "DT",
  "lmtest",
  "sandwich",
  "nortest",
  "boot",
  "jsonlite",
  "haven",
  "readr",
  "htmltools",
  "openxlsx",
  "glmnet"
)

ensure_required_packages <- function(packages = required_packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Install required packages first: install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  }
  invisible(TRUE)
}

load_app_packages <- function(packages = required_packages) {
  ensure_required_packages(packages)
  for (package in packages) {
    library(package, character.only = TRUE)
  }
  invisible(TRUE)
}

app_module_files <- c(
  "utils.R",
  "settings_io.R",
  "data_io.R",
  "analysis_regression.R",
  "analysis_penalized.R",
  "bootstrap_manager.R",
  "server_helpers.R",
  "ui_helpers.R",
  "data_ui.R",
  "diagnostic_plots.R",
  "result_formatting.R",
  "setup_ui.R",
  "result_ui.R",
  "result_export.R"
)

source_app_modules <- function(files = app_module_files, dir = "R") {
  for (file in files) {
    source(file.path(dir, file), local = FALSE)
  }
  invisible(TRUE)
}

read_app_config <- function(version_file = "VERSION") {
  list(
    version = trimws(readLines(version_file, warn = FALSE)[1]),
    dw_table_path = file.path("data", "durbin_watson_critical_values.csv")
  )
}
