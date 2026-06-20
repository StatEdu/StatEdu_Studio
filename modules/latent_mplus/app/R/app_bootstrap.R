# App bootstrap helpers for StatEdu Studio Latent Mplus.
# The app keeps the EFS 0.9.33 shell conventions and adds latent/Mplus helpers.

required_packages <- c(
  "shiny",
  "DT",
  "lmtest",
  "sandwich",
  "nortest",
  "car",
  "boot",
  "jsonlite",
  "yaml",
  "haven",
  "readr",
  "readxl",
  "cellranger",
  "htmltools",
  "markdown",
  "openxlsx",
  "officer",
  "flextable",
  "xml2",
  "rvest",
  "callr",
  "glmnet",
  "agricolae",
  "psych",
  "polycor",
  "longpower",
  "WebPower",
  "TOSTER",
  "MASS",
  "nnet",
  "dplyr"
)

startup_packages <- c("shiny", "DT")

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

load_app_packages <- function(
  packages = required_packages,
  attach_packages = startup_packages,
  check = !identical(tolower(Sys.getenv("EASYFLOW_NO_PACKAGE_INSTALL", "false")), "true")
) {
  if (isTRUE(check)) {
    ensure_required_packages(packages)
  }
  for (package in attach_packages) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }
  invisible(TRUE)
}

app_module_files <- c(
  "utils.R",
  "settings_io.R",
  "settings_dialogs.R",
  "data_io.R",
  "data_roles.R",
  "data_category_labels.R",
  "server_client.R",
  "server_data_state.R",
  "server_state.R",
  "server_settings.R",
  "server_data_outputs.R",
  "server_selection.R",
  "server_workflow.R",
  "data_ui_tables.R",
  "data_ui_steps.R",
  "data_ui.R",
  "ui_helpers.R",
  "latent_ui.R",
  "app_server.R"
)

source_app_modules <- function(files = app_module_files, dir = "R") {
  for (file in files) {
    source(file.path(dir, file), local = FALSE)
  }
  invisible(TRUE)
}

read_app_config <- function(version_file = "VERSION") {
  list(version = trimws(readLines(version_file, warn = FALSE)[1]))
}

options(easyflow.single_output = TRUE)
