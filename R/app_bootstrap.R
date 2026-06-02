# App bootstrap helpers for easyflow_statistics.
# All statistical analysis dependencies must be CRAN packages.

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
  "nnet"
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
  "data_regression_setup.R",
  "analysis_reliability.R",
  "analysis_frequencies.R",
  "analysis_crosstabs.R",
  "analysis_correlation.R",
  "analysis_factor_analysis.R",
  "analysis_pca.R",
  "analysis_paired.R",
  "analysis_paired_rm.R",
  "analysis_nonparametric_paired.R",
  "analysis_ttest_anova.R",
  "analysis_regression.R",
  "analysis_logistic.R",
  "sample_size.R",
  "sample_size_ui.R",
  "data_editor_recode.R",
  "data_editor_likert.R",
  "data_editor_missing.R",
  "data_editor_transform.R",
  "data_editor_rename.R",
  "data_editor_ui.R",
  "calculator_hint8.R",
  "calculator_metabolic.R",
  "calculator_metabolic_severity.R",
  "calculator_frs.R",
  "calculator_eq5d.R",
  "calculator_ascvd10.R",
  "analysis_penalized.R",
  "bootstrap_manager.R",
  "server_client.R",
  "server_data_state.R",
  "server_state.R",
  "server_settings.R",
  "server_selection.R",
  "server_setup.R",
  "server_workflow.R",
  "analysis_data_viewer.R",
  "server_reliability.R",
  "server_frequencies.R",
  "server_crosstabs.R",
  "server_logistic.R",
  "server_paired.R",
  "server_paired_rm.R",
  "server_ttest_anova.R",
  "server_nonparametric.R",
  "server_nonparametric_paired.R",
  "server_correlation.R",
  "server_factor_analysis.R",
  "server_pca.R",
  "server_analysis.R",
  "server_data_outputs.R",
  "ui_helpers.R",
  "data_ui_tables.R",
  "data_ui_steps.R",
  "data_ui.R",
  "diagnostic_plots.R",
  "result_formatting.R",
  "result_labels.R",
  "result_model_summary.R",
  "result_coefficients.R",
  "setup_analysis_ui.R",
  "setup_ui.R",
  "analysis_menu_ui.R",
  "setup_reliability_ui.R",
  "setup_frequencies_ui.R",
  "setup_crosstabs_ui.R",
  "setup_paired_ui.R",
  "setup_paired_rm_ui.R",
  "setup_ttest_anova_ui.R",
  "setup_nonparametric_ui.R",
  "setup_nonparametric_paired_ui.R",
  "setup_correlation_ui.R",
  "setup_factor_analysis_ui.R",
  "setup_pca_ui.R",
  "setup_regression_ui.R",
  "setup_hierarchical_ui.R",
  "setup_logistic_ui.R",
  "setup_generalized_ui.R",
  "result_table_ui.R",
  "result_penalized_ui.R",
  "result_panels_ui.R",
  "result_reliability_ui.R",
  "result_frequencies_ui.R",
  "result_crosstabs_ui.R",
  "result_logistic_ui.R",
  "result_paired_ui.R",
  "result_paired_rm_ui.R",
  "result_nonparametric_paired_ui.R",
  "result_correlation_ui.R",
  "result_factor_analysis_ui.R",
  "result_pca_ui.R",
  "result_saved_ui.R",
  "result_bootstrap_ui.R",
  "result_ui.R",
  "result_export.R",
  "result_export_files.R",
  "app_misc_ui.R",
  "app_server.R"
)

source_app_modules <- function(files = app_module_files, dir = "R") {
  for (file in files) {
    source(file.path(dir, file), local = FALSE)
  }
  invisible(TRUE)
}

read_app_config <- function(version_file = "VERSION") {
  list(
    version = trimws(readLines(version_file, warn = FALSE)[1])
  )
}
