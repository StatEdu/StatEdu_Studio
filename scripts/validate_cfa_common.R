source(file.path("R", "utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path("R", "setup_custom_model_canvas_snapshot.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_core.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_identification_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_distribution_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_local_fit_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_risk_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_higher_order.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_validity.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_reliability.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_evaluation.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_mi_evaluation.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_holdout_evaluation.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_invariance_evaluation.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_engine.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_bootstrap.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_reliability_bootstrap.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_bollen_stine_bootstrap.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_exports.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_export_fit_tables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_export_report.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_export_workbook.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_i18n.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_variables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_components.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_options.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_toolbar.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_result_snapshot.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_components.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_toolbar_components.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_options.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_summary_tables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_tables.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_data_diagnostics.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_invariance.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_heywood.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_fit_core.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_fit_summary.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_validity.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_local_fit.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render_mi.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_render.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_execute.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_events.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_handlers.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_ui.R"), encoding = "UTF-8")

ui_source <- paste(
  c(
    readLines(file.path("R", "setup_custom_model_canvas_i18n.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_variables.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_components.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_options.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_toolbar.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_result_snapshot.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_components.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_toolbar_components.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_options.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_summary_tables.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_tables.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_data_diagnostics.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_invariance.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_heywood.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit_core.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit_summary.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_validity.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_local_fit.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render_mi.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_execute.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_events.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_structural_handlers.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path("R", "setup_custom_model_canvas_ui.R"), warn = FALSE, encoding = "UTF-8")
  ),
  collapse = "\n"
)
export_source <- paste(c(
  readLines(file.path("R", "setup_custom_model_canvas_exports.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_export_fit_tables.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_export_report.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_export_workbook.R"), warn = FALSE, encoding = "UTF-8")
), collapse = "\n")
evaluation_source <- paste(c(
  readLines(file.path("R", "setup_custom_model_canvas_structural_evaluation.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_structural_mi_evaluation.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_structural_holdout_evaluation.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_structural_invariance_evaluation.R"), warn = FALSE, encoding = "UTF-8")
), collapse = "\n")
bootstrap_source <- paste(c(
  readLines(file.path("R", "setup_custom_model_canvas_structural_bootstrap.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_structural_reliability_bootstrap.R"), warn = FALSE, encoding = "UTF-8"),
  readLines(file.path("R", "setup_custom_model_canvas_structural_bollen_stine_bootstrap.R"), warn = FALSE, encoding = "UTF-8")
), collapse = "\n")

assert_close <- function(actual, expected, tolerance = 1e-8, label = "value") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("%s mismatch: actual=%s expected=%s", label, paste(actual, collapse = ","), paste(expected, collapse = ",")))
  }
}
