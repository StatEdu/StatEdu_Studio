diagnostic_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_local_fit_diagnostics.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
options_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_options.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
execute_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_execute.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
export_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_export_report.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
spec_source <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl("structural_canvas_parcel_plan <- function", diagnostic_source, fixed = TRUE),
  grepl("An admissible item-level CFA", diagnostic_source, fixed = TRUE),
  grepl("No parcel variables were created", diagnostic_source, fixed = TRUE),
  grepl('"_parcel_enabled"', options_source, fixed = TRUE),
  grepl("value = FALSE", options_source, fixed = TRUE),
  grepl("parcel_result = parcel_result", execute_source, fixed = TRUE),
  grepl('"_result_parcel_plan"', render_source, fixed = TRUE),
  grepl("variables created = no", export_source, fixed = TRUE),
  grepl("## 5단계: Parceling Safety", spec_source, fixed = TRUE)
)

cat("SEM parcel safety validations passed.\n")
