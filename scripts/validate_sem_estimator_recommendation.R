event_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_events_estimator.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
options_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_options.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
execute_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_execute.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
handler_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_handlers.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
export_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_export_report.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
spec_source <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl("structural_canvas_method_recommendation <- function", event_source, fixed = TRUE),
  grepl('primary = "CB-SEM"', event_source, fixed = TRUE),
  grepl('primary = "PLSc-SEM"', event_source, fixed = TRUE),
  grepl('primary = "PLS-SEM"', event_source, fixed = TRUE),
  grepl("Small samples, nonnormality, or better empirical fit alone", event_source, fixed = TRUE),
  grepl('"_objective"', options_source, fixed = TRUE),
  grepl('"_method_recommendation"', options_source, fixed = TRUE),
  grepl("method_recommendation = method_recommendation", execute_source, fixed = TRUE),
  grepl("structural_canvas_method_recommendation_ui", handler_source, fixed = TRUE),
  grepl("Recommended method candidate", export_source, fixed = TRUE),
  grepl("Selected method", export_source, fixed = TRUE),
  grepl("## 3단계: Estimator Recommendation", spec_source, fixed = TRUE)
)

cat("SEM estimator recommendation validations passed.\n")
