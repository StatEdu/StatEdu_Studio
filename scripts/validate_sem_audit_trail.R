report_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_export_report.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
handler_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_handlers.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
spec_source <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl("structural_canvas_audit_manifest <- function", report_source, fixed = TRUE),
  grepl('version = "1.4"', report_source, fixed = TRUE),
  grepl("specification_sha256", report_source, fixed = TRUE),
  grepl("raw_data_included = FALSE", report_source, fixed = TRUE),
  grepl("structural_effect_plan", report_source, fixed = TRUE),
  grepl('"_download_audit"', handler_source, fixed = TRUE),
  grepl("structural_canvas_write_audit_manifest", handler_source, fixed = TRUE),
  grepl('"_download_audit"', render_source, fixed = TRUE),
  grepl("Output and Audit Trail", spec_source, fixed = TRUE)
)

message("SEM audit-trail validation passed.")
