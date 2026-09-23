identification_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_identification_diagnostics.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render_fit_core.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
sample_source <- paste(readLines(file.path("R", "sample_size.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
sample_ui_source <- paste(readLines(file.path("R", "sample_size_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
spec_source <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl('"duplicate_covariance"', identification_source, fixed = TRUE),
  grepl('"structural_cycle"', identification_source, fixed = TRUE),
  grepl("Identification and power preflight", render_source, fixed = TRUE),
  grepl("Latent scaling", render_source, fixed = TRUE),
  grepl("A-priori power basis", render_source, fixed = TRUE),
  grepl("Power is not back-calculated", render_source, fixed = TRUE),
  grepl("Approximate parameter power simulation", sample_ui_source, fixed = TRUE),
  grepl("not a full SEM data-generation and model-refitting Monte Carlo study", sample_source, fixed = TRUE),
  grepl("## 2단계: Identification & Power", spec_source, fixed = TRUE)
)

cat("SEM identification and power validations passed.\n")
