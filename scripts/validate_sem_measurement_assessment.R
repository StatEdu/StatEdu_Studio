pls_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
options_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_options.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
execute_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_execute.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
table_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_tables.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
render_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_render.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
spec_source <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl("structural_canvas_pls_redundancy_analysis <- function", pls_source, fixed = TRUE),
  grepl("The global criterion must be separate", pls_source, fixed = TRUE),
  grepl('"_redundancy_construct"', options_source, fixed = TRUE),
  grepl('"_redundancy_criterion"', options_source, fixed = TRUE),
  grepl("redundancy_result = redundancy_result", execute_source, fixed = TRUE),
  grepl("Construct type", table_source, fixed = TRUE),
  grepl('"_result_redundancy"', render_source, fixed = TRUE),
  grepl(".70 is a descriptive reference, not an automatic pass rule", render_source, fixed = TRUE),
  grepl("## 4단계: Measurement Model Assessment", spec_source, fixed = TRUE)
)

cat("SEM measurement assessment validations passed.\n")
