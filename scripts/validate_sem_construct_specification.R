canvas_source <- paste(readLines(file.path("www", "model-canvas", "canvas.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
nodes_source <- paste(readLines(file.path("www", "model-canvas", "nodes.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
spec_source <- paste(readLines(file.path("docs", "SEM_DECISION_RULES_V1_KO.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
engine_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_engine.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

stopifnot(
  grepl('constructType: "unspecified"', nodes_source, fixed = TRUE),
  grepl('latent.constructType = "unspecified"', canvas_source, fixed = TRUE),
  grepl('"unspecified", ko ?', canvas_source, fixed = TRUE),
  grepl('"Not sure (show guidance)"', canvas_source, fixed = TRUE),
  grepl('"construct_type_unspecified"', canvas_source, fixed = TRUE),
  grepl('"formative_common_factor"', canvas_source, fixed = TRUE),
  grepl('"composite_in_covariance_engine"', canvas_source, fixed = TRUE),
  grepl('"factor_weighting_ignored"', canvas_source, fixed = TRUE),
  grepl("structural_canvas_validate_construct_specification", engine_source, fixed = TRUE),
  grepl("Formative indicator relationships are incompatible", engine_source, fixed = TRUE),
  grepl("does not estimate composite constructs", engine_source, fixed = TRUE),
  grepl("`.stmodel`", spec_source, fixed = TRUE),
  grepl("## 1단계: Construct Specification", spec_source, fixed = TRUE)
)

cat("SEM construct specification validations passed.\n")
