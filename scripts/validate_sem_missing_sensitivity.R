`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path("R", "setup_custom_model_canvas_structural_distribution_diagnostics.R"), local = TRUE, encoding = "UTF-8")

base <- list(
  missing = "fiml",
  missing_diagnostics = list(available = TRUE, incomplete_n = 12L)
)
unassessed <- structural_canvas_missing_sensitivity_rows(base)
documented <- structural_canvas_missing_sensitivity_rows(c(base, list(
  missing_sensitivity_method = "delta_pattern_mixture",
  missing_sensitivity_details = "Delta scenarios of 0.2 and 0.5 SD did not change the direction or interval conclusion for the primary path."
)))
complete_data <- structural_canvas_missing_sensitivity_rows(list(
  missing = "fiml", missing_diagnostics = list(available = TRUE, incomplete_n = 0L)
))

stopifnot(
  identical(unassessed$Status, "Review"),
  identical(documented$Status, "Documented"),
  grepl("Delta/pattern-mixture", documented$`Sensitivity assessment`, fixed = TRUE),
  grepl("Not required", complete_data$Status, fixed = TRUE)
)

message("SEM missing-sensitivity validation passed.")
