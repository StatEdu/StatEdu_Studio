source(file.path("R", "utils.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path("R", "setup_custom_model_canvas_snapshot.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_exports.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_ui.R"), encoding = "UTF-8")

ui_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_ui.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
export_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_exports.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

assert_close <- function(actual, expected, tolerance = 1e-8, label = "value") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("%s mismatch: actual=%s expected=%s", label, paste(actual, collapse = ","), paste(expected, collapse = ",")))
  }
}
