all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_regression_coefficients.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))
library(shiny)
library(htmltools)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

message("Checking regression coefficient variable labels...")
table <- data.frame(
  Term = c("(Intercept)", "sexFemale", "educationMiddle", "educationElementary"),
  B = c(4.13, -.212, -.230, -.410),
  Boot_SE = c(.277, .278, .104, .307),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
variable_info <- data.frame(
  name = c("y", "sex", "education"),
  var_label = c("Outcome", "Gender", "Education"),
  role = "",
  measurement = c("continuous", "binary", "category"),
  stringsAsFactors = FALSE
)
category_table <- data.frame(
  name = c("sex", "education"),
  reference = c("Male", "College"),
  value_1 = c("Male", "College"),
  label_1 = c("Male", "College"),
  value_2 = c("Female", "Middle"),
  label_2 = c("Female", "Middle"),
  value_3 = c("", "Elementary"),
  label_3 = c("", "Elementary"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

output <- coefficient_output_table_with_context(
  table,
  predictors = c("sex", "education"),
  include_references = TRUE,
  variable_info = variable_info,
  refs = regression_reference_values_static(category_table),
  value_labels = category_value_label_lookup_static(category_table),
  labels = character(0),
  category_table = category_table
)

expect_true(all(c("Gender:Male", "Gender:Female", "Education:College", "Education:Middle", "Education:Elementary") %in% output$Term), "Expected reference and coefficient rows to use variable labels")
expect_true(!any(grepl("^(sex|education):", output$Term)), "Expected no raw variable names when labels are available")

message("Checking regression guard conditions...")
guard_data <- data.frame(
  y_ok = c(1, 2, 3, 5, 4, 7),
  y_constant = rep(3, 6),
  x = c(1, 2, 3, 4, 5, 6),
  x_constant = rep(1, 6),
  stringsAsFactors = FALSE
)
guard_info <- data.frame(
  name = c("y_ok", "y_constant", "x", "x_constant"),
  var_label = c("Valid outcome", "Constant outcome", "Predictor", "Constant predictor"),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)
guard_prepared <- prepare_regression_analysis_results(
  guard_data,
  dependents = c("y_ok", "y_constant"),
  predictors = c("x", "x_constant"),
  variable_info = guard_info
)
expect_true(length(guard_prepared$results) == 0, "Expected all models with a constant predictor to be skipped")
expect_true(is.data.frame(attr(guard_prepared$results, "skipped")) && nrow(attr(guard_prepared$results, "skipped")) >= 1, "Expected skipped regression models to be attached")
expect_true(any(grepl("Constant predictor", attr(guard_prepared$results, "skipped")$Message, fixed = TRUE)), "Expected constant predictor guard message")

partial_prepared <- prepare_regression_analysis_results(
  guard_data,
  dependents = c("y_ok", "y_constant"),
  predictors = "x",
  variable_info = guard_info
)
expect_true(length(partial_prepared$results) == 1L, "Expected valid regression model to continue when another dependent variable is skipped")
expect_true(any(grepl("no variance", attr(partial_prepared$results, "skipped")$Message, fixed = TRUE)), "Expected constant dependent variable to be skipped")
panel_html <- as.character(htmltools::renderTags(regression_results_panel(partial_prepared$results, variable_table = guard_info))$html)
expect_true(grepl("<h3>Warnings / skipped models</h3>", panel_html, fixed = TRUE), "Expected skipped regression section to render")
guard_xlsx <- tempfile(fileext = ".xlsx")
save_analysis_excel_file(partial_prepared$results, guard_xlsx, variable_table = guard_info)
expect_true("Skipped models" %in% openxlsx::getSheetNames(guard_xlsx), "Expected skipped regression Excel sheet")

message("Checking regression saved Result / Word table parity...")
regression_data <- data.frame(
  y = c(1, 2, 3, 4, 5, 6, 7, 8),
  sex = rep(c("male", "female"), 4),
  age = rep(c("20s", "30s"), each = 4),
  x = c(2, 3, 4, 5, 6, 7, 8, 9),
  stringsAsFactors = FALSE
)
regression_info <- data.frame(
  name = c("y", "sex", "age", "x"),
  var_label = c("QoL", "\uc131\ubcc4", "\uc5f0\ub839\ub300", "x"),
  role = "",
  measurement = c("continuous", "binary", "category", "continuous"),
  stringsAsFactors = FALSE
)
regression_categories <- data.frame(
  name = c("sex", "age"),
  var_label = c("\uc131\ubcc4", "\uc5f0\ub839\ub300"),
  reference = c("male", "20s"),
  value_1 = c("male", "20s"),
  label_1 = c("male", "20\ub300"),
  value_2 = c("female", "30s"),
  label_2 = c("female", "30\ub300"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
regression_prepared <- prepare_regression_analysis_results(
  regression_data,
  dependents = "y",
  predictors = c("sex", "age", "x"),
  variable_info = regression_info,
  reference_values = regression_reference_values_static(regression_categories)
)
regression_html <- saved_analysis_results_html(
  regression_prepared$results,
  variable_table = regression_info,
  category_table = regression_categories,
  refs = regression_reference_values_static(regression_categories),
  value_labels = category_value_label_lookup_static(regression_categories),
  show_f2 = TRUE,
  show_vif = TRUE
)
expect_true(grepl("\uc131\ubcc4:male", regression_html, fixed = TRUE), "Expected saved HTML to include the same categorical reference row as the on-screen regression table")
expect_true(grepl("\uc131\ubcc4:female", regression_html, fixed = TRUE), "Expected saved HTML to use value labels for categorical coefficient rows")
entry <- list(title = "Regression", saved_at = "2026-05-29 00:00:00", html = regression_html)
docx_tables <- result_entry_tables(entry)
coefficient_tables <- Filter(function(item) grepl("coefficient-table", as.character(item$class %||% "")), docx_tables)
expect_true(length(coefficient_tables) >= 1L, "Expected saved regression entry to expose a coefficient table for Word export")
docx_body <- coefficient_tables[[1]]$docx$body
expect_true(any(docx_body[[1]] == "\uc131\ubcc4:male"), "Expected Word payload to retain categorical reference rows")
expect_true(any(docx_body[[1]] == "\uc5f0\ub839\ub300:20\ub300"), "Expected Word payload to retain category reference labels")
fit_rows <- apply(docx_body, 1, function(row) any(grepl("R\u00b2\\(adj\\. R\u00b2\\)", row)))
expect_true(any(fit_rows), "Expected Word payload to retain the displayed single-line model fit row")
fit_row <- docx_body[which(fit_rows)[[1]], , drop = FALSE]
fit_values <- unique(as.character(fit_row[nzchar(as.character(fit_row))]))
expect_true(length(fit_values) == 1L, "Expected Word payload to keep the model fit row as one merged display value")
expect_true(!any(as.character(fit_row) %in% c("F", "p", "R\u00b2", "adj. R\u00b2")), "Expected Word payload not to split the model fit row into separate F, p, R2, and adjusted R2 cells")
expect_true(!isTRUE(result_docx_wide_table(coefficient_tables[[1]])), "Expected ordinary regression coefficient tables to stay in a portrait Word section")

message("All regression coefficient validations passed.")
