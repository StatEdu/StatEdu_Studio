all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_regression_coefficients.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))

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

message("All regression coefficient validations passed.")
