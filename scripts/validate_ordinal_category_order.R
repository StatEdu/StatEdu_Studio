all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_ordinal_category_order.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(sprintf("%s mismatch: actual=%s expected=%s", label, paste(actual, collapse = ","), paste(expected, collapse = ",")), call. = FALSE)
  }
}

expect_close <- function(actual, expected, tolerance = 1e-10, label = "value") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance))) {
    stop(sprintf("%s mismatch: actual=%0.12f expected=%0.12f", label, actual, expected), call. = FALSE)
  }
}

ordered_levels <- c("Low", "Mid", "High")
category_table <- data.frame(
  name = c("ord", "ord2", "item1", "item2", "item3", "r1", "r2"),
  var_label = c("Ordinal", "Ordinal 2", "Item 1", "Item 2", "Item 3", "Rater 1", "Rater 2"),
  measurement = "ordered",
  reference = "Low",
  reference_label = "Low",
  value_1 = "Low",
  label_1 = "Low",
  value_2 = "Mid",
  label_2 = "Mid",
  value_3 = "High",
  label_3 = "High",
  value_4 = "",
  label_4 = "",
  value_5 = "",
  label_5 = "",
  value_6 = "",
  label_6 = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

data <- data.frame(
  ord = rep(ordered_levels, 20),
  ord2 = rep(ordered_levels, 20),
  score = rep(1:3, 20),
  item1 = rep(ordered_levels, 20),
  item2 = rep(ordered_levels, 20),
  item3 = rep(c("Low", "Mid", "Mid", "High", "High", "Low"), 10),
  r1 = rep(ordered_levels, 20),
  r2 = rep(c("Low", "Mid", "Mid", "High", "High", "Low"), 10),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = c("ordered", "ordered", "continuous", "ordered", "ordered", "ordered", "ordered", "ordered"),
  stringsAsFactors = FALSE
)

message("Checking category_table-driven ordinal level order...")
expect_identical(
  frequency_value_order(data$ord, name = "ord", category_table = category_table),
  ordered_levels,
  "frequency_value_order"
)
expect_identical(
  as.numeric(frequency_ordered_score(c("High", "Low", "Mid"), name = "ord", category_table = category_table)),
  c(3, 1, 2),
  "frequency_ordered_score"
)

message("Checking correlation ordinal scoring...")
cor_result <- prepare_correlation_results(
  data,
  variables = c("ord", "score"),
  variable_info = variable_info,
  category_table = category_table,
  options = list(continuous_method = "spearman", normality = FALSE)
)
expect_close(cor_result$correlation_matrix["Ordinal", "score"], 1, label = "ordinal Spearman correlation")

message("Checking crosstab ordinal row and column order...")
ct_result <- prepare_crosstab_results(
  data,
  "ord",
  "ord2",
  variable_info = variable_info,
  category_table = category_table,
  options = list(trend = TRUE)
)
expect_identical(rownames(ct_result$table), ordered_levels, "crosstab row levels")
expect_identical(colnames(ct_result$table), ordered_levels, "crosstab column levels")

message("Checking reliability ordinal scoring...")
rel_matrix <- reliability_numeric_matrix(data, c("item1", "item2", "item3"), "ordered", category_table = category_table)
expect_identical(as.numeric(rel_matrix$item1[1:3]), c(1, 2, 3), "reliability item1 scores")
expect_identical(as.numeric(rel_matrix$item3[1:6]), c(1, 2, 2, 3, 3, 1), "reliability item3 scores")

message("Checking inter-rater ordinal category order...")
ia_result <- prepare_interrater_agreement_results(
  data,
  variables = c("r1", "r2"),
  variable_info = variable_info,
  category_table = category_table,
  options = list(weight = "linear")
)
expect_identical(ia_result$categories, ordered_levels, "interrater categories")

message("Ordinal category order validation passed.")
