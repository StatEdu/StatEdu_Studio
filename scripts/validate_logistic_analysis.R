all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_logistic_analysis.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

source("R/app_bootstrap.R")
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

set.seed(1001)

binary_data <- data.frame(
  y = factor(rep(0:1, each = 60)),
  group = factor(rep(1:3, 40)),
  x = rnorm(120)
)
binary_info <- data.frame(
  name = c("y", "group", "x"),
  measurement = c("binary", "category", "continuous"),
  stringsAsFactors = FALSE
)
binary_result <- prepare_logistic_analysis_results(binary_data, "y", c("group", "x"), variable_info = binary_info)
stopifnot(length(binary_result) == 1L)
stopifnot(identical(binary_result[[1]]$method, "Binary logistic regression"))
stopifnot(is.data.frame(binary_result[[1]]$coef_table))
stopifnot(all(c("OR", "LLCI", "ULCI", "p") %in% names(binary_result[[1]]$coef_table)))

category_table <- data.frame(
  name = c("y", "group"),
  var_label = c("Changed", "Treatment group"),
  reference = c("0", "2"),
  reference_label = c("No", "Middle"),
  value_1 = c("0", "1"),
  label_1 = c("No", "Low"),
  value_2 = c("1", "2"),
  label_2 = c("Yes", "Middle"),
  value_3 = c("", "3"),
  label_3 = c("", "High"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
refs <- logistic_reference_values_static(category_table)
stopifnot(identical(unname(refs["group"]), "2"))
referenced_result <- prepare_logistic_analysis_results(
  binary_data,
  "y",
  c("group", "x"),
  variable_info = binary_info,
  reference_values = refs
)
stopifnot(!any(grepl("Reference for group was not set", referenced_result[[1]]$notes, fixed = TRUE)))
rendered <- as.character(htmltools::renderTags(logistic_results_panel(
  referenced_result,
  variable_table = binary_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  show_mcfadden = TRUE,
  show_cox_snell = TRUE
))$html)
stopifnot(grepl("Middle", rendered, fixed = TRUE))
stopifnot(grepl("Low", rendered, fixed = TRUE) || grepl("High", rendered, fixed = TRUE))
stopifnot(grepl("Changed(Yes vs No)", rendered, fixed = TRUE))
stopifnot(grepl("Constant", rendered, fixed = TRUE))
stopifnot(grepl("<sup>2</sup>", rendered, fixed = TRUE))
stopifnot(grepl("McFadden", rendered, fixed = TRUE))
split_rendered <- as.character(htmltools::renderTags(logistic_results_panel(
  referenced_result,
  variable_table = binary_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  split_ci = TRUE
))$html)
stopifnot(grepl("LLCI", split_rendered, fixed = TRUE))
stopifnot(grepl("ULCI", split_rendered, fixed = TRUE))

saved_html <- saved_logistic_results_html(
  referenced_result,
  variable_table = binary_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  show_mcfadden = TRUE,
  show_cox_snell = TRUE,
  split_ci = TRUE
)
stopifnot(grepl("EasyFlow Statistics Logistic Regression Results", saved_html, fixed = TRUE))
stopifnot(grepl("LLCI", saved_html, fixed = TRUE))
html_file <- tempfile(fileext = ".html")
write_logistic_results_html(
  referenced_result,
  html_file,
  variable_table = binary_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  show_mcfadden = TRUE,
  show_cox_snell = TRUE,
  split_ci = TRUE
)
stopifnot(file.exists(html_file), file.info(html_file)$size > 0)
excel_file <- tempfile(fileext = ".xlsx")
save_logistic_excel_file(
  referenced_result,
  excel_file,
  variable_table = binary_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  show_mcfadden = TRUE,
  show_cox_snell = TRUE,
  split_ci = TRUE
)
stopifnot(file.exists(excel_file), file.info(excel_file)$size > 0)

ordered_data <- data.frame(
  y = ordered(rep(1:3, each = 50)),
  x = rnorm(150),
  block2 = rnorm(150)
)
ordered_info <- data.frame(
  name = c("y", "x", "block2"),
  measurement = c("ordered", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
ordered_result <- prepare_logistic_analysis_results(ordered_data, "y", "x", "block2", variable_info = ordered_info)
stopifnot(length(ordered_result) == 2L)
stopifnot(!is.null(ordered_result[[2]]$delta_r2))
stopifnot(!is.null(ordered_result[[1]]$parallel))

multinom_data <- data.frame(
  y = factor(rep(1:3, each = 50)),
  x = rnorm(150)
)
multinom_info <- data.frame(
  name = c("y", "x"),
  measurement = c("category", "continuous"),
  stringsAsFactors = FALSE
)
multinom_result <- prepare_logistic_analysis_results(multinom_data, "y", "x", variable_info = multinom_info)
stopifnot(length(multinom_result) == 1L)
stopifnot(identical(multinom_result[[1]]$method, "Multinomial logistic regression"))
stopifnot(is.data.frame(multinom_result[[1]]$coef_table))

guard_data <- data.frame(
  y_ok = factor(c(rep(0, 20), rep(1, 20))),
  y_one = factor(rep(1, 40)),
  x = rnorm(40),
  x_constant = rep(1, 40),
  group = factor(c(rep("A", 19), "B", rep("B", 20))),
  stringsAsFactors = FALSE
)
guard_info <- data.frame(
  name = c("y_ok", "y_one", "x", "x_constant", "group"),
  measurement = c("binary", "binary", "continuous", "continuous", "category"),
  stringsAsFactors = FALSE
)
guard_result <- prepare_logistic_analysis_results(
  guard_data,
  dependents = c("y_ok", "y_one"),
  block1 = "x",
  variable_info = guard_info
)
stopifnot(length(guard_result) == 1L)
stopifnot(is.data.frame(attr(guard_result, "skipped")))
stopifnot(any(grepl("fewer than two observed outcome levels", attr(guard_result, "skipped")$Message, fixed = TRUE)))
guard_html <- as.character(htmltools::renderTags(logistic_results_panel(guard_result, variable_table = guard_info))$html)
stopifnot(grepl("<h3>Warnings / skipped models</h3>", guard_html, fixed = TRUE))

constant_predictor_result <- prepare_logistic_analysis_results(
  guard_data,
  dependents = "y_ok",
  block1 = "x_constant",
  variable_info = guard_info
)
stopifnot(length(constant_predictor_result) == 0L)
stopifnot(any(grepl("Constant predictor", attr(constant_predictor_result, "skipped")$Message, fixed = TRUE)))

sparse_result <- prepare_logistic_analysis_results(
  guard_data,
  dependents = "y_ok",
  block1 = "group",
  variable_info = guard_info
)
stopifnot(length(sparse_result) == 1L)
stopifnot(any(grepl("Zero cell", sparse_result[[1]]$notes, fixed = TRUE)))

cat("Logistic analysis validation passed.\n")
