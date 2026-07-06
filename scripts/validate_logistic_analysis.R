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

count_fixed <- function(pattern, text) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(matches[[1]], -1L)) 0L else length(matches)
}

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

binary_hierarchical_data <- binary_data
binary_hierarchical_data$block2 <- rnorm(nrow(binary_hierarchical_data))
binary_hierarchical_info <- rbind(
  binary_info,
  data.frame(name = "block2", measurement = "continuous", stringsAsFactors = FALSE)
)
binary_hierarchical_result <- prepare_logistic_analysis_results(
  binary_hierarchical_data,
  "y",
  c("group", "x"),
  "block2",
  variable_info = binary_hierarchical_info,
  reference_values = refs
)
stopifnot(length(binary_hierarchical_result) == 2L)
binary_hierarchical_rows <- logistic_coefficient_rows(
  binary_hierarchical_result[[2]],
  variable_table = binary_hierarchical_info,
  category_table = category_table,
  split_ci = TRUE
)
stopifnot(length(binary_hierarchical_rows) > 0L)
stopifnot(any(vapply(binary_hierarchical_rows, function(row) any(nzchar(row$values[3:length(row$values)])), logical(1))))
binary_hierarchical_html <- as.character(htmltools::renderTags(logistic_results_panel(
  binary_hierarchical_result,
  variable_table = binary_hierarchical_info,
  category_table = category_table,
  split_ci = TRUE
))$html)
stopifnot(grepl("logistic-hierarchical-table", binary_hierarchical_html, fixed = TRUE))
stopifnot(grepl("Model 1", binary_hierarchical_html, fixed = TRUE))
stopifnot(grepl("Model 2", binary_hierarchical_html, fixed = TRUE))
stopifnot(grepl("LLCI", binary_hierarchical_html, fixed = TRUE))
stopifnot(grepl("ULCI", binary_hierarchical_html, fixed = TRUE))
stopifnot(!grepl("landscape-table-panel", binary_hierarchical_html, fixed = TRUE))
stopifnot(grepl("width:100% !important;min-width:0 !important;max-width:100% !important;table-layout:fixed;", binary_hierarchical_html, fixed = TRUE))

binary_three_step_result <- prepare_logistic_analysis_results(
  binary_hierarchical_data,
  "y",
  "group",
  "x",
  "block2",
  variable_info = binary_hierarchical_info,
  reference_values = refs
)
stopifnot(length(binary_three_step_result) == 3L)
binary_three_step_html <- as.character(htmltools::renderTags(logistic_results_panel(
  binary_three_step_result,
  variable_table = binary_hierarchical_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  split_ci = TRUE
))$html)
stopifnot(grepl("landscape-table-panel", binary_three_step_html, fixed = TRUE))
stopifnot(grepl("fit-width-hierarchical-table", binary_three_step_html, fixed = TRUE))
stopifnot(grepl("Model 3", binary_three_step_html, fixed = TRUE))
stopifnot(grepl("reference", binary_three_step_html, fixed = TRUE))
stopifnot(grepl("1.", binary_three_step_html, fixed = TRUE))
binary_three_step_table_html <- as.character(htmltools::renderTags(logistic_hierarchical_result_table(
  binary_three_step_result,
  variable_table = binary_hierarchical_info,
  category_table = category_table,
  show_b = TRUE,
  show_se = TRUE,
  split_ci = TRUE
))$html)
stopifnot(count_fixed(">VIF<", binary_three_step_table_html) == 1L)

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
stopifnot(grepl("StatEdu Studio Logistic Regression Results", saved_html, fixed = TRUE))
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
ordered_hier_html <- as.character(htmltools::renderTags(logistic_results_panel(
  ordered_result,
  variable_table = ordered_info,
  show_mcfadden = TRUE,
  show_cox_snell = TRUE,
  split_ci = FALSE
))$html)
stopifnot(grepl("logistic-hierarchical-table", ordered_hier_html, fixed = TRUE))
stopifnot(grepl("<div style=\"white-space:nowrap;\">Nagelkerke R", ordered_hier_html, fixed = TRUE))
stopifnot(grepl("<div style=\"white-space:nowrap;\">McFadden R", ordered_hier_html, fixed = TRUE))

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

multinom_data$block2 <- rnorm(nrow(multinom_data))
multinom_hier_info <- rbind(
  multinom_info,
  data.frame(name = "block2", measurement = "continuous", stringsAsFactors = FALSE)
)
multinom_hier_result <- prepare_logistic_analysis_results(multinom_data, "y", "x", "block2", variable_info = multinom_hier_info)
stopifnot(length(multinom_hier_result) == 2L)
multinom_hier_html <- as.character(htmltools::renderTags(logistic_results_panel(
  multinom_hier_result,
  variable_table = multinom_hier_info,
  show_b = TRUE,
  show_se = TRUE,
  split_ci = TRUE
))$html)
stopifnot(count_fixed("Constant", multinom_hier_html) >= 2L)
stopifnot(grepl("(2 vs 1)", multinom_hier_html, fixed = TRUE))
stopifnot(grepl("(3 vs 1)", multinom_hier_html, fixed = TRUE))

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
