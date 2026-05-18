all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_crosstabs.R"
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

expect_close <- function(actual, expected, tolerance = 1e-6, label = "") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("%s expected %s, got %s", label, paste(expected, collapse = ", "), paste(actual, collapse = ", ")), call. = FALSE)
  }
}

variable_info <- data.frame(
  name = c("group", "outcome", "dose", "grade"),
  measurement = c("category", "binary", "ordered", "ordered"),
  stringsAsFactors = FALSE
)

data <- data.frame(
  group = c("A", "A", "A", "B", "B", "B", "B", "A"),
  outcome = c(1, 0, 1, 0, 0, 1, 0, 1),
  dose = c(1, 1, 2, 2, 3, 3, 3, 2),
  grade = c(1, 2, 2, 3, 3, 2, 1, 3),
  stringsAsFactors = FALSE
)

message("Checking Fisher exact fallback rule...")
fisher_result <- prepare_crosstab_results(
  data,
  row_var = "group",
  col_var = "outcome",
  variable_info = variable_info,
  options = list(row_percent = TRUE, column_percent = FALSE, total_percent = FALSE, trend = FALSE)
)
expect_true(identical(fisher_result$association$method, "Fisher's exact test"), "Expected Fisher's exact test")
expect_close(fisher_result$association$p, 0.4857143, tolerance = 1e-6, label = "Fisher p-value")
expect_close(fisher_result$effect_sizes$Estimate[fisher_result$effect_sizes$Effect == "Cramer's V"], 0.500, tolerance = 1e-3, label = "Cramer's V")

message("Checking 2 x k Armitage trend analysis...")
trend_2xk <- prepare_crosstab_results(
  data,
  row_var = "outcome",
  col_var = "dose",
  variable_info = variable_info,
  options = list(row_percent = TRUE, column_percent = FALSE, total_percent = FALSE, trend = TRUE)
)
expect_true(identical(trend_2xk$trend$method, "Armitage trend analysis"), "Expected Armitage trend analysis for 2 x k")
expect_close(trend_2xk$trend$statistic, 0.2051282, tolerance = 1e-6, label = "2 x k trend statistic")
expect_close(trend_2xk$trend$odds_ratio, 0.6595042, tolerance = 1e-6, label = "Trend odds ratio")

message("Checking ordered x ordered trend analysis...")
trend_ordered <- prepare_crosstab_results(
  data,
  row_var = "dose",
  col_var = "grade",
  variable_info = variable_info,
  options = list(row_percent = TRUE, column_percent = FALSE, total_percent = FALSE, trend = TRUE)
)
expect_true(identical(trend_ordered$trend$method, "Armitage trend analysis"), "Expected Armitage trend analysis for ordered x ordered")
expect_close(trend_ordered$trend$gamma, 0.2, tolerance = 1e-6, label = "Gamma")

message("Checking result table rendering...")
display_data <- data.frame(
  sex = c(rep(0, 342), rep(1, 58)),
  group = c(rep(0, 205), rep(1, 137), rep(0, 24), rep(1, 34))
)
display_info <- data.frame(
  name = c("sex", "group"),
  measurement = c("binary", "binary"),
  stringsAsFactors = FALSE
)
display_result <- prepare_crosstab_results(
  display_data,
  row_var = "sex",
  col_var = "group",
  variable_info = display_info,
  options = list(row_percent = TRUE, column_percent = FALSE, total_percent = FALSE, trend = FALSE)
)
display_html <- renderTags(crosstab_main_table_ui(display_result))$html
display_notes <- renderTags(crosstab_notes_ui(display_result))$html
expect_true(grepl("342\\(85.5\\)", display_html), "Expected n column to include row total with grand-total percent")
expect_true(grepl("205\\(59.9\\)", display_html), "Expected cross-tabulation cells to use compact n(percent) format")
expect_true(grepl("1\\(\u00A05.0\\)", crosstab_cell_text(1, 5), fixed = FALSE), "Expected compact n(percent) to pad percentages under 10")
expect_true(!grepl("OR=", display_html), "Expected ES cell to omit effect size type")
expect_true(grepl("2.120", display_html), "Expected ES cell to include numeric estimate")
expect_true(grepl("ES = odds ratio", display_notes), "Expected ES type note")

split_display_result <- display_result
split_display_result$options$split_count_percent <- TRUE
split_display_html <- renderTags(crosstab_main_table_ui(split_display_result))$html
expect_true(grepl("crosstab-percent-only-cell", split_display_html, fixed = TRUE), "Expected split n and percent cells")
expect_true(!grepl("205\\(59.9\\)", split_display_html), "Expected split display to avoid compact n(percent) cells")
trend_display_html <- renderTags(crosstab_main_table_ui(trend_2xk))$html
trend_note_html <- renderTags(crosstab_single_result_ui(trend_2xk))$html
expect_true(grepl("p for trend", trend_display_html, fixed = TRUE), "Expected p for trend column when trend analysis is requested")
expect_true(grepl(crosstab_format_p(trend_2xk$trend$p), trend_display_html, fixed = TRUE), "Expected trend p-value in the result table")
expect_true(grepl("Cochran-Armitage trend test was used for p for trend.", trend_note_html, fixed = TRUE), "Expected Cochran-Armitage trend note")
ordered_trend_note_html <- renderTags(crosstab_single_result_ui(trend_ordered))$html
expect_true(grepl("Score-based ordered-by-ordered trend association was used for p for trend.", ordered_trend_note_html, fixed = TRUE), "Expected score-based ordered trend note")
mixed_trend_notes <- crosstab_method_footnotes(list(trend_2xk, trend_ordered))
expect_true(sum(mixed_trend_notes$type == "trend") == 2, "Expected different trend methods to receive separate notes")
single_result_html <- renderTags(crosstab_single_result_ui(display_result))$html
expect_true(!grepl("Tests:", single_result_html, fixed = TRUE), "Expected single result to omit the separate tests table")
expect_true(!grepl("<h3>Effect size</h3>", single_result_html, fixed = TRUE), "Expected single result to omit the separate effect size table")

message("Checking setup listbox height...")
setup_state <- crosstab_setup_state(
  selected_names = c(display_info$name, "dose"),
  variable_table = rbind(display_info, data.frame(name = "dose", measurement = "ordered", stringsAsFactors = FALSE)),
  row_var = c("sex", "dose"),
  col_var = "group",
  selected_row = "dose",
  selected_col = "group",
  split_count_percent = TRUE
)
setup_html <- renderTags(crosstab_setup_panel(setup_state))$html
expect_true(grepl('data-input-id="crosstab_row"', setup_html, fixed = TRUE), "Expected row variable listbox")
expect_true(grepl('data-input-id="crosstab_col"', setup_html, fixed = TRUE), "Expected column variable listbox")
expect_true(identical(setup_state$row_vars, c("sex", "dose")), "Expected multiple row variables")
expect_true(identical(setup_state$col_vars, "group"), "Expected column variable")
expect_true(!"sex" %in% setup_state$available, "Expected assigned row variable to leave available list")
expect_true(!"group" %in% setup_state$available, "Expected assigned column variable to leave available list")
expect_true(grepl("regression-target-column", setup_html, fixed = TRUE), "Expected regression-style target column")
expect_true(grepl("regression-dependent-panel", setup_html, fixed = TRUE), "Expected regression-style row variable panel")
expect_true(grepl("regression-independent-panel", setup_html, fixed = TRUE), "Expected regression-style column variable panel")
expect_true(length(gregexpr("height:168px", setup_html, fixed = TRUE)[[1]]) >= 2, "Expected row and column variable panels to use size 7 height")
expect_true(grepl("crosstab_split_count_percent", setup_html, fixed = TRUE), "Expected separate n and percent option")
expect_true(grepl("crosstab_row_up", setup_html, fixed = TRUE), "Expected row variable move-up button")
expect_true(grepl("crosstab_row_down", setup_html, fixed = TRUE), "Expected row variable move-down button")
expect_true(grepl("crosstab_col_up", setup_html, fixed = TRUE), "Expected column variable move-up button")
expect_true(grepl("crosstab_col_down", setup_html, fixed = TRUE), "Expected column variable move-down button")
move_check <- move_order_item(c("sex", "dose", "group"), c("dose", "group"), "up")
expect_true(identical(move_check$order, c("dose", "group", "sex")), "Expected selected crosstab variables to move up as a block")

display_result_2 <- prepare_crosstab_results(
  data.frame(
    age_group = c(rep(1, 180), rep(2, 220)),
    group = display_data$group
  ),
  row_var = "age_group",
  col_var = "group",
  variable_info = data.frame(name = c("age_group", "group"), measurement = c("ordered", "binary"), stringsAsFactors = FALSE),
  options = list(row_percent = TRUE, column_percent = FALSE, total_percent = FALSE, trend = FALSE)
)
multi_result_html <- renderTags(crosstab_results_ui(list(display_result, display_result_2)))$html
expect_true(identical(length(gregexpr("Cross-tabulation:", multi_result_html, fixed = TRUE)[[1]]), 1L), "Expected one result panel per column variable")
expect_true(grepl("sex", multi_result_html, fixed = TRUE), "Expected first row variable in column-grouped table")
expect_true(grepl("age_group", multi_result_html, fixed = TRUE), "Expected second row variable in column-grouped table")
expect_true(length(gregexpr('class="crosstab-footnote-marker">1</sup>', multi_result_html, fixed = TRUE)[[1]]) >= 2, "Expected identical test methods to share the same p-value footnote marker")
expect_true(identical(length(gregexpr("1. Pearson chi-square test was used.", multi_result_html, fixed = TRUE)[[1]]), 1L), "Expected identical test method note to appear once")
expect_true(!grepl("Tests:", multi_result_html, fixed = TRUE), "Expected grouped result to omit the separate tests table")
expect_true(!grepl("<h3>Effect size</h3>", multi_result_html, fixed = TRUE), "Expected grouped result to omit the separate effect size table")

message("Checking result saving helpers...")
saved_html <- tempfile("crosstab_", fileext = ".html")
write_crosstab_results_html(list(display_result, display_result_2), saved_html)
expect_true(file.exists(saved_html) && file.info(saved_html)$size > 0, "Expected crosstab HTML export file")
saved_html_text <- paste(readLines(saved_html, warn = FALSE), collapse = "\n")
expect_true(grepl("Cross-tabulation", saved_html_text, fixed = TRUE), "Expected crosstab HTML export content")
saved_xlsx <- tempfile("crosstab_", fileext = ".xlsx")
save_crosstab_excel_file(list(display_result, display_result_2), saved_xlsx)
expect_true(file.exists(saved_xlsx) && file.info(saved_xlsx)$size > 0, "Expected crosstab Excel export file")
trend_xlsx <- tempfile("crosstab_trend_", fileext = ".xlsx")
save_crosstab_excel_file(list(trend_2xk), trend_xlsx)
expect_true(file.exists(trend_xlsx) && file.info(trend_xlsx)$size > 0, "Expected crosstab trend Excel export file")
trend_export <- crosstab_excel_group_table(list(trend_2xk))
expect_true("p for trend" %in% names(trend_export), "Expected p for trend in crosstab Excel export table")
expect_true(grepl("[0-9]$", trend_export[["p for trend"]][[1]]), "Expected p for trend Excel value to include a note marker")

message("All cross-tabulation validations passed.")
