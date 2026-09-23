all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_survival_complex_screen_table_contract.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
}

source("R/app_bootstrap.R")
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

render_html <- function(tag) paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")

fixed_count <- function(text, pattern) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(hits) == 1L && hits[[1]] == -1L) 0L else length(hits)
}

expect_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

expect_one_table_per_sheet <- function(html, label) {
  sheets <- fixed_count(html, 'data-result-table-sheet="true"')
  tables <- fixed_count(html, "<table")
  expect_true(sheets > 0L, paste(label, "must render at least one result-table sheet."))
  expect_true(identical(sheets, tables), sprintf("%s must render one table per sheet (sheets=%d, tables=%d).", label, sheets, tables))
}

old_language <- getOption("statedu.app_language", NULL)
options(statedu.app_language = "ko")
on.exit({
  if (is.null(old_language)) options(statedu.app_language = NULL) else options(statedu.app_language = old_language)
}, add = TRUE)

message("Checking survival table contracts...")
small_appendix <- data.frame(Item = c("Status", "Method"), Value = c("Pass", "KM"), check.names = FALSE)
small_html <- render_html(survival_simple_table(small_appendix, table_role = "appendix", table_language = "ko"))
expect_one_table_per_sheet(small_html, "Survival appendix table")
expect_true(grepl('data-result-table-role="appendix"', small_html, fixed = TRUE), "Survival diagnostic tables must be appendix tables.")
expect_true(grepl('data-result-table-language="ko"', small_html, fixed = TRUE), "Survival appendix tables must follow the Korean UI language.")
expect_true(grepl('data-result-table-orientation="portrait"', small_html, fixed = TRUE), "A compact survival table must use B5 portrait.")
expect_true(grepl("font-size:12px", small_html, fixed = TRUE) && grepl("font-size:11px", small_html, fixed = TRUE), "Survival body/header sizes must be 12/11 px.")

wide_table <- as.data.frame(stats::setNames(replicate(10L, c(".12", ".34"), simplify = FALSE), paste0("Statistic ", seq_len(10L))), check.names = FALSE)
wide_html <- render_html(survival_simple_table(
  wide_table,
  table_role = "main",
  table_language = "ko",
  note_line = survival_main_note(abbreviations = "CI = confidence interval")
))
expect_one_table_per_sheet(wide_html, "Survival main table")
expect_true(grepl('data-result-table-role="main"', wide_html, fixed = TRUE), "Core survival tables must be main tables.")
expect_true(grepl('data-result-table-language="en"', wide_html, fixed = TRUE), "Main survival tables must remain English.")
expect_true(grepl('data-result-table-orientation="landscape"', wide_html, fixed = TRUE), "A genuinely wide survival table must use B5 landscape.")
expect_true(grepl("Note. CI = confidence interval.", wide_html, fixed = TRUE), "Main survival notes must use concise SCI Note formatting.")

fixture_path <- file.path(repo_root, "scripts", "fixtures", "survival_validation.csv")
survival_fixture <- utils::read.csv(fixture_path, check.names = FALSE)
km_result <- prepare_km_analysis_result(
  data = survival_fixture,
  time = "time",
  event = "status",
  group = "sex",
  event_value = "1",
  rate_times = "100, 200, 400",
  output_tables = c("survival_table", "survival_time"),
  plot_types = character(0),
  plot_versions = character(0)
)
km_html <- render_html(survival_km_results_panel(km_result, plot_output_ids = list(character(0)), language = "ko"))
expect_one_table_per_sheet(km_html, "Kaplan-Meier result")
expect_true(grepl('data-result-table-role="main"', km_html, fixed = TRUE), "Kaplan-Meier core estimates must be main tables.")
expect_true(grepl('data-result-table-role="appendix"', km_html, fixed = TRUE), "Kaplan-Meier audit and diagnostic tables must be appendix tables.")
expect_true(grepl('data-result-table-language="en"', km_html, fixed = TRUE) && grepl('data-result-table-language="ko"', km_html, fixed = TRUE), "Kaplan-Meier main/appendix languages must be separated.")

cox_result <- prepare_cox_analysis_result(
  data = survival_fixture,
  time = "time",
  event = "status",
  covariates = c("age", "sex"),
  event_value = "1"
)
cox_html <- render_html(survival_cox_results_panel(cox_result, language = "ko"))
expect_one_table_per_sheet(cox_html, "Cox regression result")
expect_true(grepl('survival-cox-result-table', cox_html, fixed = TRUE), "The Cox journal table must be present.")
expect_true(grepl('data-result-table-role="main"', cox_html, fixed = TRUE) && grepl('data-result-table-language="en"', cox_html, fixed = TRUE), "The Cox journal table must remain English.")
expect_true(grepl('data-result-table-role="appendix"', cox_html, fixed = TRUE) && grepl('data-result-table-language="ko"', cox_html, fixed = TRUE), "Cox diagnostics must be Korean appendix tables.")

set.seed(20260826)
competing_data <- data.frame(
  time = stats::rexp(120L, rate = .08),
  status = c(
    sample(c(0L, 1L, 2L), 60L, replace = TRUE, prob = c(.25, .55, .20)),
    sample(c(0L, 1L, 2L), 60L, replace = TRUE, prob = c(.25, .30, .45))
  ),
  group = rep(c("A", "B"), each = 60L)
)
competing_result <- prepare_competing_risk_result(
  competing_data,
  time = "time",
  event = "status",
  event_of_interest = "1",
  censored_value = "0",
  competing_values = "2",
  group = "group",
  rate_times = "0, 5, 10"
)
competing_html <- render_html(survival_competing_results_panel(competing_result, language = "ko"))
expect_one_table_per_sheet(competing_html, "Competing-risks result")
expect_true(grepl('data-result-table-role="main"', competing_html, fixed = TRUE) && grepl('data-result-table-language="en"', competing_html, fixed = TRUE), "Competing-risk core inference must remain in English main tables.")
expect_true(grepl('data-result-table-role="appendix"', competing_html, fixed = TRUE) && grepl('data-result-table-language="ko"', competing_html, fixed = TRUE), "Competing-risk audit and diagnostics must follow the Korean UI language.")
expect_true(grepl("집단·원인별 사건 수", competing_html, fixed = TRUE) && grepl("Cumulative incidence at selected time points", competing_html, fixed = TRUE), "Competing-risk appendix and main titles must use their assigned languages.")

message("Checking complex-sample table contracts...")
summary_input <- list(
  p_run = 1L,
  p_strata = "", p_cluster = "psu", p_weight = "wt", p_fpc = "",
  p_variance_method = "auto", p_lonely_psu = "adjust",
  p_use_replicate_weights = FALSE, p_replicate_weights = character(0),
  p_replicate_type = "auto", p_replicate_combined_weights = FALSE,
  p_subpopulation = "", p_subpopulation_condition = "",
  p_subpopulation_condition_type = "equals", p_subpopulation_condition_value = ""
)
variable_info <- data.frame(
  name = c("x", "psu", "wt"),
  var_label = c("Outcome", "Primary sampling unit", "Sampling weight"),
  measurement = c("continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)
summary_html <- render_html(complex_sample_result_panel(
  prefix = "p",
  target_specs = list(list(key = "selected")),
  target_values = list(selected = "x"),
  input = summary_input,
  data = NULL,
  analysis_type = "frequencies",
  variable_table = variable_info,
  language = "ko"
))
expect_one_table_per_sheet(summary_html, "Complex-sample setup summary")
expect_true(fixed_count(summary_html, 'data-result-table-sheet="true"') == 2L, "Complex-sample variable and design summaries must be separate sheets.")
expect_true(!grepl('data-result-table-role="main"', summary_html, fixed = TRUE), "Complex-sample setup summaries must not be journal main tables.")
expect_true(grepl('data-result-table-language="ko"', summary_html, fixed = TRUE), "Complex-sample setup summaries must follow the UI language.")

crosstab_item <- list(
  row_var = "row_group",
  col_var = "column_group",
  weighted_tab = matrix(c(10, 15, 20, 25), nrow = 2L, dimnames = list(c("A", "B"), c("C", "D"))),
  unweighted_tab = matrix(c(8, 12, 16, 20), nrow = 2L, dimnames = list(c("A", "B"), c("C", "D"))),
  test = NULL,
  percent_ci = NULL,
  trend = NULL,
  missing_note = "",
  design_note = "Survey design applied."
)
crosstab_html <- render_html(complex_sample_crosstab_group_display(
  list(crosstab_item),
  "column_group",
  options = list(crosstab_percent_basis = "row", crosstab_test_method = "F", show_weighted_n = TRUE)
))
expect_one_table_per_sheet(crosstab_html, "Complex-sample crosstab")
expect_true(grepl('data-result-table-role="main"', crosstab_html, fixed = TRUE), "Complex-sample inferential crosstabs must be main tables.")
expect_true(grepl('data-result-table-language="en"', crosstab_html, fixed = TRUE), "Complex-sample main tables must remain English.")
expect_true(grepl('data-result-table-orientation="portrait"', crosstab_html, fixed = TRUE), "A compact two-level complex-sample crosstab must use B5 portrait.")
expect_true(grepl("min-width:566px", crosstab_html, fixed = TRUE), "Grouped two-level crosstabs must retain their calculated 566 px intrinsic width.")
expect_true(grepl("font-size:12px", crosstab_html, fixed = TRUE), "Complex-sample table bodies must use the shared 12 px size.")

single_crosstab_html <- render_html(complex_sample_crosstab_display(
  crosstab_item$weighted_tab,
  crosstab_item$unweighted_tab,
  crosstab_item$row_var,
  crosstab_item$col_var,
  test = NULL,
  options = list(crosstab_percent_basis = "row", crosstab_test_method = "F", show_weighted_n = TRUE)
))
expect_one_table_per_sheet(single_crosstab_html, "Single complex-sample crosstab")
expect_true(grepl('data-result-table-orientation="portrait"', single_crosstab_html, fixed = TRUE), "A compact single two-level crosstab must use B5 portrait.")
expect_true(grepl("min-width:530px", single_crosstab_html, fixed = TRUE), "Single two-level crosstabs must retain their calculated 530 px intrinsic width.")

wide_crosstab_item <- crosstab_item
wide_crosstab_item$weighted_tab <- matrix(
  seq_len(8L), nrow = 2L,
  dimnames = list(c("A", "B"), c("C", "D", "E", "F"))
)
wide_crosstab_item$unweighted_tab <- wide_crosstab_item$weighted_tab
wide_crosstab_html <- render_html(complex_sample_crosstab_group_display(
  list(wide_crosstab_item),
  "column_group",
  options = list(crosstab_percent_basis = "row", crosstab_test_method = "F", show_weighted_n = TRUE)
))
expect_one_table_per_sheet(wide_crosstab_html, "Wide complex-sample crosstab")
expect_true(grepl('data-result-table-orientation="landscape"', wide_crosstab_html, fixed = TRUE), "A genuinely wide complex-sample crosstab must use B5 landscape.")

design_fixture <- list(meta = list(
  original_n = 120L,
  analysis_n = 108L,
  design_excluded_n = 7L,
  subpopulation_excluded_n = 5L,
  subpopulation_missing_n = 0L,
  lonely_psu = "adjust",
  design_type = "taylor",
  variance_method = "taylor",
  replicate_count = 0L,
  replicate_combined_weights = FALSE,
  fpc_selected = FALSE,
  fpc_used = FALSE
))
main_design_note <- complex_sample_design_note(design_fixture, role = "main", language = "ko")
appendix_design_note <- complex_sample_design_note(design_fixture, role = "appendix", language = "ko")
expect_true(grepl("Survey design was constructed", main_design_note, fixed = TRUE), "A main-table survey-design note must remain English.")
expect_true(grepl("조사설계는", appendix_design_note, fixed = TRUE), "A Korean appendix survey-design note must be localized.")
expect_true(!grepl("Survey design was constructed", appendix_design_note, fixed = TRUE), "A Korean appendix survey-design note must not leak the English dynamic sentence.")

custom_fixture <- list(
  overview = data.frame(Item = "Analysis", Value = "Complex Samples Mediation / Moderation", check.names = FALSE),
  syntax = data.frame(Type = "Survey design", Equation = "Design", Syntax = "ids=psu; weights=wt", check.names = FALSE),
  coefficients = data.frame(Equation = "y", Term = "x", Estimate = .25, SE = .08, Statistic = 3.12, `p-value` = .003, check.names = FALSE),
  effects = data.frame(X = "x", Y = "y", Effect = "Direct", Path = "x → y", Condition = "", Estimate = .25, SE = .08, `Lower CI` = .09, `Upper CI` = .41, `p-value` = .003, check.names = FALSE),
  design = design_fixture
)
custom_html <- render_html(complex_sample_custom_model_result_ui(custom_fixture, language = "ko"))
expect_one_table_per_sheet(custom_html, "Complex-sample custom-model result")
expect_true(grepl("복합표본 설계", custom_html, fixed = TRUE), "The custom-model design appendix title must follow the Korean UI language.")
expect_true(grepl("조사설계는", custom_html, fixed = TRUE), "The custom-model design appendix must localize its dynamic survey-design explanation.")
expect_true(!grepl("Survey design was constructed", custom_html, fixed = TRUE), "The Korean custom-model appendix must not leak the English dynamic survey-design note.")
expect_true(!grepl("analysis-result-notes", custom_html, fixed = TRUE), "Complex-sample custom-model auxiliary prose must stay inside a contracted appendix sheet.")
expect_true(grepl('data-result-table-role="main"', custom_html, fixed = TRUE) && grepl('data-result-table-language="en"', custom_html, fixed = TRUE), "Custom-model journal tables must remain English main tables.")
expect_true(grepl('data-result-table-role="appendix"', custom_html, fixed = TRUE) && grepl('data-result-table-language="ko"', custom_html, fixed = TRUE), "Custom-model auxiliary tables must be Korean appendix tables.")

style_css <- paste(readLines(file.path(repo_root, "www", "style.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
expect_true(grepl("--result-sheet-note-font-size: 11px", style_css, fixed = TRUE), "The shared screen-table note contract must use 11 px notes.")
expect_true(grepl(".result-table-sheet .coefficient-note", style_css, fixed = TRUE), "The shared 11 px note rule must be scoped inside result-table sheets.")

message("Survival and complex-sample screen table contract validations passed.")
