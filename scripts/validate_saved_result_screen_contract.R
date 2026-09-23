all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) {
  sub("^--file=", "", file_arg[[1L]])
} else {
  "scripts/validate_saved_result_screen_contract.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
}

source(file.path(repo_root, "R", "app_bootstrap.R"), encoding = "UTF-8")
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

count_fixed <- function(text, pattern) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (length(hits) == 1L && hits[[1L]] == -1L) 0L else length(hits)
}

expect_model_overview_contract <- function(html, language, title) {
  expect_true(
    grepl(sprintf("<h3>%s</h3>", title), html, fixed = TRUE),
    sprintf("Saved regression Model overview title must follow the %s UI language", language)
  )
  overview_pattern <- paste0(
    "model-overview-panel[\\s\\S]*?data-result-table-role=\"appendix\"",
    "[\\s\\S]*?data-result-table-language=\"", language, "\""
  )
  expect_true(
    grepl(overview_pattern, html, perl = TRUE),
    sprintf("Saved regression Model overview must be a %s appendix table", language)
  )
}

expect_normality_contract <- function(html, language, title, header, value) {
  expect_true(
    grepl(sprintf("<h3>%s</h3>", title), html, fixed = TRUE),
    sprintf("Saved correlation Normality title must follow the %s UI language", language)
  )
  normality_pattern <- paste0(
    "correlation-result-section[\\s\\S]*?<h3>", title, "</h3>",
    "[\\s\\S]*?data-result-table-role=\"appendix\"",
    "[\\s\\S]*?data-result-table-language=\"", language, "\""
  )
  expect_true(
    grepl(normality_pattern, html, perl = TRUE),
    sprintf("Saved correlation Normality must be a %s appendix table", language)
  )
  expect_true(grepl(header, html, fixed = TRUE), sprintf("Saved correlation Normality must use the %s header", language))
  expect_true(grepl(value, html, fixed = TRUE), sprintf("Saved correlation Normality must use the %s result value", language))
}

old_language <- getOption("statedu.app_language")
on.exit(options(statedu.app_language = old_language), add = TRUE)

set.seed(20260826)
n <- 80L
regression_data <- data.frame(
  outcome = stats::rnorm(n),
  predictor = stats::rnorm(n),
  stringsAsFactors = FALSE
)
regression_data$outcome <- 0.4 + 0.7 * regression_data$predictor + regression_data$outcome
regression_info <- data.frame(
  name = names(regression_data),
  var_label = c("Outcome label", "Predictor label"),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)
regression_prepared <- prepare_regression_analysis_results(
  regression_data,
  dependents = "outcome",
  predictors = "predictor",
  variable_info = regression_info,
  residual_diagnostics = FALSE,
  auto_method = FALSE
)

correlation_data <- data.frame(
  normal = stats::rnorm(n),
  skewed = stats::rlnorm(n, sdlog = 1.4),
  stringsAsFactors = FALSE
)
correlation_info <- data.frame(
  name = names(correlation_data),
  var_label = c("Normal score", "Skewed score"),
  measurement = "continuous",
  stringsAsFactors = FALSE
)
correlation_result <- prepare_correlation_results(
  correlation_data,
  variables = names(correlation_data),
  variable_info = correlation_info,
  options = list(
    continuous_method = "auto",
    normality = TRUE,
    reason = TRUE,
    p_ci = FALSE,
    scatter_plot = FALSE,
    matrix_plot = FALSE
  )
)

message("Checking Korean saved-result appendix localization...")
options(statedu.app_language = "ko")
regression_ko <- saved_analysis_results_html(
  regression_prepared$results,
  variable_table = regression_info
)
expect_model_overview_contract(regression_ko, "ko", "모형 개요")
expect_true(!grepl("<h3>Model overview</h3>", regression_ko, fixed = TRUE), "English regression Model overview title leaked into Korean saved results")
expect_true(
  all(vapply(c("항목", "분석", "사유", "잔차 진단을 실행하지 않음"), grepl, logical(1), x = regression_ko, fixed = TRUE)),
  "Saved Korean regression overview headers or values were not localized"
)

correlation_ko <- saved_correlation_results_html(correlation_result)
expect_normality_contract(correlation_ko, "ko", "정규성", "왜도", "미충족")
expect_true(!grepl("<h3>Normality</h3>", correlation_ko, fixed = TRUE), "English Normality title leaked into Korean saved results")
expect_true(!grepl(">Skewness<", correlation_ko, fixed = TRUE), "English Normality column leaked into Korean saved results")
expect_true(!grepl(">not satisfied<", correlation_ko, fixed = TRUE), "English Normality value leaked into Korean saved results")
expect_true(grepl('data-result-table-role="main"', correlation_ko, fixed = TRUE), "Saved correlation publication table must remain a main table")
expect_true(grepl('data-result-table-language="en"', correlation_ko, fixed = TRUE), "Saved correlation publication table must remain English")

message("Checking English saved-result appendix preservation...")
options(statedu.app_language = "en")
regression_en <- saved_analysis_results_html(
  regression_prepared$results,
  variable_table = regression_info
)
expect_model_overview_contract(regression_en, "en", "Model overview")

correlation_en <- saved_correlation_results_html(correlation_result)
expect_normality_contract(correlation_en, "en", "Normality", "Skewness", "not satisfied")
expect_true(!grepl("<h3>정규성</h3>", correlation_en, fixed = TRUE), "Korean Normality title leaked into English saved results")

for (entry in list(regression_ko, correlation_ko, regression_en, correlation_en)) {
  rendered_markup <- gsub("<style[^>]*>[\\s\\S]*?</style>", "", entry, perl = TRUE)
  expect_true(
    count_fixed(rendered_markup, 'data-result-table-sheet="true"') == count_fixed(rendered_markup, "<table"),
    "Every saved result table must have its own independent B5 sheet"
  )
  final_contract <- '.result-table-sheet:is(#statedu-saved-result-sheet, [data-result-table-sheet="true"]) > table.result-table-contract-table'
  expect_true(
    grepl(final_contract, entry, fixed = TRUE),
    "Saved result screens must restore the common intrinsic-width table contract after legacy layout CSS"
  )
  expect_true(
    grepl('width: max(100%, var(--result-table-intrinsic-width, 480px)) !important;', entry, fixed = TRUE) &&
      grepl('min-width: var(--result-table-intrinsic-width, 480px) !important;', entry, fixed = TRUE) &&
      grepl('table-layout: auto !important;', entry, fixed = TRUE),
    "Saved result screens must retain intrinsic width and horizontal scrolling for genuinely wide tables"
  )
  expect_true(
    grepl('font-size: 11px !important;', entry, fixed = TRUE) &&
      grepl('font-size: 12px !important;', entry, fixed = TRUE),
    "Saved result screens must preserve the shared 11 px header and 12 px body typography"
  )
}

message("Saved regression/correlation result-screen contract validation passed.")
