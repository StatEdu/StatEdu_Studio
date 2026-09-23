all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1L]]) else "scripts/validate_frequencies_screen_table_contract.R"
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

style_text <- paste(
  readLines(file.path(repo_root, "www", "style.css"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
stopifnot(grepl(
  ".frequency-table-wrap .result-table-sheet > table.result-table-contract-table",
  style_text,
  fixed = TRUE
))

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

count_fixed <- function(text, pattern) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (length(hits) == 1L && hits[[1L]] == -1L) 0L else length(hits)
}

render_html <- function(tag) as.character(htmltools::renderTags(tag)$html)

fixture <- data.frame(
  group = factor(c("Control", "Treatment", "Control", "Treatment", "Control", "Treatment")),
  outcome = c(11.2, 12.7, 9.8, 13.4, 10.5, 14.1),
  score = c(3.1, 4.4, 2.8, 4.9, 3.5, 5.2),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(fixture),
  var_label = c("Study group", "Outcome", "Score"),
  measurement = c("category", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
result <- prepare_frequencies_results(
  fixture,
  variables = c("group", "outcome", "score"),
  variable_info = variable_info
)
result$options <- list(
  n_percent = TRUE,
  mean_sd = TRUE,
  min_max = TRUE,
  median_iqr = TRUE,
  skew_kurtosis = TRUE,
  pie = FALSE,
  bar = FALSE,
  histogram = FALSE,
  box = FALSE,
  violin = FALSE
)

categorical <- frequency_categorical_main_table(result)
continuous <- frequency_continuous_main_table(result, result$options)
expect_true(identical(names(categorical), c("Variable", "Value", "n", "%")), "Categorical publication table must contain only Variable, Value, n, and %")
expect_true(identical(
  names(continuous),
  c("Variable", "n", "M ± SD", "Min", "Max", "Median", "IQR (Q1–Q3)", "Skewness", "Kurtosis")
), "Continuous publication table must contain only relevant descriptive columns")
expect_true(!"Value" %in% names(continuous), "Continuous publication table must not retain the categorical Value column")
expect_true(!any(c("M", "SD", "Min", "Max", "Median", "Skewness", "Kurtosis") %in% names(categorical)), "Categorical publication table must not retain continuous-only columns")
expect_true(identical(attr(categorical, "result_table_role", exact = TRUE), "main"), "Categorical table must be a main table")
expect_true(identical(attr(continuous, "result_table_language", exact = TRUE), "en"), "Continuous main table must be English")

html <- render_html(frequencies_results_ui(result))
expect_true(count_fixed(html, 'data-result-table-sheet="true"') == 2L, "Mixed frequencies output must render two independent table sheets")
expect_true(count_fixed(html, "<table") == 2L, "Each frequencies publication sheet must contain exactly one table")
expect_true(grepl("Categorical Frequencies", html, fixed = TRUE), "Categorical publication table requires its own English title")
expect_true(grepl("Continuous Descriptive Statistics", html, fixed = TRUE), "Continuous publication table requires its own English title")
expect_true(!grepl("Frequencies / Descriptives</h3>", html, fixed = TRUE), "Mixed publication output must not retain one combined table title")
expect_true(count_fixed(html, 'data-result-table-role="main"') >= 4L, "Both frequency tables must carry main-table metadata")
expect_true(count_fixed(html, 'data-result-table-language="en"') >= 4L, "Both frequency tables must remain English")
expect_true(count_fixed(html, 'data-result-table-orientation="portrait"') >= 2L, "Narrow categorical table must use B5 portrait")
expect_true(!grepl('data-result-table-orientation="landscape"', html, fixed = TRUE), "Continuous table must use B5 portrait")
expect_true(grepl("font-size:12px", html, fixed = TRUE), "Frequency table body font must use the shared 12 px contract")
expect_true(grepl("font-size:11px", html, fixed = TRUE), "Frequency table header font must use the shared 11 px contract")
expect_true(grepl("M = mean; SD = standard deviation; IQR = interquartile range.", html, fixed = TRUE), "Continuous main note must be concise and ordered")

categorical_only <- result
categorical_only$continuous <- character(0)
categorical_only$descriptive_table <- NULL
categorical_html <- render_html(frequencies_results_ui(categorical_only))
expect_true(count_fixed(categorical_html, 'data-result-table-sheet="true"') == 1L, "Categorical-only analysis must render one independent sheet")

continuous_only <- result
continuous_only$categorical <- character(0)
continuous_only$categorical_tables <- list()
continuous_html <- render_html(frequencies_results_ui(continuous_only))
expect_true(count_fixed(continuous_html, 'data-result-table-sheet="true"') == 1L, "Continuous-only analysis must render one independent sheet")

saved_html <- saved_frequencies_results_html(result, css_path = tempfile(fileext = ".css"), report_mode = FALSE)
saved_markup <- gsub("<style[^>]*>[\\s\\S]*?</style>", "", saved_html, perl = TRUE)
expect_true(count_fixed(saved_markup, 'data-result-table-sheet="true"') == 2L, "Reopened frequencies results must preserve two independent table sheets")
expect_true(count_fixed(saved_markup, "<table") == 2L, "Reopened frequencies results must preserve one table per sheet")
expect_true(grepl("Categorical Frequencies", saved_html, fixed = TRUE), "Reopened results must preserve the categorical publication table")
expect_true(grepl("Continuous Descriptive Statistics", saved_html, fixed = TRUE), "Reopened results must preserve the continuous publication table")
expect_true(!grepl("Frequencies / Descriptives</h3>", saved_html, fixed = TRUE), "Reopened results must not revert to the combined wide table")
expect_true(count_fixed(saved_html, 'data-result-table-language="en"') >= 4L, "Reopened frequency main tables must remain English")

cat("Frequencies screen table split and B5 contract validation passed.\n")
