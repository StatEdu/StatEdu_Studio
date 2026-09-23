all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_regression_screen_table_contract.R"
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

render_html <- function(tag) as.character(htmltools::renderTags(tag)$html)

old_language <- getOption("statedu.app_language")
on.exit(options(statedu.app_language = old_language), add = TRUE)

set.seed(20260826)
n <- 72L
fixture <- data.frame(
  outcome = stats::rnorm(n),
  predictor = stats::rnorm(n),
  stringsAsFactors = FALSE
)
fixture$outcome <- .35 + .65 * fixture$predictor + fixture$outcome
variable_info <- data.frame(
  name = names(fixture),
  var_label = c("Outcome label", "Predictor label"),
  role = "",
  measurement = "continuous",
  stringsAsFactors = FALSE
)
prepared <- prepare_single_regression_result(
  dependent = "outcome",
  data = fixture,
  predictors = "predictor",
  variable_info = variable_info,
  residual_diagnostics = FALSE,
  auto_method = FALSE
)
result <- prepared$result
result$method <- "Bootstrap regression"
result$use_bootstrap <- TRUE
result$bootstrap_r <- 30L
result$bootstrap_seed <- 20260826L
result$bootstrap_ci_method <- "bias_corrected"
result$boot_table <- bootstrap_coef_table(
  fixture,
  outcome ~ predictor,
  r = result$bootstrap_r,
  seed = result$bootstrap_seed,
  ci_method = result$bootstrap_ci_method
)
results <- list(result)

message("Checking Korean-UI regression main/appendix separation and localization...")
options(statedu.app_language = "ko")
korean_html <- render_html(regression_results_panel(results, variable_table = variable_info))
sheet_count <- count_fixed(korean_html, 'data-result-table-sheet="true"')
table_count <- count_fixed(korean_html, "<table")
expect_true(sheet_count == table_count && sheet_count >= 4L, "Every regression table must have its own independent B5 sheet")
expect_true(grepl('data-result-table-role="main"', korean_html, fixed = TRUE), "Regression coefficient output must be a main table")
expect_true(grepl('data-result-table-language="en"', korean_html, fixed = TRUE), "Regression main tables must remain English in a Korean UI")
expect_true(grepl('data-result-table-role="appendix"', korean_html, fixed = TRUE), "Regression diagnostic output must be classified as appendix")
expect_true(grepl('data-result-table-language="ko"', korean_html, fixed = TRUE), "Regression appendix tables must follow the Korean UI language")
expect_true(
  all(vapply(
    c("모형 개요", "부트스트랩 회귀분석", "부트스트랩 진단", "요청", "유효", "유효 비율(%)", "충분", "가정 검토"),
    grepl,
    logical(1),
    x = korean_html,
    fixed = TRUE
  )),
  "Known regression appendix headers and values were not localized to Korean"
)
expect_true(
  !any(vapply(
    c(">Bootstrap regression<", ">Requested<", ">Valid<", ">Valid %<", ">Adequate<"),
    grepl,
    logical(1),
    x = korean_html,
    fixed = TRUE
  )),
  "Known English regression diagnostics remain in the Korean appendix"
)
expect_true(grepl("Bootstrap Regression(Outcome label)", korean_html, fixed = TRUE), "The publication-table title must remain English")
expect_true(grepl("LLCI =", korean_html, fixed = TRUE), "The publication table must define its bootstrap confidence limits")
expect_true(!grepl("Note.", korean_html, fixed = TRUE), "Regression notes must omit the Note. prefix")

korean_plot_html <- render_html(plot_result_panel("Outcome label", "qq_probe", "homogeneity_probe", result))
expect_true(
  all(vapply(c("진단 도표(Outcome label)", "Q-Q 도표", "잔차 등분산성"), grepl, logical(1), x = korean_plot_html, fixed = TRUE)),
  "Regression diagnostic-plot headings must follow the Korean UI language"
)

message("Checking English-UI regression appendix preservation...")
options(statedu.app_language = "en")
english_html <- render_html(regression_results_panel(results, variable_table = variable_info))
expect_true(grepl('data-result-table-language="en"', english_html, fixed = TRUE), "English UI regression tables must use English language metadata")
expect_true(
  all(vapply(
    c("Model overview", "Bootstrap regression", "Bootstrap diagnostics", "Requested", "Valid", "Valid %", "Adequate", "Assumption review"),
    grepl,
    logical(1),
    x = english_html,
    fixed = TRUE
  )),
  "English UI regression appendix content must remain English"
)
expect_true(
  !any(vapply(c("모형 개요", "부트스트랩 회귀분석", "부트스트랩 진단", "유효 비율", "가정 검토"), grepl, logical(1), x = english_html, fixed = TRUE)),
  "Korean regression appendix text leaked into the English UI"
)

english_plot_html <- render_html(plot_result_panel("Outcome label", "qq_probe", "homogeneity_probe", result))
expect_true(
  all(vapply(c("Diagnostic plots(Outcome label)", "Q-Q plot", "Residual homoscedasticity"), grepl, logical(1), x = english_plot_html, fixed = TRUE)),
  "Regression diagnostic-plot headings must remain English in the English UI"
)

message("Regression screen result-table contract validation passed.")
