all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_complex_sample_analysis.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(repo_root)

source("R/app_bootstrap.R")
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))

render_text <- function(tag) {
  paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")
}

expect_contains <- function(text, pattern, label = pattern) {
  if (!grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("Expected output to contain %s.", label), call. = FALSE)
  }
}

expect_true <- function(value, label) {
  if (!isTRUE(value)) {
    stop(label, call. = FALSE)
  }
}

complex_input <- function() {
  list(
    p_strata = "",
    p_cluster = "psu",
    p_weight = "wt",
    p_fpc = "",
    p_variance_method = "auto",
    p_lonely_psu = "adjust",
    p_correlation_method = "pearson",
    p_correlation_p_adjust = "holm",
    p_correlation_matrix = TRUE,
    p_use_replicate_weights = FALSE,
    p_replicate_weights = character(0),
    p_replicate_type = "auto",
    p_replicate_combined_weights = FALSE,
    p_subpopulation = "",
    p_subpopulation_condition = "",
    p_subpopulation_condition_type = "equals",
    p_subpopulation_condition_value = ""
  )
}

message("Checking complex-sample frequency skipped-variable reporting...")
freq_data <- data.frame(
  psu = 1:8,
  wt = rep(1, 8),
  x = 1:8,
  y = rep(NA_real_, 8),
  g = c("A", "B", "A", "B", "A", "B", "A", "B"),
  h = rep(NA_character_, 8),
  stringsAsFactors = FALSE
)
freq_info <- data.frame(
  name = c("x", "y", "g", "h"),
  measurement = c("continuous", "continuous", "nominal", "nominal"),
  stringsAsFactors = FALSE
)
freq_text <- render_text(complex_sample_frequency_result(freq_data, c("x", "y", "g", "h"), complex_input(), "p", variable_info = freq_info))
expect_contains(freq_text, "Variables with no usable non-missing values")
expect_contains(freq_text, "y, h")

message("Checking complex-sample crosstab independent combinations...")
cross_data <- data.frame(
  psu = 1:8,
  wt = rep(1, 8),
  row_ok = c("A", "A", "B", "B", "A", "B", "A", "B"),
  row_bad = rep(NA_character_, 8),
  col = c("X", "Y", "X", "Y", "X", "Y", "X", "Y"),
  stringsAsFactors = FALSE
)
cross_info <- data.frame(
  name = c("row_ok", "row_bad", "col"),
  measurement = c("nominal", "nominal", "nominal"),
  stringsAsFactors = FALSE
)
cross_text <- render_text(complex_sample_crosstab_results(cross_data, c("row_bad", "row_ok"), "col", complex_input(), "p", variable_info = cross_info))
expect_contains(cross_text, "row_bad by col was not computed")
expect_contains(cross_text, "row_ok")

message("Checking complex-sample t-test / ANOVA independent combinations...")
group_data <- data.frame(
  psu = 1:8,
  wt = rep(1, 8),
  y = 1:8,
  g1 = c("A", "A", "B", "B", "A", "B", "A", "B"),
  g2 = rep("A", 8),
  stringsAsFactors = FALSE
)
group_info <- data.frame(
  name = c("y", "g1", "g2"),
  measurement = c("continuous", "nominal", "nominal"),
  stringsAsFactors = FALSE
)
group_text <- render_text(complex_sample_group_result(group_data, "y", c("g1", "g2"), complex_input(), "p", variable_info = group_info))
expect_contains(group_text, "fewer than two usable groups")
expect_contains(group_text, "Complex-sample univariable analysis")

message("Checking complex-sample correlation and Filter subpopulation defaults...")
cor_data <- data.frame(
  psu = 1:10,
  wt = rep(1, 10),
  Filter = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
  x = 1:10,
  y = c(2, 3, 5, 7, 11, NA, 17, 19, 23, 29),
  ord = c("1 Low", "1 Low", "2 Middle", "2 Middle", "3 High", "3 High", "1 Low", "2 Middle", "3 High", "3 High"),
  z = rep(1, 10),
  stringsAsFactors = FALSE
)
cor_info <- data.frame(
  name = c("Filter", "x", "y", "ord", "z"),
  measurement = c("binary", "continuous", "continuous", "ordered", "continuous"),
  stringsAsFactors = FALSE
)
filter_choices <- complex_sample_subpopulation_choices(c("x", "y", "Filter"), colnames(cor_data), cor_info)
expect_true(identical(unname(filter_choices[1]), "Filter"), "Expected Filter to be the first subpopulation candidate")
expect_true(identical(unname(filter_choices[2]), ""), "Expected No variable to follow Filter in subpopulation choices")
cor_text <- render_text(complex_sample_correlation_result(cor_data, c("x", "y", "z"), complex_input(), "p", variable_info = cor_info))
expect_contains(cor_text, "Complex-sample correlation overview")
expect_contains(cor_text, "Displayed variable pairs")
expect_contains(cor_text, "P-value adjustment")
expect_contains(cor_text, "Survey design N")
expect_contains(cor_text, "Complex-sample correlation matrix")
expect_contains(cor_text, "Complex-sample correlation details")
expect_contains(cor_text, "p adjusted")
expect_contains(cor_text, "Missing N")
expect_contains(cor_text, "rows excluded from each pair")
expect_contains(cor_text, "Lower triangle shows design-based correlation coefficients")
expect_contains(cor_text, "Holm-Bonferroni-adjusted")
expect_contains(cor_text, "fewer than two unique pairwise complete values")
ordered_text <- render_text(complex_sample_correlation_result(cor_data, c("x", "ord"), complex_input(), "p", variable_info = cor_info))
expect_contains(ordered_text, "Pearson (ordinal scores)")
expect_contains(ordered_text, "Ordered variables were converted to ordinal scores")
spearman_input <- complex_input()
spearman_input$p_correlation_method <- "spearman"
spearman_text <- render_text(complex_sample_correlation_result(cor_data, c("x", "y"), spearman_input, "p", variable_info = cor_info))
expect_contains(spearman_text, "Spearman rank correlation rank-transforms each variable")

message("Checking complex-sample regression independent outcomes...")
reg_data <- data.frame(
  psu = 1:10,
  wt = rep(1, 10),
  y_ok = 1:10,
  y_bad = rep(NA_real_, 10),
  x = 1:10,
  stringsAsFactors = FALSE
)
reg_info <- data.frame(
  name = c("y_ok", "y_bad", "x"),
  measurement = c("continuous", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
reg_text <- render_text(complex_sample_regression_results(reg_data, c("y_bad", "y_ok"), "x", complex_input(), "p", logistic = FALSE, variable_info = reg_info))
expect_contains(reg_text, "Dependent variable has no usable non-missing values")
expect_contains(reg_text, "Complex-sample regression: y_ok")

message("Checking complex-sample logistic regression independent outcomes...")
log_data <- data.frame(
  psu = 1:12,
  wt = rep(1, 12),
  y_ok = rep(c("no", "yes"), 6),
  y_bad = rep("no", 12),
  x = rep(c(0, 1, 2), 4),
  stringsAsFactors = FALSE
)
log_info <- data.frame(
  name = c("y_ok", "y_bad", "x"),
  measurement = c("nominal", "nominal", "continuous"),
  stringsAsFactors = FALSE
)
log_text <- render_text(complex_sample_regression_results(log_data, c("y_bad", "y_ok"), "x", complex_input(), "p", logistic = TRUE, variable_info = log_info))
expect_contains(log_text, "Logistic regression requires a binary dependent variable")
expect_contains(log_text, "Complex-sample logistic regression: y_ok")

message("Complex-sample analysis validations passed.")
