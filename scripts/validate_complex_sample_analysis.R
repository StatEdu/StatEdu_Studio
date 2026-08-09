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

expect_not_matches <- function(text, pattern, label = pattern) {
  if (grepl(pattern, text, perl = TRUE)) {
    stop(sprintf("Output must not match %s.", label), call. = FALSE)
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

message("Checking complex-sample setup input IDs...")
candidate_info <- data.frame(
  name = c("outcome", "plain_id", "design_stratum", "survey_psu", "sample_weight", "rep_weight_1"),
  var_label = c("Outcome", "Identifier", "Stratification variable", "Primary sampling unit", "Sampling weight", "Replicate weight"),
  measurement = c("continuous", "category", "category", "category", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
strata_choices <- unname(complex_sample_design_role_choices(
  "strata", "outcome", candidate_info$name, candidate_info, language = "en"
))
cluster_choices <- unname(complex_sample_design_role_choices(
  "cluster", "outcome", candidate_info$name, candidate_info, language = "en"
))
weight_choices <- unname(complex_sample_design_role_choices(
  "weight", "outcome", candidate_info$name, candidate_info, language = "en"
))
expect_true(identical(strata_choices[1:2], c("", "design_stratum")), "Strata candidates must appear first in the design-variable list.")
expect_true(identical(cluster_choices[1:2], c("", "survey_psu")), "Cluster / PSU candidates must appear first in the design-variable list.")
expect_true(identical(weight_choices[1:2], c("", "sample_weight")), "Sampling-weight candidates must appear first in the design-variable list.")
expect_true(!identical(weight_choices[[2]], "rep_weight_1"), "Replicate weights must not be promoted as sampling-weight candidates.")

setup_html <- render_text(complex_sample_setup_panel(
  prefix = "complex_frequency",
  selected_names = c("x", "g", "psu", "wt"),
  target_specs = complex_sample_target_specs("frequencies"),
  target_values = list(selected = c("x", "g")),
  analysis_type = "frequencies",
  show_design_tabs = FALSE,
  language = "en"
))
setup_option_ids <- gregexpr('id="complex_frequency_show_ci"', setup_html, fixed = TRUE)[[1]]
expect_true(
  setup_option_ids[[1]] != -1L && identical(length(setup_option_ids), 1L),
  "Complex-sample hidden design controls must not duplicate visible analysis option IDs."
)

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
expect_contains(group_text, "M \u00B1 SE", "complex-sample t-test / ANOVA compact M +/- SE header")
expect_contains(group_text, "t(df)", "complex-sample t-test statistic header with df")
expect_true(!grepl("Statistic</th>", group_text, fixed = TRUE), "Complex-sample t-test / ANOVA should not show a generic Statistic header.")
expect_true(!grepl(">df</th>", group_text, fixed = TRUE), "Complex-sample t-test / ANOVA should not show a separate df header.")

group_mixed_data <- data.frame(
  psu = 1:12,
  wt = rep(1, 12),
  y = 1:12,
  g2 = rep(c("A", "B"), 6),
  g3 = rep(c("A", "B", "C"), 4),
  stringsAsFactors = FALSE
)
group_mixed_info <- data.frame(
  name = c("y", "g2", "g3"),
  measurement = c("continuous", "nominal", "nominal"),
  stringsAsFactors = FALSE
)
group_mixed_text <- render_text(complex_sample_group_result(group_mixed_data, "y", c("g2", "g3"), complex_input(), "p", variable_info = group_mixed_info))
expect_contains(group_mixed_text, "t/F(df)", "mixed complex-sample t-test / ANOVA statistic header")
expect_true(grepl("\\([0-9,]+\\)", group_mixed_text), "Complex-sample t-test / ANOVA should render df values under the statistic in parentheses.")
expect_true(grepl(">\\([0-9,]+\\)</td>", group_mixed_text), "Complex-sample t-test / ANOVA df should render in the next table-row cell.")
expect_true(!grepl("d=", group_mixed_text, fixed = TRUE), "Complex-sample t-test / ANOVA effect-size values should omit the d= prefix.")
expect_contains(group_mixed_text, "table-layout:fixed", "complex-sample t-test / ANOVA fixed table layout")
expect_contains(group_mixed_text, "width:18.0000%", "complex-sample t-test / ANOVA M +/- SE B5 portrait width")
expect_contains(group_mixed_text, "width:19.0000%", "complex-sample t-test / ANOVA 95% CI B5 portrait width")
expect_contains(group_mixed_text, "width:13.0000%", "complex-sample t-test / ANOVA ES B5 portrait width")
expect_contains(group_mixed_text, "width:14.0000%", "complex-sample t-test / ANOVA compact statistic B5 portrait width")
expect_contains(group_mixed_text, "padding-left:6px !important;padding-right:6px !important", "complex-sample t-test / ANOVA compact stat columns spacing")
expect_not_matches(group_mixed_text, 'coefficient-col-effect-size" style="[^"]*(^|;)width:[0-9]+px', "legacy fixed-pixel ES width")
expect_not_matches(group_mixed_text, 'coefficient-col-statistic" style="[^"]*(^|;)width:[0-9]+px', "legacy fixed-pixel t/F(df) width")
expect_not_matches(group_mixed_text, 'coefficient-col-p" style="[^"]*(^|;)width:[0-9]+px', "legacy fixed-pixel p width")

cross_wide_test <- list(statistic = c(F = 3.14159), parameter = c(ndf = 1, ddf = 24), p.value = 0.0123)
cross_wide_options <- list(crosstab_test_method = "F", show_df = TRUE, show_percent = TRUE, row_percent = FALSE, show_p = TRUE, show_trend = FALSE)
cross_wide_tab <- matrix(
  c(10, 10, 5, 15, 7, 9),
  nrow = 2,
  dimnames = list(c("A", "B"), c("One", "Two", "Three"))
)
cross_wide_text <- render_text(complex_sample_crosstab_display(cross_wide_tab, cross_wide_tab, "row_ok", "col", cross_wide_test, variable_info = cross_info, options = cross_wide_options))
expect_contains(cross_wide_text, "landscape-table-panel", "wide complex-sample crosstab landscape panel")

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
