script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_survival.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

expect_close <- function(actual, expected, tolerance = 1e-8, label = "value") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("%s mismatch: actual=%s expected=%s", label, actual, expected), call. = FALSE)
  }
}

message("Checking Kaplan-Meier analysis...")
lung <- read.csv(file.path(repo_root, "sample", "survival_examples", "survival_lung.csv"), check.names = FALSE)
stopifnot(identical(survival_example_event_value(list(name = "survival_lung.csv"), "status"), "2"))
stopifnot(identical(survival_example_event_value(list(name = "survival_pbc.csv"), "status"), "2"))
stopifnot(identical(survival_example_event_value(list(name = "study_data.csv"), "status"), ""))

km <- prepare_km_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  group = c("sex", "ph.ecog"),
  event_value = "2",
  rate_times = "100, 200, 400",
  test_method = "logrank",
  output_tables = c("survival_table", "survival_time"),
  plot_types = c("survival", "event"),
  plot_versions = c("color", "bw")
)
stopifnot(identical(km$type, "km_multi"))
stopifnot(length(km$analyses) == 2)
stopifnot(all(vapply(km$analyses, function(item) !is.null(item$fit), logical(1))))

km_table <- survival_km_summary_table(km)
stopifnot(nrow(km_table) > 0)
stopifnot(paste("M", intToUtf8(177), "SE") %in% names(km_table))
stopifnot("Median (95% CI)" %in% names(km_table))
stopifnot("Chi-square (df)" %in% names(km_table))
stopifnot(!("df" %in% names(km_table)))
stopifnot(!any(grepl("=", km_table$Level, fixed = TRUE)))
stopifnot(any(grepl("\n(", km_table[["Median (95% CI)"]], fixed = TRUE)))

km_html <- htmltools::renderTags(survival_simple_table(km_table))$html
stopifnot(grepl("<br", km_html, fixed = TRUE))
stopifnot(grepl("&chi;", km_html, fixed = TRUE))
stopifnot(grepl("<sup>2</sup>", km_html, fixed = TRUE))
stopifnot(grepl("survival-col-median-95-ci", km_html, fixed = TRUE))

sex_logrank <- km$analyses[[which(vapply(km$analyses, function(item) identical(item$group, "sex"), logical(1)))]]
expected_logrank <- survival::survdiff(survival::Surv(time, status == 2) ~ sex, data = lung)
expect_close(sex_logrank$logrank$chisq, expected_logrank$chisq, label = "Log-rank chi-square")
expect_close(
  sex_logrank$logrank$p,
  stats::pchisq(expected_logrank$chisq, df = length(expected_logrank$n) - 1L, lower.tail = FALSE),
  label = "Log-rank p"
)

km_panel <- htmltools::renderTags(survival_km_results_panel(km, plot_output_ids = list(character(0), character(0)), language = "en"))$html
stopifnot(grepl("Model overview", km_panel, fixed = TRUE))
stopifnot(grepl("M = mean; SE = standard error", km_panel, fixed = TRUE))

message("Checking ungrouped Kaplan-Meier and life-table analyses...")
km_ungrouped <- prepare_km_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  group = "",
  event_value = "2",
  rate_times = "100, 200, 400"
)
ungrouped_table <- survival_km_summary_table(km_ungrouped)
stopifnot(identical(km_ungrouped$type, "km"))
stopifnot(nrow(ungrouped_table) == 1)
stopifnot(identical(ungrouped_table$Variables[[1]], "All"))
stopifnot(identical(ungrouped_table$Level[[1]], "All"))

life_ungrouped <- prepare_km_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  group = "",
  event_value = "2",
  rate_times = "100, 200, 400",
  analysis_method = "life_table"
)
stopifnot(identical(life_ungrouped$analysis_method, "life_table"))
stopifnot(nrow(survival_life_table_display(life_ungrouped)) > 0)

zero_time <- data.frame(
  time = c(0, 0, 1, 2, 3),
  status = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE
)
life_zero <- survival_life_table(zero_time, "time", "status", breaks = c(1, 2, 3))
stopifnot(nrow(life_zero) > 0)
stopifnot(life_zero$`At risk`[[1]] == 5)
stopifnot(life_zero$Events[[1]] == 2)
stopifnot(life_zero$Censored[[1]] == 1)

nr_data <- data.frame(
  time = c(5, 6, 7, 8),
  status = c(1, 0, 0, 0),
  stringsAsFactors = FALSE
)
nr_result <- prepare_km_analysis_result(
  data = nr_data,
  time = "time",
  event = "status",
  group = "",
  event_value = "1"
)
nr_table <- survival_km_summary_table(nr_result)
stopifnot(grepl("NR", nr_table[["Median (95% CI)"]][[1]], fixed = TRUE))

message("Checking Kaplan-Meier empty table/plot options...")
km_empty_options <- prepare_km_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  group = "sex",
  event_value = "2",
  output_tables = character(0),
  plot_types = character(0),
  plot_versions = character(0)
)
stopifnot(length(km_empty_options$output_tables) == 0)
stopifnot(length(km_empty_options$plot_types) == 0)
stopifnot(identical(km_empty_options$plot_versions, "color"))

message("Checking Kaplan-Meier plots...")
plot_color <- survival_km_ggplot(km$analyses[[1]], "survival", "color")
plot_bw <- survival_km_ggplot(km$analyses[[1]], "event", "bw")
stopifnot(inherits(plot_color, "ggplot"))
stopifnot(inherits(plot_bw, "ggplot"))

message("Checking Cox regression...")
cox <- prepare_cox_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  covariates = c("age", "sex"),
  event_value = "2"
)
stopifnot(identical(cox$type, "cox"))
stopifnot(nrow(cox$coef_table) > 0)
stopifnot(is.data.frame(survival_cox_coef_table(cox)))
stopifnot(is.data.frame(survival_ph_table(cox)))
stopifnot(is.data.frame(cox$model_tests))
stopifnot(nrow(cox$model_tests) == 3)
stopifnot(inherits(survival_cox_ggplot(cox, "color"), "ggplot"))
cox_overview <- survival_cox_overview_table(cox)
stopifnot("LR chi-square (df)" %in% cox_overview$Item)
stopifnot("Concordance (95% CI)" %in% cox_overview$Item)
cox_tests <- survival_cox_model_test_table(cox)
stopifnot(nrow(cox_tests) == 3)
cox_panel <- htmltools::renderTags(survival_cox_results_panel(cox))$html
stopifnot(grepl("Hazard ratio forest plot", cox_panel, fixed = TRUE))
cox_figure_dir <- tempfile("statedu_cox_figures_")
dir.create(cox_figure_dir)
cox_figures <- save_survival_cox_figures_to_dir(cox, cox_figure_dir)
stopifnot(length(cox_figures) == 2)
stopifnot(all(file.exists(cox_figures)))
unlink(cox_figure_dir, recursive = TRUE)

expected_cox <- survival::coxph(survival::Surv(time, status == 2) ~ age + sex, data = lung, x = TRUE)
expected_cox_summary <- summary(expected_cox)
for (term in rownames(expected_cox_summary$coefficients)) {
  actual_row <- cox$coef_table[cox$coef_table$Term == term, , drop = FALSE]
  stopifnot(nrow(actual_row) == 1)
  expect_close(actual_row$HR, expected_cox_summary$conf.int[term, "exp(coef)"], label = paste(term, "HR"))
  expect_close(actual_row$LLCI, expected_cox_summary$conf.int[term, "lower .95"], label = paste(term, "lower CI"))
  expect_close(actual_row$ULCI, expected_cox_summary$conf.int[term, "upper .95"], label = paste(term, "upper CI"))
}

expected_ph <- survival::cox.zph(expected_cox)
expected_ph_table <- as.data.frame(expected_ph$table)
for (term in rownames(expected_ph_table)) {
  actual_row <- cox$ph_table[cox$ph_table$Term == term, , drop = FALSE]
  stopifnot(nrow(actual_row) == 1)
  for (column in c("chisq", "df", "p")) {
    expect_close(actual_row[[column]], expected_ph_table[term, column], label = paste(term, column))
  }
}

for (test_name in c("Likelihood ratio", "Wald", "Score")) {
  expected_test <- switch(test_name,
    `Likelihood ratio` = expected_cox_summary$logtest,
    Wald = expected_cox_summary$waldtest,
    Score = expected_cox_summary$sctest
  )
  actual_row <- cox$model_tests[cox$model_tests$Test == test_name, , drop = FALSE]
  stopifnot(nrow(actual_row) == 1)
  expect_close(actual_row$Chisq, expected_test[["test"]], label = paste(test_name, "chi-square"))
  expect_close(actual_row$df, expected_test[["df"]], label = paste(test_name, "df"))
  expect_close(actual_row$p, expected_test[["pvalue"]], label = paste(test_name, "p"))
}
expect_close(cox$concordance[[1]], expected_cox_summary$concordance[[1]], label = "C-index")
expect_close(cox$concordance[[2]], expected_cox_summary$concordance[[2]], label = "C-index SE")

message("Checking figure DPI policy...")
stopifnot(analysis_figure_dpi(edition = "pro", public_release = FALSE) == 600L)
stopifnot(analysis_figure_dpi(edition = "free", public_release = FALSE) == 300L)
stopifnot(analysis_figure_dpi(edition = "development", public_release = TRUE) == 300L)
stopifnot(analysis_figure_dpi(edition = "development", public_release = FALSE) == 600L)

message("All survival validations passed.")
