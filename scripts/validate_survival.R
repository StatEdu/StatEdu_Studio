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

message("Checking Kaplan-Meier analysis...")
lung <- read.csv(file.path(repo_root, "sample", "survival_examples", "survival_lung.csv"), check.names = FALSE)
km <- prepare_km_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  group = c("sex", "ph.ecog"),
  event_value = "1",
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
stopifnot("M ± SE" %in% names(km_table))
stopifnot("Median (95% CI)" %in% names(km_table))
stopifnot("Chi-square (df)" %in% names(km_table))
stopifnot(!("df" %in% names(km_table)))
stopifnot(!any(grepl("=", km_table$Level, fixed = TRUE)))
stopifnot(any(grepl(paste0(intToUtf8(160), "("), km_table[["Median (95% CI)"]], fixed = TRUE)))

km_html <- htmltools::renderTags(survival_simple_table(km_table))$html
stopifnot(grepl("<br", km_html, fixed = TRUE))
stopifnot(grepl("&chi;", km_html, fixed = TRUE))
stopifnot(grepl("<sup>2</sup>", km_html, fixed = TRUE))

km_panel <- htmltools::renderTags(survival_km_results_panel(km, plot_output_ids = list(character(0), character(0)), language = "en"))$html
stopifnot(grepl("Model overview", km_panel, fixed = TRUE))
stopifnot(grepl("M = mean; SE = standard error", km_panel, fixed = TRUE))

message("Checking Kaplan-Meier empty table/plot options...")
km_empty_options <- prepare_km_analysis_result(
  data = lung,
  time = "time",
  event = "status",
  group = "sex",
  event_value = "1",
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
  event_value = "1"
)
stopifnot(identical(cox$type, "cox"))
stopifnot(nrow(cox$coef_table) > 0)
stopifnot(is.data.frame(survival_cox_coef_table(cox)))
stopifnot(is.data.frame(survival_ph_table(cox)))

message("Checking figure DPI policy...")
stopifnot(analysis_figure_dpi(edition = "pro", public_release = FALSE) == 600L)
stopifnot(analysis_figure_dpi(edition = "free", public_release = FALSE) == 300L)
stopifnot(analysis_figure_dpi(edition = "development", public_release = TRUE) == 300L)
stopifnot(analysis_figure_dpi(edition = "development", public_release = FALSE) == 600L)

message("All survival validations passed.")
