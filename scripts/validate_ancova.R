source("R/app_bootstrap.R")
load_app_packages(check = FALSE)
source_app_modules()

cat("Checking ANCOVA analysis paths...\n")

set.seed(101)
data_basic <- data.frame(
  y = rnorm(90),
  group = rep(c("A", "B", "C"), each = 30),
  x = rnorm(90)
)
data_basic$y <- data_basic$y +
  ifelse(data_basic$group == "B", 0.5, ifelse(data_basic$group == "C", 1, 0)) +
  0.4 * data_basic$x
variable_info <- data.frame(
  name = c("y", "group", "x"),
  measurement = c("continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)

result <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info)
stopifnot(identical(result$type, "ancova"))
stopifnot(length(result$results) == 1L)
stopifnot(is.data.frame(result$results[[1]]$table))
stopifnot(all(c("Variable", "Label", "Adjusted mean", "SE", "F", "p", "Effect size", "post-hoc") %in% names(result$results[[1]]$table)))
stopifnot(nrow(result$results[[1]]$table) == 3L)
stopifnot(!grepl("\\([0-9.]+,[0-9.]+\\)", result$results[[1]]$table$F[[1]], perl = TRUE))
stopifnot(grepl("Bonferroni-corrected", result$results[[1]]$note, fixed = TRUE))

df_result <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(show_df = TRUE))
stopifnot(grepl("\\([0-9.]+,[0-9.]+\\)", df_result$results[[1]]$table$F[[1]], perl = TRUE))

mean_se_result <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(mean_se = TRUE))
stopifnot("M \u00B1 SE" %in% names(mean_se_result$results[[1]]$table))
stopifnot(!("Adjusted mean" %in% names(mean_se_result$results[[1]]$table)))
stopifnot(!("SE" %in% names(mean_se_result$results[[1]]$table)))
stopifnot(grepl("\u00B1", mean_se_result$results[[1]]$table[["M \u00B1 SE"]][[1]], fixed = TRUE))
stopifnot(grepl("M \u00B1 SE = adjusted mean \u00B1 standard error.", ancova_combined_note(mean_se_result, variable_info), fixed = TRUE))

ordered <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(ordered_significance = TRUE))
stopifnot("post-hoc" %in% names(ordered$results[[1]]$table))
stopifnot(grepl("ordered significance notation", ordered$results[[1]]$note, fixed = TRUE))

holm <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(posthoc_method = "holm"))
stopifnot(grepl("Holm-Bonferroni-adjusted", holm$results[[1]]$note, fixed = TRUE))

data_categorical_covariate <- data_basic
data_categorical_covariate$site <- rep(c("S1", "S2"), length.out = nrow(data_categorical_covariate))
variable_info_categorical_covariate <- data.frame(
  name = c("y", "group", "x", "site"),
  measurement = c("continuous", "category", "continuous", "binary"),
  stringsAsFactors = FALSE
)
categorical_covariate <- prepare_ancova_results(
  data_categorical_covariate,
  "y",
  "group",
  c("x", "site"),
  variable_info_categorical_covariate
)
stopifnot(length(categorical_covariate$results) == 1L)
stopifnot(is.factor(categorical_covariate$results[[1]]$fit_data$site))
stopifnot(any(grepl("^site", colnames(stats::model.matrix(categorical_covariate$results[[1]]$model)))))

plots <- prepare_ancova_results(
  data_basic,
  "y",
  "group",
  "x",
  variable_info,
  options = list(plot_adjusted_means = TRUE, plot_raw_overlay = TRUE, plot_regression_lines = TRUE)
)
plot_html <- as.character(ancova_results_ui(plots, variable_info))
stopifnot(grepl("ANCOVA plots", plot_html, fixed = TRUE))
stopifnot(grepl("Adjusted mean error bar plot", plot_html, fixed = TRUE))

data_multi <- data_basic
data_multi$y2 <- data_multi$y + rnorm(nrow(data_multi), sd = 0.2)
variable_info_multi <- data.frame(
  name = c("y", "y2", "group", "x"),
  measurement = c("continuous", "continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)
multi <- prepare_ancova_results(data_multi, c("y", "y2"), "group", "x", variable_info_multi)
combined <- ancova_combined_result_table(multi, variable_info_multi)
stopifnot(is.data.frame(combined), "DV" %in% names(combined))
stopifnot(length(unique(combined$DV[nzchar(combined$DV)])) == 2L)
combined_markers <- attr(combined, "note_markers", exact = TRUE)
stopifnot(is.null(combined_markers) || !any(combined_markers$column %in% c("DV", "Effect size", "post-hoc")))
combined_note <- ancova_combined_note(multi, variable_info_multi)
stopifnot(grepl("Analysis method:", combined_note, fixed = TRUE))
stopifnot(grepl("Effect size", combined_note, fixed = TRUE))
stopifnot(grepl("Post-hoc:", combined_note, fixed = TRUE))
stopifnot(!grepl("y: Analysis method", combined_note, fixed = TRUE))
stopifnot(!grepl("y2: Analysis method", combined_note, fixed = TRUE))

overview <- ancova_model_overview_table(result, variable_info)
stopifnot(is.data.frame(overview), nrow(overview) == 1L)
stopifnot("Analysis" %in% names(overview))

data_two_group <- subset(data_basic, group %in% c("A", "B"))
data_two_group$group <- droplevels(factor(data_two_group$group))
two_group <- prepare_ancova_results(data_two_group, "y", "group", "x", variable_info)
stopifnot(is.data.frame(two_group$results[[1]]$table))
stopifnot(!("post-hoc" %in% names(two_group$results[[1]]$table)))
stopifnot(nrow(two_group$results[[1]]$table) == 2L)

ranked <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(force_ranked = TRUE))
stopifnot(identical(ranked$results[[1]]$method, "Ranked ANCOVA"))

mixed_method <- multi
mixed_method$results[[2]] <- ranked$results[[1]]
mixed_method$results[[2]]$dependent <- "y2"
mixed_method_table <- ancova_combined_result_table(mixed_method, variable_info_multi)
mixed_method_markers <- attr(mixed_method_table, "note_markers", exact = TRUE)
stopifnot(is.data.frame(mixed_method_markers), any(mixed_method_markers$column == "p"))
stopifnot(!any(mixed_method_markers$column == "DV"))
stopifnot(!any(mixed_method_markers$column == "Effect size"))
mixed_method_note <- ancova_combined_note(mixed_method, variable_info_multi)
stopifnot(any(grepl("Ranked ANCOVA:", mixed_method_note, fixed = TRUE)))
stopifnot(regexpr("1. Analysis method:", mixed_method_note, fixed = TRUE) < regexpr("Post-hoc:", mixed_method_note, fixed = TRUE))
stopifnot(regexpr("Post-hoc:", mixed_method_note, fixed = TRUE) < regexpr("Effect size", mixed_method_note, fixed = TRUE))

mixed_posthoc <- multi
mixed_posthoc$results[[2]] <- holm$results[[1]]
mixed_posthoc$results[[2]]$dependent <- "y2"
mixed_posthoc_table <- ancova_combined_result_table(mixed_posthoc, variable_info_multi)
mixed_posthoc_markers <- attr(mixed_posthoc_table, "note_markers", exact = TRUE)
stopifnot(is.data.frame(mixed_posthoc_markers), any(mixed_posthoc_markers$column == "post-hoc"))
stopifnot(!any(mixed_posthoc_markers$column == "Effect size"))

set.seed(102)
n <- 120
data_interaction <- data.frame(
  group = rep(c("A", "B"), each = n / 2),
  x = rnorm(n)
)
data_interaction$y <- rnorm(n) + 0.2 * data_interaction$x + ifelse(data_interaction$group == "B", 1.5 * data_interaction$x, 0)
variable_info_interaction <- data.frame(
  name = c("y", "group", "x"),
  measurement = c("continuous", "binary", "continuous"),
  stringsAsFactors = FALSE
)
interaction <- prepare_ancova_results(data_interaction, "y", "group", "x", variable_info_interaction)
stopifnot(identical(interaction$results[[1]]$method, "Interaction ANCOVA"))

html <- as.character(ancova_results_ui(result, variable_info))
stopifnot(grepl("Model overview", html, fixed = TRUE))
stopifnot(grepl("ANCOVA table", html, fixed = TRUE))

html_file <- tempfile(fileext = ".html")
write_ancova_results_html(result, html_file, variable_info)
stopifnot(file.exists(html_file))
stopifnot(grepl("Model overview", paste(readLines(html_file, warn = FALSE), collapse = " "), fixed = TRUE))

excel_file <- tempfile(fileext = ".xlsx")
save_ancova_excel_file(result, excel_file, variable_info)
stopifnot(file.exists(excel_file))

cat("All ANCOVA validations passed.\n")
