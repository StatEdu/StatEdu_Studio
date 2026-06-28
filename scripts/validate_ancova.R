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

setup_html <- as.character(ancova_setup_panel(ancova_setup_state(
  selected_names = names(data_basic),
  dependent_variables = "y",
  factor_variable = "group",
  covariates = "x",
  variable_table = variable_info,
  language = "en"
)))
stopifnot(grepl("Analysis mode", setup_html, fixed = TRUE))
stopifnot(grepl("ancova_auto_method", setup_html, fixed = TRUE))
stopifnot(grepl("ancova_decision_alpha", setup_html, fixed = TRUE))
stopifnot(grepl("Sensitivity analysis", setup_html, fixed = TRUE))
stopifnot(grepl("Automatic selection", setup_html, fixed = TRUE))
stopifnot(grepl("White test", setup_html, fixed = TRUE))

result <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info)
stopifnot(identical(result$type, "ancova"))
stopifnot(length(result$results) == 1L)
stopifnot(is.data.frame(result$results[[1]]$table))
stopifnot(all(c("Variable", "Label", "Adjusted mean", "SE", "F", "p", "Effect size", "post-hoc") %in% names(result$results[[1]]$table)))
stopifnot(nrow(result$results[[1]]$table) == 3L)
stopifnot(identical(result$results[[1]]$sum_of_squares, "type2"))
stopifnot(grepl("Sum of squares: Type II", result$results[[1]]$note, fixed = TRUE))
stopifnot(grepl("Complete-case analysis:", result$results[[1]]$note, fixed = TRUE))
stopifnot(grepl("Assumption decision rule:", result$results[[1]]$note, fixed = TRUE))
stopifnot(grepl("homogeneity test = Levene", result$results[[1]]$note, fixed = TRUE))
stopifnot(is.data.frame(result$results[[1]]$assumptions$linearity_table))
assumption_review <- ancova_assumption_review_table(result, variable_info)
stopifnot("Residual normality p" %in% names(assumption_review))
stopifnot("Decision mode" %in% names(assumption_review))
stopifnot("Decision alpha" %in% names(assumption_review))
stopifnot(identical(assumption_review[["Decision mode"]][[1]], "Auto switch"))
stopifnot("Homogeneity test" %in% names(assumption_review))
stopifnot(identical(assumption_review[["Homogeneity test"]][[1]], "Levene"))
stopifnot(identical(result$results[[1]]$assumptions$normality_method, "Shapiro-Wilk"))
stopifnot("Linearity" %in% names(assumption_review))
stopifnot("Collinearity" %in% names(assumption_review))
stopifnot("Influence" %in% names(assumption_review))
slope_review <- ancova_slope_homogeneity_review_table(result, variable_info)
stopifnot(is.data.frame(slope_review), nrow(slope_review) >= 1L)
normality_review <- ancova_normality_review_table(result, variable_info)
stopifnot("Outcome p" %in% names(normality_review))
stopifnot("Residual p" %in% names(normality_review))
stopifnot(any(grepl("descriptive", normality_review$Note, fixed = TRUE)))
stopifnot(is.data.frame(result$results[[1]]$collinearity$table))
stopifnot(is.data.frame(result$results[[1]]$influence$table))
stopifnot(is.finite(result$results[[1]]$assumptions$outcome_normality_p))
stopifnot(!grepl("\\([0-9.]+,[0-9.]+\\)", result$results[[1]]$table$F[[1]], perl = TRUE))
stopifnot(grepl("Bonferroni-corrected", result$results[[1]]$note, fixed = TRUE))

data_collinear <- data_basic
data_collinear$x2 <- data_collinear$x + rnorm(nrow(data_collinear), sd = 0.001)
variable_info_collinear <- data.frame(
  name = c("y", "group", "x", "x2"),
  measurement = c("continuous", "category", "continuous", "continuous"),
  stringsAsFactors = FALSE
)
collinear <- prepare_ancova_results(data_collinear, "y", "group", c("x", "x2"), variable_info_collinear, options = list(normality_enabled = FALSE))
stopifnot(is.finite(collinear$results[[1]]$collinearity$max_vif))
stopifnot(collinear$results[[1]]$collinearity$max_vif > 10)
stopifnot(grepl("Multicollinearity warning", collinear$results[[1]]$note, fixed = TRUE))
collinearity_review <- ancova_collinearity_review_table(collinear, variable_info_collinear)
stopifnot(is.data.frame(collinearity_review), any(collinearity_review$Status == "VIF > 10"))

data_influence <- data_basic
data_influence$y[[1]] <- data_influence$y[[1]] + 25
influence <- prepare_ancova_results(data_influence, "y", "group", "x", variable_info, options = list(normality_enabled = FALSE, influence_sensitivity = TRUE))
stopifnot(influence$results[[1]]$influence$flagged_n > 0)
stopifnot(grepl("Influence diagnostics flagged", influence$results[[1]]$note, fixed = TRUE))
stopifnot(grepl("Influence sensitivity analysis compares", influence$results[[1]]$note, fixed = TRUE))
influence_review <- ancova_influence_review_table(influence, variable_info)
stopifnot(is.data.frame(influence_review), nrow(influence_review) > 0)
influence_sensitivity_review <- ancova_influence_sensitivity_review_table(influence, variable_info)
stopifnot(is.data.frame(influence_sensitivity_review), nrow(influence_sensitivity_review) >= 2L)
stopifnot(any(influence_sensitivity_review$Model == "Excluding flagged cases"))

data_nonlinear <- data.frame(
  y = (seq(-2, 2, length.out = 80)^2) + rnorm(80, sd = 0.05),
  group = rep(c("A", "B"), each = 40),
  x = seq(-2, 2, length.out = 80)
)
variable_info_nonlinear <- data.frame(
  name = c("y", "group", "x"),
  measurement = c("continuous", "binary", "continuous"),
  stringsAsFactors = FALSE
)
nonlinear <- prepare_ancova_results(data_nonlinear, "y", "group", "x", variable_info_nonlinear, options = list(normality_enabled = FALSE))
stopifnot(length(nonlinear$results[[1]]$assumptions$linearity_flagged) == 1L)
stopifnot(grepl("Possible covariate-outcome nonlinearity", nonlinear$results[[1]]$note, fixed = TRUE))
linearity_review <- ancova_linearity_review_table(nonlinear, variable_info_nonlinear)
stopifnot(is.data.frame(linearity_review), nrow(linearity_review) == 1L)
stopifnot(identical(linearity_review$Status[[1]], "possible nonlinearity"))

type1_result <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(sum_of_squares = "type1"))
stopifnot(identical(type1_result$results[[1]]$sum_of_squares, "type1"))
stopifnot(grepl("Sum of squares: Type I", type1_result$results[[1]]$note, fixed = TRUE))

type3_result <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(sum_of_squares = "type3"))
stopifnot(identical(type3_result$results[[1]]$sum_of_squares, "type3"))
stopifnot(grepl("Sum of squares: Type III", type3_result$results[[1]]$note, fixed = TRUE))

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
ordered_markers <- attr(ordered$results[[1]]$table, "note_markers", exact = TRUE)
stopifnot(is.data.frame(ordered_markers), any(ordered_markers$column == "Label"))
stopifnot(identical(ordered_markers$marker[order(ordered_markers$row)], c("a", "b", "c")))
ordered_html <- as.character(ancova_results_ui(ordered, variable_info))
stopifnot(grepl("coefficient-footnote-marker", ordered_html, fixed = TRUE))

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

data_rank_deficient <- data.frame(
  y = 1:6,
  group = rep(c("A", "B", "C"), each = 2),
  x = rep(c(0, 1, 0), each = 2)
)
variable_info_rank_deficient <- data.frame(
  name = c("y", "group", "x"),
  measurement = c("continuous", "category", "continuous"),
  stringsAsFactors = FALSE
)
captured_warnings <- character(0)
rank_deficient_result <- withCallingHandlers(
  prepare_ancova_results(data_rank_deficient, "y", "group", "x", variable_info_rank_deficient),
  warning = function(w) {
    captured_warnings <<- c(captured_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
stopifnot(!any(grepl("rank-deficient|non-estim", captured_warnings, ignore.case = TRUE)))
stopifnot(length(rank_deficient_result$results) == 1L)
stopifnot(grepl("rank-deficient", rank_deficient_result$results[[1]]$note, fixed = TRUE))

plots <- prepare_ancova_results(
  data_basic,
  "y",
  "group",
  "x",
  variable_info,
  options = list(plot_adjusted_means = TRUE, plot_raw_overlay = TRUE, plot_regression_lines = TRUE, plot_linearity_diagnostics = TRUE)
)
plot_html <- as.character(ancova_results_ui(plots, variable_info))
stopifnot(grepl("ANCOVA plots", plot_html, fixed = TRUE))
stopifnot(grepl("Adjusted mean error bar plot", plot_html, fixed = TRUE))
stopifnot(grepl("Linearity diagnostic: residuals vs x", plot_html, fixed = TRUE))

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
stopifnot(grepl("Complete-case analysis:", combined_note, fixed = TRUE))
stopifnot(grepl("Assumption decision rule:", combined_note, fixed = TRUE))
stopifnot(grepl("Sum of squares: Type II", combined_note, fixed = TRUE))
stopifnot(grepl("ES = effect size", combined_note, fixed = TRUE))
stopifnot(grepl("Post-hoc:", combined_note, fixed = TRUE))
stopifnot(regexpr("Analysis method:", combined_note, fixed = TRUE) < regexpr("ES = effect size", combined_note, fixed = TRUE))
stopifnot(regexpr("ES = effect size", combined_note, fixed = TRUE) < regexpr("Post-hoc:", combined_note, fixed = TRUE))
stopifnot(regexpr("Post-hoc:", combined_note, fixed = TRUE) < regexpr("Complete-case analysis:", combined_note, fixed = TRUE))
stopifnot(regexpr("Complete-case analysis:", combined_note, fixed = TRUE) < regexpr("Assumption decision rule:", combined_note, fixed = TRUE))
stopifnot(!grepl("y: Analysis method", combined_note, fixed = TRUE))
stopifnot(!grepl("y2: Analysis method", combined_note, fixed = TRUE))

overview <- ancova_model_overview_table(result, variable_info)
stopifnot(is.data.frame(overview), nrow(overview) == 1L)
stopifnot("Analysis" %in% names(overview))
stopifnot("Sum of squares" %in% names(overview))
stopifnot(all(c("Raw N", "Complete N", "Excluded N") %in% names(overview)))
stopifnot(overview[["Raw N"]][[1]] == nrow(data_basic))
stopifnot(overview[["Complete N"]][[1]] == result$results[[1]]$n)
stopifnot(identical(overview[["Sum of squares"]][[1]], "Type II SS"))

brown_forsythe <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(homogeneity_method = "brown_forsythe"))
stopifnot(identical(brown_forsythe$results[[1]]$assumptions$homogeneity_method, "Brown-Forsythe"))
breusch_pagan <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(homogeneity_method = "breusch_pagan"))
stopifnot(identical(breusch_pagan$results[[1]]$assumptions$homogeneity_method, "Breusch-Pagan"))
white <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(homogeneity_method = "white"))
stopifnot(identical(white$results[[1]]$assumptions$homogeneity_method, "White test"))

data_large <- data.frame(
  y = rnorm(180),
  group = rep(c("A", "B", "C"), each = 60),
  x = rnorm(180)
)
data_large$y <- data_large$y + 0.3 * data_large$x
large_auto <- prepare_ancova_results(data_large, "y", "group", "x", variable_info, options = list(normality_method = "auto"))
stopifnot(identical(large_auto$results[[1]]$assumptions$normality_method, "Lilliefors (K-S)"))

data_two_group <- subset(data_basic, group %in% c("A", "B"))
data_two_group$group <- droplevels(factor(data_two_group$group))
two_group <- prepare_ancova_results(data_two_group, "y", "group", "x", variable_info)
stopifnot(is.data.frame(two_group$results[[1]]$table))
stopifnot(!("post-hoc" %in% names(two_group$results[[1]]$table)))
stopifnot(nrow(two_group$results[[1]]$table) == 2L)

ranked <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(force_ranked = TRUE))
stopifnot(identical(ranked$results[[1]]$method, "Ranked ANCOVA"))
stopifnot("Adjusted rank mean" %in% names(ranked$results[[1]]$table))
stopifnot(!("Adjusted mean" %in% names(ranked$results[[1]]$table)))
stopifnot("Adjusted mean" %in% names(ranked$results[[1]]$display_table))
stopifnot(!("Adjusted rank mean" %in% names(ranked$results[[1]]$display_table)))
ranked_display_table <- ancova_combined_result_table(ranked, variable_info)
ranked_rank_table <- ancova_combined_result_table(ranked, variable_info, table_type = "rank")
stopifnot("M" %in% names(ranked_display_table))
stopifnot(!("Adjusted mean" %in% names(ranked_display_table)))
stopifnot(!("Adjusted rank mean" %in% names(ranked_display_table)))
stopifnot("Rank M" %in% names(ranked_rank_table))
stopifnot(!("Adjusted rank mean" %in% names(ranked_rank_table)))
stopifnot(!("Adjusted mean" %in% names(ranked_rank_table)))
ranked_html <- as.character(ancova_results_ui(ranked, variable_info))
stopifnot(grepl("Ranked ANCOVA table", ranked_html, fixed = TRUE))
stopifnot(grepl("ancova-normality-diagnostics-table", ranked_html, fixed = TRUE))
stopifnot(grepl("interpreted on the rank scale", ranked$results[[1]]$note, fixed = TRUE))

mixed_method <- multi
mixed_method$results[[2]] <- ranked$results[[1]]
mixed_method$results[[2]]$dependent <- "y2"
mixed_method_table <- ancova_combined_result_table(mixed_method, variable_info_multi)
stopifnot("M" %in% names(mixed_method_table))
stopifnot(!("Adjusted mean" %in% names(mixed_method_table)))
stopifnot(!("Adjusted rank mean" %in% names(mixed_method_table)))
mixed_method_markers <- attr(mixed_method_table, "note_markers", exact = TRUE)
stopifnot(is.data.frame(mixed_method_markers), any(mixed_method_markers$column == "p"))
stopifnot(!any(mixed_method_markers$column == "DV"))
stopifnot(!any(mixed_method_markers$column == "Effect size"))
mixed_method_note <- ancova_combined_note(mixed_method, variable_info_multi)
stopifnot(any(grepl("Ranked ANCOVA:", mixed_method_note, fixed = TRUE)))
stopifnot(regexpr("1. Analysis method:", mixed_method_note, fixed = TRUE) < regexpr("ES = effect size", mixed_method_note, fixed = TRUE))
stopifnot(regexpr("ES = effect size", mixed_method_note, fixed = TRUE) < regexpr("Post-hoc:", mixed_method_note, fixed = TRUE))

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
stopifnot(is.data.frame(interaction$results[[1]]$interaction_terms), nrow(interaction$results[[1]]$interaction_terms) >= 1L)
stopifnot(is.data.frame(interaction$results[[1]]$simple_effects), nrow(interaction$results[[1]]$simple_effects) == 3L)
stopifnot(grepl("Simple group effects", as.character(ancova_results_ui(interaction, variable_info_interaction)), fixed = TRUE))
stopifnot(grepl("Simple group effects are reported", interaction$results[[1]]$note, fixed = TRUE))

warn_only <- prepare_ancova_results(data_interaction, "y", "group", "x", variable_info_interaction, options = list(auto_method = "warn"))
stopifnot(identical(warn_only$results[[1]]$method, "ANCOVA"))
stopifnot(grepl("Automatic method switching is disabled", warn_only$results[[1]]$note, fixed = TRUE))
stopifnot(grepl("Assumption warning:", warn_only$results[[1]]$note, fixed = TRUE))
warn_only_review <- ancova_assumption_review_table(warn_only, variable_info_interaction)
stopifnot(identical(warn_only_review[["Decision mode"]][[1]], "Warn only"))

custom_alpha <- prepare_ancova_results(data_basic, "y", "group", "x", variable_info, options = list(decision_alpha = 0.01))
custom_alpha_review <- ancova_assumption_review_table(custom_alpha, variable_info)
stopifnot(identical(custom_alpha_review[["Decision alpha"]][[1]], ".010"))

html <- as.character(ancova_results_ui(result, variable_info))
stopifnot(grepl("Model overview", html, fixed = TRUE))
stopifnot(grepl("ANCOVA table", html, fixed = TRUE))
stopifnot(grepl("Assumption summary", html, fixed = TRUE))
stopifnot(grepl("Normality diagnostics", html, fixed = TRUE))

html_file <- tempfile(fileext = ".html")
write_ancova_results_html(result, html_file, variable_info)
stopifnot(file.exists(html_file))
html_text <- paste(readLines(html_file, warn = FALSE), collapse = " ")
stopifnot(grepl("Model overview", html_text, fixed = TRUE))
stopifnot(grepl("Assumption summary", html_text, fixed = TRUE))
stopifnot(grepl("Regression slope homogeneity", html_text, fixed = TRUE))
stopifnot(grepl("Normality diagnostics", html_text, fixed = TRUE))

report_html <- saved_ancova_results_html(result, variable_info, report_mode = TRUE)
stopifnot(grepl("Assumption summary", report_html, fixed = TRUE))
stopifnot(grepl("Regression slope homogeneity", report_html, fixed = TRUE))
stopifnot(grepl("Normality diagnostics", report_html, fixed = TRUE))

if (nzchar(find_pdf_chromium())) {
  pdf_file <- tempfile(fileext = ".pdf")
  write_ancova_results_pdf(result, pdf_file, variable_info)
  stopifnot(file.exists(pdf_file))
  stopifnot(file.info(pdf_file)$size > 0)
}

excel_file <- tempfile(fileext = ".xlsx")
save_ancova_excel_file(result, excel_file, variable_info)
stopifnot(file.exists(excel_file))
excel_sheets <- openxlsx::getSheetNames(excel_file)
stopifnot("Assumption summary" %in% excel_sheets)
stopifnot("Normality diagnostics" %in% excel_sheets)
stopifnot("Slope homogeneity" %in% excel_sheets)
stopifnot("Linearity diagnostics" %in% excel_sheets)
stopifnot("Collinearity diagnostics" %in% excel_sheets)

influence_excel_file <- tempfile(fileext = ".xlsx")
save_ancova_excel_file(influence, influence_excel_file, variable_info)
influence_excel_sheets <- openxlsx::getSheetNames(influence_excel_file)
stopifnot("Influence sensitivity" %in% influence_excel_sheets)

interaction_excel_file <- tempfile(fileext = ".xlsx")
save_ancova_excel_file(interaction, interaction_excel_file, variable_info_interaction)
interaction_excel_sheets <- openxlsx::getSheetNames(interaction_excel_file)
stopifnot("Interaction terms" %in% interaction_excel_sheets)
stopifnot("Simple effects" %in% interaction_excel_sheets)

figure_dir <- tempfile("ancova_figures_")
dir.create(figure_dir)
saved_figures <- save_ancova_figures_to_dir(plots, figure_dir, variable_info)
stopifnot(any(grepl("linearity_x", basename(saved_figures), fixed = TRUE)))
stopifnot(all(file.exists(saved_figures)))

cat("All ANCOVA validations passed.\n")
