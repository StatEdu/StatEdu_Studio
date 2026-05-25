all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_ttest_anova.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))
library(shiny)
library(htmltools)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

message("Checking t-test / ANOVA numbered notes...")
data <- data.frame(
  y = c(1:20, 1:20),
  g2 = rep(c("A", "B"), each = 20),
  g3 = rep(c("A", "B", "C", "A"), each = 10),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = c("y", "g2", "g3"),
  measurement = c("continuous", "binary", "category"),
  stringsAsFactors = FALSE
)

result <- prepare_ttest_anova_results(
  data,
  dependents = "y",
  factors = c("g2", "g3"),
  variable_info = variable_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE)
)

table <- result$results[[1]]$table
note <- result$results[[1]]$note

expect_true(grepl("[0-9]$", table[["Effect size"]][[1]]), "Expected first effect-size value to include a numbered marker")
expect_true(grepl("[0-9]$", table[["p"]][[3]]), "Expected Welch ANOVA p-value to include a numbered marker")
expect_true(grepl("[0-9]$", table[["Effect size"]][[3]]), "Expected second effect-size value to include a numbered marker")
expect_true(grepl("1\\. Effect size = Hedges' g\\.", note), "Expected numbered Hedges' g note")
expect_true(grepl("2\\. Welch test was used because homogeneity of variance was not satisfied\\.", note), "Expected numbered Welch note")
expect_true(grepl("3\\. Effect size = omega squared\\.", note), "Expected numbered omega squared note")

html <- as.character(tags_to_html(ttest_anova_results_ui(result)))
expect_true(grepl('class="coefficient-col-note-marker"', html, fixed = TRUE), "Expected a narrow footnote marker column to render")
expect_true(grepl('class="coefficient-note-cell-marker">1</sup>', html, fixed = TRUE), "Expected Hedges' g marker to render as superscript in a separate cell")
expect_true(grepl('class="coefficient-note-cell-marker">2</sup>', html, fixed = TRUE), "Expected p-value marker to render as superscript in a separate cell")
expect_true(grepl('class="coefficient-note-cell-marker">3</sup>', html, fixed = TRUE), "Expected omega squared marker to render as superscript in a separate cell")

message("Checking t-test / ANOVA post-hoc table...")
posthoc_data <- data.frame(
  y = c(1, 2, 2, 2, 3, 10, 11, 11, 12, 12, 20, 21, 22, 22, 23),
  group = rep(c("A", "B", "C"), each = 5),
  stringsAsFactors = FALSE
)
posthoc_info <- data.frame(
  name = c("y", "group"),
  var_label = c("Outcome", "Group"),
  measurement = c("continuous", "category"),
  stringsAsFactors = FALSE
)
posthoc_result <- prepare_ttest_anova_results(
  posthoc_data,
  dependents = "y",
  factors = "group",
  variable_info = posthoc_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, post_hoc_method = "tukey")
)
posthoc_table <- posthoc_result$results[[1]]$posthoc
expect_true(is.data.frame(posthoc_table) && nrow(posthoc_table) == 3, "Expected ANOVA post-hoc pairwise table")
expect_true(all(c("Variable", "Method", "Comparison", "p") %in% names(posthoc_table)), "Expected post-hoc table columns")
expect_true(all(posthoc_table$Method == "Tukey HSD"), "Expected Tukey HSD method in post-hoc table")
posthoc_html <- as.character(tags_to_html(ttest_anova_results_ui(posthoc_result)))
expect_true(grepl("<h4>Post-hoc</h4>", posthoc_html, fixed = TRUE), "Expected post-hoc section in ANOVA HTML")

message("Checking ordered post-hoc notation grouping...")
ordered_values <- c(rep(1, 3), rep(3, 3), rep(4, 3))
ordered_groups <- rep(c("1", "2", "3"), each = 3)
ordered_p <- matrix(
  1,
  nrow = 3,
  ncol = 3,
  dimnames = list(c("1", "2", "3"), c("1", "2", "3"))
)
ordered_p["3", "1"] <- ordered_p["1", "3"] <- .01
ordered_p["2", "1"] <- ordered_p["1", "2"] <- .02
ordered_p["3", "2"] <- ordered_p["2", "3"] <- .20
expect_true(
  identical(ttest_ordered_significance_notation(ordered_values, ordered_groups, ordered_p), "3, 2>1"),
  "Expected shared lower group notation to combine higher groups"
)

ordered_p["3", "2"] <- ordered_p["2", "3"] <- .01
ordered_p["2", "1"] <- ordered_p["1", "2"] <- .20
expect_true(
  identical(ttest_ordered_significance_notation(ordered_values, ordered_groups, ordered_p), "3>2, 1"),
  "Expected one higher group notation to retain multiple lower groups"
)

letter_values <- c(rep(3.61, 5), rep(3.46, 5), rep(3.84, 5))
letter_groups <- rep(c("1", "2", "3"), each = 5)
letter_p <- matrix(1, nrow = 3, ncol = 3, dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
letter_p["1", "2"] <- letter_p["2", "1"] <- .479
letter_p["1", "3"] <- letter_p["3", "1"] <- .512
letter_p["2", "3"] <- letter_p["3", "2"] <- .001
letter_map <- ttest_group_letters(letter_values, letter_groups, letter_p, ordered = FALSE)
expect_true(
  identical(unname(letter_map[c("1", "2", "3")]), c("ab", "b", "a")),
  "Expected compact post-hoc letters to mark non-significant bridge groups as ab"
)

message("Checking standalone nonparametric test helpers...")
nonparametric_data <- data.frame(
  group2 = rep(c("A", "B"), each = 12),
  group3 = rep(c("A", "B", "C"), each = 8),
  y2 = c(seq_len(12), seq_len(12) + 20),
  y3 = c(seq_len(8), seq_len(8) + 15, seq_len(8) + 30),
  stringsAsFactors = FALSE
)
nonparametric_result <- prepare_ttest_anova_results(
  nonparametric_data,
  dependents = c("y2", "y3"),
  factors = c("group2", "group3"),
  options = list(
    force_nonparametric = TRUE,
    normality_enabled = FALSE,
    normality_method = "none",
    nonparametric_post_hoc_method = "holm",
    ordered_significance = FALSE,
    effect_size = TRUE
  )
)
nonparametric_overview <- nonparametric_result$overview
expect_true(
  any(grepl("Mann-Whitney U", nonparametric_overview$Analysis, fixed = TRUE)),
  "Expected forced nonparametric two-group result to use Mann-Whitney U"
)
expect_true(
  any(grepl("Kruskal-Wallis", nonparametric_overview$Analysis, fixed = TRUE)),
  "Expected forced nonparametric three-group result to use Kruskal-Wallis"
)
expect_true(
  any(grepl("Pairwise Wilcoxon", nonparametric_overview$`Post-hoc`, fixed = TRUE)),
  "Expected significant Kruskal-Wallis results to include pairwise Wilcoxon post-hoc output"
)
expect_true(
  ncol(nonparametric_result$results[[1]]$table) == length(ttest_result_table_columns) &&
    all(names(nonparametric_result$results[[1]]$table)[-5] == ttest_result_table_columns[-5]),
  "Expected standalone nonparametric results to use the t-test / ANOVA table shape, with only the statistic label adapted"
)
expect_true(
  any(grepl("Selected by analysis menu", nonparametric_overview$Normality, fixed = TRUE)),
  "Expected standalone nonparametric overview to identify menu-selected nonparametric tests"
)
expect_true(
  grepl("[0-9]$", nonparametric_result$results[[1]]$table[["Effect size"]][[1]]),
  "Expected Mann-Whitney U results to include a Cliff's delta effect-size marker"
)
expect_true(
  grepl("Effect size = Cliff's delta", nonparametric_result$results[[1]]$note, fixed = TRUE),
  "Expected Mann-Whitney U results to label the effect size as Cliff's delta"
)

nonparametric_median <- prepare_ttest_anova_results(
  nonparametric_data,
  dependents = "y3",
  factors = "group3",
  options = list(
    force_nonparametric = TRUE,
    normality_enabled = FALSE,
    normality_method = "none",
    median_iqr = TRUE,
    effect_size = FALSE
  )
)
expect_true(
  all(c("Median", "Q1~Q3") %in% names(nonparametric_median$results[[1]]$table)),
  "Expected nonparametric Median(Q1~Q3) option to replace M and SD headers"
)
expect_true(
  !any(c("M", "SD") %in% names(nonparametric_median$results[[1]]$table)),
  "Expected nonparametric Median(Q1~Q3) table to omit M and SD headers"
)
expect_true(
  "nonparametric" %in% names(enabled_analysis_tabs()) && isTRUE(enabled_analysis_tabs()[["nonparametric"]]),
  "Expected Nonparametric Tests tab to be enabled"
)

message("All t-test / ANOVA validations passed.")
