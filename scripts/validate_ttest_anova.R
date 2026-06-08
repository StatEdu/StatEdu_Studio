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

message("Checking p-value formatting and note-marker parsing...")
expect_true(identical(format_p(.179), ".179"), "Expected p=.179 to keep three decimals without a leading zero")
expect_true(identical(format_p(.0004), "<.001"), "Expected p<.001 to display as <.001")
expect_true(identical(format_p("<.001"), "<.001"), "Expected legacy <.001 p-values to remain <.001")
plain_p_split <- result_split_inline_marker(".179", "", "p")
expect_true(
  identical(unname(plain_p_split), c(".179", "")),
  "Expected p=.179 without metadata not to split 9 into a note marker"
)
compact_marker_split <- result_split_inline_marker(".1799", "", "p")
expect_true(
  identical(unname(compact_marker_split), c(".179", "9")),
  "Expected compact p=.179 with marker 9 to split after three decimals"
)
docx_plain_p_split <- result_docx_split_marker(".179", "p")
expect_true(
  identical(unname(docx_plain_p_split), c(".179", "")),
  "Expected DOCX p=.179 without metadata not to split 9 into a note marker"
)

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
table_markers <- attr(table, "note_markers", exact = TRUE)

expect_true(!grepl("\\([0-9.]+,[0-9.]+\\)", table[[5]][[3]], perl = TRUE), "Expected default ANOVA F statistic to omit degrees of freedom")
df_result <- prepare_ttest_anova_results(
  data,
  dependents = "y",
  factors = c("g2", "g3"),
  variable_info = variable_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE)
)
expect_true(grepl("\\([0-9.]+,[0-9.]+\\)", df_result$results[[1]]$table[[5]][[3]], perl = TRUE), "Expected enabled ANOVA F statistic to include df1 and df2")
expect_true(isTRUE(attr(df_result$results[[1]]$table, "show_df", exact = TRUE)), "Expected show-df result tables to retain display metadata")
df_result_html <- as.character(tags_to_html(ttest_anova_results_ui(df_result)))
expect_true(grepl("coefficient-table-show-df", df_result_html, fixed = TRUE), "Expected show-df result tables to render with a dedicated CSS class")
expect_true(grepl("coefficient-col-statistic", df_result_html, fixed = TRUE), "Expected show-df statistic cells to render with a dedicated statistic column class")
expect_true(grepl("width:144px !important;min-width:144px !important;max-width:144px !important;", df_result_html, fixed = TRUE), "Expected show-df statistic cells to carry a wide inline width")
expect_true(grepl("padding-left:12px !important;", df_result_html, fixed = TRUE), "Expected show-df p cells to carry separation padding")
expect_true(!grepl("coefficient-table-show-df", as.character(tags_to_html(ttest_anova_results_ui(result))), fixed = TRUE), "Expected ordinary result tables not to use show-df CSS class")
mean_sd_df_result <- prepare_ttest_anova_results(
  data,
  dependents = "y",
  factors = c("g2", "g3"),
  variable_info = variable_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, mean_sd = TRUE, show_df = TRUE)
)
mean_sd_df_html <- as.character(tags_to_html(ttest_anova_results_ui(mean_sd_df_result)))
expect_true(isTRUE(attr(mean_sd_df_result$results[[1]]$table, "mean_sd", exact = TRUE)), "Expected mean-SD result tables to retain display metadata")
expect_true(grepl("coefficient-table-mean-sd", mean_sd_df_html, fixed = TRUE), "Expected mean-SD result tables to render with a dedicated CSS class")
expect_true(grepl("width:112px !important;min-width:112px !important;max-width:112px !important;", mean_sd_df_html, fixed = TRUE), "Expected mean-SD show-df statistic cells to keep their inline width")
trend_candidate_data <- data.frame(
  y = c(1.2, 1.4, 1.5, 1.6, 1.7, 2.4, 2.5, 2.7, 2.8, 2.9, 3.4, 3.6, 3.7, 3.8, 3.9),
  nominal_group = rep(c("A", "B", "C"), each = 5),
  ordered_group = rep(c("low", "middle", "high"), each = 5),
  stringsAsFactors = FALSE
)
trend_candidate_info <- data.frame(
  name = c("y", "nominal_group", "ordered_group"),
  measurement = c("continuous", "category", "ordered"),
  stringsAsFactors = FALSE
)
nominal_trend_result <- prepare_ttest_anova_results(
  trend_candidate_data,
  dependents = "y",
  factors = "nominal_group",
  variable_info = trend_candidate_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE, trend_analysis = TRUE)
)
nominal_trend_table <- nominal_trend_result$results[[1]]$table
nominal_trend_html <- as.character(tags_to_html(ttest_anova_results_ui(nominal_trend_result)))
expect_true(!("p for trend" %in% names(nominal_trend_table)), "Expected trend option not to add a trend column without an ordered independent variable")
expect_true(!grepl("trend analysis", nominal_trend_result$results[[1]]$note, fixed = TRUE), "Expected trend option not to add a trend note without an ordered independent variable")
expect_true(!grepl("coefficient-table-trend-analysis", nominal_trend_html, fixed = TRUE), "Expected non-ordered trend option not to use trend table spacing")
ordered_trend_result <- prepare_ttest_anova_results(
  trend_candidate_data,
  dependents = "y",
  factors = "ordered_group",
  variable_info = trend_candidate_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE, mean_sd = TRUE, trend_analysis = TRUE)
)
ordered_trend_table <- ordered_trend_result$results[[1]]$table
ordered_trend_html <- as.character(tags_to_html(ttest_anova_results_ui(ordered_trend_result)))
expect_true("p for trend" %in% names(ordered_trend_table), "Expected ordered independent variable to keep the trend column")
expect_true(isTRUE(attr(ordered_trend_table, "trend_analysis", exact = TRUE)), "Expected ordered trend result tables to retain display metadata")
expect_true(grepl("coefficient-table-trend-analysis", ordered_trend_html, fixed = TRUE), "Expected ordered trend result tables to render with a dedicated CSS class")
expect_true(grepl("coefficient-col-p-trend", ordered_trend_html, fixed = TRUE), "Expected p for trend cells to render with a dedicated column class")
expect_true(grepl("width:132px !important;min-width:132px !important;max-width:132px !important;", ordered_trend_html, fixed = TRUE), "Expected mean-SD show-df trend statistic cells to use the widened inline width")
ordered_trend_df_result <- prepare_ttest_anova_results(
  trend_candidate_data,
  dependents = "y",
  factors = "ordered_group",
  variable_info = trend_candidate_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, show_df = TRUE, trend_analysis = TRUE)
)
ordered_trend_df_html <- as.character(tags_to_html(ttest_anova_results_ui(ordered_trend_df_result)))
expect_true(grepl("width:136px !important;min-width:136px !important;max-width:136px !important;", ordered_trend_df_html, fixed = TRUE), "Expected show-df trend statistic cells to use the adjusted inline width")
expect_true(grepl("width:84px !important;min-width:84px !important;max-width:84px !important;", ordered_trend_df_html, fixed = TRUE), "Expected show-df trend p-for-trend cells to use the widened inline width")
mean_sd_result <- prepare_ttest_anova_results(
  data,
  dependents = "y",
  factors = c("g2", "g3"),
  variable_info = variable_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, mean_sd = TRUE)
)
expect_true("M \u00B1 SD" %in% names(mean_sd_result$results[[1]]$table), "Expected enabled mean-SD display column")
expect_true(!("M" %in% names(mean_sd_result$results[[1]]$table)), "Expected mean-SD display to omit separate M column")
expect_true(!("SD" %in% names(mean_sd_result$results[[1]]$table)), "Expected mean-SD display to omit separate SD column")
expect_true(grepl("\u00B1", mean_sd_result$results[[1]]$table[["M \u00B1 SD"]][[1]], fixed = TRUE), "Expected M plus/minus SD value")
expect_true(grepl("M \u00B1 SD = mean \u00B1 standard deviation.", mean_sd_result$results[[1]]$note, fixed = TRUE), "Expected mean-SD note")

message("Checking t-test / ANOVA model overview orientation...")
overview_layout_data <- data.frame(
  group = rep(c("A", "B"), each = 20),
  y1 = c(1:20, 2:21),
  y2 = c(2:21, 3:22),
  y3 = c(3:22, 4:23),
  y4 = c(4:23, 5:24),
  y5 = c(5:24, 6:25),
  stringsAsFactors = FALSE
)
overview_layout_info <- data.frame(
  name = c("group", paste0("y", 1:5)),
  measurement = c("binary", rep("continuous", 5)),
  stringsAsFactors = FALSE
)
overview_4 <- prepare_ttest_anova_results(
  overview_layout_data,
  dependents = paste0("y", 1:4),
  factors = "group",
  variable_info = overview_layout_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE)
)
overview_4_table <- ttest_result_overview_tables(overview_4)$overview
overview_4_assumption <- ttest_result_overview_tables(overview_4)$assumption_review
overview_4_html <- as.character(tags_to_html(ttest_anova_results_ui(overview_4)))
expect_true(identical(attr(overview_4_table, "dependent_count", exact = TRUE), 4L), "Expected four-variable overview to retain dependent count")
expect_true(identical(attr(overview_4_assumption, "dependent_count", exact = TRUE), 4L), "Expected four-variable assumption review to retain dependent count")
expect_true(grepl("width:100%", as.character(tags_to_html(model_overview_html_table(overview_4_table))), fixed = TRUE), "Expected overview table to fill its current container")
expect_true(!grepl("ttest-anova-overview-panel landscape-table-panel", overview_4_html, fixed = TRUE), "Expected four-variable overview to stay portrait")
expect_true(!grepl("ttest-anova-assumption-review-panel landscape-table-panel", overview_4_html, fixed = TRUE), "Expected four-variable assumption review to stay portrait")

overview_5 <- prepare_ttest_anova_results(
  overview_layout_data,
  dependents = paste0("y", 1:5),
  factors = "group",
  variable_info = overview_layout_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE)
)
overview_5_table <- ttest_result_overview_tables(overview_5)$overview
overview_5_assumption <- ttest_result_overview_tables(overview_5)$assumption_review
overview_5_html <- as.character(tags_to_html(ttest_anova_results_ui(overview_5)))
overview_5_saved <- as.character(saved_ttest_anova_results_html(overview_5, report_mode = TRUE))
expect_true(identical(attr(overview_5_table, "dependent_count", exact = TRUE), 5L), "Expected five-variable overview to retain dependent count")
expect_true(identical(attr(overview_5_assumption, "dependent_count", exact = TRUE), 5L), "Expected five-variable assumption review to retain dependent count")
expect_true(grepl("width:100%", as.character(tags_to_html(model_overview_html_table(overview_5_table))), fixed = TRUE), "Expected five-variable overview table to fill its current container")
expect_true(grepl("ttest-anova-overview-panel landscape-table-panel", overview_5_html, fixed = TRUE), "Expected five-variable overview panel to use landscape class")
expect_true(grepl("ttest-anova-assumption-review-panel landscape-table-panel", overview_5_html, fixed = TRUE), "Expected five-variable assumption review panel to use landscape class")
expect_true(grepl("print-mixed-landscape", overview_5_saved, fixed = TRUE), "Expected saved five-variable t-test / ANOVA report to enable mixed landscape print mode")

expect_true(is.data.frame(table_markers), "Expected numbered note metadata")
expect_true(any(table_markers$row == 1L & table_markers$column == "Effect size"), "Expected first effect-size value to include a numbered marker")
expect_true(!any(table_markers$row == 3L & table_markers$column == "p"), "Expected single Welch ANOVA method note to omit a p-value marker")
expect_true(any(table_markers$row == 3L & table_markers$column == "Effect size"), "Expected second effect-size value to include a numbered marker")
expect_true(grepl("1\\. ES = effect size \\(Hedges' g\\)\\.", note), "Expected numbered Hedges' g note")
expect_true(grepl("Welch test was used because homogeneity of variance was not satisfied\\.", note), "Expected unnumbered Welch note")
expect_true(grepl("2\\. ES = effect size \\(omega squared\\)\\.", note), "Expected numbered omega squared note")
expect_true(regexpr("Welch test", note, fixed = TRUE) < regexpr("Post-hoc:", note, fixed = TRUE), "Expected analysis-method notes before post-hoc notes")
expect_true(regexpr("Post-hoc:", note, fixed = TRUE) < regexpr("ES = effect size", note, fixed = TRUE), "Expected post-hoc notes before effect-size notes")

html <- as.character(tags_to_html(ttest_anova_results_ui(result)))
expect_true(!grepl('class="coefficient-col-note-marker"', html, fixed = TRUE), "Expected footnote markers to render inline without a narrow marker column")
expect_true(grepl('class="coefficient-footnote-marker">1</sup>', html, fixed = TRUE), "Expected Hedges' g marker to render as inline superscript")
expect_true(grepl('class="coefficient-footnote-marker">2</sup>', html, fixed = TRUE), "Expected omega squared marker to render as inline superscript")
style_css <- readLines(file.path(repo_root, "www", "style.css"), warn = FALSE, encoding = "UTF-8")
style_text <- paste(style_css, collapse = "\n")
expect_true(
  grepl(".coefficient-footnote-marker", style_text, fixed = TRUE) &&
    grepl("width: 0;", style_text, fixed = TRUE) &&
    grepl("vertical-align: super;", style_text, fixed = TRUE),
  "Expected inline footnote markers to use zero-width superscript styling so numeric values remain aligned"
)
manual_marker_table <- data.frame(
  Variable = "x",
  p = ".229 1",
  `Effect size` = "-.099 2",
  check.names = FALSE
)
manual_marker_html <- as.character(tags_to_html(coefficient_html_table(manual_marker_table)))
expect_true(
  grepl("\\.229\\s*<sup class=\"coefficient-footnote-marker\">1</sup>", manual_marker_html, perl = TRUE),
  "Expected spaced p-value marker to be split into a non-wrapping superscript"
)
expect_true(
  grepl("-\\.099\\s*<sup class=\"coefficient-footnote-marker\">2</sup>", manual_marker_html, perl = TRUE),
  "Expected spaced effect-size marker to be split into a non-wrapping superscript"
)
expect_true(
  grepl("white-space:nowrap;overflow-wrap:normal;word-break:normal;", manual_marker_html, fixed = TRUE),
  "Expected p/effect-size cells to prevent value-marker wrapping"
)
attr_marker_table <- data.frame(
  Variable = "x",
  p = ".008",
  `Effect size` = ".022",
  check.names = FALSE
)
attr(attr_marker_table, "note_markers") <- data.frame(
  row = c(1L, 1L),
  column = c("p", "Effect size"),
  marker = c("8", "1"),
  stringsAsFactors = FALSE
)
attr_marker_html <- as.character(tags_to_html(coefficient_html_table(attr_marker_table)))
expect_true(
  grepl("\\.008\\s*<sup class=\"coefficient-footnote-marker\">8</sup>", attr_marker_html, perl = TRUE),
  "Expected p=.008 with marker 8 to retain all three decimal places"
)
expect_true(
  grepl("\\.022\\s*<sup class=\"coefficient-footnote-marker\">1</sup>", attr_marker_html, perl = TRUE),
  "Expected effect size=.022 with marker 1 to retain all three decimal places"
)

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

ordered_marker_data <- data.frame(
  y = c(1:20, 10:29, 30:49),
  group = rep(c("A", "B", "C"), each = 20),
  stringsAsFactors = FALSE
)
ordered_marker_info <- data.frame(
  name = c("y", "group"),
  measurement = c("continuous", "category"),
  stringsAsFactors = FALSE
)
ordered_marker_result <- prepare_ttest_anova_results(
  ordered_marker_data,
  dependents = "y",
  factors = "group",
  variable_info = ordered_marker_info,
  options = list(effect_size = TRUE, normality_enabled = FALSE, ordered_significance = TRUE)
)
ordered_marker_table <- ordered_marker_result$results[[1]]$table
ordered_marker_rows <- attr(ordered_marker_table, "note_markers", exact = TRUE)
expect_true(
  is.data.frame(ordered_marker_rows) &&
    all(c("a", "b", "c") %in% ordered_marker_rows$marker) &&
    all(ordered_marker_rows$column == "Value") &&
    identical(ordered_marker_rows$marker[order(ordered_marker_rows$row)], c("a", "b", "c")),
  "Expected ordered post-hoc markers to be attached to Value cells in displayed value order"
)
expect_true(
  identical(as.character(ordered_marker_table[["post-hoc"]][1:2]), c("c>a,b", "b>a")),
  "Expected multi-line ordered post-hoc notation to use subsequent table rows instead of line breaks in one cell"
)
ordered_marker_html <- as.character(tags_to_html(ttest_anova_results_ui(ordered_marker_result)))
expect_true(
  grepl("coefficient-footnote-marker", ordered_marker_html, fixed = TRUE) &&
    (grepl("c&gt;a,b", ordered_marker_html, fixed = TRUE) || grepl("c>a,b", ordered_marker_html, fixed = TRUE)),
  "Expected ordered post-hoc markers to render as superscripts with marker comparisons"
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
nonparametric_overview <- ttest_result_flat_overview(nonparametric_result)
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
  all(names(nonparametric_result$results[[1]]$table) == c(
    "Variable", "Value", "M", "SD", "z/x²", "p", "Effect size", "post-hoc"
  )),
  "Expected standalone nonparametric results to use the t-test / ANOVA table shape, with only the statistic label adapted"
)
expect_true(
  any(grepl("Selected by analysis menu", nonparametric_overview$Normality, fixed = TRUE)),
  "Expected standalone nonparametric overview to identify menu-selected nonparametric tests"
)
expect_true(
  any((attr(nonparametric_result$results[[1]]$table, "note_markers", exact = TRUE) %||% data.frame())$column == "Effect size"),
  "Expected mixed nonparametric effect-size notes to include value markers"
)
expect_true(
  grepl("ES = effect size (Cliff's delta)", nonparametric_result$results[[1]]$note, fixed = TRUE),
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

message("Checking t-test / ANOVA guards and partial skips...")
guard_data <- data.frame(
  y_valid = c(1, 2, 3, 6, 7, 8),
  y_constant = rep(5, 6),
  y_zero_group_sd = c(1, 1, 1, 2, 3, 4),
  g_good = rep(c("A", "B"), each = 3),
  g_small = c("A", "A", "B", "B", "C", "B"),
  stringsAsFactors = FALSE
)
guard_result <- prepare_ttest_anova_results(
  guard_data,
  dependents = c("y_valid", "y_constant", "y_zero_group_sd"),
  factors = c("g_good", "g_small"),
  options = list(effect_size = TRUE, normality_enabled = FALSE)
)
expect_true(
  length(guard_result$results) >= 1 &&
    is.data.frame(guard_result$overview) &&
    "y_valid" %in% names(guard_result$overview) &&
    any(nzchar(as.character(guard_result$overview$y_valid %||% ""))),
  "Expected valid t-test / ANOVA combinations to continue running"
)
expect_true(
  is.data.frame(guard_result$skipped) && nrow(guard_result$skipped) >= 2,
  "Expected invalid t-test / ANOVA combinations to be collected as skipped analyses"
)
expect_true(
  any(grepl("at least 2 valid observations", guard_result$skipped$Reason, fixed = TRUE)),
  "Expected group-size guard to skip only the invalid combination"
)
expect_true(
  any(grepl("no variance", guard_result$skipped$Reason, fixed = TRUE)),
  "Expected zero dependent variance to be skipped"
)
expect_true(
  is.data.frame(guard_result$warnings) && any(grepl("Zero standard deviation", guard_result$warnings$Warning, fixed = TRUE)),
  "Expected zero group standard deviation warning"
)
guard_html <- as.character(tags_to_html(ttest_anova_results_ui(guard_result)))
expect_true(
  grepl("<h3>Warnings / skipped analyses</h3>", guard_html, fixed = TRUE),
  "Expected combined warnings / skipped analyses section in t-test / ANOVA HTML"
)

guard_xlsx <- tempfile(fileext = ".xlsx")
save_ttest_anova_excel_file(guard_result, guard_xlsx)
guard_sheets <- openxlsx::getSheetNames(guard_xlsx)
expect_true("Warnings" %in% guard_sheets, "Expected t-test / ANOVA warnings Excel sheet")
expect_true("Skipped analyses" %in% guard_sheets, "Expected t-test / ANOVA skipped Excel sheet")

message("All t-test / ANOVA validations passed.")
