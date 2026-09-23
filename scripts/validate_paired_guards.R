all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_paired_guards.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (.Platform$OS.type == "windows") {
  invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))
library(shiny)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

render_in_ui_language <- function(language, builder) {
  previous <- getOption("statedu.app_language", NULL)
  on.exit(options(statedu.app_language = previous), add = TRUE)
  options(statedu.app_language = language)
  as.character(htmltools::renderTags(builder())$html)
}

message("Checking paired guard conditions...")
expect_true(grepl('doi: "10.22934/statedu.studio"', paste(readLines(file.path(repo_root, "CITATION.cff"), warn = FALSE), collapse = "\n"), fixed = TRUE), "Expected CITATION.cff to include the active StatEdu Studio DOI")
data <- data.frame(
  pre = c(1, 2, 3, 4, 5),
  post = c(2, 3, 5, 7, 11),
  same_pre = c(1, 2, 3, 4, 5),
  same_post = c(1, 2, 3, 4, 5),
  constant_post = c(2, 3, 4, 5, 6),
  one_pre = c(1, NA, NA, NA, NA),
  one_post = c(2, NA, NA, NA, NA),
  ord_pre = c("low", "middle", "high", "middle", "low"),
  ord_post = c("middle", "middle", "high", "high", "low"),
  ord_third = c("high", "middle", "high", "high", "middle"),
  tie_pre = c(1, 2, 3, 4, 5),
  tie_post = c(2, 3, 4, 5, 6),
  t1 = c(1, 2, 3, 4, 5),
  t2 = c(1, 2, 3, 4, 5),
  t3 = c(1, 2, 3, 4, 5),
  binary_pre = c("yes", "yes", "no", "no", "yes"),
  binary_post = c("yes", "no", "no", "yes", "no"),
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = c(
    "continuous", "continuous", "continuous", "continuous", "continuous",
    "continuous", "continuous", "ordinal", "ordinal", "ordinal", "continuous",
    "continuous", "continuous", "continuous", "continuous", "binary", "binary"
  ),
  stringsAsFactors = FALSE
)
variable_info_mixed <- variable_info
variable_info_mixed$measurement[variable_info_mixed$name == "post"] <- "ordinal"

ordered_setup_state <- paired_setup_state(
  selected_names = c("x5", "QoL"),
  repeated_groups = list(c("QoL", "x5"))
)
expect_true(identical(ordered_setup_state$repeated_groups[[1]], c("QoL", "x5")), "Expected paired setup to preserve the user's selected order within a pair")
expect_true(grepl("QoL - x5", ordered_setup_state$repeated_items[[1]]$label, fixed = TRUE), "Expected paired setup labels to show the first selected variable first")
ordered_nonparametric_state <- nonparametric_paired_setup_state(
  selected_names = c("x5", "QoL"),
  repeated_groups = list(c("QoL", "x5"))
)
expect_true(identical(ordered_nonparametric_state$repeated_groups[[1]], c("QoL", "x5")), "Expected nonparametric paired setup to preserve the user's selected order within a pair")
expect_true(
  identical(paired_transfer_selection_order(c("x5", "QoL"), c("QoL", "x5"), c("x5", "QoL")), c("QoL", "x5")),
  "Expected paired transfer selection order to override DOM list order"
)
paired_two_setup <- paired_setup_state(
  selected_names = c("x1", "x2"),
  repeated_groups = list(c("x1", "x2"))
)
paired_two_setup_html <- as.character(htmltools::renderTags(paired_setup_panel(paired_two_setup))$html)
expect_true(grepl("paired_options_tabs", paired_two_setup_html, fixed = TRUE), "Expected paired options tabs to render before a 3+ row exists")
expect_true(grepl("paired-options-disabled-tab", paired_two_setup_html, fixed = TRUE), "Expected paired repeated options tab to be disabled before a 3+ row exists")
paired_three_setup <- paired_setup_state(
  selected_names = c("x1", "x2", "x3"),
  repeated_groups = list(c("x1", "x2", "x3"))
)
paired_three_setup_html <- as.character(htmltools::renderTags(paired_setup_panel(paired_three_setup))$html)
expect_true(grepl("paired_mean_sd", paired_three_setup_html, fixed = TRUE), "Expected paired M +/- SD option to remain visible with 3+ repeated variables")
expect_true(grepl("paired_median_iqr", paired_three_setup_html, fixed = TRUE), "Expected paired Median(Q1~Q3) option to remain visible with 3+ repeated variables")
expect_true(grepl("paired_time_label_1", paired_three_setup_html, fixed = TRUE), "Expected repeated variable label inputs to be added for 3+ repeated variables")
expect_true(grepl("paired_options_tabs", paired_three_setup_html, fixed = TRUE), "Expected paired 3+ options to render in tabs")
expect_true(grepl('data-value="Options"', paired_three_setup_html, fixed = TRUE) && grepl('data-value="Repeated"', paired_three_setup_html, fixed = TRUE), "Expected paired 3+ options to split default and repeated settings into tabs")
expect_true(!grepl("paired-options-disabled-tab", paired_three_setup_html, fixed = TRUE), "Expected paired repeated options tab to be enabled with a 3+ row")

valid <- prepare_paired_results(data, "pre", "post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(valid$scale_table) && nrow(valid$scale_table) == 1, "Expected valid paired t-test scale table")
expect_true(!is.data.frame(valid$skipped), "Expected no skipped table for valid paired t-test")

all_zero <- prepare_paired_results(data, "same_pre", "same_post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(all_zero$skipped) && grepl("all zero", all_zero$skipped$Reason[[1]], fixed = TRUE), "Expected all-zero differences to be skipped")

zero_variance <- prepare_paired_results(data, "pre", "constant_post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(zero_variance$skipped) && grepl("zero variance", zero_variance$skipped$Reason[[1]], fixed = TRUE), "Expected zero-variance t-test differences to be skipped")

too_few <- prepare_paired_results(data, "one_pre", "one_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE))
expect_true(is.data.frame(too_few$skipped) && grepl("At least two complete paired cases", too_few$skipped$Reason[[1]], fixed = TRUE), "Expected N<2 paired case to be skipped")
too_few_html <- as.character(htmltools::renderTags(paired_results_ui(too_few))$html)
expect_true(grepl("diagnostics-message-table", too_few_html, fixed = TRUE), "Expected paired warning/skipped diagnostics to use message-width table")
expect_true(grepl("width:58.000% !important", too_few_html, fixed = TRUE), "Expected paired warning/skipped diagnostics Message column to remain widest")
too_few_html_ko <- render_in_ui_language("ko", function() paired_results_ui(too_few))
too_few_html_en <- render_in_ui_language("en", function() paired_results_ui(too_few))
expect_true(
  grepl("경고 / 제외된 쌍", too_few_html_ko, fixed = TRUE) &&
    grepl("완전한 대응 사례가 최소 2개 필요합니다.", too_few_html_ko, fixed = TRUE) &&
    !grepl("At least two complete paired cases are required.", too_few_html_ko, fixed = TRUE),
  "Expected Korean paired appendix diagnostics to localize the title and dynamic guard reason"
)
expect_true(
  grepl("Warnings / skipped pairs", too_few_html_en, fixed = TRUE) &&
    grepl("At least two complete paired cases are required.", too_few_html_en, fixed = TRUE) &&
    !grepl("완전한 대응 사례가 최소 2개 필요합니다.", too_few_html_en, fixed = TRUE),
  "Expected English paired appendix diagnostics to remain English"
)

binary <- prepare_paired_results(data, "binary_pre", "binary_post", variable_info, options = list(effect_size = TRUE))
binary_html <- as.character(htmltools::renderTags(paired_results_ui(binary))$html)
expect_true(grepl("<th[^>]*text-align:left[^>]*>\\s*Pre\\s*</th>", binary_html, perl = TRUE), "Expected paired binary/categorical Pre header to be left aligned")

mismatch <- prepare_paired_results(data, c("pre", "same_pre"), c("post", "same_post"), variable_info_mixed, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(mismatch$skipped) && any(grepl("different measurement levels", mismatch$skipped$Reason, fixed = TRUE)), "Expected measurement-mismatched pair to be skipped")

ordinal <- prepare_paired_results(data, "ord_pre", "ord_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE))
expect_true(is.data.frame(ordinal$scale_table) && identical(ordinal$scale_table$Method[[1]], "Wilcoxon signed-rank test"), "Expected text ordinal pair to use Wilcoxon")
ordinal_html_ko <- render_in_ui_language("ko", function() paired_results_ui(ordinal))
ordinal_html_en <- render_in_ui_language("en", function() paired_results_ui(ordinal))
expect_true(
  grepl("모형 개요", ordinal_html_ko, fixed = TRUE) &&
    grepl("Wilcoxon 부호순위 검정", ordinal_html_ko, fixed = TRUE),
  "Expected Korean paired model-overview appendix body to follow the UI language"
)
expect_true(
  grepl("Model overview", ordinal_html_en, fixed = TRUE) &&
    grepl("Wilcoxon", ordinal_html_en, fixed = TRUE) &&
    !grepl("부호순위 검정", ordinal_html_en, fixed = TRUE),
  "Expected English paired model-overview appendix body to remain English"
)

paired_mean_sd <- prepare_paired_results(data, "pre", "post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE, cohen_d = TRUE, mean_sd = TRUE))
paired_mean_sd_html <- as.character(htmltools::renderTags(paired_results_ui(paired_mean_sd))$html)
expect_true(grepl("M \u00B1 SD", paired_mean_sd_html, fixed = TRUE), "Expected paired M +/- SD option to render a combined summary header")
expect_true(grepl("3.00 \u00B1 1.58", paired_mean_sd_html, fixed = TRUE), "Expected paired M +/- SD option to combine mean and SD in one cell")
expect_true(grepl(">\\s*g\\s*</th>", paired_mean_sd_html, perl = TRUE) && grepl(">\\s*d\\s*</th>", paired_mean_sd_html, perl = TRUE), "Expected paired effect-size headers to use g and d abbreviations")
expect_true(!grepl(">\\s*Hedges' g\\s*</th>", paired_mean_sd_html, perl = TRUE) && !grepl(">\\s*Cohen's d\\s*</th>", paired_mean_sd_html, perl = TRUE), "Expected paired effect-size headers to omit full effect-size names")
expect_true(grepl("g = Hedges' g", paired_mean_sd_html, fixed = TRUE) && grepl("d = Cohen's d", paired_mean_sd_html, fixed = TRUE), "Expected paired effect-size note to explain g and d")
expect_true(grepl('class="paired-two-col-stat" style="width:16.000% !important;"', paired_mean_sd_html, fixed = TRUE), "Expected paired Statistic column to be widened")
expect_true(grepl('class="paired-two-col-effect" style="width:8.000% !important;"', paired_mean_sd_html, fixed = TRUE), "Expected paired ES columns to be narrowed")
expect_true(!grepl("table-layout:fixed;width:840px", paired_mean_sd_html, fixed = TRUE) && !grepl("table-layout:fixed;width:900px", paired_mean_sd_html, fixed = TRUE), "Expected paired M +/- SD-only table to retain the default paired table layout")

paired_median_iqr <- prepare_paired_results(data, "ord_pre", "ord_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE, median_iqr = TRUE))
expect_true(identical(paired_median_iqr$scale_table$SummaryCenter[[1]], "Median"), "Expected paired Median(Q1~Q3) option to use median summaries for Wilcoxon rows")
expect_true(grepl("~", paired_median_iqr$scale_table$Pre_SD[[1]], fixed = TRUE), "Expected paired Median(Q1~Q3) option to place Q1~Q3 in the spread cell")
paired_median_iqr_html <- as.character(htmltools::renderTags(paired_results_ui(paired_median_iqr))$html)
expect_true(grepl("Median", paired_median_iqr_html, fixed = TRUE) && grepl("Q1~Q3", paired_median_iqr_html, fixed = TRUE), "Expected paired Median(Q1~Q3) option to render split median and Q1~Q3 headers")
expect_true(grepl(">\\s*r\\s*</th>", paired_median_iqr_html, perl = TRUE), "Expected paired Wilcoxon effect-size header to use r abbreviation")
expect_true(grepl("r = Wilcoxon signed-rank effect size", paired_median_iqr_html, fixed = TRUE), "Expected paired effect-size note to explain r")
expect_true(grepl("table-layout:fixed;width:900px", paired_median_iqr_html, fixed = TRUE), "Expected paired Median(Q1~Q3)-only table to use the wider option layout")
paired_mixed_median_iqr <- prepare_paired_results(data, c("pre", "ord_pre"), c("post", "ord_post"), variable_info, options = list(assumption_check = TRUE, effect_size = TRUE, median_iqr = TRUE))
paired_mixed_median_iqr_html <- as.character(htmltools::renderTags(paired_results_ui(paired_mixed_median_iqr))$html)
expect_true(grepl("M/Median", paired_mixed_median_iqr_html, fixed = TRUE) && grepl("SD/\\s*<br/>\\s*Q1~Q3", paired_mixed_median_iqr_html, perl = TRUE), "Expected mixed median split headers to render without crowding")

paired_combined_median <- prepare_paired_results(data, "ord_pre", "ord_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE, mean_sd = TRUE, median_iqr = TRUE))
paired_combined_median_html <- as.character(htmltools::renderTags(paired_results_ui(paired_combined_median))$html)
expect_true(grepl("Median(Q1~Q3)", paired_combined_median_html, fixed = TRUE), "Expected paired M +/- SD plus Median(Q1~Q3) options to render a combined median header")
expect_true(grepl("(", paired_combined_median$scale_table$Pre_MS[[1]], fixed = TRUE), "Expected paired M +/- SD plus Median(Q1~Q3) options to combine median and Q1~Q3 in one cell")
expect_true(grepl("table-layout:fixed;width:840px", paired_combined_median_html, fixed = TRUE), "Expected paired M +/- SD plus Median(Q1~Q3) table to use the combined option layout")
paired_mixed_combined_median <- prepare_paired_results(data, c("pre", "ord_pre"), c("post", "ord_post"), variable_info, options = list(assumption_check = TRUE, effect_size = TRUE, mean_sd = TRUE, median_iqr = TRUE))
paired_mixed_combined_median_html <- as.character(htmltools::renderTags(paired_results_ui(paired_mixed_combined_median))$html)
expect_true(grepl("M \u00B1 SD /\\s*<br/>\\s*Median\\(Q1~Q3\\)", paired_mixed_combined_median_html, perl = TRUE), "Expected paired M +/- SD plus Median(Q1~Q3) header to render as two lines")

outlier_data <- data.frame(pre = rep(0, 10), post = c(-2, -1, 0, 1, 2, 3, 4, 5, 100, -80))
outlier_info <- data.frame(name = names(outlier_data), measurement = c("continuous", "continuous"), stringsAsFactors = FALSE)
outlier_result <- prepare_paired_results(outlier_data, "pre", "post", outlier_info, options = list(assumption_check = TRUE, effect_size = TRUE))
expect_true(grepl("2 detected \\(IDs: 9, 10\\)", outlier_result$checks$Outliers[[1]], perl = TRUE), "Expected paired outlier summary to list top outlier IDs by severity")

nonparam_ties <- prepare_nonparametric_paired_results(data, "tie_pre", "tie_post", variable_info, options = list(effect_size = TRUE, median_iqr = TRUE))
expect_true(is.data.frame(nonparam_ties$warnings) && grepl("Tied absolute differences", nonparam_ties$warnings$Warning[[1]], fixed = TRUE), "Expected Wilcoxon tied-difference warning")
nonparam_ties_html_ko <- render_in_ui_language("ko", function() nonparametric_paired_results_ui(nonparam_ties))
nonparam_ties_html_en <- render_in_ui_language("en", function() nonparametric_paired_results_ui(nonparam_ties))
expect_true(
  grepl("비모수 대응표본 검정", nonparam_ties_html_ko, fixed = TRUE) &&
    grepl("절대 차이의 동률", nonparam_ties_html_ko, fixed = TRUE) &&
    !grepl("Tied absolute differences", nonparam_ties_html_ko, fixed = TRUE),
  "Expected Korean nonparametric paired appendix body and warning to follow the UI language"
)
expect_true(
  grepl("Nonparametric paired test", nonparam_ties_html_en, fixed = TRUE) &&
    grepl("Tied absolute differences", nonparam_ties_html_en, fixed = TRUE) &&
    !grepl("비모수 대응표본 검정", nonparam_ties_html_en, fixed = TRUE),
  "Expected English nonparametric paired appendix body and warning to remain English"
)

zero_and_tie_data <- data.frame(
  pre = c(1, 2, 3, 4, 5),
  post = c(1, 3, 4, 5, 6),
  stringsAsFactors = FALSE
)
zero_and_tie_info <- data.frame(
  name = names(zero_and_tie_data),
  measurement = c("ordinal", "ordinal"),
  stringsAsFactors = FALSE
)
zero_and_tie_result <- prepare_paired_results(
  zero_and_tie_data,
  "pre",
  "post",
  zero_and_tie_info,
  options = list(assumption_check = FALSE, effect_size = TRUE)
)
expect_true(
  is.data.frame(zero_and_tie_result$warnings) &&
    identical(
      zero_and_tie_result$warnings$Warning[[1]],
      "1 zero difference(s) were omitted from the Wilcoxon signed-rank calculation. Tied absolute differences were present; the large-sample Wilcoxon approximation was used."
    ),
  "Expected the actual paired result to retain the parameterized Wilcoxon warning before UI localization"
)
zero_and_tie_html_ko <- render_in_ui_language("ko", function() paired_results_ui(zero_and_tie_result))
zero_and_tie_html_en <- render_in_ui_language("en", function() paired_results_ui(zero_and_tie_result))
expect_true(
  grepl("차이가 0인 사례 1개를 Wilcoxon 부호순위 계산에서 제외했습니다.", zero_and_tie_html_ko, fixed = TRUE) &&
    grepl("절대 차이의 동률이 있어 큰 표본 Wilcoxon 근사를 사용했습니다.", zero_and_tie_html_ko, fixed = TRUE) &&
    !grepl("zero difference", zero_and_tie_html_ko, fixed = TRUE) &&
    !grepl("Tied absolute differences", zero_and_tie_html_ko, fixed = TRUE),
  "Expected the Korean paired appendix HTML to localize the combined parameterized Wilcoxon warning"
)
expect_true(
  grepl('data-result-table-role="main"', zero_and_tie_html_ko, fixed = TRUE) &&
    grepl('data-result-table-language="en"', zero_and_tie_html_ko, fixed = TRUE) &&
    grepl("Analysis method: Wilcoxon signed-rank test", zero_and_tie_html_ko, fixed = TRUE) &&
    grepl("r = Wilcoxon signed-rank effect size", zero_and_tie_html_ko, fixed = TRUE),
  "Expected the actual Korean-UI result HTML to keep its publication table and note in English"
)
expect_true(
  grepl("1 zero difference(s) were omitted", zero_and_tie_html_en, fixed = TRUE) &&
    grepl("Tied absolute differences were present", zero_and_tie_html_en, fixed = TRUE) &&
    !grepl("차이가 0인 사례", zero_and_tie_html_en, fixed = TRUE),
  "Expected the English paired appendix HTML to retain the combined parameterized Wilcoxon warning"
)
expect_true(
  identical(
    paired_appendix_text("1 zero difference was omitted from the Wilcoxon signed-rank calculation.", "ko"),
    "차이가 0인 사례 1개를 Wilcoxon 부호순위 계산에서 제외했습니다."
  ) &&
    identical(
      paired_appendix_text("7 zero differences were omitted from the Wilcoxon signed-rank calculation.", "ko"),
      "차이가 0인 사례 7개를 Wilcoxon 부호순위 계산에서 제외했습니다."
    ),
  "Expected singular and plural saved-result Wilcoxon warning variants to localize with their counts preserved"
)

rm_result <- prepare_paired_rm_results(
  data,
  variable_groups = list(c("pre", "post", "constant_post"), c("t1", "t2", "t3")),
  variable_info = variable_info,
  options = list(assumption_check = TRUE)
)
expect_true(is.data.frame(rm_result$display_table) && nrow(rm_result$display_table) == 1, "Expected valid RM row to still be analyzed")
expect_true(is.data.frame(rm_result$skipped) && grepl("identical within subjects", rm_result$skipped$Reason[[1]], fixed = TRUE), "Expected invalid RM row to be skipped")

rm_marker_data <- data.frame(
  pre = c(1, 2, 3, 3, 4, 5),
  post1 = c(2, 4, 5, 5, 6, 7),
  post2 = c(4, 6, 7, 8, 9, 10)
)
rm_marker_info <- data.frame(name = names(rm_marker_data), measurement = rep("continuous", 3), stringsAsFactors = FALSE)
rm_marker_result <- prepare_paired_rm_results(
  rm_marker_data,
  variable_groups = list(c("pre", "post1", "post2")),
  variable_info = rm_marker_info,
  options = list(assumption_check = FALSE)
)
expect_true(identical(as.character(rm_marker_result$display_table$Time1_marker[[1]]), "a") && identical(as.character(rm_marker_result$display_table$Time2_marker[[1]]), "b") && identical(as.character(rm_marker_result$display_table$Time3_marker[[1]]), "c"), "Expected paired RM display table to assign time markers a/b/c")
expect_true(all(c("a-b", "a-c") %in% as.character(rm_marker_result$display_table[1, c("ES_1_2_label", "ES_1_3_label")])), "Expected paired RM pairwise ES labels to use time markers")
expect_true(grepl("b>a", as.character(rm_marker_result$display_table$`Post-hoc`[[1]]), fixed = TRUE) || grepl("c>a", as.character(rm_marker_result$display_table$`Post-hoc`[[1]]), fixed = TRUE), "Expected paired RM post-hoc notation to use time markers")
rm_marker_html <- as.character(htmltools::renderTags(paired_rm_results_ui(rm_marker_result))$html)
expect_true(grepl("landscape-table-panel", rm_marker_html, fixed = TRUE), "Expected paired RM 3+ result table to use landscape layout")
expect_true(grepl("pre", rm_marker_html, fixed = TRUE) && grepl(">a</sup>", rm_marker_html, fixed = TRUE), "Expected paired RM HTML header to mark pre as a")
expect_true(grepl("post1", rm_marker_html, fixed = TRUE) && grepl(">b</sup>", rm_marker_html, fixed = TRUE), "Expected paired RM HTML header to mark post1 as b")
expect_true(grepl("post2", rm_marker_html, fixed = TRUE) && grepl(">c</sup>", rm_marker_html, fixed = TRUE), "Expected paired RM HTML header to mark post2 as c")
expect_true(grepl("white-space:nowrap;[^>]*>F</th>", rm_marker_html, perl = TRUE), "Expected paired RM Statistic header to stay on one line")
rm_marker_html_ko <- render_in_ui_language("ko", function() paired_rm_results_ui(rm_marker_result))
rm_marker_html_en <- render_in_ui_language("en", function() paired_rm_results_ui(rm_marker_result))
expect_true(
  grepl("반복측정 변수", rm_marker_html_ko, fixed = TRUE) &&
    grepl("반복측정 분산분석", rm_marker_html_ko, fixed = TRUE),
  "Expected Korean repeated-measures appendix header and method to follow the UI language"
)
expect_true(
  grepl("Repeated variables", rm_marker_html_en, fixed = TRUE) &&
    grepl("RM ANOVA", rm_marker_html_en, fixed = TRUE) &&
    !grepl("반복측정 분산분석", rm_marker_html_en, fixed = TRUE),
  "Expected English repeated-measures appendix header and method to remain English"
)
mixed_method_marker_table <- data.frame(Method = c("Friedman test", "RM ANOVA + Wilks' lambda"), stringsAsFactors = FALSE)
expect_true(identical(unname(paired_rm_method_marker_map(mixed_method_marker_table)), c("1", "2")), "Expected paired RM p-value method markers to use numeric markers")
wilks_note_table <- data.frame(
  `Repeated variables` = "x1 - x2 - x3",
  Method = "RM ANOVA + Wilks' lambda",
  `Wilks' lambda` = ".393",
  `GG epsilon` = ".970",
  `GG p` = "<.001",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
wilks_note <- paired_rm_table_method_note(wilks_note_table)
expect_true(grepl("RM ANOVA + Wilks' lambda", wilks_note, fixed = TRUE), "Expected paired RM note to state the Wilks method")
expect_true(grepl("Wilks' lambda = .393", wilks_note, fixed = TRUE), "Expected paired RM note to include only the Wilks detail used for the test")
expect_true(!grepl("GG epsilon", wilks_note, fixed = TRUE) && !grepl("GG p", wilks_note, fixed = TRUE), "Expected paired RM Wilks note not to mix in GG details")

message("Checking paired RM sphericity and binary coding...")
sphericity_matrix <- matrix(
  c(
    1.1, 1.6, 2.0,
    0.9, 1.2, 1.8,
    1.4, 1.7, 2.4,
    1.0, 1.5, 1.9,
    1.3, 1.8, 2.1,
    0.8, 1.4, 1.7,
    1.2, 1.6, 2.2,
    1.5, 1.9, 2.5
  ),
  ncol = 3,
  byrow = TRUE
)
sphericity_fit <- stats::lm(sphericity_matrix ~ 1)
sphericity_idata <- data.frame(time = factor(seq_len(ncol(sphericity_matrix))))
sphericity_ref <- stats::mauchly.test(sphericity_fit, M = ~time, X = ~1, idata = sphericity_idata)
sphericity_app <- paired_rm_sphericity(sphericity_matrix)
expect_true(abs(sphericity_app$w - unname(as.numeric(sphericity_ref$statistic))) < 1e-12, "Expected paired RM Mauchly W to match stats::mauchly.test")
expect_true(abs(sphericity_app$p - as.numeric(sphericity_ref$p.value)) < 1e-12, "Expected paired RM Mauchly p-value to match stats::mauchly.test")

binary_rm_data <- data.frame(
  t1 = c("yes", "yes", "no", "no", "yes", "no"),
  t2 = c("yes", "no", "no", "yes", "yes", "no"),
  t3 = c("no", "yes", "no", "yes", "yes", "no"),
  stringsAsFactors = FALSE
)
binary_rm_info <- data.frame(name = names(binary_rm_data), measurement = rep("binary", 3), stringsAsFactors = FALSE)
binary_rm_result <- prepare_paired_rm_results(
  binary_rm_data,
  variable_groups = list(names(binary_rm_data)),
  variable_info = binary_rm_info,
  options = list(effect_size = TRUE)
)
binary_coded <- paired_rm_binary_matrix(binary_rm_data)
binary_col_totals <- colSums(binary_coded$matrix)
binary_row_totals <- rowSums(binary_coded$matrix)
binary_total <- sum(binary_col_totals)
binary_expected_q <- (ncol(binary_coded$matrix) - 1) *
  (ncol(binary_coded$matrix) * sum(binary_col_totals ^ 2) - binary_total ^ 2) /
  (ncol(binary_coded$matrix) * binary_total - sum(binary_row_totals ^ 2))
expect_true(identical(binary_coded$levels, c("no", "yes")), "Expected paired RM binary coding to map the second observed level to 1")
expect_true(abs(as.numeric(binary_rm_result$count_table$Statistic[[1]]) - binary_expected_q) < 1e-12, "Expected Cochran's Q to use yes/no binary coding")
expect_true(identical(attr(binary_rm_result$count_table, "binary_levels", exact = TRUE), c("no", "yes")), "Expected paired RM count table to retain binary level labels")
binary_rm_html <- as.character(htmltools::renderTags(paired_rm_results_ui(binary_rm_result))$html)
expect_true(grepl(">no</th>", binary_rm_html, fixed = TRUE) && grepl(">yes</th>", binary_rm_html, fixed = TRUE), "Expected paired RM binary count headers to show actual levels")

nonparam_rm_result <- prepare_nonparametric_paired_rm_results(
  data,
  variable_groups = list(c("ord_pre", "ord_post", "ord_third"), c("t1", "t2", "t3")),
  variable_info = variable_info,
  options = list(effect_size = TRUE)
)
expect_true(is.data.frame(nonparam_rm_result$display_table) && nrow(nonparam_rm_result$display_table) == 1, "Expected valid nonparametric RM row to still be analyzed")
expect_true(is.data.frame(nonparam_rm_result$skipped) && grepl("identical within subjects", nonparam_rm_result$skipped$Reason[[1]], fixed = TRUE), "Expected invalid nonparametric RM row to be skipped")
nonparam_posthoc_display <- nonparametric_paired_posthoc_display_table(nonparam_rm_result)
expect_true(
  is.data.frame(nonparam_posthoc_display) &&
    all(c("ES metric", "ES") %in% names(nonparam_posthoc_display)) &&
    anyDuplicated(names(nonparam_posthoc_display)) == 0L,
  "Expected the nonparametric paired post-hoc main table to use distinct SCI effect-size headers"
)

invisible(capture.output(htmltools::renderTags(paired_results_ui(all_zero))))
invisible(capture.output(htmltools::renderTags(nonparametric_paired_results_ui(nonparam_ties))))
paired_xlsx <- tempfile(fileext = ".xlsx")
nonparametric_xlsx <- tempfile(fileext = ".xlsx")
save_paired_excel_file(all_zero, paired_xlsx)
save_nonparametric_paired_excel_file(nonparam_ties, nonparametric_xlsx)
rm_xlsx <- tempfile(fileext = ".xlsx")
save_paired_excel_file(rm_result, rm_xlsx)
expect_true(file.exists(paired_xlsx), "Expected paired Excel export with skipped pairs")
expect_true(file.exists(nonparametric_xlsx), "Expected nonparametric paired Excel export with warnings")
expect_true(file.exists(rm_xlsx), "Expected paired RM Excel export with skipped rows")

message("All paired guard validations passed.")
