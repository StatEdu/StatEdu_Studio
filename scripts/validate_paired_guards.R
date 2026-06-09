all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_paired_guards.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages()
source_app_modules(dir = file.path(repo_root, "R"))
library(shiny)

expect_true <- function(value, label) {
  if (!isTRUE(value)) stop(label, call. = FALSE)
}

message("Checking paired guard conditions...")
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
  stringsAsFactors = FALSE
)
variable_info <- data.frame(
  name = names(data),
  measurement = c(
    "continuous", "continuous", "continuous", "continuous", "continuous",
    "continuous", "continuous", "ordinal", "ordinal", "ordinal", "continuous",
    "continuous", "continuous", "continuous", "continuous"
  ),
  stringsAsFactors = FALSE
)
variable_info_mixed <- variable_info
variable_info_mixed$measurement[variable_info_mixed$name == "post"] <- "ordinal"

valid <- prepare_paired_results(data, "pre", "post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(valid$scale_table) && nrow(valid$scale_table) == 1, "Expected valid paired t-test scale table")
expect_true(!is.data.frame(valid$skipped), "Expected no skipped table for valid paired t-test")

all_zero <- prepare_paired_results(data, "same_pre", "same_post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(all_zero$skipped) && grepl("all zero", all_zero$skipped$Reason[[1]], fixed = TRUE), "Expected all-zero differences to be skipped")

zero_variance <- prepare_paired_results(data, "pre", "constant_post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(zero_variance$skipped) && grepl("zero variance", zero_variance$skipped$Reason[[1]], fixed = TRUE), "Expected zero-variance t-test differences to be skipped")

too_few <- prepare_paired_results(data, "one_pre", "one_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE))
expect_true(is.data.frame(too_few$skipped) && grepl("At least two complete paired cases", too_few$skipped$Reason[[1]], fixed = TRUE), "Expected N<2 paired case to be skipped")

mismatch <- prepare_paired_results(data, c("pre", "same_pre"), c("post", "same_post"), variable_info_mixed, options = list(assumption_check = FALSE, effect_size = TRUE))
expect_true(is.data.frame(mismatch$skipped) && any(grepl("different measurement levels", mismatch$skipped$Reason, fixed = TRUE)), "Expected measurement-mismatched pair to be skipped")

ordinal <- prepare_paired_results(data, "ord_pre", "ord_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE))
expect_true(is.data.frame(ordinal$scale_table) && identical(ordinal$scale_table$Method[[1]], "Wilcoxon signed-rank test"), "Expected text ordinal pair to use Wilcoxon")

paired_mean_sd <- prepare_paired_results(data, "pre", "post", variable_info, options = list(assumption_check = FALSE, effect_size = TRUE, cohen_d = TRUE, mean_sd = TRUE))
paired_mean_sd_html <- as.character(htmltools::renderTags(paired_results_ui(paired_mean_sd))$html)
expect_true(grepl("M \u00B1 SD", paired_mean_sd_html, fixed = TRUE), "Expected paired M +/- SD option to render a combined summary header")
expect_true(grepl("3.00 \u00B1 1.58", paired_mean_sd_html, fixed = TRUE), "Expected paired M +/- SD option to combine mean and SD in one cell")
expect_true(grepl(">\\s*g\\s*</th>", paired_mean_sd_html, perl = TRUE) && grepl(">\\s*d\\s*</th>", paired_mean_sd_html, perl = TRUE), "Expected paired effect-size headers to use g and d abbreviations")
expect_true(!grepl(">\\s*Hedges' g\\s*</th>", paired_mean_sd_html, perl = TRUE) && !grepl(">\\s*Cohen's d\\s*</th>", paired_mean_sd_html, perl = TRUE), "Expected paired effect-size headers to omit full effect-size names")
expect_true(grepl("g = Hedges' g", paired_mean_sd_html, fixed = TRUE) && grepl("d = Cohen's d", paired_mean_sd_html, fixed = TRUE), "Expected paired effect-size note to explain g and d")

paired_median_iqr <- prepare_paired_results(data, "ord_pre", "ord_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE, median_iqr = TRUE))
expect_true(identical(paired_median_iqr$scale_table$SummaryCenter[[1]], "Median"), "Expected paired Median(Q1~Q3) option to use median summaries for Wilcoxon rows")
expect_true(grepl("~", paired_median_iqr$scale_table$Pre_SD[[1]], fixed = TRUE), "Expected paired Median(Q1~Q3) option to place Q1~Q3 in the spread cell")
paired_median_iqr_html <- as.character(htmltools::renderTags(paired_results_ui(paired_median_iqr))$html)
expect_true(grepl("Median", paired_median_iqr_html, fixed = TRUE) && grepl("Q1~Q3", paired_median_iqr_html, fixed = TRUE), "Expected paired Median(Q1~Q3) option to render split median and Q1~Q3 headers")
expect_true(grepl(">\\s*r\\s*</th>", paired_median_iqr_html, perl = TRUE), "Expected paired Wilcoxon effect-size header to use r abbreviation")
expect_true(grepl("r = Wilcoxon signed-rank effect size", paired_median_iqr_html, fixed = TRUE), "Expected paired effect-size note to explain r")

paired_combined_median <- prepare_paired_results(data, "ord_pre", "ord_post", variable_info, options = list(assumption_check = TRUE, effect_size = TRUE, mean_sd = TRUE, median_iqr = TRUE))
paired_combined_median_html <- as.character(htmltools::renderTags(paired_results_ui(paired_combined_median))$html)
expect_true(grepl("Median(Q1~Q3)", paired_combined_median_html, fixed = TRUE), "Expected paired M +/- SD plus Median(Q1~Q3) options to render a combined median header")
expect_true(grepl("(", paired_combined_median$scale_table$Pre_MS[[1]], fixed = TRUE), "Expected paired M +/- SD plus Median(Q1~Q3) options to combine median and Q1~Q3 in one cell")

nonparam_ties <- prepare_nonparametric_paired_results(data, "tie_pre", "tie_post", variable_info, options = list(effect_size = TRUE, median_iqr = TRUE))
expect_true(is.data.frame(nonparam_ties$warnings) && grepl("Tied absolute differences", nonparam_ties$warnings$Warning[[1]], fixed = TRUE), "Expected Wilcoxon tied-difference warning")

rm_result <- prepare_paired_rm_results(
  data,
  variable_groups = list(c("pre", "post", "constant_post"), c("t1", "t2", "t3")),
  variable_info = variable_info,
  options = list(assumption_check = TRUE)
)
expect_true(is.data.frame(rm_result$display_table) && nrow(rm_result$display_table) == 1, "Expected valid RM row to still be analyzed")
expect_true(is.data.frame(rm_result$skipped) && grepl("identical within subjects", rm_result$skipped$Reason[[1]], fixed = TRUE), "Expected invalid RM row to be skipped")

nonparam_rm_result <- prepare_nonparametric_paired_rm_results(
  data,
  variable_groups = list(c("ord_pre", "ord_post", "ord_third"), c("t1", "t2", "t3")),
  variable_info = variable_info,
  options = list(effect_size = TRUE)
)
expect_true(is.data.frame(nonparam_rm_result$display_table) && nrow(nonparam_rm_result$display_table) == 1, "Expected valid nonparametric RM row to still be analyzed")
expect_true(is.data.frame(nonparam_rm_result$skipped) && grepl("identical within subjects", nonparam_rm_result$skipped$Reason[[1]], fixed = TRUE), "Expected invalid nonparametric RM row to be skipped")

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
