script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_data_editor_recode.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_io.R"))
source(file.path(repo_root, "R", "data_category_labels.R"))
source(file.path(repo_root, "R", "data_editor_recode.R"))
source(file.path(repo_root, "R", "data_editor_likert.R"))

message("Checking same-variable recoding rules...")
rules <- data.frame(old = c("1", "2", ""), new = c("10", "20", ""), stringsAsFactors = FALSE)
numeric_result <- recode_same_values(c(1, 2, 3, NA), rules, keep_unmatched = TRUE)
stopifnot(identical(numeric_result, c(10, 20, 3, NA)))

missing_result <- recode_same_values(c(1, 2, 3), rules, keep_unmatched = FALSE)
stopifnot(identical(missing_result, c(10, 20, NA)))

label_rules <- data.frame(old = c("1", "2"), new = c("low", "high"), stringsAsFactors = FALSE)
label_result <- recode_same_values(c(1, 2, 3), label_rules, keep_unmatched = TRUE)
stopifnot(identical(label_result, c("low", "high", "3")))

reverse_result <- reverse_score_values(c(1, 2, 3, 4, 5, NA), minimum = 1, maximum = 5)
stopifnot(identical(reverse_result, c(5, 4, 3, 2, 1, NA)))

observed_reverse_result <- reverse_score_values(c(2, 3, 4, NA))
stopifnot(identical(observed_reverse_result, c(4, 3, 2, NA)))

factor_reverse_result <- reverse_score_values(factor(c("1", "2", "5")), minimum = 1, maximum = 5)
stopifnot(identical(factor_reverse_result, c(5, 4, 1)))

message("Checking different-variable recoding helpers...")
sample_data <- data.frame(
  id = 1:4,
  q1 = c(1, 2, 3, 5),
  q2 = c("1", "2.5", "6", "x"),
  check.names = FALSE
)
range_result <- recode_selected_range(sample_data, c("q1", "q2"))
stopifnot(identical(range_result, c(minimum = 1, maximum = 6)))

issues <- recode_range_issues(sample_data, c("q1", "q2"), minimum = 1, maximum = 5)
stopifnot(nrow(issues) == 3)
stopifnot(identical(as.character(issues$Variable), c("q2", "q2", "q2")))
stopifnot(identical(as.character(issues$Value), c("2.5", "6", "x")))

display_issues <- coding_error_issues_display(issues)
stopifnot("Corrected value" %in% names(display_issues))
stopifnot(grepl("coding_error_fix_1", display_issues$`Corrected value`[[1]], fixed = TRUE))
stopifnot(grepl("coding_error_apply_one", display_issues$`Corrected value`[[1]], fixed = TRUE))
stopifnot(grepl('tabindex="-1"', display_issues$`Corrected value`[[1]], fixed = TRUE))

stopifnot(identical(recode_new_variable_name("{variable}_R", "q1"), "q1_R"))
stopifnot(identical(recode_new_variable_name("_rev", "q1", literal = FALSE), "q1_rev"))
stopifnot(identical(recode_new_variable_name("custom_name", "q1", literal = TRUE), "custom_name"))

message("Checking automatic variable calculation helpers...")
calculation_data <- data.frame(
  item1 = c(1, 2, NA, NA),
  item2 = c(3, 4, 6, NA),
  item3 = c(5, NA, 8, NA),
  check.names = FALSE
)
calculated <- calculate_variable_outputs(
  calculation_data,
  c("item1", "item2", "item3"),
  c("mean", "sum", "sd", "var"),
  "scale"
)
stopifnot(identical(names(calculated), c("M_scale", "S_scale", "SD_scale", "Var_scale")))
stopifnot(identical(calculated$M_scale, c(3, 3, 7, NA)))
stopifnot(identical(calculated$S_scale, c(9, 6, 14, NA)))
stopifnot(all.equal(calculated$SD_scale, c(2, sqrt(2), sqrt(2), NA), check.attributes = FALSE))
stopifnot(all.equal(calculated$Var_scale, c(4, 2, 2, NA), check.attributes = FALSE))

message("Checking automatic Likert detection and conversion helpers...")
likert_data <- data.frame(
  sat1 = c("매우 불만족", "불만족", "보통", "만족", "매우 만족"),
  sat2 = c("불만족", "보통", "만족", "매우 만족", "매우 불만족"),
  note = c("a", "b", "c", "d", "e"),
  check.names = FALSE
)
detected <- detect_likert_variables(likert_data)
stopifnot(nrow(detected) == 2)
summary <- likert_group_summary(detected)
stopifnot(nrow(summary) == 1)
stopifnot(summary$variable_count[[1]] == 2)
mapping <- detected$mapping[[1]]
stopifnot(identical(as.character(mapping$label), c("매우 불만족", "불만족", "보통", "만족", "매우 만족")))
stopifnot(identical(as.numeric(mapping$value), c(1, 2, 3, 4, 5)))
converted <- recode_likert_values(likert_data$sat1, mapping)
stopifnot(identical(converted, c(1, 2, 3, 4, 5)))
reversed <- recode_likert_values(likert_data$sat1, mapping, reverse = TRUE)
stopifnot(identical(reversed, c(5, 4, 3, 2, 1)))
payload <- likert_category_payload(c("sat1"), mapping)
stopifnot(identical(payload$sat1$value_1, "1"))
stopifnot(identical(payload$sat1$label_5, "매우 만족"))

partial_likert_data <- data.frame(
  item1 = c("1 (전혀 그렇지 않다)", "2 (그렇지 않다)", "3 (보통이다)", "4 (그렇다)", "5 (매우 그렇다)"),
  item2 = c("2 (그렇지 않다)", "3 (보통이다)", "4 (그렇다)", "5 (매우 그렇다)", "5 (매우 그렇다)"),
  check.names = FALSE
)
partial_detected <- detect_likert_variables(partial_likert_data)
partial_summary <- likert_group_summary(partial_detected)
stopifnot(nrow(partial_summary) == 1)
stopifnot(partial_summary$variable_count[[1]] == 2)
stopifnot(partial_summary$levels[[1]] == 5)
partial_mapping <- likert_group_representative_mapping(partial_detected)
stopifnot(identical(as.numeric(partial_mapping$value), c(1, 2, 3, 4, 5)))
partial_choices <- likert_variable_choice_labels(partial_detected, partial_mapping)
stopifnot(identical(unname(partial_choices), c("item1", "item2")))
stopifnot(grepl("observed 4 of 5 levels", names(partial_choices)[[2]], fixed = TRUE))
partial_converted <- recode_likert_values(partial_likert_data$item2, partial_mapping)
stopifnot(identical(partial_converted, c(2, 3, 4, 5, 5)))

message("All data editor recoding validations passed.")
