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
source(file.path(repo_root, "R", "data_editor_missing.R"))
source(file.path(repo_root, "R", "data_editor_transform.R"))

message("Checking same-variable recoding rules...")
rules <- data.frame(old = c("1", "2", ""), new = c("10", "20", ""), stringsAsFactors = FALSE)
numeric_result <- recode_same_values(c(1, 2, 3, NA), rules, keep_unmatched = TRUE)
stopifnot(identical(numeric_result, c(10, 20, 3, NA)))

missing_result <- recode_same_values(c(1, 2, 3), rules, keep_unmatched = FALSE)
stopifnot(identical(missing_result, c(10, 20, NA)))

label_rules <- data.frame(old = c("1", "2"), new = c("low", "high"), stringsAsFactors = FALSE)
label_result <- recode_same_values(c(1, 2, 3), label_rules, keep_unmatched = TRUE)
stopifnot(identical(label_result, c("low", "high", "3")))

missing_to_value_rules <- data.frame(old = "NA", new = "0", stringsAsFactors = FALSE)
missing_to_value_result <- recode_same_values(c(1, NA, 3), missing_to_value_rules, keep_unmatched = TRUE)
stopifnot(identical(missing_to_value_result, c(1, 0, 3)))

blank_to_value_result <- recode_same_values(c("A", "", "B"), missing_to_value_rules, keep_unmatched = TRUE)
stopifnot(identical(blank_to_value_result, c("A", "0", "B")))

value_to_missing_rules <- data.frame(old = "2", new = "NA", stringsAsFactors = FALSE)
value_to_missing_result <- recode_same_values(c(1, 2, 3), value_to_missing_rules, keep_unmatched = TRUE)
stopifnot(identical(value_to_missing_result, c(1, NA, 3)))

single_rule_result <- recode_apply_single_rules(
  c(1, 2, 3),
  data.frame(old = "2", new = "20", stringsAsFactors = FALSE),
  keep_unmatched = TRUE
)
stopifnot(identical(single_rule_result, c(1, 20, 3)))
single_unmatched_entry <- list(
  rule_type = "single",
  keep_unmatched = TRUE,
  single_rules = data.frame(old = "2", new = "20", stringsAsFactors = FALSE)
)
stopifnot(identical(recode_unmatched_values(c(1, 2, 3), single_unmatched_entry), c("1", "3")))
stopifnot(grepl("1, 3", recode_unmatched_message(c(1, 2, 3), single_unmatched_entry), fixed = TRUE))
single_unmatched_entry$keep_unmatched <- FALSE
stopifnot(identical(recode_unmatched_message(c(1, 2, 3), single_unmatched_entry), NULL))

category_rule_input <- list(
  recode_same_cat_lower_to = 2,
  recode_same_cat_lower_upper_op = "le",
  recode_same_cat_lower_new = "low",
  recode_same_cat_middle_from_1 = 2,
  recode_same_cat_middle_lower_op_1 = "gt",
  recode_same_cat_middle_to_1 = 4,
  recode_same_cat_middle_upper_op_1 = "lt",
  recode_same_cat_middle_new_1 = "mid",
  recode_same_cat_upper_from = 4,
  recode_same_cat_upper_lower_op = "ge",
  recode_same_cat_upper_new = "high"
)
category_rule_result <- recode_apply_category_rules(c(1, 2, 3, 4, 5), category_rule_input, middle_count = 1, keep_unmatched = TRUE)
stopifnot(identical(category_rule_result, c("low", "low", "mid", "high", "high")))
category_missing_input <- category_rule_input
category_missing_input$recode_same_cat_middle_new_1 <- "NA"
category_missing_result <- recode_apply_category_rules(c(1, 2, 3, 4, 5), category_missing_input, middle_count = 1, keep_unmatched = TRUE)
stopifnot(identical(category_missing_result, c("low", "low", NA, "high", "high")))
category_unmatched_entry <- list(
  rule_type = "category",
  keep_unmatched = TRUE,
  category_input = category_rule_input,
  middle_count = 1L
)
stopifnot(identical(recode_unmatched_values(c("1", "2", "3", "4", "x"), category_unmatched_entry), "x"))

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

measurement_info <- data.frame(
  name = c("group", "rank", "score"),
  measurement = c("binary", "ordered", "continuous"),
  stringsAsFactors = FALSE
)
stopifnot(identical(recode_default_rule_type("group", measurement_info), "single"))
stopifnot(identical(recode_default_rule_type("rank", measurement_info), "single"))
stopifnot(identical(recode_default_rule_type("score", measurement_info), "category"))

single_two_entry <- list(
  rule_type = "single",
  single_rules = data.frame(old = c("0", "1"), new = c("1", "2"), stringsAsFactors = FALSE)
)
single_three_entry <- list(
  rule_type = "single",
  single_rules = data.frame(old = c("1", "2", "3"), new = c("1", "2", "3"), stringsAsFactors = FALSE)
)
category_two_entry <- list(
  rule_type = "category",
  middle_count = 1L,
  category_input = list(
    recode_same_cat_lower_new = "1",
    recode_same_cat_middle_new_1 = "2",
    recode_same_cat_upper_new = ""
  )
)
category_three_entry <- list(
  rule_type = "category",
  middle_count = 1L,
  category_input = list(
    recode_same_cat_lower_new = "1",
    recode_same_cat_middle_new_1 = "2",
    recode_same_cat_upper_new = "3"
  )
)
empty_entry <- list(rule_type = "single", single_rules = data.frame(old = character(0), new = character(0)))

stopifnot(identical(recode_infer_output_measurement(empty_entry, "ordered"), NULL))
stopifnot(identical(recode_infer_output_measurement(single_two_entry, "ordered"), "binary"))
stopifnot(identical(recode_infer_output_measurement(single_three_entry, "ordered"), "ordered"))
stopifnot(identical(recode_infer_output_measurement(single_three_entry, "category"), "category"))
stopifnot(identical(recode_infer_output_measurement(category_two_entry, "continuous"), "binary"))
stopifnot(identical(recode_infer_output_measurement(category_three_entry, "continuous"), "ordered"))
stopifnot(identical(recode_infer_output_measurement(category_three_entry, "category"), "category"))

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

message("Checking automatic missing-value detection helpers...")
missing_data <- data.frame(
  scale = c(1, 2, 3, 9, NA),
  score = c(10, 12, 999, 15, 11),
  comment = c("ok", "NA", "", "\ubaa8\ub984", "done"),
  group = factor(c("A", "N/A", "B", "A", "B")),
  check.names = FALSE
)
missing_candidates <- detect_missing_value_candidates(missing_data)
stopifnot(all(c("scale", "score", "comment", "group") %in% missing_candidates$variable))
stopifnot(any(missing_candidates$variable == "scale" & missing_candidates$value == "9"))
stopifnot(any(missing_candidates$variable == "score" & missing_candidates$value == "999"))
stopifnot(any(missing_candidates$variable == "comment" & missing_candidates$value == ""))
stopifnot(any(missing_candidates$variable == "comment" & missing_candidates$value == "\ubaa8\ub984"))
stopifnot(any(missing_candidates$variable == "group" & missing_candidates$value == "N/A"))

score_rule <- missing_candidates[missing_candidates$variable == "score" & missing_candidates$value == "999", , drop = FALSE]
score_result <- apply_missing_value_rules(missing_data$score, score_rule)
stopifnot(identical(score_result, c(10, 12, NA, 15, 11)))

comment_rules <- missing_candidates[missing_candidates$variable == "comment", , drop = FALSE]
comment_result <- apply_missing_value_rules(missing_data$comment, comment_rules)
stopifnot(identical(is.na(comment_result), c(FALSE, TRUE, TRUE, TRUE, FALSE)))

group_rule <- missing_candidates[missing_candidates$variable == "group" & missing_candidates$value == "N/A", , drop = FALSE]
group_result <- apply_missing_value_rules(missing_data$group, group_rule)
stopifnot(is.factor(group_result))
stopifnot(identical(is.na(group_result), c(FALSE, TRUE, FALSE, FALSE, FALSE)))

message("Checking formula-based variable transformation helpers...")
stopifnot(identical(transform_quote_variable("q1"), "q1"))
stopifnot(identical(transform_quote_variable("q 1"), "`q 1`"))
stopifnot(identical(transform_template_expression("row_mean", c("q1", "q2")), "row_mean(q1, q2)"))
stopifnot(identical(transform_template_expression("row_sum", c("q 1", "q-2")), "row_sum(`q 1`, `q-2`)"))
stopifnot(identical(transform_template_expression("reverse_1_5", "q1"), "6 - q1"))
stopifnot(identical(transform_template_expression("high_low_mean", "score"), "if_else(score >= mean(score, na.rm = TRUE), 'high', 'low')"))
displayed_functions <- unique(unlist(transform_function_groups(), use.names = FALSE))
stopifnot(all(displayed_functions %in% names(transform_allowed_functions())))
stopifnot(identical(transform_function_template("row_mean", c("q1", "q2")), "row_mean(q1, q2)"))
stopifnot(identical(transform_function_template("round", "score"), "round(score, 2)"))
stopifnot(identical(transform_function_template("if_else", "score"), "if_else(score >= 0, 'yes', 'no')"))
stopifnot(identical(transform_function_template("paste0", c("group", "id")), "paste0(group, '_', id)"))
stopifnot(identical(transform_function_template("today"), "today()"))
transform_data <- data.frame(
  x = c(1, 2, 3),
  y = c(10, 20, 30),
  `q 1` = c(2, 4, 6),
  `q-2` = c(1, 3, 5),
  txt = c("a", "b", "c"),
  start = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
  end = as.Date(c("2026-01-03", "2026-01-05", "2026-01-07")),
  check.names = FALSE
)
linear_result <- transform_eval_expression(transform_data, "2*x + 3*y")
stopifnot(identical(linear_result, c(32, 64, 96)))
implicit_linear_result <- transform_eval_expression(transform_data, "2x + 3y")
stopifnot(identical(implicit_linear_result, c(32, 64, 96)))
ln_result <- transform_eval_expression(transform_data, "ln(y)")
stopifnot(all.equal(ln_result, log(c(10, 20, 30)), check.attributes = FALSE))
text_result <- transform_eval_expression(transform_data, "paste0(txt, '_', x)")
stopifnot(identical(text_result, c("a_1", "b_2", "c_3")))
condition_result <- transform_eval_expression(transform_data, "if_else(x >= 2, 'high', 'low')")
stopifnot(identical(condition_result, c("low", "high", "high")))
case_result <- transform_eval_expression(transform_data, "case_when(x == 1, 'one', x == 2, 'two', default = 'other')")
stopifnot(identical(case_result, c("one", "two", "other")))
date_result <- transform_eval_expression(transform_data, "date_diff(end, start)")
stopifnot(identical(date_result, c(2, 3, 4)))
stats_result <- transform_eval_expression(transform_data, "row_mean(x, y)")
stopifnot(identical(stats_result, c(5.5, 11, 16.5)))
quoted_result <- transform_eval_expression(transform_data, "row_sum(`q 1`, `q-2`)")
stopifnot(identical(quoted_result, c(3, 7, 11)))
blocked <- tryCatch({
  transform_eval_expression(transform_data, "system('whoami')")
  FALSE
}, error = function(e) TRUE)
stopifnot(isTRUE(blocked))

message("All data editor recoding validations passed.")
