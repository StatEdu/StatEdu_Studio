invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))

source(file.path("R", "analysis_meta.R"), encoding = "UTF-8")

close_enough <- function(actual, expected, tolerance = 1e-10) {
  isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance))
}

g_means <- meta_normalize_effect(list(
  study_id = "G1", family = "g", input_type = "means", direction = "positive",
  m1 = 10, sd1 = 2, n1 = 50, m0 = 8, sd0 = 2, n0 = 50
))
expected_j <- 0.992324087420560
stopifnot(g_means$status == "valid")
stopifnot(close_enough(g_means$yi, expected_j))
stopifnot(close_enough(g_means$vi, 0.044923535472375))

metadata_effect <- meta_normalize_effect(list(
  study_id = "META-1", study_name = "Kim et al.", publication_year = 2024,
  outcome = "Depression", predictor = "Intervention", family = "g", input_type = "g_se", direction = "positive",
  g = 0.35, se = 0.10,
  moderator_categorical = "region=Asia; design=RCT",
  moderator_continuous = "mean_age=42.5; female_percent=60"
))
stopifnot(metadata_effect$status == "valid")
stopifnot(metadata_effect$study_name[[1]] == "Kim et al.", metadata_effect$publication_year[[1]] == 2024)
stopifnot(metadata_effect$predictor[[1]] == "Intervention")
stopifnot(metadata_effect$moderator_categorical[[1]] == "region=Asia; design=RCT")
stopifnot(metadata_effect$moderator_continuous[[1]] == "mean_age=42.5; female_percent=60")
moderator_long <- meta_moderators_long(metadata_effect)
stopifnot(nrow(moderator_long) == 4L)
stopifnot(sum(moderator_long$type == "categorical") == 2L, sum(moderator_long$type == "continuous") == 2L)
stopifnot(moderator_long$numeric_value[moderator_long$name == "mean_age"] == 42.5)

single_level_moderator <- meta_normalize_effect(list(
  included = FALSE, study_id = "META-LEVEL", family = "g", input_type = "g_se", direction = "positive",
  g = 0.20, se = 0.10, moderator_categorical = "M1"
))
stopifnot(single_level_moderator$status == "valid")
stopifnot(single_level_moderator$moderator_categorical[[1]] == "moderator=M1")
stopifnot(!single_level_moderator$included[[1]])

invalid_moderator <- meta_normalize_effect(list(
  study_id = "META-2", family = "g", input_type = "g_se", direction = "positive",
  g = 0.20, se = 0.10, moderator_continuous = "mean_age=unknown"
))
stopifnot(invalid_moderator$status == "error")
invalid_year <- meta_normalize_effect(list(
  study_id = "META-3", publication_year = 1799, family = "g", input_type = "g_se", direction = "positive",
  g = 0.20, se = 0.10
))
stopifnot(invalid_year$status == "error")
stopifnot(all(c("study_name", "publication_year", "predictor", "moderator_categorical", "moderator_continuous") %in% names(meta_template_data("g"))))
stopifnot(grepl("region=Asia", meta_template_data("g")$moderator_categorical[[1]], fixed = TRUE))

g_d <- meta_normalize_effect(list(
  study_id = "G2", family = "g", input_type = "d", direction = "positive",
  d_value = 0.5, n1 = 30, n0 = 30
))
stopifnot(g_d$status == "valid", g_d$yi > 0, g_d$yi < 0.5, g_d$vi > 0)

g_reverse <- meta_normalize_effect(list(
  study_id = "G3", family = "g", input_type = "g_se", direction = "reverse",
  g = 0.4, se = 0.1
))
stopifnot(close_enough(g_reverse$yi, -0.4), close_enough(g_reverse$vi, 0.01))
stopifnot(g_reverse$direction[[1]] == "negative")
g_negative <- meta_normalize_effect(list(
  study_id = "G4", family = "g", input_type = "g_se", direction = "NEGATIVE",
  g = 0.4, se = 0.1
))
stopifnot(close_enough(g_negative$display_effect, -0.4), g_negative$direction[[1]] == "negative")

r_raw <- meta_normalize_effect(list(
  study_id = "R1", family = "r", input_type = "r", direction = "positive",
  r = 0.3, n = 100
))
stopifnot(r_raw$status == "valid")
stopifnot(close_enough(r_raw$yi, atanh(0.3)))
stopifnot(close_enough(r_raw$vi, 1 / 97))
stopifnot(close_enough(r_raw$display_effect, 0.3))

r_partial <- meta_normalize_effect(list(
  study_id = "R2", family = "r", input_type = "partial_r", direction = "positive",
  r = -0.2, n = 80, k_controls = 4
))
stopifnot(r_partial$status == "valid", close_enough(r_partial$vi, 1 / 73), nzchar(r_partial$assumption))

or_counts <- meta_normalize_effect(list(
  study_id = "O1", family = "or", input_type = "2x2", direction = "positive",
  cell_a = 20, cell_b = 80, cell_c = 10, cell_d = 90
))
stopifnot(or_counts$status == "valid")
stopifnot(close_enough(or_counts$yi, log(2.25)))
stopifnot(close_enough(or_counts$vi, 1 / 20 + 1 / 80 + 1 / 10 + 1 / 90))
stopifnot(close_enough(or_counts$display_effect, 2.25))

or_zero <- meta_normalize_effect(list(
  study_id = "O2", family = "or", input_type = "2x2", direction = "positive",
  cell_a = 0, cell_b = 20, cell_c = 5, cell_d = 25
))
stopifnot(or_zero$status == "warning", grepl("0.5", or_zero$assumption, fixed = TRUE))

or_reverse <- meta_normalize_effect(list(
  study_id = "O3", family = "or", input_type = "or_ci", direction = "reverse",
  or_value = 2, ci_lower = 1.2, ci_upper = 3.5
))
stopifnot(or_reverse$status == "valid", close_enough(or_reverse$display_effect, 0.5))
or_negative <- meta_normalize_effect(list(
  study_id = "O4", family = "or", input_type = "or_ci", direction = "NEGATIVE",
  or_value = 2, ci_lower = 1.2, ci_upper = 3.5
))
stopifnot(or_negative$status == "valid", close_enough(or_negative$display_effect, 0.5))

invalid <- meta_normalize_effect(list(
  study_id = "", family = "r", input_type = "r", direction = "positive",
  r = 1.2, n = 10
))
stopifnot(invalid$status == "error", grepl("Study ID", invalid$message, fixed = TRUE))

import_data <- data.frame(
  study_id = c("I1", "I2"), outcome = c("A", "B"),
  input_type = c("r", "r"), direction = c("positive", "reverse"),
  r = c(0.1, 0.2), n = c(40, 50), stringsAsFactors = FALSE
)
imported <- meta_import_effects(import_data, "r", start_id = 10L)
stopifnot(nrow(imported) == 2L, identical(imported$row_id, 10:11))
stopifnot(all(imported$status == "valid"), imported$yi[[1]] > 0, imported$yi[[2]] < 0)

dynamic_import <- data.frame(
  included = c("TRUE", "FALSE"), study_id = c("D1", "D2"), input_type = c("means", "g_se"),
  direction = c("POSITIVE", "NEGATIVE"), value_1 = c("12", "0.4"), value_2 = c("3", "0.1"),
  value_3 = c("40", ""), value_4 = c("10", ""), value_5 = c("3", ""), value_6 = c("40", ""),
  stringsAsFactors = FALSE, check.names = FALSE
)
dynamic_effects <- meta_import_effects(dynamic_import, "g")
stopifnot(nrow(dynamic_effects) == 2L, all(dynamic_effects$status == "valid"))
stopifnot(dynamic_effects$included[[1]], !dynamic_effects$included[[2]])
stopifnot(dynamic_effects$display_effect[[1]] > 0, close_enough(dynamic_effects$display_effect[[2]], -0.4))

export_source <- rbind(g_means, metadata_effect)
exported <- meta_export_effects(export_source, "g")
stopifnot(identical(names(exported), meta_template_columns("g")))
stopifnot(nrow(exported) == 2L)
stopifnot(identical(exported$study_id, c("G1", "META-1")))
stopifnot(exported$study_name[[2]] == "Kim et al.")
stopifnot(exported$moderator_categorical[[2]] == "region=Asia; design=RCT")
stopifnot(!any(c("yi", "vi", "status", "row_id") %in% names(exported)))
roundtrip <- meta_import_effects(exported, "g")
stopifnot(nrow(roundtrip) == 2L, all(roundtrip$status == "valid"))
stopifnot(close_enough(roundtrip$yi, export_source$yi))
stopifnot(close_enough(roundtrip$vi, export_source$vi))
stopifnot(nrow(meta_export_effects(export_source, "or")) == 0L)

for (family in c("g", "r", "or")) {
  template <- meta_template_data(family)
  template_effects <- meta_import_effects(template, family)
  stopifnot(nrow(template) == length(meta_input_types(family)))
  stopifnot(all(template_effects$included), all(template_effects$status %in% c("valid", "warning")))
}

summary <- meta_effect_summary(rbind(r_raw, r_partial), "r")
stopifnot(summary$total == 2L, summary$valid == 2L, summary$errors == 0L)

revalidation_source <- rbind(g_means, metadata_effect, invalid_year)
revalidated <- meta_revalidate_effects(revalidation_source, "g")
stopifnot(nrow(revalidated) == 3L)
stopifnot(identical(revalidated$row_id, revalidation_source$row_id))
stopifnot(sum(revalidated$status == "valid") == 2L, sum(revalidated$status == "error") == 1L)

message("Meta-analysis effect input validation passed.")
