`%||%` <- function(x, y) if (is.null(x)) y else x
fmt_p_tbl <- function(p, digits = 3L) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(is.na(p), NA_character_, ifelse(p < .001, "<.001", formatC(p, format = "f", digits = digits)))
}
p_to_sig_tbl <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
}

source(file.path("modules", "latent_mplus", "app", "R", "common", "10_bch_core.R"), local = TRUE)

expect_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}
expect_equal <- function(actual, expected, tolerance = 1e-8, message = "Values differ.") {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(message, "\nActual: ", paste(actual, collapse = ", "), "\nExpected: ", paste(expected, collapse = ", "), call. = FALSE)
  }
}

sample_output <- c(
  "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE",
  "WITH 2 DEGREE(S) OF FREEDOM FOR THE OVERALL TEST",
  "Mean S.E.",
  "Class 1 3.295 0.050 Class 2 4.350 0.090 Class 3 3.585 0.060",
  "Chi-Square P-Value",
  "Overall test 35.475 0.000",
  "Class 1 vs. 2 35.109 0.000",
  "Class 1 vs. 3 8.708 0.003",
  "Class 2 vs. 3 21.490 0.000",
  "TECHNICAL 1 OUTPUT"
)

parsed <- parse_bch_model_results(
  lines = sample_output,
  outcome_name = "identity",
  outcome_meta = data.frame(var_name = "identity", var_label = "Professional identity", outcome_type = "continuous"),
  best_k = 3L,
  best_tag = "qa_model2_k3",
  model_structure = "model2"
)

overall <- parsed[parsed$result_type == "overall", , drop = FALSE]
classes <- parsed[parsed$result_type == "class_estimate", , drop = FALSE]
pairs <- parsed[parsed$result_type == "posthoc", , drop = FALSE]

expect_true(nrow(overall) >= 1L, "BCH overall row was not retained.")
expect_equal(overall$stat[which(!is.na(overall$stat))[1]], 35.475, message = "BCH overall Wald statistic is incorrect.")
expect_equal(overall$df[which(!is.na(overall$df))[1]], 2, message = "BCH overall df is incorrect.")
expect_equal(overall$p[which(!is.na(overall$p))[1]], 0, message = "BCH overall p is incorrect.")
expect_equal(classes$estimate[order(classes$class_num)], c(3.295, 4.350, 3.585), message = "BCH class means are incorrect.")
expect_equal(classes$se[order(classes$class_num)], c(.050, .090, .060), message = "BCH class SEs are incorrect.")
expect_true(nrow(pairs) == 3L, "All BCH pairwise rows must be retained.")
expect_equal(pairs$class1, c(1, 1, 2), message = "Pairwise class1 identifiers are incorrect.")
expect_equal(pairs$class2, c(2, 3, 3), message = "Pairwise class2 identifiers are incorrect.")
expect_true(all(pairs$df == 1), "Every BCH pairwise Wald test must report df = 1.")

posthoc_attr <- attr(parsed, "posthoc")
expect_true(is.data.frame(posthoc_attr) && nrow(posthoc_attr) == 3L, "BCH pairwise attribute was lost.")

tables_text <- paste(readLines(file.path("modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "05_tables.R"), warn = FALSE), collapse = "\n")
bch_text <- paste(readLines(file.path("modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "04d_bch.R"), warn = FALSE), collapse = "\n")

expect_true(grepl("resolve_bch_omnibus_values(outcome_var, df_full, om_basic)", tables_text, fixed = TRUE), "T6 is not using saved full-sample BCH omnibus results.")
expect_true(grepl("normalize_bch_pairwise_rows(df_ph)", tables_text, fixed = TRUE), "T6 is not using saved BCH pairwise rows.")
expect_true(!grepl("moderated_t6 <- build_T6_bch_moderated_summary()", tables_text, fixed = TRUE), "T6 still prefers a modal ANOVA/moderator summary over the full-sample BCH result.")
expect_true(!grepl("parse_bch_overall_from_out", tables_text, fixed = TRUE), "Tables still reparse mutable Mplus .out files instead of saved BCH results.")
expect_true(!grepl("out$SD <- fmt_sd2(sd_vals)", tables_text, fixed = TRUE), "T6 still replaces BCH SE with modal SD.")
expect_true(grepl("R3STEP_RESULTS_RAW$univariable", tables_text, fixed = TRUE), "T5 is not wired to the distinct univariable source.")
expect_true(grepl("BCH pairwise Wald test", tables_text, fixed = TRUE), "The detailed BCH pairwise table is missing.")
expect_true(grepl("Zero cell; separation risk", tables_text, fixed = TRUE), "Sparse-cell diagnostics are missing.")
expect_true(grepl("subset_dataset_id <- paste0(DATASET_ID, \"_\", subset_stub)", bch_text, fixed = TRUE), "Stratified BCH does not use a unique dataset/model tag.")
expect_true(grepl("BCH_DATA_FILE                = subset_data_file", bch_text, fixed = TRUE), "Stratified BCH still shares the full-sample data file.")

cat("Latent BCH reporting validation passed.\n")
