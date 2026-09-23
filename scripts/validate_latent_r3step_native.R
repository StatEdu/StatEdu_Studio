options(stringsAsFactors = FALSE)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
core_file <- file.path(
  root,
  "modules", "latent_mplus", "app", "R", "common", "13_r3step_core.R"
)
source(core_file, local = .GlobalEnv)

expect_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

expect_equal <- function(actual, expected, tolerance = 1e-10, message = NULL) {
  ok <- if (is.numeric(actual) && is.numeric(expected)) {
    length(actual) == length(expected) &&
      all(is.na(actual) == is.na(expected)) &&
      all(abs(actual[!is.na(actual)] - expected[!is.na(expected)]) <= tolerance)
  } else {
    identical(actual, expected)
  }
  if (!ok) stop(message %||% "Values are not equal.", call. = FALSE)
}

collect_statement <- function(lines, keyword) {
  start <- grep(paste0("^\\s*", keyword, "\\s*="), lines, ignore.case = TRUE)
  if (length(start) != 1L) return(character(0))
  out <- character(0)
  for (i in seq.int(start, length(lines))) {
    out <- c(out, trimws(lines[i]))
    if (grepl(";\\s*$", lines[i])) break
  }
  paste(out, collapse = " ")
}

spec <- data.frame(
  predictor = c("age__scaled", "sex__female"),
  source_var = c("age", "sex"),
  var_name = c("age", "sex"),
  var_label = c("Age", "Sex"),
  level = c(NA, "2"),
  value_label = c(NA, "Female"),
  predictor_type = c("continuous", "binary_dummy"),
  reference_level = c(NA, "Male"),
  stringsAsFactors = FALSE
)
spec <- r3step_make_alias_map(spec, reserved = c("Y1", "Y2", "R3X0001"))

expect_true(all(nchar(spec$mplus_alias) <= 8L), "Mplus aliases exceeded 8 characters.")
expect_true(!anyDuplicated(toupper(spec$mplus_alias)), "Mplus aliases were not unique.")
expect_true(
  all(grepl("^[A-Za-z][A-Za-z0-9]*$", spec$mplus_alias)),
  "Mplus aliases contained invalid characters."
)
expect_true(!any(toupper(spec$mplus_alias) %in% c("Y1", "Y2", "R3X0001")), "Alias collision was not avoided.")

input_lines <- r3step_build_input_lines(
  title = "validator",
  data_file = "C:/tmp/r3step.dat",
  data_names = c("Y1", "Y2", spec$mplus_alias, "WGT"),
  indicators = c("Y1", "Y2"),
  predictor_aliases = spec$mplus_alias,
  categorical = "Y2",
  best_k = 3L,
  mixture_type = "mixed",
  model_structure = "model3",
  indicators_continuous = "Y1",
  indicators_categorical = "Y2",
  starts = "500 100",
  stiterations = 20L,
  processors = 4L,
  lrtstarts = "0 0 200 40",
  weight_var = "WGT"
)

use_statement <- collect_statement(input_lines, "USEVARIABLES")
aux_statement <- collect_statement(input_lines, "AUXILIARY")
expect_true(length(aux_statement) == 1L, "A single joint AUXILIARY statement was not generated.")
expect_true(grepl("\\(R3STEP\\)", aux_statement), "AUXILIARY did not request R3STEP.")
expect_true(all(vapply(spec$mplus_alias, grepl, logical(1), x = aux_statement, fixed = TRUE)), "Not all aliases were entered jointly.")
expect_true(!any(vapply(spec$mplus_alias, grepl, logical(1), x = use_statement, fixed = TRUE)), "Auxiliary predictors leaked into USEVARIABLES.")
expect_true(all(vapply(c("Y1", "Y2"), grepl, logical(1), x = use_statement, fixed = TRUE)), "Indicators were missing from USEVARIABLES.")
expect_true(any(grepl("STARTS = 500 100;", input_lines, fixed = TRUE)), "STARTS pair was not preserved.")
expect_true(any(grepl("TYPE = MIXTURE;", input_lines, fixed = TRUE)), "Weight-only analysis was incorrectly declared COMPLEX.")
expect_true(!any(grepl("MIXTURE COMPLEX", input_lines, fixed = TRUE)), "Weight-only analysis used MIXTURE COMPLEX.")
expect_true(any(grepl("CLASSES = c(3);", input_lines, fixed = TRUE)), "Class count was not written.")
expect_true(any(grepl("Y1 WITH", input_lines, fixed = TRUE)) == FALSE, "A one-indicator model created an invalid covariance.")

cluster_lines <- r3step_build_input_lines(
  title = "cluster validator",
  data_file = "C:/tmp/r3step.dat",
  data_names = c("Y1", "Y2", spec$mplus_alias, "CLU"),
  indicators = c("Y1", "Y2"),
  predictor_aliases = spec$mplus_alias,
  categorical = character(0),
  best_k = 3L,
  mixture_type = "lpa",
  model_structure = "model2",
  indicators_continuous = c("Y1", "Y2"),
  cluster_var = "CLU"
)
expect_true(any(grepl("TYPE = MIXTURE COMPLEX;", cluster_lines, fixed = TRUE)), "Cluster design did not request MIXTURE COMPLEX.")

spec$mplus_alias <- c("R3X0001", "R3X0002")
r3_lines <- c(
  "Mplus VERSION 8.11",
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS USING",
  "THE 3-STEP PROCEDURE", "",
  "                                                     Two-Tailed",
  "                     Estimate       S.E.  Est./S.E.    P-Value", "",
  " C#1       ON",
  "    R3X0001       4.880D-01  1.900D-01  2.568D+00  1.020D-02",
  "    R3X0002      -2.500D-01  1.250D-01 -2.000D+00  4.550D-02", "",
  " C#2       ON",
  "    R3X0001      -3.100D-01  1.000D-01 -3.100D+00  1.900D-03",
  "    R3X0002       1.200D-01  6.000D-02  2.000D+00  4.550D-02", "",
  " Intercepts", "    C#1 1.0 0.2 5.0 0.0", "",
  " Parameterization using Reference Class 1", "",
  " C#2       ON",
  "    R3X0001      -7.980D-01  2.250D-01 -3.547D+00  4.000D-04",
  "    R3X0002       3.700D-01  1.400D-01  2.643D+00  8.200D-03", "",
  " C#3       ON",
  "    R3X0001      -4.880D-01  1.900D-01 -2.568D+00  1.020D-02",
  "    R3X0002       2.500D-01  1.250D-01  2.000D+00  4.550D-02", "",
  " Parameterization using Reference Class 2", "",
  " C#1       ON",
  "    R3X0001       7.980d-01  2.250d-01  3.547d+00  4.000d-04",
  "    R3X0002      -3.700D-01  1.400D-01 -2.643D+00  8.200D-03", "",
  " C#3       ON",
  "    R3X0001       3.100D-01  1.000D-01  3.100D+00  1.900D-03",
  "    R3X0002      -1.200D-01  6.000D-02 -2.000D+00  4.550D-02", "",
  "ODDS RATIOS FOR TESTS OF CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS",
  "USING THE 3-STEP PROCEDURE", " C#1 ON", " R3X0001 1.629 0.310 1.122 2.364"
)

parsed3 <- r3step_parse_native_output(
  r3_lines,
  spec = spec,
  best_k = 3L,
  best_tag = "best",
  model_structure = "model2",
  reference_class = 3L
)
expect_true(parsed3$inference_available, parsed3$reason)
expect_equal(nrow(parsed3$table), 4L, message = "Default-reference R3STEP row count was wrong.")
expect_true(all(parsed3$table$reference_class == "Class 3"), "Reference Class 3 was not retained.")
expect_equal(parsed3$table$estimate[1], 0.488, tolerance = 1e-12)
expect_equal(parsed3$table$se[1], 0.190, tolerance = 1e-12)
expect_equal(parsed3$table$rrr[1], exp(0.488), tolerance = 1e-12)
expect_true(all(parsed3$table$analysis == "multivariable"), "Native joint results were not marked multivariable.")
expect_true(all(parsed3$table$model_type == "mplus_native_r3step"), "Native model type was not retained.")

parsed1 <- r3step_parse_native_output(
  r3_lines,
  spec = spec,
  best_k = 3L,
  reference_class = 1L
)
expect_true(parsed1$inference_available, parsed1$reason)
expect_equal(nrow(parsed1$table), 4L)
expect_true(all(parsed1$table$reference_class == "Class 1"), "Explicit alternative reference class was not selected.")
expect_equal(parsed1$table$estimate[1], -0.798, tolerance = 1e-12)

bad_missing <- r3_lines[-which(grepl("R3X0002       1.200D-01", r3_lines, fixed = TRUE))]
bad_result <- r3step_parse_native_output(bad_missing, spec, best_k = 3L, reference_class = 3L)
expect_true(!bad_result$inference_available && nrow(bad_result$table) == 0L, "Incomplete output was not rejected fail-closed.")

bad_se <- sub("1.900D-01", "0.000D+00", r3_lines, fixed = TRUE)
bad_result <- r3step_parse_native_output(bad_se, spec, best_k = 3L, reference_class = 3L)
expect_true(!bad_result$inference_available && nrow(bad_result$table) == 0L, "Zero SE was not rejected fail-closed.")

fatal_result <- r3step_parse_native_output(c(r3_lines, "*** ERROR in MODEL command"), spec, best_k = 3L)
expect_true(!fatal_result$inference_available && nrow(fatal_result$table) == 0L, "Mplus fatal error was not rejected.")

generic_result <- r3step_parse_native_output(
  c("THE MODEL ESTIMATION TERMINATED NORMALLY", "MODEL RESULTS", "R3X0001 0.5 0.2 2.5 0.01"),
  spec,
  best_k = 3L
)
expect_true(!generic_result$inference_available && nrow(generic_result$table) == 0L, "Generic model output was misread as R3STEP.")

pipeline_text <- readLines(file.path(
  root, "modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "00_run_pipeline.R"
), warn = FALSE)
r3step_text <- readLines(file.path(
  root, "modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "04c_r3step.R"
), warn = FALSE)
tables_text <- readLines(file.path(
  root, "modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "05_tables.R"
), warn = FALSE)
expect_true(any(grepl("13_r3step_core.R", pipeline_text, fixed = TRUE)), "R3STEP core was not loaded by the pipeline.")
expect_true(any(grepl("r3step_parse_native_output", r3step_text, fixed = TRUE)), "Native parser was not integrated.")
expect_true(!any(grepl("nnet::multinom|manual_3step_r_multinom|Manual 3-step multinomial", r3step_text)), "A manual/modal-class inferential fallback remains active.")
expect_true(!any(grepl("^\\s*df\\$reference_class\\s*<-\\s*ref_class\\s*$", tables_text)), "Table code still overwrites the native reference class.")

cat("native R3STEP validation PASS\n")
