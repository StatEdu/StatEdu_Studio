options(stringsAsFactors = FALSE)

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, tolerance = 1e-10, message = "Values differ.") {
  if (is.numeric(actual) || is.numeric(expected)) {
    ok <- length(actual) == length(expected) &&
      all(is.na(actual) == is.na(expected)) &&
      all(abs(actual[!is.na(actual)] - expected[!is.na(expected)]) <= tolerance)
  } else {
    ok <- identical(actual, expected)
  }
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"), error = function(e) "")
repo_root <- if (nzchar(script_path)) dirname(dirname(script_path)) else normalizePath(".", winslash = "/")
core_path <- file.path(repo_root, "modules", "latent_mplus", "app", "R", "common", "14_profile_core.R")
source(core_path, local = TRUE)

synthetic_output <- c(
  "Mplus VERSION 8.11",
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "MODEL RESULTS",
  "Latent Class 1",
  "Means",
  "Y1  1.000D+00  2.000D-01  5.000  0.000",
  "Y2  2.000       0.300       6.667  0.000",
  "Variances",
  "Y1  1.100 0.100",
  "Latent Class 2",
  "Means",
  "Y1  3.000       0.400       7.500  0.000",
  "Y2  4.000D+00  5.000D-01  8.000  0.000",
  "Variances",
  "Y1  1.200 0.100",
  "QUALITY OF NUMERICAL RESULTS"
)

parsed <- profile_parse_mplus_means(
  lines = synthetic_output,
  indicators = c("Y1", "Y2"),
  model_tag = "retained_model",
  expected_k = 2L
)
assert_true(parsed$available, parsed$reason)
assert_equal(nrow(parsed$data), 4L, message = "The retained profile grid is not rectangular.")
assert_equal(parsed$data$Mean, c(1, 2, 3, 4), message = "Mplus means were parsed incorrectly.")
assert_equal(parsed$data$SE, c(.2, .3, .4, .5), message = "Mplus standard errors were parsed incorrectly.")
critical <- qnorm(.975)
assert_equal(parsed$data$LLCI, parsed$data$Mean - critical * parsed$data$SE, message = "Lower Wald limits are incorrect.")
assert_equal(parsed$data$ULCI, parsed$data$Mean + critical * parsed$data$SE, message = "Upper Wald limits are incorrect.")
assert_true(all(parsed$data$source_type == "retained_mplus_model"), "Model-estimate provenance is missing.")
assert_true(all(parsed$data$spread_type == "SE"), "Model spread type must always be SE.")
assert_true(!"SD" %in% names(parsed$data), "The model-estimate parser must not emit SD.")

incomplete <- synthetic_output[!grepl("^Y2  4", synthetic_output)]
parsed_incomplete <- profile_parse_mplus_means(
  lines = incomplete,
  indicators = c("Y1", "Y2"),
  model_tag = "retained_model",
  expected_k = 2L
)
assert_true(!parsed_incomplete$available && nrow(parsed_incomplete$data) == 0L,
            "An incomplete retained profile must fail closed.")
assert_true(nzchar(parsed_incomplete$reason), "Fail-closed parsing must provide a reason.")

zero_se <- sub("Y2  4.000D\\+00  5.000D-01", "Y2  4.000D+00  0.000D+00", synthetic_output, fixed = FALSE)
parsed_zero_se <- profile_parse_mplus_means(
  lines = zero_se,
  indicators = c("Y1", "Y2"),
  model_tag = "retained_model",
  expected_k = 2L
)
assert_true(!parsed_zero_se$available, "A non-positive model SE must fail closed.")

abnormal <- synthetic_output[synthetic_output != "THE MODEL ESTIMATION TERMINATED NORMALLY"]
parsed_abnormal <- profile_parse_mplus_means(
  lines = abnormal,
  indicators = c("Y1", "Y2"),
  model_tag = "retained_model",
  expected_k = 2L
)
assert_true(!parsed_abnormal$available, "An abnormally terminated model must fail closed.")

temp_dir <- tempfile("latent-profile-")
dir.create(temp_dir, recursive = TRUE)
on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
retained_path <- file.path(temp_dir, "retained_model.out")
stale_path <- file.path(temp_dir, "stale_model.out")
writeLines(synthetic_output, retained_path)
writeLines(synthetic_output, stale_path)

resolved <- profile_resolve_retained_out_file(
  best_tag = "retained_model",
  best_model_row = data.frame(model_tag = "stale_model", out_file = stale_path, status = "ok", parse_ok = TRUE),
  registry = data.frame(model_tag = "retained_model", out_file = retained_path, status = "ok", parse_ok = TRUE),
  fit_summary = data.frame(),
  search_dirs = temp_dir
)
assert_true(resolved$available, resolved$reason)
assert_equal(normalizePath(resolved$path), normalizePath(retained_path),
             message = "The resolver selected a stale or wrong-tag model.")

wrong_only <- profile_resolve_retained_out_file(
  best_tag = "missing_model",
  best_model_row = data.frame(model_tag = "stale_model", out_file = stale_path, status = "ok", parse_ok = TRUE),
  registry = data.frame(),
  fit_summary = data.frame(),
  search_dirs = character(0)
)
assert_true(!wrong_only$available && nzchar(wrong_only$reason),
            "A wrong-tag output must not be accepted.")

classified <- data.frame(
  class_num = c(1L, 1L, 2L, 2L),
  score = c(1, 3, 10, 14),
  response = c("a", "b", "a", "a"),
  weight = c(1, 3, 1, 1),
  stringsAsFactors = FALSE
)
modal <- profile_modal_descriptives(
  classified = classified,
  continuous = "score",
  categorical = "response"
)
modal_cont <- modal[modal$indicator_type == "continuous", , drop = FALSE]
modal_cat <- modal[modal$indicator_type == "categorical", , drop = FALSE]
assert_equal(modal_cont$Mean, c(2, 12), message = "Modal-class means are incorrect.")
assert_equal(modal_cont$SD, c(sqrt(2), sqrt(8)), message = "Modal-class SDs are incorrect.")
assert_true(all(modal_cont$spread_type == "SD"), "Modal continuous spread must be SD.")
assert_true(!"SE" %in% names(modal), "Modal descriptives must not expose model SE.")
class1_cat <- modal_cat[modal_cat$class_num == 1L, , drop = FALSE]
assert_equal(class1_cat$n, c(1, 1), message = "Modal categorical counts are incorrect.")
assert_equal(class1_cat$percent, c(50, 50), message = "Modal categorical percentages are incorrect.")
assert_true(all(modal$source_type == "modal_class_descriptive"), "Modal provenance is missing.")

weighted_modal <- profile_modal_descriptives(
  classified = classified,
  continuous = "score",
  categorical = "response",
  weight_var = "weight"
)
weighted_cont <- weighted_modal[weighted_modal$indicator_type == "continuous", , drop = FALSE]
weighted_cat <- weighted_modal[weighted_modal$indicator_type == "categorical" & weighted_modal$class_num == 1L, , drop = FALSE]
assert_true(all(weighted_cont$spread_type == "SD"), "Weights must not relabel modal SD as SE.")
assert_equal(weighted_cont$Mean[[1]], 2.5, message = "Weighted modal mean is incorrect.")
assert_equal(weighted_cat$percent, c(25, 75), message = "Weighted modal percentages are incorrect.")
assert_true(all(weighted_cat$spread_type == "weighted_percent"), "Weighted percentage provenance is missing.")

pipeline_text <- paste(readLines(file.path(repo_root, "modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "00_run_pipeline.R"), warn = FALSE), collapse = "\n")
tables_text <- paste(readLines(file.path(repo_root, "modules", "latent_mplus", "app", "R", "cross_sectional_mixture", "05_tables.R"), warn = FALSE), collapse = "\n")
server_text <- paste(readLines(file.path(repo_root, "modules", "latent_mplus", "app", "R", "app_server.R"), warn = FALSE), collapse = "\n")
assert_true(grepl('"14_profile_core.R"', pipeline_text, fixed = TRUE), "The shared profile core is not loaded by the pipeline.")
assert_true(grepl("resolve_retained_model_profile()", tables_text, fixed = TRUE), "T4 is not wired to the retained-model resolver.")
assert_true(grepl("resolve_modal_profile_source(prefer_z = FALSE)", tables_text, fixed = TRUE), "A3 is not wired to the modal resolver.")
assert_true(grepl('type = "twoline_model_ci"', tables_text, fixed = TRUE) ||
              grepl('t4_table_type <- "twoline_model_ci"', tables_text, fixed = TRUE),
            "The T4 model-CI table contract is missing.")
assert_true(grepl('"llci", "ulci"', server_text, fixed = TRUE), "The screen renderer does not recognize model CI headers.")
assert_true(grepl('"m/n", "sd/%"', server_text, fixed = TRUE), "The screen renderer does not recognize mixed modal headers.")

cat("latent profile reporting validation PASS\n")
