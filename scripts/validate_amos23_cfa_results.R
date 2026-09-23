source(file.path("scripts", "generate_amos_external_benchmark.R"), encoding = "UTF-8")
source(file.path("scripts", "compare_amos_external.R"), encoding = "UTF-8")

validation_dir <- tempfile("statedu-amos23-results-")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(validation_dir, recursive = TRUE, force = TRUE), add = TRUE)

benchmark <- generate_amos_external_benchmark(validation_dir)
amos_path <- file.path("sample", "amos23_holzinger_cfa_results.csv")
comparison <- compare_amos_external(
  benchmark$statedu_path, amos_path,
  absolute_tolerance = .0005, relative_tolerance = 1e-6
)

chi_row <- comparison$Section == "fit" & comparison$Item == "Chi-square"
stopifnot(
  sum(comparison$Status == "PASS") == 29L,
  sum(comparison$Status == "FAIL") == 1L,
  comparison$Status[chi_row] == "FAIL"
)

parameter_rows <- comparison$Section != "fit"
srmr_row <- comparison$Section == "fit" & comparison$Item == "SRMR"
stopifnot(
  max(comparison$AbsoluteError[parameter_rows]) < 1e-6,
  comparison$AbsoluteError[srmr_row] < 1e-8
)

n <- 301
statedu_chi <- comparison$StatEdu[chi_row]
amos_chi <- comparison$AMOS[chi_row]
amos_convention_chi <- statedu_chi * (n - 1) / n
stopifnot(abs(amos_convention_chi - amos_chi) < 1e-6)

fit_non_chi <- comparison$Section == "fit" & !chi_row
stopifnot(all(round(comparison$StatEdu[fit_non_chi], 3L) == round(comparison$AMOS[fit_non_chi], 3L)))

blocks <- list(
  visual = c("x1", "x2", "x3"), textual = c("x4", "x5", "x6"),
  speed = c("x7", "x8", "x9")
)
analysis_data <- utils::read.csv(
  file.path("sample", "HolzingerSwineford1939.csv"),
  check.names = FALSE, stringsAsFactors = FALSE
)[, unlist(blocks, use.names = FALSE), drop = FALSE]
snapshot <- amos_benchmark_snapshot(blocks)
normal_fit <- run_structural_canvas_analysis(
  snapshot, analysis_data, "cfa", estimator = "ML", missing = "listwise"
)
wishart_fit <- run_structural_canvas_analysis(
  snapshot, analysis_data, "cfa", estimator = "ML", missing = "listwise",
  ml_likelihood = "wishart"
)
stopifnot(
  identical(normal_fit$ml_likelihood, "normal"),
  identical(wishart_fit$ml_likelihood, "wishart"),
  identical(tolower(as.character(lavaan::lavInspect(normal_fit$fit, "options")$likelihood)), "normal"),
  identical(tolower(as.character(lavaan::lavInspect(wishart_fit$fit, "options")$likelihood)), "wishart"),
  inherits(try(run_structural_canvas_analysis(
    snapshot, analysis_data, "cfa", estimator = "MLR", missing = "listwise",
    ml_likelihood = "wishart"
  ), silent = TRUE), "try-error")
)

wishart_values <- amos_benchmark_results(wishart_fit$fit, names(blocks))
amos_values <- read_amos_comparison_values(amos_path, "AMOS")
wishart_values$Key <- paste(wishart_values$Section, wishart_values$Item, sep = "::")
amos_values <- amos_values[match(wishart_values$Key, amos_values$Key), , drop = FALSE]
wishart_error <- abs(wishart_values$Value - amos_values$Value)
stopifnot(
  max(wishart_error) < 1.1e-6,
  wishart_error[wishart_values$Section == "fit" & wishart_values$Item == "Chi-square"] < 2e-9
)

options_source <- paste(readLines(file.path("R", "setup_custom_model_canvas_structural_options.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
stopifnot(
  grepl("_ml_likelihood", options_source, fixed = TRUE),
  grepl("Wishart ML (AMOS/LISREL/EQS", options_source, fixed = TRUE)
)

cat(
  "AMOS 23 CFA external results validated: Normal ML reproduces the documented N versus N-1 difference; ",
  "Wishart ML matches all 30 AMOS values with maximum absolute error below 1.1e-6.\n",
  sep = ""
)
