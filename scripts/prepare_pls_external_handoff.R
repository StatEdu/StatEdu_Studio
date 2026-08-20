source(file.path("scripts", "generate_pls_external_benchmark.R"), encoding = "UTF-8")

pls_handoff_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

pls_handoff_arguments <- function(arguments) {
  values <- list(output_dir = file.path("outputs", "pls_external_handoff"))
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) == 2L) values[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <- pair[[2L]]
  }
  values
}

prepare_pls_external_handoff <- function(output_dir) {
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  benchmark <- generate_pls_external_benchmark(output_dir)

  data_source <- file.path("sample", "HolzingerSwineford1939.csv")
  model_source <- file.path("sample", "pls_external_benchmark.stmodel")
  data_copy <- file.path(output_dir, "HolzingerSwineford1939.csv")
  model_copy <- file.path(output_dir, "pls_external_benchmark.stmodel")
  external_fit <- file.path(output_dir, "external_fit.csv")
  file.copy(data_source, data_copy, overwrite = TRUE)
  file.copy(model_source, model_copy, overwrite = TRUE)
  file.copy(benchmark$template_path, external_fit, overwrite = TRUE)

  measurement <- data.frame(
    Construct = rep(c("visual", "textual", "speed"), each = 3L),
    Indicator = paste0("x", 1:9),
    Measurement = "reflective",
    Ontology = "common factor",
    Weighting = "Mode A",
    stringsAsFactors = FALSE
  )
  paths <- data.frame(
    From = c("visual", "visual", "textual"),
    To = c("textual", "speed", "speed"),
    stringsAsFactors = FALSE
  )
  measurement_path <- file.path(output_dir, "measurement_model.csv")
  paths_path <- file.path(output_dir, "structural_paths.csv")
  utils::write.csv(measurement, measurement_path, row.names = FALSE, na = "")
  utils::write.csv(paths, paths_path, row.names = FALSE, na = "")

  run_record <- list(
    schema_version = "1.0",
    software = "",
    version = "",
    run_date = "",
    fit_target = "saturated",
    weighting_scheme = "path",
    standardized_results = FALSE,
    converged_before_300 = FALSE,
    reported_decimal_places = 0L,
    absolute_tolerance = 1e-6,
    relative_tolerance = 1e-4,
    data_file = gsub("\\\\", "/", data_source),
    data_sha256 = pls_handoff_sha256(data_source),
    model_file = gsub("\\\\", "/", model_source),
    model_sha256 = pls_handoff_sha256(model_source),
    statedu_fit_sha256 = pls_handoff_sha256(benchmark$statedu_path),
    external_fit_sha256 = "",
    comparison_sha256 = ""
  )
  run_record_path <- file.path(output_dir, "external_run.json")
  jsonlite::write_json(run_record, run_record_path, auto_unbox = TRUE, pretty = TRUE)

  instructions <- c(
    "# SmartPLS/ADANCO external validation handoff",
    "",
    "1. Import `HolzingerSwineford1939.csv` and use x1-x9 only.",
    "2. Recreate the reflective common-factor blocks in `measurement_model.csv` and paths in `structural_paths.csv`.",
    "3. Use path weighting, standardized results, +1 initial outer weights, and stop criterion 1e-7.",
    "4. Run PLS and PLSc separately; record saturated-model SRMR, d_G, and d_ULS at maximum available precision.",
    "5. Confirm convergence occurred before 300 iterations. Do not substitute estimated-model fit.",
    "6. Enter the six values in `external_fit.csv` without changing Model or Fit and record the actual displayed/exported decimal places.",
    "7. Finalize with `scripts/finalize_pls_external_evidence.R` and the exact software name/version/run date/decimal places.",
    "",
    "No SmartPLS/ADANCO equivalence claim is permitted until finalization passes."
  )
  instructions_path <- file.path(output_dir, "RUN_EXTERNAL.md")
  writeLines(instructions, instructions_path, useBytes = TRUE)

  invisible(list(
    output_dir = output_dir, statedu_path = benchmark$statedu_path, external_path = external_fit,
    manifest_path = benchmark$manifest_path, run_record_path = run_record_path,
    measurement_path = measurement_path, paths_path = paths_path, instructions_path = instructions_path
  ))
}

if (sys.nframe() == 0L) {
  arguments <- pls_handoff_arguments(commandArgs(trailingOnly = TRUE))
  result <- prepare_pls_external_handoff(arguments$output_dir)
  cat("PLS external-validation handoff prepared: ", result$output_dir, "\n", sep = "")
  cat("Enter SmartPLS/ADANCO values in: ", result$external_path, "\n", sep = "")
}
