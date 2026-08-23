source(file.path("scripts", "generate_pls_external_benchmark.R"), encoding = "UTF-8")

pls_handoff_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

pls_handoff_text_sha256 <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  if (any(bytes == as.raw(0L))) stop("Public text artifact contains a NUL byte: ", path, call. = FALSE)
  normalized <- gsub("\r\n?", "\n", rawToChar(bytes), perl = TRUE)
  digest::digest(charToRaw(normalized), algo = "sha256", serialize = FALSE)
}

pls_handoff_arguments <- function(arguments) {
  values <- list(
    output_dir = file.path("outputs", "pls_external_handoff"),
    profile = "holzinger-swineford-301"
  )
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) == 2L) values[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <- pair[[2L]]
  }
  values
}

prepare_pls_external_handoff <- function(output_dir, profile = "holzinger-swineford-301") {
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  profile <- pls_external_benchmark_profile(profile)
  benchmark <- generate_pls_external_benchmark(output_dir, profile$id)

  data_source <- file.path("sample", "HolzingerSwineford1939.csv")
  selected_rows <- profile$row_indices
  if (is.null(selected_rows)) {
    selected_rows <- seq_len(nrow(utils::read.csv(data_source, check.names = FALSE, stringsAsFactors = FALSE)))
  }
  model_source <- file.path("sample", "pls_external_benchmark.stmodel")
  data_copy <- benchmark$data_path
  model_copy <- file.path(output_dir, "pls_external_benchmark.stmodel")
  external_fit <- file.path(output_dir, "external_fit.csv")
  comparison_path <- file.path(output_dir, "comparison.csv")
  file.copy(model_source, model_copy, overwrite = TRUE)
  file.copy(benchmark$template_path, external_fit, overwrite = TRUE)
  if (file.exists(comparison_path)) unlink(comparison_path)

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
    schema_version = "1.2",
    public_text_hash_normalization = "CRLF and CR normalized to LF before SHA-256",
    profile = profile$id,
    software = "",
    version = "",
    run_date = "",
    license_edition = "",
    output_provenance = "",
    fit_target = "saturated",
    weighting_scheme = "path",
    standardized_results = FALSE,
    converged_before_300 = FALSE,
    reported_decimal_places = 0L,
    absolute_tolerance = 1e-6,
    relative_tolerance = 1e-4,
    data_file = basename(data_copy),
    data_sha256 = pls_handoff_text_sha256(data_copy),
    source_data_file = gsub("\\\\", "/", data_source),
    source_data_sha256 = pls_handoff_text_sha256(data_source),
    row_selection = list(
      index_basis = "1-based data rows excluding the header",
      start = min(selected_rows), end = max(selected_rows), count = length(selected_rows),
      indicators = paste0("x", 1:9), order = "source-file order preserved"
    ),
    model_file = basename(model_copy),
    model_sha256 = pls_handoff_text_sha256(model_copy),
    settings_contract = list(
      weighting_scheme = "path", standardized_results = TRUE,
      initial_outer_weights = 1.0, stop_criterion = 1e-7,
      external_maximum_iterations = 3000L, fit_target = "saturated"
    ),
    artifact_contract_required = identical(profile$id, "holzinger-swineford-first100-smartpls-student"),
    execution_artifacts = list(
      imported_data_file = "", imported_data_bytes = 0L, imported_data_sha256 = "",
      imported_data_settings_file = "", imported_data_settings_bytes = 0L, imported_data_settings_sha256 = "",
      executed_model_file = "", executed_model_bytes = 0L, executed_model_sha256 = "",
      algorithm_settings_file = "", algorithm_settings_bytes = 0L, algorithm_settings_sha256 = "",
      data_confirmation_file = "", data_confirmation_bytes = 0L, data_confirmation_sha256 = "",
      model_canvas_file = "", model_canvas_bytes = 0L, model_canvas_sha256 = "",
      settings_confirmation_file = "", settings_confirmation_bytes = 0L, settings_confirmation_sha256 = "",
      export_lock_file = "", export_lock_bytes = 0L, export_lock_sha256 = ""
    ),
    runs = list(
      pls = list(
        converged = FALSE, iterations = 0L, fit_evidence_file = "", fit_evidence_bytes = 0L, fit_evidence_sha256 = "",
        convergence_evidence_file = "", convergence_evidence_bytes = 0L, convergence_evidence_sha256 = "",
        execution_log_file = "", execution_log_bytes = 0L, execution_log_sha256 = ""
      ),
      plsc = list(
        converged = FALSE, iterations = 0L, fit_evidence_file = "", fit_evidence_bytes = 0L, fit_evidence_sha256 = "",
        convergence_evidence_file = "", convergence_evidence_bytes = 0L, convergence_evidence_sha256 = "",
        execution_log_file = "", execution_log_bytes = 0L, execution_log_sha256 = ""
      )
    ),
    finalization_status = "pending",
    statedu_fit_sha256 = pls_handoff_text_sha256(benchmark$statedu_path),
    external_fit_sha256 = "",
    comparison_sha256 = ""
  )
  run_record_path <- file.path(output_dir, "external_run.json")
  jsonlite::write_json(run_record, run_record_path, auto_unbox = TRUE, pretty = TRUE)

  instructions <- c(
    "# SmartPLS/ADANCO external validation handoff",
    "",
    paste0("Profile: `", profile$id, "`."),
    paste0("1. Import `", basename(data_copy), "`; use the included rows in their existing order and x1-x9 only."),
    "2. Recreate the reflective common-factor blocks in `measurement_model.csv` and paths in `structural_paths.csv`.",
    "3. Use path weighting, standardized results, +1 initial outer weights, and stop criterion 1e-7.",
    "4. Run PLS and PLSc separately; record saturated-model SRMR, d_G, and d_ULS at maximum available precision.",
    "5. Confirm convergence occurred before 300 iterations. Do not substitute estimated-model fit.",
    "6. Enter the six values in `external_fit.csv` without changing Model or Fit and record whether they were exported or displayed, plus the actual decimal places.",
    "7. For the SmartPLS Student first-100 profile, retain vendor UI/project/settings artifacts outside the repository under STATEDU_SMARTPLS_EVIDENCE_ROOT; record only their basename, byte length, and SHA-256 in `external_run.json`.",
    "8. Finalize with `scripts/finalize_pls_external_evidence.R` and the exact software name/version/run date/decimal places.",
    "",
    "No SmartPLS/ADANCO equivalence claim is permitted until finalization passes."
  )
  instructions_path <- file.path(output_dir, "RUN_EXTERNAL.md")
  writeLines(instructions, instructions_path, useBytes = TRUE)

  invisible(list(
    profile = profile$id, output_dir = output_dir, data_path = data_copy,
    statedu_path = benchmark$statedu_path, external_path = external_fit,
    manifest_path = benchmark$manifest_path, run_record_path = run_record_path,
    measurement_path = measurement_path, paths_path = paths_path, instructions_path = instructions_path
  ))
}

if (sys.nframe() == 0L) {
  arguments <- pls_handoff_arguments(commandArgs(trailingOnly = TRUE))
  result <- prepare_pls_external_handoff(arguments$output_dir, arguments$profile)
  cat("PLS external-validation handoff prepared: ", result$output_dir, "\n", sep = "")
  cat("Enter SmartPLS/ADANCO values in: ", result$external_path, "\n", sep = "")
}
