source(file.path("scripts", "compare_pls_fit_external.R"), encoding = "UTF-8")
source(file.path("scripts", "generate_pls_external_benchmark.R"), encoding = "UTF-8")

pls_evidence_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

pls_evidence_arguments <- function(arguments) {
  values <- list(
    evidence_dir = file.path("outputs", "pls_external_handoff"),
    private_evidence_dir = "",
    software = "", software_version = "", run_date = "",
    converged_before_300 = "false", reported_decimal_places = "",
    absolute_tolerance = "1e-6", relative_tolerance = "1e-4"
  )
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) == 2L) values[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <- pair[[2L]]
  }
  values
}

pls_evidence_resolve_file <- function(evidence_dir, path, label) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) stop(label, " file is required.", call. = FALSE)
  candidates <- unique(c(file.path(evidence_dir, path), path))
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) stop(label, " file was not found: ", path, call. = FALSE)
  normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
}

pls_evidence_require_recorded_hash <- function(path, recorded, label) {
  recorded <- tolower(trimws(as.character(recorded %||% "")))
  if (!grepl("^[0-9a-f]{64}$", recorded)) stop(label, " SHA-256 is missing or invalid.", call. = FALSE)
  actual <- tolower(pls_evidence_sha256(path))
  if (!identical(actual, recorded)) stop(label, " SHA-256 does not match the recorded value.", call. = FALSE)
  invisible(actual)
}

pls_evidence_resolve_private_file <- function(private_evidence_dir, path, label) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) stop(label, " file is required.", call. = FALSE)
  if (grepl("^(?:[A-Za-z]:|[\\\\/])", path, perl = TRUE) ||
      any(strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1L]] == "..")) {
    stop(label, " must use a safe relative path below the private evidence root.", call. = FALSE)
  }
  private_evidence_dir <- normalizePath(private_evidence_dir, winslash = "/", mustWork = TRUE)
  candidate <- normalizePath(file.path(private_evidence_dir, path), winslash = "/", mustWork = TRUE)
  prefix <- paste0(sub("/+$", "", private_evidence_dir), "/")
  if (!startsWith(tolower(candidate), tolower(prefix))) {
    stop(label, " resolves outside the private evidence root.", call. = FALSE)
  }
  candidate
}

pls_evidence_require_recorded_artifact <- function(path, recorded_hash, recorded_bytes, label) {
  bytes <- suppressWarnings(as.numeric(recorded_bytes))
  if (length(bytes) != 1L || !is.finite(bytes) || bytes <= 0 || bytes != file.info(path)$size) {
    stop(label, " byte length does not match the recorded value.", call. = FALSE)
  }
  pls_evidence_require_recorded_hash(path, recorded_hash, label)
  invisible(TRUE)
}

pls_evidence_normalized_text_sha256 <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  if (any(bytes == as.raw(0L))) stop("Public text artifact contains a NUL byte: ", path, call. = FALSE)
  normalized <- gsub("\r\n?", "\n", rawToChar(bytes), perl = TRUE)
  digest::digest(charToRaw(normalized), algo = "sha256", serialize = FALSE)
}

pls_evidence_require_recorded_text_hash <- function(path, recorded, label) {
  recorded <- tolower(trimws(as.character(recorded %||% "")))
  if (!grepl("^[0-9a-f]{64}$", recorded)) stop(label, " SHA-256 is missing or invalid.", call. = FALSE)
  actual <- tolower(pls_evidence_normalized_text_sha256(path))
  if (!identical(actual, recorded)) stop(label, " normalized-text SHA-256 does not match the recorded value.", call. = FALSE)
  invisible(actual)
}

pls_evidence_read_semicolon_data <- function(path, label) {
  value <- try(utils::read.table(
    path, header = TRUE, sep = ";", check.names = FALSE, stringsAsFactors = FALSE,
    quote = "", comment.char = ""
  ), silent = TRUE)
  if (inherits(value, "try-error")) stop(label, " could not be parsed as semicolon-delimited data.", call. = FALSE)
  value
}

pls_evidence_validate_student100_artifacts <- function(evidence_dir, run_record, private_evidence_dir = evidence_dir) {
  if (!identical(tolower(trimws(as.character(run_record$software))), "smartpls")) {
    stop("The SmartPLS Student first-100 profile requires software=SmartPLS.", call. = FALSE)
  }
  if (!identical(tolower(trimws(as.character(run_record$license_edition %||% ""))), "student") ||
      !identical(trimws(as.character(run_record$license_description %||% "")), "Student license (free limited, non-Professional)")) {
    stop("The SmartPLS first-100 profile requires the retained Student-license description.", call. = FALSE)
  }
  if (!identical(tolower(trimws(as.character(run_record$output_provenance %||% ""))), "displayed")) {
    stop("The SmartPLS Student first-100 profile must record output_provenance=displayed.", call. = FALSE)
  }

  contract <- run_record$settings_contract
  if (
    is.null(contract) ||
    !identical(tolower(as.character(contract$weighting_scheme %||% "")), "path") ||
    !isTRUE(contract$standardized_results) ||
    !isTRUE(all.equal(as.numeric(contract$initial_outer_weights), 1, tolerance = 0)) ||
    !isTRUE(all.equal(as.numeric(contract$stop_criterion), 1e-7, tolerance = 0)) ||
    !identical(as.integer(contract$external_maximum_iterations), 3000L) ||
    !identical(tolower(as.character(contract$fit_target %||% "")), "saturated")
  ) stop("The recorded SmartPLS settings contract is incomplete or inconsistent.", call. = FALSE)

  artifact_fields <- c(
    imported_data = "imported_data", imported_data_settings = "imported_data_settings",
    executed_model = "executed_model", algorithm_settings = "algorithm_settings",
    data_confirmation = "data_confirmation", model_canvas = "model_canvas",
    settings_confirmation = "settings_confirmation", export_lock = "export_lock"
  )
  artifacts <- run_record$execution_artifacts
  if (is.null(artifacts)) stop("SmartPLS execution_artifacts are required.", call. = FALSE)
  resolved <- list()
  for (field in names(artifact_fields)) {
    prefix <- artifact_fields[[field]]
    path <- pls_evidence_resolve_private_file(private_evidence_dir, artifacts[[paste0(prefix, "_file")]], paste("SmartPLS", field))
    pls_evidence_require_recorded_artifact(
      path, artifacts[[paste0(prefix, "_sha256")]], artifacts[[paste0(prefix, "_bytes")]], paste("SmartPLS", field)
    )
    resolved[[field]] <- path
  }

  settings <- jsonlite::fromJSON(resolved$algorithm_settings, simplifyVector = TRUE)
  pls_settings <- settings$plsAlgorithmSettings
  initial <- pls_settings$initialWeights
  if (
    is.null(pls_settings) ||
    !identical(as.character(pls_settings$weightingScheme %||% ""), "PATH") ||
    !identical(as.integer(pls_settings$maxIterations), 3000L) ||
    !identical(as.character(pls_settings$stopCriterium %||% ""), "TEN_HIGH_MINUS_7") ||
    !identical(as.character(pls_settings$initialWeightsStrategy %||% ""), "INDIVIDUAL") ||
    !identical(as.character(pls_settings$standardizationOption %||% ""), "STANDARDIZED") ||
    !is.data.frame(initial) || !identical(as.character(initial$mv), paste0("x", 1:9)) ||
    any(!is.finite(as.numeric(initial$weight))) || any(as.numeric(initial$weight) != 1) ||
    is.null(settings$plscAlgorithmSettings)
  ) stop("The retained SmartPLS algorithm settings do not satisfy the benchmark contract.", call. = FALSE)

  source_data <- utils::read.csv(file.path("sample", "HolzingerSwineford1939.csv"), check.names = FALSE, stringsAsFactors = FALSE)
  expected_data <- source_data[seq_len(100L), paste0("x", 1:9), drop = FALSE]
  imported_data <- pls_evidence_read_semicolon_data(resolved$imported_data, "SmartPLS imported data")
  if (
    !identical(names(imported_data), paste0("x", 1:9)) || nrow(imported_data) != 100L ||
    !isTRUE(all.equal(as.matrix(imported_data), as.matrix(expected_data), tolerance = 0, check.attributes = FALSE))
  ) stop("The retained SmartPLS imported data are not the exact first 100 source rows and x1-x9.", call. = FALSE)

  runs <- run_record$runs
  if (is.null(runs)) stop("SmartPLS PLS/PLSc run records are required.", call. = FALSE)
  for (estimator in c("pls", "plsc")) {
    run <- runs[[estimator]]
    iterations <- suppressWarnings(as.integer(run$iterations %||% NA_integer_))
    if (!isTRUE(run$converged) || length(iterations) != 1L || is.na(iterations) || iterations < 1L || iterations >= 300L) {
      stop(toupper(estimator), " must retain an explicit converged iteration count below 300.", call. = FALSE)
    }
    for (kind in c("fit_evidence", "convergence_evidence", "execution_log")) {
      path <- pls_evidence_resolve_private_file(private_evidence_dir, run[[paste0(kind, "_file")]], paste(toupper(estimator), kind))
      pls_evidence_require_recorded_artifact(
        path, run[[paste0(kind, "_sha256")]], run[[paste0(kind, "_bytes")]], paste(toupper(estimator), kind)
      )
    }
  }
  invisible(TRUE)
}

finalize_pls_external_evidence <- function(
  evidence_dir, software, software_version, run_date, converged_before_300,
  reported_decimal_places, absolute_tolerance = 1e-6, relative_tolerance = 1e-4,
  private_evidence_dir = evidence_dir
) {
  evidence_dir <- normalizePath(evidence_dir, winslash = "/", mustWork = TRUE)
  if (!nzchar(trimws(software))) stop("External software name is required.", call. = FALSE)
  if (!nzchar(trimws(software_version))) stop("External software version is required.", call. = FALSE)
  parsed_run_date <- suppressWarnings(try(as.Date(run_date, format = "%Y-%m-%d"), silent = TRUE))
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", run_date) || inherits(parsed_run_date, "try-error") || is.na(parsed_run_date) || format(parsed_run_date, "%Y-%m-%d") != run_date) {
    stop("Run date must be a valid calendar date using YYYY-MM-DD.", call. = FALSE)
  }
  if (!isTRUE(converged_before_300)) stop("External convergence before 300 iterations must be explicitly confirmed.", call. = FALSE)
  reported_decimal_places <- suppressWarnings(as.integer(reported_decimal_places))
  if (length(reported_decimal_places) != 1L || is.na(reported_decimal_places) || reported_decimal_places < 3L || reported_decimal_places > 15L) {
    stop("Reported decimal places must be an integer from 3 to 15.", call. = FALSE)
  }
  rounding_tolerance <- 0.5 * 10^(-reported_decimal_places) + .Machine$double.eps
  absolute_tolerance <- max(as.numeric(absolute_tolerance), rounding_tolerance)

  statedu_path <- file.path(evidence_dir, "statedu_fit.csv")
  external_path <- file.path(evidence_dir, "external_fit.csv")
  run_record_path <- file.path(evidence_dir, "external_run.json")
  comparison_path <- file.path(evidence_dir, "comparison.csv")
  for (path in c(statedu_path, external_path, run_record_path)) {
    if (!file.exists(path)) stop("Required handoff file was not found: ", path, call. = FALSE)
  }

  run_record <- jsonlite::fromJSON(run_record_path, simplifyVector = TRUE)
  if (!identical(as.character(run_record$schema_version %||% ""), "1.2")) {
    stop("external_run.json schema_version must be 1.2.", call. = FALSE)
  }
  finalization_status <- tolower(trimws(as.character(run_record$finalization_status %||% "")))
  if (!finalization_status %in% c("pending", "finalized")) {
    stop("external_run.json must declare finalization_status=pending or finalized.", call. = FALSE)
  }
  if (!identical(run_record$public_text_hash_normalization %||% "", "CRLF and CR normalized to LF before SHA-256")) {
    stop("The public text-hash normalization contract is missing.", call. = FALSE)
  }
  pls_evidence_require_recorded_text_hash(statedu_path, run_record$statedu_fit_sha256, "StatEdu fit input")
  if (identical(finalization_status, "pending")) {
    if (nzchar(trimws(as.character(run_record$external_fit_sha256 %||% ""))) ||
        nzchar(trimws(as.character(run_record$comparison_sha256 %||% ""))) || file.exists(comparison_path)) {
      stop("Pending evidence must not contain a finalized external/comparison seal or comparison.csv.", call. = FALSE)
    }
  } else {
    pls_evidence_require_recorded_text_hash(external_path, run_record$external_fit_sha256, "External fit input")
    if (!file.exists(comparison_path)) stop("Finalized comparison.csv is missing.", call. = FALSE)
    pls_evidence_require_recorded_text_hash(comparison_path, run_record$comparison_sha256, "Finalized comparison")
    if (
      !identical(trimws(as.character(run_record$software %||% "")), trimws(software)) ||
      !identical(trimws(as.character(run_record$version %||% "")), trimws(software_version)) ||
      !identical(as.character(run_record$run_date %||% ""), run_date) ||
      !isTRUE(run_record$standardized_results) || !isTRUE(run_record$converged_before_300) ||
      !identical(tolower(as.character(run_record$fit_target %||% "")), "saturated") ||
      !identical(tolower(as.character(run_record$weighting_scheme %||% "")), "path") ||
      !identical(as.integer(run_record$reported_decimal_places), reported_decimal_places) ||
      abs(as.numeric(run_record$absolute_tolerance) - absolute_tolerance) > 1e-15 ||
      abs(as.numeric(run_record$relative_tolerance) - as.numeric(relative_tolerance)) > 1e-15
    ) stop("Finalized comparison settings do not match the retained seal.", call. = FALSE)
  }
  profile_id <- as.character(run_record$profile %||% "holzinger-swineford-301")
  profile <- pls_external_benchmark_profile(profile_id)
  canonical_data <- file.path("sample", "HolzingerSwineford1939.csv")
  canonical_model <- file.path("sample", "pls_external_benchmark.stmodel")
  if (!file.exists(canonical_data) || !file.exists(canonical_model)) stop("Canonical external benchmark fixtures were not found.", call. = FALSE)
  data_file <- pls_evidence_resolve_file(evidence_dir, run_record$data_file, "External run data")
  model_file <- pls_evidence_resolve_file(evidence_dir, run_record$model_file, "External run model contract")
  source_data_file <- pls_evidence_resolve_file(evidence_dir, run_record$source_data_file, "External source data")
  pls_evidence_require_recorded_text_hash(data_file, run_record$data_sha256, "External run data")
  pls_evidence_require_recorded_text_hash(model_file, run_record$model_sha256, "External run model contract")
  pls_evidence_require_recorded_text_hash(source_data_file, run_record$source_data_sha256, "External source data")
  if (!identical(tolower(pls_evidence_normalized_text_sha256(source_data_file)), tolower(pls_evidence_normalized_text_sha256(canonical_data)))) {
    stop("External source data do not match the canonical benchmark source.", call. = FALSE)
  }
  if (!identical(
    tolower(pls_evidence_normalized_text_sha256(model_file)),
    tolower(pls_evidence_normalized_text_sha256(canonical_model))
  )) {
    stop("External run model contract does not match the canonical benchmark.", call. = FALSE)
  }
  if (identical(profile$id, "holzinger-swineford-301")) {
    if (!identical(tolower(pls_evidence_normalized_text_sha256(data_file)), tolower(pls_evidence_normalized_text_sha256(canonical_data)))) {
      stop("External run data do not match the historical 301-row benchmark.", call. = FALSE)
    }
  } else {
    if (
      !identical(trimws(as.character(run_record$software %||% "")), trimws(software)) ||
      !identical(trimws(as.character(run_record$version %||% "")), trimws(software_version)) ||
      !identical(as.character(run_record$run_date %||% ""), run_date)
    ) stop("The retained SmartPLS software/version/date record must match the finalization arguments.", call. = FALSE)
    expected <- utils::read.csv(canonical_data, check.names = FALSE, stringsAsFactors = FALSE)[seq_len(100L), paste0("x", 1:9), drop = FALSE]
    observed <- pls_evidence_read_semicolon_data(data_file, "External run first-100 data")
    if (
      !identical(names(observed), paste0("x", 1:9)) || nrow(observed) != 100L ||
      !isTRUE(all.equal(as.matrix(observed), as.matrix(expected), tolerance = 0, check.attributes = FALSE))
    ) stop("External run data do not match the exact first 100 source rows and x1-x9.", call. = FALSE)
    selection <- run_record$row_selection
    if (
      is.null(selection) || !identical(as.integer(selection$start), 1L) ||
      !identical(as.integer(selection$end), 100L) || !identical(as.integer(selection$count), 100L) ||
      !identical(as.character(selection$indicators), paste0("x", 1:9)) ||
      !identical(as.character(selection$order %||% ""), "source-file order preserved")
    ) stop("The first-100 row-selection record is incomplete or inconsistent.", call. = FALSE)
    pls_evidence_validate_student100_artifacts(evidence_dir, run_record, private_evidence_dir)
  }

  comparison <- pls_fit_compare_external(statedu_path, external_path, absolute_tolerance, relative_tolerance)
  if (nrow(comparison) != 6L || !all(comparison$Pass)) {
    stop("External PLS/PLSc comparison failed; evidence was not finalized.", call. = FALSE)
  }
  staged_comparison <- tempfile(pattern = "comparison-", tmpdir = evidence_dir, fileext = ".csv")
  on.exit(unlink(staged_comparison), add = TRUE)
  utils::write.csv(comparison, staged_comparison, row.names = FALSE, na = "")
  if (identical(finalization_status, "finalized")) {
    if (!identical(pls_evidence_normalized_text_sha256(staged_comparison), pls_evidence_normalized_text_sha256(comparison_path))) {
      stop("Finalized comparison.csv does not match canonical regeneration.", call. = FALSE)
    }
    return(invisible(list(comparison = comparison, comparison_path = comparison_path, run_record_path = run_record_path)))
  }
  if (!file.rename(staged_comparison, comparison_path)) stop("Could not publish comparison.csv atomically.", call. = FALSE)

  run_record$software <- trimws(software)
  run_record$version <- trimws(software_version)
  run_record$run_date <- run_date
  run_record$fit_target <- "saturated"
  run_record$weighting_scheme <- "path"
  run_record$standardized_results <- TRUE
  run_record$converged_before_300 <- TRUE
  run_record$reported_decimal_places <- reported_decimal_places
  run_record$absolute_tolerance <- as.numeric(absolute_tolerance)
  run_record$relative_tolerance <- as.numeric(relative_tolerance)
  run_record$finalization_status <- "finalized"
  run_record$statedu_fit_sha256 <- pls_evidence_normalized_text_sha256(statedu_path)
  run_record$external_fit_sha256 <- pls_evidence_normalized_text_sha256(external_path)
  run_record$comparison_sha256 <- pls_evidence_normalized_text_sha256(comparison_path)
  jsonlite::write_json(run_record, run_record_path, auto_unbox = TRUE, pretty = TRUE)

  invisible(list(comparison = comparison, comparison_path = comparison_path, run_record_path = run_record_path))
}

if (sys.nframe() == 0L) {
  arguments <- pls_evidence_arguments(commandArgs(trailingOnly = TRUE))
  converged <- tolower(trimws(arguments$converged_before_300)) %in% c("true", "t", "1", "yes")
  result <- finalize_pls_external_evidence(
    arguments$evidence_dir, arguments$software, arguments$software_version, arguments$run_date,
    converged, arguments$reported_decimal_places,
    as.numeric(arguments$absolute_tolerance), as.numeric(arguments$relative_tolerance),
    if (nzchar(trimws(arguments$private_evidence_dir))) arguments$private_evidence_dir else arguments$evidence_dir
  )
  print(result$comparison, row.names = FALSE)
  cat("External PLS/PLSc evidence finalized: ", result$run_record_path, "\n", sep = "")
}
