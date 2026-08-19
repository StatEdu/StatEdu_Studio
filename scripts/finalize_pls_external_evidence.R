source(file.path("scripts", "compare_pls_fit_external.R"), encoding = "UTF-8")

pls_evidence_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

pls_evidence_arguments <- function(arguments) {
  values <- list(
    evidence_dir = file.path("outputs", "pls_external_handoff"),
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

finalize_pls_external_evidence <- function(
  evidence_dir, software, software_version, run_date, converged_before_300,
  reported_decimal_places, absolute_tolerance = 1e-6, relative_tolerance = 1e-4
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
  canonical_data <- file.path("sample", "HolzingerSwineford1939.csv")
  canonical_model <- file.path("sample", "pls_external_benchmark.stmodel")
  if (!file.exists(canonical_data) || !file.exists(canonical_model)) stop("Canonical external benchmark fixtures were not found.", call. = FALSE)
  if (!identical(tolower(run_record$data_sha256), tolower(pls_evidence_sha256(canonical_data)))) stop("External run data hash does not match the canonical benchmark.", call. = FALSE)
  if (!identical(tolower(run_record$model_sha256), tolower(pls_evidence_sha256(canonical_model)))) stop("External run model hash does not match the canonical benchmark.", call. = FALSE)

  comparison <- pls_fit_compare_external(statedu_path, external_path, absolute_tolerance, relative_tolerance)
  if (nrow(comparison) != 6L || !all(comparison$Pass)) {
    stop("External PLS/PLSc comparison failed; evidence was not finalized.", call. = FALSE)
  }
  utils::write.csv(comparison, comparison_path, row.names = FALSE, na = "")

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
  run_record$statedu_fit_sha256 <- pls_evidence_sha256(statedu_path)
  run_record$external_fit_sha256 <- pls_evidence_sha256(external_path)
  run_record$comparison_sha256 <- pls_evidence_sha256(comparison_path)
  jsonlite::write_json(run_record, run_record_path, auto_unbox = TRUE, pretty = TRUE)

  invisible(list(comparison = comparison, comparison_path = comparison_path, run_record_path = run_record_path))
}

if (sys.nframe() == 0L) {
  arguments <- pls_evidence_arguments(commandArgs(trailingOnly = TRUE))
  converged <- tolower(trimws(arguments$converged_before_300)) %in% c("true", "t", "1", "yes")
  result <- finalize_pls_external_evidence(
    arguments$evidence_dir, arguments$software, arguments$software_version, arguments$run_date,
    converged, arguments$reported_decimal_places,
    as.numeric(arguments$absolute_tolerance), as.numeric(arguments$relative_tolerance)
  )
  print(result$comparison, row.names = FALSE)
  cat("External PLS/PLSc evidence finalized: ", result$run_record_path, "\n", sep = "")
}
