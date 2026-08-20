source(file.path("scripts", "compare_pls_fit_external.R"), encoding = "UTF-8")

statedu_file <- tempfile(fileext = ".csv")
external_file <- tempfile(fileext = ".csv")
on.exit(unlink(c(statedu_file, external_file)), add = TRUE)
fixture <- data.frame(
  Model = c("pls", "pls", "plsc", "plsc"),
  Fit = rep(c("saturated", "estimated"), 2L),
  srmr = c(.041, .044, .038, .040), d_G = c(.012, .014, .009, .011), d_ULS = c(.020, .023, .017, .019),
  check.names = FALSE
)
utils::write.csv(fixture, statedu_file, row.names = FALSE)
external <- fixture[c(4L, 2L, 1L, 3L), ]
external$srmr <- external$srmr + c(1e-8, -1e-8, 1e-8, -1e-8)
utils::write.csv(external, external_file, row.names = FALSE)
comparison <- pls_fit_compare_external(statedu_file, external_file)
stopifnot(nrow(comparison) == 12L, all(comparison$Pass), identical(unique(comparison$Model), c("pls", "plsc")))

external$d_G[[1L]] <- external$d_G[[1L]] + .01
utils::write.csv(external, external_file, row.names = FALSE)
failed <- pls_fit_compare_external(statedu_file, external_file)
stopifnot(any(!failed$Pass & failed$Model == "plsc" & failed$Metric == "d_G"))

invalid_fit <- external
invalid_fit$Fit[[1L]] <- "unknown"
utils::write.csv(invalid_fit, external_file, row.names = FALSE)
stopifnot(inherits(try(pls_fit_compare_external(statedu_file, external_file), silent = TRUE), "try-error"))
stopifnot(inherits(try(pls_fit_compare_external(statedu_file, statedu_file, absolute_tolerance = -1), silent = TRUE), "try-error"))

source(file.path("scripts", "generate_pls_external_benchmark.R"), encoding = "UTF-8")
benchmark_dir <- tempfile(pattern = "pls-external-benchmark-")
dir.create(benchmark_dir, recursive = TRUE)
on.exit(unlink(benchmark_dir, recursive = TRUE, force = TRUE), add = TRUE)
benchmark <- generate_pls_external_benchmark(benchmark_dir)
benchmark_values <- pls_fit_read_comparison_csv(benchmark$statedu_path, "StatEdu")
benchmark_manifest <- jsonlite::fromJSON(benchmark$manifest_path, simplifyVector = FALSE)
stopifnot(
  nrow(benchmark_values) == 2L,
  identical(benchmark_values$Model, c("pls", "plsc")),
  identical(benchmark_values$Fit, c("saturated", "saturated")),
  any(benchmark_values[1L, c("srmr", "d_G", "d_ULS")] != benchmark_values[2L, c("srmr", "d_G", "d_ULS")]),
  identical(benchmark_manifest$schema_version, "1.0"),
  identical(benchmark_manifest$data$rows, 301L),
  nchar(benchmark_manifest$data$sha256) == 64L,
  nchar(benchmark_manifest$model$sha256) == 64L,
  identical(benchmark_manifest$algorithm$fit_target, "saturated"),
  identical(benchmark_manifest$algorithm$maximum_iterations, 300L),
  identical(benchmark_manifest$algorithm$stop_criterion, 1e-7),
  file.exists(benchmark$template_path)
)
benchmark_external <- benchmark_values
utils::write.csv(benchmark_external, external_file, row.names = FALSE)
benchmark_comparison <- pls_fit_compare_external(benchmark$statedu_path, external_file)
stopifnot(nrow(benchmark_comparison) == 6L, all(benchmark_comparison$Pass))

source(file.path("scripts", "prepare_pls_external_handoff.R"), encoding = "UTF-8")
source(file.path("scripts", "finalize_pls_external_evidence.R"), encoding = "UTF-8")
handoff_dir <- tempfile(pattern = "pls-external-handoff-")
handoff <- prepare_pls_external_handoff(handoff_dir)
on.exit(unlink(handoff_dir, recursive = TRUE, force = TRUE), add = TRUE)
stopifnot(
  file.exists(handoff$external_path),
  file.exists(handoff$measurement_path),
  file.exists(handoff$paths_path),
  file.exists(handoff$instructions_path)
)
handoff_run <- jsonlite::fromJSON(handoff$run_record_path, simplifyVector = TRUE)
stopifnot(
  identical(handoff_run$fit_target, "saturated"),
  identical(handoff_run$weighting_scheme, "path"),
  !isTRUE(handoff_run$standardized_results),
  !isTRUE(handoff_run$converged_before_300),
  identical(handoff_run$reported_decimal_places, 0L),
  nchar(handoff_run$data_sha256) == 64L,
  nchar(handoff_run$model_sha256) == 64L,
  nchar(handoff_run$statedu_fit_sha256) == 64L
)
handoff_external <- pls_fit_read_comparison_csv(handoff$statedu_path, "StatEdu")
handoff_external[, c("srmr", "d_G", "d_ULS")] <- round(handoff_external[, c("srmr", "d_G", "d_ULS")], 3L)
utils::write.csv(handoff_external, handoff$external_path, row.names = FALSE, na = "")
finalized <- finalize_pls_external_evidence(
  handoff_dir, "ExternalTest", "1.0", "2026-08-19", TRUE, 3L
)
finalized_run <- jsonlite::fromJSON(finalized$run_record_path, simplifyVector = TRUE)
stopifnot(
  nrow(finalized$comparison) == 6L,
  all(finalized$comparison$Pass),
  identical(finalized_run$software, "ExternalTest"),
  isTRUE(finalized_run$standardized_results),
  isTRUE(finalized_run$converged_before_300),
  identical(finalized_run$reported_decimal_places, 3L),
  finalized_run$absolute_tolerance >= 0.0005,
  identical(finalized_run$external_fit_sha256, pls_evidence_sha256(handoff$external_path)),
  identical(finalized_run$comparison_sha256, pls_evidence_sha256(finalized$comparison_path))
)
unlink(finalized$comparison_path)
finalized_run$data_sha256 <- paste(rep("0", 64L), collapse = "")
jsonlite::write_json(finalized_run, finalized$run_record_path, auto_unbox = TRUE, pretty = TRUE)
invalid_hash_result <- try(finalize_pls_external_evidence(
  handoff_dir, "ExternalTest", "1.0", "2026-08-19", TRUE, 3L
), silent = TRUE)
stopifnot(inherits(invalid_hash_result, "try-error"), !file.exists(finalized$comparison_path))
stopifnot(inherits(try(finalize_pls_external_evidence(
  handoff_dir, "ExternalTest", "1.0", "2026-08-19", FALSE, 3L
), silent = TRUE), "try-error"))

cat("External PLS fit comparator validation passed.\n")
