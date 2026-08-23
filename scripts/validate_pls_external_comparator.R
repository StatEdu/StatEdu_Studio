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
  identical(benchmark_manifest$schema_version, "1.1"),
  identical(benchmark_manifest$public_text_hash_normalization, "CRLF and CR normalized to LF before SHA-256"),
  identical(benchmark_manifest$profile$id, "holzinger-swineford-301"),
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
  identical(handoff_run$schema_version, "1.2"),
  identical(handoff_run$fit_target, "saturated"),
  identical(handoff_run$weighting_scheme, "path"),
  !isTRUE(handoff_run$standardized_results),
  !isTRUE(handoff_run$converged_before_300),
  identical(handoff_run$reported_decimal_places, 0L),
  identical(handoff_run$finalization_status, "pending"),
  nchar(handoff_run$data_sha256) == 64L,
  nchar(handoff_run$model_sha256) == 64L,
  nchar(handoff_run$statedu_fit_sha256) == 64L
)
handoff_external <- pls_fit_read_comparison_csv(handoff$statedu_path, "StatEdu")
handoff_external[, c("srmr", "d_G", "d_ULS")] <- round(handoff_external[, c("srmr", "d_G", "d_ULS")], 3L)
utils::write.csv(handoff_external, handoff$external_path, row.names = FALSE, na = "")
writeLines("stale comparison", file.path(handoff_dir, "comparison.csv"), useBytes = TRUE)
stopifnot(inherits(try(finalize_pls_external_evidence(
  handoff_dir, "ExternalTest", "1.0", "2026-08-19", TRUE, 3L
), silent = TRUE), "try-error"))
unlink(file.path(handoff_dir, "comparison.csv"))
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
  identical(finalized_run$finalization_status, "finalized"),
  finalized_run$absolute_tolerance >= 0.0005,
  identical(finalized_run$external_fit_sha256, pls_evidence_normalized_text_sha256(handoff$external_path)),
  identical(finalized_run$comparison_sha256, pls_evidence_normalized_text_sha256(finalized$comparison_path))
)
finalized_run$data_sha256 <- paste(rep("0", 64L), collapse = "")
jsonlite::write_json(finalized_run, finalized$run_record_path, auto_unbox = TRUE, pretty = TRUE)
invalid_hash_result <- try(finalize_pls_external_evidence(
  handoff_dir, "ExternalTest", "1.0", "2026-08-19", TRUE, 3L
), silent = TRUE)
stopifnot(inherits(invalid_hash_result, "try-error"), file.exists(finalized$comparison_path))
stopifnot(inherits(try(finalize_pls_external_evidence(
  handoff_dir, "ExternalTest", "1.0", "2026-08-19", FALSE, 3L
), silent = TRUE), "try-error"))

basic_dir <- tempfile(pattern = "pls-external-student100-")
basic <- prepare_pls_external_handoff(basic_dir, "holzinger-swineford-first100-smartpls-student")
on.exit(unlink(basic_dir, recursive = TRUE, force = TRUE), add = TRUE)
basic_manifest <- jsonlite::fromJSON(basic$manifest_path, simplifyVector = TRUE)
basic_data <- utils::read.table(
  basic$data_path, header = TRUE, sep = ";", check.names = FALSE,
  stringsAsFactors = FALSE, quote = "", comment.char = ""
)
stopifnot(
  identical(basic$profile, "holzinger-swineford-first100-smartpls-student"),
  identical(basic_manifest$schema_version, "1.1"),
  identical(basic_manifest$data$rows, 100L),
  identical(names(basic_data), paste0("x", 1:9)),
  nrow(basic_data) == 100L,
  identical(toupper(pls_handoff_text_sha256(basic$data_path)), "F95E19EC5474CDA087F42D348FCEE447FF3AA271009E021E43F9ED0C6CC52C32")
)

basic_external <- pls_fit_read_comparison_csv(basic$statedu_path, "StatEdu")
basic_external[, c("srmr", "d_G", "d_ULS")] <- round(basic_external[, c("srmr", "d_G", "d_ULS")], 3L)
utils::write.csv(basic_external, basic$external_path, row.names = FALSE, na = "")

artifact_names <- c(
  imported_data_settings = "smartpls_imported_data.settings.json",
  executed_model = "smartpls_model.splsm", algorithm_settings = "smartpls_algorithm.settings.json",
  data_confirmation = "data_confirmation.png", model_canvas = "model_canvas.png",
  settings_confirmation = "settings_confirmation.png", export_lock = "export_lock.png",
  pls_fit = "pls_model_fit.png", pls_convergence = "pls_convergence.png", pls_log = "pls_log.png",
  plsc_fit = "plsc_model_fit.png", plsc_convergence = "plsc_convergence.png", plsc_log = "plsc_log.png"
)
for (name in names(artifact_names)) writeLines(paste("synthetic validator fixture", name), file.path(basic_dir, artifact_names[[name]]), useBytes = TRUE)
invisible(file.copy(basic$data_path, file.path(basic_dir, "smartpls_imported_data.txt"), overwrite = TRUE))
initial_weights <- lapply(seq_len(9L), function(index) list(
  mv = paste0("x", index), lv = rep(c("visual", "textual", "speed"), each = 3L)[[index]], weight = 1.0
))
jsonlite::write_json(list(
  plsAlgorithmSettings = list(
    weightingScheme = "PATH", maxIterations = 3000L, initialWeights = initial_weights,
    stopCriterium = "TEN_HIGH_MINUS_7", initialWeightsStrategy = "INDIVIDUAL",
    standardizationOption = "STANDARDIZED"
  ),
  plscAlgorithmSettings = list(decoratorSettings = list(innerModelDecoratorId = "PLS_PATH_COEFFICIENTS"))
), file.path(basic_dir, artifact_names[["algorithm_settings"]]), auto_unbox = TRUE, pretty = TRUE, null = "null")

basic_run <- jsonlite::fromJSON(basic$run_record_path, simplifyVector = FALSE)
basic_run$software <- "SmartPLS"
basic_run$version <- "4.1.1.8"
basic_run$run_date <- "2026-08-23"
basic_run$license_edition <- "Student"
basic_run$license_description <- "Student license (free limited, non-Professional)"
basic_run$output_provenance <- "displayed"
basic_run$execution_artifacts <- list(
  imported_data_file = "smartpls_imported_data.txt",
  imported_data_bytes = file.info(file.path(basic_dir, "smartpls_imported_data.txt"))$size,
  imported_data_sha256 = pls_handoff_sha256(file.path(basic_dir, "smartpls_imported_data.txt")),
  imported_data_settings_file = artifact_names[["imported_data_settings"]],
  imported_data_settings_bytes = file.info(file.path(basic_dir, artifact_names[["imported_data_settings"]]))$size,
  imported_data_settings_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["imported_data_settings"]])),
  executed_model_file = artifact_names[["executed_model"]],
  executed_model_bytes = file.info(file.path(basic_dir, artifact_names[["executed_model"]]))$size,
  executed_model_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["executed_model"]])),
  algorithm_settings_file = artifact_names[["algorithm_settings"]],
  algorithm_settings_bytes = file.info(file.path(basic_dir, artifact_names[["algorithm_settings"]]))$size,
  algorithm_settings_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["algorithm_settings"]])),
  data_confirmation_file = artifact_names[["data_confirmation"]],
  data_confirmation_bytes = file.info(file.path(basic_dir, artifact_names[["data_confirmation"]]))$size,
  data_confirmation_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["data_confirmation"]])),
  model_canvas_file = artifact_names[["model_canvas"]],
  model_canvas_bytes = file.info(file.path(basic_dir, artifact_names[["model_canvas"]]))$size,
  model_canvas_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["model_canvas"]])),
  settings_confirmation_file = artifact_names[["settings_confirmation"]],
  settings_confirmation_bytes = file.info(file.path(basic_dir, artifact_names[["settings_confirmation"]]))$size,
  settings_confirmation_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["settings_confirmation"]])),
  export_lock_file = artifact_names[["export_lock"]],
  export_lock_bytes = file.info(file.path(basic_dir, artifact_names[["export_lock"]]))$size,
  export_lock_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["export_lock"]]))
)
basic_run$runs <- list(
  pls = list(
    converged = TRUE, iterations = 26L,
    fit_evidence_file = artifact_names[["pls_fit"]], fit_evidence_bytes = file.info(file.path(basic_dir, artifact_names[["pls_fit"]]))$size, fit_evidence_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["pls_fit"]])),
    convergence_evidence_file = artifact_names[["pls_convergence"]], convergence_evidence_bytes = file.info(file.path(basic_dir, artifact_names[["pls_convergence"]]))$size, convergence_evidence_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["pls_convergence"]])),
    execution_log_file = artifact_names[["pls_log"]], execution_log_bytes = file.info(file.path(basic_dir, artifact_names[["pls_log"]]))$size, execution_log_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["pls_log"]]))
  ),
  plsc = list(
    converged = TRUE, iterations = 26L,
    fit_evidence_file = artifact_names[["plsc_fit"]], fit_evidence_bytes = file.info(file.path(basic_dir, artifact_names[["plsc_fit"]]))$size, fit_evidence_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["plsc_fit"]])),
    convergence_evidence_file = artifact_names[["plsc_convergence"]], convergence_evidence_bytes = file.info(file.path(basic_dir, artifact_names[["plsc_convergence"]]))$size, convergence_evidence_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["plsc_convergence"]])),
    execution_log_file = artifact_names[["plsc_log"]], execution_log_bytes = file.info(file.path(basic_dir, artifact_names[["plsc_log"]]))$size, execution_log_sha256 = pls_handoff_sha256(file.path(basic_dir, artifact_names[["plsc_log"]]))
  )
)
jsonlite::write_json(basic_run, basic$run_record_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
basic_finalized <- finalize_pls_external_evidence(
  basic_dir, "SmartPLS", "4.1.1.8", "2026-08-23", TRUE, 3L
)
stopifnot(
  nrow(basic_finalized$comparison) == 6L,
  all(basic_finalized$comparison$Pass),
  identical(jsonlite::fromJSON(basic$run_record_path)$finalization_status, "finalized")
)
invisible(finalize_pls_external_evidence(
  basic_dir, "SmartPLS", "4.1.1.8", "2026-08-23", TRUE, 3L
))

assert_finalized_tamper_blocked <- function(path) {
  original <- readBin(path, what = "raw", n = file.info(path)$size)
  on.exit(writeBin(original, path), add = TRUE)
  sealed_record_hash <- pls_evidence_sha256(basic$run_record_path)
  writeBin(c(original, as.raw(0L)), path)
  result <- try(finalize_pls_external_evidence(
    basic_dir, "SmartPLS", "4.1.1.8", "2026-08-23", TRUE, 3L
  ), silent = TRUE)
  stopifnot(
    inherits(result, "try-error"),
    identical(pls_evidence_sha256(basic$run_record_path), sealed_record_hash)
  )
}
for (path in c(
  basic$statedu_path, basic$external_path, basic_finalized$comparison_path,
  file.path(basic_dir, artifact_names[["export_lock"]])
)) {
  assert_finalized_tamper_blocked(path)
}

basic_run <- jsonlite::fromJSON(basic$run_record_path, simplifyVector = FALSE)
basic_run$execution_artifacts$export_lock_sha256 <- paste(rep("0", 64L), collapse = "")
jsonlite::write_json(basic_run, basic$run_record_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
stopifnot(inherits(try(finalize_pls_external_evidence(
  basic_dir, "SmartPLS", "4.1.1.8", "2026-08-23", TRUE, 3L
), silent = TRUE), "try-error"))

tam_evidence_dir <- file.path(
  "docs", "evidence", "release_1_2_4", "pls", "smartpls_4_1_1_8_tam100_supplement"
)
tam_manifest_path <- file.path(tam_evidence_dir, "evidence_manifest.json")
stopifnot(file.exists(tam_manifest_path))
tam_manifest <- jsonlite::fromJSON(tam_manifest_path, simplifyVector = TRUE)
tam_evidence <- tam_manifest$retained_evidence
reference_path <- file.path(tam_evidence_dir, tam_evidence$reference_file)
stopifnot(
  file.exists(reference_path),
  identical(tolower(pls_evidence_normalized_text_sha256(reference_path)), tolower(tam_evidence$reference_sha256))
)
tam_reference <- utils::read.csv(
  reference_path,
  check.names = FALSE, stringsAsFactors = FALSE
)
stopifnot(
  identical(tam_manifest$status, "supplementary"),
  identical(tam_manifest$software, "SmartPLS"),
  identical(tam_manifest$version, "4.1.1.8"),
  identical(tam_manifest$license_edition, "Student"),
  identical(tam_manifest$output_provenance, "manual displayed transcription; source/path evidence not retained; optional check"),
  !isTRUE(tam_manifest$source_artifacts_included),
  is.null(tam_manifest$local_execution_hashes),
  all(c(tam_evidence$pls_model_fit_bytes, tam_evidence$plsc_model_fit_bytes) > 0),
  nrow(tam_reference) == 20L,
  sum(tam_reference$Type == "fit") == 6L,
  sum(tam_reference$Type == "path") == 14L,
  is.na(tam_reference$SmartPLS[tam_reference$Type == "fit" & tam_reference$Estimator == "plsc" & tam_reference$Key == "d_g"])
)

cat("External PLS fit comparator validation passed.\n")
