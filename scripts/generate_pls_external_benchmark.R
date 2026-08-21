source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

benchmark_arguments <- function(arguments) {
  values <- list(output_dir = file.path("outputs", "pls_external_benchmark"))
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) != 2L) next
    values[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <- pair[[2L]]
  }
  values
}

benchmark_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

generate_pls_external_benchmark <- function(output_dir) {
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  data_path <- file.path("sample", "HolzingerSwineford1939.csv")
  model_path <- file.path("sample", "pls_external_benchmark.stmodel")
  indicators <- paste0("x", 1:9)
  analysis_data <- utils::read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)[, indicators, drop = FALSE]
  snapshot <- jsonlite::fromJSON(model_path, simplifyVector = FALSE)

  analysis <- run_structural_canvas_analysis(snapshot, analysis_data, "plssem", estimator = "PLS")
  bundle <- list(
    fit = analysis$fit,
    diagnostics = analysis,
    estimator = "PLS",
    snapshot = snapshot,
    analysis_data = analysis_data
  )

  rows <- lapply(c("PLS", "PLSC"), function(estimator) {
    fit <- structural_canvas_pls_diagnostic_fit(bundle, estimator)
    if (is.null(fit)) stop(estimator, " benchmark fit was unavailable.", call. = FALSE)
    diagnostic_bundle <- bundle
    diagnostic_bundle$fit <- fit
    diagnostic_bundle$estimator <- estimator
    values <- structural_canvas_pls_approximate_fit_indices(diagnostic_bundle, summary(fit))
    if (any(!is.finite(values[c("srmr", "d_g", "d_uls")]))) {
      stop(estimator, " benchmark produced non-finite fit diagnostics.", call. = FALSE)
    }
    data.frame(
      Model = tolower(estimator), Fit = "saturated",
      srmr = unname(values[["srmr"]]), d_G = unname(values[["d_g"]]), d_ULS = unname(values[["d_uls"]]),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  statedu <- do.call(rbind, rows)

  statedu_path <- file.path(output_dir, "statedu_fit.csv")
  template_path <- file.path(output_dir, "external_fit_template.csv")
  manifest_path <- file.path(output_dir, "benchmark_manifest.json")
  old_digits <- getOption("digits")
  on.exit(options(digits = old_digits), add = TRUE)
  options(digits = 17)
  utils::write.csv(statedu, statedu_path, row.names = FALSE, na = "")
  external_template <- statedu
  external_template[, c("srmr", "d_G", "d_ULS")] <- NA_real_
  utils::write.csv(external_template, template_path, row.names = FALSE, na = "")

  manifest <- list(
  schema_version = "1.0",
  purpose = "External SmartPLS/ADANCO saturated-model numerical comparison",
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  statedu = list(
    version = trimws(readLines("VERSION", warn = FALSE, n = 1L)),
    R = R.version.string,
    seminr = as.character(utils::packageVersion("seminr")),
    lavaan = as.character(utils::packageVersion("lavaan"))
  ),
  data = list(
    file = gsub("\\\\", "/", data_path), sha256 = benchmark_sha256(data_path),
    rows = nrow(analysis_data), indicators = indicators, missing_cells = sum(is.na(analysis_data)),
    source = "lavaan::HolzingerSwineford1939",
    export = "utils::write.csv(lavaan::HolzingerSwineford1939, row.names = FALSE)",
    preprocessing = "seminr default mean replacement with standardized PLS results"
  ),
  model = list(
    file = gsub("\\\\", "/", model_path), sha256 = benchmark_sha256(model_path),
    constructs = list(visual = c("x1", "x2", "x3"), textual = c("x4", "x5", "x6"), speed = c("x7", "x8", "x9")),
    structural_paths = c("visual -> textual", "visual -> speed", "textual -> speed"),
    ontology = "All constructs are reflective common factors initialized with Mode A weights"
  ),
  algorithm = list(
    weighting_scheme = "path", maximum_iterations = 300L, stop_criterion = 1e-7,
    missing_data = "mean replacement", sign_changes = "seminr default",
    fit_target = "saturated",
    fit_definition = "Local reflective measurement-model approximation with all construct correlations free; no estimated structural-model fit is claimed",
    external_iteration_note = "SmartPLS 4 uses a fixed 3,000-iteration maximum; numerical comparison is valid only when both implementations converge before StatEdu/seminr's 300-iteration maximum"
  ),
  external_run_required = list(
    software = NULL, version = NULL, run_date = NULL,
    settings_confirmation = "Record the exact external software version; use standardized results, path weighting, +1 initial outer weights, the fixed 1e-7 stop criterion, and saturated-model output. SmartPLS 4 fixes its maximum at 3,000 iterations, so confirm convergence occurred before 300 iterations to match the StatEdu/seminr ceiling. The benchmark data contain no missing cells.",
    output_file = "external_fit_template.csv"
  )
  )
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(list(statedu = statedu, statedu_path = statedu_path, template_path = template_path, manifest_path = manifest_path))
}

if (sys.nframe() == 0L) {
  arguments <- benchmark_arguments(commandArgs(trailingOnly = TRUE))
  result <- generate_pls_external_benchmark(arguments$output_dir)
  cat("PLS external benchmark bundle generated:\n")
  cat(" - ", result$statedu_path, "\n", sep = "")
  cat(" - ", result$template_path, "\n", sep = "")
  cat(" - ", result$manifest_path, "\n", sep = "")
  print(result$statedu, row.names = FALSE)
}
