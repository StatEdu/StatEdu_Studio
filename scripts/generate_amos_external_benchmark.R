source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

amos_benchmark_arguments <- function(arguments) {
  values <- list(output_dir = file.path("outputs", "amos_external_benchmark"))
  for (argument in arguments) {
    pair <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    if (length(pair) != 2L) next
    values[[gsub("-", "_", pair[[1L]], fixed = TRUE)]] <- pair[[2L]]
  }
  values
}

amos_benchmark_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

amos_benchmark_snapshot <- function(blocks) {
  latent_nodes <- Map(function(id, index) list(
    id = id, role = "latent", name = id, canvasLabel = id,
    x = index * 260, y = 120, measurementMode = "reflective",
    constructType = "commonFactor"
  ), names(blocks), seq_along(blocks))
  indicators <- unlist(blocks, use.names = FALSE)
  indicator_nodes <- lapply(seq_along(indicators), function(index) list(
    id = indicators[[index]], role = "indicator", name = indicators[[index]],
    variableId = indicators[[index]], canvasLabel = indicators[[index]],
    x = index * 80, y = 340
  ))
  measurement_edges <- list()
  edge_index <- 0L
  for (construct in names(blocks)) {
    for (indicator in blocks[[construct]]) {
      edge_index <- edge_index + 1L
      measurement_edges[[edge_index]] <- list(
        id = paste0("m", edge_index), from = construct, to = indicator
      )
    }
  }
  construct_pairs <- utils::combn(names(blocks), 2L, simplify = FALSE)
  covariance_edges <- Map(function(pair, index) list(
    id = paste0("c", index), from = pair[[1L]], to = pair[[2L]], kind = "covariance"
  ), construct_pairs, seq_along(construct_pairs))
  list(
    modelSchemaVersion = 7L, analysisType = "cfa",
    nodes = c(latent_nodes, indicator_nodes), edges = c(measurement_edges, covariance_edges),
    moderations = list(), covariates = list()
  )
}

amos_benchmark_results <- function(fit, constructs) {
  fit_names <- c(
    chisq = "Chi-square", df = "Degrees of freedom", pvalue = "Chi-square p",
    cfi = "CFI", tli = "TLI", rmsea = "RMSEA",
    rmsea.ci.lower = "RMSEA 90% CI lower", rmsea.ci.upper = "RMSEA 90% CI upper",
    srmr = "SRMR"
  )
  fit_values <- lavaan::fitMeasures(fit, names(fit_names))
  fit_rows <- data.frame(
    Section = "fit", Item = unname(fit_names), Value = as.numeric(fit_values),
    stringsAsFactors = FALSE
  )

  parameters <- lavaan::parameterEstimates(fit, standardized = TRUE)
  loading_rows <- parameters[parameters$op == "=~", , drop = FALSE]
  unstandardized <- data.frame(
    Section = "loading_unstandardized",
    Item = paste0(loading_rows$lhs, " -> ", loading_rows$rhs),
    Value = loading_rows$est, stringsAsFactors = FALSE
  )
  standardized <- data.frame(
    Section = "loading_standardized",
    Item = paste0(loading_rows$lhs, " -> ", loading_rows$rhs),
    Value = loading_rows$std.all, stringsAsFactors = FALSE
  )

  standardized_solution <- lavaan::standardizedSolution(fit)
  latent_pairs <- standardized_solution$op == "~~" &
    standardized_solution$lhs %in% constructs &
    standardized_solution$rhs %in% constructs &
    standardized_solution$lhs != standardized_solution$rhs
  latent_rows <- standardized_solution[latent_pairs, , drop = FALSE]
  latent_rows <- latent_rows[!duplicated(vapply(seq_len(nrow(latent_rows)), function(index) {
    paste(sort(c(latent_rows$lhs[[index]], latent_rows$rhs[[index]])), collapse = " <-> ")
  }, character(1))), , drop = FALSE]
  latent_correlations <- data.frame(
    Section = "latent_correlation",
    Item = vapply(seq_len(nrow(latent_rows)), function(index) {
      paste(sort(c(latent_rows$lhs[[index]], latent_rows$rhs[[index]])), collapse = " <-> ")
    }, character(1)),
    Value = latent_rows$est.std, stringsAsFactors = FALSE
  )
  rows <- rbind(fit_rows, unstandardized, standardized, latent_correlations)
  rows[order(rows$Section, rows$Item), , drop = FALSE]
}

amos_benchmark_program <- function(data_path) {
  normalized <- gsub("/", "\\", normalizePath(data_path, winslash = "/", mustWork = FALSE), fixed = TRUE)
  c(
    "Imports System", "Imports AmosEngineLib", "Module StatEduAmosCfaBenchmark", "  Sub Main()",
    "    Using Sem As New AmosEngine", "      Sem.TextOutput()", "      Sem.Standardized()", "      Sem.Smc()",
    paste0("      Sem.BeginGroup(\"", normalized, "\")"),
    "      Sem.AStructure(\"x1 <--- visual (1)\")", "      Sem.AStructure(\"x2 <--- visual\")", "      Sem.AStructure(\"x3 <--- visual\")",
    "      Sem.AStructure(\"x4 <--- textual (1)\")", "      Sem.AStructure(\"x5 <--- textual\")", "      Sem.AStructure(\"x6 <--- textual\")",
    "      Sem.AStructure(\"x7 <--- speed (1)\")", "      Sem.AStructure(\"x8 <--- speed\")", "      Sem.AStructure(\"x9 <--- speed\")",
    vapply(seq_len(9L), function(index) paste0("      Sem.AStructure(\"x", index, " <--- e", index, " (1)\")"), character(1)),
    "      Sem.AStructure(\"visual\")", "      Sem.AStructure(\"textual\")", "      Sem.AStructure(\"speed\")",
    "      Sem.AStructure(\"visual <--> textual\")", "      Sem.AStructure(\"visual <--> speed\")", "      Sem.AStructure(\"textual <--> speed\")",
    vapply(seq_len(9L), function(index) paste0("      Sem.AStructure(\"e", index, "\")"), character(1)),
    "      Sem.FitModel()", "    End Using", "  End Sub", "End Module"
  )
}

generate_amos_external_benchmark <- function(output_dir) {
  stopifnot(requireNamespace("digest", quietly = TRUE), requireNamespace("haven", quietly = TRUE))
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path("sample", "HolzingerSwineford1939.csv")
  blocks <- list(
    visual = c("x1", "x2", "x3"), textual = c("x4", "x5", "x6"),
    speed = c("x7", "x8", "x9")
  )
  indicators <- unlist(blocks, use.names = FALSE)
  analysis_data <- utils::read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)[, indicators, drop = FALSE]
  snapshot <- amos_benchmark_snapshot(blocks)
  analysis <- run_structural_canvas_analysis(
    snapshot, analysis_data, "cfa", estimator = "ML", missing = "listwise"
  )
  wishart_analysis <- run_structural_canvas_analysis(
    snapshot, analysis_data, "cfa", estimator = "ML", missing = "listwise",
    ml_likelihood = "wishart"
  )
  if (!inherits(analysis$fit, "lavaan") || !isTRUE(analysis$converged)) {
    stop("StatEdu CFA benchmark did not converge.", call. = FALSE)
  }
  if (!inherits(wishart_analysis$fit, "lavaan") || !isTRUE(wishart_analysis$converged)) {
    stop("StatEdu Wishart CFA benchmark did not converge.", call. = FALSE)
  }
  results <- amos_benchmark_results(analysis$fit, names(blocks))
  wishart_results <- amos_benchmark_results(wishart_analysis$fit, names(blocks))
  statedu_path <- file.path(output_dir, "statedu_results.csv")
  wishart_path <- file.path(output_dir, "statedu_wishart_results.csv")
  template_path <- file.path(output_dir, "amos_results_template.csv")
  sav_path <- file.path(output_dir, "HolzingerSwineford1939_x1-x9.sav")
  snapshot_path <- file.path(output_dir, "statedu_cfa_snapshot.json")
  syntax_path <- file.path(output_dir, "statedu_lavaan_syntax.txt")
  program_path <- file.path(output_dir, "amos_cfa_model.vb")
  manifest_path <- file.path(output_dir, "benchmark_manifest.json")
  old_digits <- getOption("digits")
  on.exit(options(digits = old_digits), add = TRUE)
  options(digits = 17)
  utils::write.csv(results, statedu_path, row.names = FALSE, na = "")
  utils::write.csv(wishart_results, wishart_path, row.names = FALSE, na = "")
  template <- results
  template$Value <- NA_real_
  utils::write.csv(template, template_path, row.names = FALSE, na = "")
  haven::write_sav(analysis_data, sav_path)
  jsonlite::write_json(snapshot, snapshot_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(analysis$syntax, syntax_path, useBytes = TRUE)
  writeLines(amos_benchmark_program(sav_path), program_path, useBytes = TRUE)
  manifest <- list(
    schema_version = "1.0",
    purpose = "StatEdu Studio versus IBM SPSS Amos ML-CFA numerical comparison",
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    statedu = list(
      version = trimws(readLines("VERSION", warn = FALSE, n = 1L)),
      R = R.version.string, lavaan = as.character(utils::packageVersion("lavaan"))
    ),
    external_target = list(software = "IBM SPSS Amos", version = NULL, status = "external_results_required"),
    data = list(
      source_file = gsub("\\\\", "/", data_path), sha256 = amos_benchmark_sha256(data_path),
      rows = nrow(analysis_data), indicators = indicators, missing_cells = sum(is.na(analysis_data)),
      source = "lavaan::HolzingerSwineford1939"
    ),
    model = list(
      type = "three-factor confirmatory factor analysis", estimator = "maximum likelihood",
      likelihood_conventions = list(
        default = "normal (lavaan default; N chi-square multiplier)",
        amos_compatible = "wishart (unbiased covariance; N-1 chi-square multiplier)"
      ),
      identification = "first loading fixed to 1 for each factor",
      mean_structure = FALSE, missing = "listwise; benchmark has no missing cells",
      factors = blocks, factor_covariances = "all freely estimated", degrees_of_freedom = unname(analysis$df)
    ),
    comparison = list(
      required_items = nrow(results), default_reported_decimal_places = 3L,
      interpretation = "Equality is assessed within half of the final displayed decimal unit unless a more precise AMOS export is supplied."
    ),
    files = list(
      statedu_results = basename(statedu_path), statedu_wishart_results = basename(wishart_path),
      amos_template = basename(template_path),
      amos_data = basename(sav_path), amos_program = basename(program_path),
      statedu_snapshot = basename(snapshot_path), statedu_syntax = basename(syntax_path)
    )
  )
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(list(
    results = results, wishart_results = wishart_results,
    statedu_path = statedu_path, wishart_path = wishart_path, template_path = template_path,
    sav_path = sav_path, program_path = program_path, manifest_path = manifest_path
  ))
}

if (sys.nframe() == 0L) {
  arguments <- amos_benchmark_arguments(commandArgs(trailingOnly = TRUE))
  result <- generate_amos_external_benchmark(arguments$output_dir)
  cat("AMOS external CFA benchmark generated:\n")
  cat(" - ", result$statedu_path, "\n", sep = "")
  cat(" - ", result$wishart_path, "\n", sep = "")
  cat(" - ", result$template_path, "\n", sep = "")
  cat(" - ", result$sav_path, "\n", sep = "")
  cat(" - ", result$program_path, "\n", sep = "")
  cat(" - ", result$manifest_path, "\n", sep = "")
  print(result$results, row.names = FALSE)
}
