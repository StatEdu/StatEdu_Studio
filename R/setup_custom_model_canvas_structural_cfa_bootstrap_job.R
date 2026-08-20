# Background job orchestration for CFA reliability, Bollen-Stine, and HTMT bootstraps.

structural_canvas_write_cfa_bootstrap_progress <- function(progress_file, phase, completed, total, valid = 0L) {
  saveRDS(list(
    phase = as.character(phase), completed = as.integer(completed), total = as.integer(total),
    valid = as.integer(valid), updated_at = Sys.time()
  ), progress_file)
  invisible(NULL)
}

structural_canvas_cfa_bootstrap_total <- function(args) {
  reliability_total <- as.integer(args$reliability_bootstrap %||% 0L) +
    if (identical(structural_canvas_bootstrap_ci_method(args$reliability_ci_method), "bca") && as.integer(args$reliability_bootstrap %||% 0L) > 0L) nrow(args$data) else 0L
  sum(c(reliability_total, as.integer(args$bollen_stine_bootstrap %||% 0L), as.integer(args$htmt_bootstrap %||% 0L)), na.rm = TRUE)
}

structural_canvas_cfa_bootstrap_job_value <- function(args, progress_file) {
  fit <- args$fit
  data <- args$data
  reliability_total <- as.integer(args$reliability_bootstrap) + if (identical(structural_canvas_bootstrap_ci_method(args$reliability_ci_method), "bca") && args$reliability_bootstrap > 0L) nrow(data) else 0L
  total <- structural_canvas_cfa_bootstrap_total(args)
  offset <- 0L
  report <- function(phase, done, phase_total, valid) {
    structural_canvas_write_cfa_bootstrap_progress(progress_file, phase, offset + done, total, valid)
  }
  output <- list(reliability_bootstrap_result = NULL, bollen_stine_result = NULL, htmt_bootstrap_result = NULL)
  if (args$reliability_bootstrap > 0L) {
    value <- structural_canvas_reliability_bootstrap(
      args$syntax, data, reps = args$reliability_bootstrap, confidence = .95,
      seed = args$reliability_seed, estimator = args$estimator, missing = args$missing,
      std_lv = args$std_lv, ordered = args$ordered, formula_mode = args$validity_formula,
      original_fit = fit, ci_method = args$reliability_ci_method,
      progress = function(done, phase_total, valid) report("reliability", done, phase_total, valid)
    )
    if (nrow(value)) {
      point <- structural_canvas_reliability_estimates(fit, args$validity_formula)
      statistic_column <- c(AVE = "AVE", CR = "CR", Alpha = "Alpha", Omega = "Omega")
      value$Estimate <- vapply(seq_len(nrow(value)), function(index) {
        row <- point[point$Factor == value$Factor[[index]], , drop = FALSE]
        column <- statistic_column[[value$Statistic[[index]]]]
        if (nrow(row) && column %in% names(row)) as.numeric(row[[column]][[1L]]) else NA_real_
      }, numeric(1))
      value$`Valid %` <- 100 * value[["Valid replicates"]] / value[["Requested replicates"]]
      value <- value[, c("Factor", "Statistic", "Estimate", "Lower", "Upper", "CI method", "Valid replicates", "Requested replicates", "Valid %", "Status"), drop = FALSE]
    }
    output$reliability_bootstrap_result <- value
    offset <- offset + reliability_total
  }
  if (args$bollen_stine_bootstrap > 0L) {
    output$bollen_stine_result <- structural_canvas_bollen_stine(
      fit, args$bollen_stine_bootstrap, args$bollen_stine_seed,
      progress = function(done, phase_total, valid) report("bollen_stine", done, phase_total, valid)
    )
    offset <- offset + args$bollen_stine_bootstrap
  }
  if (args$htmt_bootstrap > 0L) {
    standardized <- lavaan::standardizedSolution(fit)
    observed <- lavaan::lavNames(fit, "ov")
    loadings <- standardized[standardized$op == "=~" & standardized$rhs %in% observed, c("lhs", "rhs"), drop = FALSE]
    factors <- unique(loadings$lhs)
    if (length(factors) >= 2L) {
      indicators <- stats::setNames(lapply(factors, function(name) unique(loadings$rhs[loadings$lhs == name])), factors)
      output$htmt_bootstrap_result <- structural_canvas_htmt_bootstrap(
        data, indicators, reps = args$htmt_bootstrap, confidence = .95,
        seed = args$htmt_seed, ordered = args$ordered, threshold = args$htmt_threshold,
        ci_method = args$htmt_ci_method,
        progress = function(done, phase_total, valid) report("htmt", done, phase_total, valid)
      )
    }
    offset <- offset + args$htmt_bootstrap
  }
  structural_canvas_write_cfa_bootstrap_progress(progress_file, "complete", total, total, total)
  output
}

structural_canvas_start_cfa_bootstrap_job <- function(bundle) {
  stopifnot(requireNamespace("callr", quietly = TRUE))
  job_dir <- tempfile("statedu-cfa-bootstrap-")
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  input_file <- file.path(job_dir, "input.rds")
  result_file <- file.path(job_dir, "result.rds")
  progress_file <- file.path(job_dir, "progress.rds")
  error_file <- file.path(job_dir, "error.txt")
  args <- list(
    fit = bundle$fit, syntax = bundle$syntax, data = bundle$analysis_data,
    estimator = bundle$estimator, missing = bundle$missing, std_lv = bundle$std_lv,
    ordered = bundle$ordered, validity_formula = bundle$validity_formula,
    reliability_bootstrap = as.integer(bundle$reliability_bootstrap %||% 0L),
    reliability_seed = bundle$reliability_seed, reliability_ci_method = bundle$reliability_ci_method,
    bollen_stine_bootstrap = as.integer(bundle$bollen_stine_bootstrap %||% 0L),
    bollen_stine_seed = bundle$bollen_stine_seed,
    htmt_bootstrap = as.integer(bundle$htmt_bootstrap %||% 0L), htmt_seed = bundle$htmt_seed,
    htmt_threshold = bundle$htmt_threshold, htmt_ci_method = bundle$htmt_ci_method
  )
  saveRDS(args, input_file)
  total <- structural_canvas_cfa_bootstrap_total(args)
  structural_canvas_write_cfa_bootstrap_progress(progress_file, "starting", 0L, total, 0L)
  process <- callr::r_bg(
    func = function(input_file, result_file, progress_file, error_file, project_dir) {
      tryCatch({
        setwd(project_dir)
        source(file.path("R", "app_bootstrap.R"), encoding = "UTF-8")
        load_app_packages()
        source_app_modules()
        saveRDS(structural_canvas_cfa_bootstrap_job_value(readRDS(input_file), progress_file), result_file)
      }, error = function(error) {
        writeLines(conditionMessage(error), error_file, useBytes = TRUE)
        quit(status = 1L, save = "no")
      })
      invisible(TRUE)
    },
    args = list(input_file = input_file, result_file = result_file, progress_file = progress_file, error_file = error_file, project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)),
    supervise = TRUE
  )
  list(process = process, directory = job_dir, result_file = result_file, progress_file = progress_file,
       error_file = error_file, started_at = Sys.time(), total = total)
}

structural_canvas_cleanup_cfa_bootstrap_job <- function(job) {
  if (is.null(job)) return(invisible(FALSE))
  directory <- as.character(job$directory %||% "")
  if (nzchar(directory) && dir.exists(directory)) unlink(directory, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
