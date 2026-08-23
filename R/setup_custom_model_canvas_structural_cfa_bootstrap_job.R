# Background job orchestration for CFA reliability, Bollen-Stine, and HTMT bootstraps.

structural_canvas_write_cfa_bootstrap_progress <- function(progress_file, phase, completed, total, valid = 0L) {
  value <- list(
    phase = as.character(phase), completed = as.integer(completed), total = as.integer(total),
    valid = as.integer(valid), updated_at = Sys.time()
  )
  temporary_file <- tempfile(
    pattern = paste0(basename(progress_file), "."),
    tmpdir = dirname(progress_file)
  )
  on.exit(if (file.exists(temporary_file)) unlink(temporary_file, force = TRUE), add = TRUE)
  saveRDS(value, temporary_file)
  replaced <- FALSE
  for (attempt in seq_len(20L)) {
    replaced <- isTRUE(suppressWarnings(file.rename(temporary_file, progress_file)))
    if (replaced) break
    Sys.sleep(0.005)
  }
  invisible(replaced)
}

structural_canvas_cfa_bootstrap_progress_merge <- function(previous, current) {
  if (!is.list(current)) return(previous)
  required <- c("phase", "completed", "total", "valid")
  if (!all(required %in% names(current))) return(previous)
  completed <- suppressWarnings(as.integer(current$completed))
  total <- suppressWarnings(as.integer(current$total))
  valid <- suppressWarnings(as.integer(current$valid))
  phase <- as.character(current$phase %||% character(0))
  phase_order <- c(starting = 0L, reliability = 1L, bollen_stine = 2L, htmt = 3L, complete = 4L)
  if (length(completed) != 1L || length(total) != 1L || length(valid) != 1L ||
      length(phase) != 1L || anyNA(c(completed, total, valid)) ||
      completed < 0L || total < 0L || completed > total || valid < 0L || valid > completed ||
      !phase %in% names(phase_order)) return(previous)
  if (is.list(previous)) {
    previous_completed <- suppressWarnings(as.integer(previous$completed %||% 0L))
    previous_total <- suppressWarnings(as.integer(previous$total %||% total))
    previous_valid <- suppressWarnings(as.integer(previous$valid %||% 0L))
    previous_phase <- as.character(previous$phase %||% character(0))
    phase_regressed <- length(previous_phase) == 1L && previous_phase %in% names(phase_order) &&
      phase_order[[phase]] < phase_order[[previous_phase]]
    if ((length(previous_completed) == 1L && is.finite(previous_completed) && completed < previous_completed) ||
        (length(previous_total) == 1L && is.finite(previous_total) && total != previous_total) ||
        (length(previous_valid) == 1L && is.finite(previous_valid) && valid < previous_valid) ||
        phase_regressed) return(previous)
  }
  current$completed <- completed
  current$total <- total
  current$valid <- valid
  current
}

structural_canvas_cfa_bootstrap_total <- function(args) {
  reliability_total <- as.integer(args$reliability_bootstrap %||% 0L) +
    if (identical(structural_canvas_bootstrap_ci_method(args$reliability_ci_method), "bca") && as.integer(args$reliability_bootstrap %||% 0L) > 0L) nrow(args$data) else 0L
  sum(c(reliability_total, as.integer(args$bollen_stine_bootstrap %||% 0L), as.integer(args$htmt_bootstrap %||% 0L)), na.rm = TRUE)
}

structural_canvas_cfa_bootstrap_progress_offsets <- function(
  completed_offset, valid_offset, completed, valid
) {
  values <- suppressWarnings(vapply(list(
    completed_offset = completed_offset, valid_offset = valid_offset,
    completed = completed, valid = valid
  ), function(value) as.integer(value)[[1L]], integer(1)))
  if (length(values) != 4L || anyNA(values) || any(values < 0L)) {
    stop("CFA bootstrap progress offsets must be non-negative integers.")
  }
  list(
    completed = values[["completed_offset"]] + values[["completed"]],
    valid = values[["valid_offset"]] + values[["valid"]]
  )
}

structural_canvas_cfa_bootstrap_job_value <- function(args, progress_file) {
  job_started_at <- Sys.time()
  fit <- args$fit
  data <- args$data
  use_lavaan_metadata_fast_path <-
    structural_canvas_isolated_lavaan_bootstrap_fast_path_enabled() &&
    (as.integer(args$reliability_bootstrap %||% 0L) > 0L ||
       as.integer(args$bollen_stine_bootstrap %||% 0L) > 0L)
  fast_path_started_at <- Sys.time()
  lavaan_metadata_fast_path_state <- if (use_lavaan_metadata_fast_path) {
    structural_canvas_lavaan_worker_metadata_fast_path_install()
  } else {
    list(applied = FALSE, reason = "not requested", restore = function() invisible(FALSE))
  }
  fast_path_seconds <- as.numeric(difftime(Sys.time(), fast_path_started_at, units = "secs"))
  on.exit(try(lavaan_metadata_fast_path_state$restore(), silent = TRUE), add = TRUE)
  reliability_total <- as.integer(args$reliability_bootstrap) + if (identical(structural_canvas_bootstrap_ci_method(args$reliability_ci_method), "bca") && args$reliability_bootstrap > 0L) nrow(data) else 0L
  total <- structural_canvas_cfa_bootstrap_total(args)
  completed_offset <- 0L
  valid_offset <- 0L
  phase_last_valid <- 0L
  report <- function(phase, done, phase_total, valid) {
    phase_last_valid <<- max(
      phase_last_valid,
      suppressWarnings(as.integer(valid %||% 0L)),
      na.rm = TRUE
    )
    cumulative <- structural_canvas_cfa_bootstrap_progress_offsets(
      completed_offset, valid_offset, done, valid
    )
    structural_canvas_write_cfa_bootstrap_progress(
      progress_file, phase, cumulative$completed, total, cumulative$valid
    )
  }
  output <- list(reliability_bootstrap_result = NULL, bollen_stine_result = NULL, htmt_bootstrap_result = NULL)
  reliability_seconds <- 0
  reliability_engine_timing <- NULL
  bollen_stine_seconds <- 0
  htmt_seconds <- 0
  if (args$reliability_bootstrap > 0L) {
    phase_last_valid <- 0L
    phase_started_at <- Sys.time()
    value <- structural_canvas_reliability_bootstrap(
      args$syntax, data, reps = args$reliability_bootstrap, confidence = .95,
      seed = args$reliability_seed, estimator = args$estimator, missing = args$missing,
      std_lv = args$std_lv, ordered = args$ordered, formula_mode = args$validity_formula,
      original_fit = fit, ci_method = args$reliability_ci_method,
      ml_likelihood = args$ml_likelihood %||% "normal",
      workers = args$reliability_workers %||% NULL,
      chunk_size = args$reliability_chunk_size %||% NULL,
      progress = function(done, phase_total, valid) report("reliability", done, phase_total, valid)
    )
    reliability_engine_timing <- attr(value, "timings")
    if (nrow(value)) {
      point <- structural_canvas_reliability_estimates(fit, args$validity_formula)
      statistic_column <- c(AVE = "AVE", CR = "CR", Alpha = "Alpha", Omega = "Omega")
      value$Estimate <- vapply(seq_len(nrow(value)), function(index) {
        row <- point[point$Factor == value$Factor[[index]], , drop = FALSE]
        column <- statistic_column[[value$Statistic[[index]]]]
        if (nrow(row) && column %in% names(row)) as.numeric(row[[column]][[1L]]) else NA_real_
      }, numeric(1))
      value$`Valid %` <- 100 * value[["Valid replicates"]] / value[["Requested replicates"]]
      value <- value[, c("Factor", "Statistic", "Estimate", "Lower", "Upper", "CI method", "Quantile type", "Valid replicates", "Requested replicates", "Valid %", "Status"), drop = FALSE]
    }
    output$reliability_bootstrap_result <- value
    reliability_seconds <- as.numeric(difftime(Sys.time(), phase_started_at, units = "secs"))
    completed_offset <- completed_offset + reliability_total
    valid_offset <- valid_offset + phase_last_valid
  }
  if (args$bollen_stine_bootstrap > 0L) {
    phase_last_valid <- 0L
    phase_started_at <- Sys.time()
    output$bollen_stine_result <- structural_canvas_bollen_stine(
      fit, args$bollen_stine_bootstrap, args$bollen_stine_seed,
      progress = function(done, phase_total, valid) report("bollen_stine", done, phase_total, valid)
    )
    bollen_stine_seconds <- as.numeric(difftime(Sys.time(), phase_started_at, units = "secs"))
    completed_offset <- completed_offset + args$bollen_stine_bootstrap
    valid_offset <- valid_offset + phase_last_valid
  }
  # HTMT uses lavCor rather than repeatedly creating fitted lavaan objects, so
  # it intentionally runs after the worker-local metadata patch is restored.
  if (use_lavaan_metadata_fast_path) {
    try(lavaan_metadata_fast_path_state$restore(), silent = TRUE)
  }
  if (args$htmt_bootstrap > 0L) {
    phase_last_valid <- 0L
    phase_started_at <- Sys.time()
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
    htmt_seconds <- as.numeric(difftime(Sys.time(), phase_started_at, units = "secs"))
    completed_offset <- completed_offset + args$htmt_bootstrap
    valid_offset <- valid_offset + phase_last_valid
  }
  structural_canvas_write_cfa_bootstrap_progress(
    progress_file, "complete", total, total, valid_offset
  )
  attr(output, "timings") <- list(
    total = as.numeric(difftime(Sys.time(), job_started_at, units = "secs")),
    fast_path_install = fast_path_seconds,
    reliability = reliability_seconds,
    reliability_engine = reliability_engine_timing,
    bollen_stine = bollen_stine_seconds,
    htmt = htmt_seconds,
    lavaan_metadata_fast_path = list(
      requested = use_lavaan_metadata_fast_path,
      applied = isTRUE(lavaan_metadata_fast_path_state$applied),
      reason = as.character(lavaan_metadata_fast_path_state$reason %||% "")
    )
  )
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
    ml_likelihood = bundle$ml_likelihood %||% "normal",
    ordered = bundle$ordered, validity_formula = bundle$validity_formula,
    reliability_bootstrap = as.integer(bundle$reliability_bootstrap %||% 0L),
    reliability_seed = bundle$reliability_seed, reliability_ci_method = bundle$reliability_ci_method,
    reliability_workers = bundle$reliability_workers %||% NULL,
    reliability_chunk_size = bundle$reliability_chunk_size %||% NULL,
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
        if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
          for (candidate in c("English_United States.utf8", "Korean_Korea.utf8")) {
            suppressWarnings(try(Sys.setlocale("LC_CTYPE", candidate), silent = TRUE))
            if (isTRUE(l10n_info()[["UTF-8"]])) break
          }
          if (!isTRUE(l10n_info()[["UTF-8"]])) {
            stop("The CFA bootstrap worker could not activate a Windows UTF-8 locale.")
          }
        }
        options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
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
