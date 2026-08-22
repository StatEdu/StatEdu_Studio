# Mediation / moderation setup UI.

mediation_moderation_models <- function() {
  c("1", "2", "3", "4", "5", "6", "7", "8", "14", "15", "58", "59")
}

mediation_moderation_text <- function(language, en, ko) {
  language <- normalize_app_language(language)
  if (identical(language, "ko")) ko else en
}

mediation_moderation_title <- function(language = statedu_initial_language()) {
  mediation_moderation_text(language, "Mediation / Moderation", "\ub9e4\uac1c\u00b7\uc870\uc808")
}

mediation_moderation_model_label <- function(model, language = statedu_initial_language()) {
  switch(
    as.character(model),
    "1" = mediation_moderation_text(language, "Model 1: moderation", "Model 1: \uc870\uc808"),
    "2" = mediation_moderation_text(language, "Model 2: two-moderator moderation", "Model 2: \ub450 \uc870\uc808\ubcc0\uc218 \uc870\uc808"),
    "3" = mediation_moderation_text(language, "Model 3: three-way moderation", "Model 3: 3\uc6d0 \uc0c1\ud638\uc791\uc6a9 \uc870\uc808"),
    "4" = mediation_moderation_text(language, "Model 4: simple mediation", "Model 4: \ub2e8\uc21c \ub9e4\uac1c"),
    "5" = mediation_moderation_text(language, "Model 5: mediation + direct-path moderation", "Model 5: \ub9e4\uac1c + \uc9c1\uc811\uacbd\ub85c \uc870\uc808"),
    "6" = mediation_moderation_text(language, "Model 6: serial mediation", "Model 6: \uc21c\ucc28 \ub9e4\uac1c"),
    "7" = mediation_moderation_text(language, "Model 7: first-stage moderated mediation", "Model 7: 1\ub2e8\uacc4 \uc870\uc808\ub41c \ub9e4\uac1c"),
    "8" = mediation_moderation_text(language, "Model 8: first-stage + direct-path moderation", "Model 8: 1\ub2e8\uacc4 + \uc9c1\uc811\uacbd\ub85c \uc870\uc808"),
    "14" = mediation_moderation_text(language, "Model 14: second-stage moderated mediation", "Model 14: 2\ub2e8\uacc4 \uc870\uc808\ub41c \ub9e4\uac1c"),
    "15" = mediation_moderation_text(language, "Model 15: second-stage + direct-path moderation", "Model 15: 2\ub2e8\uacc4 + \uc9c1\uc811\uacbd\ub85c \uc870\uc808"),
    "58" = mediation_moderation_text(language, "Model 58: first- and second-stage moderated mediation", "Model 58: 1\ub2e8\uacc4 \ubc0f 2\ub2e8\uacc4 \uc870\uc808\ub41c \ub9e4\uac1c"),
    "59" = mediation_moderation_text(language, "Model 59: all-path moderated mediation", "Model 59: \uc804\uccb4 \uacbd\ub85c \uc870\uc808\ub41c \ub9e4\uac1c"),
    paste("Model", model)
  )
}

mediation_moderation_model_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    mediation_moderation_models(),
    vapply(
      mediation_moderation_models(),
      function(model) mediation_moderation_model_label(model, language),
      character(1)
    )
  )
}

mediation_moderation_scalar_choice <- function(value, default, allowed = NULL) {
  value <- as.character(value %||% character(0))
  value <- value[!is.na(value) & nzchar(value)]
  if (length(value) == 0) {
    value <- default
  } else {
    value <- value[[1]]
  }
  if (!is.null(allowed) && !value %in% allowed) {
    value <- default
  }
  value
}

mediation_moderation_numeric_vector <- function(value) {
  value <- value %||% numeric(0)
  if (is.data.frame(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }
  if (is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }
  if (is.character(value)) {
    value <- sub("^\\.", "0.", sub("^<\\s*", "", trimws(value)))
  }
  suppressWarnings(as.numeric(value))
}

mediation_moderation_numeric_scalar <- function(value, default = NA_real_) {
  value <- mediation_moderation_numeric_vector(value)
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    default
  } else {
    value[[1]]
  }
}

mediation_moderation_numeric_choice <- function(value, default) {
  mediation_moderation_numeric_scalar(value, default)
}

mediation_moderation_numeric_model_vector <- function(values) {
  if (is.factor(values)) {
    values <- as.character(values)
  }
  if (is.character(values)) {
    values <- trimws(values)
    values[!nzchar(values)] <- NA_character_
    values <- sub("^\\.", "0.", sub("^<\\s*", "", values))
  }
  suppressWarnings(as.numeric(values))
}

mediation_moderation_numeric_model_data <- function(data, variables, required = character(0)) {
  variables <- intersect(unique(as.character(variables %||% character(0))), names(data))
  required <- intersect(unique(as.character(required %||% character(0))), variables)
  invalid <- character(0)
  for (name in variables) {
    values <- data[[name]]
    if (is.numeric(values)) {
      next
    }
    converted <- mediation_moderation_numeric_model_vector(values)
    observed <- !is.na(values)
    if (any(observed & is.na(converted))) {
      if (name %in% required) {
        invalid <- c(invalid, name)
      }
      next
    }
    data[[name]] <- converted
  }
  if (length(invalid) > 0L) {
    stop(
      sprintf(
        "Dependent and mediator variables must be numeric for mediation/moderation regression. Check: %s",
        paste(unique(invalid), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  data
}

mediation_moderation_second_moderator <- function(moderators) {
  moderators <- as.character(moderators %||% character(0))
  moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
  if (length(moderators) >= 2L) {
    moderators[[2L]]
  } else {
    ""
  }
}

mediation_moderation_has_scalar_name <- function(value) {
  value <- as.character(value %||% character(0))
  length(value) >= 1L && !is.na(value[[1L]]) && nzchar(value[[1L]])
}

mediation_moderation_bootstrap_progress_counts <- function(done, total, boot_r) {
  total <- max(1L, as.integer(total %||% 1L))
  done <- min(total, max(0L, as.integer(done %||% 0L)))
  boot_r <- max(1L, as.integer(boot_r %||% total))
  model_total <- max(1L, ceiling(total / boot_r))
  model_index <- if (done <= 0L) 1L else min(model_total, floor((done - 1L) / boot_r) + 1L)
  current_done <- done - ((model_index - 1L) * boot_r)
  current_done <- min(boot_r, max(0L, current_done))
  list(
    done = done,
    total = total,
    current_done = current_done,
    current_total = boot_r,
    model_index = model_index,
    model_total = model_total
  )
}

mediation_moderation_bootstrap_progress_detail <- function(done, total, focal, boot_r, language = statedu_initial_language()) {
  counts <- mediation_moderation_bootstrap_progress_counts(done, total, boot_r)
  mediation_moderation_text(
    language,
    sprintf(
      "Bootstrap %s / %s (X: %s; model %s / %s)",
      counts$current_done,
      counts$current_total,
      focal,
      counts$model_index,
      counts$model_total
    ),
    sprintf(
      "\ubd80\ud2b8\uc2a4\ud2b8\ub7a9 %s / %s (X: %s; \ubaa8\ud615 %s / %s)",
      counts$current_done,
      counts$current_total,
      focal,
      counts$model_index,
      counts$model_total
    )
  )
}

mediation_moderation_bootstrap_coordinator <- function(session) {
  coordinator <- session$userData$mediation_moderation_bootstrap_coordinator
  if (!is.environment(coordinator)) {
    coordinator <- new.env(parent = emptyenv())
    coordinator$owner <- NULL
    coordinator$cancel <- NULL
    session$userData$mediation_moderation_bootstrap_coordinator <- coordinator
  }
  coordinator
}

mediation_moderation_claim_bootstrap <- function(session, owner, cancel) {
  coordinator <- mediation_moderation_bootstrap_coordinator(session)
  previous_owner <- as.character(coordinator$owner %||% "")
  if (nzchar(previous_owner) && !identical(previous_owner, owner) && is.function(coordinator$cancel)) {
    coordinator$cancel()
  }
  coordinator$owner <- as.character(owner)
  coordinator$cancel <- cancel
  invisible(coordinator)
}

mediation_moderation_release_bootstrap <- function(session, owner) {
  coordinator <- mediation_moderation_bootstrap_coordinator(session)
  if (identical(as.character(coordinator$owner %||% ""), as.character(owner))) {
    coordinator$owner <- NULL
    coordinator$cancel <- NULL
  }
  invisible(coordinator)
}

mediation_moderation_write_bootstrap_progress <- function(
  progress_file,
  done,
  total,
  focal = "",
  boot_r = total,
  phase = "resampling"
) {
  progress <- list(
    phase = as.character(phase),
    done = as.integer(done),
    total = as.integer(total),
    focal = as.character(focal %||% ""),
    boot_r = as.integer(boot_r),
    updated_at = Sys.time()
  )
  # A reader can briefly encounter an incomplete RDS while this small payload
  # is replaced. The parent keeps the last valid value, so a direct write is
  # both safe for the UI and much faster than Windows rename/copy loops.
  saveRDS(progress, progress_file)
  invisible(NULL)
}

mediation_moderation_dw_critical_cache <- new.env(parent = emptyenv())

mediation_moderation_read_dw_critical_table <- function(path) {
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(error) NULL)
}

mediation_moderation_cached_dw_critical <- function(n, p, path = regression_dw_table_path) {
  path <- as.character(path %||% "")
  path <- if (length(path) > 0L && !is.na(path[[1L]])) path[[1L]] else ""
  table_key <- paste0("table\r", path)
  if (!exists(table_key, envir = mediation_moderation_dw_critical_cache, inherits = FALSE)) {
    cached_table <- if (!nzchar(path) || !file.exists(path)) {
      list(table = NULL, note = "Durbin-Watson critical value table was not found.")
    } else {
      table <- mediation_moderation_read_dw_critical_table(path)
      if (is.null(table) || !all(c("n", "p", "dL", "dU") %in% names(table))) {
        list(table = NULL, note = "The Durbin-Watson critical value table has an invalid format.")
      } else {
        list(table = table, note = NA_character_)
      }
    }
    assign(table_key, cached_table, envir = mediation_moderation_dw_critical_cache)
  }
  cached_table <- get(table_key, envir = mediation_moderation_dw_critical_cache, inherits = FALSE)
  if (is.null(cached_table$table)) {
    return(list(dL = NA_real_, dU = NA_real_, note = cached_table$note))
  }
  n <- as.integer(n %||% NA_integer_)
  p <- as.integer(p %||% NA_integer_)
  if (length(n) == 0L || length(p) == 0L || is.na(n[[1L]]) || is.na(p[[1L]]) ||
      n[[1L]] < 1L || n[[1L]] > 2000L || p[[1L]] < 1L || p[[1L]] > 20L) {
    return(list(dL = NA_real_, dU = NA_real_, note = "The critical value table supports n = 1-2000 and p = 1-20."))
  }
  row <- cached_table$table[cached_table$table$n == n[[1L]] & cached_table$table$p == p[[1L]], , drop = FALSE]
  if (nrow(row) == 0L) {
    return(list(dL = NA_real_, dU = NA_real_, note = "Durbin-Watson critical value was not found for this n and p."))
  }
  list(
    dL = as.numeric(row$dL[[1L]]),
    dU = as.numeric(row$dU[[1L]]),
    note = NA_character_
  )
}

mediation_moderation_start_bootstrap_job <- function(args) {
  stopifnot(requireNamespace("callr", quietly = TRUE))
  job_dir <- tempfile("statedu-mediation-bootstrap-")
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  input_file <- file.path(job_dir, "input.rds")
  result_file <- file.path(job_dir, "result.rds")
  progress_file <- file.path(job_dir, "progress.rds")
  error_file <- file.path(job_dir, "error.txt")
  args$worker_preferences <- args$worker_preferences %||% list(
    result_zoom_percent = getOption("statedu.result_zoom_percent", statedu_result_zoom_default()),
    output_decimal_digits = getOption("statedu.output_decimal_digits", 3L),
    p_value_format = getOption("statedu.p_value_format", "apa"),
    multiple_correction_default = getOption("statedu.multiple_correction_default", "holm"),
    selected_variables_only_default = getOption("statedu.selected_variables_only_default", TRUE),
    default_save_dir = getOption("statedu.default_save_dir", "")
  )
  args$progress <- NULL
  saveRDS(args, input_file)
  boot_r <- max(1L, as.integer(args$boot_r %||% 5000L))
  model_total <- max(1L, length(args$roles$y %||% character(0)) * length(args$roles$x %||% character(0)))
  requested_total <- boot_r * model_total
  started_at <- Sys.time()
  mediation_moderation_write_bootstrap_progress(progress_file, 0L, requested_total, "", boot_r, "starting")
  progress_state <- new.env(parent = emptyenv())
  progress_state$progress <- list(
    phase = "starting",
    done = 0L,
    total = requested_total,
    focal = "",
    boot_r = boot_r,
    updated_at = started_at
  )
  progress_state$last_sample_done <- 0L
  progress_state$last_sample_at <- as.numeric(started_at)
  progress_state$rate_samples <- numeric(0)
  process <- callr::r_bg(
    func = function(input_file, result_file, progress_file, error_file, project_dir) {
      tryCatch({
        setwd(project_dir)
        tags <- htmltools::tags
        tagList <- htmltools::tagList
        module_files <- c(
          "utils.R", "result_labels.R", "result_coefficients.R",
          "result_panels_ui.R", "analysis_regression.R",
          "setup_mediation_moderation_ui.R"
        )
        for (module_file in module_files) {
          source(file.path("R", module_file), encoding = "UTF-8")
        }
        args <- readRDS(input_file)
        boot_r <- max(1L, as.integer(args$boot_r %||% 5000L))
        # A callr worker does not inherit the application's R options. Apply
        # persisted preferences once so numeric formatting does not reopen and
        # parse the preferences JSON for every result cell during postprocessing.
        worker_preferences <- args$worker_preferences %||% statedu_default_preferences()
        args$worker_preferences <- NULL
        statedu_apply_preferences(worker_preferences)
        initial_total <- max(
          boot_r,
          boot_r * max(1L, length(args$roles$y %||% character(0)) * length(args$roles$x %||% character(0)))
        )
        mediation_moderation_write_bootstrap_progress(
          progress_file, 0L, initial_total, "", boot_r, "preparing"
        )
        last_progress_write_at <- 0
        last_progress_focal <- ""
        args$progress <- function(done, total, focal) {
          now <- as.numeric(Sys.time())
          focal <- as.character(focal %||% "")
          if (
            done <= 0L || done >= total || !identical(focal, last_progress_focal) ||
              (now - last_progress_write_at) >= 1
          ) {
            mediation_moderation_write_bootstrap_progress(
              progress_file, done, total, focal, boot_r,
              if (done >= total) "finalizing" else "resampling"
            )
            last_progress_write_at <<- now
            last_progress_focal <<- focal
          }
        }
        value <- do.call(run_mediation_moderation_analysis, args)
        final_progress <- tryCatch(readRDS(progress_file), error = function(error) list(total = boot_r))
        final_total <- max(1L, as.integer(final_progress$total %||% boot_r))
        final_focal <- final_progress$focal %||% ""
        mediation_moderation_write_bootstrap_progress(
          progress_file, final_total, final_total, final_focal, boot_r, "serializing"
        )
        saveRDS(value, result_file)
        mediation_moderation_write_bootstrap_progress(
          progress_file, final_total, final_total, final_focal, boot_r, "complete"
        )
      }, error = function(error) {
        writeLines(conditionMessage(error), error_file, useBytes = TRUE)
        quit(status = 1L, save = "no")
      })
      invisible(TRUE)
    },
    args = list(
      input_file = input_file,
      result_file = result_file,
      progress_file = progress_file,
      error_file = error_file,
      project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)
    ),
    supervise = TRUE
  )
  list(
    process = process,
    directory = job_dir,
    result_file = result_file,
    progress_file = progress_file,
    error_file = error_file,
    started_at = started_at,
    boot_r = boot_r,
    requested_total = requested_total,
    progress_state = progress_state
  )
}

mediation_moderation_cleanup_bootstrap_job <- function(job) {
  if (is.null(job)) return(invisible(FALSE))
  directory <- as.character(job$directory %||% "")
  if (nzchar(directory) && dir.exists(directory)) unlink(directory, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

mediation_moderation_bootstrap_job_progress <- function(job, language = statedu_initial_language()) {
  progress_state <- job$progress_state
  if (!is.environment(progress_state)) {
    progress_state <- new.env(parent = emptyenv())
  }
  cached_progress <- progress_state$progress
  progress <- tryCatch(
    if (file.exists(job$progress_file)) readRDS(job$progress_file) else NULL,
    error = function(error) NULL
  )
  fallback_progress <- cached_progress %||% list(
    phase = "starting", done = 0L, total = job$requested_total %||% job$boot_r,
    focal = "", boot_r = job$boot_r, updated_at = job$started_at
  )
  if (is.null(progress)) {
    progress <- fallback_progress
  } else {
    progress_done <- max(0L, as.integer(progress$done %||% 0L))
    cached_done <- max(0L, as.integer(cached_progress$done %||% 0L))
    phase_order <- c(starting = 1L, preparing = 2L, resampling = 3L, finalizing = 4L, serializing = 5L, complete = 6L)
    progress_phase_rank <- unname(phase_order[as.character(progress$phase %||% "starting")[[1L]]])
    cached_phase_rank <- unname(phase_order[as.character(cached_progress$phase %||% "starting")[[1L]]])
    progress_phase_rank <- if (length(progress_phase_rank) == 0L || is.na(progress_phase_rank)) 0L else progress_phase_rank
    cached_phase_rank <- if (length(cached_phase_rank) == 0L || is.na(cached_phase_rank)) 0L else cached_phase_rank
    progress_regressed <- progress_done < cached_done ||
      (progress_done == cached_done && progress_phase_rank < cached_phase_rank)
    if (!is.null(cached_progress) && isTRUE(progress_regressed)) {
      progress <- cached_progress
    } else {
      progress_state$progress <- progress
    }
  }
  done <- max(0L, as.integer(progress$done %||% 0L))
  total <- max(1L, as.integer(progress$total %||% job$requested_total %||% job$boot_r))
  done <- min(done, total)
  boot_r <- max(1L, as.integer(progress$boot_r %||% job$boot_r %||% total))
  counts <- mediation_moderation_bootstrap_progress_counts(done, total, boot_r)
  elapsed <- max(0, as.numeric(difftime(Sys.time(), job$started_at, units = "secs")))
  sample_at <- suppressWarnings(as.numeric(progress$updated_at %||% Sys.time()))
  if (length(sample_at) == 0L || !is.finite(sample_at[[1L]])) sample_at <- as.numeric(Sys.time())
  sample_at <- sample_at[[1L]]
  last_sample_done <- max(0L, as.integer(progress_state$last_sample_done %||% 0L))
  last_sample_at <- suppressWarnings(as.numeric(progress_state$last_sample_at %||% as.numeric(job$started_at)))
  if (length(last_sample_at) == 0L || !is.finite(last_sample_at[[1L]])) last_sample_at <- as.numeric(job$started_at)
  last_sample_at <- last_sample_at[[1L]]
  if (
    done > last_sample_done &&
      is.finite(sample_at) && is.finite(last_sample_at) && sample_at > last_sample_at
  ) {
    # The first interval includes process/module/model setup and is not a
    # representative resampling rate. Exclude it from the ETA estimate.
    if (last_sample_done > 0L) {
      interval_rate <- (done - last_sample_done) / (sample_at - last_sample_at)
      if (is.finite(interval_rate) && interval_rate > 0) {
        samples <- c(progress_state$rate_samples %||% numeric(0), interval_rate)
        progress_state$rate_samples <- tail(samples, 5L)
      }
    }
    progress_state$last_sample_done <- done
    progress_state$last_sample_at <- sample_at
  }
  rate_samples <- progress_state$rate_samples %||% numeric(0)
  rate <- if (length(rate_samples) >= 3L) stats::median(rate_samples) else NA_real_
  remaining <- if (is.finite(rate) && rate > 0 && total > done) ceiling((total - done) / rate) else NA_integer_
  ko <- identical(normalize_app_language(language), "ko")
  focal <- as.character(progress$focal %||% "")
  focal <- if (length(focal) > 0L && !is.na(focal[[1L]])) focal[[1L]] else ""
  phase <- as.character(progress$phase %||% "starting")
  phase <- if (length(phase) > 0L && !is.na(phase[[1L]])) phase[[1L]] else "starting"
  phase_label <- if (ko) {
    switch(
      phase,
      starting = "작업 시작 중",
      preparing = "모형 준비 중",
      resampling = "재표집 중",
      finalizing = "부트스트랩 통계 계산 중",
      serializing = "결과 저장 중",
      complete = "완료",
      "실행 중"
    )
  } else {
    switch(
      phase,
      starting = "Starting worker",
      preparing = "Preparing models",
      resampling = "Resampling",
      finalizing = "Computing bootstrap summaries",
      serializing = "Saving results",
      complete = "Complete",
      "Running"
    )
  }
  if (!identical(phase, "resampling")) {
    rate <- NA_real_
    remaining <- NA_integer_
  }
  resampling_detail <- if (ko) {
    paste0(
      phase_label, " ", round(100 * done / total), "% · 전체 ", format(done, big.mark = ","), "/", format(total, big.mark = ","), "회",
      if (nzchar(focal)) paste0(" · X: ", focal) else "",
      " · 모형 ", counts$model_index, "/", counts$model_total,
      if (is.finite(rate)) paste0(" · ", format(round(rate, 1), nsmall = 1), "회/초") else "",
      " · 경과 ", floor(elapsed), "초",
      if (is.finite(remaining)) paste0(" · 재표집 예상 잔여 ", remaining, "초") else if (done > 0L && done < total) " · 재표집 잔여 계산 중" else ""
    )
  } else {
    paste0(
      phase_label, " ", round(100 * done / total), "% · total ", format(done, big.mark = ","), "/", format(total, big.mark = ","), " resamples",
      if (nzchar(focal)) paste0(" · X: ", focal) else "",
      " · model ", counts$model_index, "/", counts$model_total,
      if (is.finite(rate)) paste0(" · ", format(round(rate, 1), nsmall = 1), "/s") else "",
      " · elapsed ", floor(elapsed), " s",
      if (is.finite(remaining)) paste0(" · about ", remaining, " s of resampling remaining") else if (done > 0L && done < total) " · estimating resampling time remaining" else ""
    )
  }
  detail <- if (identical(phase, "resampling")) {
    resampling_detail
  } else if (ko) {
    switch(
      phase,
      starting = paste0("부트스트랩 작업 프로세스를 시작하는 중 · 경과 ", floor(elapsed), "초"),
      preparing = paste0("모형 행렬과 진단 통계를 준비하는 중 · 예정 ", format(total, big.mark = ","), "회 · 경과 ", floor(elapsed), "초"),
      finalizing = paste0("재표집 ", format(done, big.mark = ","), "/", format(total, big.mark = ","), "회 완료 · 신뢰구간과 결과표를 계산하는 중 · 경과 ", floor(elapsed), "초"),
      serializing = paste0("계산된 분석 결과를 저장하는 중 · 경과 ", floor(elapsed), "초"),
      complete = paste0("분석 완료 · 결과 화면을 준비하는 중 · 총 경과 ", floor(elapsed), "초"),
      paste0(phase_label, " · 경과 ", floor(elapsed), "초")
    )
  } else {
    switch(
      phase,
      starting = paste0("Starting the bootstrap worker · elapsed ", floor(elapsed), " s"),
      preparing = paste0("Preparing model matrices and diagnostics · ", format(total, big.mark = ","), " resamples planned · elapsed ", floor(elapsed), " s"),
      finalizing = paste0("Resampling complete (", format(done, big.mark = ","), "/", format(total, big.mark = ","), ") · computing confidence intervals and result tables · elapsed ", floor(elapsed), " s"),
      serializing = paste0("Saving the computed analysis result · elapsed ", floor(elapsed), " s"),
      complete = paste0("Analysis complete · preparing the result view · total elapsed ", floor(elapsed), " s"),
      paste0(phase_label, " · elapsed ", floor(elapsed), " s")
    )
  }
  percent <- if (identical(phase, "resampling")) {
    100 * done / total
  } else if (identical(phase, "complete")) {
    100
  } else {
    NA_real_
  }
  list(
    percent = percent,
    detail = detail,
    phase_label = phase_label,
    phase = phase,
    done = done,
    total = total,
    rate = rate,
    remaining = remaining
  )
}

mediation_moderation_numeric_match <- function(value, target_names) {
  target_names <- as.character(target_names %||% character(0))
  output <- stats::setNames(rep(NA_real_, length(target_names)), target_names)
  if (length(target_names) == 0L) {
    return(output)
  }
  if (is.null(value)) {
    return(output)
  }
  value_names <- names(value)
  if (!is.null(value_names) && any(nzchar(value_names))) {
    matched <- match(target_names, value_names)
    present <- !is.na(matched)
    output[present] <- mediation_moderation_numeric_vector(value[matched[present]])
    return(output)
  }
  numeric_value <- mediation_moderation_numeric_vector(value)
  count <- min(length(output), length(numeric_value))
  if (count > 0L) {
    output[seq_len(count)] <- numeric_value[seq_len(count)]
  }
  output
}

mediation_moderation_analysis_method_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("statedu", "process_ols"),
    c(
      mediation_moderation_text(language, "StatEdu diagnostic-based", "StatEdu \uc9c4\ub2e8 \uae30\ubc18"),
      mediation_moderation_text(language, "PROCESS-compatible OLS", "PROCESS \ud638\ud658 OLS")
    )
  )
}

mediation_moderation_analysis_method_label <- function(method = "statedu") {
  method <- as.character(method %||% "statedu")[[1]]
  if (identical(method, "process_ols")) "PROCESS-compatible OLS" else "StatEdu diagnostic-based"
}

mediation_moderation_ci_method_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("bias_corrected", "percentile"),
    c(
      mediation_moderation_text(language, "Bias-corrected (BC)", "Bias-corrected (BC)"),
      mediation_moderation_text(language, "Percentile", "Percentile")
    )
  )
}

mediation_moderation_builder_structure_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("none", "single", "parallel", "serial"),
    c(
      mediation_moderation_text(language, "No mediator", "\ub9e4\uac1c\ubcc0\uc218 \uc5c6\uc74c"),
      mediation_moderation_text(language, "Single mediator", "\ub2e8\uc77c \ub9e4\uac1c"),
      mediation_moderation_text(language, "Parallel multiple mediation", "\ubcd1\ub82c \ubcf5\uc218\ub9e4\uac1c"),
      mediation_moderation_text(language, "Serial mediation", "\uc21c\ucc28 \ub9e4\uac1c")
    )
  )
}

mediation_moderation_mediator_arrangement_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("parallel", "serial"),
    c(
      mediation_moderation_text(language, "Parallel multiple mediation", "\ubcd1\ub82c \ubcf5\uc218\ub9e4\uac1c"),
      mediation_moderation_text(language, "Serial mediation", "\uc21c\ucc28 \ub9e4\uac1c")
    )
  )
}

mediation_moderation_builder_path_choices <- function(language = statedu_initial_language()) {
  stats::setNames(
    c("xm", "my", "xy"),
    c(
      mediation_moderation_text(language, "W moderates X -> M", "W\uac00 X -> M \uacbd\ub85c \uc870\uc808"),
      mediation_moderation_text(language, "W moderates M -> Y", "W\uac00 M -> Y \uacbd\ub85c \uc870\uc808"),
      mediation_moderation_text(language, "W moderates X -> Y", "W\uac00 X -> Y \uc9c1\uc811\uacbd\ub85c \uc870\uc808")
    )
  )
}

mediation_moderation_checkbox_group_input <- function(input_id, choices, selected = character(0), disabled = FALSE, disabled_values = character(0)) {
  values <- unname(choices)
  labels <- names(choices)
  if (is.null(labels) || length(labels) != length(values)) labels <- values
  selected <- intersect(as.character(selected %||% character(0)), values)
  disabled_values <- intersect(as.character(disabled_values %||% character(0)), values)
  div(
    id = input_id,
    class = paste(
      "form-group shiny-input-checkboxgroup shiny-input-container",
      if (isTRUE(disabled)) "mm-disabled-checkbox-group" else ""
    ),
    role = "group",
    div(
      class = "shiny-options-group",
      Map(function(value, label) {
        item_disabled <- isTRUE(disabled) || value %in% disabled_values
        div(
          class = "checkbox",
          tags$label(
            tags$input(
              type = "checkbox",
              name = input_id,
              value = value,
              checked = if (value %in% selected) "checked" else NULL,
              disabled = if (isTRUE(item_disabled)) "disabled" else NULL
            ),
            span(label)
          )
        )
      }, values, labels)
    )
  )
}

mediation_moderation_checkbox_input <- function(input_id, label, value = FALSE, disabled = FALSE) {
  control <- checkboxInput(input_id, label, value = isTRUE(value) && !isTRUE(disabled))
  if (isTRUE(disabled)) {
    control <- htmltools::tagQuery(control)$find("input")$addAttrs(disabled = "disabled")$allTags()
  }
  control
}

mediation_moderation_moderation_option_group <- function(
  disabled = FALSE,
  dash_nonsignificant = TRUE,
  language = statedu_initial_language()
) {
  div(
    class = "analysis-option-group",
    div(class = "analysis-option-title", analysis_ui_text("Options", language)),
    mediation_moderation_checkbox_input(
      "mm_mean_center",
      analysis_ui_label("Mean-center X/W", language),
      value = FALSE,
      disabled = disabled
    ),
    mediation_moderation_checkbox_input(
      "mm_johnson_neyman",
      analysis_ui_label("Johnson-Neyman", language),
      value = TRUE,
      disabled = disabled
    ),
    mediation_moderation_checkbox_input(
      "mm_simple_slopes",
      analysis_ui_label("Simple slopes", language),
      value = TRUE,
      disabled = disabled
    ),
    mediation_moderation_checkbox_input(
      "mm_dash_nonsignificant",
      mediation_moderation_text(language, "Dash non-significant paths", "\uc720\uc758\ud558\uc9c0 \uc54a\uc740 \uacbd\ub85c \uc810\uc120"),
      value = dash_nonsignificant,
      disabled = FALSE
    )
  )
}

mediation_moderation_default_mediator_arrangement <- function(input = NULL) {
  value <- if (!is.null(input)) input$mm_mediator_arrangement else NULL
  mediation_moderation_scalar_choice(value, "parallel", c("parallel", "serial"))
}

mediation_moderation_structure_from_mediators <- function(mediators, arrangement = "parallel") {
  mediators <- as.character(mediators %||% character(0))
  mediators <- mediators[nzchar(mediators)]
  if (length(mediators) == 0) return("none")
  if (length(mediators) == 1) return("single")
  arrangement <- mediation_moderation_scalar_choice(arrangement, "parallel", c("parallel", "serial"))
  if (identical(arrangement, "serial")) "serial" else "parallel"
}

mediation_moderation_default_structure <- function(input = NULL) {
  if (!is.null(input) && !is.null(input$mm_mediators)) {
    return(mediation_moderation_structure_from_mediators(
      input$mm_mediators,
      mediation_moderation_default_mediator_arrangement(input)
    ))
  }
  value <- if (!is.null(input)) input$mm_mediator_structure else NULL
  mediation_moderation_scalar_choice(value, "single", c("none", "single", "parallel", "serial"))
}

mediation_moderation_default_moderated_paths <- function(input = NULL, structure = "single") {
  values <- if (!is.null(input)) input$mm_moderated_paths else NULL
  values <- intersect(as.character(values %||% character(0)), c("xm", "my", "xy"))
  if (identical(structure, "none")) {
    return(intersect(values, "xy"))
  }
  if (identical(structure, "serial")) {
    return(character(0))
  }
  values
}

mediation_moderation_two_moderator_model <- function(value = NULL, default = "3") {
  mediation_moderation_scalar_choice(value, default, c("2", "3"))
}

mediation_moderation_infer_model <- function(structure, moderated_paths, moderator_count = NULL, two_moderator_model = "3") {
  structure <- mediation_moderation_scalar_choice(structure, "single", c("none", "single", "parallel", "serial"))
  moderated_paths <- sort(intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy")))
  moderator_count <- suppressWarnings(as.integer(moderator_count %||% if (length(moderated_paths) > 0L) 1L else 0L))
  if (is.na(moderator_count)) moderator_count <- 0L
  key <- paste(moderated_paths, collapse = "+")
  if (identical(structure, "none") && identical(key, "xy")) {
    if (moderator_count >= 2L) return(mediation_moderation_two_moderator_model(two_moderator_model))
    return("1")
  }
  if (structure %in% c("single", "parallel")) {
    if (identical(key, "")) return("4")
    if (identical(key, "xy")) return("5")
    if (identical(key, "xm")) return("7")
    if (identical(key, "xm+xy")) return("8")
    if (identical(key, "my")) return("14")
    if (identical(key, "my+xy")) return("15")
    if (identical(key, "my+xm")) return("58")
    if (identical(key, "my+xm+xy")) return("59")
  }
  if (identical(structure, "serial") && identical(key, "")) return("6")
  NA_character_
}

mediation_moderation_model_moderated_paths <- function(model) {
  switch(
    as.character(model %||% ""),
    "1" = "xy",
    "2" = "xy",
    "3" = "xy",
    "4" = character(0),
    "5" = "xy",
    "6" = character(0),
    "7" = "xm",
    "8" = c("xm", "xy"),
    "14" = "my",
    "15" = c("my", "xy"),
    "58" = c("xm", "my"),
    "59" = c("xm", "my", "xy"),
    character(0)
  )
}

mediation_moderation_model_requires_w <- function(model) {
  as.character(model %||% "") %in% c("1", "2", "3", "5", "7", "8", "14", "15", "58", "59")
}

mediation_moderation_model_moderator_count <- function(model) {
  switch(
    as.character(model %||% ""),
    "2" = 2L,
    "3" = 2L,
    if (mediation_moderation_model_requires_w(model)) 1L else 0L
  )
}

mediation_moderation_no_mediator_terms <- function(focal, w, model, y_cov_terms = character(0)) {
  model <- as.character(model %||% "1")[[1L]]
  w <- as.character(w %||% character(0))
  w <- w[nzchar(w)]
  focal_term <- mediation_moderation_var_term(focal)
  if (identical(model, "1")) {
    w1 <- w[[1L]]
    return(c(focal_term, mediation_moderation_var_term(w1), mediation_moderation_interaction_term(focal, w1), y_cov_terms))
  }
  w <- utils::head(w, 2L)
  w_terms <- vapply(w, mediation_moderation_var_term, character(1))
  xw_terms <- vapply(w, function(moderator) mediation_moderation_interaction_term(focal, moderator), character(1))
  if (identical(model, "3")) {
    return(c(
      focal_term,
      w_terms,
      xw_terms,
      mediation_moderation_interaction_term(w[[1L]], w[[2L]]),
      mediation_moderation_interaction_term(focal, w[[1L]], w[[2L]]),
      y_cov_terms
    ))
  }
  c(focal_term, w_terms, xw_terms, y_cov_terms)
}

mediation_moderation_moderated_predictor_terms <- function(predictor, w, include_three_way = TRUE) {
  w <- utils::head(as.character(w %||% character(0)), 2L)
  w <- w[nzchar(w)]
  if (length(w) == 0L) {
    return(character(0))
  }
  terms <- vapply(w, function(moderator) mediation_moderation_interaction_term(predictor, moderator), character(1))
  if (isTRUE(include_three_way) && length(w) >= 2L) {
    terms <- c(
      mediation_moderation_interaction_term(w[[1L]], w[[2L]]),
      terms,
      mediation_moderation_interaction_term(predictor, w[[1L]], w[[2L]])
    )
  }
  terms
}

mediation_moderation_no_mediator_effects <- function(model, coefficients, focal, w, coef_fn) {
  model <- as.character(model %||% "1")[[1L]]
  w <- as.character(w %||% character(0))
  w <- w[nzchar(w)]
  effects <- c(Direct = coef_fn(coefficients, focal))
  if (length(w) >= 1L) {
    effects[["X:W interaction"]] <- coef_fn(coefficients, paste0(focal, ":", w[[1L]]))
  }
  if (length(w) >= 2L) {
    effects[["X:Z interaction"]] <- coef_fn(coefficients, paste0(focal, ":", w[[2L]]))
  }
  if (identical(model, "3") && length(w) >= 2L) {
    effects[["W:Z interaction"]] <- coef_fn(coefficients, paste0(w[[1L]], ":", w[[2L]]))
    effects[["X:W:Z interaction"]] <- coef_fn(coefficients, paste0(focal, ":", w[[1L]], ":", w[[2L]]))
  }
  effects
}

mediation_moderation_conditional_w_values <- function(data, w) {
  if (length(w) != 1L || !nzchar(w) || !is.data.frame(data) || !w %in% names(data)) {
    return(stats::setNames(numeric(0), character(0)))
  }
  if (!is.numeric(data[[w]])) {
    values <- data[[w]]
    levels <- if (is.factor(values)) levels(values) else unique(as.character(values[!is.na(values)]))
    levels <- levels[!is.na(levels) & nzchar(levels)]
    if (length(levels) == 0L) {
      return(stats::setNames(character(0), character(0)))
    }
    return(stats::setNames(as.character(levels), as.character(levels)))
  }
  values <- data[[w]]
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }
  center <- mean(values)
  spread <- stats::sd(values)
  if (!is.finite(spread)) spread <- 0
  stats::setNames(
    c(center - spread, center, center + spread),
    c("Low (M-SD)", "Mean", "High (M+SD)")
  )
}

mediation_moderation_condition_row <- function(row, moderators) {
  moderators <- intersect(as.character(moderators %||% character(0)), names(row))
  stats::setNames(lapply(moderators, function(name) row[[name]][[1L]]), moderators)
}

mediation_moderation_reference_newdata <- function(frame, conditions = list()) {
  if (!is.data.frame(frame) || nrow(frame) == 0L) {
    return(NULL)
  }
  row_values <- lapply(names(frame), function(name) {
    values <- frame[[name]]
    has_condition <- name %in% names(conditions)
    condition_value <- if (has_condition) conditions[[name]] else NULL
    if (is.factor(values)) {
      value <- if (has_condition) as.character(condition_value) else levels(values)[[1L]]
      return(factor(value, levels = levels(values), ordered = is.ordered(values)))
    }
    if (is.numeric(values) || is.integer(values)) {
      if (has_condition) {
        numeric_value <- suppressWarnings(as.numeric(condition_value))
      } else {
        numeric_value <- mean(values, na.rm = TRUE)
      }
      if (!is.finite(numeric_value)) numeric_value <- 0
      return(numeric_value)
    }
    if (is.logical(values)) {
      return(if (has_condition) as.logical(condition_value) else FALSE)
    }
    as.character(if (has_condition) condition_value else values[which(!is.na(values))[1L]] %||% "")
  })
  names(row_values) <- names(frame)
  as.data.frame(row_values, stringsAsFactors = FALSE, check.names = FALSE)
}

mediation_moderation_slope_weights_from_frame <- function(terms_object, frame, predictor, conditions = list(), contrasts = NULL) {
  predictor <- as.character(predictor %||% "")[[1L]]
  if (!nzchar(predictor) || !is.data.frame(frame) || !predictor %in% names(frame) || !is.numeric(frame[[predictor]])) {
    return(NULL)
  }
  base_row <- mediation_moderation_reference_newdata(frame, conditions)
  if (is.null(base_row)) {
    return(NULL)
  }
  low_row <- base_row
  high_row <- base_row
  low_row[[predictor]] <- 0
  high_row[[predictor]] <- 1
  model_terms <- stats::delete.response(terms_object)
  x_low <- tryCatch(stats::model.matrix(model_terms, low_row, contrasts.arg = contrasts), error = function(e) NULL)
  x_high <- tryCatch(stats::model.matrix(model_terms, high_row, contrasts.arg = contrasts), error = function(e) NULL)
  if (is.null(x_low) || is.null(x_high) || ncol(x_low) != ncol(x_high)) {
    return(NULL)
  }
  weights <- as.numeric(x_high[1L, , drop = TRUE] - x_low[1L, , drop = TRUE])
  names(weights) <- colnames(x_high)
  weights[is.finite(weights) & abs(weights) > .Machine$double.eps^0.5]
}

mediation_moderation_model_slope_weights <- function(model, predictor, conditions = list()) {
  frame <- stats::model.frame(model)
  model_matrix <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  contrasts <- if (is.null(model_matrix)) NULL else attr(model_matrix, "contrasts", exact = TRUE)
  mediation_moderation_slope_weights_from_frame(stats::terms(model), frame, predictor, conditions, contrasts)
}

mediation_moderation_model_conditional_slope <- function(model, predictor, conditions = list()) {
  weights <- mediation_moderation_model_slope_weights(model, predictor, conditions)
  if (is.null(weights) || length(weights) == 0L) {
    return(NA_real_)
  }
  mediation_moderation_weight_value(stats::coef(model), weights)
}

mediation_moderation_fast_conditional_slope <- function(coefficients, spec, predictor, conditions = list()) {
  weights <- mediation_moderation_slope_weights_from_frame(
    stats::terms(spec$formula),
    spec$frame,
    predictor,
    conditions,
    attr(spec$x, "contrasts", exact = TRUE)
  )
  if (is.null(weights) || length(weights) == 0L) {
    return(NA_real_)
  }
  mediation_moderation_weight_value(coefficients, weights)
}

mediation_moderation_conditional_moderator_grid <- function(data, w) {
  w <- utils::head(as.character(w %||% character(0)), 2L)
  w <- w[nzchar(w)]
  if (length(w) == 0L) {
    return(data.frame(stringsAsFactors = FALSE, check.names = FALSE))
  }
  value_list <- lapply(w, function(moderator) mediation_moderation_conditional_w_values(data, moderator))
  if (any(vapply(value_list, length, integer(1)) == 0L)) {
    return(data.frame(stringsAsFactors = FALSE, check.names = FALSE))
  }
  names(value_list) <- w
  level_grid <- expand.grid(lapply(value_list, names), stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  names(level_grid) <- w
  value_grid <- as.data.frame(
    lapply(w, function(moderator) unname(value_list[[moderator]][level_grid[[moderator]]])),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(value_grid) <- w
  for (moderator in w) {
    value_grid[[paste0(".level_", moderator)]] <- level_grid[[moderator]]
  }
  moderator_symbols <- stats::setNames(c("W", "Z")[seq_along(w)], w)
  value_grid$.condition <- vapply(seq_len(nrow(level_grid)), function(row_index) {
    paste(vapply(w, function(moderator) {
      sprintf("%s %s", moderator_symbols[[moderator]], level_grid[[moderator]][[row_index]])
    }, character(1)), collapse = "; ")
  }, character(1))
  value_grid
}

mediation_moderation_conditional_indirect_effects <- function(
  mediators,
  focal,
  w,
  conditional_grid,
  x_to_m,
  moderated_x_to_m,
  moderated_m_to_y,
  coef_a,
  coef_b,
  coef_a_interaction,
  coef_b_interaction,
  coef_a_three_way = NULL,
  coef_b_three_way = NULL,
  coef_a_at = NULL,
  coef_b_at = NULL,
  moderation_map = NULL,
  outcome = ""
) {
  w <- utils::head(as.character(w %||% character(0)), 2L)
  w <- w[nzchar(w)]
  if (length(w) == 0L || !is.data.frame(conditional_grid) || nrow(conditional_grid) == 0L) {
    return(c())
  }
  effects <- c()
  for (mediator in as.character(mediators %||% character(0))) {
    xm_moderated <- focal %in% as.character(moderated_x_to_m[[mediator]] %||% character(0))
    my_moderated <- mediator %in% as.character(moderated_m_to_y %||% character(0))
    if (!isTRUE(xm_moderated) && !isTRUE(my_moderated)) next
    xm_w <- mediation_moderation_path_moderators(moderation_map, "xm", focal = focal, mediator = mediator, w = w, moderated = xm_moderated)
    my_w <- mediation_moderation_path_moderators(moderation_map, "my", mediator = mediator, outcome = outcome, w = w, moderated = my_moderated)
    path_w <- unique(c(xm_w, my_w))
    if (length(path_w) == 0L) next
    a0 <- if (focal %in% as.character(x_to_m[[mediator]] %||% character(0))) coef_a(mediator) else NA_real_
    b0 <- coef_b(mediator)
    condition_columns <- intersect(c(path_w, paste0(".level_", path_w)), names(conditional_grid))
    mediator_grid <- unique(conditional_grid[, condition_columns, drop = FALSE])
    condition_effect_values <- c()
    use_slope_callbacks <- (is.function(coef_a_at) || is.function(coef_b_at)) &&
      any(vapply(path_w, function(moderator) {
        moderator %in% names(mediator_grid) && !is.numeric(mediator_grid[[moderator]])
      }, logical(1)))
    if (isTRUE(use_slope_callbacks)) {
      for (row_index in seq_len(nrow(mediator_grid))) {
        conditions <- mediation_moderation_condition_row(mediator_grid[row_index, , drop = FALSE], path_w)
        condition <- paste(vapply(path_w, function(moderator) {
          level_column <- paste0(".level_", moderator)
          level <- as.character(mediator_grid[[level_column]][[row_index]] %||% "")
          symbol_index <- match(moderator, w)
          symbol <- if (is.na(symbol_index)) moderator else c("W", "Z")[[symbol_index]]
          sprintf("%s %s", symbol, level)
        }, character(1)), collapse = "; ")
        a_value <- if (length(xm_w) > 0L && is.function(coef_a_at)) {
          coef_a_at(mediator, conditions)
        } else {
          a0
        }
        b_value <- if (length(my_w) > 0L && is.function(coef_b_at)) {
          coef_b_at(mediator, conditions)
        } else {
          b0
        }
        effect_name <- sprintf(
          "Conditional indirect: X -> %s -> Y | %s",
          mediator,
          condition
        )
        effects[[effect_name]] <- a_value * b_value
        condition_effect_values[[condition]] <- effects[[effect_name]]
      }
      first_condition <- names(condition_effect_values)[[1L]] %||% ""
      if (length(condition_effect_values) >= 2L && nzchar(first_condition)) {
        first_value <- condition_effect_values[[first_condition]]
        for (condition in names(condition_effect_values)[-1L]) {
          effect_name <- sprintf(
            "Relative indirect: X -> %s -> Y | %s vs %s",
            mediator,
            condition,
            first_condition
          )
          effects[[effect_name]] <- condition_effect_values[[condition]] - first_value
        }
      }
      next
    }
    a1 <- stats::setNames(rep(0, length(path_w)), path_w)
    b1 <- stats::setNames(rep(0, length(path_w)), path_w)
    if (length(xm_w) > 0L) {
      a1[xm_w] <- vapply(xm_w, function(moderator) coef_a_interaction(mediator, moderator), numeric(1))
      a1[!is.finite(a1)] <- 0
    }
    if (length(my_w) > 0L) {
      b1[my_w] <- vapply(my_w, function(moderator) coef_b_interaction(mediator, moderator), numeric(1))
      b1[!is.finite(b1)] <- 0
    }
    a_wz <- 0
    b_wz <- 0
    if (length(xm_w) >= 2L && is.function(coef_a_three_way)) {
      a_wz <- coef_a_three_way(mediator, xm_w[[1L]], xm_w[[2L]])
      if (!is.finite(a_wz)) a_wz <- 0
    }
    if (length(my_w) >= 2L && is.function(coef_b_three_way)) {
      b_wz <- coef_b_three_way(mediator, my_w[[1L]], my_w[[2L]])
      if (!is.finite(b_wz)) b_wz <- 0
    }
    for (row_index in seq_len(nrow(mediator_grid))) {
      moderator_values <- mediation_moderation_numeric_match(mediator_grid[row_index, path_w, drop = FALSE], path_w)
      moderator_product <- if (length(moderator_values) >= 2L) prod(moderator_values[seq_len(2L)]) else 0
      condition <- paste(vapply(path_w, function(moderator) {
        level_column <- paste0(".level_", moderator)
        level <- as.character(mediator_grid[[level_column]][[row_index]] %||% "")
        symbol_index <- match(moderator, w)
        symbol <- if (is.na(symbol_index)) moderator else c("W", "Z")[[symbol_index]]
        sprintf("%s %s", symbol, level)
      }, character(1)), collapse = "; ")
      effect_name <- sprintf(
        "Conditional indirect: X -> %s -> Y | %s",
        mediator,
        condition
      )
      effects[[effect_name]] <- (a0 + sum(a1 * moderator_values, na.rm = TRUE) + a_wz * moderator_product) *
        (b0 + sum(b1 * moderator_values, na.rm = TRUE) + b_wz * moderator_product)
    }
    if (length(path_w) == 1L && xor(length(xm_w) > 0L, length(my_w) > 0L)) {
      index_name <- sprintf("Index of moderated mediation: X -> %s -> Y", mediator)
      effects[[index_name]] <- if (length(xm_w) > 0L) a1[[1L]] * b0 else a0 * b1[[1L]]
    }
  }
  effects
}

mediation_moderation_mediator_slots <- function(count) {
  count <- max(1L, as.integer(count %||% 1L))
  if (count == 1L) "m" else paste0("m", seq_len(count))
}

mediation_moderation_mediator_top_y <- function() 42

mediation_moderation_moderator_top_y <- function() 22

mediation_moderation_lerp_point <- function(from, to, amount = 0.5) {
  from <- mediation_moderation_numeric_vector(from)
  to <- mediation_moderation_numeric_vector(to)
  from + (to - from) * amount
}

mediation_moderation_spread_xy_positions <- function(positions, amount = 4) {
  if ("x" %in% names(positions)) {
    positions$x[[1]] <- max(10, positions$x[[1]] - amount)
  }
  if ("y" %in% names(positions)) {
    positions$y[[1]] <- min(90, positions$y[[1]] + amount)
  }
  positions
}

mediation_moderation_first_mediator_slot <- function(positions) {
  mediator_slots <- names(positions)[grepl("^m[0-9]*$", names(positions))]
  mediator_slots <- mediator_slots[mediator_slots != ""]
  if ("m" %in% mediator_slots) return("m")
  if ("m1" %in% mediator_slots) return("m1")
  utils::head(mediator_slots, 1)
}

mediation_moderation_anchor_model <- function(spec) {
  model <- as.character(spec$model %||% NA_character_)
  if (length(model) == 1L && !is.na(model) && nzchar(model)) {
    return(model)
  }
  moderated_paths <- sort(intersect(as.character(spec$moderated_paths %||% character(0)), c("xm", "my", "xy")))
  key <- paste(moderated_paths, collapse = "+")
  if (identical(key, "my+xm")) return("58")
  if (identical(key, "my+xm+xy")) return("59")
  NA_character_
}

mediation_moderation_xm_anchor_amount <- function(mediator_slot, positions, anchor_model = NA_character_) {
  0.5
}

mediation_moderation_my_anchor_amount <- function(mediator_slot, positions, anchor_model = NA_character_) {
  0.5
}

mediation_moderation_moderator_position <- function(positions, moderated_paths = character(0)) {
  moderated_paths <- intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy"))
  if (length(moderated_paths) == 0 || !"x" %in% names(positions) || !"y" %in% names(positions)) {
    return(positions$w %||% c(50, 12))
  }
  first_m <- mediation_moderation_first_mediator_slot(positions)
  mediator_slots <- names(positions)[grepl("^m[0-9]*$", names(positions))]
  mediator_slots <- mediator_slots[mediator_slots != ""]
  has_mediator <- length(first_m) == 1L && first_m %in% names(positions)
  mediator_y <- if (length(mediator_slots) > 0L) {
    vapply(mediator_slots, function(slot) positions[[slot]][[2]], numeric(1))
  } else if (has_mediator) {
    positions[[first_m]][[2]]
  } else {
    numeric(0)
  }
  top_mediator_y <- if (length(mediator_y) > 0L) min(mediator_y) else min(positions$x[[2]], positions$y[[2]]) - 35
  direct_y <- mean(c(positions$x[[2]], positions$y[[2]]))
  point <- positions$w %||% c(50, 12)
  if (all(c("xm", "my", "xy") %in% moderated_paths) && has_mediator) {
    point <- c(mean(c(positions$x[[1]], positions$y[[1]])), mediation_moderation_moderator_top_y())
  } else if (all(c("xm", "my") %in% moderated_paths) && has_mediator) {
    point <- c(mean(c(positions$x[[1]], positions$y[[1]])), mediation_moderation_moderator_top_y())
  } else if ("xm" %in% moderated_paths && has_mediator) {
    first_m_x <- if (first_m %in% names(positions)) positions[[first_m]][[1]] else 50
    point <- c(mean(c(positions$x[[1]], first_m_x)), mediation_moderation_moderator_top_y())
  } else if ("my" %in% moderated_paths && has_mediator) {
    first_m_x <- if (first_m %in% names(positions)) positions[[first_m]][[1]] else 50
    point <- c(mean(c(first_m_x, positions$y[[1]])), mediation_moderation_moderator_top_y())
  } else if ("xy" %in% moderated_paths && has_mediator) {
    first_m_x <- if (first_m %in% names(positions)) positions[[first_m]][[1]] else 50
    point <- c(mean(c(positions$x[[1]], first_m_x)), mediation_moderation_moderator_top_y())
  } else if ("xy" %in% moderated_paths) {
    point <- c(mean(c(positions$x[[1]], positions$y[[1]])), direct_y - 32)
  }
  c(
    min(86, max(12, point[[1]])),
    min(88, max(16, point[[2]]))
  )
}

mediation_moderation_add_dynamic_moderators <- function(positions, moderated_paths = character(0), moderator_count = 1L) {
  moderator_count <- max(1L, min(2L, as.integer(moderator_count %||% 1L)))
  positions$w <- mediation_moderation_moderator_position(positions, moderated_paths)
  if (moderator_count >= 2L) {
    base <- mediation_moderation_numeric_vector(positions$w)
    positions$w <- c(max(12, base[[1]] - 8), base[[2]])
    positions$z <- c(min(88, base[[1]] + 8), base[[2]])
  }
  positions
}

mediation_moderation_dynamic_positions <- function(structure, mediator_count = 1L, moderated_paths = character(0), moderator_count = 1L) {
  mediator_count <- max(1L, as.integer(mediator_count %||% 1L))
  if (identical(structure, "none")) {
    positions <- list(x = c(20, 67), y = c(80, 67), w = c(50, 34))
    return(mediation_moderation_add_dynamic_moderators(positions, moderated_paths, moderator_count))
  }
  if (identical(structure, "parallel")) {
    slots <- mediation_moderation_mediator_slots(mediator_count)
    if (mediator_count == 1L) {
      mediator_y <- mediation_moderation_mediator_top_y()
    } else {
      above_count <- ceiling(mediator_count / 2)
      below_count <- mediator_count - above_count
      if (mediator_count == 2L) {
        above_y <- mediation_moderation_mediator_top_y() - 4
        below_y <- mediation_moderation_mediator_top_y() + 40
      } else {
        above_y <- if (above_count == 1L) mediation_moderation_mediator_top_y() else seq(30, mediation_moderation_mediator_top_y(), length.out = above_count)
        below_y <- if (below_count == 0L) numeric(0) else if (below_count == 1L) 78 else seq(74, 90, length.out = below_count)
      }
      mediator_y <- c(above_y, below_y)
    }
    positions <- list(x = c(20, 58), y = c(80, 58), w = c(50, 12))
    for (index in seq_along(slots)) {
      positions[[slots[[index]]]] <- c(50, mediator_y[[index]])
    }
    return(mediation_moderation_add_dynamic_moderators(positions, moderated_paths, moderator_count))
  }
  if (identical(structure, "serial")) {
    slots <- mediation_moderation_mediator_slots(mediator_count)
    mediator_x <- if (mediator_count == 1L) 50 else seq(34, 66, length.out = mediator_count)
    positions <- list(x = c(20, 72), y = c(80, 72), w = c(50, 12))
    for (index in seq_along(slots)) {
      positions[[slots[[index]]]] <- c(mediator_x[[index]], mediation_moderation_mediator_top_y())
    }
    return(mediation_moderation_add_dynamic_moderators(positions, moderated_paths, moderator_count))
  }
  positions <- list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72), w = c(50, 18))
  mediation_moderation_add_dynamic_moderators(positions, moderated_paths, moderator_count)
}

mediation_moderation_dynamic_paths <- function(structure, mediator_count = 1L, moderated_paths = character(0), moderator_count = 1L) {
  mediator_count <- max(1L, as.integer(mediator_count %||% 1L))
  moderator_sources <- c("w", if (as.integer(moderator_count %||% 1L) >= 2L) "z")
  paths <- list()
  if (identical(structure, "none")) {
    paths <- list(c("x", "y"))
  } else {
    slots <- mediation_moderation_mediator_slots(mediator_count)
    if (identical(structure, "serial")) {
      for (slot in slots) paths[[length(paths) + 1L]] <- c("x", slot)
      if (length(slots) > 1L) {
        for (from_index in seq_len(length(slots) - 1L)) {
          for (to_index in seq(from_index + 1L, length(slots))) {
            paths[[length(paths) + 1L]] <- c(slots[[from_index]], slots[[to_index]])
          }
        }
      }
      for (slot in slots) paths[[length(paths) + 1L]] <- c(slot, "y")
    } else {
      for (slot in slots) paths[[length(paths) + 1L]] <- c("x", slot)
      for (slot in slots) paths[[length(paths) + 1L]] <- c(slot, "y")
    }
    paths[[length(paths) + 1L]] <- c("x", "y")
  }

  if ("xm" %in% moderated_paths && !identical(structure, "none")) {
    for (source in moderator_sources) for (slot in slots) paths[[length(paths) + 1L]] <- c(source, paste0("xm_", slot))
  }
  if ("my" %in% moderated_paths && !identical(structure, "none")) {
    for (source in moderator_sources) for (slot in slots) paths[[length(paths) + 1L]] <- c(source, paste0("my_", slot))
  }
  if ("xy" %in% moderated_paths) {
    for (source in moderator_sources) paths[[length(paths) + 1L]] <- c(source, "xy")
  }
  paths
}

mediation_moderation_builder_spec <- function(
  structure,
  moderated_paths,
  mediator_count = 1L,
  moderator_count = NULL,
  two_moderator_model = "3",
  language = statedu_initial_language()
) {
  structure <- mediation_moderation_scalar_choice(structure, "single", c("none", "single", "parallel", "serial"))
  mediator_count <- if (identical(structure, "none")) 0L else max(1L, as.integer(mediator_count %||% 1L))
  moderator_count <- suppressWarnings(as.integer(moderator_count %||% if (length(moderated_paths) > 0L) 1L else 0L))
  if (is.na(moderator_count)) moderator_count <- 0L
  moderator_count <- max(0L, min(2L, moderator_count))
  moderated_paths <- mediation_moderation_default_moderated_paths(
    list(mm_moderated_paths = moderated_paths),
    structure = structure
  )
  model <- mediation_moderation_infer_model(
    structure,
    moderated_paths,
    moderator_count = moderator_count,
    two_moderator_model = two_moderator_model
  )
  recognized <- !is.na(model) && model %in% mediation_moderation_models()
  if (!identical(structure, "none") && moderator_count >= 2L && length(moderated_paths) > 0L) {
    recognized <- FALSE
  }
  moderator_slots <- if (length(moderated_paths) > 0L) c("w", if (moderator_count >= 2L) "z") else character(0)

  if (identical(structure, "parallel")) {
    slots <- c("x", mediation_moderation_mediator_slots(mediator_count), "y", moderator_slots)
    title <- if (isTRUE(recognized)) {
      if (identical(model, "4") && mediator_count > 1L) {
        mediation_moderation_text(language, "Model 4: parallel multiple mediation", "Model 4: \ubcd1\ub82c \ubcf5\uc218\ub9e4\uac1c")
      } else {
        mediation_moderation_model_label(model, language)
      }
    } else {
      mediation_moderation_text(language, "Custom model", "\uc0ac\uc6a9\uc790\uc815\uc758 \ubaa8\ud615")
    }
    return(list(
      model = model,
      recognized = isTRUE(recognized),
      title = title,
      slots = slots,
      positions = mediation_moderation_dynamic_positions("parallel", mediator_count, moderated_paths, moderator_count),
      paths = mediation_moderation_dynamic_paths("parallel", mediator_count, moderated_paths, moderator_count),
      moderated_paths = moderated_paths,
      structure = structure
    ))
  }

  if (identical(structure, "serial") && mediator_count != 2L) {
    slots <- c("x", mediation_moderation_mediator_slots(mediator_count), "y", moderator_slots)
    return(list(
      model = model,
      recognized = isTRUE(recognized),
      title = if (isTRUE(recognized)) mediation_moderation_model_label(model, language) else mediation_moderation_text(language, "Custom model", "\uc0ac\uc6a9\uc790\uc815\uc758 \ubaa8\ud615"),
      slots = slots,
      positions = mediation_moderation_dynamic_positions("serial", mediator_count, moderated_paths, moderator_count),
      paths = mediation_moderation_dynamic_paths("serial", mediator_count, moderated_paths, moderator_count),
      moderated_paths = moderated_paths,
      structure = structure
    ))
  }

  if (isTRUE(recognized)) {
    return(list(
      model = model,
      recognized = TRUE,
      title = mediation_moderation_model_label(model, language),
      slots = mediation_moderation_slots(model),
      positions = mediation_moderation_node_positions(model),
      paths = mediation_moderation_paths(model),
      moderated_paths = moderated_paths,
      structure = structure
    ))
  }

  has_w <- length(moderated_paths) > 0
  if (identical(structure, "none")) {
    slots <- c("x", "y", moderator_slots)
    positions <- mediation_moderation_dynamic_positions("none", 1L, moderated_paths, moderator_count)
    paths <- list(c("x", "y"))
  } else if (identical(structure, "serial")) {
    slots <- c("x", "m1", "m2", "y", moderator_slots)
    positions <- mediation_moderation_dynamic_positions("serial", 2L, moderated_paths, moderator_count)
    paths <- list(c("x", "m1"), c("m1", "m2"), c("m2", "y"), c("x", "m2"), c("m1", "y"), c("x", "y"))
  } else {
    slots <- c("x", "m", "y", moderator_slots)
    positions <- mediation_moderation_dynamic_positions("single", 1L, moderated_paths, moderator_count)
    paths <- list(c("x", "m"), c("m", "y"), c("x", "y"))
  }

  if ("xm" %in% moderated_paths) for (source in moderator_slots) paths[[length(paths) + 1L]] <- c(source, "xm")
  if ("my" %in% moderated_paths) for (source in moderator_slots) paths[[length(paths) + 1L]] <- c(source, "my")
  if ("xy" %in% moderated_paths) for (source in moderator_slots) paths[[length(paths) + 1L]] <- c(source, "xy")

  list(
    model = NA_character_,
    recognized = FALSE,
    title = mediation_moderation_text(language, "Custom model", "\uc0ac\uc6a9\uc790\uc815\uc758 \ubaa8\ud615"),
    slots = slots,
    positions = positions,
    paths = paths,
    moderated_paths = moderated_paths,
    structure = structure
  )
}

mediation_moderation_slots <- function(model) {
  model <- mediation_moderation_scalar_choice(model, "4")
  switch(
    model,
    "1" = c("x", "y", "w"),
    "2" = c("x", "y", "w", "z"),
    "3" = c("x", "y", "w", "z"),
    "6" = c("x", "m1", "m2", "y"),
    c("x", "m", "y", if (model %in% c("5", "7", "8", "14", "15", "58", "59")) "w")
  )
}

mediation_moderation_node_positions <- function(model) {
  model <- mediation_moderation_scalar_choice(model, "4")
  if (model %in% c("2", "3")) {
    return(list(x = c(20, 70), w = c(42, 32), z = c(58, 32), y = c(80, 70)))
  }
  if (identical(model, "1")) {
    return(list(x = c(20, 67), w = c(50, 34), y = c(80, 67)))
  }
  if (identical(model, "6")) {
    return(list(x = c(20, 72), m1 = c(38, mediation_moderation_mediator_top_y()), m2 = c(62, mediation_moderation_mediator_top_y()), y = c(80, 72)))
  }
  if (model %in% c("14", "15")) {
    return(list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), w = c(80, 36), y = c(80, 72)))
  }
  if (model %in% c("58")) {
    return(list(x = c(20, 72), w = c(50, mediation_moderation_moderator_top_y()), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72)))
  }
  if (model %in% c("59")) {
    return(list(x = c(20, 70), w = c(50, mediation_moderation_moderator_top_y()), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 70)))
  }
  if (model %in% c("7", "8")) {
    return(list(x = c(20, 70), w = c(20, 36), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 70)))
  }
  if (identical(model, "5")) {
    return(list(x = c(20, 70), w = c(20, 36), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 70)))
  }
  list(x = c(20, 72), m = c(50, mediation_moderation_mediator_top_y()), y = c(80, 72))
}

mediation_moderation_paths <- function(model) {
  model <- mediation_moderation_scalar_choice(model, "4")
  switch(
    model,
    "1" = list(c("x", "y"), c("w", "xy")),
    "2" = list(c("x", "y"), c("w", "xy"), c("z", "xy")),
    "3" = list(c("x", "y"), c("w", "xy"), c("z", "xy")),
    "4" = list(c("x", "m"), c("m", "y"), c("x", "y")),
    "5" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xy")),
    "6" = list(c("x", "m1"), c("m1", "m2"), c("m2", "y"), c("x", "m2"), c("m1", "y"), c("x", "y")),
    "7" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm")),
    "8" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm"), c("w", "xy")),
    "14" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "my")),
    "15" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "my"), c("w", "xy")),
    "58" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm"), c("w", "my")),
    "59" = list(c("x", "m"), c("m", "y"), c("x", "y"), c("w", "xm"), c("w", "my"), c("w", "xy")),
    list(c("x", "m"), c("m", "y"), c("x", "y"))
  )
}

mediation_moderation_edge_point <- function(source, target, positions, anchor_model = NA_character_) {
  if (target %in% names(positions)) {
    return(positions[[target]])
  }
  if (grepl("^xy_", target) && "y" %in% names(positions)) {
    parts <- strsplit(target, "_", fixed = TRUE)[[1]]
    if (length(parts) == 2L && parts[[2L]] %in% names(positions)) {
      amount <- if (identical(parts[[2L]], "x1")) 0.5 else if (identical(parts[[2L]], "x2")) 0.25 else 0.5
      return(mediation_moderation_lerp_point(positions[[parts[[2L]]]], positions$y, amount))
    }
  }
  if (grepl("^xm_[^_]+_[^_]+$", target)) {
    parts <- strsplit(target, "_", fixed = TRUE)[[1]]
    if (length(parts) == 3L && all(parts[2:3] %in% names(positions))) {
      amount <- mediation_moderation_xm_anchor_amount(parts[[3L]], positions, anchor_model)
      if (!"x" %in% names(positions)) {
        amount <- 0.3
      }
      return(mediation_moderation_lerp_point(positions[[parts[[2L]]]], positions[[parts[[3L]]]], amount))
    }
  }
  if (identical(target, "xy") && all(c("x", "y") %in% names(positions))) {
    return(mediation_moderation_lerp_point(positions$x, positions$y, 0.5))
  }
  if (grepl("^xm_", target)) {
    mediator_slot <- sub("^xm_", "", target)
    if (all(c("x", mediator_slot) %in% names(positions))) {
      amount <- mediation_moderation_xm_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions$x, positions[[mediator_slot]], amount))
    }
  }
  if (grepl("^my_", target)) {
    mediator_slot <- sub("^my_", "", target)
    if (all(c(mediator_slot, "y") %in% names(positions))) {
      amount <- mediation_moderation_my_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions[[mediator_slot]], positions$y, amount))
    }
  }
  if (identical(target, "xm") && "x" %in% names(positions)) {
    mediator_slot <- mediation_moderation_first_mediator_slot(positions)
    if (length(mediator_slot) == 1L && mediator_slot %in% names(positions)) {
      amount <- mediation_moderation_xm_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions$x, positions[[mediator_slot]], amount))
    }
  }
  if (identical(target, "my") && "y" %in% names(positions)) {
    mediator_slot <- mediation_moderation_first_mediator_slot(positions)
    if (length(mediator_slot) == 1L && mediator_slot %in% names(positions)) {
      amount <- mediation_moderation_my_anchor_amount(mediator_slot, positions, anchor_model)
      return(mediation_moderation_lerp_point(positions[[mediator_slot]], positions$y, amount))
    }
  }
  positions[[source]]
}

mediation_moderation_moderation_edge_point <- function(source, target, positions, anchor_model = NA_character_) {
  point <- mediation_moderation_edge_point(source, target, positions, anchor_model)
  source <- as.character(source %||% "")
  target <- as.character(target %||% "")
  if (!source %in% c("w", "z") || !is.numeric(point) || length(point) != 2L) {
    return(point)
  }
  path_from <- NULL
  path_to <- NULL
  if (grepl("^xm_[^_]+_[^_]+$", target)) {
    parts <- strsplit(target, "_", fixed = TRUE)[[1]]
    if (length(parts) == 3L && all(parts[2:3] %in% names(positions))) {
      path_from <- positions[[parts[[2L]]]]
      path_to <- positions[[parts[[3L]]]]
    }
  } else if (grepl("^xm_", target)) {
    mediator_slot <- sub("^xm_", "", target)
    if (all(c("x", mediator_slot) %in% names(positions))) {
      path_from <- positions$x
      path_to <- positions[[mediator_slot]]
    }
  } else if (grepl("^my_", target)) {
    mediator_slot <- sub("^my_", "", target)
    if (all(c(mediator_slot, "y") %in% names(positions))) {
      path_from <- positions[[mediator_slot]]
      path_to <- positions$y
    }
  } else if (grepl("^xy", target) && all(c("x", "y") %in% names(positions))) {
    path_from <- positions$x
    path_to <- positions$y
  }
  mediator_slots <- names(positions)[grepl("^m[0-9]*$", names(positions))]
  mediator_slots <- mediator_slots[mediator_slots != ""]
  mediator_slot <- ""
  if (grepl("^xm_[^_]+_[^_]+$", target)) {
    parts <- strsplit(target, "_", fixed = TRUE)[[1]]
    mediator_slot <- parts[[3L]]
  } else if (grepl("^xm_", target) || grepl("^my_", target)) {
    mediator_slot <- sub("^(xm|my)_", "", target)
  }
  index <- match(mediator_slot, mediator_slots)
  if (!is.na(index) && length(mediator_slots) >= 2L) {
    offset <- (index - ((length(mediator_slots) + 1) / 2)) * 4.2
    point[[1]] <- max(8, min(92, point[[1]] + offset))
  }
  if (!is.null(path_from) && !is.null(path_to) && "w" %in% names(positions)) {
    direction <- mediation_moderation_numeric_vector(path_to) - mediation_moderation_numeric_vector(path_from)
    direction_norm <- sqrt(sum(direction^2))
    if (is.finite(direction_norm) && direction_norm > 0) {
      unit <- direction / direction_norm
      toward_moderator <- mediation_moderation_numeric_vector(positions[[source]]) - mediation_moderation_numeric_vector(point)
      normal_component <- toward_moderator - unit * sum(toward_moderator * unit)
      normal_norm <- sqrt(sum(normal_component^2))
      if (is.finite(normal_norm) && normal_norm > 0) {
        point <- mediation_moderation_numeric_vector(point) + (normal_component / normal_norm) * 2.6
      }
    }
  }
  point[[1]] <- max(8, min(92, point[[1]]))
  point[[2]] <- max(8, min(92, point[[2]]))
  point
}

mediation_moderation_diagram_metrics <- function(variant = "setup") {
  variant <- as.character(variant %||% "setup")[[1]]
  if (identical(variant, "result")) {
    return(list(half_width = 7.0, half_height = 5.0, gap = 1.5))
  }
  list(half_width = 8.0, half_height = 4.7, gap = 0.8)
}

mediation_moderation_node_arrow_endpoint <- function(from, to, metrics = mediation_moderation_diagram_metrics()) {
  from <- mediation_moderation_numeric_vector(from)
  to <- mediation_moderation_numeric_vector(to)
  delta <- to - from
  if (length(delta) != 2L || !all(is.finite(delta)) || sum(abs(delta)) == 0) {
    return(to)
  }
  half_width <- mediation_moderation_numeric_scalar(metrics$half_width, 8.0)
  half_height <- mediation_moderation_numeric_scalar(metrics$half_height, 4.7)
  gap <- mediation_moderation_numeric_scalar(metrics$gap, 0.8)
  scale <- max(abs(delta[[1]]) / half_width, abs(delta[[2]]) / half_height)
  if (!is.finite(scale) || scale <= 0) {
    return(to)
  }
  edge <- to - (delta / scale)
  unit <- delta / sqrt(sum(delta^2))
  edge - unit * gap
}

mediation_moderation_arrowhead_polygon <- function(from, to, metrics = mediation_moderation_diagram_metrics()) {
  from <- mediation_moderation_numeric_vector(from)
  to <- mediation_moderation_numeric_vector(to)
  delta <- to - from
  if (length(delta) != 2L || !all(is.finite(delta)) || sum(abs(delta)) == 0) {
    return(NULL)
  }
  unit <- delta / sqrt(sum(delta^2))
  normal <- c(-unit[[2]], unit[[1]])
  arrow_length <- if (mediation_moderation_numeric_scalar(metrics$half_height, 4.7) < 4) 1.9 else 2.3
  arrow_width <- arrow_length * 0.82
  base <- to - unit * arrow_length
  left <- base + normal * (arrow_width / 2)
  right <- base - normal * (arrow_width / 2)
  p1 <- to
  p2 <- left
  p3 <- right
  tags$polygon(
    points = sprintf(
      "%.3f,%.3f %.3f,%.3f %.3f,%.3f",
      p1[[1]], p1[[2]],
      p2[[1]], p2[[2]],
      p3[[1]], p3[[2]]
    ),
    class = "mm-diagram-arrowhead-shape"
  )
}

mediation_moderation_arrow <- function(path, positions, anchor_model = NA_character_, metrics = mediation_moderation_diagram_metrics(), edge_significance = NULL) {
  source <- path[[1]]
  target <- path[[2]]
  from <- positions[[source]]
  to <- mediation_moderation_moderation_edge_point(source, target, positions, anchor_model)
  if (target %in% names(positions)) {
    to <- mediation_moderation_node_arrow_endpoint(from, to, metrics)
  }
  if (source %in% names(positions)) {
    from <- mediation_moderation_node_arrow_endpoint(to, from, metrics)
  }
  key <- mediation_moderation_path_key(path)
  significant <- edge_significance[[key]] %||% TRUE
  tagList(
    tags$line(
      x1 = from[[1]], y1 = from[[2]],
      x2 = to[[1]], y2 = to[[2]],
      class = paste("mm-diagram-arrow", if (!isTRUE(significant)) "mm-diagram-arrow-nonsignificant" else "")
    ),
    mediation_moderation_arrowhead_polygon(from, to, metrics)
  )
}

mediation_moderation_path_key <- function(path) {
  paste(as.character(path %||% character(0)), collapse = "->")
}

mediation_moderation_arrow_label_amount <- function(path) {
  target <- as.character(path[[2]] %||% "")
  source <- as.character(path[[1]] %||% "")
  if (target %in% c("xm", "my", "xy") || grepl("^(xm|my|xy)_", target)) {
    return(0.34)
  }
  if (grepl("^m[0-9]*$", source) && identical(target, "y")) {
    return(0.35)
  }
  if ((identical(source, "x") || grepl("^x[0-9]+$", source) || grepl("^m[0-9]*$", source)) && (grepl("^m[0-9]*$", target) || identical(target, "y"))) {
    return(0.42)
  }
  0.5
}

mediation_moderation_arrow_label <- function(path, edge_labels, positions, anchor_model = NA_character_, metrics = mediation_moderation_diagram_metrics()) {
  key <- mediation_moderation_path_key(path)
  label <- as.character(edge_labels[[key]] %||% "")
  if (!nzchar(label)) {
    return(NULL)
  }
  source <- path[[1]]
  target <- path[[2]]
  from <- positions[[source]]
  to <- mediation_moderation_moderation_edge_point(source, target, positions, anchor_model)
  if (target %in% names(positions)) {
    to <- mediation_moderation_node_arrow_endpoint(from, to, metrics)
  }
  if (source %in% names(positions)) {
    from <- mediation_moderation_node_arrow_endpoint(to, from, metrics)
  }
  amount <- mediation_moderation_arrow_label_amount(path)
  is_my_path <- grepl("^m[0-9]*$", source) && identical(target, "y")
  label_point <- mediation_moderation_lerp_point(from, to, amount)
  x <- label_point[[1]]
  y <- label_point[[2]]
  if (identical(target, "xy") || grepl("^xy", target)) {
    y <- y - 3.2
  } else if (target %in% names(positions)) {
    y <- y - 2.2
  } else {
    y <- y + 2.4
  }
  tags$g(
    class = paste("mm-diagram-edge-label", if (isTRUE(is_my_path)) "mm-diagram-edge-label-my" else ""),
    tags$text(x = x, y = y, class = "mm-diagram-edge-label-halo", label),
    tags$text(x = x, y = y, class = "mm-diagram-edge-label-text", label)
  )
}

mediation_moderation_slot_label <- function(slot) {
  slot <- as.character(slot %||% "")
  if (grepl("^m[0-9]+$", slot)) {
    return(toupper(slot))
  }
  switch(
    slot,
    x = "X",
    y = "Y",
    m = "M",
    m1 = "M1",
    m2 = "M2",
    w = "W",
    z = "Z",
    toupper(slot)
  )
}

mediation_moderation_slot_input_id <- function(slot) {
  paste0("mm_", slot)
}

mediation_moderation_display_name <- function(name, variable_table = NULL, labels = character(0)) {
  name <- mediation_moderation_scalar_choice(name, "")
  if (!nzchar(name)) return("-")
  display_variable_name_static(name, variable_table, labels, label_only = TRUE)
}

mediation_moderation_slot_variable <- function(slot, roles) {
  slot <- as.character(slot %||% "")
  slot_variables <- roles$slot_variables %||% NULL
  if (!is.null(slot_variables) && slot %in% names(slot_variables)) {
    return(as.character(slot_variables[[slot]] %||% character(0)))
  }
  if (grepl("^m[0-9]+$", slot)) {
    index <- suppressWarnings(as.integer(sub("^m", "", slot)))
    return(as.character(roles$mediators %||% character(0))[index])
  }
  switch(
    slot,
    x = utils::head(as.character(roles$x %||% character(0)), 1),
    y = utils::head(as.character(roles$y %||% character(0)), 1),
    m = utils::head(as.character(roles$mediators %||% character(0)), 1),
    m1 = utils::head(as.character(roles$mediators %||% character(0)), 1),
    m2 = utils::head(as.character(roles$mediators %||% character(0)), 2)[2],
    w = utils::head(as.character(roles$w %||% character(0)), 1),
    z = utils::head(as.character(roles$w %||% character(0)), 2)[2],
    character(0)
  )
}

mediation_moderation_node <- function(slot, position, roles, variable_table = NULL, labels = character(0)) {
  variable <- mediation_moderation_slot_variable(slot, roles)
  has_variable <- length(variable) > 0L && nzchar(as.character(variable[[1]] %||% ""))
  div(
    class = paste("mm-diagram-node", if (isTRUE(has_variable)) "mm-node-assigned" else "mm-node-empty"),
    style = sprintf("left:%s%%;top:%s%%;", position[[1]], position[[2]]),
    if (isTRUE(has_variable)) {
      div(class = "mm-node-variable-text", mediation_moderation_display_name(variable, variable_table, labels))
    } else {
      div(class = "mm-node-role", mediation_moderation_slot_label(slot))
    }
  )
}

mediation_moderation_diagram <- function(spec, roles, variable_table = NULL, labels = character(0), language = statedu_initial_language(), edge_labels = NULL, edge_significance = NULL, variant = "setup") {
  slots <- spec$slots
  positions <- mediation_moderation_spread_xy_positions(spec$positions)
  paths <- spec$paths
  anchor_model <- mediation_moderation_anchor_model(spec)
  edge_labels <- edge_labels %||% list()
  edge_significance <- edge_significance %||% list()
  variant <- mediation_moderation_scalar_choice(variant, "setup", c("setup", "result"))
  metrics <- mediation_moderation_diagram_metrics(variant)
  div(
    class = paste("mm-diagram-panel", paste0("mm-diagram-panel-", variant)),
    div(
      class = "mm-diagram-title",
      span(spec$title),
      if (!isTRUE(spec$recognized)) span(mediation_moderation_text(language, "User-defined", "\uc0ac\uc6a9\uc790\uc815\uc758"), class = "mm-model-custom-badge")
    ),
    tags$svg(
      class = "mm-diagram-svg",
      viewBox = "0 0 100 100",
      preserveAspectRatio = "none",
      tags$defs(
        tags$marker(
          id = "mm-arrowhead",
          markerWidth = "6",
          markerHeight = "6",
          refX = "5.5",
          refY = "3",
          orient = "auto",
          tags$path(d = "M0,0 L0,6 L5.5,3 z", class = "mm-diagram-arrowhead")
        )
      ),
      lapply(paths, mediation_moderation_arrow, positions = positions, anchor_model = anchor_model, metrics = metrics, edge_significance = edge_significance),
      lapply(paths, mediation_moderation_arrow_label, edge_labels = edge_labels, positions = positions, anchor_model = anchor_model, metrics = metrics)
    ),
    lapply(slots, function(slot) mediation_moderation_node(slot, positions[[slot]], roles, variable_table, labels))
  )
}

mediation_moderation_role_label <- function(role, language = statedu_initial_language()) {
  switch(
    role,
    variables = analysis_ui_text("Variables", language),
    y = analysis_ui_text("Dependent variable", language),
    x = analysis_ui_text("Independent variable", language),
    mediators = mediation_moderation_text(language, "Mediator variables", "\ub9e4\uac1c\ubcc0\uc218"),
    w = mediation_moderation_text(language, "Moderator variables", "\uc870\uc808\ubcc0\uc218"),
    covariates = analysis_ui_text("Covariates", language),
    role
  )
}

mediation_moderation_field_label_tag <- function(role, allowed_measurements = character(0), language = statedu_initial_language()) {
  allowed_measurements <- as.character(allowed_measurements %||% character(0))
  div(
    class = "analysis-field-label analysis-field-label-with-icons",
    span(mediation_moderation_role_label(role, language)),
    if (length(allowed_measurements) > 0) {
      span(class = "analysis-allowed-measurements", lapply(allowed_measurements, measurement_symbol_tag))
    }
  )
}

mediation_moderation_target_panel <- function(
  role,
  input_id,
  items,
  selected = character(0),
  size = 1,
  allowed_measurements = c("binary", "category", "ordered", "continuous"),
  language = statedu_initial_language(),
  order_buttons = FALSE,
  up_id = NULL,
  down_id = NULL
) {
  div(
    class = paste("mm-target-field mm-target-panel", paste0("mm-", role, "-panel")),
    mediation_moderation_field_label_tag(role, allowed_measurements, language),
    analysis_transfer_listbox_input(
      input_id,
      items = items,
      selected = selected,
      size = size,
      min_size = 1,
      height_offset = if (role %in% c("y", "w")) 10 else 0
    ),
    if (isTRUE(order_buttons)) {
      div(
        class = "mm-order-actions",
        actionButton(up_id, analysis_ui_text("Up", language), class = "btn-default btn-sm"),
        actionButton(down_id, analysis_ui_text("Down", language), class = "btn-default btn-sm")
      )
    }
  )
}

mediation_moderation_role_values <- function(y = character(0), x = character(0), mediators = character(0), w = character(0), covariates = character(0), selected_names = NULL) {
  clean <- function(values, max_n = Inf) {
    values <- unique(as.character(values %||% character(0)))
    values <- values[nzchar(values)]
    if (!is.null(selected_names)) values <- intersect(values, selected_names)
    if (is.finite(max_n)) values <- utils::head(values, max_n)
    values
  }
  list(
    y = clean(y, Inf),
    x = clean(x, Inf),
    mediators = clean(mediators, Inf),
    w = clean(w, 2),
    covariates = clean(covariates, Inf)
  )
}

mediation_moderation_covariate_control_values <- function(value = NULL) {
  value <- tolower(as.character(value %||% c("y", "m")))
  intersect(value[nzchar(value)], c("y", "m"))
}

mediation_moderation_normalize_outcome_map <- function(map = NULL, outcomes = character(0), allowed = character(0), default = allowed) {
  outcomes <- as.character(outcomes %||% character(0))
  outcomes <- outcomes[nzchar(outcomes)]
  allowed <- as.character(allowed %||% character(0))
  allowed <- allowed[nzchar(allowed)]
  default <- intersect(allowed, as.character(default %||% character(0)))
  if (length(outcomes) == 0L) {
    return(list())
  }
  if (is.null(map)) {
    return(stats::setNames(lapply(outcomes, function(outcome) default), outcomes))
  }
  if (is.data.frame(map) && all(c("y", "value") %in% names(map))) {
    return(stats::setNames(lapply(outcomes, function(outcome) {
      intersect(allowed, as.character(map$value[as.character(map$y) == outcome] %||% character(0)))
    }), outcomes))
  }
  if (is.data.frame(map) && all(c("y", "x") %in% names(map))) {
    return(stats::setNames(lapply(outcomes, function(outcome) {
      intersect(allowed, as.character(map$x[as.character(map$y) == outcome] %||% character(0)))
    }), outcomes))
  }
  if (is.data.frame(map) && all(c("y", "mediator") %in% names(map))) {
    return(stats::setNames(lapply(outcomes, function(outcome) {
      intersect(allowed, as.character(map$mediator[as.character(map$y) == outcome] %||% character(0)))
    }), outcomes))
  }
  if (is.data.frame(map)) {
    return(stats::setNames(lapply(outcomes, function(outcome) default), outcomes))
  }
  if (is.list(map) && !is.null(names(map))) {
    return(stats::setNames(lapply(outcomes, function(outcome) {
      intersect(allowed, as.character(map[[outcome]] %||% character(0)))
    }), outcomes))
  }
  values <- intersect(allowed, as.character(map %||% default))
  stats::setNames(lapply(outcomes, function(outcome) values), outcomes)
}

mediation_moderation_normalize_mediator_map <- function(map = NULL, mediators = character(0), default = character(0)) {
  mediators <- as.character(mediators %||% character(0))
  mediators <- mediators[nzchar(mediators)]
  default <- intersect(mediators, as.character(default %||% character(0)))
  if (length(mediators) == 0L) {
    return(list())
  }
  if (is.null(map)) {
    return(stats::setNames(lapply(mediators, function(mediator) default), mediators))
  }
  if (is.data.frame(map) && all(c("from", "to") %in% names(map))) {
    return(stats::setNames(lapply(mediators, function(mediator) {
      setdiff(intersect(mediators, as.character(map$from[as.character(map$to) == mediator] %||% character(0))), mediator)
    }), mediators))
  }
  if (is.data.frame(map)) {
    return(stats::setNames(lapply(mediators, function(mediator) setdiff(default, mediator)), mediators))
  }
  if (is.list(map) && !is.null(names(map))) {
    return(stats::setNames(lapply(mediators, function(mediator) {
      setdiff(intersect(mediators, as.character(map[[mediator]] %||% character(0))), mediator)
    }), mediators))
  }
  values <- intersect(mediators, as.character(map %||% default))
  stats::setNames(lapply(mediators, function(mediator) setdiff(values, mediator)), mediators)
}

mediation_moderation_focal_reaches_outcome <- function(focal, outcome, direct_x_to_y, x_to_m, m_to_y, m_to_m, mediators = character(0)) {
  focal <- as.character(focal %||% "")[[1]]
  outcome <- as.character(outcome %||% "")[[1]]
  mediators <- as.character(mediators %||% character(0))
  mediators <- mediators[nzchar(mediators)]
  if (!nzchar(focal) || !nzchar(outcome)) {
    return(FALSE)
  }
  if (focal %in% as.character(direct_x_to_y[[outcome]] %||% character(0))) {
    return(TRUE)
  }
  outcome_mediators <- intersect(mediators, as.character(m_to_y[[outcome]] %||% character(0)))
  if (length(outcome_mediators) == 0L) {
    return(FALSE)
  }
  current <- mediators[vapply(mediators, function(mediator) {
    focal %in% as.character(x_to_m[[mediator]] %||% character(0))
  }, logical(1))]
  seen <- character(0)
  while (length(current) > 0L) {
    mediator <- current[[1L]]
    current <- current[-1L]
    if (mediator %in% seen) next
    seen <- c(seen, mediator)
    if (mediator %in% outcome_mediators) {
      return(TRUE)
    }
    children <- mediators[vapply(mediators, function(candidate) {
      mediator %in% as.character(m_to_m[[candidate]] %||% character(0))
    }, logical(1))]
    current <- unique(c(current, setdiff(children, seen)))
  }
  FALSE
}

mediation_moderation_indirect_paths <- function(focal, outcome, x_to_m, m_to_y, m_to_m, mediators = character(0)) {
  focal <- as.character(focal %||% "")[[1]]
  outcome <- as.character(outcome %||% "")[[1]]
  mediators <- as.character(mediators %||% character(0))
  mediators <- mediators[nzchar(mediators)]
  outcome_mediators <- intersect(mediators, as.character(m_to_y[[outcome]] %||% character(0)))
  if (!nzchar(focal) || !nzchar(outcome) || length(outcome_mediators) == 0L) {
    return(list())
  }
  starts <- mediators[vapply(mediators, function(mediator) {
    focal %in% as.character(x_to_m[[mediator]] %||% character(0))
  }, logical(1))]
  paths <- list()
  walk <- function(path) {
    current <- path[[length(path)]]
    if (current %in% outcome_mediators) {
      paths[[length(paths) + 1L]] <<- path
    }
    children <- mediators[vapply(mediators, function(candidate) {
      current %in% as.character(m_to_m[[candidate]] %||% character(0))
    }, logical(1))]
    for (child in setdiff(children, path)) {
      walk(c(path, child))
    }
  }
  for (start in starts) {
    walk(start)
  }
  paths
}

mediation_moderation_indirect_path_name <- function(path) {
  sprintf("Indirect: X -> %s -> Y", paste(as.character(path), collapse = " -> "))
}

mediation_moderation_reachable_mediators <- function(focal, outcome, x_to_m, m_to_y, m_to_m, mediators = character(0)) {
  paths <- mediation_moderation_indirect_paths(focal, outcome, x_to_m, m_to_y, m_to_m, mediators)
  unique(unlist(paths, recursive = TRUE, use.names = FALSE))
}

mediation_moderation_normalize_moderated_x_to_m <- function(moderated_x_to_m = NULL, mediators = character(0), all_x = character(0), moderated_paths = character(0)) {
  mediators <- as.character(mediators %||% character(0))
  all_x <- as.character(all_x %||% character(0))
  all_x <- all_x[nzchar(all_x)]
  has_xm <- "xm" %in% as.character(moderated_paths %||% character(0))
  if (!isTRUE(has_xm)) {
    return(stats::setNames(lapply(mediators, function(mediator) character(0)), mediators))
  }
  default_x <- all_x
  if (is.null(moderated_x_to_m)) {
    moderated_x_to_m <- stats::setNames(lapply(mediators, function(mediator) default_x), mediators)
  }
  stats::setNames(lapply(mediators, function(mediator) {
    values <- as.character(moderated_x_to_m[[mediator]] %||% default_x)
    intersect(all_x, values[nzchar(values)])
  }), mediators)
}

mediation_moderation_normalize_moderated_m_to_y <- function(moderated_m_to_y = NULL, mediators = character(0), moderated_paths = character(0)) {
  mediators <- as.character(mediators %||% character(0))
  mediators <- mediators[nzchar(mediators)]
  has_my <- "my" %in% as.character(moderated_paths %||% character(0))
  if (!isTRUE(has_my)) {
    return(character(0))
  }
  if (is.null(moderated_m_to_y)) {
    return(mediators)
  }
  intersect(mediators, as.character(moderated_m_to_y %||% character(0)))
}

mediation_moderation_normalize_moderation_map <- function(moderation_map = NULL, roles = list()) {
  if (!is.data.frame(moderation_map) || nrow(moderation_map) == 0L) {
    return(NULL)
  }
  required <- c("path_type", "moderator", "x", "mediator", "y")
  for (column in required) {
    if (!column %in% names(moderation_map)) {
      moderation_map[[column]] <- ""
    }
  }
  map <- data.frame(
    path_type = as.character(moderation_map$path_type %||% ""),
    moderator = as.character(moderation_map$moderator %||% ""),
    x = as.character(moderation_map$x %||% ""),
    mediator = as.character(moderation_map$mediator %||% ""),
    y = as.character(moderation_map$y %||% ""),
    stringsAsFactors = FALSE
  )
  map <- map[map$path_type %in% c("xm", "my", "xy") & nzchar(map$moderator), , drop = FALSE]
  map <- map[map$moderator %in% as.character(roles$w %||% character(0)), , drop = FALSE]
  if (nrow(map) == 0L) {
    return(NULL)
  }
  map <- map[!duplicated(map), , drop = FALSE]
  rownames(map) <- NULL
  map
}

mediation_moderation_path_moderators <- function(
  moderation_map = NULL,
  path_type,
  focal = "",
  mediator = "",
  outcome = "",
  w = character(0),
  moderated = FALSE
) {
  w <- utils::head(as.character(w %||% character(0)), 2L)
  w <- w[nzchar(w)]
  if (length(w) == 0L || !isTRUE(moderated)) {
    return(character(0))
  }
  if (!is.data.frame(moderation_map) || nrow(moderation_map) == 0L) {
    return(w)
  }
  rows <- moderation_map$path_type == as.character(path_type %||% "")[[1L]]
  if (nzchar(focal)) {
    rows <- rows & (moderation_map$x == focal | !nzchar(moderation_map$x))
  }
  if (nzchar(mediator)) {
    rows <- rows & (moderation_map$mediator == mediator | !nzchar(moderation_map$mediator))
  }
  if (nzchar(outcome) && "y" %in% names(moderation_map)) {
    rows <- rows & (moderation_map$y == outcome | !nzchar(moderation_map$y))
  }
  intersect(w, unique(as.character(moderation_map$moderator[rows] %||% character(0))))
}

mediation_moderation_setup_panel <- function(
  selected_names,
  variable_table,
  labels = character(0),
  roles = list(),
  mediator_arrangement = "parallel",
  moderated_paths = character(0),
  selected_available = NULL,
  selected_y = NULL,
  selected_x = NULL,
  selected_mediators = NULL,
  selected_w = NULL,
  selected_covariates = NULL,
  input = NULL,
  language = statedu_initial_language()
) {
  selected_names <- as.character(selected_names %||% character(0))
  roles <- mediation_moderation_role_values(
    y = roles$y,
    x = roles$x,
    mediators = roles$mediators,
    w = roles$w,
    covariates = roles$covariates,
    selected_names = selected_names
  )
  mediator_arrangement <- mediation_moderation_scalar_choice(mediator_arrangement, "parallel", c("parallel", "serial"))
  structure <- mediation_moderation_structure_from_mediators(roles$mediators, mediator_arrangement)
  moderated_paths <- mediation_moderation_default_moderated_paths(
    list(mm_moderated_paths = moderated_paths),
    structure = structure
  )
  moderation_controls_disabled <- identical(structure, "serial") || length(roles$w) == 0L
  if (isTRUE(moderation_controls_disabled)) {
    moderated_paths <- character(0)
  }
  disabled_moderated_paths <- if (identical(structure, "none")) c("xm", "my") else character(0)
  spec <- mediation_moderation_builder_spec(
    structure,
    moderated_paths,
    mediator_count = max(1L, length(roles$mediators)),
    moderator_count = length(roles$w),
    two_moderator_model = mediation_moderation_two_moderator_model(if (!is.null(input)) isolate(input$mm_two_moderator_model) else NULL),
    language = language
  )
  assigned <- unique(c(roles$y, roles$x, roles$mediators, roles$w, roles$covariates))
  available <- setdiff(selected_names, assigned)
  available_items <- analysis_variable_items(available, variable_table, labels)
  options_tab <- if (!is.null(input)) isolate(input$mm_options_tab) else NULL
  options_tab <- if (as.character(options_tab %||% "Model") %in% c("Model", "Bootstrap", "Output")) as.character(options_tab %||% "Model") else "Model"
  output_table_style <- analysis_output_table_style(if (!is.null(input)) isolate(input$mm_output_table_style) else NULL)

  div(
    class = "mm-setup-grid",
    div(
      class = "mm-role-grid",
      div(
        class = "analysis-transfer-column analysis-transfer-panel regression-available-panel mm-available-panel",
        mediation_moderation_field_label_tag("variables", language = language),
        analysis_transfer_listbox_input(
          "mm_available",
          items = available_items,
          selected = selected_order_items(selected_available, available),
          size = 17
        )
      ),
      div(
        class = "analysis-transfer-controls regression-transfer-controls mm-transfer-controls",
        actionButton("mm_y_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_x_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_mediators_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_w_move", ">", class = "btn btn-default analysis-move-button"),
        actionButton("mm_covariates_move", ">", class = "btn btn-default analysis-move-button")
      ),
      div(
        class = "analysis-transfer-column analysis-transfer-panel mm-role-targets-panel",
        div(
          class = "mm-role-targets",
          mediation_moderation_target_panel("y", "mm_y", analysis_variable_items(roles$y, variable_table, labels), selected_order_items(selected_y, roles$y), size = 3, allowed_measurements = "continuous", language = language),
          mediation_moderation_target_panel("x", "mm_x", analysis_variable_items(roles$x, variable_table, labels), selected_order_items(selected_x, roles$x), size = 3, language = language, order_buttons = TRUE, up_id = "mm_x_up", down_id = "mm_x_down"),
          mediation_moderation_target_panel("mediators", "mm_mediators", analysis_variable_items(roles$mediators, variable_table, labels), selected_order_items(selected_mediators, roles$mediators), size = 3, allowed_measurements = "continuous", language = language, order_buttons = TRUE, up_id = "mm_mediators_up", down_id = "mm_mediators_down"),
          mediation_moderation_target_panel("w", "mm_w", analysis_variable_items(roles$w, variable_table, labels), selected_order_items(selected_w, roles$w), size = 2, language = language),
          mediation_moderation_target_panel("covariates", "mm_covariates", analysis_variable_items(roles$covariates, variable_table, labels), selected_order_items(selected_covariates, roles$covariates), size = 5, language = language, order_buttons = TRUE, up_id = "mm_covariates_up", down_id = "mm_covariates_down")
        )
      ),
      div(
        class = "mm-model-column",
        div(
          class = "analysis-options-column mm-model-options-column",
          analysis_options_tabs_panel(
            id = "mm_options_tab",
            selected = options_tab,
            class = "mm-model-panel",
            tabPanel(
              analysis_ui_text("Model", language),
              value = "Model",
              div(
                class = "factor-options-tab-content regression-options-tab-content mm-options-tab-content",
                if (length(roles$mediators) >= 2L) {
                  div(
                    class = "analysis-option-group mm-mediator-structure-group",
                    div(class = "analysis-option-title", mediation_moderation_text(language, "Mediator structure", "\ub9e4\uac1c \uad6c\uc870")),
                    radioButtons(
                      "mm_mediator_arrangement",
                      label = NULL,
                      choices = mediation_moderation_mediator_arrangement_choices(language),
                      selected = mediator_arrangement
                    )
                  )
                },
                div(
                  class = paste("analysis-option-group", if (isTRUE(moderation_controls_disabled)) "mm-disabled-option-group" else ""),
                  div(class = "analysis-option-title", mediation_moderation_text(language, "Moderated paths", "\uc870\uc808 \uacbd\ub85c")),
                  mediation_moderation_checkbox_group_input(
                    "mm_moderated_paths",
                    choices = mediation_moderation_builder_path_choices(language),
                    selected = moderated_paths,
                    disabled = moderation_controls_disabled,
                    disabled_values = disabled_moderated_paths
                  )
                ),
                mediation_moderation_moderation_option_group(
                  disabled = moderation_controls_disabled,
                  dash_nonsignificant = isTRUE((if (!is.null(input)) isolate(input$mm_dash_nonsignificant) else NULL) %||% TRUE),
                  language = language
                ),
                div(
                  class = "analysis-option-group",
                  div(class = "analysis-option-title", mediation_moderation_text(language, "Analysis method", "\ubd84\uc11d \ubc29\ubc95")),
                  selectInput(
                    "mm_analysis_method",
                    NULL,
                    choices = mediation_moderation_analysis_method_choices(language),
                    selected = mediation_moderation_scalar_choice(
                      if (!is.null(input)) isolate(input$mm_analysis_method) else NULL,
                      "statedu",
                      c("statedu", "process_ols")
                    ),
                    selectize = FALSE
                  )
                ),
                div(
                  class = "analysis-option-group",
                  div(class = "analysis-option-title", mediation_moderation_text(language, "Two-moderator model", "\ub450 \uc870\uc808\ubcc0\uc218 \ubaa8\ud615")),
                  selectInput(
                    "mm_two_moderator_model",
                    NULL,
                    choices = stats::setNames(
                      c("3", "2"),
                      c(
                        mediation_moderation_model_label("3", language),
                        mediation_moderation_model_label("2", language)
                      )
                    ),
                    selected = mediation_moderation_two_moderator_model(if (!is.null(input)) isolate(input$mm_two_moderator_model) else NULL),
                    selectize = FALSE
                  )
                ),
                analysis_option_group(
                  "Covariate control",
                  list(
                    list(
                      id = "mm_covariate_control_y",
                      label = mediation_moderation_text(language, "Dependent variable", "\uc885\uc18d\ubcc0\uc218"),
                      value = isTRUE(if (!is.null(input)) isolate(input$mm_covariate_control_y %||% TRUE) else TRUE)
                    ),
                    list(
                      id = "mm_covariate_control_m",
                      label = mediation_moderation_text(language, "Mediator variable", "\ub9e4\uac1c\ubcc0\uc218"),
                      value = isTRUE(if (!is.null(input)) isolate(input$mm_covariate_control_m %||% TRUE) else TRUE)
                    )
                  ),
                  language = language
                )
              )
            ),
            tabPanel(
              analysis_ui_text("Bootstrap", language),
              value = "Bootstrap",
              div(
                class = "factor-options-tab-content regression-options-tab-content mm-options-tab-content",
                div(
                  class = "analysis-option-group",
                  selectInput(
                    "mm_boot_r",
                    analysis_ui_text("Number of bootstrap samples", language),
                    choices = bootstrap_resample_choices(language),
                    selected = normalized_bootstrap_resamples(if (!is.null(input)) isolate(input$mm_boot_r) else NULL),
                    selectize = FALSE
                  ),
                  numericInput(
                    "mm_seed",
                    analysis_ui_text("Seed number", language),
                    value = mediation_moderation_numeric_choice(if (!is.null(input)) isolate(input$mm_seed) else NULL, default_seed()),
                    min = 1,
                    step = 1
                  ),
                  selectInput(
                    "mm_ci_method",
                    mediation_moderation_text(language, "Bootstrap CI method", "Bootstrap CI \ubc29\uc2dd"),
                    choices = mediation_moderation_ci_method_choices(language),
                    selected = mediation_moderation_scalar_choice(
                      if (!is.null(input)) isolate(input$mm_ci_method) else NULL,
                      "bias_corrected",
                      c("bias_corrected", "percentile")
                    ),
                    selectize = FALSE
                  )
                )
              )
            ),
            tabPanel(
              analysis_ui_text("Output", language),
              value = "Output",
              div(
                class = "factor-options-tab-content regression-options-tab-content mm-options-tab-content",
                analysis_output_table_style_tabs("mm_output_table_style", output_table_style, language)
              )
            )
          )
        )
      ),
      mediation_moderation_diagram(spec, roles, variable_table, labels, language)
    )
  )
}

mediation_moderation_var_term <- function(name) {
  name <- as.character(name %||% "")
  paste0("`", gsub("`", "\\\\`", name), "`")
}

mediation_moderation_interaction_term <- function(...) {
  paste(vapply(list(...), mediation_moderation_var_term, character(1)), collapse = ":")
}

mediation_moderation_lm_formula <- function(response, terms) {
  terms <- unique(as.character(terms %||% character(0)))
  terms <- terms[nzchar(terms)]
  stats::as.formula(paste(
    mediation_moderation_var_term(response),
    "~",
    if (length(terms) == 0) "1" else paste(terms, collapse = " + ")
  ))
}

mediation_moderation_model_coef <- function(model, term) {
  coefs <- stats::coef(model)
  term <- as.character(term %||% "")
  term <- gsub("`", "", term, fixed = TRUE)
  coef_names <- gsub("`", "", names(coefs), fixed = TRUE)
  if (!nzchar(term) || !term %in% names(coefs)) {
    matched <- which(coef_names == term)
    if (length(matched) == 0L) {
      return(NA_real_)
    }
    return(mediation_moderation_numeric_scalar(unname(coefs[[matched[[1]]]])))
  }
  mediation_moderation_numeric_scalar(unname(coefs[[term]]))
}

mediation_moderation_fit_lm <- function(data, response, terms) {
  stats::lm(mediation_moderation_lm_formula(response, terms), data = data)
}

mediation_moderation_clean_term <- function(term) {
  gsub("`", "", as.character(term %||% ""), fixed = TRUE)
}

mediation_moderation_model_terms <- function(model) {
  as.character(attr(stats::terms(model), "term.labels") %||% character(0))
}

mediation_moderation_interaction_terms <- function(model) {
  terms <- mediation_moderation_model_terms(model)
  terms[grepl(":", mediation_moderation_clean_term(terms), fixed = TRUE)]
}

mediation_moderation_has_interaction <- function(result) {
  length(mediation_moderation_interaction_terms(result$model)) > 0L
}

mediation_moderation_base_model <- function(model) {
  terms <- mediation_moderation_model_terms(model)
  base_terms <- terms[!grepl(":", mediation_moderation_clean_term(terms), fixed = TRUE)]
  if (length(base_terms) == length(terms)) {
    return(NULL)
  }
  response <- all.vars(stats::formula(model))[[1]]
  model_data <- stats::model.frame(model)
  stats::lm(mediation_moderation_lm_formula(response, base_terms), data = model_data)
}

mediation_moderation_term_variable <- function(term, variables) {
  term <- mediation_moderation_clean_term(term)
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  if (!nzchar(term) || length(variables) == 0L || grepl(":", term, fixed = TRUE)) {
    return(NA_character_)
  }
  matched <- variables[term == variables | startsWith(term, variables)]
  if (length(matched) == 0L) {
    return(NA_character_)
  }
  matched[[which.max(nchar(matched))]]
}

mediation_moderation_term_rank <- function(clean_terms, variables, base_rank) {
  variables <- as.character(variables %||% character(0))
  variables <- variables[nzchar(variables)]
  if (length(variables) == 0L) {
    return(rep(NA_integer_, length(clean_terms)))
  }
  matched <- vapply(clean_terms, mediation_moderation_term_variable, character(1), variables = variables)
  variable_index <- match(matched, variables)
  ifelse(is.na(variable_index), NA_integer_, base_rank + variable_index)
}

mediation_moderation_sort_terms <- function(terms, covariates = character(0), focal = "", w = character(0), mediators = character(0), all_x = NULL) {
  clean_terms <- mediation_moderation_clean_term(terms)
  rank <- rep(90L, length(clean_terms))
  rank[clean_terms == "(Intercept)"] <- 0L
  display_x <- unique(c(as.character(all_x %||% character(0)), as.character(focal %||% character(0))))
  display_x <- display_x[nzchar(display_x)]
  display_covariates <- setdiff(as.character(covariates %||% character(0)), display_x)
  covariate_rank <- mediation_moderation_term_rank(clean_terms, display_covariates, 10L)
  focal_rank <- mediation_moderation_term_rank(clean_terms, display_x, 20L)
  mediator_rank <- mediation_moderation_term_rank(clean_terms, mediators, 30L)
  covariate_index <- !is.na(covariate_rank)
  mediator_index <- !is.na(mediator_rank)
  focal_index <- !is.na(focal_rank)
  rank[covariate_index] <- covariate_rank[covariate_index]
  rank[mediator_index] <- mediator_rank[mediator_index]
  rank[focal_index] <- focal_rank[focal_index]
  w <- as.character(w %||% character(0))
  w <- w[nzchar(w)]
  if (length(w) > 0L) {
    w_rank <- mediation_moderation_term_rank(clean_terms, w, 40L)
    w_index <- !is.na(w_rank)
    rank[w_index] <- w_rank[w_index]
  }
  rank[grepl(":", clean_terms, fixed = TRUE)] <- 50L
  order(rank, seq_along(clean_terms))
}

mediation_moderation_order_output_table <- function(table, result) {
  if (!is.data.frame(table) || nrow(table) == 0L) {
    return(table)
  }
  terms <- mediation_moderation_clean_term(table$Term %||% character(0))
  raw_variables <- if (".raw_variable" %in% names(table)) {
    as.character(table$.raw_variable %||% "")
  } else {
    rep("", nrow(table))
  }
  interaction_rows <- grepl("\\s+x\\s+", terms, perl = TRUE) | grepl(":", raw_variables, fixed = TRUE)
  rank <- rep(900L, nrow(table))
  rank[terms == "(Intercept)"] <- 0L

  assign_variable_rank <- function(variables, base_rank) {
    variables <- as.character(variables %||% character(0))
    variables <- variables[nzchar(variables)]
    if (length(variables) == 0L) return()
    for (index in seq_along(variables)) {
      aliases <- variable_aliases(variables[[index]])
      prefix_rows <- rep(FALSE, length(terms))
      for (alias in aliases) {
        prefix_rows <- prefix_rows | startsWith(terms, paste0(alias, ":"))
      }
      rows <- !interaction_rows & (
        raw_variables %in% aliases |
          terms %in% aliases |
          prefix_rows
      )
      rank[rows] <<- pmin(rank[rows], base_rank + index)
    }
  }

  variable_aliases <- function(variable) {
    variable <- as.character(variable %||% "")[[1]]
    aliases <- variable
    if (nzchar(variable)) {
      display_label <- tryCatch(
        display_variable_name_static(variable, result$variable_info, result$labels %||% character(0), label_only = TRUE),
        error = function(e) ""
      )
      aliases <- c(aliases, display_label)
      if (is.data.frame(result$variable_info) && all(c("name", "var_label") %in% names(result$variable_info))) {
        row_index <- match(variable, as.character(result$variable_info$name))
        if (!is.na(row_index)) {
          aliases <- c(aliases, as.character(result$variable_info$var_label[[row_index]] %||% ""))
        }
      }
      if (is.data.frame(result$category_table) && all(c("name", "var_label") %in% names(result$category_table))) {
        row_index <- match(variable, as.character(result$category_table$name))
        if (!is.na(row_index)) {
          aliases <- c(aliases, as.character(result$category_table$var_label[[row_index]] %||% ""))
        }
      }
    }
    unique(aliases[nzchar(aliases)])
  }

  variable_info_order <- function(variables) {
    variables <- unique(as.character(variables %||% character(0)))
    variables <- variables[nzchar(variables)]
    if (length(variables) == 0L) {
      return(variables)
    }
    info_names <- if (is.data.frame(result$variable_info) && "name" %in% names(result$variable_info)) {
      as.character(result$variable_info$name)
    } else {
      character(0)
    }
    if (length(info_names) == 0L) {
      return(variables)
    }
    order_rank <- match(variables, info_names)
    missing_rank <- is.na(order_rank)
    order_rank[missing_rank] <- length(info_names) + seq_len(sum(missing_rank))
    variables[order(order_rank, seq_along(variables))]
  }

  display_x <- unique(c(as.character(result$all_x %||% character(0)), as.character(result$focal %||% character(0))))
  excluded_roles <- unique(c(display_x, as.character(result$w %||% character(0)), as.character(result$mediators %||% character(0))))
  model_covariates <- setdiff(as.character(result$predictors %||% character(0)), excluded_roles)
  display_covariates <- variable_info_order(setdiff(
    unique(c(as.character(result$covariates %||% character(0)), model_covariates)),
    display_x
  ))
  assign_variable_rank(display_covariates, 100L)
  assign_variable_rank(display_x, 200L)
  assign_variable_rank(result$mediators, 300L)
  assign_variable_rank(result$w, 400L)
  rank[interaction_rows] <- 500L

  table[order(rank, seq_len(nrow(table))), , drop = FALSE]
}

mediation_moderation_compact_xm_variables <- function(result) {
  variables <- unique(c(
    as.character(result$focal %||% character(0)),
    as.character(result$mediators %||% character(0))
  ))
  variables[nzchar(variables)]
}

mediation_moderation_filter_compact_xm_rows <- function(table, result, output_table_style = "standard") {
  if (!identical(analysis_output_table_style(output_table_style), "compact_xm") ||
      !is.data.frame(table) || nrow(table) == 0L) {
    return(table)
  }
  keep_variables <- mediation_moderation_compact_xm_variables(result)
  if (length(keep_variables) == 0L) {
    return(table[0, , drop = FALSE])
  }
  terms <- mediation_moderation_clean_term(table$Term %||% character(0))
  raw_variables <- if (".raw_variable" %in% names(table)) {
    as.character(table$.raw_variable %||% "")
  } else {
    rep("", nrow(table))
  }
  interaction_rows <- grepl("\\s+x\\s+", terms, perl = TRUE) | grepl(":", raw_variables, fixed = TRUE)
  keep <- !interaction_rows & (raw_variables %in% keep_variables | terms %in% keep_variables)
  table[keep, , drop = FALSE]
}

mediation_moderation_model_summary_row <- function(model, focal, equation) {
  model_summary <- summary(model)
  f_stat <- unname(model_summary$fstatistic["value"])
  f_df1 <- unname(model_summary$fstatistic["numdf"])
  f_df2 <- unname(model_summary$fstatistic["dendf"])
  f_p <- stats::pf(f_stat, f_df1, f_df2, lower.tail = FALSE)
  residuals <- stats::residuals(model)
  normality <- tryCatch(nortest::lillie.test(residuals), error = function(e) NULL)
  homogeneity <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  dw_d <- tryCatch(durbin_watson_stat(model), error = function(e) NA_real_)
  dw_p <- tryCatch(ncol(stats::model.matrix(model)) - 1L, error = function(e) NA_integer_)
  dw_crit <- tryCatch(mediation_moderation_cached_dw_critical(stats::nobs(model), dw_p), error = function(e) list(dL = NA_real_, dU = NA_real_, note = NA_character_))
  data.frame(
    X = focal,
    Equation = equation,
    N = stats::nobs(model),
    `F(p)` = sprintf("%s(%s)", format_decimal3(f_stat), format_p(f_p)),
    `R²(adj R²)` = sprintf("%s (%s)", format_decimal3(unname(model_summary$r.squared)), format_decimal3(unname(model_summary$adj.r.squared))),
    `d(dU~4-dU)` = sprintf(
      "%s (%s~%s)",
      format_decimal3(dw_d),
      format_decimal3(dw_crit$dU),
      format_decimal3(4 - dw_crit$dU)
    ),
    `z(p)` = if (is.null(normality)) "" else sprintf("%s(%s)", format_decimal3(unname(normality$statistic)), format_p(normality$p.value)),
    `chisq(p)` = if (is.null(homogeneity)) "" else sprintf("%s(%s)", format_decimal3(unname(homogeneity$statistic)), format_p(homogeneity$p.value)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mediation_moderation_path_result <- function(
  model,
  focal,
  equation,
  covariates = character(0),
  w = character(0),
  mediators = character(0),
  boot_r = 5000L,
  seed = default_seed(),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  all_x = NULL,
  show_f2 = TRUE
) {
  analysis_method <- mediation_moderation_scalar_choice(analysis_method, "statedu", c("statedu", "process_ols"))
  ci_method <- mediation_moderation_scalar_choice(ci_method, "bias_corrected", c("bias_corrected", "percentile"))
  residual_diagnostics <- isTRUE(residual_diagnostics)
  auto_method <- isTRUE(auto_method) && residual_diagnostics
  residuals <- stats::residuals(model)
  normality <- if (residual_diagnostics) tryCatch(nortest::lillie.test(residuals), error = function(e) NULL) else NULL
  homogeneity <- if (residual_diagnostics) tryCatch(lmtest::bptest(model), error = function(e) NULL) else NULL
  normality_p <- if (is.null(normality)) NA_real_ else unname(normality$p.value)
  homogeneity_p <- if (is.null(homogeneity)) NA_real_ else unname(homogeneity$p.value)
  normal_ok <- is.na(normality_p) || normality_p > .05
  homo_ok <- is.na(homogeneity_p) || homogeneity_p > .05
  use_bootstrap <- isTRUE(auto_method) && !normal_ok
  use_hc3 <- isTRUE(auto_method) && !homo_ok
  if (identical(analysis_method, "process_ols")) {
    use_bootstrap <- FALSE
    use_hc3 <- FALSE
  }
  vcov_matrix <- if (isTRUE(use_hc3)) sandwich::vcovHC(model, type = "HC3") else NULL
  coef_table <- coeftest_table(model, vcov_matrix)
  if (isTRUE(use_hc3) && "SE" %in% names(coef_table)) {
    names(coef_table)[names(coef_table) == "SE"] <- "HC3 SE"
  }
  model_summary <- summary(model)
  f_stat <- unname(model_summary$fstatistic["value"])
  f_df1 <- unname(model_summary$fstatistic["numdf"])
  f_df2 <- unname(model_summary$fstatistic["dendf"])
  dw_d <- tryCatch(durbin_watson_stat(model), error = function(e) NA_real_)
  dw_p <- tryCatch(ncol(stats::model.matrix(model)) - 1L, error = function(e) NA_integer_)
  dw_crit <- tryCatch(mediation_moderation_cached_dw_critical(stats::nobs(model), dw_p), error = function(e) list(dL = NA_real_, dU = NA_real_, note = NA_character_))
  method <- if (identical(analysis_method, "process_ols")) {
    "PROCESS-compatible OLS regression"
  } else if (normal_ok && homo_ok) {
    "OLS regression"
  } else if (normal_ok && !homo_ok) {
    "OLS regression with HC3 robust standard errors"
  } else if (!normal_ok && homo_ok) {
    "Bootstrap regression"
  } else {
    "Bootstrap regression with HC3 robust standard errors"
  }
  list(
    model = model,
    formula = stats::formula(model),
    focal = focal,
    all_x = unique(as.character(all_x %||% focal)),
    equation = equation,
    covariates = covariates,
    w = w,
    mediators = mediators,
    n = stats::nobs(model),
    r_squared = unname(model_summary$r.squared),
    adjusted_r_squared = unname(model_summary$adj.r.squared),
    f_statistic = f_stat,
    f_df1 = f_df1,
    f_df2 = f_df2,
    f_p = stats::pf(f_stat, f_df1, f_df2, lower.tail = FALSE),
    dw_d = dw_d,
    dw_crit = dw_crit,
    normality_statistic = if (is.null(normality)) NA_real_ else unname(normality$statistic),
    normality_p = normality_p,
    homogeneity_statistic = if (is.null(homogeneity)) NA_real_ else unname(homogeneity$statistic),
    homogeneity_p = homogeneity_p,
    method = method,
    analysis_method = analysis_method,
    bootstrap_ci_method = ci_method,
    use_hc3 = use_hc3,
    use_bootstrap = use_bootstrap,
    residual_diagnostics = residual_diagnostics,
    auto_method = auto_method,
    show_f2 = isTRUE(show_f2),
    bootstrap_r = as.integer(boot_r %||% 5000L),
    bootstrap_seed = as.integer(seed %||% default_seed()),
    coef_table = coef_table,
    boot_table = NULL,
    predictors = setdiff(all.vars(stats::formula(model)), all.vars(stats::formula(model))[[1]]),
    variable_info = variable_info,
    labels = labels,
    category_table = category_table,
    refs = refs,
    value_labels = value_labels
  )
}

mediation_moderation_display_coefficient_table <- function(result, include_vif = FALSE) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(data.frame())
  }
  table$Term <- mediation_moderation_clean_term(table$Term)
  order_index <- mediation_moderation_sort_terms(
    table$Term,
    covariates = result$covariates,
    focal = result$focal,
    w = result$w,
    mediators = result$mediators,
    all_x = result$all_x
  )
  table <- table[order_index, , drop = FALSE]
  show_f2 <- isTRUE(result$show_f2 %||% TRUE)
  if (isTRUE(show_f2) && "f2" %in% names(table)) {
    effect_sizes <- stats::setNames(mediation_moderation_numeric_vector(table$f2), table$Term)
    table$f2_effect <- effect_sizes[table$Term]
    table$f2_effect[table$Term == "(Intercept)"] <- NA_real_
  }
  add_f2 <- function(output) {
    if (isTRUE(show_f2) && "f2_effect" %in% names(table)) {
      output$f2 <- table$f2_effect
    }
    output
  }
  if (isTRUE(result$use_bootstrap) && is.data.frame(result$boot_table) && nrow(result$boot_table) > 0) {
    boot_table <- result$boot_table
    boot_table$Term <- mediation_moderation_clean_term(boot_table$Term)
    boot_match <- match(table$Term, boot_table$Term)
    if (isTRUE(result$use_hc3)) {
      output <- data.frame(
        Term = table$Term,
        B = table$B,
        `HC3 SE` = table[["HC3 SE"]],
        LLCI = boot_table$Boot_LLCI[boot_match],
        ULCI = boot_table$Boot_ULCI[boot_match],
        `Boot p` = boot_table$Boot_p[boot_match],
        check.names = FALSE
      )
      output <- add_f2(output)
      if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
      return(output)
    }
    output <- data.frame(
      Term = table$Term,
      B = table$B,
      `Boot SE` = boot_table$Boot_SE[boot_match],
      LLCI = boot_table$Boot_LLCI[boot_match],
      ULCI = boot_table$Boot_ULCI[boot_match],
      `Boot p` = boot_table$Boot_p[boot_match],
      check.names = FALSE
    )
    output <- add_f2(output)
    if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
    return(output)
  }
  if (isTRUE(result$use_hc3)) {
    output <- data.frame(
      Term = table$Term,
      B = table$B,
      `HC3 SE` = table[["HC3 SE"]],
      t = table$t,
      p = table$p,
      check.names = FALSE
    )
    output <- add_f2(output)
    if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
    return(output)
  }
  output <- data.frame(
    Term = table$Term,
    B = table$B,
    SE = table$SE,
    t = table$t,
    p = table$p,
    check.names = FALSE
  )
  output <- add_f2(output)
  if (isTRUE(include_vif) && "VIF" %in% names(table)) output$VIF <- table$VIF
  output
}

mediation_moderation_display_interaction_symbol <- function(text) {
  text <- as.character(text %||% "")
  gsub("\\s*:+\\s*", " x ", text, perl = TRUE)
}

mediation_moderation_format_interaction_terms <- function(table) {
  if (!is.data.frame(table) || nrow(table) == 0) {
    return(table)
  }
  for (column in intersect(c("Interaction term(s)", "Effect"), names(table))) {
    table[[column]] <- mediation_moderation_display_interaction_symbol(table[[column]])
  }
  table
}

mediation_moderation_path_note_line <- function(result) {
  method_note <- if (identical(result$analysis_method, "process_ols") || !isTRUE(result$auto_method)) {
    "Path coefficients, standard errors, t tests, p values, model F tests, and interaction R\u00B2 change tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Path coefficients use the StatEdu diagnostic-based method: HC3 robust standard errors are used when homoscedasticity is rejected and bootstrap coefficient intervals are used when residual normality is rejected;"
  }
  parts <- c(
    method_note,
    sprintf("Bootstrap confidence limits use the %s method;", bootstrap_ci_method_label(result$bootstrap_ci_method)),
    if (isTRUE(result$show_f2 %||% TRUE)) "f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient;" else "",
    if (isTRUE(result$residual_diagnostics)) "d(dU~4-dU) = Durbin-Watson statistic (upper critical value~4-upper critical value);" else "",
    if (isTRUE(result$residual_diagnostics)) "z(p) = Lilliefors corrected Kolmogorov-Smirnov residual normality test statistic (p-value);" else "",
    if (isTRUE(result$residual_diagnostics)) sprintf("%s = Breusch-Pagan residual homoscedasticity test statistic (p-value)", stat_chisq_label(with_p = TRUE)) else "Residual diagnostics were not run."
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

mediation_moderation_path_title <- function(result) {
  sprintf("%s: %s (X: %s)", result$method, result$equation, result$focal)
}

mediation_moderation_hierarchical_steps <- function(result) {
  if (!mediation_moderation_has_interaction(result)) {
    return(NULL)
  }
  if (is.list(result$hierarchical_base) && !is.null(result$hierarchical_base$model)) {
    step1 <- result$hierarchical_base
  } else {
    base_model <- mediation_moderation_base_model(result$model)
    if (is.null(base_model)) {
      return(NULL)
    }
    step1 <- mediation_moderation_path_result(
      base_model,
      result$focal,
      result$equation,
      covariates = result$covariates,
      w = result$w,
      mediators = result$mediators,
      boot_r = result$bootstrap_r,
      seed = result$bootstrap_seed,
      variable_info = result$variable_info,
      labels = result$labels,
      category_table = result$category_table,
      refs = result$refs,
      value_labels = result$value_labels,
      analysis_method = result$analysis_method,
      ci_method = result$bootstrap_ci_method,
      residual_diagnostics = result$residual_diagnostics,
      auto_method = result$auto_method,
      all_x = result$all_x,
      show_f2 = result$show_f2
    )
  }
  step1$hierarchical_step <- 1L
  step2 <- result
  step2$hierarchical_step <- 2L
  list(step1, step2)
}

mediation_moderation_hierarchical_model_table <- function(result, include_vif = FALSE, output_table_style = "standard") {
  table <- mediation_moderation_display_coefficient_table(result, include_vif = include_vif)
  table <- coefficient_output_table_with_context(
    table,
    predictors = result$predictors,
    include_references = TRUE,
    variable_info = result$variable_info,
    refs = result$refs,
    value_labels = result$value_labels,
    labels = result$labels,
    category_table = result$category_table,
    keep_raw_columns = TRUE
  )
  table <- mediation_moderation_order_output_table(table, result)
  table <- mediation_moderation_filter_compact_xm_rows(table, result, output_table_style)
  table <- table[, setdiff(names(table), c(".raw_variable", ".raw_level")), drop = FALSE]
  names(table)[names(table) == "f2"] <- "f\u00B2"
  table <- mediation_moderation_format_interaction_terms(table)
  table <- mediation_moderation_path_coefficient_widths(table)
  if (isTRUE(result$use_bootstrap)) {
    attr(table, "bootstrap_regression") <- TRUE
  }
  table
}

mediation_moderation_hierarchical_note_line <- function(group) {
  analysis_method <- as.character(group[[length(group)]]$analysis_method %||% "statedu")[[1]]
  auto_method <- isTRUE(group[[length(group)]]$auto_method)
  method_note <- if (identical(analysis_method, "process_ols") || !isTRUE(auto_method)) {
    "Coefficients, standard errors, t tests, p values, model F tests, and R\u00B2 change tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Coefficients use the StatEdu diagnostic-based method; R\u00B2 change is reported with bootstrap CI when bootstrap is active and robust Wald F p when HC3 is active;"
  }
  paste(
    "Model 1 estimates main effects before interaction terms are added;",
    "Model 2 adds the interaction terms for the selected moderated path;",
    "VIF is reported in Model 1 for the main-effect model;",
    method_note,
    if (any(vapply(group, function(result) isTRUE(result$show_f2 %||% TRUE), logical(1)))) {
      "f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient; standardized beta is not reported for mediation/moderation path coefficients."
    } else {
      "Standardized beta is not reported for mediation/moderation path coefficients."
    }
  )
}

mediation_moderation_hierarchical_path_result_ui <- function(result, landscape = FALSE, output_table_style = "standard") {
  group <- mediation_moderation_hierarchical_steps(result)
  if (is.null(group)) {
    return(NULL)
  }
  model_tables <- list(
    mediation_moderation_hierarchical_model_table(group[[1]], include_vif = TRUE, output_table_style = output_table_style),
    mediation_moderation_hierarchical_model_table(group[[2]], include_vif = FALSE, output_table_style = output_table_style)
  )
  div(
    class = mediation_moderation_result_panel_class("result-section regression-result-panel mm-path-result-section", landscape),
    h3(sprintf("Moderation Analysis: %s (X: %s)", result$equation, result$focal)),
    hierarchical_coefficient_html_table(
      model_tables,
      c("Model 1", "Model 2"),
      hierarchical_summary_values(group),
      mediation_moderation_hierarchical_note_line(group),
      model_note_lines = c(
        "Model 1: main effects without interaction terms.",
        "Model 2: main effects plus interaction terms."
      ),
      output_table_style = output_table_style
    )
  )
}

mediation_moderation_model4_path_label <- function(result) {
  equation <- as.character(result$equation %||% "")[[1]]
  response <- tryCatch(all.vars(stats::formula(result$model))[[1]], error = function(e) "")
  response_label <- if (nzchar(response)) {
    display_variable_name_static(response, result$variable_info, result$labels, label_only = TRUE)
  } else {
    ""
  }
  label <- equation
  if (grepl("^M model:", equation)) {
    label <- "M model"
  }
  if (nzchar(response_label)) {
    return(tagList(label, tags$br(), tags$span(sprintf("(%s)", response_label))))
  }
  label
}

mediation_moderation_model4_path_note_line <- function(group) {
  analysis_method <- as.character(group[[length(group)]]$analysis_method %||% "statedu")[[1]]
  auto_method <- isTRUE(group[[length(group)]]$auto_method)
  method_note <- if (identical(analysis_method, "process_ols") || !isTRUE(auto_method)) {
    "Coefficients, standard errors, t tests, p values, and model F tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Coefficients use the StatEdu diagnostic-based method;"
  }
  paste(
    "Mediation path coefficients are displayed in the hierarchical regression table style;",
    method_note,
    if (any(vapply(group, function(result) isTRUE(result$show_f2 %||% TRUE), logical(1)))) {
      "f\u00B2 = Cohen's f-squared effect size for each non-intercept coefficient; standardized beta is not reported for mediation path coefficients."
    } else {
      "Standardized beta is not reported for mediation path coefficients."
    }
  )
}

mediation_moderation_mediator_count <- function(result = NULL, path_results = NULL) {
  roles <- result$roles %||% list()
  mediators <- unique(as.character(roles$mediators %||% character(0)))
  mediators <- mediators[nzchar(mediators)]
  if (length(mediators) > 0L) {
    return(length(mediators))
  }
  path_results <- path_results %||% result$path_results %||% list()
  for (path_result in path_results) {
    path_mediators <- unique(as.character(path_result$mediators %||% character(0)))
    path_mediators <- path_mediators[nzchar(path_mediators)]
    if (length(path_mediators) > length(mediators)) {
      mediators <- path_mediators
    }
  }
  if (length(mediators) > 0L) {
    return(length(mediators))
  }
  sum(vapply(path_results, function(path_result) {
    grepl("^M model:", as.character(path_result$equation %||% "")[[1L]])
  }, logical(1)))
}

mediation_moderation_custom_coefficients_landscape <- function(result = NULL, path_results = NULL) {
  isTRUE(result$custom_model_canvas) && mediation_moderation_mediator_count(result, path_results) >= 3L
}

mediation_moderation_result_panel_class <- function(base_class, landscape = FALSE) {
  paste(c(base_class, if (isTRUE(landscape)) "landscape-table-panel"), collapse = " ")
}

mediation_moderation_model4_path_group_ui <- function(group, show_focal = FALSE, landscape = FALSE, output_table_style = "standard") {
  group <- Filter(function(result) is.list(result) && !is.null(result$model), group)
  if (length(group) == 0L) {
    return(NULL)
  }
  model_tables <- lapply(group, mediation_moderation_hierarchical_model_table, include_vif = FALSE, output_table_style = output_table_style)
  model_labels <- lapply(group, mediation_moderation_model4_path_label)
  focal <- as.character(group[[1]]$focal %||% "")[[1]]
  title <- "Model 4 mediation path coefficients"
  if (isTRUE(show_focal) && nzchar(focal)) {
    title <- sprintf("%s (X: %s)", title, focal)
  }
  div(
    class = mediation_moderation_result_panel_class("result-section regression-result-panel mm-model4-path-section", landscape),
    h3(title),
    hierarchical_coefficient_html_table(
      model_tables,
      model_labels,
      hierarchical_summary_values(group),
      mediation_moderation_model4_path_note_line(group),
      include_delta = FALSE,
      output_table_style = output_table_style
    )
  )
}

mediation_moderation_model4_path_result_ui <- function(path_results, landscape = FALSE, output_table_style = "standard") {
  path_results <- Filter(function(result) is.list(result) && !is.null(result$model), path_results %||% list())
  if (length(path_results) > 1L) {
    keys <- vapply(path_results, function(result) {
      model_terms <- stats::terms(result$model)
      response <- all.vars(stats::formula(result$model)[[2]])
      term_labels <- sort(attr(model_terms, "term.labels") %||% character(0))
      paste(
        as.character(result$equation %||% "")[[1]],
        paste(response, collapse = "+"),
        paste(term_labels, collapse = "+"),
        sep = "\r"
      )
    }, character(1))
    path_results <- path_results[!duplicated(keys)]
  }
  groups <- list(path_results)
  Filter(
    Negate(is.null),
    lapply(groups, mediation_moderation_model4_path_group_ui, show_focal = FALSE, landscape = landscape, output_table_style = output_table_style)
  )
}

mediation_moderation_path_result_ui <- function(result, landscape = FALSE, output_table_style = "standard") {
  if (isTRUE(mediation_moderation_has_interaction(result))) {
    hierarchical_ui <- mediation_moderation_hierarchical_path_result_ui(result, landscape = landscape, output_table_style = output_table_style)
    if (!is.null(hierarchical_ui)) {
      return(hierarchical_ui)
    }
  }
  table <- mediation_moderation_display_coefficient_table(result)
  table <- coefficient_output_table_with_context(
    table,
    predictors = result$predictors,
    include_references = TRUE,
    variable_info = result$variable_info,
    refs = result$refs,
    value_labels = result$value_labels,
    labels = result$labels,
    category_table = result$category_table,
    keep_raw_columns = TRUE
  )
  table <- mediation_moderation_order_output_table(table, result)
  table <- mediation_moderation_filter_compact_xm_rows(table, result, output_table_style)
  table <- table[, setdiff(names(table), c(".raw_variable", ".raw_level")), drop = FALSE]
  names(table)[names(table) == "f2"] <- "f\u00B2"
  table <- mediation_moderation_format_interaction_terms(table)
  table <- mediation_moderation_path_coefficient_widths(table)
  if (isTRUE(result$use_bootstrap)) {
    attr(table, "bootstrap_regression") <- TRUE
  }
  div(
    class = mediation_moderation_result_panel_class("result-section regression-result-panel mm-path-result-section", landscape),
    h3(mediation_moderation_path_title(result)),
    coefficient_html_table(
      table,
      coefficient_fit_line(result),
      coefficient_stat_lines(result),
      warning_line = NULL,
      note_line = mediation_moderation_path_note_line(result),
      output_table_style = output_table_style
    )
  )
}

mediation_moderation_match_coef_name <- function(model, term) {
  term <- mediation_moderation_clean_term(term)
  coef_names <- names(stats::coef(model))
  clean_names <- mediation_moderation_clean_term(coef_names)
  matched <- which(clean_names == term)
  if (length(matched) == 0L) {
    return(NA_character_)
  }
  coef_names[[matched[[1L]]]]
}

mediation_moderation_match_interaction_coef_name <- function(model, variables) {
  variables <- sort(as.character(variables %||% character(0)))
  variables <- variables[nzchar(variables)]
  if (length(variables) < 2L) {
    return(NA_character_)
  }
  coef_names <- names(stats::coef(model))
  clean_names <- mediation_moderation_clean_term(coef_names)
  for (index in seq_along(clean_names)) {
    parts <- sort(strsplit(clean_names[[index]], ":", fixed = TRUE)[[1]])
    if (identical(parts, variables)) {
      return(coef_names[[index]])
    }
  }
  NA_character_
}

mediation_moderation_model_variable_is_factor <- function(model, variable) {
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  is.data.frame(frame) && variable %in% names(frame) && is.factor(frame[[variable]])
}

mediation_moderation_interaction_specs <- function(result) {
  model <- result$model
  moderators <- utils::head(as.character(result$w %||% character(0)), 2L)
  moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
  if (is.null(model) || length(moderators) == 0L) {
    return(list())
  }
  terms <- mediation_moderation_interaction_terms(model)
  specs <- list()
  for (term in terms) {
    clean_term <- mediation_moderation_clean_term(term)
    parts <- strsplit(clean_term, ":", fixed = TRUE)[[1]]
    if (length(parts) != 2L) {
      next
    }
    moderator <- intersect(parts, moderators)
    if (length(moderator) != 1L) {
      next
    }
    predictor <- setdiff(parts, moderator)
    if (length(predictor) != 1L || !nzchar(predictor)) {
      next
    }
    predictor_term <- mediation_moderation_match_coef_name(model, predictor)
    interaction_term <- mediation_moderation_match_interaction_coef_name(model, c(predictor, moderator))
    moderator_is_factor <- mediation_moderation_model_variable_is_factor(model, moderator)
    if (is.na(predictor_term) || (is.na(interaction_term) && !isTRUE(moderator_is_factor))) {
      next
    }
    conditioning_moderator <- setdiff(moderators, moderator)
    conditioning_term <- if (length(conditioning_moderator) == 1L) {
      mediation_moderation_match_interaction_coef_name(model, c(predictor, conditioning_moderator))
    } else {
      NA_character_
    }
    three_way_term <- if (length(conditioning_moderator) == 1L) {
      mediation_moderation_match_interaction_coef_name(model, c(predictor, moderator, conditioning_moderator))
    } else {
      NA_character_
    }
    equation <- as.character(result$equation %||% "")
    path <- if (identical(predictor, result$focal) && grepl("^M", equation)) {
      "X -> M"
    } else if (identical(predictor, result$focal) && identical(equation, "Y model")) {
      "X -> Y"
    } else if (predictor %in% result$mediators && identical(equation, "Y model")) {
      "M -> Y"
    } else {
      paste0(predictor, " -> ", all.vars(stats::formula(model))[[1]])
    }
    specs[[length(specs) + 1L]] <- list(
      equation = equation,
      path = path,
      predictor = predictor,
      moderator = moderator,
      interaction = clean_term,
      predictor_term = predictor_term,
      interaction_term = interaction_term,
      moderator_is_factor = moderator_is_factor,
      conditioning_moderator = if (length(conditioning_moderator) == 1L) conditioning_moderator else NA_character_,
      conditioning_term = conditioning_term,
      three_way_term = three_way_term
    )
  }
  specs
}

mediation_moderation_interaction_result_key <- function(result) {
  if (is.null(result$model)) {
    return("")
  }
  response <- tryCatch(all.vars(stats::formula(result$model))[[1L]], error = function(e) "")
  terms <- tryCatch(attr(stats::terms(result$model), "term.labels") %||% character(0), error = function(e) character(0))
  terms <- sort(unique(mediation_moderation_clean_term(terms)))
  group <- mediation_moderation_hierarchical_steps(result)
  base_terms <- if (is.list(group) && length(group) >= 1L && !is.null(group[[1L]]$model)) {
    tryCatch(attr(stats::terms(group[[1L]]$model), "term.labels") %||% character(0), error = function(e) character(0))
  } else {
    character(0)
  }
  base_terms <- sort(unique(mediation_moderation_clean_term(base_terms)))
  paste(
    as.character(result$equation %||% ""),
    response,
    paste(base_terms, collapse = "+"),
    paste(terms, collapse = "+"),
    sep = "\r"
  )
}

mediation_moderation_model_result_key <- function(result) {
  if (is.null(result$model)) {
    return("")
  }
  response <- tryCatch(all.vars(stats::formula(result$model))[[1L]], error = function(e) "")
  terms <- tryCatch(attr(stats::terms(result$model), "term.labels") %||% character(0), error = function(e) character(0))
  terms <- sort(unique(mediation_moderation_clean_term(terms)))
  paste(
    as.character(result$equation %||% ""),
    response,
    paste(terms, collapse = "+"),
    sep = "\r"
  )
}

mediation_moderation_interaction_spec_key <- function(result, spec) {
  response <- tryCatch(all.vars(stats::formula(result$model))[[1L]], error = function(e) "")
  paste(
    as.character(spec$equation %||% result$equation %||% ""),
    response,
    as.character(spec$predictor %||% ""),
    as.character(spec$moderator %||% ""),
    as.character(spec$interaction_term %||% spec$interaction %||% ""),
    as.character(spec$conditioning_moderator %||% ""),
    as.character(spec$conditioning_term %||% ""),
    as.character(spec$three_way_term %||% ""),
    sep = "\r"
  )
}

mediation_moderation_path_display_label <- function(result, spec) {
  outcome <- tryCatch(all.vars(stats::formula(result$model))[[1]], error = function(e) "")
  predictor_label <- mediation_moderation_display_name(spec$predictor, result$variable_info, result$labels)
  outcome_label <- mediation_moderation_display_name(outcome, result$variable_info, result$labels)
  if (!nzchar(predictor_label) || !nzchar(outcome_label)) {
    return(as.character(spec$path %||% ""))
  }
  paste0(predictor_label, "-->", outcome_label)
}

mediation_moderation_add_group_bottom_border <- function(table, row_indices) {
  if (!is.data.frame(table) || nrow(table) == 0L || length(row_indices) == 0L) {
    return(table)
  }
  row_indices <- unique(as.integer(row_indices))
  row_indices <- row_indices[is.finite(row_indices) & row_indices >= 1L & row_indices <= nrow(table)]
  if (length(row_indices) == 0L) {
    return(table)
  }
  styles <- expand.grid(
    row = row_indices,
    column = names(table),
    stringsAsFactors = FALSE
  )
  styles$style <- "border-bottom:2px solid #1f2937 !important;"
  attr(table, "cell_styles") <- rbind(attr(table, "cell_styles", exact = TRUE), styles)
  table
}

mediation_moderation_add_column_group_bottom_border <- function(table, column) {
  if (!is.data.frame(table) || nrow(table) == 0L || !column %in% names(table)) {
    return(table)
  }
  values <- as.character(table[[column]] %||% character(0))
  if (length(values) != nrow(table)) {
    return(table)
  }
  group_end_rows <- which(c(values[-1L] != values[-length(values)], TRUE))
  mediation_moderation_add_group_bottom_border(table, group_end_rows)
}

mediation_moderation_add_path_group_bottom_border <- function(table) {
  mediation_moderation_add_column_group_bottom_border(table, "Path")
}

mediation_moderation_nowrap_column <- function(table, column) {
  if (!is.data.frame(table) || nrow(table) == 0L || !column %in% names(table)) {
    return(table)
  }
  styles <- data.frame(
    row = seq_len(nrow(table)),
    column = column,
    style = "white-space:nowrap !important;overflow-wrap:normal !important;word-break:normal !important;",
    stringsAsFactors = FALSE
  )
  attr(table, "cell_styles") <- rbind(attr(table, "cell_styles", exact = TRUE), styles)
  table
}

mediation_moderation_set_widths <- function(table, widths) {
  if (!is.data.frame(table) || nrow(table) == 0L || length(widths) == 0L) {
    return(table)
  }
  mapped <- unname(widths[names(table)])
  mapped[!is.finite(mapped) | mapped <= 0] <- 8
  mapped <- mapped / sum(mapped, na.rm = TRUE) * 100
  attr(table, "compact_column_widths") <- mapped
  table
}

mediation_moderation_path_coefficient_widths <- function(table) {
  widths <- c(
    Term = 24,
    B = 8,
    SE = 9,
    `HC3 SE` = 10,
    `Boot SE` = 10,
    LLCI = 9,
    ULCI = 9,
    t = 8,
    p = 7,
    `Boot p` = 8,
    f2 = 6,
    VIF = 8
  )
  mediation_moderation_set_widths(table, widths)
}

mediation_moderation_conditional_table_layout <- function(table) {
  if ("Condition" %in% names(table)) {
    condition_values <- trimws(as.character(table[["Condition"]] %||% ""))
    condition_values[is.na(condition_values)] <- ""
    if (length(condition_values) == 0L || all(!nzchar(condition_values))) {
      table[["Condition"]] <- NULL
    }
  }
  table <- mediation_moderation_nowrap_column(table, "Path")
  table <- mediation_moderation_set_widths(
    table,
    c(Path = 21, Moderator = 12, Condition = 13, Level = 14, W = 10, Effect = 9, SE = 7, t = 8, p = 6, LLCI = 7, ULCI = 7, Significant = 8)
  )
  attr(table, "column_display_labels") <- c(Moderator = "Moderator\nvariable", Condition = "Condition\nvalue")
  attr(table, "right_align_columns") <- unique(c(as.character(attr(table, "right_align_columns", exact = TRUE) %||% character(0)), "Effect"))
  table
}

mediation_moderation_interaction_effect_label <- function(result) {
  specs <- mediation_moderation_interaction_specs(result)
  labels <- vapply(specs, function(spec) {
    path <- mediation_moderation_path_display_label(result, spec)
    label <- sprintf("%s: %s x %s", path, spec$predictor, spec$moderator)
    if (!is.na(spec$conditioning_moderator %||% NA_character_) && nzchar(spec$conditioning_moderator %||% "")) {
      label <- sprintf("%s | %s", label, spec$conditioning_moderator)
    }
    label
  }, character(1))
  labels <- unique(labels[nzchar(labels)])
  if (length(labels) == 0L) {
    return(as.character(result$equation %||% ""))
  }
  paste(labels, collapse = "\n")
}

mediation_moderation_interaction_table_layout <- function(table) {
  table <- mediation_moderation_set_widths(
    table,
    c(Effect = 27, Test = 19, `R²-chng` = 10, LLCI = 9, ULCI = 9, F = 8, df1 = 8, df2 = 9, p = 8)
  )
  table
}

mediation_moderation_jn_table_layout <- function(table) {
  table <- mediation_moderation_nowrap_column(table, "Path")
  table <- mediation_moderation_set_widths(
    table,
    c(Path = 20, Moderator = 13, Condition = 14, `Moderator range` = 22, `Midpoint effect` = 16, p = 7, Significant = 8)
  )
  attr(table, "column_display_labels") <- c(
    Moderator = "Moderator\nvariable",
    Condition = "Condition\nvalue",
    `Moderator range` = "Moderator\nrange",
    `Midpoint effect` = "Midpoint\neffect"
  )
  table
}

mediation_moderation_process_summary_layout <- function(table) {
  table <- mediation_moderation_add_column_group_bottom_border(table, "X")
  mediation_moderation_set_widths(
    table,
    c(X = 12, Equation = 20, R = 9, `R²` = 10, MSE = 10, F = 11, df1 = 9, df2 = 10, p = 9)
  )
}

mediation_moderation_model_summary_process_table <- function(path_results) {
  groups <- list()
  for (result in path_results %||% list()) {
    if (is.null(result$model)) next
    key <- mediation_moderation_model_result_key(result)
    if (!nzchar(key)) {
      key <- paste0("row-", length(groups) + 1L)
    }
    if (!key %in% names(groups)) {
      groups[[key]] <- list(result = result, focal = character(0))
    }
    groups[[key]]$focal <- unique(c(groups[[key]]$focal, as.character(result$focal %||% "")))
  }
  rows <- lapply(groups, function(group) {
    result <- group$result
    mse <- stats::deviance(result$model) / stats::df.residual(result$model)
    data.frame(
      X = paste(group$focal[nzchar(group$focal)], collapse = ", "),
      Equation = result$equation,
      R = format_decimal3(sqrt(max(0, result$r_squared))),
      `R²` = format_decimal3(result$r_squared),
      MSE = format_decimal3(mse),
      F = format_decimal3(result$f_statistic),
      df1 = format_decimal3(result$f_df1),
      df2 = format_decimal3(result$f_df2),
      p = format_p(result$f_p),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  mediation_moderation_process_summary_layout(analysis_bind_rows(rows))
}

mediation_moderation_interaction_change_table <- function(path_results) {
  rows <- list()
  seen <- character(0)
  for (result in path_results %||% list()) {
    group <- mediation_moderation_hierarchical_steps(result)
    if (is.null(group) || length(group) < 2L) next
    result_key <- mediation_moderation_interaction_result_key(result)
    if (nzchar(result_key) && result_key %in% seen) next
    if (nzchar(result_key)) seen <- c(seen, result_key)
    previous <- group[[1L]]
    current <- group[[2L]]
    df1 <- current$f_df1 - previous$f_df1
    df2 <- current$f_df2
    delta_r2 <- current$r_squared - previous$r_squared
    f_change <- if (is.finite(delta_r2) && is.finite(df1) && is.finite(df2) && df1 > 0 && df2 > 0) {
      (delta_r2 / df1) / ((1 - current$r_squared) / df2)
    } else {
      NA_real_
    }
    test_label <- "F change"
    llci <- NA_real_
    ulci <- NA_real_
    p_change <- if (is.finite(f_change)) stats::pf(f_change, df1, df2, lower.tail = FALSE) else NA_real_
    if (isTRUE(previous$use_bootstrap) || isTRUE(current$use_bootstrap)) {
      previous_r2 <- mediation_moderation_numeric_vector(previous$bootstrap_r_squared %||% numeric(0))
      current_r2 <- mediation_moderation_numeric_vector(current$bootstrap_r_squared %||% numeric(0))
      count <- min(length(previous_r2), length(current_r2))
      delta_samples <- if (count > 0L) current_r2[seq_len(count)] - previous_r2[seq_len(count)] else numeric(0)
      delta_samples <- delta_samples[is.finite(delta_samples)]
      if (length(delta_samples) > 0L) {
        ci <- bootstrap_ci(delta_r2, delta_samples, method = current$bootstrap_ci_method %||% "bias_corrected")
        llci <- ci[[1L]]
        ulci <- ci[[2L]]
        lower <- (sum(delta_samples <= 0, na.rm = TRUE) + 1) / (length(delta_samples) + 1)
        upper <- (sum(delta_samples >= 0, na.rm = TRUE) + 1) / (length(delta_samples) + 1)
        p_change <- min(1, 2 * min(lower, upper))
      }
      test_label <- sprintf("%s bootstrap", bootstrap_ci_method_label(current$bootstrap_ci_method))
      f_change <- NA_real_
    } else if (isTRUE(previous$use_hc3) || isTRUE(current$use_hc3)) {
      p_change <- hierarchical_robust_wald_f_p(previous, current)
      test_label <- "Robust Wald F"
    }
    rows[[length(rows) + 1L]] <- data.frame(
      Effect = mediation_moderation_interaction_effect_label(result),
      Test = test_label,
      `R²-chng` = format_decimal3(delta_r2),
      LLCI = format_decimal3(llci),
      ULCI = format_decimal3(ulci),
      F = format_decimal3(f_change),
      df1 = format_decimal3(df1),
      df2 = format_decimal3(df2),
      p = format_p(p_change),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  mediation_moderation_interaction_table_layout(analysis_bind_rows(rows))
}

mediation_moderation_coef_vcov <- function(result) {
  if (isTRUE(result$use_hc3)) {
    robust_vcov <- tryCatch(sandwich::vcovHC(result$model, type = "HC3"), error = function(e) NULL)
    if (!is.null(robust_vcov)) {
      return(robust_vcov)
    }
  }
  stats::vcov(result$model)
}

mediation_moderation_interaction_p <- function(result, term) {
  term <- mediation_moderation_clean_term(term)
  if (isTRUE(result$use_bootstrap) && is.data.frame(result$boot_table) && nrow(result$boot_table) > 0L) {
    matched <- which(mediation_moderation_clean_term(result$boot_table$Term) == term)
    if (length(matched) > 0L && "Boot_p" %in% names(result$boot_table)) {
      return(mediation_moderation_numeric_scalar(result$boot_table$Boot_p[[matched[[1L]]]]))
    }
  }
  coef_table <- result$coef_table
  if (!is.data.frame(coef_table) || nrow(coef_table) == 0L || !"p" %in% names(coef_table)) {
    return(NA_real_)
  }
  matched <- which(mediation_moderation_clean_term(coef_table$Term) == term)
  if (length(matched) == 0L) {
    return(NA_real_)
  }
  mediation_moderation_numeric_scalar(coef_table$p[[matched[[1L]]]])
}

mediation_moderation_conditional_values <- function(model, moderator) {
  frame <- stats::model.frame(model)
  if (!moderator %in% names(frame)) {
    return(NULL)
  }
  if (is.factor(frame[[moderator]])) {
    levels <- levels(frame[[moderator]])
    levels <- levels[!is.na(levels) & nzchar(levels)]
    if (length(levels) == 0L) {
      return(NULL)
    }
    return(data.frame(
      Level = as.character(levels),
      W = as.character(levels),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.numeric(frame[[moderator]])) {
    values <- unique(as.character(frame[[moderator]][!is.na(frame[[moderator]])]))
    values <- values[nzchar(values)]
    if (length(values) == 0L) {
      return(NULL)
    }
    return(data.frame(Level = values, W = values, stringsAsFactors = FALSE))
  }
  values <- frame[[moderator]]
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(NULL)
  }
  center <- mean(values)
  spread <- stats::sd(values)
  if (!is.finite(spread)) spread <- 0
  data.frame(
    Level = c("Low (M-SD)", "Mean", "High (M+SD)"),
    W = c(center - spread, center, center + spread),
    stringsAsFactors = FALSE
  )
}

mediation_moderation_weight_value <- function(coefficients, weights) {
  weights <- weights[is.finite(weights) & nzchar(names(weights))]
  matched <- intersect(names(weights), names(coefficients))
  if (length(matched) == 0L) {
    return(NA_real_)
  }
  sum(coefficients[matched] * weights[matched], na.rm = TRUE)
}

mediation_moderation_weight_variance <- function(covariance, weights) {
  weights <- weights[is.finite(weights) & nzchar(names(weights))]
  matched <- intersect(names(weights), rownames(covariance))
  matched <- intersect(matched, colnames(covariance))
  if (length(matched) == 0L) {
    return(NA_real_)
  }
  as.numeric(t(weights[matched]) %*% covariance[matched, matched, drop = FALSE] %*% weights[matched])
}

mediation_moderation_weight_covariance <- function(covariance, weights1, weights2) {
  weights1 <- weights1[is.finite(weights1) & nzchar(names(weights1))]
  weights2 <- weights2[is.finite(weights2) & nzchar(names(weights2))]
  matched1 <- intersect(names(weights1), rownames(covariance))
  matched2 <- intersect(names(weights2), colnames(covariance))
  if (length(matched1) == 0L || length(matched2) == 0L) {
    return(NA_real_)
  }
  as.numeric(t(weights1[matched1]) %*% covariance[matched1, matched2, drop = FALSE] %*% weights2[matched2])
}

mediation_moderation_conditional_effect_weights <- function(spec, conditioning_value = 0) {
  intercept_weights <- stats::setNames(1, spec$predictor_term)
  slope_weights <- stats::setNames(1, spec$interaction_term)
  if (!is.na(spec$conditioning_term %||% NA_character_) && nzchar(spec$conditioning_term)) {
    intercept_weights[[spec$conditioning_term]] <- conditioning_value
  }
  if (!is.na(spec$three_way_term %||% NA_character_) && nzchar(spec$three_way_term)) {
    slope_weights[[spec$three_way_term]] <- conditioning_value
  }
  list(intercept = intercept_weights, slope = slope_weights)
}

mediation_moderation_conditional_moderation_weights <- function(spec) {
  if (is.na(spec$three_way_term %||% NA_character_) || !nzchar(spec$three_way_term)) {
    return(NULL)
  }
  list(
    intercept = stats::setNames(1, spec$interaction_term),
    slope = stats::setNames(1, spec$three_way_term)
  )
}

mediation_moderation_simple_slope_row <- function(result, spec, w_value, level = "", conditioning_value = 0, conditioning_level = "") {
  model <- result$model
  coefficients <- stats::coef(model)
  conditions <- stats::setNames(list(w_value), spec$moderator)
  if (!is.na(spec$conditioning_moderator %||% NA_character_) && nzchar(spec$conditioning_moderator %||% "")) {
    conditions[[spec$conditioning_moderator]] <- conditioning_value
  }
  slope_weights <- mediation_moderation_model_slope_weights(model, spec$predictor, conditions)
  if (is.null(slope_weights) || length(slope_weights) == 0L) {
    return(NULL)
  }
  needed <- names(slope_weights)
  if (!all(needed %in% names(coefficients))) {
    return(NULL)
  }
  effect <- mediation_moderation_weight_value(coefficients, slope_weights)
  covariance <- mediation_moderation_coef_vcov(result)
  variance <- mediation_moderation_weight_variance(covariance, slope_weights)
  se <- sqrt(max(0, variance))
  df <- stats::df.residual(model)
  t_value <- effect / se
  p_value <- 2 * stats::pt(abs(t_value), df = df, lower.tail = FALSE)
  critical <- stats::qt(.975, df = df)
  data.frame(
    Path = mediation_moderation_path_display_label(result, spec),
    Moderator = spec$moderator,
    Condition = if (nzchar(conditioning_level)) sprintf("%s %s", spec$conditioning_moderator, conditioning_level) else "",
    Level = level,
    W = if (is.numeric(w_value)) format_decimal3(w_value) else as.character(w_value),
    Effect = format_decimal3(effect),
    SE = format_decimal3(se),
    t = format_decimal3(t_value),
    p = format_p(p_value),
    LLCI = format_decimal3(effect - critical * se),
    ULCI = format_decimal3(effect + critical * se),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

mediation_moderation_simple_slopes_table <- function(path_results) {
  rows <- list()
  seen <- character(0)
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      spec_key <- mediation_moderation_interaction_spec_key(result, spec)
      if (nzchar(spec_key) && spec_key %in% seen) next
      if (nzchar(spec_key)) seen <- c(seen, spec_key)
      values <- mediation_moderation_conditional_values(result$model, spec$moderator)
      if (!is.data.frame(values) || nrow(values) == 0L) next
      condition_values <- if (!is.na(spec$conditioning_term %||% NA_character_) && nzchar(spec$conditioning_term) && !is.na(spec$conditioning_moderator %||% NA_character_)) {
        mediation_moderation_conditional_values(result$model, spec$conditioning_moderator)
      } else {
        data.frame(Level = "", W = 0, stringsAsFactors = FALSE)
      }
      for (condition_index in seq_len(nrow(condition_values))) {
        for (index in seq_len(nrow(values))) {
          rows[[length(rows) + 1L]] <- mediation_moderation_simple_slope_row(
            result,
            spec,
            values$W[[index]],
            values$Level[[index]],
            conditioning_value = condition_values$W[[condition_index]],
            conditioning_level = condition_values$Level[[condition_index]]
          )
        }
      }
    }
  }
  table <- analysis_bind_rows(rows)
  table <- mediation_moderation_add_path_group_bottom_border(table)
  mediation_moderation_conditional_table_layout(table)
}

mediation_moderation_jn_intervals_for_weights <- function(
  result,
  path,
  moderator,
  moderator_values,
  intercept_weights,
  slope_weights,
  alpha = .05,
  condition = ""
) {
  model <- result$model
  w_values <- mediation_moderation_numeric_vector(moderator_values)
  w_values <- w_values[is.finite(w_values)]
  if (length(w_values) == 0L) {
    return(NULL)
  }
  coefficients <- stats::coef(model)
  covariance <- mediation_moderation_coef_vcov(result)
  needed <- unique(c(names(intercept_weights), names(slope_weights)))
  if (!all(needed %in% names(coefficients))) {
    return(NULL)
  }
  b1 <- mediation_moderation_weight_value(coefficients, intercept_weights)
  b3 <- mediation_moderation_weight_value(coefficients, slope_weights)
  v11 <- mediation_moderation_weight_variance(covariance, intercept_weights)
  v13 <- mediation_moderation_weight_covariance(covariance, intercept_weights, slope_weights)
  v33 <- mediation_moderation_weight_variance(covariance, slope_weights)
  if (!all(is.finite(c(b1, b3, v11, v13, v33)))) {
    return(NULL)
  }
  df <- stats::df.residual(model)
  critical <- stats::qt(1 - alpha / 2, df = df)
  quadratic <- c(
    a = b3^2 - critical^2 * v33,
    b = 2 * b1 * b3 - 2 * critical^2 * v13,
    c = b1^2 - critical^2 * v11
  )
  w_min <- min(w_values)
  w_max <- max(w_values)
  roots <- numeric(0)
  if (abs(quadratic[["a"]]) < .Machine$double.eps^0.5) {
    if (abs(quadratic[["b"]]) > .Machine$double.eps^0.5) {
      roots <- -quadratic[["c"]] / quadratic[["b"]]
    }
  } else {
    discriminant <- quadratic[["b"]]^2 - 4 * quadratic[["a"]] * quadratic[["c"]]
    if (is.finite(discriminant) && discriminant >= 0) {
      roots <- c(
        (-quadratic[["b"]] - sqrt(discriminant)) / (2 * quadratic[["a"]]),
        (-quadratic[["b"]] + sqrt(discriminant)) / (2 * quadratic[["a"]])
      )
    }
  }
  roots <- sort(unique(roots[is.finite(roots) & roots >= w_min & roots <= w_max]))
  cuts <- sort(unique(c(w_min, roots, w_max)))
  if (length(cuts) < 2L) {
    cuts <- c(w_min, w_max)
  }
  rows <- list()
  for (index in seq_len(length(cuts) - 1L)) {
    lower <- cuts[[index]]
    upper <- cuts[[index + 1L]]
    midpoint <- mean(c(lower, upper))
    effect <- b1 + midpoint * b3
    variance <- v11 + (midpoint^2) * v33 + 2 * midpoint * v13
    se <- sqrt(max(0, variance))
    t_value <- effect / se
    p_value <- 2 * stats::pt(abs(t_value), df = df, lower.tail = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(
      Path = path,
      Moderator = moderator,
      Condition = condition,
      `Moderator range` = sprintf("%s to %s", format_decimal3(lower), format_decimal3(upper)),
      `Midpoint effect` = format_decimal3(effect),
      p = format_p(p_value),
      Significant = if (isTRUE(is.finite(p_value) && p_value < alpha)) "Yes" else "No",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  attr(rows, "roots") <- roots
  mediation_moderation_jn_table_layout(mediation_moderation_add_path_group_bottom_border(analysis_bind_rows(rows)))
}

mediation_moderation_jn_intervals <- function(result, spec, alpha = .05) {
  model <- result$model
  frame <- stats::model.frame(model)
  if (!spec$moderator %in% names(frame) || !is.numeric(frame[[spec$moderator]])) {
    return(NULL)
  }
  condition_values <- if (!is.na(spec$conditioning_term %||% NA_character_) && nzchar(spec$conditioning_term) && !is.na(spec$conditioning_moderator %||% NA_character_)) {
    mediation_moderation_conditional_values(model, spec$conditioning_moderator)
  } else {
    data.frame(Level = "", W = 0, stringsAsFactors = FALSE)
  }
  rows <- list()
  for (condition_index in seq_len(nrow(condition_values))) {
    weights <- mediation_moderation_conditional_effect_weights(spec, condition_values$W[[condition_index]])
    condition <- if (nzchar(condition_values$Level[[condition_index]])) {
      sprintf("%s %s", spec$conditioning_moderator, condition_values$Level[[condition_index]])
    } else {
      ""
    }
    jn_rows <- mediation_moderation_jn_intervals_for_weights(
      result = result,
      path = mediation_moderation_path_display_label(result, spec),
      moderator = spec$moderator,
      moderator_values = frame[[spec$moderator]],
      intercept_weights = weights$intercept,
      slope_weights = weights$slope,
      alpha = alpha,
      condition = condition
    )
    if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
      rows[[length(rows) + 1L]] <- jn_rows
    }
  }
  mediation_moderation_jn_table_layout(mediation_moderation_add_path_group_bottom_border(analysis_bind_rows(rows)))
}

mediation_moderation_jn_detail_values <- function(result, spec, n_points = 25L) {
  frame <- stats::model.frame(result$model)
  if (!spec$moderator %in% names(frame) || !is.numeric(frame[[spec$moderator]])) {
    return(numeric(0))
  }
  w_values <- frame[[spec$moderator]]
  w_values <- w_values[is.finite(w_values)]
  if (length(w_values) < 2L) {
    return(numeric(0))
  }
  grid <- seq(min(w_values), max(w_values), length.out = max(2L, as.integer(n_points)))
  jn_rows <- mediation_moderation_jn_intervals(result, spec)
  jn_points <- numeric(0)
  if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
    ranges <- as.character(jn_rows[["Moderator range"]] %||% character(0))
    parsed <- unlist(strsplit(gsub("\\s+to\\s+", " ", ranges), "\\s+"), use.names = FALSE)
    jn_points <- mediation_moderation_numeric_vector(parsed)
    jn_points <- jn_points[is.finite(jn_points) & jn_points > min(w_values) & jn_points < max(w_values)]
  }
  sort(unique(round(c(grid, jn_points), 10)))
}

mediation_moderation_conditional_moderation_jn_table <- function(result, spec, alpha = .05) {
  if (is.na(spec$three_way_term %||% NA_character_) || !nzchar(spec$three_way_term)) {
    return(NULL)
  }
  if (is.na(spec$conditioning_moderator %||% NA_character_) || !nzchar(spec$conditioning_moderator)) {
    return(NULL)
  }
  frame <- stats::model.frame(result$model)
  if (!spec$conditioning_moderator %in% names(frame) || !is.numeric(frame[[spec$conditioning_moderator]])) {
    return(NULL)
  }
  weights <- mediation_moderation_conditional_moderation_weights(spec)
  if (is.null(weights)) {
    return(NULL)
  }
  path <- sprintf(
    "Conditional moderation: %s by %s",
    mediation_moderation_path_display_label(result, spec),
    spec$moderator
  )
  mediation_moderation_jn_intervals_for_weights(
    result = result,
    path = path,
    moderator = spec$conditioning_moderator,
    moderator_values = frame[[spec$conditioning_moderator]],
    intercept_weights = weights$intercept,
    slope_weights = weights$slope,
    alpha = alpha,
    condition = sprintf("Moderation term: %s:%s", spec$predictor, spec$moderator)
  )
}

mediation_moderation_jn_detail_table <- function(path_results, n_points = 25L, alpha = .05) {
  rows <- list()
  seen <- character(0)
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      spec_key <- mediation_moderation_interaction_spec_key(result, spec)
      if (nzchar(spec_key) && spec_key %in% seen) next
      if (nzchar(spec_key)) seen <- c(seen, spec_key)
      if (!is.na(spec$conditioning_term %||% NA_character_) && nzchar(spec$conditioning_term)) next
      interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
      if (!is.finite(interaction_p) || interaction_p >= alpha) next
      values <- mediation_moderation_jn_detail_values(result, spec, n_points = n_points)
      if (length(values) == 0L) next
      for (w_value in values) {
        row <- mediation_moderation_simple_slope_row(result, spec, w_value, "")
        if (!is.data.frame(row) || nrow(row) == 0L) next
        p_text <- row$p[[1]]
        if (is.list(p_text)) {
          p_text <- unlist(p_text, recursive = TRUE, use.names = FALSE)
        }
        p_value <- mediation_moderation_numeric_scalar(sub("^\\.", "0.", sub("^<", "", as.character(p_text))))
        row$Level <- NULL
        row$Significant <- if (isTRUE(is.finite(p_value) && p_value < alpha)) "Yes" else "No"
        rows[[length(rows) + 1L]] <- row
      }
    }
  }
  table <- analysis_bind_rows(rows)
  table <- table[, setdiff(names(table), c("X", "Equation")), drop = FALSE]
  table <- unique(table)
  table <- mediation_moderation_add_path_group_bottom_border(table)
  mediation_moderation_conditional_table_layout(table)
}

mediation_moderation_johnson_neyman_table <- function(path_results) {
  rows <- list()
  seen <- character(0)
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      spec_key <- mediation_moderation_interaction_spec_key(result, spec)
      if (nzchar(spec_key) && spec_key %in% seen) next
      if (nzchar(spec_key)) seen <- c(seen, spec_key)
      interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
      three_way_p <- if (!is.na(spec$three_way_term %||% NA_character_) && nzchar(spec$three_way_term)) {
        mediation_moderation_interaction_p(result, spec$three_way_term)
      } else {
        NA_real_
      }
      if (isTRUE(is.finite(interaction_p) && interaction_p < .05) || isTRUE(is.finite(three_way_p) && three_way_p < .05)) {
        jn_rows <- mediation_moderation_jn_intervals(result, spec)
        if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
          rows[[length(rows) + 1L]] <- jn_rows
        }
      }
      if (isTRUE(is.finite(three_way_p) && three_way_p < .05)) {
        moderation_rows <- mediation_moderation_conditional_moderation_jn_table(result, spec)
        if (is.data.frame(moderation_rows) && nrow(moderation_rows) > 0L) {
          rows[[length(rows) + 1L]] <- moderation_rows
        }
      }
    }
  }
  table <- analysis_bind_rows(rows)
  table <- mediation_moderation_add_path_group_bottom_border(table)
  mediation_moderation_jn_table_layout(table)
}

mediation_moderation_prediction_base_row <- function(model) {
  frame <- stats::model.frame(model)
  as.data.frame(lapply(frame, function(column) {
    if (is.numeric(column)) {
      return(mean(column, na.rm = TRUE))
    }
    if (is.factor(column)) {
      tab <- sort(table(column), decreasing = TRUE)
      return(factor(names(tab)[[1L]], levels = levels(column)))
    }
    values <- column[!is.na(column)]
    if (length(values) == 0L) return(NA)
    values[[1L]]
  }), stringsAsFactors = FALSE)
}

mediation_moderation_plot_variable_label <- function(result, name) {
  label <- mediation_moderation_display_name(name, result$variable_info, result$labels)
  if (length(label) == 0L || is.na(label) || !nzchar(label) || identical(label, "-")) {
    return(as.character(name %||% ""))
  }
  label
}

mediation_moderation_plot_equation_label <- function(result) {
  equation <- as.character(result$equation %||% "")[[1]]
  outcome <- tryCatch(all.vars(stats::formula(result$model))[[1]], error = function(e) "")
  outcome_label <- mediation_moderation_plot_variable_label(result, outcome)
  if (grepl("^M model:", equation)) {
    return(sprintf("M model: %s", outcome_label))
  }
  if (identical(equation, "Y model")) {
    return(sprintf("Y model: %s", outcome_label))
  }
  if (nzchar(equation)) equation else outcome_label
}

mediation_moderation_conditional_plot_spec <- function(result, spec) {
  frame <- stats::model.frame(result$model)
  if (!all(c(spec$predictor, spec$moderator) %in% names(frame))) {
    return(NULL)
  }
  if (!is.numeric(frame[[spec$predictor]]) || !is.numeric(frame[[spec$moderator]])) {
    return(NULL)
  }
  interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
  if (!is.finite(interaction_p) || interaction_p >= .05) {
    return(NULL)
  }
  x_values <- frame[[spec$predictor]]
  x_values <- x_values[is.finite(x_values)]
  w_values <- mediation_moderation_conditional_values(result$model, spec$moderator)
  if (length(x_values) == 0L || !is.data.frame(w_values) || nrow(w_values) == 0L) {
    return(NULL)
  }
  x_grid <- seq(min(x_values), max(x_values), length.out = 40L)
  base_row <- mediation_moderation_prediction_base_row(result$model)
  plot_df <- do.call(rbind, lapply(seq_len(nrow(w_values)), function(index) {
    newdata <- base_row[rep(1L, length(x_grid)), , drop = FALSE]
    newdata[[spec$predictor]] <- x_grid
    newdata[[spec$moderator]] <- w_values$W[[index]]
    data.frame(
      moderator_level = c("M-SD", "Mean", "M+SD")[[index]],
      moderator_label = w_values$Level[[index]],
      x = x_grid,
      yhat = as.numeric(stats::predict(result$model, newdata = newdata)),
      stringsAsFactors = FALSE
    )
  }))
  predictor_label <- mediation_moderation_plot_variable_label(result, spec$predictor)
  moderator_label <- mediation_moderation_plot_variable_label(result, spec$moderator)
  outcome <- all.vars(stats::formula(result$model))[[1]]
  outcome_label <- mediation_moderation_plot_variable_label(result, outcome)
  list(
    title = sprintf("%s: %s by %s", mediation_moderation_plot_equation_label(result), predictor_label, moderator_label),
    x_label = predictor_label,
    y_label = outcome_label,
    moderator = spec$moderator,
    moderator_label = moderator_label,
    kind = "moderation",
    plot_df = plot_df
  )
}

mediation_moderation_jn_plot_spec <- function(result, spec, n_points = 200L) {
  if (!is.na(spec$conditioning_term %||% NA_character_) && nzchar(spec$conditioning_term)) {
    return(NULL)
  }
  frame <- stats::model.frame(result$model)
  if (!spec$moderator %in% names(frame) || !is.numeric(frame[[spec$moderator]])) {
    return(NULL)
  }
  interaction_p <- mediation_moderation_interaction_p(result, spec$interaction)
  if (!is.finite(interaction_p) || interaction_p >= .05) {
    return(NULL)
  }
  coefficients <- stats::coef(result$model)
  covariance <- mediation_moderation_coef_vcov(result)
  predictor_term <- spec$predictor_term
  interaction_term <- spec$interaction_term
  if (!all(c(predictor_term, interaction_term) %in% names(coefficients))) {
    return(NULL)
  }
  w_obs <- frame[[spec$moderator]]
  w_obs <- w_obs[is.finite(w_obs)]
  if (length(w_obs) < 2L) {
    return(NULL)
  }
  w_grid <- seq(min(w_obs), max(w_obs), length.out = n_points)
  df <- stats::df.residual(result$model)
  tcrit <- stats::qt(.975, df = df)
  effect <- coefficients[[predictor_term]] + coefficients[[interaction_term]] * w_grid
  se <- sqrt(pmax(
    covariance[predictor_term, predictor_term] +
      2 * w_grid * covariance[predictor_term, interaction_term] +
      (w_grid^2) * covariance[interaction_term, interaction_term],
    0
  ))
  plot_df <- data.frame(
    moderator_value = w_grid,
    conditional_effect = effect,
    se = se,
    llci = effect - tcrit * se,
    ulci = effect + tcrit * se,
    stringsAsFactors = FALSE
  )
  jn_rows <- mediation_moderation_jn_intervals(result, spec)
  jn_points <- numeric(0)
  if (is.data.frame(jn_rows) && nrow(jn_rows) > 0L) {
    # J-N roots are also recovered from the CI crossing so plotted labels stay
    # aligned with the displayed confidence band.
    add_roots <- function(y) {
      roots <- numeric(0)
      for (index in seq_len(length(w_grid) - 1L)) {
        y1 <- y[[index]]
        y2 <- y[[index + 1L]]
        if (!is.finite(y1) || !is.finite(y2) || sign(y1) == sign(y2)) next
        roots <- c(roots, w_grid[[index]] - y1 * (w_grid[[index + 1L]] - w_grid[[index]]) / (y2 - y1))
      }
      roots
    }
    jn_points <- sort(unique(round(c(add_roots(plot_df$llci), add_roots(plot_df$ulci)), 10)))
    jn_points <- jn_points[is.finite(jn_points) & jn_points >= min(w_obs) & jn_points <= max(w_obs)]
  }
  predictor_label <- mediation_moderation_plot_variable_label(result, spec$predictor)
  moderator_label <- mediation_moderation_plot_variable_label(result, spec$moderator)
  outcome <- all.vars(stats::formula(result$model))[[1]]
  outcome_label <- mediation_moderation_plot_variable_label(result, outcome)
  list(
    title = sprintf("Johnson-Neyman: %s", mediation_moderation_path_display_label(result, spec)),
    x_label = predictor_label,
    y_label = outcome_label,
    moderator = spec$moderator,
    moderator_label = moderator_label,
    kind = "johnson_neyman",
    plot_df = plot_df,
    jn_points = jn_points
  )
}

mediation_moderation_vcov_value <- function(result, term1, term2 = term1) {
  if (is.null(result$model) || is.na(term1) || is.na(term2)) return(0)
  covariance <- mediation_moderation_coef_vcov(result)
  if (!all(c(term1, term2) %in% rownames(covariance)) || !all(c(term1, term2) %in% colnames(covariance))) {
    return(0)
  }
  as.numeric(covariance[term1, term2])
}

mediation_moderation_indirect_jn_plot_specs <- function(path_results, n_points = 200L) {
  plots <- list()
  focal_values <- unique(vapply(path_results %||% list(), function(result) as.character(result$focal %||% ""), character(1)))
  focal_values <- focal_values[nzchar(focal_values)]
  for (focal in focal_values) {
    focal_results <- Filter(function(result) identical(as.character(result$focal %||% ""), focal), path_results %||% list())
    y_result <- NULL
    for (result in focal_results) {
      if (identical(as.character(result$equation %||% ""), "Y model")) {
        y_result <- result
        break
      }
    }
    if (is.null(y_result) || length(y_result$w) != 1L || !nzchar(y_result$w)) next
    w <- y_result$w
    mediator_results <- Filter(function(result) grepl("^M model:", as.character(result$equation %||% "")), focal_results)
    for (m_result in mediator_results) {
      mediator <- sub("^M model:\\s*", "", as.character(m_result$equation %||% ""))
      if (!nzchar(mediator)) next
      has_xm <- length(mediation_moderation_interaction_specs(m_result)) > 0L
      has_my <- any(vapply(mediation_moderation_interaction_specs(y_result), function(spec) identical(spec$predictor, mediator), logical(1)))
      if (!isTRUE(has_xm) && !isTRUE(has_my)) next
      xm_p <- if (isTRUE(has_xm)) mediation_moderation_interaction_p(m_result, paste0(focal, ":", w)) else NA_real_
      my_p <- if (isTRUE(has_my)) mediation_moderation_interaction_p(y_result, paste0(mediator, ":", w)) else NA_real_
      if (!isTRUE((is.finite(xm_p) && xm_p < .05) || (is.finite(my_p) && my_p < .05))) next
      frame <- stats::model.frame(if (isTRUE(has_xm)) m_result$model else y_result$model)
      if (!w %in% names(frame) || !is.numeric(frame[[w]])) next
      w_obs <- frame[[w]]
      w_obs <- w_obs[is.finite(w_obs)]
      if (length(w_obs) < 2L) next
      w_grid <- seq(min(w_obs), max(w_obs), length.out = n_points)

      a0_term <- mediation_moderation_match_coef_name(m_result$model, focal)
      a1_term <- if (isTRUE(has_xm)) mediation_moderation_match_coef_name(m_result$model, paste0(focal, ":", w)) else NA_character_
      b0_term <- mediation_moderation_match_coef_name(y_result$model, mediator)
      b1_term <- if (isTRUE(has_my)) mediation_moderation_match_coef_name(y_result$model, paste0(mediator, ":", w)) else NA_character_
      if (is.na(a0_term) || is.na(b0_term)) next
      a0 <- stats::coef(m_result$model)[[a0_term]]
      a1 <- if (!is.na(a1_term)) stats::coef(m_result$model)[[a1_term]] else 0
      b0 <- stats::coef(y_result$model)[[b0_term]]
      b1 <- if (!is.na(b1_term)) stats::coef(y_result$model)[[b1_term]] else 0
      a <- a0 + a1 * w_grid
      b <- b0 + b1 * w_grid
      var_a <- mediation_moderation_vcov_value(m_result, a0_term) +
        (w_grid^2) * mediation_moderation_vcov_value(m_result, a1_term) +
        2 * w_grid * mediation_moderation_vcov_value(m_result, a0_term, a1_term)
      var_b <- mediation_moderation_vcov_value(y_result, b0_term) +
        (w_grid^2) * mediation_moderation_vcov_value(y_result, b1_term) +
        2 * w_grid * mediation_moderation_vcov_value(y_result, b0_term, b1_term)
      effect <- a * b
      se <- sqrt(pmax((b^2) * var_a + (a^2) * var_b, 0))
      df <- min(stats::df.residual(m_result$model), stats::df.residual(y_result$model), na.rm = TRUE)
      tcrit <- if (is.finite(df) && df > 0) stats::qt(.975, df = df) else stats::qnorm(.975)
      plot_df <- data.frame(
        moderator_value = w_grid,
        conditional_effect = effect,
        se = se,
        llci = effect - tcrit * se,
        ulci = effect + tcrit * se,
        stringsAsFactors = FALSE
      )
      add_roots <- function(y) {
        roots <- numeric(0)
        for (index in seq_len(length(w_grid) - 1L)) {
          y1 <- y[[index]]
          y2 <- y[[index + 1L]]
          if (!is.finite(y1) || !is.finite(y2) || sign(y1) == sign(y2)) next
          roots <- c(roots, w_grid[[index]] - y1 * (w_grid[[index + 1L]] - w_grid[[index]]) / (y2 - y1))
        }
        roots
      }
      jn_points <- sort(unique(round(c(add_roots(plot_df$llci), add_roots(plot_df$ulci)), 10)))
      jn_points <- jn_points[is.finite(jn_points) & jn_points >= min(w_obs) & jn_points <= max(w_obs)]
      focal_label <- mediation_moderation_plot_variable_label(y_result, focal)
      y_label <- mediation_moderation_plot_variable_label(y_result, all.vars(stats::formula(y_result$model))[[1]])
      mediator_label <- mediation_moderation_plot_variable_label(y_result, mediator)
      moderator_label <- mediation_moderation_plot_variable_label(y_result, w)
      plots[[length(plots) + 1L]] <- list(
        title = sprintf("Johnson-Neyman: indirect effect through %s", mediator_label),
        x_label = focal_label,
        y_label = y_label,
        moderator = w,
        moderator_label = moderator_label,
        kind = "indirect_johnson_neyman",
        effect_label = sprintf("Conditional indirect effect of %s on %s through %s", focal_label, y_label, mediator_label),
        plot_df = plot_df,
        jn_points = jn_points
      )
    }
  }
  plots
}

mediation_moderation_conditional_plot_specs <- function(path_results) {
  plots <- list()
  for (result in path_results %||% list()) {
    for (spec in mediation_moderation_interaction_specs(result)) {
      plot_spec <- mediation_moderation_conditional_plot_spec(result, spec)
      if (!is.null(plot_spec)) {
        plots[[length(plots) + 1L]] <- plot_spec
      }
      jn_plot_spec <- mediation_moderation_jn_plot_spec(result, spec)
      if (!is.null(jn_plot_spec)) {
        plots[[length(plots) + 1L]] <- jn_plot_spec
      }
    }
  }
  plots <- c(plots, mediation_moderation_indirect_jn_plot_specs(path_results))
  plots
}

mediation_moderation_current_edition <- function() {
  edition <- if (exists("analysis_save_edition", mode = "function", inherits = TRUE)) {
    tryCatch(analysis_save_edition(), error = function(e) Sys.getenv("STATEDU_EDITION", "development"))
  } else {
    Sys.getenv("STATEDU_EDITION", "development")
  }
  edition <- tolower(as.character(edition %||% "development")[[1]])
  if (!edition %in% c("free", "pro", "development", "personal", "institution")) {
    edition <- "development"
  }
  edition
}

mediation_moderation_figure_dpi <- function() {
  if (identical(mediation_moderation_current_edition(), "free")) 300L else 600L
}

mediation_moderation_plot_theme <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "#222222"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "#222222"),
      plot.title = ggplot2::element_text(hjust = 0, size = 9, lineheight = 1.05),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 8.5),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 8.5),
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8.5)
    )
}

mediation_moderation_build_moderation_plot <- function(plot_spec) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  plot_df <- as.data.frame(plot_spec$plot_df, stringsAsFactors = FALSE)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0L) return(NULL)
  plot_df$moderator_level <- factor(as.character(plot_df$moderator_level), levels = c("M-SD", "Mean", "M+SD"))
  ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = yhat, color = moderator_level)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("M-SD" = "#1B5E20", "Mean" = "#1565C0", "M+SD" = "#D84315"), drop = FALSE) +
    ggplot2::labs(
      title = plot_spec$title,
      x = plot_spec$x_label,
      y = plot_spec$y_label,
      color = plot_spec$moderator_label %||% plot_spec$moderator
    ) +
    mediation_moderation_plot_theme() +
    ggplot2::theme(legend.position = "right")
}

mediation_moderation_build_jn_plot <- function(plot_spec) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  plot_df <- as.data.frame(plot_spec$plot_df, stringsAsFactors = FALSE)
  if (!is.data.frame(plot_df) || nrow(plot_df) == 0L) return(NULL)
  plot_df$sig_dir <- ifelse(plot_df$ulci < 0, "negative", ifelse(plot_df$llci > 0, "positive", "nonsignificant"))
  rect_df <- data.frame(stringsAsFactors = FALSE)
  run <- rle(as.character(plot_df$sig_dir))
  ends <- cumsum(run$lengths)
  starts <- c(1L, head(ends + 1L, -1L))
  for (index in seq_along(run$values)) {
    if (identical(run$values[[index]], "nonsignificant")) next
    segment <- plot_df[starts[[index]]:ends[[index]], , drop = FALSE]
    rect_df <- rbind(
      rect_df,
      data.frame(
        xmin = min(segment$moderator_value, na.rm = TRUE),
        xmax = max(segment$moderator_value, na.rm = TRUE),
        ymin = -Inf,
        ymax = Inf,
        sig_dir = run$values[[index]],
        stringsAsFactors = FALSE
      )
    )
  }
  jn_points <- mediation_moderation_numeric_vector(plot_spec$jn_points %||% numeric(0))
  subtitle <- if (length(jn_points) == 0L) {
    "No Johnson-Neyman transition point within the observed moderator range."
  } else {
    paste0("Johnson-Neyman point", if (length(jn_points) > 1L) "s" else "", ": ", paste(formatC(jn_points, format = "f", digits = 2), collapse = ", "))
  }
  plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = moderator_value, y = conditional_effect)) +
    {
      if (nrow(rect_df) > 0L) {
        ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sig_dir),
          inherit.aes = FALSE,
          alpha = 0.16,
          color = NA,
          show.legend = FALSE
        )
      }
    } +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "#666666") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = llci, ymax = ulci), fill = "#B0BEC5", alpha = 0.28, linewidth = 0, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = llci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(ggplot2::aes(y = ulci), linewidth = 0.45, alpha = 0.8, linetype = "22", color = "#607D8B") +
    ggplot2::geom_line(linewidth = 0.9, color = "#1565C0") +
    ggplot2::labs(
      title = plot_spec$title,
      subtitle = subtitle,
      x = plot_spec$moderator_label %||% plot_spec$moderator,
      y = plot_spec$effect_label %||% paste0("Conditional effect of ", plot_spec$x_label, " on ", plot_spec$y_label)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    mediation_moderation_plot_theme() +
    ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(5.5, 5.5, 16, 5.5))
  if (nrow(rect_df) > 0L) {
    plot <- plot + ggplot2::scale_fill_manual(
      values = c("negative" = "#BBDEFB", "positive" = "#FFE0B2", "nonsignificant" = "transparent"),
      drop = FALSE
    )
  }
  if (length(jn_points) > 0L) {
    y_rng <- range(c(plot_df$llci, plot_df$ulci), na.rm = TRUE)
    y_span <- diff(y_rng)
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1
    ann_df <- data.frame(
      x = jn_points,
      y = y_rng[[1L]] + 0.03 * y_span,
      lab = paste0("JN=", formatC(jn_points, format = "f", digits = 2)),
      stringsAsFactors = FALSE
    )
    plot <- plot +
      ggplot2::geom_vline(xintercept = jn_points, linewidth = 0.45, linetype = "42", color = "#424242") +
      ggplot2::geom_text(
        data = ann_df,
        ggplot2::aes(x = x, y = y, label = lab),
        inherit.aes = FALSE,
        hjust = -0.08,
        vjust = 1,
        size = 2.8,
        color = "#424242"
      )
  }
  plot
}

mediation_moderation_print_plot <- function(plot_spec) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    plot.new()
    text(0.5, 0.5, "Package 'ggplot2' is required for Johnson-Neyman plots.")
    return(invisible(NULL))
  }
  plot <- if (plot_spec$kind %in% c("johnson_neyman", "indirect_johnson_neyman")) {
    mediation_moderation_build_jn_plot(plot_spec)
  } else {
    mediation_moderation_build_moderation_plot(plot_spec)
  }
  if (is.null(plot)) {
    plot.new()
    text(0.5, 0.5, "No plot data.")
    return(invisible(NULL))
  }
  print(plot)
}

mediation_moderation_conditional_plot_tag <- function(plot_spec) {
  dpi <- mediation_moderation_figure_dpi()
  tags$div(
    class = "mm-conditional-plot-card",
    tags$img(
      src = plot_data_uri(
        mediation_moderation_print_plot,
        plot_spec,
        width = 6.8 * dpi,
        height = 4.8 * dpi,
        res = dpi
      ),
      style = "max-width:900px;width:100%;height:auto;"
    )
  )
}

mediation_moderation_conditional_plots_ui <- function(plot_specs) {
  plot_tags <- Filter(Negate(is.null), lapply(plot_specs %||% list(), mediation_moderation_conditional_plot_tag))
  if (length(plot_tags) == 0L) {
    return(NULL)
  }
  tags$div(
    class = "result-section regression-result-panel mm-conditional-plots-section",
    tags$h3("Conditional effect plots"),
    tags$div(class = "mm-conditional-plot-grid", plot_tags)
  )
}

mediation_moderation_result_mediator_slots <- function(spec) {
  slots <- as.character(spec$slots %||% character(0))
  mediator_slots <- slots[grepl("^m[0-9]*$", slots)]
  mediator_slots[nzchar(mediator_slots)]
}

mediation_moderation_compact_y_range <- function(values, factor = 0.75) {
  values <- mediation_moderation_numeric_vector(values)
  finite <- is.finite(values)
  if (sum(finite) < 2L) {
    return(values)
  }
  center <- mean(range(values[finite]))
  values[finite] <- center + (values[finite] - center) * factor
  values
}

mediation_moderation_result_column_y_positions <- function(count, center = 58) {
  count <- max(1L, as.integer(count %||% 1L))
  if (count == 1L) {
    return(center)
  }
  if (count == 2L) {
    return(c(center - 20, center + 20))
  }
  if (count == 3L) {
    return(c(center - 28, center, center + 28))
  }
  if (count == 4L) {
    return(c(20, 44, 68, 88))
  }
  seq(18, 88, length.out = count)
}

mediation_moderation_result_x_y_positions <- function(x_count, mediator_y, center = 58) {
  x_count <- max(1L, as.integer(x_count %||% 1L))
  mediator_y <- mediation_moderation_numeric_vector(mediator_y)
  mediator_y <- mediator_y[is.finite(mediator_y)]
  if (x_count == 1L) {
    return(center)
  }
  if (length(mediator_y) >= 2L) {
    return(seq(min(mediator_y), max(mediator_y), length.out = x_count))
  }
  mediation_moderation_result_column_y_positions(x_count, center)
}

mediation_moderation_result_layout_positions <- function(positions, x_slots, mediator_slots, has_w = FALSE, moderated_paths = character(0)) {
  center_y <- 58
  positions <- positions %||% list()
  x_slots <- as.character(x_slots %||% character(0))
  mediator_slots <- as.character(mediator_slots %||% character(0))
  moderated_paths <- intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy"))
  wide_multi <- length(x_slots) > 1L && length(mediator_slots) > 1L
  x_column <- if (isTRUE(wide_multi)) 8 else if (length(x_slots) > 1L) 16 else 20
  mediator_column <- 50
  y_column <- if (isTRUE(wide_multi)) 86 else 80
  x_column <- mediator_column - ((mediator_column - x_column) * 0.8)
  y_column <- mediator_column + ((y_column - mediator_column) * 0.8)
  mediator_y <- numeric(0)
  if (length(mediator_slots) > 0L) {
    mediator_y <- mediation_moderation_result_column_y_positions(length(mediator_slots), center_y)
    if (length(mediator_slots) %% 2L == 1L) {
      mediator_y[[ceiling(length(mediator_slots) / 2)]] <- center_y
    }
    for (index in seq_along(mediator_slots)) {
      positions[[mediator_slots[[index]]]] <- c(mediator_column, mediator_y[[index]])
    }
  }
  x_y <- mediation_moderation_result_x_y_positions(length(x_slots), mediator_y, center_y)
  for (index in seq_along(x_slots)) {
    positions[[x_slots[[index]]]] <- c(x_column, x_y[[index]])
  }
  if ("y" %in% names(positions)) {
    positions$y <- c(y_column, center_y)
  }
  if (isTRUE(has_w) && "w" %in% names(positions)) {
    base_w_x <- positions$w[[1]]
    rendered_x_column <- if (identical(x_slots, "x")) max(10, x_column - 4) else x_column
    rendered_y_column <- if ("y" %in% names(positions)) min(90, y_column + 4) else y_column
    w_x <- base_w_x
    if (length(mediator_slots) > 0L && "xm" %in% moderated_paths && !"my" %in% moderated_paths) {
      w_x <- mean(c(rendered_x_column, mediator_column))
    } else if (length(mediator_slots) > 0L && "my" %in% moderated_paths && !"xm" %in% moderated_paths) {
      w_x <- mean(c(mediator_column, rendered_y_column))
    } else if (length(x_slots) > 1L) {
      w_x <- 50
    }
    w_y_gap <- if (length(x_slots) >= 3L) 32L else 24L
    w_y <- min(x_y) - w_y_gap
    if (length(mediator_y) > 1L) {
      w_y <- min(w_y, min(mediator_y) - 15)
    }
    positions$w <- c(w_x, max(10L, w_y))
  }
  positions
}

mediation_moderation_unique_paths <- function(paths) {
  if (length(paths) == 0L) return(paths)
  keys <- vapply(paths, mediation_moderation_path_key, character(1))
  paths[!duplicated(keys)]
}

mediation_moderation_custom_snapshot_records <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  if (is.data.frame(value)) {
    return(lapply(seq_len(nrow(value)), function(index) as.list(value[index, , drop = FALSE])))
  }
  if (is.list(value)) {
    return(value)
  }
  list()
}

mediation_moderation_custom_record_value <- function(record, key, default = "") {
  value <- record[[key]] %||% default
  if (length(value) == 0L || is.null(value) || is.na(value[[1L]])) {
    return(default)
  }
  as.character(value[[1L]])
}

mediation_moderation_custom_node_variable <- function(node) {
  mediation_moderation_custom_record_value(
    node,
    "variableId",
    mediation_moderation_custom_record_value(node, "name", "")
  )
}

mediation_moderation_custom_snapshot_position_scale <- function(values) {
  values <- mediation_moderation_numeric_vector(values)
  if (length(values) == 0L || !any(is.finite(values))) {
    return(values)
  }
  finite <- values[is.finite(values)]
  if (min(finite) >= 0 && max(finite) <= 100) {
    return(values)
  }
  range <- range(finite)
  if (!is.finite(diff(range)) || diff(range) <= 0) {
    values[is.finite(values)] <- 50
    return(values)
  }
  10 + ((values - range[[1L]]) / diff(range)) * 80
}

mediation_moderation_custom_result_diagram_data <- function(result) {
  snapshot <- result$custom_model_canvas_result_snapshot %||% result$custom_model_canvas_snapshot %||% NULL
  roles <- result$roles
  if (!isTRUE(result$custom_model_canvas) || is.null(snapshot) || !is.list(roles)) {
    return(NULL)
  }
  nodes <- mediation_moderation_custom_snapshot_records(snapshot$nodes)
  edges <- mediation_moderation_custom_snapshot_records(snapshot$edges)
  moderations <- mediation_moderation_custom_snapshot_records(snapshot$moderations)
  if (length(nodes) == 0L) {
    return(NULL)
  }
  node_ids <- vapply(nodes, mediation_moderation_custom_record_value, character(1), key = "id")
  names(nodes) <- node_ids
  node_role <- function(node) mediation_moderation_custom_record_value(node, "role", "independent")
  role_variables <- list(
    independent = as.character(roles$x %||% character(0)),
    mediator = as.character(roles$mediators %||% character(0)),
    dependent = as.character(roles$y %||% character(0)),
    moderator = as.character(roles$w %||% character(0))
  )
  slot_for <- function(node) {
    role <- node_role(node)
    variable <- mediation_moderation_custom_node_variable(node)
    values <- as.character(role_variables[[role]] %||% character(0))
    index <- match(variable, values)
    if (is.na(index)) {
      index <- 1L
    }
    switch(
      role,
      independent = if (length(values) <= 1L) "x" else paste0("x", index),
      mediator = if (length(values) <= 1L) "m" else paste0("m", index),
      dependent = if (index <= 1L) "y" else paste0("y", index),
      moderator = if (index <= 1L) "w" else if (index == 2L) "z" else paste0("w", index),
      ""
    )
  }
  node_slots <- vapply(nodes, slot_for, character(1))
  valid_nodes <- nzchar(node_slots)
  nodes <- nodes[valid_nodes]
  node_slots <- node_slots[valid_nodes]
  if (length(nodes) == 0L) {
    return(NULL)
  }
  x_values <- vapply(nodes, function(node) mediation_moderation_numeric_scalar(node$x, NA_real_), numeric(1))
  y_values <- vapply(nodes, function(node) mediation_moderation_numeric_scalar(node$y, NA_real_), numeric(1))
  x_scaled <- mediation_moderation_custom_snapshot_position_scale(x_values)
  y_scaled <- mediation_moderation_custom_snapshot_position_scale(y_values)
  positions <- stats::setNames(lapply(seq_along(node_slots), function(index) {
    c(
      if (is.finite(x_scaled[[index]])) x_scaled[[index]] else 50,
      if (is.finite(y_scaled[[index]])) y_scaled[[index]] else 50
    )
  }), node_slots)
  slot_variables <- stats::setNames(
    vapply(nodes, mediation_moderation_custom_node_variable, character(1)),
    node_slots
  )
  node_slot_by_id <- stats::setNames(node_slots, names(nodes))
  edge_ids <- vapply(edges, mediation_moderation_custom_record_value, character(1), key = "id")
  names(edges) <- edge_ids
  edge_path_type <- function(edge) {
    from <- nodes[[mediation_moderation_custom_record_value(edge, "from")]] %||% NULL
    to <- nodes[[mediation_moderation_custom_record_value(edge, "to")]] %||% NULL
    if (is.null(from) || is.null(to)) {
      return("")
    }
    paste(node_role(from), node_role(to), sep = "->")
  }
  paths <- Filter(Negate(is.null), lapply(edges, function(edge) {
    from_slot <- node_slot_by_id[[mediation_moderation_custom_record_value(edge, "from")]] %||% ""
    to_slot <- node_slot_by_id[[mediation_moderation_custom_record_value(edge, "to")]] %||% ""
    if (!nzchar(from_slot) || !nzchar(to_slot)) {
      return(NULL)
    }
    c(from_slot, to_slot)
  }))
  edge_by_id <- edges
  for (moderation in moderations) {
    moderator_slot <- node_slot_by_id[[mediation_moderation_custom_record_value(moderation, "from")]] %||% ""
    target_edge <- edge_by_id[[mediation_moderation_custom_record_value(moderation, "toEdge")]] %||% NULL
    if (!nzchar(moderator_slot) || is.null(target_edge)) {
      next
    }
    from_slot <- node_slot_by_id[[mediation_moderation_custom_record_value(target_edge, "from")]] %||% ""
    to_slot <- node_slot_by_id[[mediation_moderation_custom_record_value(target_edge, "to")]] %||% ""
    if (!nzchar(from_slot) || !nzchar(to_slot)) {
      next
    }
    target <- switch(
      edge_path_type(target_edge),
      "independent->mediator" = paste("xm", from_slot, to_slot, sep = "_"),
      "mediator->dependent" = paste0("my_", from_slot),
      "independent->dependent" = paste0("xy_", from_slot),
      ""
    )
    if (nzchar(target)) {
      paths[[length(paths) + 1L]] <- c(moderator_slot, target)
    }
  }
  diagram_roles <- roles
  diagram_roles$slot_variables <- slot_variables
  title <- ""
  if (is.data.frame(result$overview) && all(c("Item", "Value") %in% names(result$overview))) {
    matched_title <- result$overview$Value[result$overview$Item == "Model"]
    title <- as.character(utils::head(matched_title, 1) %||% "")
  }
  if (!nzchar(title)) {
    title <- "User-defined mediation / moderation model"
  }
  spec <- list(
    model = "custom",
    recognized = FALSE,
    title = title,
    slots = names(positions),
    positions = positions,
    paths = mediation_moderation_unique_paths(paths),
    moderated_paths = as.character(result$moderated_paths %||% character(0)),
    structure = result$structure %||% "custom"
  )
  list(spec = spec, roles = diagram_roles)
}

mediation_moderation_result_diagram_data <- function(result) {
  custom_diagram <- mediation_moderation_custom_result_diagram_data(result)
  if (!is.null(custom_diagram)) {
    return(custom_diagram)
  }
  spec <- result$diagram_spec
  roles <- result$roles
  x_vars <- as.character(roles$x %||% character(0))
  x_vars <- x_vars[nzchar(x_vars)]
  if (!is.list(spec) || !is.list(roles) || length(x_vars) == 0L) {
    return(list(spec = spec, roles = roles))
  }
  model <- as.character(result$model_number %||% spec$model %||% "")[[1]]
  moderated_paths <- mediation_moderation_model_moderated_paths(model)
  positions <- spec$positions
  slots <- as.character(spec$slots %||% character(0))
  w_vars <- as.character(roles$w %||% character(0))
  w_vars <- w_vars[nzchar(w_vars)]
  has_result_w <- length(w_vars) >= 1L && "w" %in% slots && length(moderated_paths) > 0L
  has_result_z <- length(w_vars) >= 2L && "z" %in% slots && model %in% c("2", "3") && "xy" %in% moderated_paths
  if (!isTRUE(has_result_w)) {
    positions$w <- NULL
    positions$z <- NULL
    slots <- setdiff(slots, c("w", "z"))
  } else if (!isTRUE(has_result_z)) {
    positions$z <- NULL
    slots <- setdiff(slots, "z")
  }
  if (!isTRUE(has_result_w) || !isTRUE(has_result_z)) {
    spec$positions <- positions
    spec$slots <- slots
  }
  mediator_slots <- mediation_moderation_result_mediator_slots(spec)
  if (length(x_vars) <= 1L) {
    if (length(mediator_slots) <= 1L) {
      return(list(spec = spec, roles = roles))
    }
    positions <- mediation_moderation_result_layout_positions(
      positions,
      x_slots = "x",
      mediator_slots = mediator_slots,
      has_w = has_result_w && "w" %in% names(positions),
      moderated_paths = moderated_paths
    )
    spec$positions <- positions
    return(list(spec = spec, roles = roles))
  }
  x_slots <- paste0("x", seq_along(x_vars))
  mediators <- as.character(roles$mediators %||% character(0))
  moderated_x_to_m <- mediation_moderation_normalize_moderated_x_to_m(
    result$moderated_x_to_m,
    mediators,
    x_vars,
    moderated_paths
  )
  positions <- mediation_moderation_result_layout_positions(
    positions,
    x_slots = x_slots,
    mediator_slots = mediator_slots,
    has_w = has_result_w && "w" %in% names(positions),
    moderated_paths = moderated_paths
  )
  positions$x <- NULL
  paths <- list()
  if (length(mediator_slots) > 0L) {
    for (x_slot in x_slots) {
      for (mediator_slot in mediator_slots) {
        paths[[length(paths) + 1L]] <- c(x_slot, mediator_slot)
      }
      paths[[length(paths) + 1L]] <- c(x_slot, "y")
    }
    if (identical(model, "6") && length(mediator_slots) >= 2L) {
      paths[[length(paths) + 1L]] <- c(mediator_slots[[1L]], mediator_slots[[2L]])
      paths[[length(paths) + 1L]] <- c(mediator_slots[[1L]], "y")
      paths[[length(paths) + 1L]] <- c(mediator_slots[[2L]], "y")
    } else {
      for (mediator_slot in mediator_slots) {
        paths[[length(paths) + 1L]] <- c(mediator_slot, "y")
      }
    }
  } else {
    for (x_slot in x_slots) {
      paths[[length(paths) + 1L]] <- c(x_slot, "y")
    }
  }
  if (isTRUE(has_result_w) && "w" %in% names(positions)) {
    if ("xm" %in% moderated_paths) {
      for (x_index in seq_along(x_slots)) {
        x_slot <- x_slots[[x_index]]
        x_var <- x_vars[[x_index]]
        for (mediator_index in seq_along(mediator_slots)) {
          mediator_slot <- mediator_slots[[mediator_index]]
          mediator <- mediators[[mediator_index]]
          if (!x_var %in% as.character(moderated_x_to_m[[mediator]] %||% character(0))) next
          paths[[length(paths) + 1L]] <- c("w", paste("xm", x_slot, mediator_slot, sep = "_"))
        }
      }
    }
    if ("my" %in% moderated_paths) {
      for (mediator_slot in mediator_slots) {
        paths[[length(paths) + 1L]] <- c("w", paste0("my_", mediator_slot))
      }
    }
    if ("xy" %in% moderated_paths) {
      for (x_slot in x_slots) {
        paths[[length(paths) + 1L]] <- c("w", paste0("xy_", x_slot))
        if (isTRUE(has_result_z) && "z" %in% names(positions)) {
          paths[[length(paths) + 1L]] <- c("z", paste0("xy_", x_slot))
        }
      }
    }
  }
  slot_variables <- stats::setNames(x_vars, x_slots)
  if (length(mediator_slots) > 0L) {
    slot_variables <- c(slot_variables, stats::setNames(as.character(roles$mediators %||% character(0))[seq_along(mediator_slots)], mediator_slots))
  }
  if ("y" %in% names(positions)) {
    slot_variables <- c(slot_variables, y = as.character(roles$y %||% character(0))[[1]])
  }
  if (isTRUE(has_result_w) && "w" %in% names(positions) && length(roles$w) > 0L) {
    slot_variables <- c(slot_variables, w = as.character(roles$w %||% character(0))[[1]])
  }
  if (isTRUE(has_result_z) && "z" %in% names(positions) && length(roles$w) > 1L) {
    slot_variables <- c(slot_variables, z = as.character(roles$w %||% character(0))[[2]])
  }
  diagram_roles <- roles
  diagram_roles$slot_variables <- slot_variables
  spec$positions <- positions
  spec$slots <- c(
    x_slots,
    mediator_slots,
    if (isTRUE(has_result_w) && "w" %in% names(positions)) "w",
    if (isTRUE(has_result_z) && "z" %in% names(positions)) "z",
    intersect("y", names(positions))
  )
  spec$paths <- mediation_moderation_unique_paths(paths)
  list(spec = spec, roles = diagram_roles)
}

mediation_moderation_result_edge_coefficient_labels <- function(result, spec) {
  roles <- result$roles
  x_vars <- as.character(roles$x %||% character(0))
  x_vars <- x_vars[nzchar(x_vars)]
  mediator_slots <- mediation_moderation_result_mediator_slots(spec)
  if (length(x_vars) <= 1L && length(mediator_slots) <= 1L) {
    return(mediation_moderation_edge_coefficient_labels(result$path_results))
  }
  x_slots <- if (length(x_vars) == 1L) "x" else paste0("x", seq_along(x_vars))
  x_map <- stats::setNames(x_slots, x_vars)
  mediators <- as.character(roles$mediators %||% character(0))
  moderated_paths <- if (isTRUE(result$custom_model_canvas) || identical(as.character(spec$model %||% ""), "custom")) {
    as.character(result$moderated_paths %||% character(0))
  } else {
    mediation_moderation_model_moderated_paths(result$model_number %||% spec$model %||% "")
  }
  moderated_x_to_m <- mediation_moderation_normalize_moderated_x_to_m(
    result$moderated_x_to_m,
    mediators,
    x_vars,
    moderated_paths
  )
  m_map <- stats::setNames(mediator_slots, mediators[seq_along(mediator_slots)])
  labels <- list()
  for (path_result in result$path_results %||% list()) {
    focal <- as.character(path_result$focal %||% "")[[1]]
    x_slot <- unname(x_map[[focal]] %||% "")
    if (!nzchar(x_slot)) next
    moderators <- as.character(path_result$w %||% character(0))
    moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
    w <- utils::head(moderators, 1)
    z <- mediation_moderation_second_moderator(moderators)
    equation <- as.character(path_result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      mediator <- trimws(sub("^M model:\\s*", "", equation))
      m_slot <- unname(m_map[[mediator]] %||% "")
      if (!nzchar(m_slot)) next
      labels[[paste0(x_slot, "->", m_slot)]] <- mediation_moderation_path_coefficient_label(path_result, focal)
      if (length(w) >= 1L && nzchar(w) && focal %in% as.character(moderated_x_to_m[[mediator]] %||% character(0))) {
        labels[[paste0("w->xm_", x_slot, "_", m_slot)]] <- mediation_moderation_path_coefficient_label(path_result, paste0(focal, ":", w))
      }
    } else if (identical(equation, "Y model")) {
      labels[[paste0(x_slot, "->y")]] <- mediation_moderation_path_coefficient_label(path_result, focal)
      for (mediator in mediators) {
        m_slot <- unname(m_map[[mediator]] %||% "")
        if (!nzchar(m_slot)) next
        key <- paste0(m_slot, "->y")
        if (is.null(labels[[key]]) || !nzchar(labels[[key]])) {
          labels[[key]] <- mediation_moderation_path_coefficient_label(path_result, mediator)
        }
      }
      if (length(w) >= 1L && nzchar(w)) {
        labels[[paste0("w->xy_", x_slot)]] <- mediation_moderation_path_coefficient_label(path_result, paste0(focal, ":", w))
        if (mediation_moderation_has_scalar_name(z)) {
          labels[[paste0("z->xy_", x_slot)]] <- mediation_moderation_path_coefficient_label(path_result, paste0(focal, ":", z))
        }
        for (mediator in mediators) {
          m_slot <- unname(m_map[[mediator]] %||% "")
          if (!nzchar(m_slot)) next
          key <- paste0("w->my_", m_slot)
          if (is.null(labels[[key]]) || !nzchar(labels[[key]])) {
            labels[[key]] <- mediation_moderation_path_coefficient_label(path_result, paste0(mediator, ":", w))
          }
        }
      }
    }
  }
  labels[nzchar(unlist(labels, use.names = FALSE))]
}

mediation_moderation_edge_coefficient_significance <- function(path_results) {
  significance <- list()
  for (result in path_results %||% list()) {
    if (!is.list(result) || is.null(result$model)) next
    focal <- as.character(result$focal %||% "")[[1]]
    moderators <- as.character(result$w %||% character(0))
    moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
    w <- utils::head(moderators, 1)
    z <- mediation_moderation_second_moderator(moderators)
    equation <- as.character(result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      significance[["x->m"]] <- mediation_moderation_path_coefficient_info(result, focal)$significant
      if (length(w) >= 1L && nzchar(w)) {
        significance[["w->xm"]] <- mediation_moderation_path_coefficient_info(result, paste0(focal, ":", w))$significant
        if (mediation_moderation_has_scalar_name(z)) {
          significance[["z->xm"]] <- mediation_moderation_path_coefficient_info(result, paste0(focal, ":", z))$significant
        }
      }
    } else if (identical(equation, "Y model")) {
      significance[["x->y"]] <- mediation_moderation_path_coefficient_info(result, focal)$significant
      mediators <- as.character(result$mediators %||% character(0))
      if (length(mediators) > 0L) {
        significance[["m->y"]] <- mediation_moderation_path_coefficient_info(result, mediators[[1L]])$significant
      }
      if (length(w) >= 1L && nzchar(w)) {
        significance[["w->xy"]] <- mediation_moderation_path_coefficient_info(result, paste0(focal, ":", w))$significant
        if (mediation_moderation_has_scalar_name(z)) {
          significance[["z->xy"]] <- mediation_moderation_path_coefficient_info(result, paste0(focal, ":", z))$significant
        }
        if (length(mediators) > 0L) {
          significance[["w->my"]] <- mediation_moderation_path_coefficient_info(result, paste0(mediators[[1L]], ":", w))$significant
          if (mediation_moderation_has_scalar_name(z)) {
            significance[["z->my"]] <- mediation_moderation_path_coefficient_info(result, paste0(mediators[[1L]], ":", z))$significant
          }
        }
      }
    }
  }
  significance
}

mediation_moderation_result_edge_coefficient_significance <- function(result, spec) {
  roles <- result$roles
  x_vars <- as.character(roles$x %||% character(0))
  x_vars <- x_vars[nzchar(x_vars)]
  mediator_slots <- mediation_moderation_result_mediator_slots(spec)
  if (length(x_vars) <= 1L && length(mediator_slots) <= 1L) {
    return(mediation_moderation_edge_coefficient_significance(result$path_results))
  }
  x_slots <- if (length(x_vars) == 1L) "x" else paste0("x", seq_along(x_vars))
  x_map <- stats::setNames(x_slots, x_vars)
  mediators <- as.character(roles$mediators %||% character(0))
  moderated_paths <- if (isTRUE(result$custom_model_canvas) || identical(as.character(spec$model %||% ""), "custom")) {
    as.character(result$moderated_paths %||% character(0))
  } else {
    mediation_moderation_model_moderated_paths(result$model_number %||% spec$model %||% "")
  }
  moderated_x_to_m <- mediation_moderation_normalize_moderated_x_to_m(
    result$moderated_x_to_m,
    mediators,
    x_vars,
    moderated_paths
  )
  m_map <- stats::setNames(mediator_slots, mediators[seq_along(mediator_slots)])
  significance <- list()
  for (path_result in result$path_results %||% list()) {
    focal <- as.character(path_result$focal %||% "")[[1]]
    x_slot <- unname(x_map[[focal]] %||% "")
    if (!nzchar(x_slot)) next
    moderators <- as.character(path_result$w %||% character(0))
    moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
    w <- utils::head(moderators, 1)
    z <- mediation_moderation_second_moderator(moderators)
    equation <- as.character(path_result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      mediator <- trimws(sub("^M model:\\s*", "", equation))
      m_slot <- unname(m_map[[mediator]] %||% "")
      if (!nzchar(m_slot)) next
      significance[[paste0(x_slot, "->", m_slot)]] <- mediation_moderation_path_coefficient_info(path_result, focal)$significant
      if (length(w) >= 1L && nzchar(w) && focal %in% as.character(moderated_x_to_m[[mediator]] %||% character(0))) {
        significance[[paste0("w->xm_", x_slot, "_", m_slot)]] <- mediation_moderation_path_coefficient_info(path_result, paste0(focal, ":", w))$significant
        if (mediation_moderation_has_scalar_name(z)) {
          significance[[paste0("z->xm_", x_slot, "_", m_slot)]] <- mediation_moderation_path_coefficient_info(path_result, paste0(focal, ":", z))$significant
        }
      }
    } else if (identical(equation, "Y model")) {
      significance[[paste0(x_slot, "->y")]] <- mediation_moderation_path_coefficient_info(path_result, focal)$significant
      for (mediator in mediators) {
        m_slot <- unname(m_map[[mediator]] %||% "")
        if (!nzchar(m_slot)) next
        key <- paste0(m_slot, "->y")
        if (is.null(significance[[key]])) {
          significance[[key]] <- mediation_moderation_path_coefficient_info(path_result, mediator)$significant
        }
      }
      if (length(w) >= 1L && nzchar(w)) {
        significance[[paste0("w->xy_", x_slot)]] <- mediation_moderation_path_coefficient_info(path_result, paste0(focal, ":", w))$significant
        if (mediation_moderation_has_scalar_name(z)) {
          significance[[paste0("z->xy_", x_slot)]] <- mediation_moderation_path_coefficient_info(path_result, paste0(focal, ":", z))$significant
        }
        for (mediator in mediators) {
          m_slot <- unname(m_map[[mediator]] %||% "")
          if (!nzchar(m_slot)) next
          key <- paste0("w->my_", m_slot)
          if (is.null(significance[[key]])) {
            significance[[key]] <- mediation_moderation_path_coefficient_info(path_result, paste0(mediator, ":", w))$significant
          }
          if (mediation_moderation_has_scalar_name(z)) {
            z_key <- paste0("z->my_", m_slot)
            if (is.null(significance[[z_key]])) {
              significance[[z_key]] <- mediation_moderation_path_coefficient_info(path_result, paste0(mediator, ":", z))$significant
            }
          }
        }
      }
    }
  }
  significance
}

mediation_moderation_result_diagram_ui <- function(result, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  diagram <- mediation_moderation_result_diagram_data(result)
  spec <- diagram$spec
  roles <- diagram$roles
  if (!is.list(spec) || !is.list(roles)) {
    return(NULL)
  }
  div(
    class = "result-section regression-result-panel mm-result-diagram-section",
    h3("Model diagram"),
    mediation_moderation_diagram(
      spec,
      roles,
      result$variable_info,
      result$labels,
      language,
      edge_labels = mediation_moderation_result_edge_coefficient_labels(result, spec),
      edge_significance = if (isTRUE(dash_nonsignificant)) mediation_moderation_result_edge_coefficient_significance(result, spec) else list(),
      variant = "result"
    )
  )
}

mediation_moderation_boot_status <- function(valid, requested) {
  valid <- as.integer(valid %||% 0L)
  requested <- as.integer(requested %||% 0L)
  ratio <- if (requested > 0L) valid / requested else 0
  if (valid < max(20L, ceiling(.50 * requested))) return("Unreliable")
  if (ratio < .80) return("Caution")
  "Adequate"
}

mediation_moderation_boot_p <- function(values) {
  values <- mediation_moderation_numeric_vector(values)
  values <- values[is.finite(values)]
  n <- length(values)
  if (n == 0L) return(NA_real_)
  lower <- (sum(values <= 0) + 1) / (n + 1)
  upper <- (sum(values >= 0) + 1) / (n + 1)
  min(1, 2 * min(lower, upper))
}

mediation_moderation_boot_summary <- function(point, boot_values, ci_method = "bias_corrected") {
  requested <- length(boot_values %||% numeric(0))
  boot_values <- mediation_moderation_numeric_vector(boot_values)
  boot_values <- boot_values[is.finite(boot_values)]
  valid <- length(boot_values)
  status <- mediation_moderation_boot_status(valid, requested)
  interval_available <- !identical(status, "Unreliable") && is.finite(point)
  interval <- if (interval_available) {
    bootstrap_ci(point, boot_values, method = ci_method)
  } else {
    c(NA_real_, NA_real_)
  }
  summary <- c(
    Estimate = point,
    `Boot SE` = if (valid > 1L) stats::sd(boot_values) else NA_real_,
    LLCI = interval[[1]],
    ULCI = interval[[2]],
    `Boot p` = if (interval_available) mediation_moderation_boot_p(boot_values) else NA_real_,
    Valid = valid,
    Requested = requested,
    `Valid %` = if (requested > 0L) 100 * valid / requested else NA_real_
  )
  attr(summary, "status") <- status
  summary
}

mediation_moderation_effect_sum <- function(values) {
  values <- mediation_moderation_numeric_vector(values)
  if (length(values) == 0L) return(0)
  if (any(!is.finite(values))) return(NA_real_)
  sum(values)
}

mediation_moderation_effect_variable_label <- function(name, variable_info = NULL, labels = character(0)) {
  name <- mediation_moderation_scalar_choice(name, "")
  if (!nzchar(name)) {
    return("")
  }
  display_variable_name_static(name, variable_info, labels, label_only = TRUE)
}

mediation_moderation_effect_path_text <- function(path_text, focal, y, mediators = character(0), variable_info = NULL, labels = character(0)) {
  tokens <- trimws(strsplit(path_text, "->", fixed = TRUE)[[1]])
  mapped <- vapply(tokens, function(token) {
    if (identical(token, "X")) {
      return(mediation_moderation_effect_variable_label(focal, variable_info, labels))
    }
    if (identical(token, "Y")) {
      return(mediation_moderation_effect_variable_label(y, variable_info, labels))
    }
    if (grepl("^M[0-9]+$", token)) {
      index <- suppressWarnings(as.integer(sub("^M", "", token)))
      mediator <- as.character(mediators %||% character(0))[index]
      if (length(mediator) == 1L && nzchar(mediator)) {
        return(mediation_moderation_effect_variable_label(mediator, variable_info, labels))
      }
    }
    if (identical(token, "M")) {
      mediator <- utils::head(as.character(mediators %||% character(0)), 1)
      if (length(mediator) == 1L && nzchar(mediator)) {
        return(mediation_moderation_effect_variable_label(mediator, variable_info, labels))
      }
    }
    if (token %in% as.character(mediators %||% character(0))) {
      return(mediation_moderation_effect_variable_label(token, variable_info, labels))
    }
    token
  }, character(1))
  paste(mapped[nzchar(mapped)], collapse = "-->")
}

mediation_moderation_effect_condition_text <- function(condition, w = character(0), variable_info = NULL, labels = character(0)) {
  condition <- trimws(as.character(condition %||% "")[[1]])
  if (!nzchar(condition)) {
    return("")
  }
  w <- utils::head(as.character(w %||% character(0)), 2L)
  w <- w[nzchar(w)]
  symbol_map <- stats::setNames(w, c("W", "Z")[seq_along(w)])
  parts <- trimws(strsplit(condition, ";", fixed = TRUE)[[1]])
  parts <- vapply(parts, function(part) {
    symbol <- sub("\\s+.*$", "", part)
    suffix <- trimws(sub("^[^[:space:]]+", "", part))
    if (symbol %in% names(symbol_map)) {
      variable_label <- mediation_moderation_effect_variable_label(symbol_map[[symbol]], variable_info, labels)
      return(paste(c(variable_label, suffix)[nzchar(c(variable_label, suffix))], collapse = " "))
    }
    part
  }, character(1))
  paste(parts[nzchar(parts)], collapse = "; ")
}

mediation_moderation_effect_path_label <- function(
  effect_name,
  focal,
  y,
  mediators = character(0),
  w = character(0),
  variable_info = NULL,
  labels = character(0)
) {
  effect_name <- as.character(effect_name %||% "")
  if (grepl("^Conditional indirect: X -> ", effect_name)) {
    parts <- strsplit(sub("^Conditional indirect: ", "", effect_name), "|", fixed = TRUE)[[1]]
    path <- mediation_moderation_effect_path_text(parts[[1]], focal, y, mediators, variable_info, labels)
    condition <- if (length(parts) >= 2L) trimws(parts[[2]]) else ""
    condition <- mediation_moderation_effect_condition_text(condition, w, variable_info, labels)
    return(paste(c("Conditional indirect effect", path, condition)[nzchar(c("Conditional indirect effect", path, condition))], collapse = "\n"))
  }
  if (grepl("^Index of moderated mediation: X -> ", effect_name)) {
    path <- mediation_moderation_effect_path_text(sub("^Index of moderated mediation: ", "", effect_name), focal, y, mediators, variable_info, labels)
    return(sprintf("Index of moderated mediation\n%s", path))
  }
  if (grepl("^Relative indirect: X -> ", effect_name)) {
    parts <- strsplit(sub("^Relative indirect: ", "", effect_name), "|", fixed = TRUE)[[1]]
    path <- mediation_moderation_effect_path_text(parts[[1]], focal, y, mediators, variable_info, labels)
    condition <- if (length(parts) >= 2L) trimws(parts[[2]]) else ""
    condition <- mediation_moderation_effect_condition_text(condition, w, variable_info, labels)
    return(paste(c("Relative indirect effect", path, condition)[nzchar(c("Relative indirect effect", path, condition))], collapse = "\n"))
  }
  if (!grepl("^Indirect: X -> ", effect_name)) {
    return(effect_name)
  }
  path_text <- sub("^Indirect: ", "", effect_name)
  sprintf("Indirect effect\n%s", mediation_moderation_effect_path_text(path_text, focal, y, mediators, variable_info, labels))
}

mediation_moderation_effect_label_parts <- function(effect_name, focal, y = character(0), mediators = character(0), w = character(0), variable_info = NULL, labels = character(0)) {
  label <- mediation_moderation_effect_path_label(effect_name, focal, y, mediators, w, variable_info, labels)
  lines <- strsplit(as.character(label %||% ""), "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  effect <- if (length(lines) > 0L) lines[[1L]] else as.character(label %||% "")
  path <- if (length(lines) >= 2L) paste(lines[-1L], collapse = "\n") else ""
  list(effect = effect, path = path)
}

mediation_moderation_effect_table <- function(
  model,
  focal,
  effects,
  boot_matrix,
  ci_method = "bias_corrected",
  y = character(0),
  mediators = character(0),
  w = character(0),
  variable_info = NULL,
  labels = character(0),
  model_label = NULL
) {
  model_label <- as.character(model_label %||% paste("Model", model))[[1L]]
  diagnostic_rows <- list()
  rows <- lapply(names(effects), function(effect_name) {
    summary <- mediation_moderation_boot_summary(effects[[effect_name]], boot_matrix[, effect_name], ci_method = ci_method)
    label_parts <- mediation_moderation_effect_label_parts(effect_name, focal, y, mediators, w, variable_info, labels)
    diagnostic_rows[[length(diagnostic_rows) + 1L]] <<- data.frame(
      Model = model_label,
      X = focal,
      Effect = label_parts$effect,
      Path = label_parts$path,
      Requested = as.integer(summary[["Requested"]]),
      Valid = as.integer(summary[["Valid"]]),
      `Valid %` = format_decimal3(summary[["Valid %"]]),
      Status = as.character(attr(summary, "status", exact = TRUE) %||% "Unreliable"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    data.frame(
      Model = model_label,
      X = focal,
      Effect = label_parts$effect,
      Path = label_parts$path,
      Estimate = format_decimal3(summary[["Estimate"]]),
      `Boot SE` = format_decimal3(summary[["Boot SE"]]),
      LLCI = format_decimal3(summary[["LLCI"]]),
      ULCI = format_decimal3(summary[["ULCI"]]),
      `Boot p` = format_p(summary[["Boot p"]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  table <- do.call(rbind, rows)
  if (is.data.frame(table) && nrow(table) > 0L) {
    value_columns <- intersect(c("Estimate", "Boot SE", "LLCI", "ULCI"), names(table))
    if (length(value_columns) > 0L) {
      has_value <- apply(table[, value_columns, drop = FALSE], 1L, function(row) {
        values <- trimws(as.character(row %||% character(0)))
        any(nzchar(values) & !tolower(values) %in% c("na", "nan"))
      })
      table <- table[has_value, , drop = FALSE]
    }
    indirect_rows <- !is.na(table$Effect) & table$Effect == "Indirect effect"
    if (any(indirect_rows)) {
      value_columns <- intersect(c("Estimate", "Boot SE", "LLCI", "ULCI"), names(table))
      has_value <- apply(table[indirect_rows, value_columns, drop = FALSE], 1L, function(row) {
        values <- trimws(as.character(row %||% character(0)))
        any(nzchar(values) & !tolower(values) %in% c("na", "nan"))
      })
      keep <- rep(TRUE, nrow(table))
      keep[which(indirect_rows)] <- has_value
      table <- table[keep, , drop = FALSE]
    }
  }
  diagnostics <- if (length(diagnostic_rows)) do.call(rbind, diagnostic_rows) else data.frame()
  attr(table, "bootstrap_diagnostics") <- diagnostics
  attr(table, "compact_column_widths") <- c(9, 6, 17, 31, 8, 8, 8, 8, 5)
  table
}

mediation_moderation_path_coefficient_info <- function(result, term) {
  term <- mediation_moderation_clean_term(term)
  coef_table <- result$coef_table
  if (!is.data.frame(coef_table) || nrow(coef_table) == 0L || !nzchar(term)) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  term_column <- intersect(c("Term", "Variable"), names(coef_table))
  if (length(term_column) == 0L) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  matched <- which(mediation_moderation_clean_term(coef_table[[term_column[[1L]]]]) == term)
  if (length(matched) == 0L) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  row <- coef_table[matched[[1L]], , drop = FALSE]
  b_column <- intersect(c("B", "Estimate"), names(row))
  if (length(b_column) == 0L || !"p" %in% names(row)) {
    return(list(label = "", p = NA_real_, significant = TRUE))
  }
  b_value <- mediation_moderation_numeric_scalar(row[[b_column[[1L]]]][[1L]])
  p_value <- mediation_moderation_numeric_scalar(row$p[[1L]])
  if (!is.finite(b_value)) {
    return(list(label = "", p = p_value, significant = TRUE))
  }
  list(
    label = sprintf("%s(%s)", format_decimal3(b_value), format_p(p_value)),
    p = p_value,
    significant = !is.finite(p_value) || p_value < 0.05
  )
}

mediation_moderation_path_coefficient_label <- function(result, term) {
  mediation_moderation_path_coefficient_info(result, term)$label
}

mediation_moderation_edge_coefficient_labels <- function(path_results) {
  labels <- list()
  for (result in path_results %||% list()) {
    if (!is.list(result) || is.null(result$model)) next
    focal <- as.character(result$focal %||% "")[[1]]
    moderators <- as.character(result$w %||% character(0))
    moderators <- moderators[!is.na(moderators) & nzchar(moderators)]
    w <- utils::head(moderators, 1)
    z <- mediation_moderation_second_moderator(moderators)
    equation <- as.character(result$equation %||% "")[[1]]
    if (grepl("^M model:", equation)) {
      labels[["x->m"]] <- mediation_moderation_path_coefficient_label(result, focal)
      if (length(w) >= 1L && nzchar(w)) {
        labels[["w->xm"]] <- mediation_moderation_path_coefficient_label(result, paste0(focal, ":", w))
        if (mediation_moderation_has_scalar_name(z)) {
          labels[["z->xm"]] <- mediation_moderation_path_coefficient_label(result, paste0(focal, ":", z))
        }
      }
    } else if (identical(equation, "Y model")) {
      labels[["x->y"]] <- mediation_moderation_path_coefficient_label(result, focal)
      mediators <- as.character(result$mediators %||% character(0))
      if (length(mediators) > 0L) {
        labels[["m->y"]] <- mediation_moderation_path_coefficient_label(result, mediators[[1L]])
      }
      if (length(w) >= 1L && nzchar(w)) {
        labels[["w->xy"]] <- mediation_moderation_path_coefficient_label(result, paste0(focal, ":", w))
        if (mediation_moderation_has_scalar_name(z)) {
          labels[["z->xy"]] <- mediation_moderation_path_coefficient_label(result, paste0(focal, ":", z))
        }
        if (length(mediators) > 0L) {
          labels[["w->my"]] <- mediation_moderation_path_coefficient_label(result, paste0(mediators[[1L]], ":", w))
          if (mediation_moderation_has_scalar_name(z)) {
            labels[["z->my"]] <- mediation_moderation_path_coefficient_label(result, paste0(mediators[[1L]], ":", z))
          }
        }
      }
    }
  }
  labels <- labels[nzchar(unlist(labels, use.names = FALSE))]
  labels
}

mediation_moderation_fast_lm_spec <- function(data, response, terms) {
  formula <- mediation_moderation_lm_formula(response, terms)
  frame <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  model_terms <- stats::terms(formula)
  list(
    response = response,
    terms = unique(as.character(terms %||% character(0))),
    formula = formula,
    x = stats::model.matrix(model_terms, frame),
    y = stats::model.response(frame),
    frame = frame
  )
}

mediation_moderation_fast_lm_fit <- function(spec, rows) {
  x <- spec$x[rows, , drop = FALSE]
  y <- spec$y[rows]
  fit <- tryCatch(stats::lm.fit(x, y), error = function(e) NULL)
  if (is.null(fit)) {
    return(NULL)
  }
  coefficients <- as.numeric(fit$coefficients)
  names(coefficients) <- gsub("`", "", colnames(x), fixed = TRUE)
  total_ss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  rss <- sum(fit$residuals^2, na.rm = TRUE)
  r_squared <- if (is.finite(total_ss) && total_ss > 0) 1 - rss / total_ss else NA_real_
  list(coefficients = coefficients, r_squared = r_squared)
}

mediation_moderation_fast_coef <- function(coefficients, term) {
  term <- as.character(term %||% "")
  term <- gsub("`", "", term, fixed = TRUE)
  if (!nzchar(term) || is.null(names(coefficients)) || !term %in% names(coefficients)) {
    return(NA_real_)
  }
  mediation_moderation_numeric_scalar(unname(coefficients[[term]]))
}

mediation_moderation_fast_path_spec <- function(name, data, response, terms) {
  full_spec <- mediation_moderation_fast_lm_spec(data, response, terms)
  base_terms <- terms[!grepl(":", mediation_moderation_clean_term(terms), fixed = TRUE)]
  base_spec <- if (length(base_terms) < length(terms)) {
    mediation_moderation_fast_lm_spec(data, response, base_terms)
  } else {
    NULL
  }
  list(name = name, full = full_spec, base = base_spec)
}

mediation_moderation_fast_boot_context <- function(base, roles, focal, structure, model) {
  custom_path_model <- isTRUE(base$custom_path_model)
  moderated_paths <- if (custom_path_model) {
    intersect(as.character(base$moderated_paths %||% character(0)), c("xm", "my", "xy"))
  } else {
    mediation_moderation_model_moderated_paths(model)
  }
  direct_x <- unique(as.character(base$direct_x %||% roles$x %||% focal))
  direct_to_y <- focal %in% direct_x
  y <- roles$y[[1]]
  all_x <- unique(as.character(base$all_x %||% c(focal, roles$x)))
  x_vars <- setdiff(all_x, focal)
  covariate_control <- mediation_moderation_covariate_control_values(base$covariate_control %||% c("y", "m"))
  input_covariates <- setdiff(as.character(roles$covariates %||% character(0)), all_x)
  m_input_covariates <- if ("m" %in% covariate_control) input_covariates else character(0)
  y_input_covariates <- if ("y" %in% covariate_control) input_covariates else character(0)
  x_to_m <- base$x_to_m %||% stats::setNames(lapply(roles$mediators, function(mediator) all_x), roles$mediators)
  m_to_y <- mediation_moderation_normalize_outcome_map(
    base$m_to_y,
    outcomes = y,
    allowed = roles$mediators,
    default = if (custom_path_model) character(0) else roles$mediators
  )
  m_to_m <- mediation_moderation_normalize_mediator_map(
    base$m_to_m,
    mediators = roles$mediators,
    default = character(0)
  )
  y_mediators <- if (custom_path_model) {
    intersect(
      roles$mediators,
      mediation_moderation_reachable_mediators(focal, y, x_to_m, m_to_y, m_to_m, roles$mediators)
    )
  } else {
    intersect(roles$mediators, as.character(m_to_y[[y]] %||% character(0)))
  }
  y_model_direct_x <- if (custom_path_model) {
    intersect(all_x, as.character(direct_x %||% character(0)))
  } else {
    if (direct_to_y) focal else character(0)
  }
  y_model_mediators <- if (custom_path_model) {
    intersect(roles$mediators, as.character(m_to_y[[y]] %||% character(0)))
  } else {
    y_mediators
  }
  moderated_x_to_m <- mediation_moderation_normalize_moderated_x_to_m(
    base$moderated_x_to_m,
    roles$mediators,
    all_x,
    moderated_paths
  )
  mediators <- roles$mediators
  moderated_m_to_y <- mediation_moderation_normalize_moderated_m_to_y(
    base$moderated_m_to_y,
    mediators,
    moderated_paths
  )
  moderation_map <- mediation_moderation_normalize_moderation_map(base$moderation_map, roles)
  w <- as.character(roles$w %||% character(0))
  w <- w[nzchar(w)]
  w <- utils::head(w, 2L)
  has_w <- length(w) >= 1L
  y_covariates <- unique(c(y_input_covariates, intersect(x_vars, direct_x)))
  focal_term <- mediation_moderation_var_term(focal)
  w_term <- if (has_w) vapply(w, mediation_moderation_var_term, character(1)) else character(0)
  y_cov_terms <- vapply(y_covariates, mediation_moderation_var_term, character(1))
  data <- base$data
  path_specs <- list()
  mediator_x_vars <- function(mediator) {
    intersect(all_x, as.character(x_to_m[[mediator]] %||% all_x))
  }
  mediator_moderated_x_vars <- function(mediator) {
    intersect(all_x, as.character(moderated_x_to_m[[mediator]] %||% character(0)))
  }
  mediator_terms_for <- function(mediator) {
    mx <- mediator_x_vars(mediator)
    c(
      if (focal %in% mx) focal_term else character(0),
      vapply(intersect(x_vars, mx), mediation_moderation_var_term, character(1)),
      vapply(intersect(mediators, m_to_m[[mediator]] %||% character(0)), mediation_moderation_var_term, character(1)),
      vapply(m_input_covariates, mediation_moderation_var_term, character(1))
    )
  }

  add_path <- function(name, response, terms) {
    path_specs[[length(path_specs) + 1L]] <<- mediation_moderation_fast_path_spec(name, data, response, terms)
  }

  if (identical(structure, "none")) {
    xy_w <- mediation_moderation_path_moderators(moderation_map, "xy", focal = focal, outcome = y, w = w, moderated = "xy" %in% moderated_paths)
    path_model <- if (length(xy_w) >= 2L) {
      if (custom_path_model) "custom" else model
    } else {
      "1"
    }
    y_terms <- if (length(xy_w) > 0L) mediation_moderation_no_mediator_terms(focal, xy_w, path_model, y_cov_terms) else c(focal_term, y_cov_terms)
    add_path("y", y, y_terms)
  } else if (identical(structure, "serial") && !custom_path_model) {
    m1 <- mediators[[1]]
    m2 <- mediators[[2]]
    add_path("m1", m1, mediator_terms_for(m1))
    add_path("m2", m2, c(mediator_terms_for(m2), mediation_moderation_var_term(m1)))
    add_path("y", y, c(if (direct_to_y) focal_term else character(0), mediation_moderation_var_term(m1), mediation_moderation_var_term(m2), y_cov_terms))
  } else {
    mediator_terms <- vapply(y_model_mediators, mediation_moderation_var_term, character(1))
    for (mediator in mediators) {
      m_terms <- mediator_terms_for(mediator)
      xm_moderated <- has_w && focal %in% mediator_moderated_x_vars(mediator)
      xm_w <- mediation_moderation_path_moderators(moderation_map, "xm", focal = focal, mediator = mediator, w = w, moderated = xm_moderated)
      if (length(xm_w) > 0L) {
        m_terms <- c(m_terms, vapply(xm_w, mediation_moderation_var_term, character(1)))
        m_terms <- c(m_terms, mediation_moderation_moderated_predictor_terms(focal, xm_w))
      }
      add_path(paste0("m_", mediator), mediator, m_terms)
    }
    y_w <- character(0)
    y_terms <- c(vapply(y_model_direct_x, mediation_moderation_var_term, character(1)), mediator_terms, y_cov_terms)
    if (has_w && ("xy" %in% moderated_paths || length(moderated_m_to_y) > 0L)) {
      xy_w <- mediation_moderation_path_moderators(moderation_map, "xy", focal = focal, outcome = y, w = w, moderated = "xy" %in% moderated_paths)
      my_mediators <- intersect(moderated_m_to_y, y_model_mediators)
      my_w_list <- stats::setNames(lapply(my_mediators, function(mediator) {
        mediation_moderation_path_moderators(moderation_map, "my", mediator = mediator, outcome = y, w = w, moderated = TRUE)
      }), my_mediators)
      y_w <- unique(c(xy_w, unlist(my_w_list, use.names = FALSE)))
      direct_terms <- vapply(unique(c(y_model_direct_x, if (length(xy_w) > 0L) focal else character(0))), mediation_moderation_var_term, character(1))
      y_terms <- c(direct_terms, mediator_terms, vapply(y_w, mediation_moderation_var_term, character(1)), y_cov_terms)
      if (length(xy_w) > 0L) {
        y_terms <- c(y_terms, mediation_moderation_moderated_predictor_terms(focal, xy_w))
      }
      if (length(my_mediators) > 0L) {
        y_terms <- c(y_terms, unlist(lapply(my_mediators, function(mediator) {
          mediation_moderation_moderated_predictor_terms(mediator, my_w_list[[mediator]])
        }), use.names = FALSE))
      }
    }
    add_path("y", y, y_terms)
  }

  list(
    path_specs = path_specs,
    effect_names = names(base$effects),
    focal = focal,
    y = y,
    mediators = mediators,
    w = w,
    no_mediator_w = if (identical(structure, "none")) xy_w else character(0),
    no_mediator_model = if (identical(structure, "none") && length(xy_w) >= 2L) model else "1",
    has_w = has_w,
    conditional_moderator_grid = if (has_w) mediation_moderation_conditional_moderator_grid(data, w) else data.frame(stringsAsFactors = FALSE),
    moderated_paths = moderated_paths,
    moderated_x_to_m = moderated_x_to_m,
    moderated_m_to_y = moderated_m_to_y,
    moderation_map = moderation_map,
    direct_x = direct_x,
    x_to_m = x_to_m,
    m_to_y = m_to_y,
    m_to_m = m_to_m,
    y_mediators = y_mediators,
    y_model_mediators = y_model_mediators,
    indirect_paths = if (identical(structure, "none") || identical(structure, "serial")) list() else {
      mediation_moderation_indirect_paths(focal, y, x_to_m, m_to_y, m_to_m, mediators)
    },
    has_xm_moderated_effect = any(vapply(mediators, function(mediator) {
      focal %in% as.character(moderated_x_to_m[[mediator]] %||% character(0))
    }, logical(1))),
    has_my_moderated_effect = length(moderated_m_to_y %||% character(0)) > 0L,
    all_x = all_x,
    structure = structure,
    model = model,
    custom_path_model = custom_path_model
  )
}

mediation_moderation_fast_boot_fit <- function(context, rows) {
  fits <- lapply(context$path_specs, function(path_spec) {
    list(
      spec = path_spec,
      full = mediation_moderation_fast_lm_fit(path_spec$full, rows),
      base = if (is.null(path_spec$base)) NULL else mediation_moderation_fast_lm_fit(path_spec$base, rows)
    )
  })
  names(fits) <- vapply(context$path_specs, `[[`, character(1), "name")
  if (any(vapply(fits, function(fit) is.null(fit$full), logical(1)))) {
    return(NULL)
  }

  focal <- context$focal
  mediators <- context$mediators
  w <- context$w
  has_w <- context$has_w

  if (identical(context$structure, "none")) {
    y_coef <- fits$y$full$coefficients
    no_mediator_w <- as.character(context$no_mediator_w %||% character(0))
    effects <- c(Direct = mediation_moderation_fast_coef(y_coef, focal))
    if (length(no_mediator_w) > 0L) {
      effects <- mediation_moderation_no_mediator_effects(
        context$no_mediator_model %||% context$model,
        y_coef,
        focal,
        no_mediator_w,
        function(coefficients, term) mediation_moderation_fast_coef(coefficients, term)
      )
    }
  } else if (identical(context$structure, "serial")) {
    m1 <- mediators[[1]]
    m2 <- mediators[[2]]
    a1 <- if (focal %in% context$x_to_m[[m1]]) mediation_moderation_fast_coef(fits$m1$full$coefficients, focal) else NA_real_
    d21 <- mediation_moderation_fast_coef(fits$m2$full$coefficients, m1)
    a2 <- if (focal %in% context$x_to_m[[m2]]) mediation_moderation_fast_coef(fits$m2$full$coefficients, focal) else NA_real_
    b1 <- mediation_moderation_fast_coef(fits$y$full$coefficients, m1)
    b2 <- mediation_moderation_fast_coef(fits$y$full$coefficients, m2)
    direct <- if (focal %in% context$direct_x) mediation_moderation_fast_coef(fits$y$full$coefficients, focal) else NA_real_
    direct_total <- if (is.finite(direct)) direct else 0
    effects <- c(
      Direct = direct,
      `Indirect: X -> M1 -> Y` = a1 * b1,
      `Indirect: X -> M2 -> Y` = a2 * b2,
      `Indirect: X -> M1 -> M2 -> Y` = a1 * d21 * b2
    )
    indirect_total <- mediation_moderation_effect_sum(effects[grepl("^Indirect", names(effects))])
    effects <- c(effects, `Total indirect` = indirect_total, Total = if (is.finite(indirect_total)) direct_total + indirect_total else NA_real_)
  } else {
    y_coef <- fits$y$full$coefficients
    direct <- if (focal %in% context$direct_x) mediation_moderation_fast_coef(y_coef, focal) else NA_real_
    direct_total <- if (is.finite(direct)) direct else 0
    y_mediators <- intersect(mediators, as.character(context$y_mediators %||% mediators))
    indirect_paths <- context$indirect_paths %||% list()
    indirects <- vapply(indirect_paths, function(path) {
      value <- mediation_moderation_fast_coef(fits[[paste0("m_", path[[1L]])]]$full$coefficients, focal)
      if (length(path) >= 2L) {
        for (edge_index in seq_len(length(path) - 1L)) {
          value <- value * mediation_moderation_fast_coef(fits[[paste0("m_", path[[edge_index + 1L]])]]$full$coefficients, path[[edge_index]])
        }
      }
      value * mediation_moderation_fast_coef(y_coef, path[[length(path)]])
    }, numeric(1))
    names(indirects) <- vapply(indirect_paths, mediation_moderation_indirect_path_name, character(1))
    indirect_total <- mediation_moderation_effect_sum(indirects)
    effects <- c(Direct = direct, indirects, `Total indirect` = indirect_total, Total = if (is.finite(indirect_total)) direct_total + indirect_total else NA_real_)
    has_xm_moderated_effect <- isTRUE(context$has_xm_moderated_effect)
    has_my_moderated_effect <- isTRUE(context$has_my_moderated_effect)
    if (isTRUE(has_w) && is.data.frame(context$conditional_moderator_grid) && nrow(context$conditional_moderator_grid) > 0L && (isTRUE(has_xm_moderated_effect) || isTRUE(has_my_moderated_effect))) {
      conditional_effects <- mediation_moderation_conditional_indirect_effects(
        mediators = y_mediators,
        focal = focal,
        w = w,
        conditional_grid = context$conditional_moderator_grid,
        x_to_m = context$x_to_m,
        moderated_x_to_m = context$moderated_x_to_m,
        moderated_m_to_y = context$moderated_m_to_y,
        moderation_map = context$moderation_map,
        coef_a = function(mediator) mediation_moderation_fast_coef(fits[[paste0("m_", mediator)]]$full$coefficients, focal),
        coef_b = function(mediator) mediation_moderation_fast_coef(y_coef, mediator),
        coef_a_interaction = function(mediator, moderator) mediation_moderation_fast_coef(fits[[paste0("m_", mediator)]]$full$coefficients, paste0(focal, ":", moderator)),
        coef_b_interaction = function(mediator, moderator) mediation_moderation_fast_coef(y_coef, paste0(mediator, ":", moderator)),
        coef_a_three_way = function(mediator, moderator1, moderator2) mediation_moderation_fast_coef(fits[[paste0("m_", mediator)]]$full$coefficients, paste0(focal, ":", moderator1, ":", moderator2)),
        coef_b_three_way = function(mediator, moderator1, moderator2) mediation_moderation_fast_coef(y_coef, paste0(mediator, ":", moderator1, ":", moderator2)),
        coef_a_at = function(mediator, conditions) mediation_moderation_fast_conditional_slope(
          fits[[paste0("m_", mediator)]]$full$coefficients,
          fits[[paste0("m_", mediator)]]$spec$full,
          focal,
          conditions
        ),
        coef_b_at = function(mediator, conditions) mediation_moderation_fast_conditional_slope(
          y_coef,
          fits$y$spec$full,
          mediator,
          conditions
        ),
        outcome = context$y
      )
      effects <- c(effects, conditional_effects)
    }
  }

  list(fits = fits, effects = effects[context$effect_names])
}

mediation_moderation_fit_focal <- function(
  data,
  roles,
  focal,
  structure,
  model,
  mean_center = FALSE,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  boot_r = 5000L,
  seed = default_seed(),
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  moderated_paths = NULL,
  direct_x = NULL,
  x_to_m = NULL,
  m_to_y = NULL,
  m_to_m = NULL,
  moderated_x_to_m = NULL,
  moderated_m_to_y = NULL,
  moderation_map = NULL,
  all_x = NULL,
  custom_path_model = FALSE,
  effect_size_models = "y",
  covariate_control = c("y", "m")
) {
  model <- as.character(model %||% NA_character_)
  custom_path_model <- isTRUE(custom_path_model)
  moderated_paths <- if (custom_path_model) {
    intersect(as.character(moderated_paths %||% character(0)), c("xm", "my", "xy"))
  } else {
    mediation_moderation_model_moderated_paths(model)
  }
  y <- roles$y[[1]]
  mediators <- roles$mediators
  w <- as.character(roles$w %||% character(0))
  w <- w[nzchar(w)]
  w <- utils::head(w, 2L)
  has_w <- length(w) >= 1L
  all_x <- unique(as.character(all_x %||% c(focal, setdiff(roles$x, focal))))
  x_vars <- setdiff(all_x, focal)
  direct_x <- unique(as.character(direct_x %||% all_x))
  direct_to_y <- focal %in% direct_x
  covariate_control <- mediation_moderation_covariate_control_values(covariate_control)
  input_covariates <- setdiff(as.character(roles$covariates %||% character(0)), all_x)
  m_input_covariates <- if ("m" %in% covariate_control) input_covariates else character(0)
  y_input_covariates <- if ("y" %in% covariate_control) input_covariates else character(0)
  y_covariates <- unique(c(y_input_covariates, intersect(x_vars, direct_x)))
  x_to_m <- x_to_m %||% stats::setNames(lapply(mediators, function(mediator) all_x), mediators)
  x_to_m <- stats::setNames(lapply(mediators, function(mediator) {
    intersect(all_x, as.character(x_to_m[[mediator]] %||% all_x))
  }), mediators)
  m_to_y <- mediation_moderation_normalize_outcome_map(
    m_to_y,
    outcomes = y,
    allowed = mediators,
    default = if (custom_path_model) character(0) else mediators
  )
  m_to_m <- mediation_moderation_normalize_mediator_map(
    m_to_m,
    mediators = mediators,
    default = character(0)
  )
  y_mediators <- if (custom_path_model) {
    intersect(
      mediators,
      mediation_moderation_reachable_mediators(focal, y, x_to_m, m_to_y, m_to_m, mediators)
    )
  } else {
    intersect(mediators, as.character(m_to_y[[y]] %||% character(0)))
  }
  y_model_direct_x <- if (custom_path_model) {
    intersect(all_x, as.character(direct_x %||% character(0)))
  } else {
    if (direct_to_y) focal else character(0)
  }
  y_model_mediators <- if (custom_path_model) {
    intersect(mediators, as.character(m_to_y[[y]] %||% character(0)))
  } else {
    y_mediators
  }
  moderated_x_to_m <- mediation_moderation_normalize_moderated_x_to_m(
    moderated_x_to_m,
    mediators,
    all_x,
    moderated_paths
  )
  moderated_m_to_y <- mediation_moderation_normalize_moderated_m_to_y(
    moderated_m_to_y,
    mediators,
    moderated_paths
  )
  moderation_map <- mediation_moderation_normalize_moderation_map(moderation_map, roles)
  effect_size_models <- intersect(tolower(as.character(effect_size_models %||% "y")), c("y", "m"))
  used_vars <- unique(c(y, all_x, mediators, if (has_w) w, m_input_covariates, y_input_covariates))
  used_vars <- intersect(used_vars, names(data))
  fit_data <- data[, used_vars, drop = FALSE]
  fit_data <- prepare_regression_model_data_static(
    fit_data,
    used_vars,
    variable_info = variable_info,
    reference_values = refs,
    variable_table = variable_info
  )
  variable_info_normalized <- normalize_regression_variable_info_static(variable_info, variable_info)
  continuous_vars <- if (is.data.frame(variable_info_normalized) && all(c("name", "measurement") %in% names(variable_info_normalized))) {
    as.character(variable_info_normalized$name[variable_info_normalized$measurement %in% "continuous"])
  } else {
    character(0)
  }
  fit_data <- mediation_moderation_numeric_model_data(
    fit_data,
    unique(c(y, mediators, intersect(continuous_vars, used_vars))),
    required = unique(c(y, mediators))
  )
  fit_data <- fit_data[stats::complete.cases(fit_data), , drop = FALSE]
  if (nrow(fit_data) < 10L) {
    stop("\ubd84\uc11d \uac00\ub2a5\ud55c \uc644\uc804\uc0ac\ub840\uac00 10\uac1c \ubbf8\ub9cc\uc785\ub2c8\ub2e4.")
  }
  if (isTRUE(mean_center)) {
    for (center_var in unique(c(focal, if (has_w) w))) {
      if (center_var %in% names(fit_data) && is.numeric(fit_data[[center_var]])) {
        fit_data[[center_var]] <- fit_data[[center_var]] - mean(fit_data[[center_var]], na.rm = TRUE)
      }
    }
  }

  focal_term <- mediation_moderation_var_term(focal)
  w_term <- if (has_w) vapply(w, mediation_moderation_var_term, character(1)) else character(0)
  y_cov_terms <- vapply(y_covariates, mediation_moderation_var_term, character(1))
  models <- list()
  path_results <- list()
  mediator_x_vars <- function(mediator) {
    intersect(all_x, as.character(x_to_m[[mediator]] %||% all_x))
  }
  mediator_moderated_x_vars <- function(mediator) {
    intersect(all_x, as.character(moderated_x_to_m[[mediator]] %||% character(0)))
  }
  mediator_moderated_y <- function(mediator) {
    mediator %in% moderated_m_to_y
  }
  mediator_covariates <- function(mediator) {
    unique(c(m_input_covariates, intersect(x_vars, mediator_x_vars(mediator)), intersect(mediators, m_to_m[[mediator]] %||% character(0))))
  }
  mediator_terms_for <- function(mediator) {
    mx <- mediator_x_vars(mediator)
    c(
      if (focal %in% mx) focal_term else character(0),
      vapply(intersect(x_vars, mx), mediation_moderation_var_term, character(1)),
      vapply(intersect(mediators, m_to_m[[mediator]] %||% character(0)), mediation_moderation_var_term, character(1)),
      vapply(m_input_covariates, mediation_moderation_var_term, character(1))
    )
  }
  make_path_result <- function(model, focal, equation, covariates = character(0), w = character(0), mediators = character(0), model_type = "m") {
    mediation_moderation_path_result(
      model,
      focal,
      equation,
      covariates = covariates,
      w = w,
      mediators = mediators,
      boot_r = boot_r,
      seed = seed,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      analysis_method = analysis_method,
      ci_method = ci_method,
      residual_diagnostics = residual_diagnostics,
      auto_method = auto_method,
      all_x = all_x,
      show_f2 = model_type %in% effect_size_models
    )
  }

  if (identical(structure, "none")) {
    xy_w <- mediation_moderation_path_moderators(moderation_map, "xy", focal = focal, outcome = y, w = w, moderated = "xy" %in% moderated_paths)
    path_model <- if (length(xy_w) >= 2L) {
      if (custom_path_model) "custom" else model
    } else {
      "1"
    }
    if (!custom_path_model && (!has_w || !"xy" %in% moderated_paths)) {
      stop("\ub9e4\uac1c\ubcc0\uc218\uac00 \uc5c6\uc744 \ub54c\ub294 \uc870\uc808\ubcc0\uc218\uc640 X -> Y \uc870\uc808\uacbd\ub85c\uac00 \ud544\uc694\ud569\ub2c8\ub2e4.")
    }
    if (custom_path_model && !direct_to_y && length(xy_w) == 0L) {
      stop("\uce94\ubc84\uc2a4\uc5d0 X -> Y \uacbd\ub85c\uac00 \uc5c6\uc2b5\ub2c8\ub2e4.")
    }
    y_terms <- if (length(xy_w) > 0L) mediation_moderation_no_mediator_terms(focal, xy_w, path_model, y_cov_terms) else c(focal_term, y_cov_terms)
    models$y <- mediation_moderation_fit_lm(fit_data, y, y_terms)
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$y,
      focal,
      "Y model",
      covariates = y_covariates,
      w = xy_w,
      model_type = "y"
    )
    effects <- c(Direct = mediation_moderation_model_coef(models$y, focal))
    if (length(xy_w) > 0L) {
      effects <- mediation_moderation_no_mediator_effects(
        path_model,
        models$y,
        focal,
        xy_w,
        function(model, term) mediation_moderation_model_coef(model, term)
      )
    }
  } else if (identical(structure, "serial") && !custom_path_model) {
    if (length(mediators) != 2L) {
      stop("\uc21c\ucc28 \ub9e4\uac1c\ub294 \ud604\uc7ac \ub9e4\uac1c\ubcc0\uc218 2\uac1c \ubaa8\ud615\uc73c\ub85c \uc2e4\ud589\ud569\ub2c8\ub2e4.")
    }
    m1 <- mediators[[1]]
    m2 <- mediators[[2]]
    models$m1 <- mediation_moderation_fit_lm(fit_data, m1, mediator_terms_for(m1))
    models$m2 <- mediation_moderation_fit_lm(fit_data, m2, c(mediator_terms_for(m2), mediation_moderation_var_term(m1)))
    models$y <- mediation_moderation_fit_lm(fit_data, y, c(if (direct_to_y) focal_term else character(0), mediation_moderation_var_term(m1), mediation_moderation_var_term(m2), y_cov_terms))
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$m1,
      focal,
      "M1 model",
      covariates = mediator_covariates(m1),
      model_type = "m"
    )
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$m2,
      focal,
      "M2 model",
      covariates = mediator_covariates(m2),
      mediators = m1,
      model_type = "m"
    )
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$y,
      focal,
      "Y model",
      covariates = y_covariates,
      mediators = c(m1, m2),
      model_type = "y"
    )
    a1 <- if (focal %in% mediator_x_vars(m1)) mediation_moderation_model_coef(models$m1, focal) else NA_real_
    d21 <- mediation_moderation_model_coef(models$m2, m1)
    a2 <- if (focal %in% mediator_x_vars(m2)) mediation_moderation_model_coef(models$m2, focal) else NA_real_
    b1 <- mediation_moderation_model_coef(models$y, m1)
    b2 <- mediation_moderation_model_coef(models$y, m2)
    direct <- if (direct_to_y) mediation_moderation_model_coef(models$y, focal) else NA_real_
    direct_total <- if (is.finite(direct)) direct else 0
    effects <- c(
      Direct = direct,
      `Indirect: X -> M1 -> Y` = a1 * b1,
      `Indirect: X -> M2 -> Y` = a2 * b2,
      `Indirect: X -> M1 -> M2 -> Y` = a1 * d21 * b2
    )
    indirect_total <- mediation_moderation_effect_sum(effects[grepl("^Indirect", names(effects))])
    effects <- c(effects, `Total indirect` = indirect_total, Total = if (is.finite(indirect_total)) direct_total + indirect_total else NA_real_)
  } else {
    mediator_terms <- vapply(y_model_mediators, mediation_moderation_var_term, character(1))
    for (mediator in mediators) {
      m_terms <- mediator_terms_for(mediator)
      xm_moderated <- has_w && focal %in% mediator_moderated_x_vars(mediator)
      xm_w <- mediation_moderation_path_moderators(moderation_map, "xm", focal = focal, mediator = mediator, w = w, moderated = xm_moderated)
      if (length(xm_w) > 0L) {
        m_terms <- c(m_terms, vapply(xm_w, mediation_moderation_var_term, character(1)))
        m_terms <- c(m_terms, mediation_moderation_moderated_predictor_terms(focal, xm_w))
      }
      models[[paste0("m_", mediator)]] <- mediation_moderation_fit_lm(fit_data, mediator, m_terms)
      path_results[[length(path_results) + 1L]] <- make_path_result(
        models[[paste0("m_", mediator)]],
        focal,
        paste0("M model: ", mediator),
        covariates = mediator_covariates(mediator),
        w = xm_w,
        model_type = "m"
      )
    }
    y_w <- character(0)
    y_terms <- c(vapply(y_model_direct_x, mediation_moderation_var_term, character(1)), mediator_terms, y_cov_terms)
    if (has_w && ("xy" %in% moderated_paths || length(moderated_m_to_y) > 0L)) {
      xy_w <- mediation_moderation_path_moderators(moderation_map, "xy", focal = focal, outcome = y, w = w, moderated = "xy" %in% moderated_paths)
      my_mediators <- intersect(moderated_m_to_y, y_model_mediators)
      my_w_list <- stats::setNames(lapply(my_mediators, function(mediator) {
        mediation_moderation_path_moderators(moderation_map, "my", mediator = mediator, outcome = y, w = w, moderated = TRUE)
      }), my_mediators)
      y_w <- unique(c(xy_w, unlist(my_w_list, use.names = FALSE)))
      direct_terms <- vapply(unique(c(y_model_direct_x, if (length(xy_w) > 0L) focal else character(0))), mediation_moderation_var_term, character(1))
      y_terms <- c(direct_terms, mediator_terms, vapply(y_w, mediation_moderation_var_term, character(1)), y_cov_terms)
      if (length(xy_w) > 0L) {
        y_terms <- c(y_terms, mediation_moderation_moderated_predictor_terms(focal, xy_w))
      }
      if (length(my_mediators) > 0L) {
        y_terms <- c(y_terms, unlist(lapply(my_mediators, function(mediator) {
          mediation_moderation_moderated_predictor_terms(mediator, my_w_list[[mediator]])
        }), use.names = FALSE))
      }
    }
    models$y <- mediation_moderation_fit_lm(fit_data, y, y_terms)
    path_results[[length(path_results) + 1L]] <- make_path_result(
      models$y,
      focal,
      "Y model",
      covariates = y_covariates,
      w = y_w,
      mediators = y_model_mediators,
      model_type = "y"
    )
    direct <- if (direct_to_y) mediation_moderation_model_coef(models$y, focal) else NA_real_
    direct_total <- if (is.finite(direct)) direct else 0
    indirect_paths <- mediation_moderation_indirect_paths(focal, y, x_to_m, m_to_y, m_to_m, mediators)
    indirects <- vapply(indirect_paths, function(path) {
      value <- mediation_moderation_model_coef(models[[paste0("m_", path[[1L]])]], focal)
      if (length(path) >= 2L) {
        for (edge_index in seq_len(length(path) - 1L)) {
          value <- value * mediation_moderation_model_coef(models[[paste0("m_", path[[edge_index + 1L]])]], path[[edge_index]])
        }
      }
      value * mediation_moderation_model_coef(models$y, path[[length(path)]])
    }, numeric(1))
    names(indirects) <- vapply(indirect_paths, mediation_moderation_indirect_path_name, character(1))
    indirect_total <- mediation_moderation_effect_sum(indirects)
    effects <- c(Direct = direct, indirects, `Total indirect` = indirect_total, Total = if (is.finite(indirect_total)) direct_total + indirect_total else NA_real_)
    conditional_moderator_grid <- if (has_w) mediation_moderation_conditional_moderator_grid(fit_data, w) else data.frame(stringsAsFactors = FALSE)
    has_xm_moderated_effect <- any(vapply(mediators, function(mediator) {
      focal %in% mediator_moderated_x_vars(mediator)
    }, logical(1)))
    has_my_moderated_effect <- length(intersect(moderated_m_to_y, y_mediators)) > 0L
    if (has_w && is.data.frame(conditional_moderator_grid) && nrow(conditional_moderator_grid) > 0L && (isTRUE(has_xm_moderated_effect) || isTRUE(has_my_moderated_effect))) {
      conditional_effects <- mediation_moderation_conditional_indirect_effects(
        mediators = y_mediators,
        focal = focal,
        w = w,
        conditional_grid = conditional_moderator_grid,
        x_to_m = x_to_m,
        moderated_x_to_m = moderated_x_to_m,
        moderated_m_to_y = moderated_m_to_y,
        moderation_map = moderation_map,
        coef_a = function(mediator) mediation_moderation_model_coef(models[[paste0("m_", mediator)]], focal),
        coef_b = function(mediator) mediation_moderation_model_coef(models$y, mediator),
        coef_a_interaction = function(mediator, moderator) mediation_moderation_model_coef(models[[paste0("m_", mediator)]], paste0(focal, ":", moderator)),
        coef_b_interaction = function(mediator, moderator) mediation_moderation_model_coef(models$y, paste0(mediator, ":", moderator)),
        coef_a_three_way = function(mediator, moderator1, moderator2) mediation_moderation_model_coef(models[[paste0("m_", mediator)]], paste0(focal, ":", moderator1, ":", moderator2)),
        coef_b_three_way = function(mediator, moderator1, moderator2) mediation_moderation_model_coef(models$y, paste0(mediator, ":", moderator1, ":", moderator2)),
        coef_a_at = function(mediator, conditions) mediation_moderation_model_conditional_slope(models[[paste0("m_", mediator)]], focal, conditions),
        coef_b_at = function(mediator, conditions) mediation_moderation_model_conditional_slope(models$y, mediator, conditions),
        outcome = y
      )
      effects <- c(effects, conditional_effects)
    }
  }

  path_table <- analysis_bind_rows(lapply(path_results, mediation_moderation_display_coefficient_table))

  list(
    data = fit_data,
    n = nrow(fit_data),
    models = models,
    path_results = path_results,
    path_table = path_table,
    effects = effects,
    direct_x = direct_x,
    x_to_m = x_to_m,
    m_to_y = m_to_y,
    m_to_m = m_to_m,
    moderated_paths = moderated_paths,
    moderated_x_to_m = moderated_x_to_m,
    moderated_m_to_y = moderated_m_to_y,
    moderation_map = moderation_map,
    all_x = all_x,
    custom_path_model = custom_path_model,
    covariate_control = covariate_control,
    variables = used_vars
  )
}

mediation_moderation_boot_effects <- function(
  data,
  roles,
  focal,
  structure,
  model,
  mean_center,
  boot_r,
  seed,
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  refs = character(0),
  value_labels = list(),
  progress = NULL,
  progress_offset = 0L,
  progress_total = NULL,
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  moderated_paths = NULL,
  direct_x = NULL,
  x_to_m = NULL,
  m_to_y = NULL,
  m_to_m = NULL,
  moderated_x_to_m = NULL,
  moderated_m_to_y = NULL,
  moderation_map = NULL,
  all_x = NULL,
  custom_path_model = FALSE,
  effect_size_models = "y",
  covariate_control = c("y", "m")
) {
  base <- mediation_moderation_fit_focal(
    data,
    roles,
    focal,
    structure,
    model,
    mean_center = mean_center,
    variable_info = variable_info,
    labels = labels,
    category_table = category_table,
    refs = refs,
    value_labels = value_labels,
    boot_r = boot_r,
    seed = seed,
    analysis_method = analysis_method,
    ci_method = ci_method,
    residual_diagnostics = residual_diagnostics,
    auto_method = auto_method,
    moderated_paths = moderated_paths,
    direct_x = direct_x,
    x_to_m = x_to_m,
    m_to_y = m_to_y,
    m_to_m = m_to_m,
    moderated_x_to_m = moderated_x_to_m,
    moderated_m_to_y = moderated_m_to_y,
    moderation_map = moderation_map,
    all_x = all_x,
    custom_path_model = custom_path_model,
    effect_size_models = effect_size_models,
    covariate_control = covariate_control
  )
  base$path_results <- lapply(base$path_results, function(path_result) {
    hierarchy <- mediation_moderation_hierarchical_steps(path_result)
    if (!is.null(hierarchy)) {
      path_result$hierarchical_base <- hierarchy[[1]]
    }
    path_result
  })
  effect_names <- names(base$effects)
  boot_r <- max(1L, as.integer(boot_r %||% 5000L))
  progress_total <- max(boot_r, as.integer(progress_total %||% boot_r))
  progress_step <- max(1L, floor(boot_r / 100L))
  seed <- as.integer(seed %||% default_seed())
  if (is.na(seed)) seed <- default_seed()
  set.seed(seed)
  boot_matrix <- matrix(NA_real_, nrow = boot_r, ncol = length(effect_names), dimnames = list(NULL, effect_names))
  coefficient_boot_samples <- lapply(base$path_results, function(path_result) {
    terms <- names(stats::coef(path_result$model))
    matrix(NA_real_, nrow = boot_r, ncol = length(terms), dimnames = list(NULL, terms))
  })
  hierarchical_boot_samples <- lapply(base$path_results, function(path_result) {
    if (!is.list(path_result$hierarchical_base) || is.null(path_result$hierarchical_base$model)) {
      return(NULL)
    }
    terms <- names(stats::coef(path_result$hierarchical_base$model))
    matrix(NA_real_, nrow = boot_r, ncol = length(terms), dimnames = list(NULL, terms))
  })
  full_r2_boot_samples <- lapply(base$path_results, function(path_result) {
    if (!is.list(path_result$hierarchical_base) || is.null(path_result$hierarchical_base$model)) {
      return(NULL)
    }
    rep(NA_real_, boot_r)
  })
  hierarchical_r2_boot_samples <- lapply(base$path_results, function(path_result) {
    if (!is.list(path_result$hierarchical_base) || is.null(path_result$hierarchical_base$model)) {
      return(NULL)
    }
    rep(NA_real_, boot_r)
  })
  fast_context <- mediation_moderation_fast_boot_context(base, roles, focal, structure, model)
  n <- nrow(base$data)
  if (is.function(progress)) {
    progress(progress_offset, progress_total, focal)
  }
  for (index in seq_len(boot_r)) {
    boot_fit <- tryCatch(
      mediation_moderation_fast_boot_fit(fast_context, sample.int(n, n, replace = TRUE)),
      error = function(e) NULL
    )
    boot_effects <- if (is.null(boot_fit)) NULL else boot_fit$effects
    boot_matrix[index, ] <- mediation_moderation_numeric_match(boot_effects, effect_names)
    if (!is.null(boot_fit) && length(boot_fit$fits) == length(base$path_results)) {
      for (path_index in seq_along(base$path_results)) {
        terms <- colnames(coefficient_boot_samples[[path_index]])
        coefficients <- boot_fit$fits[[path_index]]$full$coefficients
        coefficient_boot_samples[[path_index]][index, ] <- mediation_moderation_numeric_match(coefficients, terms)
        if (!is.null(full_r2_boot_samples[[path_index]])) {
          full_r2_boot_samples[[path_index]][index] <- boot_fit$fits[[path_index]]$full$r_squared
        }
        if (!is.null(hierarchical_boot_samples[[path_index]])) {
          boot_base_fit <- boot_fit$fits[[path_index]]$base
          if (!is.null(boot_base_fit)) {
            base_terms <- colnames(hierarchical_boot_samples[[path_index]])
            base_coefficients <- boot_base_fit$coefficients
            hierarchical_boot_samples[[path_index]][index, ] <- mediation_moderation_numeric_match(base_coefficients, base_terms)
            hierarchical_r2_boot_samples[[path_index]][index] <- boot_base_fit$r_squared
          }
        }
      }
    }
    if (is.function(progress) && (index == 1L || index == boot_r || index %% progress_step == 0L)) {
      progress(progress_offset + index, progress_total, focal)
    }
  }
  for (path_index in seq_along(base$path_results)) {
    if (isTRUE(base$path_results[[path_index]]$use_bootstrap)) {
      base$path_results[[path_index]]$boot_table <- bootstrap_summary_table(
        coefficient_boot_samples[[path_index]],
        base$path_results[[path_index]]$model,
        ci_method = ci_method
      )
    }
    if (!is.null(full_r2_boot_samples[[path_index]])) {
      base$path_results[[path_index]]$bootstrap_r_squared <- full_r2_boot_samples[[path_index]]
    }
    if (is.list(base$path_results[[path_index]]$hierarchical_base) && !is.null(base$path_results[[path_index]]$hierarchical_base$model)) {
      base$path_results[[path_index]]$hierarchical_base$bootstrap_r_squared <- hierarchical_r2_boot_samples[[path_index]]
      if (isTRUE(base$path_results[[path_index]]$hierarchical_base$use_bootstrap) && !is.null(hierarchical_boot_samples[[path_index]])) {
        base$path_results[[path_index]]$hierarchical_base$boot_table <- bootstrap_summary_table(
          hierarchical_boot_samples[[path_index]],
          base$path_results[[path_index]]$hierarchical_base$model,
          ci_method = ci_method
        )
      }
    }
  }
  base$path_table <- analysis_bind_rows(lapply(base$path_results, mediation_moderation_display_coefficient_table))
  base$effect_table <- mediation_moderation_effect_table(
    model,
    focal,
    base$effects,
    boot_matrix,
    ci_method = ci_method,
    y = roles$y[[1]],
    mediators = roles$mediators,
    w = roles$w,
    variable_info = variable_info,
    labels = labels,
    model_label = if (isTRUE(custom_path_model)) "Custom" else paste("Model", model)
  )
  base$effect_bootstrap_diagnostics <- attr(base$effect_table, "bootstrap_diagnostics", exact = TRUE) %||% data.frame()
  base
}

mediation_moderation_combined_landscape_note_line <- function(last_result) {
  analysis_method <- as.character(last_result$analysis_method %||% "statedu")[[1L]]
  method_note <- if (identical(analysis_method, "process_ols")) {
    "Coefficients, standard errors, t tests, p values, and model F tests use ordinary least squares for PROCESS-compatible comparison;"
  } else {
    "Coefficients use the StatEdu diagnostic-based method: HC3 robust SE when homoscedasticity is rejected, bootstrap CI when residual normality is rejected;"
  }
  ci_label <- bootstrap_ci_method_label(last_result$bootstrap_ci_method %||% "bias_corrected")
  paste(
    "Mediator model columns show the full moderation model (with interaction terms);",
    method_note,
    sprintf("Bootstrap confidence limits use the %s method;", ci_label),
    if (isTRUE(last_result$show_f2 %||% TRUE)) "f2 = Cohen's f-squared effect size for each non-intercept coefficient;" else "",
    "d(dU~4-dU) = Durbin-Watson statistic (upper critical value~4-upper critical value);",
    "z(p) = Lilliefors corrected Kolmogorov-Smirnov residual normality test (p-value);",
    sprintf("%s = Breusch-Pagan residual homoscedasticity test (p-value)", stat_chisq_label(with_p = TRUE))
  )
}

mediation_moderation_conditional_effect_group_label <- function(label) {
  label <- trimws(as.character(label %||% ""))
  if (!nzchar(label)) {
    return("")
  }
  level_pattern <- "(Low \\(M-SD\\)|Mean|High \\(M\\+SD\\))"
  first_condition <- strsplit(label, "\\s*;\\s*", perl = TRUE)[[1L]][[1L]]
  trimws(sub(paste0("\\s+[^;[:space:]]+\\s+", level_pattern, "$"), "", first_condition, perl = TRUE))
}

mediation_mini_effects_table <- function(section_title, rows) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) return(NULL)
  if ("Path" %in% names(rows) && any(nzchar(trimws(as.character(rows[["Path"]] %||% ""))))) {
    path_labels <- as.character(rows[["Path"]] %||% "")
  } else if ("Indirect effect" %in% names(rows) && any(nzchar(trimws(as.character(rows[["Indirect effect"]] %||% ""))))) {
    path_labels <- as.character(rows[["Indirect effect"]] %||% "")
  } else {
    path_labels <- vapply(rows$Effect, function(eff) {
      lines <- strsplit(as.character(eff %||% ""), "\n", fixed = TRUE)[[1L]]
      lines <- trimws(lines)
      lines <- lines[nzchar(lines)]
      if (length(lines) >= 2L) lines[[length(lines)]] else as.character(eff %||% "")
    }, character(1L))
  }
  h1_st <- "padding:5px 18px;font-weight:700;font-size:12px;border-top:2px solid #1f2937;text-align:center;"
  h2_st <- "padding:5px 18px;font-weight:400;font-size:12px;border-bottom:2px solid #1f2937;text-align:right;"
  c_st <- "padding:5px 18px;font-size:12px;text-align:right;"
  effect_col_st <- "text-align:left;width:300px;min-width:300px;white-space:nowrap;overflow-wrap:normal;word-break:normal;"
  group_end <- rep(FALSE, length(path_labels))
  if (identical(tolower(as.character(section_title %||% "")), "conditional indirect effect")) {
    group_labels <- vapply(path_labels, mediation_moderation_conditional_effect_group_label, character(1))
    group_end <- c(group_labels[-1L] != group_labels[-length(group_labels)], TRUE)
  } else if (length(group_end) > 0L) {
    group_end[[length(group_end)]] <- TRUE
  }
  tags$div(
    class = "mm-effects-mini-section",
    tags$div(class = "mm-effects-section-title", section_title),
    tags$table(
      class = "mm-effects-table coefficient-table",
      style = "width:auto;min-width:400px;border-collapse:collapse;margin:0;",
      tags$thead(
        tags$tr(
          tags$th(style = paste0(h1_st, effect_col_st), ""),
          tags$th(colspan = 5, style = h1_st, section_title)
        ),
        tags$tr(
          tags$th(style = paste0(h2_st, effect_col_st), "Effect"),
          tags$th(style = h2_st, "B"),
          tags$th(style = h2_st, "Boot SE"),
          tags$th(style = h2_st, "LLCI"),
          tags$th(style = h2_st, "ULCI"),
          tags$th(style = h2_st, "p")
        )
      ),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        row_st <- if (isTRUE(group_end[[i]])) "border-bottom:2px solid #1f2937;" else "border-bottom:1px solid #d7dde5;"
        tags$tr(
          tags$td(style = paste0(c_st, effect_col_st, row_st), path_labels[[i]]),
          tags$td(style = paste0(c_st, row_st), as.character(row$Estimate[[1L]] %||% "")),
          tags$td(style = paste0(c_st, row_st), as.character(row[["Boot SE"]][[1L]] %||% "")),
          tags$td(style = paste0(c_st, row_st), as.character(row$LLCI[[1L]] %||% "")),
          tags$td(style = paste0(c_st, row_st), as.character(row$ULCI[[1L]] %||% "")),
          tags$td(style = paste0(c_st, row_st), as.character(if ("Boot p" %in% names(row)) row[["Boot p"]][[1L]] else ""))
        )
      }))
    )
  )
}

mediation_moderation_effect_path_labels <- function(rows) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) return(character(0))
  if ("Path" %in% names(rows) && any(nzchar(trimws(as.character(rows[["Path"]] %||% ""))))) {
    return(as.character(rows[["Path"]] %||% ""))
  }
  if ("Indirect effect" %in% names(rows) && any(nzchar(trimws(as.character(rows[["Indirect effect"]] %||% ""))))) {
    return(as.character(rows[["Indirect effect"]] %||% ""))
  }
  vapply(rows$Effect, function(eff) {
    lines <- strsplit(as.character(eff %||% ""), "\n", fixed = TRUE)[[1L]]
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    if (length(lines) >= 2L) lines[[length(lines)]] else as.character(eff %||% "")
  }, character(1L))
}

mediation_moderation_effect_rows <- function(result, prefix) {
  effect_table <- result$effect_table
  if (is.null(effect_table) || !is.data.frame(effect_table) || nrow(effect_table) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  eff_chr <- as.character(effect_table$Effect %||% "")
  rows <- effect_table[!is.na(eff_chr) & startsWith(eff_chr, prefix), , drop = FALSE]
  value_columns <- intersect(c("Estimate", "Boot SE", "LLCI", "ULCI"), names(rows))
  if (length(value_columns) > 0L && nrow(rows) > 0L) {
    has_value <- apply(rows[, value_columns, drop = FALSE], 1L, function(row) {
      values <- trimws(as.character(row %||% character(0)))
      any(nzchar(values) & !tolower(values) %in% c("na", "nan"))
    })
    rows <- rows[has_value, , drop = FALSE]
  }
  rows
}

mediation_moderation_indirect_effect_section_ui <- function(result) {
  indirect_rows <- mediation_moderation_effect_rows(result, "Indirect effect")
  if (nrow(indirect_rows) == 0L) {
    return(NULL)
  }
  div(
    class = "result-section regression-result-panel mm-indirect-effect-section",
    mediation_mini_effects_table("indirect effect", indirect_rows)
  )
}

mediation_moderation_combined_effects_section_ui <- function(result) {
  indirect_rows <- mediation_moderation_effect_rows(result, "Indirect effect")
  index_rows <- mediation_moderation_effect_rows(result, "Index of moderated mediation")
  relative_rows <- mediation_moderation_effect_rows(result, "Relative indirect effect")
  moderated_rows <- analysis_bind_rows(list(index_rows, relative_rows))
  sections <- Filter(
    Negate(is.null),
    list(
      mediation_mini_effects_table("indirect effect", indirect_rows),
      mediation_mini_effects_table("moderated mediation effect", moderated_rows)
    )
  )
  if (length(sections) == 0L) {
    return(NULL)
  }
  div(
    class = "result-section regression-result-panel mm-combined-effects-section",
    sections
  )
}

mediation_moderation_conditional_indirect_section_ui <- function(result) {
  effect_table <- result$effect_table
  if (is.null(effect_table) || !is.data.frame(effect_table) || nrow(effect_table) == 0L) {
    return(NULL)
  }
  eff_chr <- as.character(effect_table$Effect %||% "")
  conditional_rows <- effect_table[!is.na(eff_chr) & startsWith(eff_chr, "Conditional indirect effect"), , drop = FALSE]
  if (nrow(conditional_rows) == 0L) {
    return(NULL)
  }
  div(
    class = "result-section regression-result-panel mm-conditional-indirect-section",
    mediation_mini_effects_table("conditional indirect effect", conditional_rows)
  )
}

mediation_moderation_dw_summary_value <- function(result) {
  if (!isTRUE(result$residual_diagnostics)) {
    return(format_decimal3(result$dw_d))
  }
  dw_crit <- result$dw_crit %||% list(dU = NA_real_)
  sprintf(
    "%s (%s~%s)",
    format_decimal3(result$dw_d),
    format_decimal3(dw_crit$dU),
    format_decimal3(4 - dw_crit$dU)
  )
}

mediation_moderation_combined_table_widths <- function(table) {
  columns <- names(table)
  if (length(columns) == 0L) {
    return(table)
  }
  weights <- vapply(columns, function(column) {
    key <- result_column_key(column)
    if (key %in% c("term", "variable")) return(36)
    if (key %in% c("b", "beta", "t", "p", "bootp", "sr2", "f2")) return(9)
    if (key %in% c("se", "hc3se", "bootse", "llci", "ulci")) return(10)
    9
  }, numeric(1))
  if (all(is.finite(weights)) && sum(weights) > 0) {
    attr(table, "compact_column_widths") <- weights / sum(weights) * 100
  }
  table
}

mediation_moderation_path_signature_terms <- function(model) {
  tryCatch(
    {
      model_terms <- stats::terms(model)
      outcome <- as.character(stats::formula(model_terms)[[2L]] %||% "")
      predictors <- attr(model_terms, "term.labels") %||% character(0)
      paste(c(outcome, sort(as.character(predictors))), collapse = "\n")
    },
    error = function(e) ""
  )
}

mediation_moderation_path_signature_table <- function(result) {
  table <- result$coef_table
  if (!is.data.frame(table) || nrow(table) == 0L) {
    table <- result$display_table
  }
  if (!is.data.frame(table) || nrow(table) == 0L) {
    return("")
  }
  comparable_columns <- intersect(
    c("Term", "Variable", "B", "SE", "HC3 SE", "Boot SE", "t", "p", "LLCI", "ULCI", "Boot p", "f2"),
    names(table)
  )
  table <- table[, comparable_columns, drop = FALSE]
  if ("Term" %in% names(table)) {
    table$Term <- mediation_moderation_clean_term(table$Term)
  }
  if ("Variable" %in% names(table)) {
    table$Variable <- mediation_moderation_clean_term(table$Variable)
  }
  order_column <- intersect(c("Term", "Variable"), names(table))
  if (length(order_column) > 0L) {
    table <- table[order(as.character(table[[order_column[[1L]]]]), seq_len(nrow(table))), , drop = FALSE]
  }
  paste(vapply(table, function(column) paste(as.character(column), collapse = "\r"), character(1)), collapse = "\n")
}

mediation_moderation_path_result_signature <- function(result) {
  paste(
    mediation_moderation_path_signature_terms(result$model),
    as.character(result$n %||% ""),
    mediation_moderation_path_signature_table(result),
    sep = "\n---\n"
  )
}

mediation_moderation_path_result_outcome <- function(result) {
  tryCatch(
    as.character(stats::formula(stats::terms(result$model))[[2L]] %||% ""),
    error = function(e) ""
  )
}

mediation_moderation_collapse_duplicate_y_models <- function(path_results) {
  if (length(path_results) <= 1L) {
    return(path_results)
  }
  outcomes <- vapply(path_results, mediation_moderation_path_result_outcome, character(1))
  signatures <- ifelse(
    nzchar(outcomes),
    paste0("outcome:", outcomes),
    paste0("signature:", vapply(path_results, mediation_moderation_path_result_signature, character(1)))
  )
  signature_order <- unique(signatures)
  groups <- lapply(signature_order, function(signature) which(signatures == signature))
  keep <- vapply(groups, `[[`, integer(1), 1L)
  collapsed <- path_results[keep]
  for (index in seq_along(groups)) {
    source_indices <- groups[[index]]
    focal_values <- unique(vapply(path_results[source_indices], function(result) {
      as.character(result$focal %||% "")
    }, character(1)))
    focal_values <- focal_values[nzchar(focal_values)]
    attr(collapsed[[index]], "collapsed_focals") <- focal_values
    attr(collapsed[[index]], "shared_y_model") <- length(source_indices) > 1L
  }
  collapsed
}

mediation_moderation_combined_path_table_ui <- function(path_results, result, output_table_style = "standard") {
  output_table_style <- analysis_output_table_style(output_table_style)
  coefficient_output_table_style <- output_table_style
  m_path_results <- Filter(function(r) {
    grepl("^M model:", as.character(r$equation %||% "")[[1L]])
  }, path_results %||% list())
  if (length(m_path_results) > 1L) {
    m_groups <- split(m_path_results, vapply(m_path_results, function(r) {
      trimws(sub("^M model:\\s*", "", as.character(r$equation %||% "")[[1L]]))
    }, character(1L)))
    m_path_results <- lapply(m_groups, function(group) {
      interaction_index <- which(vapply(group, mediation_moderation_has_interaction, logical(1)))
      if (length(interaction_index) > 0L) {
        return(group[[interaction_index[[1L]]]])
      }
      group[[1L]]
    })
    roles <- result$roles %||% list()
    mediator_order <- as.character(roles$mediators %||% character(0))
    m_names <- vapply(m_path_results, function(r) {
      trimws(sub("^M model:\\s*", "", as.character(r$equation %||% "")[[1L]]))
    }, character(1L))
    order_index <- match(m_names, mediator_order)
    order_index[is.na(order_index)] <- length(mediator_order) + seq_len(sum(is.na(order_index)))
    m_path_results <- m_path_results[order(order_index, seq_along(m_path_results))]
  }

  y_path_results <- Filter(function(r) {
    identical(as.character(r$equation %||% "")[[1L]], "Y model")
  }, path_results %||% list())
  y_path_results <- mediation_moderation_collapse_duplicate_y_models(y_path_results)

  if (length(m_path_results) == 0L && length(y_path_results) == 0L) return(NULL)

  model_tables <- list()
  model_labels_html <- list()
  all_summary_values <- list()
  m_delta_label <- NULL
  last_result <- NULL
  residual_diagnostics_used <- FALSE

  for (i in seq_along(m_path_results)) {
    m_res <- m_path_results[[i]]
    group <- mediation_moderation_hierarchical_steps(m_res)
    full_result <- if (is.null(group)) m_res else group[[2L]]
    last_result <- full_result
    residual_diagnostics_used <- isTRUE(residual_diagnostics_used) ||
      if (is.null(group)) {
        isTRUE(full_result$residual_diagnostics)
      } else {
        any(vapply(group, function(step) isTRUE(step$residual_diagnostics), logical(1)))
      }

    tbl <- mediation_moderation_combined_table_widths(
      mediation_moderation_hierarchical_model_table(full_result, include_vif = FALSE, output_table_style = coefficient_output_table_style)
    )
    model_tables[[length(model_tables) + 1L]] <- tbl

    me_raw <- trimws(sub("^M model:\\s*", "", as.character(m_res$equation %||% "")[[1L]]))
    me_label <- mediation_moderation_effect_variable_label(
      me_raw, m_res$variable_info, m_res$labels %||% character(0)
    )
    if (!nzchar(me_label)) me_label <- me_raw

    model_labels_html[[length(model_labels_html) + 1L]] <- tagList(
      sprintf("Model 1-%d", i),
      tags$br(),
      tags$span(class = "mm-combined-sublabel", sprintf("(%s)", me_label))
    )

    if (is.null(group)) {
      sv <- list(
        f = sprintf("%s(%s)", format_decimal3(full_result$f_statistic), format_p(full_result$f_p)),
        r2 = sprintf("%s (%s)", format_decimal3(full_result$r_squared), format_decimal3(full_result$adjusted_r_squared)),
        delta = NULL,
        dw = mediation_moderation_dw_summary_value(full_result),
        normality = sprintf("%s (%s)", format_decimal3(full_result$normality_statistic), format_p(full_result$normality_p)),
        homogeneity = sprintf("%s (%s)", format_decimal3(full_result$homogeneity_statistic), format_p(full_result$homogeneity_p))
      )
      all_summary_values[[length(all_summary_values) + 1L]] <- sv
    } else {
      sv <- hierarchical_summary_values(group)
      if (is.null(m_delta_label)) m_delta_label <- attr(sv, "delta_label")
      all_summary_values[[length(all_summary_values) + 1L]] <- sv[[2L]]
    }
  }

  for (y_index in seq_along(y_path_results)) {
    y_path_result <- y_path_results[[y_index]]
    y_group <- mediation_moderation_hierarchical_steps(y_path_result)
    y_full_result <- if (is.null(y_group)) y_path_result else y_group[[2L]]
    last_result <- y_full_result
    residual_diagnostics_used <- isTRUE(residual_diagnostics_used) ||
      if (is.null(y_group)) {
        isTRUE(y_full_result$residual_diagnostics)
      } else {
        any(vapply(y_group, function(step) isTRUE(step$residual_diagnostics), logical(1)))
      }
    tbl <- mediation_moderation_combined_table_widths(
      mediation_moderation_hierarchical_model_table(y_full_result, include_vif = FALSE, output_table_style = coefficient_output_table_style)
    )
    model_tables[[length(model_tables) + 1L]] <- tbl

    y_name <- tryCatch(all.vars(stats::formula(y_full_result$model))[[1L]], error = function(e) "")
    y_label <- mediation_moderation_effect_variable_label(
      y_name, y_full_result$variable_info, y_full_result$labels %||% character(0)
    )
    if (!nzchar(y_label)) y_label <- y_name
    collapsed_focals <- as.character(attr(y_path_result, "collapsed_focals", exact = TRUE) %||% y_path_result$focal %||% character(0))
    focal_label <- vapply(collapsed_focals, function(focal_name) {
      label <- mediation_moderation_effect_variable_label(
        focal_name, y_path_result$variable_info, y_path_result$labels %||% character(0)
      )
      if (nzchar(label)) label else as.character(focal_name %||% "")
    }, character(1))
    focal_label <- paste(focal_label[nzchar(focal_label)], collapse = ", ")

    shared_y_model <- isTRUE(attr(y_path_result, "shared_y_model", exact = TRUE))
    y_sublabel <- if (isTRUE(shared_y_model)) {
      sprintf("(%s)", y_label)
    } else {
      sprintf("(%s; X: %s)", y_label, focal_label)
    }
    model_labels_html[[length(model_labels_html) + 1L]] <- tagList(
      if (length(y_path_results) == 1L) "Model 2" else sprintf("Model 2-%d", y_index),
      tags$br(),
      tags$span(class = "mm-combined-sublabel", y_sublabel)
    )

    if (is.null(y_group)) {
      all_summary_values[[length(all_summary_values) + 1L]] <- list(
        f         = sprintf("%s(%s)", format_decimal3(y_full_result$f_statistic), format_p(y_full_result$f_p)),
        r2        = sprintf("%s (%s)", format_decimal3(y_full_result$r_squared), format_decimal3(y_full_result$adjusted_r_squared)),
        delta     = NULL,
        dw        = mediation_moderation_dw_summary_value(y_full_result),
        normality = sprintf("%s (%s)", format_decimal3(y_full_result$normality_statistic), format_p(y_full_result$normality_p)),
        homogeneity = sprintf("%s (%s)", format_decimal3(y_full_result$homogeneity_statistic), format_p(y_full_result$homogeneity_p))
      )
    } else {
      y_summary_values <- hierarchical_summary_values(y_group)
      if (is.null(m_delta_label)) m_delta_label <- attr(y_summary_values, "delta_label")
      all_summary_values[[length(all_summary_values) + 1L]] <- y_summary_values[[2L]]
    }
  }

  if (length(model_tables) == 0L) return(NULL)

  attr(all_summary_values, "delta_label") <- m_delta_label %||% "Delta R\u00B2(bootstrap 95% CI)"
  attr(all_summary_values, "any_residual_diagnostics") <- isTRUE(residual_diagnostics_used)
  has_delta <- any(vapply(all_summary_values, function(sv) {
    hierarchical_summary_value_available(sv$delta)
  }, logical(1)))
  note_line <- if (!is.null(last_result)) mediation_moderation_combined_landscape_note_line(last_result) else NULL
  combined_landscape <- identical(output_table_style, "wide")

  div(
    class = paste(
      "result-section regression-result-panel mm-combined-path-section",
      if (isTRUE(combined_landscape)) "landscape-table-panel mm-combined-landscape-section" else "mm-combined-portrait-section"
    ),
    hierarchical_coefficient_html_table(
      model_tables,
      model_labels_html,
      all_summary_values,
      note_line = note_line,
      include_delta = has_delta,
      output_table_style = coefficient_output_table_style
    )
  )
}

mediation_moderation_result_ui <- function(result, language = statedu_initial_language(), dash_nonsignificant = TRUE, output_table_style = "standard") {
  if (is.null(result)) return(NULL)
  output_table_style <- analysis_output_table_style(output_table_style)
  overview <- result$overview
  path_results <- result$path_results
  effect_table <- result$effect_table

  has_m_interaction <- any(vapply(path_results %||% list(), function(r) {
    grepl("^M model:", as.character(r$equation %||% "")[[1L]]) &&
      isTRUE(mediation_moderation_has_interaction(r))
  }, logical(1)))
  has_moderated_mediation_index <- nrow(mediation_moderation_effect_rows(result, "Index of moderated mediation")) > 0L ||
    nrow(mediation_moderation_effect_rows(result, "Relative indirect effect")) > 0L
  use_combined_path_table <- isTRUE(has_m_interaction) || isTRUE(has_moderated_mediation_index)
  coefficient_landscape <- identical(output_table_style, "wide") && mediation_moderation_custom_coefficients_landscape(result, path_results)

  tags$div(
    class = "mm-results",
    tags$hr(),
    tags$h2("Results"),
    analysis_result_table_section("Model overview", overview, class = "result-section regression-result-panel", table_fn = model_overview_html_table),
    if (use_combined_path_table) {
      tagList(
        mediation_moderation_combined_path_table_ui(path_results, result, output_table_style = output_table_style),
        mediation_moderation_combined_effects_section_ui(result)
      )
    } else if (identical(as.character(result$model_number %||% ""), "4")) {
      mediation_moderation_model4_path_result_ui(path_results, landscape = coefficient_landscape, output_table_style = output_table_style)
    } else {
      lapply(path_results, mediation_moderation_path_result_ui, landscape = coefficient_landscape, output_table_style = output_table_style)
    },
    if (!use_combined_path_table) mediation_moderation_indirect_effect_section_ui(result),
    analysis_result_table_section("Model summary", result$model_summary_table, class = "result-section regression-result-panel mm-process-summary-section"),
    analysis_result_table_section("Interaction tests", result$interaction_table, class = "result-section regression-result-panel mm-interaction-tests-section"),
    analysis_result_table_section("Conditional effects", result$simple_slopes_table, class = "result-section regression-result-panel mm-conditional-effects-section"),
    mediation_moderation_conditional_indirect_section_ui(result),
    analysis_result_table_section("Johnson-Neyman", result$johnson_neyman_table, class = "result-section regression-result-panel mm-johnson-neyman-section"),
    mediation_moderation_conditional_plots_ui(result$conditional_plot_specs),
    if (!use_combined_path_table) {
      analysis_result_table_section("Bootstrap effects", effect_table, class = "result-section regression-result-panel")
    },
    analysis_result_table_section("Bootstrap diagnostics", result$effect_bootstrap_diagnostics, class = "result-section regression-result-panel mm-bootstrap-diagnostics-section"),
    result_note_tag(result$note),
    if (!isTRUE(result$custom_model_canvas)) {
      mediation_moderation_result_diagram_ui(result, language, dash_nonsignificant = dash_nonsignificant)
    }
  )
}

mediation_moderation_saved_results_html <- function(result, language = statedu_initial_language(), report_mode = FALSE, dash_nonsignificant = TRUE, output_table_style = "standard") {
  if (is.null(result)) {
    stop("No mediation / moderation result is available.", call. = FALSE)
  }
  output_table_style <- analysis_output_table_style(output_table_style)
  print_landscape <- identical(output_table_style, "wide")
  saved_results_document(
    "Mediation / moderation",
    mediation_moderation_result_ui(result, language, dash_nonsignificant = dash_nonsignificant, output_table_style = output_table_style),
    max_width = if (isTRUE(print_landscape)) 1500 else 1280,
    css_path = file.path("www", "style.css"),
    print_landscape = print_landscape,
    report_mode = report_mode
  )
}

write_mediation_moderation_results_html <- function(result, file, language = statedu_initial_language(), dash_nonsignificant = TRUE, output_table_style = "standard") {
  html <- mediation_moderation_saved_results_html(result, language = language, report_mode = FALSE, dash_nonsignificant = dash_nonsignificant, output_table_style = output_table_style)
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

write_mediation_moderation_results_pdf <- function(result, file, language = statedu_initial_language(), dash_nonsignificant = TRUE, output_table_style = "standard") {
  html <- mediation_moderation_saved_results_html(result, language = language, report_mode = TRUE, dash_nonsignificant = dash_nonsignificant, output_table_style = output_table_style)
  write_pdf_from_html(html, file)
  invisible(file)
}

mediation_moderation_export_tables <- function(result) {
  tables <- list(
    `Model overview` = result$overview,
    `Model summary` = result$model_summary_table,
    `Interaction tests` = result$interaction_table,
    `Conditional effects` = result$simple_slopes_table,
    `Johnson-Neyman` = result$johnson_neyman_table,
    `JN conditional effects` = result$johnson_neyman_detail_table,
    `Bootstrap effects` = result$effect_table,
    `Bootstrap diagnostics` = result$effect_bootstrap_diagnostics
  )
  path_tables <- lapply(seq_along(result$path_results %||% list()), function(index) {
    path_result <- result$path_results[[index]]
    table <- path_result$display_table %||% path_result$coef_table
    if (!is.data.frame(table)) return(NULL)
    table
  })
  names(path_tables) <- paste0("Path ", seq_along(path_tables))
  tables <- c(tables, path_tables)
  Filter(function(table) is.data.frame(table) && nrow(table) > 0L, tables)
}

save_mediation_moderation_excel_file <- function(result, file) {
  if (is.null(result)) {
    stop("No mediation / moderation result is available.", call. = FALSE)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Excel export requires the openxlsx package.")
  }
  workbook <- openxlsx::createWorkbook()
  styles <- excel_styles()
  used_names <- character(0)
  tables <- mediation_moderation_export_tables(result)
  if (length(tables) == 0L) {
    openxlsx::addWorksheet(workbook, "Results")
    openxlsx::writeData(workbook, "Results", "No data")
    openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    return(invisible(file))
  }
  for (name in names(tables)) {
    table <- tables[[name]]
    sheet <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", " ", name), 1L, 31L)
    sheet <- trimws(sheet)
    if (!nzchar(sheet)) sheet <- "Table"
    base <- sheet
    suffix <- 1L
    while (tolower(sheet) %in% tolower(used_names)) {
      suffix <- suffix + 1L
      sheet <- substr(sprintf("%s %s", base, suffix), 1L, 31L)
    }
    used_names <- c(used_names, sheet)
    openxlsx::addWorksheet(workbook, sheet)
    openxlsx::writeData(workbook, sheet, name, startRow = 1, startCol = 1, colNames = FALSE)
    openxlsx::mergeCells(workbook, sheet, cols = seq_len(max(1L, ncol(table))), rows = 1)
    openxlsx::addStyle(workbook, sheet, styles$title, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::writeData(workbook, sheet, table, startRow = 3, startCol = 1, withFilter = FALSE)
    openxlsx::addStyle(workbook, sheet, styles$header, rows = 3, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
    if (nrow(table) > 0L) {
      body_rows <- 4:(3 + nrow(table))
      openxlsx::addStyle(workbook, sheet, styles$body, rows = body_rows, cols = seq_len(ncol(table)), gridExpand = TRUE, stack = TRUE)
      openxlsx::addStyle(workbook, sheet, styles$left, rows = body_rows, cols = 1, gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::setColWidths(workbook, sheet, cols = seq_len(ncol(table)), widths = excel_table_column_widths(table))
    openxlsx::freezePane(workbook, sheet, firstActiveRow = 4)
  }
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  invisible(file)
}

mediation_moderation_plot_text_halo <- function(x, y, label, cex = 0.65, font = 1, adj = c(0.5, 0.5)) {
  label <- as.character(label %||% "")
  if (!nzchar(label)) {
    return(invisible(NULL))
  }
  offsets <- expand.grid(dx = c(-0.18, 0, 0.18), dy = c(-0.18, 0, 0.18))
  offsets <- offsets[!(offsets$dx == 0 & offsets$dy == 0), , drop = FALSE]
  for (index in seq_len(nrow(offsets))) {
    graphics::text(x + offsets$dx[[index]], y + offsets$dy[[index]], label, cex = cex, font = font, adj = adj, col = "#ffffff")
  }
  graphics::text(x, y, label, cex = cex, font = font, adj = adj, col = "#111827")
  invisible(NULL)
}

mediation_moderation_save_result_diagram_png <- function(result, file, language = statedu_initial_language(), dash_nonsignificant = TRUE, dpi = mediation_moderation_figure_dpi()) {
  diagram <- mediation_moderation_result_diagram_data(result)
  spec <- diagram$spec
  roles <- diagram$roles
  if (!is.list(spec) || !is.list(roles)) {
    return(FALSE)
  }
  positions <- mediation_moderation_spread_xy_positions(spec$positions)
  paths <- spec$paths %||% list()
  slots <- spec$slots %||% character(0)
  anchor_model <- mediation_moderation_anchor_model(spec)
  metrics <- mediation_moderation_diagram_metrics("result")
  edge_labels <- mediation_moderation_result_edge_coefficient_labels(result, spec)
  edge_significance <- if (isTRUE(dash_nonsignificant)) mediation_moderation_result_edge_coefficient_significance(result, spec) else list()
  grDevices::png(file, width = 6.8, height = 4.8, units = "in", res = dpi)
  closed <- FALSE
  tryCatch(
    {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mar = c(0.25, 0.25, 1.15, 0.25), xaxs = "i", yaxs = "i")
      graphics::plot.new()
      graphics::plot.window(xlim = c(0, 100), ylim = c(100, 0))
      graphics::rect(0, 0, 100, 100, border = "#b7c3cf", col = "#fbfcfd", lwd = 1)
      graphics::text(50, 5.5, as.character(spec$title %||% ""), cex = 1.05, font = 2, col = "#111827")
      for (path in paths) {
        source <- as.character(path[[1]] %||% "")
        target <- as.character(path[[2]] %||% "")
        if (!source %in% names(positions)) {
          next
        }
        from <- positions[[source]]
        to <- mediation_moderation_moderation_edge_point(source, target, positions, anchor_model)
        if (target %in% names(positions)) {
          to <- mediation_moderation_node_arrow_endpoint(from, to, metrics)
        }
        if (source %in% names(positions)) {
          from <- mediation_moderation_node_arrow_endpoint(to, from, metrics)
        }
        key <- mediation_moderation_path_key(path)
        significant <- edge_significance[[key]] %||% TRUE
        graphics::arrows(
          from[[1]], from[[2]], to[[1]], to[[2]],
          length = 0.08,
          angle = 24,
          code = 2,
          lwd = 1,
          lty = if (!isTRUE(significant)) 2 else 1,
          col = "#111827"
        )
        label <- as.character(edge_labels[[key]] %||% "")
        if (nzchar(label)) {
          amount <- mediation_moderation_arrow_label_amount(path)
          is_my_path <- grepl("^m[0-9]*$", source) && identical(target, "y")
          label_from <- if (isTRUE(is_my_path) && target %in% names(positions)) {
            mediation_moderation_node_arrow_endpoint(to, from, metrics)
          } else {
            from
          }
          label_point <- mediation_moderation_lerp_point(label_from, to, amount)
          x <- label_point[[1]]
          y <- label_point[[2]]
          if (identical(target, "xy") || grepl("^xy", target)) {
            y <- y - 3.2
          } else if (target %in% names(positions)) {
            y <- y - 2.2
          } else {
            y <- y + 2.4
          }
          mediation_moderation_plot_text_halo(x, y, label, cex = 0.58, font = 2, adj = if (isTRUE(is_my_path)) c(0, 0.5) else c(0.5, 0.5))
        }
      }
      for (slot in slots) {
        if (!slot %in% names(positions)) {
          next
        }
        position <- positions[[slot]]
        variable <- mediation_moderation_slot_variable(slot, roles)
        label <- if (length(variable) > 0L && nzchar(as.character(variable[[1]] %||% ""))) {
          mediation_moderation_display_name(variable[[1]], result$variable_info, result$labels)
        } else {
          mediation_moderation_slot_label(slot)
        }
        label <- paste(strwrap(label, width = 13), collapse = "\n")
        graphics::rect(
          position[[1]] - metrics$half_width,
          position[[2]] - metrics$half_height,
          position[[1]] + metrics$half_width,
          position[[2]] + metrics$half_height,
          border = "#111827",
          col = "#ffffff",
          lwd = 1
        )
        graphics::text(position[[1]], position[[2]], label, cex = 0.78, font = 2, col = "#111827")
      }
      grDevices::dev.off()
      closed <- TRUE
      TRUE
    },
    error = function(e) {
      FALSE
    },
    finally = {
      if (!closed) {
        try(grDevices::dev.off(), silent = TRUE)
      }
    }
  )
}

save_mediation_moderation_figures_to_dir <- function(result, directory, language = statedu_initial_language(), dash_nonsignificant = TRUE) {
  if (is.null(result)) {
    stop("No mediation / moderation result is available.", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  saved <- character(0)
  dpi <- mediation_moderation_figure_dpi()
  diagram_png_file <- file.path(directory, "mediation_moderation_model_diagram.png")
  if (isTRUE(mediation_moderation_save_result_diagram_png(result, diagram_png_file, language, dash_nonsignificant = dash_nonsignificant, dpi = dpi))) {
    saved <- c(saved, diagram_png_file)
  }
  diagram_file <- file.path(directory, "mediation_moderation_model_diagram.html")
  diagram_ui <- mediation_moderation_result_diagram_ui(result, language, dash_nonsignificant = dash_nonsignificant)
  if (!is.null(diagram_ui)) {
    diagram_html <- saved_results_document(
      "Mediation / moderation model diagram",
      diagram_ui,
      max_width = 720,
      css_path = file.path("www", "style.css")
    )
    writeLines(diagram_html, diagram_file, useBytes = TRUE)
    saved <- c(saved, diagram_file)
  }
  plot_specs <- result$conditional_plot_specs %||% list()
  for (index in seq_along(plot_specs)) {
    file <- file.path(directory, sprintf("mediation_moderation_plot_%02d.png", index))
    grDevices::png(file, width = 6.8, height = 4.8, units = "in", res = dpi)
    closed <- FALSE
    tryCatch(
      {
        mediation_moderation_print_plot(plot_specs[[index]])
        grDevices::dev.off()
        closed <- TRUE
        saved <- c(saved, file)
      },
      error = function(e) {
        NULL
      },
      finally = {
        if (!closed) {
          try(grDevices::dev.off(), silent = TRUE)
        }
      }
    )
  }
  saved
}

run_mediation_moderation_analysis <- function(
  data,
  roles,
  mediator_arrangement,
  moderated_paths,
  boot_r,
  seed,
  mean_center = FALSE,
  simple_slopes = TRUE,
  johnson_neyman = TRUE,
  analysis_method = "statedu",
  ci_method = "bias_corrected",
  residual_diagnostics = TRUE,
  auto_method = TRUE,
  direct_x = NULL,
  direct_x_to_y = NULL,
  x_to_m = NULL,
  m_to_y = NULL,
  m_to_m = NULL,
  moderated_x_to_m = NULL,
  moderated_m_to_y = NULL,
  moderation_map = NULL,
  two_moderator_model = "3",
  custom_path_model = FALSE,
  effect_size_models = "y",
  covariate_control = c("y", "m"),
  language = statedu_initial_language(),
  variable_info = NULL,
  labels = character(0),
  category_table = NULL,
  progress = NULL
) {
  shiny::validate(shiny::need(is.data.frame(data) && nrow(data) > 0, mediation_moderation_text(language, "Load a data file before running the analysis.", "\ubd84\uc11d \uc804\uc5d0 \ub370\uc774\ud130\ub97c \ubd88\ub7ec\uc624\uc138\uc694.")))
  roles <- mediation_moderation_role_values(roles$y, roles$x, roles$mediators, roles$w, roles$covariates, selected_names = names(data))
  shiny::validate(shiny::need(length(roles$y) >= 1L, mediation_moderation_text(language, "Select at least one dependent variable.", "\uc885\uc18d\ubcc0\uc218\ub97c 1\uac1c \uc774\uc0c1 \uc120\ud0dd\ud558\uc138\uc694.")))
  shiny::validate(shiny::need(length(roles$x) >= 1L, mediation_moderation_text(language, "Select at least one independent variable.", "\ub3c5\ub9bd\ubcc0\uc218\ub97c 1\uac1c \uc774\uc0c1 \uc120\ud0dd\ud558\uc138\uc694.")))
  boot_r <- as.integer(boot_r %||% 5000L)
  if (is.na(boot_r) || boot_r < 1L) {
    boot_r <- 5000L
  }
  seed <- as.integer(seed %||% default_seed())
  if (is.na(seed)) seed <- default_seed()
  analysis_method <- mediation_moderation_scalar_choice(analysis_method, "statedu", c("statedu", "process_ols"))
  ci_method <- mediation_moderation_scalar_choice(ci_method, "bias_corrected", c("bias_corrected", "percentile"))
  two_moderator_model <- mediation_moderation_two_moderator_model(two_moderator_model)
  residual_diagnostics <- isTRUE(residual_diagnostics)
  auto_method <- isTRUE(auto_method) && residual_diagnostics
  custom_path_model <- isTRUE(custom_path_model)
  direct_x_to_y <- mediation_moderation_normalize_outcome_map(
    direct_x_to_y %||% direct_x,
    outcomes = roles$y,
    allowed = roles$x,
    default = if (custom_path_model) character(0) else roles$x
  )
  direct_x <- unique(unlist(direct_x_to_y, use.names = FALSE))
  direct_x <- intersect(direct_x[nzchar(direct_x)], roles$x)
  x_to_m <- x_to_m %||% stats::setNames(lapply(roles$mediators, function(mediator) roles$x), roles$mediators)
  x_to_m <- stats::setNames(lapply(roles$mediators, function(mediator) {
    intersect(roles$x, as.character(x_to_m[[mediator]] %||% roles$x))
  }), roles$mediators)
  m_to_y <- mediation_moderation_normalize_outcome_map(
    m_to_y,
    outcomes = roles$y,
    allowed = roles$mediators,
    default = if (custom_path_model) character(0) else roles$mediators
  )
  m_to_m <- mediation_moderation_normalize_mediator_map(
    m_to_m,
    mediators = roles$mediators,
    default = character(0)
  )
  moderated_x_to_m <- mediation_moderation_normalize_moderated_x_to_m(
    moderated_x_to_m,
    roles$mediators,
    roles$x,
    moderated_paths
  )
  moderated_m_to_y <- mediation_moderation_normalize_moderated_m_to_y(
    moderated_m_to_y,
    roles$mediators,
    moderated_paths
  )
  moderation_map <- mediation_moderation_normalize_moderation_map(moderation_map, roles)
  effect_size_models <- intersect(tolower(as.character(effect_size_models %||% "y")), c("y", "m"))
  covariate_control <- mediation_moderation_covariate_control_values(covariate_control)
  refs <- regression_reference_values_static(category_table)
  value_labels <- category_value_label_lookup_static(category_table)
  structure <- mediation_moderation_structure_from_mediators(roles$mediators, mediator_arrangement)
  moderated_paths <- mediation_moderation_default_moderated_paths(list(mm_moderated_paths = moderated_paths), structure)
  spec <- mediation_moderation_builder_spec(
    structure,
    moderated_paths,
    mediator_count = max(1L, length(roles$mediators)),
    moderator_count = length(roles$w),
    two_moderator_model = two_moderator_model,
    language = language
  )
  result_spec <- mediation_moderation_builder_spec(
    structure,
    moderated_paths,
    mediator_count = max(1L, length(roles$mediators)),
    moderator_count = length(roles$w),
    two_moderator_model = two_moderator_model,
    language = "en"
  )
  model <- as.character(spec$model %||% NA_character_)
  if (isTRUE(custom_path_model) && (is.na(model) || !model %in% mediation_moderation_models())) {
    model <- "custom"
  }
  shiny::validate(shiny::need(isTRUE(custom_path_model) || (!is.na(model) && model %in% mediation_moderation_models()), mediation_moderation_text(language, "The current variable arrangement is not mapped to a supported model number.", "\ud604\uc7ac \ubcc0\uc218 \uad6c\uc131\uc740 \uc9c0\uc6d0\ud558\ub294 \ubaa8\ub378 \ubc88\ud638\uc640 \ub9e4\uce6d\ub418\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4.")))
  structure <- spec$structure
  if (!isTRUE(custom_path_model)) {
    moderated_paths <- mediation_moderation_model_moderated_paths(model)
  }
  if (identical(structure, "none")) {
    if (!isTRUE(custom_path_model)) {
      required_w <- mediation_moderation_model_moderator_count(model)
      shiny::validate(shiny::need(length(roles$w) >= required_w && length(roles$w) <= 2L && "xy" %in% moderated_paths, mediation_moderation_text(language, "Without mediators, select one or two moderators and the X -> Y moderated path.", "\ub9e4\uac1c\ubcc0\uc218\uac00 \uc5c6\uc73c\uba74 \uc870\uc808\ubcc0\uc218 1~2\uac1c\uc640 X -> Y \uc870\uc808\uacbd\ub85c\ub97c \uc120\ud0dd\ud558\uc138\uc694.")))
    }
  } else {
    shiny::validate(shiny::need(length(roles$mediators) >= 1L, mediation_moderation_text(language, "Select at least one mediator.", "\ub9e4\uac1c\ubcc0\uc218\ub97c 1\uac1c \uc774\uc0c1 \uc120\ud0dd\ud558\uc138\uc694.")))
  }
  required_w <- mediation_moderation_model_moderator_count(model)
  if (required_w > 0L && !identical(model, "custom")) {
    if (identical(structure, "none")) {
      shiny::validate(shiny::need(length(roles$w) == required_w, mediation_moderation_text(language, sprintf("This model number requires %s moderator(s).", required_w), sprintf("\uc774 \ubaa8\ub378 \ubc88\ud638\ub294 \uc870\uc808\ubcc0\uc218 %s\uac1c\uac00 \ud544\uc694\ud569\ub2c8\ub2e4.", required_w))))
    } else {
      shiny::validate(shiny::need(length(roles$w) >= 1L && length(roles$w) <= 2L, mediation_moderation_text(language, "Select one or two moderators for this moderated mediation model.", "\uc774 \uc870\uc808\ub41c \ub9e4\uac1c\ubaa8\ud615\uc5d0\ub294 \uc870\uc808\ubcc0\uc218 1~2\uac1c\ub97c \uc120\ud0dd\ud558\uc138\uc694.")))
    }
  }

  analysis_grid <- expand.grid(y = roles$y, x = roles$x, stringsAsFactors = FALSE)
  if (isTRUE(custom_path_model)) {
    keep_grid <- vapply(seq_len(nrow(analysis_grid)), function(index) {
      mediation_moderation_focal_reaches_outcome(
        focal = analysis_grid$x[[index]],
        outcome = analysis_grid$y[[index]],
        direct_x_to_y = direct_x_to_y,
        x_to_m = x_to_m,
        m_to_y = m_to_y,
        m_to_m = m_to_m,
        mediators = roles$mediators
      )
    }, logical(1))
    analysis_grid <- analysis_grid[keep_grid, , drop = FALSE]
    shiny::validate(shiny::need(nrow(analysis_grid) > 0L, mediation_moderation_text(language, "Draw at least one path from an independent variable to a dependent variable.", "\ub3c5\ub9bd\ubcc0\uc218\uc5d0\uc11c \uc885\uc18d\ubcc0\uc218\ub85c \uc774\uc5b4\uc9c0\ub294 \uacbd\ub85c\ub97c 1\uac1c \uc774\uc0c1 \uadf8\ub824\uc8fc\uc138\uc694.")))
  }
  progress_total <- nrow(analysis_grid) * boot_r
  results <- lapply(seq_len(nrow(analysis_grid)), function(grid_index) {
    outcome <- analysis_grid$y[[grid_index]]
    focal <- analysis_grid$x[[grid_index]]
    adjusted_roles <- roles
    adjusted_roles$y <- outcome
    adjusted_roles$covariates <- unique(c(setdiff(roles$x, focal), roles$covariates))
    adjusted_roles$x <- focal
    outcome_direct_x <- as.character(direct_x_to_y[[outcome]] %||% character(0))
    mediation_moderation_boot_effects(
      data = data,
      roles = adjusted_roles,
      focal = focal,
      structure = structure,
      model = model,
      mean_center = isTRUE(mean_center),
      boot_r = boot_r,
      seed = seed,
      variable_info = variable_info,
      labels = labels,
      category_table = category_table,
      refs = refs,
      value_labels = value_labels,
      progress = progress,
      progress_offset = (grid_index - 1L) * boot_r,
      progress_total = progress_total,
      analysis_method = analysis_method,
      ci_method = ci_method,
      residual_diagnostics = residual_diagnostics,
      auto_method = auto_method,
      moderated_paths = moderated_paths,
      direct_x = outcome_direct_x,
      x_to_m = x_to_m,
      m_to_y = m_to_y,
      m_to_m = m_to_m,
      moderated_x_to_m = moderated_x_to_m,
      moderated_m_to_y = moderated_m_to_y,
      moderation_map = moderation_map,
      custom_path_model = custom_path_model,
      all_x = roles$x,
      effect_size_models = effect_size_models,
      covariate_control = covariate_control
    )
  })

  covariate_control_label <- c(
    if ("y" %in% covariate_control) "Y model" else character(0),
    if ("m" %in% covariate_control) "M model" else character(0)
  )
  covariate_control_label <- if (length(covariate_control_label) == 0L) "-" else paste(covariate_control_label, collapse = ", ")
  overview <- data.frame(
    Item = c("Model", "Outcome", "Focal X analyses", "Direct X -> Y paths", "Mediators", "Moderator", "Covariate control", "Analysis method", "Residual diagnostics", "Automatic method selection", "Bootstrap samples", "Bootstrap CI", "Seed", "Missing data"),
    Value = c(
      spec$title,
      paste(roles$y, collapse = ", "),
      paste(roles$x, collapse = ", "),
      if (length(direct_x) == 0L) "-" else paste(direct_x, collapse = ", "),
      if (length(roles$mediators) == 0) "-" else paste(roles$mediators, collapse = ", "),
      if (length(roles$w) == 0) "-" else paste(roles$w, collapse = ", "),
      covariate_control_label,
      mediation_moderation_analysis_method_label(analysis_method),
      if (isTRUE(residual_diagnostics)) "Run" else "Not run",
      if (isTRUE(auto_method)) "On" else "Off",
      as.character(boot_r),
      bootstrap_ci_method_label(ci_method),
      as.character(seed),
      "Complete cases for each focal-X model"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  path_table <- analysis_bind_rows(lapply(results, `[[`, "path_table"))
  path_results <- unlist(lapply(results, `[[`, "path_results"), recursive = FALSE)
  effect_table <- do.call(rbind, lapply(results, `[[`, "effect_table"))
  attr(effect_table, "compact_column_widths") <- c(9, 6, 17, 31, 8, 8, 8, 8, 5)
  effect_bootstrap_diagnostics <- analysis_bind_rows(lapply(results, `[[`, "effect_bootstrap_diagnostics"))
  model_summary_table <- mediation_moderation_model_summary_process_table(path_results)
  interaction_table <- mediation_moderation_interaction_change_table(path_results)
  simple_slopes_table <- if (isTRUE(simple_slopes)) {
    mediation_moderation_simple_slopes_table(path_results)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  johnson_neyman_table <- if (isTRUE(johnson_neyman)) {
    mediation_moderation_johnson_neyman_table(path_results)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  johnson_neyman_detail_table <- if (isTRUE(johnson_neyman)) {
    mediation_moderation_jn_detail_table(path_results)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  conditional_plot_specs <- if (isTRUE(johnson_neyman)) {
    mediation_moderation_conditional_plot_specs(path_results)
  } else {
    list()
  }
  focal_note <- if (length(roles$x) > 1L) {
    "Each independent variable was analyzed as the focal X once. The other independent variables were included as covariates with the same bootstrap sample count and seed."
  } else {
    ""
  }
  note_parts <- c(
    focal_note,
    if (identical(analysis_method, "process_ols") || !isTRUE(auto_method)) {
      paste(
        "Path coefficients are ordered as covariates, independent variables, mediators, moderators, and interaction terms.",
        "Coefficient p values and interaction R\u00B2 change tests use ordinary least squares for PROCESS-compatible comparison.",
        if (length(effect_size_models) > 0L) "f\u00B2 = Cohen's f-squared effect size for each selected model coefficient." else "",
        "Standardized beta is not reported for mediation/moderation path coefficients."
      )
    } else {
      paste(
        "Path coefficients are ordered as covariates, independent variables, mediators, moderators, and interaction terms.",
        "StatEdu diagnostic-based output uses HC3 robust standard errors when homoscedasticity is rejected and bootstrap coefficient intervals when residual normality is rejected.",
        if (length(effect_size_models) > 0L) "f\u00B2 = Cohen's f-squared effect size for each selected model coefficient." else "",
        "Standardized beta is not reported for mediation/moderation path coefficients."
      )
    },
    sprintf("Bootstrap effect confidence limits use the %s method.", bootstrap_ci_method_label(ci_method)),
    "Bootstrap p values use a two-sided plus-one sign-count calculation. Adequate requires at least 80% valid resamples; Caution denotes 50% to less than 80%; intervals and p values are suppressed below 50% valid or fewer than 20 valid resamples.",
    if (isTRUE(residual_diagnostics)) "Residual normality, homoscedasticity, and Durbin-Watson diagnostics are reported for review." else "Residual diagnostics were not run."
  )
  note <- paste(note_parts[nzchar(note_parts)], collapse = "\n")
  overview$Value[overview$Item == "Model"] <- result_spec$title
  list(
    model_number = as.character(model),
    diagram_spec = result_spec,
    roles = roles,
    direct_x = direct_x,
    direct_x_to_y = direct_x_to_y,
    x_to_m = x_to_m,
    m_to_y = m_to_y,
    m_to_m = m_to_m,
    moderated_paths = moderated_paths,
    moderated_x_to_m = moderated_x_to_m,
    moderated_m_to_y = moderated_m_to_y,
    moderation_map = moderation_map,
    effect_size_models = effect_size_models,
    covariate_control = covariate_control,
    variable_info = variable_info,
    labels = labels,
    overview = overview,
    path_table = path_table,
    path_results = path_results,
    model_summary_table = model_summary_table,
    interaction_table = interaction_table,
    simple_slopes_table = simple_slopes_table,
    johnson_neyman_table = johnson_neyman_table,
    johnson_neyman_detail_table = johnson_neyman_detail_table,
    conditional_plot_specs = conditional_plot_specs,
    effect_table = effect_table,
    effect_bootstrap_diagnostics = effect_bootstrap_diagnostics,
    note = note
  )
}

mediation_moderation_tab_panel <- function(title = "Mediation / Moderation", language = statedu_initial_language()) {
  language <- normalize_app_language(language)
  tabPanel(
    title,
    value = "analysis_mediation_moderation",
    div(
      class = "page-shell",
      div(
        class = "app-heading",
        h1(mediation_moderation_title(language)),
        div(
          mediation_moderation_text(
            language,
            "Build a PROCESS-style model by assigning variables to outcome, predictor, mediator, moderator, and covariate roles.",
            "\uc885\uc18d\ubcc0\uc218, \ub3c5\ub9bd\ubcc0\uc218, \ub9e4\uac1c\ubcc0\uc218, \uc870\uc808\ubcc0\uc218, \uacf5\ubcc0\ub7c9 \uc5ed\ud560\uc5d0 \ubcc0\uc218\ub97c \ubc30\uce58\ud574 PROCESS \ud615\ud0dc\uc758 \ubaa8\ud615\uc744 \ub9cc\ub4ed\ub2c8\ub2e4."
          ),
          class = "app-subtitle"
        )
      ),
      div(
        class = "workspace-panel frequencies-workspace-panel mm-workspace-panel",
        analysis_workspace_heading(mediation_moderation_title(language), "mediation_moderation", language),
        analysis_workspace_body(
          "mediation_moderation",
          uiOutput("mediation_moderation_setup"),
          div(
            class = "analysis-action-row regression-action-row mm-action-row",
            actionButton(
              "run_mediation_moderation",
              analysis_ui_text("Run analysis", language),
              class = "btn-primary"
            ),
            uiOutput("mediation_moderation_save_control")
          ),
          uiOutput("mediation_moderation_results")
        )
      )
    )
  )
}

register_mediation_moderation_setup_output <- function(
  input,
  output,
  session,
  dataset_fn,
  selected_names_fn,
  variable_table_fn,
  labels_fn,
  category_table_fn = function() NULL,
  mark_settings_dirty = function() NULL,
  app_language_fn = NULL
) {
  mm_y <- reactiveVal(character(0))
  mm_x <- reactiveVal(character(0))
  mm_mediators <- reactiveVal(character(0))
  mm_w <- reactiveVal(character(0))
  mm_covariates <- reactiveVal(character(0))
  mm_moderated_paths <- reactiveVal(character(0))
  active_mm_list <- reactiveVal("mm_available")
  mm_setup_revision <- reactiveVal(0L)

  mm_ids <- c("mm_available", "mm_y", "mm_mediators", "mm_x", "mm_w", "mm_covariates")

  refresh_mm_setup <- function() {
    mm_setup_revision(isolate(mm_setup_revision()) + 1L)
  }

  current_moderation_structure <- function() {
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected_names_fn()
    )
    arrangement <- input$mm_mediator_arrangement %||% "parallel"
    mediation_moderation_structure_from_mediators(roles$mediators, arrangement)
  }

  normalize_moderated_paths <- function(values, structure = current_moderation_structure()) {
    mediation_moderation_default_moderated_paths(
      list(mm_moderated_paths = values),
      structure
    )
  }

  normalize_selected <- function(values) {
    values <- as.character(values %||% character(0))
    intersect(values[nzchar(values)], as.character(selected_names_fn() %||% character(0)))
  }

  clear_transfer_selection <- function() {
    session$sendCustomMessage("easyflow-clear-transfer-selection", list(inputIds = mm_ids))
  }

  remove_from_target <- function(target, selected) {
    updated <- remove_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  append_to_target <- function(target, selected) {
    updated <- append_order_items(target(), selected)
    if (!updated$changed) return(FALSE)
    target(updated$order)
    TRUE
  }

  remove_from_all_targets <- function(items) {
    changed <- FALSE
    if (remove_from_target(mm_y, items)) changed <- TRUE
    if (remove_from_target(mm_x, items)) changed <- TRUE
    if (remove_from_target(mm_mediators, items)) changed <- TRUE
    if (remove_from_target(mm_w, items)) changed <- TRUE
    if (remove_from_target(mm_covariates, items)) changed <- TRUE
    changed
  }

  set_single_role <- function(target, selected) {
    selected <- normalize_selected(selected)
    if (length(selected) == 0) return(FALSE)
    selected <- selected[[1]]
    changed <- remove_from_all_targets(selected)
    if (!identical(target(), selected)) {
      target(selected)
      changed <- TRUE
    }
    changed
  }

  add_multi_role <- function(target, selected) {
    selected <- normalize_selected(selected)
    if (length(selected) == 0) return(FALSE)
    changed <- remove_from_all_targets(selected)
    if (append_to_target(target, selected)) changed <- TRUE
    changed
  }

  sync_current_variables <- function() {
    selected <- as.character(selected_names_fn() %||% character(0))
    mm_y(intersect(mm_y(), selected))
    mm_x(intersect(mm_x(), selected))
    mm_mediators(intersect(mm_mediators(), selected))
    mm_w(utils::head(intersect(mm_w(), selected), 2))
    mm_covariates(intersect(mm_covariates(), selected))
  }

  move_direction <- function(target_input_id) {
    available_selected <- as.character(input$mm_available %||% character(0))
    target_selected <- as.character(input[[target_input_id]] %||% character(0))
    active <- active_mm_list()
    if (identical(active, target_input_id) && length(target_selected) > 0) return("remove")
    if (identical(active, "mm_available") && length(available_selected) > 0) return("add")
    if (length(target_selected) > 0) return("remove")
    if (length(available_selected) > 0) return("add")
    "add"
  }

  move_button_label <- function(target_input_id) {
    if (identical(move_direction(target_input_id), "remove")) "<" else ">"
  }

  output$mediation_moderation_setup <- renderUI({
    language <- statedu_current_language(app_language_fn)
    mm_setup_revision()
    selected <- as.character(selected_names_fn() %||% character(0))
    if (length(selected) == 0) {
      return(setup_empty_message("Complete Step 2 in the Data tab before setting up regression.", language = language))
    }
    sync_current_variables()
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected
    )
    arrangement <- isolate(input$mm_mediator_arrangement %||% "parallel")
    structure <- mediation_moderation_structure_from_mediators(roles$mediators, arrangement)
    mediation_moderation_setup_panel(
      selected_names = selected,
      variable_table = variable_table_fn(),
      labels = labels_fn(),
      roles = roles,
      mediator_arrangement = arrangement,
      moderated_paths = normalize_moderated_paths(isolate(mm_moderated_paths()), structure),
      selected_available = isolate(input$mm_available),
      selected_y = isolate(input$mm_y),
      selected_x = isolate(input$mm_x),
      selected_mediators = isolate(input$mm_mediators),
      selected_w = isolate(input$mm_w),
      selected_covariates = isolate(input$mm_covariates),
      input = input,
      language = language
    )
  })

  mm_result <- reactiveVal(NULL)
  mm_bootstrap_job <- reactiveVal(NULL)
  cancel_mediation_moderation_bootstrap <- function() {
    job <- shiny::isolate(mm_bootstrap_job())
    statedu_stop_background_process_tree(job$process)
    mediation_moderation_cleanup_bootstrap_job(job)
    mm_bootstrap_job(NULL)
    shiny::removeNotification("mediation-moderation-bootstrap-progress")
    invisible(!is.null(job))
  }
  output$mediation_moderation_results <- renderUI({
    mediation_moderation_result_ui(
      mm_result(),
      statedu_current_language(app_language_fn),
      dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE),
      output_table_style = analysis_output_table_style(input$mm_output_table_style)
    )
  })
  output$mediation_moderation_save_control <- renderUI({
    if (is.null(mm_result())) {
      return(NULL)
    }
    div(
      class = "mm-save-control",
      analysis_save_buttons(
        html_button_id = "save_mediation_moderation_html_dialog",
        pdf_button_id = "save_mediation_moderation_pdf_dialog",
        figure_button_id = "save_mediation_moderation_figures_dialog",
        excel_button_id = "save_mediation_moderation_excel_dialog",
        add_result_button_id = "add_mediation_moderation_result",
        language = statedu_current_language(app_language_fn)
      )
    )
  })

  register_analysis_data_viewer_handlers(
    input = input,
    output = output,
    prefix = "mediation_moderation",
    title = mediation_moderation_text(
      statedu_current_language(app_language_fn),
      "Mediation / Moderation Data Viewer",
      "\ub9e4\uac1c\u00b7\uc870\uc808 \ub370\uc774\ud130 \ubcf4\uae30"
    ),
    dataset_fn = dataset_fn,
    selected_names_fn = selected_names_fn,
    variables_fn = function() unique(c(mm_y(), mm_x(), mm_mediators(), mm_w(), mm_covariates())),
    variable_table_fn = variable_table_fn,
    labels_fn = labels_fn,
    category_table_fn = category_table_fn,
    language_fn = app_language_fn
  )

  lapply(mm_ids, function(id) {
    force(id)
    observeEvent(input[[paste0(id, "_active")]], {
      active_mm_list(id)
    }, ignoreInit = TRUE)
  })

  observe({
    updateActionButton(session, "mm_y_move", label = move_button_label("mm_y"))
    updateActionButton(session, "mm_mediators_move", label = move_button_label("mm_mediators"))
    updateActionButton(session, "mm_x_move", label = move_button_label("mm_x"))
    updateActionButton(session, "mm_w_move", label = move_button_label("mm_w"))
    updateActionButton(session, "mm_covariates_move", label = move_button_label("mm_covariates"))
  })

  handle_move <- function(target_id, target, multi = FALSE) {
    changed <- FALSE
    if (identical(move_direction(target_id), "remove")) {
      if (remove_from_target(target, normalize_selected(input[[target_id]]))) changed <- TRUE
      active_mm_list("mm_available")
    } else if (isTRUE(multi)) {
      if (add_multi_role(target, input$mm_available)) changed <- TRUE
    } else {
      if (set_single_role(target, input$mm_available)) changed <- TRUE
    }
    clear_transfer_selection()
    if (changed) {
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }

  handle_target_doubleclick <- function(target_id, target) {
    event <- input[[paste0(target_id, "_doubleclick")]]
    value <- normalize_selected(event$value %||% character(0))
    if (length(value) == 0) return()
    if (remove_from_target(target, value)) {
      active_mm_list("mm_available")
      clear_transfer_selection()
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }

  observeEvent(input$mm_y_move, handle_move("mm_y", mm_y, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_mediators_move, handle_move("mm_mediators", mm_mediators, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_x_move, handle_move("mm_x", mm_x, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_w_move, handle_move("mm_w", mm_w, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_covariates_move, handle_move("mm_covariates", mm_covariates, multi = TRUE), ignoreInit = TRUE)
  observeEvent(input$mm_y_doubleclick, handle_target_doubleclick("mm_y", mm_y), ignoreInit = TRUE)
  observeEvent(input$mm_x_doubleclick, handle_target_doubleclick("mm_x", mm_x), ignoreInit = TRUE)
  observeEvent(input$mm_mediators_doubleclick, handle_target_doubleclick("mm_mediators", mm_mediators), ignoreInit = TRUE)
  observeEvent(input$mm_w_doubleclick, handle_target_doubleclick("mm_w", mm_w), ignoreInit = TRUE)
  observeEvent(input$mm_covariates_doubleclick, handle_target_doubleclick("mm_covariates", mm_covariates), ignoreInit = TRUE)

  observeEvent(input$analysis_transfer_drop, {
    drop <- input$analysis_transfer_drop
    source <- as.character(drop$source %||% "")
    target <- as.character(drop$target %||% "")
    values <- normalize_selected(drop$values %||% character(0))
    if (!source %in% mm_ids || !target %in% mm_ids || identical(source, target) || length(values) == 0) return()

    changed <- FALSE
    if (identical(target, "mm_available")) {
      changed <- remove_from_all_targets(values)
      active_mm_list("mm_available")
    } else if (identical(target, "mm_y")) {
      changed <- add_multi_role(mm_y, values)
      active_mm_list("mm_y")
    } else if (identical(target, "mm_x")) {
      changed <- add_multi_role(mm_x, values)
      active_mm_list("mm_x")
    } else if (identical(target, "mm_w")) {
      changed <- add_multi_role(mm_w, values)
      active_mm_list("mm_w")
    } else if (identical(target, "mm_mediators")) {
      changed <- add_multi_role(mm_mediators, values)
      active_mm_list("mm_mediators")
    } else if (identical(target, "mm_covariates")) {
      changed <- add_multi_role(mm_covariates, values)
      active_mm_list("mm_covariates")
    }

    if (changed) {
      clear_transfer_selection()
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_mediators_up, {
    updated <- move_order_item(mm_mediators(), input$mm_mediators, "up")
    if (updated$changed) {
      mm_mediators(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_x_up, {
    updated <- move_order_item(mm_x(), input$mm_x, "up")
    if (updated$changed) {
      mm_x(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_x_down, {
    updated <- move_order_item(mm_x(), input$mm_x, "down")
    if (updated$changed) {
      mm_x(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_mediators_down, {
    updated <- move_order_item(mm_mediators(), input$mm_mediators, "down")
    if (updated$changed) {
      mm_mediators(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_covariates_up, {
    updated <- move_order_item(mm_covariates(), input$mm_covariates, "up")
    if (updated$changed) {
      mm_covariates(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_covariates_down, {
    updated <- move_order_item(mm_covariates(), input$mm_covariates, "down")
    if (updated$changed) {
      mm_covariates(updated$order)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$mm_mediator_arrangement, {
    if (identical(input$mm_mediator_arrangement, "serial")) {
      mm_moderated_paths(character(0))
      updateCheckboxGroupInput(session, "mm_moderated_paths", selected = character(0))
      updateCheckboxInput(session, "mm_mean_center", value = FALSE)
      updateCheckboxInput(session, "mm_johnson_neyman", value = FALSE)
      updateCheckboxInput(session, "mm_simple_slopes", value = FALSE)
    }
    refresh_mm_setup()
    mark_settings_dirty()
  }, ignoreInit = TRUE)

  observe({
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected_names_fn()
    )
    arrangement <- input$mm_mediator_arrangement %||% "parallel"
    structure <- mediation_moderation_structure_from_mediators(roles$mediators, arrangement)
    if (identical(structure, "serial") || length(roles$w) == 0L) {
      if (length(isolate(mm_moderated_paths())) > 0L) {
        mm_moderated_paths(character(0))
      }
      updateCheckboxGroupInput(session, "mm_moderated_paths", selected = character(0))
      updateCheckboxInput(session, "mm_mean_center", value = FALSE)
      updateCheckboxInput(session, "mm_johnson_neyman", value = FALSE)
      updateCheckboxInput(session, "mm_simple_slopes", value = FALSE)
    }
  })

  observeEvent(input$mm_moderated_paths, {
    updated <- normalize_moderated_paths(input$mm_moderated_paths)
    if (!identical(isolate(mm_moderated_paths()), updated)) {
      mm_moderated_paths(updated)
      refresh_mm_setup()
      mark_settings_dirty()
    }
  }, ignoreInit = TRUE)

  lapply(c("mm_mean_center", "mm_johnson_neyman", "mm_simple_slopes", "mm_dash_nonsignificant", "mm_analysis_method", "mm_two_moderator_model", "mm_covariate_control_y", "mm_covariate_control_m", "mm_boot_r", "mm_seed", "mm_ci_method", "mm_options_tab", "mm_output_table_style"), function(input_id) {
    observeEvent(input[[input_id]], {
      mark_settings_dirty()
    }, ignoreInit = TRUE)
  })

  observeEvent(input$run_mediation_moderation, {
    language <- statedu_current_language(app_language_fn)
    data <- dataset_fn()
    roles <- mediation_moderation_role_values(
      y = mm_y(),
      x = mm_x(),
      mediators = mm_mediators(),
      w = mm_w(),
      covariates = mm_covariates(),
      selected_names = selected_names_fn()
    )
    cancel_mediation_moderation_bootstrap()
    job <- tryCatch(
      mediation_moderation_start_bootstrap_job(list(
        data = data,
        roles = roles,
        mediator_arrangement = input$mm_mediator_arrangement %||% "parallel",
        moderated_paths = mm_moderated_paths(),
        boot_r = as.integer(input$mm_boot_r %||% 5000L),
        seed = as.integer(input$mm_seed %||% default_seed()),
        mean_center = isTRUE(input$mm_mean_center),
        simple_slopes = isTRUE(input$mm_simple_slopes),
        johnson_neyman = isTRUE(input$mm_johnson_neyman),
        analysis_method = input$mm_analysis_method %||% "statedu",
        ci_method = input$mm_ci_method %||% "bias_corrected",
        two_moderator_model = input$mm_two_moderator_model %||% "3",
        covariate_control = c(
          if (isTRUE(input$mm_covariate_control_y %||% TRUE)) "y" else character(0),
          if (isTRUE(input$mm_covariate_control_m %||% TRUE)) "m" else character(0)
        ),
        language = language,
        variable_info = variable_table_fn(),
        labels = labels_fn(),
        category_table = category_table_fn()
      )),
      error = function(e) {
        showNotification(conditionMessage(e), type = "warning", duration = 7)
        NULL
      }
    )
    if (is.null(job)) return()
    mediation_moderation_claim_bootstrap(
      session,
      "mediation_moderation",
      cancel_mediation_moderation_bootstrap
    )
    mm_bootstrap_job(job)
    ko <- identical(normalize_app_language(language), "ko")
    structural_canvas_show_notification(
      statedu_bootstrap_status_ui(
        if (ko) "매개·조절 부트스트랩 진행 상태" else "Mediation / moderation bootstrap progress",
        if (ko) paste0("부트스트랩 작업 프로세스를 시작하는 중 · 예정 ", format(job$requested_total, big.mark = ","), "회") else paste0("Starting the bootstrap worker · ", format(job$requested_total, big.mark = ","), " resamples planned"),
        percent = NA_real_,
        stop_input_id = "mediation_moderation_bootstrap_stop",
        stop_label = if (ko) "부트스트랩 중단" else "Stop bootstrap",
        phase_label = if (ko) "작업 시작 중" else "Starting worker"
      ),
      type = "message", duration = NULL, id = "mediation-moderation-bootstrap-progress"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$mediation_moderation_bootstrap_stop, {
    job <- mm_bootstrap_job()
    if (is.null(job)) return()
    cancel_mediation_moderation_bootstrap()
    mediation_moderation_release_bootstrap(session, "mediation_moderation")
    language <- statedu_current_language(app_language_fn)
    structural_canvas_show_notification(
      mediation_moderation_text(language, "The mediation / moderation bootstrap was stopped.", "매개·조절 부트스트랩을 중단했습니다."),
      type = "warning", duration = 8
    )
  }, ignoreInit = TRUE)

  observe({
    job <- mm_bootstrap_job()
    if (is.null(job) || is.null(job$process)) return()
    if (job$process$is_alive()) {
      shiny::invalidateLater(400, session)
      language <- statedu_current_language(app_language_fn)
      progress <- mediation_moderation_bootstrap_job_progress(job, language)
      ko <- identical(normalize_app_language(language), "ko")
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          if (ko) "매개·조절 부트스트랩 진행 상태" else "Mediation / moderation bootstrap progress",
          progress$detail,
          percent = progress$percent,
          stop_input_id = "mediation_moderation_bootstrap_stop",
          stop_label = if (ko) "부트스트랩 중단" else "Stop bootstrap",
          phase_label = progress$phase_label
        ),
        type = "message", duration = NULL, id = "mediation-moderation-bootstrap-progress"
      )
      return()
    }
    status <- job$process$get_exit_status()
    language <- statedu_current_language(app_language_fn)
    ko <- identical(normalize_app_language(language), "ko")
    if (identical(status, 0L) && file.exists(job$result_file)) {
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          if (ko) "매개·조절 부트스트랩 진행 상태" else "Mediation / moderation bootstrap progress",
          if (ko) "저장된 분석 결과를 불러오는 중" else "Loading the saved analysis result",
          percent = NA_real_,
          stop_input_id = "mediation_moderation_bootstrap_stop",
          stop_label = if (ko) "부트스트랩 중단" else "Stop bootstrap",
          phase_label = if (ko) "결과 불러오는 중" else "Loading results"
        ),
        type = "message", duration = NULL, id = "mediation-moderation-bootstrap-progress"
      )
      result_read_started <- proc.time()[["elapsed"]]
      result <- readRDS(job$result_file)
      result_read_elapsed <- proc.time()[["elapsed"]] - result_read_started
      result_render_started <- proc.time()[["elapsed"]]
      mm_result(result)
      structural_canvas_show_notification(
        statedu_bootstrap_status_ui(
          if (ko) "매개·조절 부트스트랩 진행 상태" else "Mediation / moderation bootstrap progress",
          if (ko) "결과 표와 그림을 화면에 구성하는 중" else "Rendering result tables and figures",
          percent = NA_real_,
          stop_input_id = "mediation_moderation_bootstrap_stop",
          stop_label = if (ko) "부트스트랩 중단" else "Stop bootstrap",
          phase_label = if (ko) "결과 화면 구성 중" else "Rendering results"
        ),
        type = "message", duration = NULL, id = "mediation-moderation-bootstrap-progress"
      )
      session$onFlushed(function() {
        render_elapsed <- proc.time()[["elapsed"]] - result_render_started
        shiny::removeNotification("mediation-moderation-bootstrap-progress", session = session)
        message(sprintf(
          "[StatEdu timing] mediation/moderation result read %.3fs; server UI flush %.3fs",
          result_read_elapsed,
          render_elapsed
        ))
        shiny::showNotification(
          statedu_t("analysis.status.mediation_moderation_finished", language),
          type = "message", duration = 4, session = session
        )
      }, once = TRUE)
    } else {
      shiny::removeNotification("mediation-moderation-bootstrap-progress")
      error_text <- if (file.exists(job$error_file)) paste(readLines(job$error_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
      structural_canvas_show_notification(
        paste0(
          mediation_moderation_text(language, "The mediation / moderation bootstrap did not complete.", "매개·조절 부트스트랩을 완료하지 못했습니다."),
          if (nzchar(error_text)) paste0(" ", error_text) else ""
        ),
        type = "error", duration = 10
      )
    }
    mediation_moderation_cleanup_bootstrap_job(job)
    mm_bootstrap_job(NULL)
    mediation_moderation_release_bootstrap(session, "mediation_moderation")
  })

  session$onSessionEnded(function() {
    cancel_mediation_moderation_bootstrap()
    mediation_moderation_release_bootstrap(session, "mediation_moderation")
  })

  observeEvent(input$save_mediation_moderation_html_dialog, {
    shiny::req(!is.null(mm_result()))
    path <- choose_html_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.html?$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".html")
    }
    tryCatch(
      {
        write_mediation_moderation_results_html(
          mm_result(),
          path,
          statedu_current_language(app_language_fn),
          dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE),
          output_table_style = analysis_output_table_style(input$mm_output_table_style)
        )
        showNotification(sprintf(statedu_t("result.html_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.html_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_pdf_dialog, {
    shiny::req(!is.null(mm_result()))
    path <- choose_pdf_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".pdf")
    }
    tryCatch(
      {
        write_mediation_moderation_results_pdf(
          mm_result(),
          path,
          statedu_current_language(app_language_fn),
          dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE),
          output_table_style = analysis_output_table_style(input$mm_output_table_style)
        )
        showNotification(sprintf(statedu_t("result.pdf_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.pdf_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_excel_dialog, {
    shiny::req(!is.null(mm_result()))
    path <- choose_excel_save_path()
    if (length(path) == 0 || !nzchar(path[[1]])) {
      showNotification(statedu_t("result.save_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".xlsx")
    }
    tryCatch(
      {
        save_mediation_moderation_excel_file(mm_result(), path)
        showNotification(sprintf(statedu_t("result.excel_saved", statedu_current_language(app_language_fn)), path), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.excel_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$save_mediation_moderation_figures_dialog, {
    shiny::req(!is.null(mm_result()))
    directory <- choose_figure_save_dir()
    if (length(directory) == 0 || !nzchar(directory[[1]])) {
      showNotification(statedu_t("result.folder_dialog_canceled", statedu_current_language(app_language_fn)), type = "warning", duration = 5)
      return(invisible(NULL))
    }
    tryCatch(
      {
        saved <- save_mediation_moderation_figures_to_dir(
          mm_result(),
          directory,
          statedu_current_language(app_language_fn),
          dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE)
        )
        showNotification(sprintf(statedu_t("result.figures_saved", statedu_current_language(app_language_fn)), length(saved), directory), type = "message")
      },
      error = function(e) {
        showNotification(paste(statedu_t("result.figures_save_failed", statedu_current_language(app_language_fn)), conditionMessage(e)), type = "error", duration = 8)
      }
    )
  }, ignoreInit = TRUE)

  register_add_result_snapshot(
    input,
    session,
    "add_mediation_moderation_result",
    "Mediation / moderation",
    html_fn = function() {
      mediation_moderation_saved_results_html(
        mm_result(),
        statedu_current_language(app_language_fn),
        dash_nonsignificant = isTRUE(input$mm_dash_nonsignificant %||% TRUE),
        output_table_style = analysis_output_table_style(input$mm_output_table_style)
      )
    }
  )

  invisible(TRUE)
}
