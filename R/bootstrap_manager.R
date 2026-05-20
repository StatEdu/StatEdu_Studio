# Bootstrap job state management for easyflow_statistics.

create_bootstrap_manager <- function(
  bootstrap_job,
  bootstrap_job_queue,
  bootstrap_status,
  bootstrap_cancel_requested,
  bootstrap_process,
  bootstrap_stop_visible,
  analysis_result
) {
  clear_state <- function(clear_queue = TRUE, notify = NULL, type = "message") {
    bootstrap_process(NULL)
    bootstrap_job(NULL)
    if (isTRUE(clear_queue)) {
      bootstrap_job_queue(list())
    }
    bootstrap_status(NULL)
    bootstrap_stop_visible(FALSE)
    bootstrap_cancel_requested(FALSE)
    if (!is.null(notify)) {
      shiny::showNotification(notify, type = type)
    }
    invisible(NULL)
  }

  cancel_jobs <- function(notify = NULL) {
    bootstrap_cancel_requested(TRUE)
    process <- bootstrap_process()
    if (!is.null(process) && process$is_alive()) {
      process$kill()
    }
    job <- bootstrap_job()
    if (!is.null(job)) {
      job$cancel <- TRUE
    }
    clear_state(clear_queue = TRUE, notify = notify, type = "warning")
  }

  start_job <- function(job, message = NULL) {
    bootstrap_status(list(
      done = 0L,
      r = job$r,
      stopping = FALSE,
      message = message %||% sprintf("Starting bootstrap %s", job$dependent %||% "")
    ))
    bootstrap_stop_visible(TRUE)
    bootstrap_job(job)
    bootstrap_process(start_bootstrap_process(job))
  }

  poll <- function() {
    job <- bootstrap_job()
    if (is.null(job)) {
      return()
    }
    process <- bootstrap_process()

    if (isTRUE(job$cancel) || isTRUE(bootstrap_cancel_requested())) {
      cancel_jobs("Bootstrap stopped.")
      return()
    }

    if (file.exists(job$progress_file)) {
      progress <- tryCatch(readRDS(job$progress_file), error = function(e) NULL)
      if (is.list(progress)) {
        job$done <- as.integer(progress$done %||% job$done)
        bootstrap_job(job)
        bootstrap_status(list(done = job$done, r = job$r, stopping = FALSE, message = "Running regression"))
      }
    }

    if (is.null(process)) {
      return()
    }
    exit_status <- process$get_exit_status()
    if (is.null(exit_status)) {
      return()
    }

    if (!identical(exit_status, 0L)) {
      clear_state(clear_queue = TRUE, notify = "Bootstrap failed or was interrupted.", type = "error")
      return()
    }

    if (file.exists(job$result_file)) {
      boot_result <- tryCatch(readRDS(job$result_file), error = function(e) NULL)
      results <- analysis_result()
      if (is.list(results) && length(results) >= job$result_index) {
        results[[job$result_index]]$boot_table <- bootstrap_summary_table(boot_result$samples, job$original_fit)
        results[[job$result_index]]$bootstrap_r_squared <- as.numeric(boot_result$r_squared %||% numeric(0))
        analysis_result(results)
      }
      bootstrap_process(NULL)
      bootstrap_job(NULL)
      bootstrap_cancel_requested(FALSE)
      queue <- bootstrap_job_queue()
      if (length(queue) > 0) {
        next_job <- queue[[1]]
        bootstrap_job_queue(queue[-1])
        start_job(next_job)
      } else {
        clear_state(clear_queue = FALSE, notify = "Bootstrap regression finished.", type = "message")
      }
    }
  }

  list(
    clear = clear_state,
    cancel = cancel_jobs,
    start = start_job,
    poll = poll
  )
}
