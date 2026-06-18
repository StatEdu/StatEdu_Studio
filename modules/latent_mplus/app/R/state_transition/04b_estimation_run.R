T0_RUN <- Sys.time()

log_step_start("ESTIMATION_RUN", "04b_estimation_run.R")
log_info("Running Mplus state-transition model ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_read_lines <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) return(character(0))
  tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0))
  )
}

detect_mplus_status <- function(out_file) {
  txt <- safe_read_lines(out_file)
  if (length(txt) == 0) {
    return(list(status = "failed", parse_ok = FALSE, error_text = "Output file missing or unreadable"))
  }
  up <- toupper(txt)
  has_fatal <- any(grepl("^\\s*\\*\\*\\*\\s+ERROR", up))
  has_error_word <- any(grepl("AN ERROR HAS OCCURRED", up, fixed = TRUE))
  completed <- any(grepl("ENDING TIME:", up, fixed = TRUE)) || any(grepl("ELAPSED TIME:", up, fixed = TRUE))
  err_idx <- grep("^\\s*\\*\\*\\*\\s+ERROR", up)
  error_text <- if (length(err_idx) > 0) paste(txt[seq.int(err_idx[1], min(length(txt), err_idx[1] + 6L))], collapse = "\n") else NA_character_
  list(
    status = if (!has_fatal && !has_error_word && completed) "ok" else "failed",
    parse_ok = !has_fatal && !has_error_word && completed,
    error_text = error_text
  )
}

ESTIMATION_REGISTRY <- load_step_rds("ESTIMATION_REGISTRY", dir_rds = DIR_RDS, required = TRUE)
CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)

if (!exists("run_mplus_model")) {
  stop("run_mplus_model() not found. Make sure 07_mplus.R is sourced before 04b.", call. = FALSE)
}

run_rows <- lapply(seq_len(nrow(ESTIMATION_REGISTRY)), function(i) {
  inp_file <- as.character(ESTIMATION_REGISTRY$inp_file[i])
  mplus_exe <- as.character(ESTIMATION_REGISTRY$mplus_exe[i] %||% resolve_mplus_exe(CFG, must_exist = TRUE))

  exec_res <- run_mplus_model(
    inp_file = inp_file,
    mplus_exe = mplus_exe,
    workdir = dirname(inp_file),
    wait = TRUE,
    quiet = TRUE
  )

  status_info <- detect_mplus_status(exec_res$out_file %||% ESTIMATION_REGISTRY$out_file[i])

  data.frame(
    model_tag = ESTIMATION_REGISTRY$model_tag[i],
    model_type = ESTIMATION_REGISTRY$model_type[i] %||% NA_character_,
    inp_file = inp_file,
    out_file = exec_res$out_file %||% ESTIMATION_REGISTRY$out_file[i],
    log_file = exec_res$log_file %||% ESTIMATION_REGISTRY$log_file[i],
    status = status_info$status,
    parse_ok = status_info$parse_ok,
    system_ok = exec_res$ok %||% FALSE,
    system_status = exec_res$status %||% NA_integer_,
    error_text = status_info$error_text %||% exec_res$error %||% NA_character_,
    stringsAsFactors = FALSE
  )
})

ESTIMATION_RUN_RESULTS <- do.call(rbind, run_rows)
ESTIMATION_REGISTRY$status <- ESTIMATION_RUN_RESULTS$status
ESTIMATION_REGISTRY$parse_ok <- ESTIMATION_RUN_RESULTS$parse_ok
ESTIMATION_REGISTRY$out_file <- ESTIMATION_RUN_RESULTS$out_file
ESTIMATION_REGISTRY$log_file <- ESTIMATION_RUN_RESULTS$log_file

save_named_rds_list(
  list(
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY,
    ESTIMATION_RUN_RESULTS = ESTIMATION_RUN_RESULTS
  ),
  dir_rds = DIR_RDS
)

if (any(!ESTIMATION_RUN_RESULTS$parse_ok)) {
  stop("Mplus state-transition run failed. Check .out/.log files.", call. = FALSE)
}

log_info("Mplus run status = ", paste(ESTIMATION_RUN_RESULTS$model_type, ESTIMATION_RUN_RESULTS$status, sep = ":", collapse = ", "))
log_step_end("estimation_run", round(as.numeric(difftime(Sys.time(), T0_RUN, units = "secs")), 2), ok = TRUE)
