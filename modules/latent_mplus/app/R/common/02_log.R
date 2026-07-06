# ============================================================
# 02_log.R
# Logging utilities for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) 로그 파일 경로 설정
# 2) 콘솔/파일 로그 옵션 설정
# 3) 공통 logging helper 함수 등록
# 4) 파이프라인 단계별 일관된 로그 출력 지원
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.now_txt <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

.norm_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- gsub("/+", "/", x)
  x
}

.ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

.as_flag <- function(x, default = TRUE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(default)
  isTRUE(as.logical(x)[1])
}

# ------------------------------------------------------------
# 1. path / options
# ------------------------------------------------------------
if (!exists("DIR_LOGS") || is.null(DIR_LOGS) || !nzchar(DIR_LOGS)) {
  if (exists("DIR_OUTPUT") && !is.null(DIR_OUTPUT) && nzchar(DIR_OUTPUT)) {
    DIR_LOGS <- file.path(DIR_OUTPUT, "logs")
  } else {
    DIR_LOGS <- file.path(getwd(), "logs")
  }
}
DIR_LOGS <- .norm_path(DIR_LOGS)
.ensure_dir(DIR_LOGS)

if (!exists("PATH_RUN_LOG") || is.null(PATH_RUN_LOG) || !nzchar(PATH_RUN_LOG)) {
  PATH_RUN_LOG <- file.path(DIR_LOGS, "run_log.txt")
}
PATH_RUN_LOG <- .norm_path(PATH_RUN_LOG)

if (!exists("LOG_ENABLED")) LOG_ENABLED <- TRUE
if (!exists("LOG_TO_FILE")) LOG_TO_FILE <- TRUE
if (!exists("LOG_TO_CONSOLE")) LOG_TO_CONSOLE <- TRUE
if (!exists("LOG_APPEND")) LOG_APPEND <- TRUE

LOG_ENABLED    <- .as_flag(LOG_ENABLED, TRUE)
LOG_TO_FILE    <- .as_flag(LOG_TO_FILE, TRUE)
LOG_TO_CONSOLE <- .as_flag(LOG_TO_CONSOLE, TRUE)
LOG_APPEND     <- .as_flag(LOG_APPEND, TRUE)

# ------------------------------------------------------------
# 2. low-level writer
# ------------------------------------------------------------
.log_write_line <- function(line,
                            to_console = LOG_TO_CONSOLE,
                            to_file = LOG_TO_FILE,
                            file = PATH_RUN_LOG,
                            append = LOG_APPEND) {
  if (!LOG_ENABLED) return(invisible(line))

  line <- enc2utf8(as.character(line))

  if (isTRUE(to_console)) {
    cat(line, "\n", sep = "")
  }

  if (isTRUE(to_file)) {
    .ensure_dir(dirname(file))
    cat(
      line, "\n",
      file = file,
      append = append,
      sep = ""
    )
  }

  invisible(line)
}

.log_build <- function(level = "INFO", ...) {
  msg <- paste0(...)
  sprintf("[%s] [%s] %s", .now_txt(), level, msg)
}

# ------------------------------------------------------------
# 3. public logging functions
# ------------------------------------------------------------
log_txt <- function(...) {
  .log_write_line(.log_build("INFO", ...))
}

log_info <- function(...) {
  .log_write_line(.log_build("INFO", ...))
}

log_warn <- function(...) {
  .log_write_line(.log_build("WARN", ...))
}

log_error <- function(...) {
  .log_write_line(.log_build("ERROR", ...))
}

log_ok <- function(...) {
  .log_write_line(.log_build("OK", ...))
}

log_debug <- function(...) {
  .log_write_line(.log_build("DEBUG", ...))
}

log_rule <- function(char = "-", n = 56, level = "INFO") {
  .log_write_line(.log_build(level, paste(rep(char, n), collapse = "")))
}

log_blank <- function() {
  .log_write_line("")
}

log_kv <- function(key, value, level = "INFO", width = 16) {
  key <- as.character(key)[1]
  value <- if (length(value) == 0 || is.null(value)) "NULL" else paste(value, collapse = ", ")
  key_fmt <- sprintf(paste0("%-", width, "s"), key)
  .log_write_line(.log_build(level, key_fmt, " = ", value))
}

log_block <- function(title, ..., level = "INFO") {
  log_rule(level = level)
  .log_write_line(.log_build(level, title))
  if (length(list(...)) > 0) {
    .log_write_line(.log_build(level, paste0(...)))
  }
  log_rule(level = level)
}

# ------------------------------------------------------------
# 4. convenience helpers for pipeline steps
# ------------------------------------------------------------
log_step_start <- function(step_label = NULL, file_label = NULL) {
  if (!is.null(step_label)) {
    log_rule()
    log_info("[STEP] ", step_label)
    if (!is.null(file_label)) log_info("[FILE] ", file_label)
    log_rule()
  }
}

log_step_end <- function(step_name, elapsed_sec, ok = TRUE) {
  if (isTRUE(ok)) {
    log_ok(step_name, " completed (", elapsed_sec, " sec)")
  } else {
    log_error(step_name, " failed (", elapsed_sec, " sec)")
  }
}

# ------------------------------------------------------------
# 5. initialize log file header (optional)
# ------------------------------------------------------------
if (LOG_ENABLED && LOG_TO_FILE) {
  if (!file.exists(PATH_RUN_LOG) || !isTRUE(LOG_APPEND)) {
    .ensure_dir(dirname(PATH_RUN_LOG))
    cat("", file = PATH_RUN_LOG, append = FALSE)
  }
}

# ------------------------------------------------------------
# 6. export log settings summary
# ------------------------------------------------------------
LOG_SETTINGS <- list(
  LOG_ENABLED = LOG_ENABLED,
  LOG_TO_FILE = LOG_TO_FILE,
  LOG_TO_CONSOLE = LOG_TO_CONSOLE,
  LOG_APPEND = LOG_APPEND,
  DIR_LOGS = DIR_LOGS,
  PATH_RUN_LOG = PATH_RUN_LOG
)

# ------------------------------------------------------------
# 7. messages
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("02_log.R loaded\n")
cat("------------------------------------------------------------\n")
cat("PATH_RUN_LOG : ", PATH_RUN_LOG, "\n", sep = "")
cat("LOG_ENABLED  : ", LOG_ENABLED, "\n", sep = "")
cat("LOG_TO_FILE  : ", LOG_TO_FILE, "\n", sep = "")
cat("============================================================\n\n")