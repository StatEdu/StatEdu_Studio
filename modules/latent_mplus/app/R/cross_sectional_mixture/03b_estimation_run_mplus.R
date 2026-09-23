# ============================================================
# 03b_estimation_run_mplus.R
# Run prepared Mplus inputs and capture outputs robustly
# ------------------------------------------------------------
# 역할
# 1) 03a에서 생성한 run_plan 재로딩
# 2) 각 inp 파일에 대해 Mplus 실행
# 3) out / cprob 결과를 강건하게 탐지
# 4) run_results / ESTIMATION_REGISTRY 저장
#
# 핵심 수정
# - 03a에서 추가된 model_structure 유지
# - run_results에 k / model_structure 포함
# - ESTIMATION_REGISTRY에도 model_structure 안정 유지
# - model_structure 접두형 cprob 파일 우선 탐색
# ============================================================

T0_03B <- Sys.time()

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
.now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

if (!exists("run_mplus_model")) {
  stop("run_mplus_model() not found. Make sure 07_mplus.R is sourced before 03b.", call. = FALSE)
}
if (!exists("mixture_parse_run_quality")) {
  stop("mixture_parse_run_quality() not found. Make sure 15_mixture_selection_core.R is sourced before 03b.", call. = FALSE)
}

log_txt <- function(level = "INFO", ...) {
  cat(sprintf("[%s] [%s] %s\n", .now_txt(), level, paste0(...)))
}
log_info <- function(...) log_txt("INFO", ...)
log_warn <- function(...) log_txt("WARN", ...)
log_err  <- function(...) log_txt("ERROR", ...)

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_read_lines <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) {
    return(character(0))
  }
  tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0))
    }
  )
}

find_existing_first <- function(paths) {
  paths <- as.character(paths)
  paths <- unique(paths[!is.na(paths) & nzchar(trimws(paths))])
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

wait_for_nonempty_file <- function(path, max_tries = 8L, sleep_sec = 1) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  if (!file.exists(path)) {
    return(NA_character_)
  }
  for (i in seq_len(max_tries)) {
    finfo <- tryCatch(file.info(path), error = function(e) NULL)
    if (!is.null(finfo) && nrow(finfo) == 1 && is.finite(finfo$size) && finfo$size > 0) {
      return(path)
    }
    Sys.sleep(sleep_sec)
  }
  path
}

snapshot_file_states <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(trimws(paths))]
  if (length(paths) == 0L) {
    return(data.frame(path_key = character(0), existed = logical(0), size = numeric(0), mtime = as.POSIXct(character(0))))
  }
  info <- file.info(paths)
  data.frame(
    path_key = tolower(normalizePath(paths, winslash = "/", mustWork = FALSE)),
    existed = !is.na(info$size),
    size = suppressWarnings(as.numeric(info$size)),
    mtime = as.POSIXct(info$mtime),
    stringsAsFactors = FALSE
  )
}

file_was_fresh <- function(path, before, started_at, tolerance_sec = 5) {
  if (is.null(path) || length(path) == 0L || is.na(path[1]) || !file.exists(path[1])) return(FALSE)
  path <- as.character(path[1])
  after <- file.info(path)
  if (nrow(after) != 1L || !is.finite(after$size) || after$size <= 0) return(FALSE)

  key <- tolower(normalizePath(path, winslash = "/", mustWork = FALSE))
  hit <- match(key, before$path_key)
  changed <- is.na(hit) || !isTRUE(before$existed[hit]) ||
    !identical(suppressWarnings(as.numeric(after$size)), suppressWarnings(as.numeric(before$size[hit]))) ||
    is.na(before$mtime[hit]) || as.numeric(difftime(after$mtime, before$mtime[hit], units = "secs")) > 0
  recent <- !is.na(after$mtime) &&
    as.numeric(difftime(after$mtime, started_at, units = "secs")) >= -abs(tolerance_sec)
  isTRUE(changed && recent)
}

detect_mplus_status <- function(out_file, k = NA_integer_, exec_ok = TRUE, out_fresh = TRUE) {
  txt <- safe_read_lines(out_file)
  quality <- mixture_parse_run_quality(txt, k = k)
  execution_ok <- isTRUE(exec_ok)
  fresh_output <- isTRUE(out_fresh)
  quality_ok <- execution_ok && fresh_output && isTRUE(quality$run_quality_ok)
  reasons <- unique(c(
    if (!execution_ok) "execution_failed" else character(0),
    if (!fresh_output) "output_not_fresh" else character(0),
    quality$failure_reasons
  ))
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]

  list(
    status = if (quality_ok) "ok" else "failed",
    parse_ok = isTRUE(quality$output_present),
    exec_ok = execution_ok,
    out_fresh = fresh_output,
    output_present = isTRUE(quality$output_present),
    terminated_normally = isTRUE(quality$terminated_normally),
    converged = isTRUE(quality$converged),
    best_ll_replicated = quality$best_ll_replicated,
    loglik_replicated = quality$loglik_replicated,
    replication_ok = isTRUE(quality$replication_ok),
    local_maxima_warning = isTRUE(quality$local_maxima_warning),
    admissible = isTRUE(quality$admissible),
    run_quality_ok = quality_ok,
    failure_reasons = paste(reasons, collapse = ";"),
    error_text = if (length(reasons) > 0L) paste(reasons, collapse = "; ") else NA_character_,
    warning_text = if (length(quality$warning_lines) > 0L) paste(quality$warning_lines, collapse = "\n") else NA_character_,
    tail_text = if (length(txt) > 0L) paste(tail(txt, 30), collapse = "\n") else NA_character_
  )
}

# ------------------------------------------------------------
# 1. reload prepared objects
# ------------------------------------------------------------
CFG <- load_step_rds(
  "CFG",
  dir_rds = DIR_RDS,
  required = TRUE
)

ESTIMATION_REGISTRY <- load_step_rds(
  "ESTIMATION_REGISTRY",
  dir_rds = DIR_RDS,
  required = TRUE
)
ESTIMATION_REGISTRY_BUILD <- mixture_registry_to_row_df(ESTIMATION_REGISTRY)
if (!is.data.frame(ESTIMATION_REGISTRY_BUILD) || nrow(ESTIMATION_REGISTRY_BUILD) == 0L ||
    !"model_tag" %in% names(ESTIMATION_REGISTRY_BUILD)) {
  stop("Failed to normalize the estimation-build registry.", call. = FALSE)
}

ESTIMATION_BUILD_SUMMARY <- load_step_rds(
  "ESTIMATION_BUILD_SUMMARY",
  dir_rds = DIR_RDS,
  required = TRUE
)

SETTINGS_SUMMARY <- load_step_rds(
  "SETTINGS_SUMMARY",
  dir_rds = DIR_RDS,
  required = TRUE
)

log_info("Reloading estimation-build outputs ...")
dir.create(DIR_RDS, recursive = TRUE, showWarnings = FALSE)

if (!exists("DIR_MPLUS_INP") || is.null(DIR_MPLUS_INP) || !nzchar(DIR_MPLUS_INP)) {
  DIR_MPLUS_INP <- file.path(PROJECT_ROOT, "mplus_tmp", "inp")
}
if (!exists("DIR_MPLUS_OUT") || is.null(DIR_MPLUS_OUT) || !nzchar(DIR_MPLUS_OUT)) {
  DIR_MPLUS_OUT <- file.path(PROJECT_ROOT, "mplus_tmp", "out")
}
if (!exists("DIR_MPLUS_SAVEDATA") || is.null(DIR_MPLUS_SAVEDATA) || !nzchar(DIR_MPLUS_SAVEDATA)) {
  DIR_MPLUS_SAVEDATA <- file.path(PROJECT_ROOT, "mplus_tmp", "savedata")
}
if (!exists("DIR_TABLES") || is.null(DIR_TABLES) || !nzchar(DIR_TABLES)) {
  DIR_TABLES <- file.path(DIR_OUTPUT, "tables")
}

dir.create(DIR_MPLUS_INP, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MPLUS_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MPLUS_SAVEDATA, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TABLES, recursive = TRUE, showWarnings = FALSE)

EST_BUILD_RDS <- file.path(DIR_RDS, "estimation_build.rds")
if (!file.exists(EST_BUILD_RDS)) {
  stop("Missing estimation_build.rds: ", EST_BUILD_RDS)
}

est_build <- readRDS(EST_BUILD_RDS)

run_plan <- NULL
if (!is.null(est_build$run_plan)) {
  run_plan <- est_build$run_plan
} else if (!is.null(est_build$models)) {
  run_plan <- est_build$models
} else if (!is.null(est_build$model_list)) {
  run_plan <- est_build$model_list
} else if (!is.null(est_build$inp_files)) {
  run_plan <- data.frame(
    model_tag = tools::file_path_sans_ext(basename(est_build$inp_files)),
    inp_file  = est_build$inp_files,
    stringsAsFactors = FALSE
  )
}

if (is.null(run_plan) || nrow(run_plan) == 0) {
  stop("No model run plan found in estimation_build.rds. Check 03a output structure.")
}

if (!is.data.frame(run_plan)) {
  run_plan <- as.data.frame(run_plan, stringsAsFactors = FALSE)
}

if (!"model_tag" %in% names(run_plan)) {
  stop("run_plan must contain 'model_tag'.", call. = FALSE)
}
if (!"inp_file" %in% names(run_plan)) {
  stop("run_plan must contain 'inp_file'.", call. = FALSE)
}
if (!"k" %in% names(run_plan)) {
  run_plan$k <- suppressWarnings(as.integer(sub(".*_k([0-9]+)_.*", "\\1", run_plan$model_tag)))
}
if (!"model_structure" %in% names(run_plan)) {
  run_plan$model_structure <- sub(
    paste0("^", tolower(DATASET_ID), "_", tolower(ANALYSIS_ID), "_(model[0-9]+)_k[0-9]+_.*$"),
    "\\1",
    run_plan$model_tag
  )
}

if (!exists("CFG")) stop("Missing object: CFG")

MPLUS_EXE    <- resolve_mplus_exe(CFG, must_exist = FALSE)
AUTO_RUN_BAT <- AUTO_RUN_BAT %||% FALSE

if (!nzchar(MPLUS_EXE)) {
  stop("Mplus executable not found. Set CFG$mplus_exe or add Mplus to PATH.")
}

# ------------------------------------------------------------
# 2. run models
# ------------------------------------------------------------
results_list <- vector("list", nrow(run_plan))

for (i in seq_len(nrow(run_plan))) {
  row_i <- run_plan[i, , drop = FALSE]

  model_tag <- as.character(row_i$model_tag %||% paste0("model_", i))
  inp_file  <- as.character(row_i$inp_file %||% file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")))
  k_i       <- suppressWarnings(as.integer(row_i$k[1] %||% NA_integer_))
  model_structure_i <- as.character(row_i$model_structure[1] %||% NA_character_)

  log_info("--------------------------------------------------------")
  log_info("[MODEL] ", model_tag)
  log_info("[K]     ", ifelse(is.na(k_i), "NA", k_i))
  log_info("[STRUCT] ", ifelse(is.na(model_structure_i), "NA", model_structure_i))
  log_info("[INP]   ", inp_file)

  if (!file.exists(inp_file)) {
    log_warn("Input file not found: ", inp_file)

    results_list[[i]] <- data.frame(
      k            = k_i,
      model_structure = model_structure_i,
      model_tag    = model_tag,
      inp_file     = inp_file,
      out_file     = NA_character_,
      cprob_file   = NA_character_,
      log_file     = NA_character_,
      status       = "failed",
      parse_ok     = FALSE,
      exec_ok      = FALSE,
      out_fresh    = FALSE,
      cprob_fresh  = FALSE,
      out_size     = NA_real_,
      out_mtime    = NA_real_,
      out_md5      = NA_character_,
      cprob_size   = NA_real_,
      cprob_mtime  = NA_real_,
      cprob_md5    = NA_character_,
      output_present = FALSE,
      terminated_normally = FALSE,
      converged    = FALSE,
      best_ll_replicated = NA,
      loglik_replicated = NA,
      replication_ok = FALSE,
      local_maxima_warning = FALSE,
      admissible   = FALSE,
      run_quality_ok = FALSE,
      failure_reasons = "input_missing",
      error_text   = "Input file missing",
      warning_text = NA_character_,
      stringsAsFactors = FALSE
    )
    next
  }

  expected_out_candidates <- c(
    as.character(row_i$out_file %||% NA_character_),
    file.path(DIR_MPLUS_OUT, paste0(model_tag, ".out")),
    sub("\\.inp$", ".out", inp_file, ignore.case = TRUE),
    file.path(dirname(inp_file), paste0(model_tag, ".out"))
  )
  expected_cprob_candidates <- c(
    as.character(row_i$cprob_file %||% NA_character_),
    if (!is.na(model_structure_i) && !is.na(k_i)) file.path(DIR_MPLUS_SAVEDATA, paste0(model_structure_i, "_cprob_k", k_i, ".dat")) else NA_character_,
    if (!is.na(k_i)) file.path(DIR_MPLUS_SAVEDATA, paste0("cprob_k", k_i, ".dat")) else NA_character_,
    if (!is.na(model_structure_i) && !is.na(k_i)) file.path(dirname(inp_file), paste0(model_structure_i, "_cprob_k", k_i, ".dat")) else NA_character_,
    if (!is.na(k_i)) file.path(dirname(inp_file), paste0("cprob_k", k_i, ".dat")) else NA_character_,
    if (!is.na(model_structure_i) && !is.na(k_i)) file.path(DIR_MPLUS_OUT, paste0(model_structure_i, "_cprob_k", k_i, ".dat")) else NA_character_,
    if (!is.na(k_i)) file.path(DIR_MPLUS_OUT, paste0("cprob_k", k_i, ".dat")) else NA_character_
  )
  out_before <- snapshot_file_states(expected_out_candidates)
  cprob_before <- snapshot_file_states(expected_cprob_candidates)
  run_started_at <- Sys.time()

  exec_res <- tryCatch(
    run_mplus_model(
      inp_file  = inp_file,
      mplus_exe = MPLUS_EXE,
      workdir   = dirname(inp_file),
      wait      = TRUE,
      intern    = FALSE,
      quiet     = TRUE
    ),
    error = function(e) {
      list(
        ok = FALSE,
        status = NA_integer_,
        output = NULL,
        error = conditionMessage(e),
        log_file = NA_character_,
        out_file = sub("\\.inp$", ".out", inp_file, ignore.case = TRUE)
      )
    }
  )

  out_candidates <- c(
    as.character(exec_res$out_file %||% NA_character_),
    expected_out_candidates
  )
  out_file <- find_existing_first(out_candidates)

  cprob_candidates <- expected_cprob_candidates
  cprob_file <- find_existing_first(cprob_candidates)
  cprob_file <- wait_for_nonempty_file(cprob_file, max_tries = 8L, sleep_sec = 1)
  out_fresh <- file_was_fresh(out_file, out_before, run_started_at)
  cprob_fresh <- file_was_fresh(cprob_file, cprob_before, run_started_at)
  out_signature <- mixture_file_signature(out_file)
  cprob_signature <- mixture_file_signature(cprob_file)

  det <- detect_mplus_status(
    out_file,
    k = k_i,
    exec_ok = isTRUE(exec_res$ok),
    out_fresh = out_fresh
  )

  if (!is.na(out_file)) {
    log_info("[OUT]   ", out_file)
  } else {
    log_warn("[OUT]   not found")
  }

  if (!is.na(cprob_file)) {
    log_info("[CPROB] ", cprob_file)
  } else {
    log_warn("[CPROB] not found")
  }

  if (!is.null(exec_res$log_file) && !is.na(exec_res$log_file)) {
    log_info("[LOG]   ", exec_res$log_file)
  }

  if (!identical(det$status, "ok")) {
    log_warn("Mplus failed for ", model_tag)
    if (!is.na(det$error_text)) log_warn(det$error_text)
  } else {
    log_info("Mplus completed successfully: ", model_tag)
  }

  results_list[[i]] <- data.frame(
    k            = k_i,
    model_structure = model_structure_i,
    model_tag    = model_tag,
    inp_file     = inp_file,
    out_file     = out_file,
    cprob_file   = cprob_file,
    log_file     = exec_res$log_file %||% NA_character_,
    status       = det$status,
    parse_ok     = det$parse_ok,
    exec_ok      = det$exec_ok,
    out_fresh    = det$out_fresh,
    cprob_fresh  = cprob_fresh,
    out_size     = out_signature$size,
    out_mtime    = out_signature$mtime,
    out_md5      = out_signature$md5,
    cprob_size   = cprob_signature$size,
    cprob_mtime  = cprob_signature$mtime,
    cprob_md5    = cprob_signature$md5,
    output_present = det$output_present,
    terminated_normally = det$terminated_normally,
    converged    = det$converged,
    best_ll_replicated = det$best_ll_replicated,
    loglik_replicated = det$loglik_replicated,
    replication_ok = det$replication_ok,
    local_maxima_warning = det$local_maxima_warning,
    admissible   = det$admissible,
    run_quality_ok = det$run_quality_ok,
    failure_reasons = det$failure_reasons,
    error_text   = det$error_text,
    warning_text = det$warning_text,
    stringsAsFactors = FALSE
  )
}

run_results <- dplyr::bind_rows(results_list)

# ------------------------------------------------------------
# 3. build ESTIMATION_REGISTRY
# ------------------------------------------------------------
if (anyDuplicated(as.character(ESTIMATION_REGISTRY_BUILD$model_tag))) {
  stop("The estimation-build registry contains duplicate model_tag values.", call. = FALSE)
}
registry_match <- match(as.character(run_results$model_tag), as.character(ESTIMATION_REGISTRY_BUILD$model_tag))
if (anyNA(registry_match)) {
  stop("Run results could not be matched exactly to the estimation-build registry.", call. = FALSE)
}
ESTIMATION_REGISTRY <- ESTIMATION_REGISTRY_BUILD[registry_match, , drop = FALSE]
for (nm in names(run_results)) {
  ESTIMATION_REGISTRY[[nm]] <- run_results[[nm]]
}

if (is.data.frame(run_plan) && nrow(run_plan) > 0) {
  idx_match <- match(ESTIMATION_REGISTRY$model_tag, run_plan$model_tag)

  if ("k" %in% names(run_plan)) {
    ESTIMATION_REGISTRY$k <- run_plan$k[idx_match]
  }
  if ("model_structure" %in% names(run_plan)) {
    ESTIMATION_REGISTRY$model_structure <- as.character(run_plan$model_structure[idx_match])
  }
  if ("inp_file" %in% names(run_plan)) {
    ESTIMATION_REGISTRY$inp_file <- run_plan$inp_file[idx_match]
  }
  if ("out_file" %in% names(run_plan)) {
    tmp_out <- run_plan$out_file[idx_match]
    keep_old <- !is.na(ESTIMATION_REGISTRY$out_file) & nzchar(ESTIMATION_REGISTRY$out_file)
    ESTIMATION_REGISTRY$out_file[!keep_old] <- tmp_out[!keep_old]
  }
  if ("cprob_file" %in% names(run_plan)) {
    tmp_cp <- run_plan$cprob_file[idx_match]
    keep_old <- !is.na(ESTIMATION_REGISTRY$cprob_file) & nzchar(ESTIMATION_REGISTRY$cprob_file)
    ESTIMATION_REGISTRY$cprob_file[!keep_old] <- tmp_cp[!keep_old]
  }
  for (nm in c("starts", "stiterations", "processors", "estimator", "lratio_starts")) {
    if (nm %in% names(run_plan)) ESTIMATION_REGISTRY[[nm]] <- run_plan[[nm]][idx_match]
  }
}

ESTIMATION_REGISTRY$status_ok <- ESTIMATION_REGISTRY$status %in% "ok"
ESTIMATION_REGISTRY$out_exists <- !is.na(ESTIMATION_REGISTRY$out_file) &
  file.exists(ESTIMATION_REGISTRY$out_file)

ESTIMATION_REGISTRY$cprob_exists <- !is.na(ESTIMATION_REGISTRY$cprob_file) &
  file.exists(ESTIMATION_REGISTRY$cprob_file) &
  (file.info(ESTIMATION_REGISTRY$cprob_file)$size > 0)
ESTIMATION_REGISTRY$cprob_ready <- ESTIMATION_REGISTRY$status_ok &
  ESTIMATION_REGISTRY$cprob_exists &
  !is.na(ESTIMATION_REGISTRY$cprob_fresh) &
  ESTIMATION_REGISTRY$cprob_fresh

# ------------------------------------------------------------
# 4. save
# ------------------------------------------------------------
log_info("Saving estimation-run outputs ...")

saveRDS(
  list(
    run_plan            = run_plan,
    run_results         = run_results,
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY,
    elapsed_sec         = as.numeric(difftime(Sys.time(), T0_03B, units = "secs"))
  ),
  file = file.path(DIR_RDS, "estimation_run.rds")
)

save_named_rds_list(
  list(
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY
  ),
  dir_rds = DIR_RDS
)

utils::write.csv(
  run_results,
  file = file.path(DIR_TABLES, "estimation_run_results.csv"),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  ESTIMATION_REGISTRY,
  file = file.path(DIR_TABLES, "estimation_registry.csv"),
  row.names = FALSE,
  na = ""
)

log_info("03b_estimation_run_mplus.R completed.")
log_info("n(models total)     = ", nrow(run_results))
log_info("n(models ok)        = ", sum(run_results$status == "ok", na.rm = TRUE))
log_info("n(models failed)    = ", sum(run_results$status == "failed", na.rm = TRUE))
log_info("n(cprob created)    = ", sum(!is.na(run_results$cprob_file)))
log_info("n(out created)      = ", sum(!is.na(run_results$out_file)))
log_info(
  "elapsed             = ",
  round(as.numeric(difftime(Sys.time(), T0_03B, units = "secs")), 2),
  " sec"
)
