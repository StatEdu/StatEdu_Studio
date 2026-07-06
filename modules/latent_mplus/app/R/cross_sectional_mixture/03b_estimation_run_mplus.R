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

detect_mplus_status <- function(out_file) {
  txt <- safe_read_lines(out_file)

  if (length(txt) == 0) {
    return(list(
      status       = "failed",
      parse_ok     = FALSE,
      error_text   = "Output file missing or unreadable",
      warning_text = NA_character_,
      tail_text    = NA_character_
    ))
  }

  up <- toupper(txt)
  tail_text <- paste(tail(txt, 30), collapse = "\n")

  has_fatal <- any(grepl("^\\s*\\*\\*\\*\\s+ERROR", up))
  has_error_word <- any(grepl("AN ERROR HAS OCCURRED", up, fixed = TRUE))
  completed <- any(grepl("ENDING TIME:", up, fixed = TRUE)) ||
    any(grepl("ELAPSED TIME:", up, fixed = TRUE))
  savedata_written <- any(grepl("SAVE FILE FORMAT", up, fixed = TRUE)) ||
    any(grepl("ORDER AND FORMAT OF VARIABLES", up, fixed = TRUE))

  warn_idx <- grep("^\\s*\\*\\*\\*\\s+WARNING", up)

  error_text <- NA_character_
  if (has_fatal || has_error_word) {
    err_idx <- grep("^\\s*\\*\\*\\*\\s+ERROR", up)
    if (length(err_idx) > 0) {
      rng <- seq.int(err_idx[1], min(length(txt), err_idx[1] + 8))
      error_text <- paste(txt[rng], collapse = "\n")
    } else {
      error_text <- tail_text
    }
  }

  warning_text <- NA_character_
  if (length(warn_idx) > 0) {
    rng <- seq.int(warn_idx[1], min(length(txt), warn_idx[1] + 6))
    warning_text <- paste(txt[rng], collapse = "\n")
  }

  status <- if (!has_fatal && !has_error_word && (completed || savedata_written)) "ok" else "failed"

  list(
    status       = status,
    parse_ok     = identical(status, "ok"),
    error_text   = error_text,
    warning_text = warning_text,
    tail_text    = tail_text
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

if (!exists("DIR_OUTPUT_RDS") || is.null(DIR_OUTPUT_RDS) || !nzchar(DIR_OUTPUT_RDS)) {
  DIR_OUTPUT_RDS <- file.path(DIR_OUTPUT, "rds")
}
dir.create(DIR_OUTPUT_RDS, recursive = TRUE, showWarnings = FALSE)

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

EST_BUILD_RDS <- file.path(DIR_OUTPUT_RDS, "estimation_build.rds")
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
      error_text   = "Input file missing",
      warning_text = NA_character_,
      stringsAsFactors = FALSE
    )
    next
  }

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
    as.character(row_i$out_file %||% NA_character_),
    file.path(DIR_MPLUS_OUT, paste0(model_tag, ".out")),
    sub("\\.inp$", ".out", inp_file),
    file.path(dirname(inp_file), paste0(model_tag, ".out"))
  )
  out_file <- find_existing_first(out_candidates)

  cprob_candidates <- c(
    as.character(row_i$cprob_file %||% NA_character_),
    if (!is.na(model_structure_i) && !is.na(k_i)) {
      file.path(DIR_MPLUS_SAVEDATA, paste0(model_structure_i, "_cprob_k", k_i, ".dat"))
    } else {
      NA_character_
    },
    if (!is.na(k_i)) {
      file.path(DIR_MPLUS_SAVEDATA, paste0("cprob_k", k_i, ".dat"))
    } else {
      NA_character_
    },
    if (!is.na(model_structure_i) && !is.na(k_i)) {
      file.path(dirname(inp_file), paste0(model_structure_i, "_cprob_k", k_i, ".dat"))
    } else {
      NA_character_
    },
    if (!is.na(k_i)) {
      file.path(dirname(inp_file), paste0("cprob_k", k_i, ".dat"))
    } else {
      NA_character_
    },
    if (!is.na(model_structure_i) && !is.na(k_i)) {
      file.path(DIR_MPLUS_OUT, paste0(model_structure_i, "_cprob_k", k_i, ".dat"))
    } else {
      NA_character_
    },
    if (!is.na(k_i)) {
      file.path(DIR_MPLUS_OUT, paste0("cprob_k", k_i, ".dat"))
    } else {
      NA_character_
    }
  )
  cprob_file <- find_existing_first(cprob_candidates)
  cprob_file <- wait_for_nonempty_file(cprob_file, max_tries = 8L, sleep_sec = 1)

  det <- detect_mplus_status(out_file)

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
    error_text   = det$error_text,
    warning_text = det$warning_text,
    stringsAsFactors = FALSE
  )
}

run_results <- dplyr::bind_rows(results_list)

# ------------------------------------------------------------
# 3. build ESTIMATION_REGISTRY
# ------------------------------------------------------------
ESTIMATION_REGISTRY <- run_results

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
}

ESTIMATION_REGISTRY$status_ok <- ESTIMATION_REGISTRY$status %in% "ok"
ESTIMATION_REGISTRY$out_exists <- !is.na(ESTIMATION_REGISTRY$out_file) &
  file.exists(ESTIMATION_REGISTRY$out_file)

ESTIMATION_REGISTRY$cprob_exists <- !is.na(ESTIMATION_REGISTRY$cprob_file) &
  file.exists(ESTIMATION_REGISTRY$cprob_file) &
  (file.info(ESTIMATION_REGISTRY$cprob_file)$size > 0)

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
  file = file.path(DIR_OUTPUT_RDS, "estimation_run.rds")
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
