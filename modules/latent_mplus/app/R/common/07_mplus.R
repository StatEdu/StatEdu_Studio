# ============================================================
# 07_mplus.R
# Mplus helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) Mplus 실행 파일 경로 해석
# 2) model tag 기준 inp/out/savedata 경로 생성
# 3) Mplus 실행 / bat 생성
# 4) output / savedata 존재 여부 점검
# 5) Mplus 관련 공통 helper 제공
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.norm_path_m <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- gsub("/+", "/", x)
  x
}

.ensure_dir_m <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.file_exists_m <- function(path) {
  !is.na(path) && nzchar(path) && file.exists(path)
}

.safe_info_m <- function(...) {
  if (exists("log_info")) log_info(...) else cat(paste0(..., "\n"))
}

.safe_warn_m <- function(...) {
  if (exists("log_warn")) log_warn(...) else cat(paste0("[WARN] ", ..., "\n"))
}

.safe_ok_m <- function(...) {
  if (exists("log_ok")) log_ok(...) else cat(paste0("[OK] ", ..., "\n"))
}

.safe_stop_m <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.first_existing_m <- function(paths) {
  paths <- as.character(paths)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# ------------------------------------------------------------
# 1. resolve Mplus executable
# ------------------------------------------------------------
resolve_mplus_exe <- function(cfg = NULL, default = NULL, must_exist = FALSE) {
  cand <- c(
    cfg$mplus$exe %||% NULL,
    cfg$analysis$mplus_exe %||% NULL,
    default %||% NULL,
    Sys.getenv("MPLUS_EXE", unset = NA_character_),
    "C:/Program Files/Mplus/Mplus.exe",
    "C:/Program Files (x86)/Mplus/Mplus.exe"
  )

  cand <- as.character(cand)
  cand <- cand[!is.na(cand) & nzchar(cand)]
  cand <- unique(.norm_path_m(cand))

  exe <- .first_existing_m(cand)

  if (is.na(exe)) {
    if (isTRUE(must_exist)) {
      .safe_stop_m("Mplus executable not found.")
    } else {
      exe <- .norm_path_m(cand[1] %||% "C:/Program Files/Mplus/Mplus.exe")
    }
  }

  exe
}

# ------------------------------------------------------------
# 2. path builders
# ------------------------------------------------------------
make_model_tag <- function(dataset_id, k, mixture_type, prefix = "main") {
  paste0(
    tolower(dataset_id),
    "_",
    tolower(prefix),
    "_k", as.integer(k),
    "_",
    tolower(mixture_type)
  )
}

make_mplus_paths <- function(model_tag,
                             dir_mplus_inp = DIR_MPLUS_INP,
                             dir_mplus_out = DIR_MPLUS_OUT,
                             dir_mplus_savedata = DIR_MPLUS_SAVEDATA,
                             dir_mplus_data = DIR_MPLUS_DATA) {
  list(
    inp_file = file.path(dir_mplus_inp, paste0(model_tag, ".inp")),
    out_file = file.path(dir_mplus_out, paste0(model_tag, ".out")),
    savedata_file = file.path(dir_mplus_savedata, paste0(model_tag, "_cprob.dat")),
    fscores_file = file.path(dir_mplus_savedata, paste0(model_tag, "_fscores.dat")),
    data_file = file.path(dir_mplus_data, paste0(model_tag, ".dat"))
  )
}

# ------------------------------------------------------------
# 3. write .dat helper
# ------------------------------------------------------------
write_mplus_data <- function(data, path, missing_code = -9999, sep = "\t") {
  if (!is.data.frame(data)) .safe_stop_m("write_mplus_data: data must be a data.frame")

  d <- data

  for (nm in names(d)) {
    x <- d[[nm]]

    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) x <- as.integer(x)

    if (is.character(x)) {
      suppressWarnings(x_num <- as.numeric(x))
      x <- x_num
    }

    if (!is.numeric(x)) {
      suppressWarnings(x <- as.numeric(x))
    }

    x[is.na(x)] <- missing_code
    d[[nm]] <- x
  }

  .ensure_dir_m(dirname(path))
  utils::write.table(
    d,
    file = path,
    quote = FALSE,
    sep = sep,
    row.names = FALSE,
    col.names = FALSE,
    na = as.character(missing_code)
  )

  invisible(path)
}

write_mplus_header <- function(var_names, path, sep = "\t") {
  .ensure_dir_m(dirname(path))
  cat(paste(var_names, collapse = sep), file = path)
  invisible(path)
}

# ------------------------------------------------------------
# 4. build batch file
# ------------------------------------------------------------
build_mplus_bat <- function(inp_files,
                            bat_file,
                            mplus_exe,
                            pause_at_end = FALSE) {
  inp_files <- as.character(inp_files)
  inp_files <- inp_files[file.exists(inp_files)]

  .ensure_dir_m(dirname(bat_file))

  lines <- c("@echo off", "setlocal")

  exe_win <- gsub("/", "\\\\", .norm_path_m(mplus_exe), fixed = TRUE)

  for (inp in inp_files) {
    inp_win <- gsub("/", "\\\\", .norm_path_m(inp), fixed = TRUE)
    lines <- c(lines, paste0("\"", exe_win, "\" \"", inp_win, "\""))
  }

  if (isTRUE(pause_at_end)) lines <- c(lines, "pause")
  lines <- c(lines, "endlocal")

  writeLines(lines, con = bat_file, useBytes = TRUE)
  invisible(bat_file)
}

# ------------------------------------------------------------
# 5. run Mplus
# ------------------------------------------------------------
run_mplus_model <- function(inp_file,
                            mplus_exe,
                            workdir = dirname(inp_file),
                            wait    = TRUE,
                            intern  = FALSE,
                            quiet = TRUE) {
  if (is.null(inp_file) || !file.exists(inp_file)) {
    return(list(
      ok       = FALSE,
      status   = NA_integer_,
      output   = NULL,
      error    = "inp_file not found",
      log_file = NA_character_,
      out_file = if (!is.null(inp_file)) sub("\\.inp$", ".out", inp_file, ignore.case = TRUE) else NA_character_
    ))
  }

  if (is.null(mplus_exe) || !file.exists(mplus_exe)) {
    return(list(
      ok       = FALSE,
      status   = NA_integer_,
      output   = NULL,
      error    = "mplus executable not found",
      log_file = NA_character_,
      out_file = sub("\\.inp$", ".out", inp_file, ignore.case = TRUE)
    ))
  }

  inp_file <- normalizePath(inp_file, winslash = "/", mustWork = FALSE)
  workdir  <- normalizePath(workdir,  winslash = "/", mustWork = FALSE)

  inp_base <- basename(inp_file)
  out_file <- sub("\\.inp$", ".out", inp_file, ignore.case = TRUE)
  log_file <- sub("\\.inp$", ".log", inp_file, ignore.case = TRUE)

  old_wd   <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(workdir)

  status   <- NA_integer_
  err_msg  <- NULL


  log_file <- normalizePath(log_file, winslash = "/", mustWork = FALSE)

  if (isTRUE(quiet)) {
    status <- tryCatch(
      system2(
        command = mplus_exe,
        args = shQuote(inp_base),
        wait = wait,
        stdout = log_file,
        stderr = log_file
      ),
      error = function(e) {
        err_msg <<- conditionMessage(e)
        NA_integer_
      }
    )
  } else {
    status <- tryCatch(
      system2(
        command = mplus_exe,
        args = shQuote(inp_base),
        wait = wait,
        stdout = "",
        stderr = ""
      ),
      error = function(e) {
        err_msg <<- conditionMessage(e)
        NA_integer_
      }
    )
  }

  list(
    ok = isTRUE(!is.na(status) && status == 0),
    status = status,
    output = NULL,
    error = err_msg,
    log_file = log_file,
    out_file = out_file
  )
}

run_mplus_batch <- function(bat_file, wait = TRUE, intern = FALSE) {
  if (!.file_exists_m(bat_file)) .safe_stop_m("Batch file not found: ", bat_file)

  .safe_info_m("Running batch file: ", bat_file)

  out <- tryCatch(
    shell(bat_file, intern = intern, wait = wait),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    return(list(
      ok = FALSE,
      status = NA_integer_,
      output = conditionMessage(out),
      error = conditionMessage(out)
    ))
  }

  list(
    ok = TRUE,
    status = 0L,
    output = out,
    error = NULL
  )
}

# ------------------------------------------------------------
# 6. output checks
# ------------------------------------------------------------
mplus_output_exists <- function(model_info) {
  out_file <- model_info$out_file %||% NA_character_
  .file_exists_m(out_file)
}

mplus_savedata_exists <- function(model_info) {
  savedata_file <- model_info$savedata_file %||% NA_character_
  .file_exists_m(savedata_file)
}

mplus_inp_exists <- function(model_info) {
  inp_file <- model_info$inp_file %||% NA_character_
  .file_exists_m(inp_file)
}

collect_mplus_file_status <- function(model_info) {
  data.frame(
    model_tag = model_info$model_tag %||% NA_character_,
    k = model_info$k %||% NA_integer_,
    inp_file = model_info$inp_file %||% NA_character_,
    inp_exists = mplus_inp_exists(model_info),
    out_file = model_info$out_file %||% NA_character_,
    out_exists = mplus_output_exists(model_info),
    savedata_file = model_info$savedata_file %||% NA_character_,
    savedata_exists = mplus_savedata_exists(model_info),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 7. savedata reader
# ------------------------------------------------------------
read_mplus_savedata_free <- function(path, header = FALSE) {
  if (!.file_exists_m(path)) .safe_stop_m("Savedata file not found: ", path)

  d <- tryCatch(
    utils::read.table(
      file = path,
      header = header,
      sep = "",
      fill = TRUE,
      stringsAsFactors = FALSE,
      na.strings = c(".", "*", "NA", "")
    ),
    error = function(e) NULL
  )

  if (is.null(d)) {
    d <- tryCatch(
      utils::read.table(
        file = path,
        header = header,
        sep = "",
        fill = TRUE,
        quote = "",
        comment.char = "",
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
  }

  if (is.null(d)) .safe_stop_m("Failed to read Mplus savedata: ", path)

  if (!header) names(d) <- paste0("V", seq_len(ncol(d)))
  d
}

# ------------------------------------------------------------
# 8. inspect output text
# ------------------------------------------------------------
read_mplus_out_lines <- function(path) {
  if (!.file_exists_m(path)) return(character(0))

  out <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
  if (length(out) == 0) {
    out <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  }
  enc2utf8(out)
}

detect_mplus_normal_termination <- function(lines) {
  any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", lines, ignore.case = TRUE, perl = TRUE))
}

detect_mplus_warnings <- function(lines) {
  idx <- grep("WARNING|NOT TERMINATE NORMALLY|NO CONVERGENCE|UNRELIABLE|PROBLEM",
              lines, ignore.case = TRUE, perl = TRUE)
  if (length(idx) == 0) return(character(0))
  unique(trimws(lines[idx]))
}

summarize_mplus_run <- function(out_file, savedata_file = NULL) {
  lines <- read_mplus_out_lines(out_file)

  list(
    out_exists = .file_exists_m(out_file),
    savedata_exists = if (is.null(savedata_file)) NA else .file_exists_m(savedata_file),
    terminated_normally = detect_mplus_normal_termination(lines),
    warnings = detect_mplus_warnings(lines),
    n_lines = length(lines)
  )
}

# ------------------------------------------------------------
# 9. registry helpers
# ------------------------------------------------------------
make_estimation_registry_row <- function(k,
                                         model_tag,
                                         mixture_type,
                                         inp_file,
                                         out_file,
                                         savedata_file,
                                         data_file = NULL,
                                         data_header = NULL,
                                         id_var = NULL,
                                         usevariables = character(0),
                                         all_names = character(0),
                                         categorical = character(0),
                                         continuous = character(0),
                                         survey_case = NULL,
                                         weight_var = NULL,
                                         strata_var = NULL,
                                         cluster_var = NULL,
                                         missing_code = -9999,
                                         starts = NA_integer_,
                                         stiterations = NA_integer_,
                                         processors = NA_integer_,
                                         estimator = "MLR",
                                         lratio_starts = NULL) {
  list(
    k = as.integer(k),
    model_tag = as.character(model_tag),
    mixture_type = as.character(mixture_type),
    inp_file = inp_file,
    out_file = out_file,
    savedata_file = savedata_file,
    data_file = data_file,
    data_header = data_header,
    id_var = id_var,
    usevariables = as.character(usevariables),
    all_names = as.character(all_names),
    categorical = as.character(categorical),
    continuous = as.character(continuous),
    survey_case = survey_case,
    weight_var = weight_var,
    strata_var = strata_var,
    cluster_var = cluster_var,
    missing_code = missing_code,
    starts = starts,
    stiterations = stiterations,
    processors = processors,
    estimator = estimator,
    lratio_starts = lratio_starts
  )
}

registry_to_status_df <- function(registry) {
  if (is.null(registry) || length(registry) == 0) return(data.frame())
  out <- do.call(rbind, lapply(registry, collect_mplus_file_status))
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 10. cleanup stray root-level Mplus artifacts
# ------------------------------------------------------------
relocate_project_root_mplus_artifacts <- function(project_root,
                                                  dir_mplus = file.path(project_root, "mplus_tmp"),
                                                  target_dir = file.path(dir_mplus, "root_artifacts")) {
  project_root <- .norm_path_m(project_root)
  dir_mplus <- .norm_path_m(dir_mplus)
  target_dir <- .norm_path_m(target_dir)

  if (is.na(project_root) || !dir.exists(project_root)) {
    return(data.frame())
  }

  root_files <- list.files(
    project_root,
    pattern = "\\.(out|log|inp|dat)$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  if (length(root_files) == 0) return(data.frame())

  is_mplus_name <- grepl(
    "(cross_sectional_mixture|state_transition|latent_transition|~MT|~MTR|~TM\\.|~TS\\.|_bch_)",
    basename(root_files),
    ignore.case = TRUE
  )
  root_files <- root_files[is_mplus_name]
  if (length(root_files) == 0) return(data.frame())

  duplicated_in_mplus <- vapply(root_files, function(fp) {
    hits <- list.files(
      dir_mplus,
      pattern = paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", basename(fp)), "$"),
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    any(.norm_path_m(hits) != .norm_path_m(fp))
  }, logical(1))

  move_files <- root_files[duplicated_in_mplus]
  if (length(move_files) == 0) return(data.frame())

  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  moved <- lapply(move_files, function(from) {
    to <- file.path(target_dir, basename(from))
    if (file.exists(to)) {
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      to <- file.path(
        target_dir,
        paste0(tools::file_path_sans_ext(basename(from)), "_", stamp, ".", tools::file_ext(from))
      )
    }
    ok <- tryCatch(file.rename(from, to), error = function(e) FALSE)
    if (!isTRUE(ok) && file.exists(from)) {
      ok_copy <- tryCatch(file.copy(from, to, overwrite = FALSE), error = function(e) FALSE)
      ok_rm <- FALSE
      if (isTRUE(ok_copy)) {
        ok_rm <- tryCatch(file.remove(from), error = function(e) FALSE)
      }
      ok <- isTRUE(ok_copy) && isTRUE(ok_rm)
    }
    data.frame(
      from = from,
      to = if (isTRUE(ok)) to else NA_character_,
      moved = isTRUE(ok),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, moved)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 11. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("07_mplus.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Mplus helpers registered\n")
cat("============================================================\n\n")
