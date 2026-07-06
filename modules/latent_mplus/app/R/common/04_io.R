# ============================================================
# 04_io.R
# I/O helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) config 파일(CFG / missing / subsets / dictionary) 읽기
# 2) raw data(csv / rds / sav) 읽기
# 3) RDS / CSV / TXT / YAML 안전 저장
# 4) 파일 존재 여부 및 optional 파일 처리 공통화
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

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

.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.file_exists2 <- function(path) {
  !is.na(path) && nzchar(path) && file.exists(path)
}

.safe_message <- function(...) {
  if (exists("log_info")) {
    log_info(...)
  } else {
    cat(paste0(..., "\n"))
  }
}

.safe_warn <- function(...) {
  if (exists("log_warn")) {
    log_warn(...)
  } else {
    cat(paste0("[WARN] ", ..., "\n"))
  }
}

.safe_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

# ------------------------------------------------------------
# 1. package availability flags
# ------------------------------------------------------------
YAML_AVAILABLE  <- .has_pkg("yaml")
READR_AVAILABLE <- .has_pkg("readr")
HAVEN_AVAILABLE <- .has_pkg("haven")

# ------------------------------------------------------------
# 2. basic file helpers
# ------------------------------------------------------------
path_exists <- function(path) {
  .file_exists2(path)
}

path_must_exist <- function(path, label = NULL) {
  if (!.file_exists2(path)) {
    .safe_stop((label %||% "Required file"), " not found: ", path)
  }
  invisible(TRUE)
}

first_existing_path <- function(paths) {
  paths <- as.character(paths)
  hit <- paths[.file_exists2(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

list_existing_paths <- function(paths) {
  paths <- as.character(paths)
  paths[.file_exists2(paths)]
}

# ------------------------------------------------------------
# 3. YAML readers
# ------------------------------------------------------------
read_yaml_safe <- function(path, default = NULL, required = FALSE, empty_as_default = TRUE) {
  if (!.file_exists2(path)) {
    if (required) .safe_stop("YAML file not found: ", path)
    return(default)
  }

  file_bytes <- tryCatch(file.info(path)$size, error = function(e) NA_real_)
  if (isTRUE(empty_as_default) && !is.na(file_bytes) && file_bytes == 0) {
    return(default)
  }

  if (isTRUE(empty_as_default)) {
    lines <- tryCatch(
      readLines(path, warn = FALSE, encoding = "UTF-8"),
      error = function(e) {
        tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0))
      }
    )
    if (length(lines) == 0L) return(default)

    # Treat BOM-only or whitespace-only YAML files as intentionally empty.
    lines[1] <- sub("^\ufeff", "", lines[1])
    if (!any(nzchar(trimws(lines)))) return(default)
  }

  if (!YAML_AVAILABLE) {
    if (required) .safe_stop("Package 'yaml' is required to read: ", path)
    .safe_warn("yaml package not available. Returning default for: ", basename(path))
    return(default)
  }

  out <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      if (required) .safe_stop("Failed to read YAML: ", path, " / ", conditionMessage(e))
      .safe_warn("Failed to read YAML: ", basename(path), " / ", conditionMessage(e))
      default
    }
  )

  if (is.null(out) && isTRUE(empty_as_default)) return(default)
  out
}

write_yaml_safe <- function(x, path) {
  if (!YAML_AVAILABLE) .safe_stop("Package 'yaml' is required to write YAML.")
  .ensure_dir(dirname(path))
  yaml::write_yaml(x, file = path)
  invisible(path)
}

read_cfg <- function(path = PATH_CFG, required = TRUE) {
  read_yaml_safe(path, default = list(), required = required)
}

read_missing_yml <- function(path = PATH_MISSING, required = FALSE) {
  read_yaml_safe(path, default = list(), required = required)
}

read_subsets_yml <- function(path = PATH_SUBSETS, required = FALSE) {
  read_yaml_safe(path, default = list(), required = required)
}

# ------------------------------------------------------------
# 4. table readers
# ------------------------------------------------------------
read_csv_safe <- function(path, stringsAsFactors = FALSE, ...) {
  if (!.file_exists2(path)) .safe_stop("CSV file not found: ", path)

  if (READR_AVAILABLE) {
    out <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...),
      error = function(e) NULL
    )
    if (!is.null(out)) return(as.data.frame(out, stringsAsFactors = stringsAsFactors))
  }

  utils::read.csv(path, stringsAsFactors = stringsAsFactors, ...)
}

write_csv_safe <- function(x, path, row.names = FALSE, na = "") {
  .ensure_dir(dirname(path))
  utils::write.csv(x, file = path, row.names = row.names, na = na)
  invisible(path)
}

read_dictionary_csv <- function(path = PATH_DICT, required = TRUE) {
  if (!.file_exists2(path)) {
    if (required) .safe_stop("Dictionary file not found: ", path)
    return(data.frame())
  }

  d <- read_csv_safe(path, stringsAsFactors = FALSE)
  if (!is.data.frame(d)) d <- as.data.frame(d, stringsAsFactors = FALSE)
  if (nrow(d) == 0) return(d)

  names(d) <- if (exists("make_clean_names")) make_clean_names(names(d)) else make.names(names(d), unique = TRUE)

  # 최소 컬럼 보강
  if (!"var_name" %in% names(d)) {
    cand <- intersect(c("variable", "var", "name"), names(d))
    if (length(cand) > 0) names(d)[match(cand[1], names(d))] <- "var_name"
  }

  if (!"var_label" %in% names(d)) {
    if ("label_ko" %in% names(d)) {
      d$var_label <- d$label_ko
    } else if ("label_en" %in% names(d)) {
      d$var_label <- d$label_en
    } else {
      d$var_label <- d$var_name %||% NA_character_
    }
  }

  d
}

# ------------------------------------------------------------
# 5. raw data readers
# ------------------------------------------------------------
guess_data_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv")) return("csv")
  if (ext %in% c("rds")) return("rds")
  if (ext %in% c("sav")) return("sav")
  if (ext %in% c("dta")) return("dta")
  "unknown"
}

read_rds_safe <- function(path, default = NULL, required = FALSE) {
  if (!.file_exists2(path)) {
    if (required) .safe_stop("RDS file not found: ", path)
    return(default)
  }

  tryCatch(
    readRDS(path),
    error = function(e) {
      if (required) .safe_stop("Failed to read RDS: ", path, " / ", conditionMessage(e))
      .safe_warn("Failed to read RDS: ", basename(path), " / ", conditionMessage(e))
      default
    }
  )
}

save_rds_safe <- function(object, path) {
  .ensure_dir(dirname(path))
  saveRDS(object, file = path)
  invisible(path)
}

read_sav_safe <- function(path, required = FALSE) {
  if (!.file_exists2(path)) {
    if (required) .safe_stop("SAV file not found: ", path)
    return(data.frame())
  }

  if (!HAVEN_AVAILABLE) {
    .safe_stop("Package 'haven' is required to read SPSS .sav files: ", path)
  }

  out <- tryCatch(
    haven::read_sav(path),
    error = function(e) {
      if (required) .safe_stop("Failed to read SAV: ", path, " / ", conditionMessage(e))
      .safe_warn("Failed to read SAV: ", basename(path), " / ", conditionMessage(e))
      data.frame()
    }
  )

  as.data.frame(out, stringsAsFactors = FALSE)
}

read_dta_safe <- function(path, required = FALSE) {
  if (!.file_exists2(path)) {
    if (required) .safe_stop("DTA file not found: ", path)
    return(data.frame())
  }

  if (!HAVEN_AVAILABLE) {
    .safe_stop("Package 'haven' is required to read Stata .dta files: ", path)
  }

  out <- tryCatch(
    haven::read_dta(path),
    error = function(e) {
      if (required) .safe_stop("Failed to read DTA: ", path, " / ", conditionMessage(e))
      .safe_warn("Failed to read DTA: ", basename(path), " / ", conditionMessage(e))
      data.frame()
    }
  )

  as.data.frame(out, stringsAsFactors = FALSE)
}

read_data_file <- function(path = PATH_DATA, required = TRUE) {
  if (!.file_exists2(path)) {
    if (required) .safe_stop("Data file not found: ", path)
    return(data.frame())
  }

  fmt <- guess_data_format(path)

  out <- switch(
    fmt,
    csv = read_csv_safe(path, stringsAsFactors = FALSE),
    rds = read_rds_safe(path, required = required),
    sav = read_sav_safe(path, required = required),
    dta = read_dta_safe(path, required = required),
    .safe_stop("Unsupported data file format: ", path)
  )

  if (is.null(out)) out <- data.frame()
  if (is.data.frame(out)) return(out)
  as.data.frame(out, stringsAsFactors = FALSE)
}

find_data_file <- function(dir_data = DIR_DATA, dataset_id = DATASET_ID) {
  candidates <- c(
    file.path(dir_data, paste0(dataset_id, ".csv")),
    file.path(dir_data, paste0(dataset_id, ".rds")),
    file.path(dir_data, paste0(dataset_id, ".sav")),
    file.path(dir_data, paste0(dataset_id, ".dta")),
    file.path(dir_data, "data.csv"),
    file.path(dir_data, "data.rds"),
    file.path(dir_data, "data.sav"),
    file.path(dir_data, "data.dta")
  )
  first_existing_path(candidates)
}

# ------------------------------------------------------------
# 6. text writers / readers
# ------------------------------------------------------------
read_lines_safe <- function(path, default = character(0), warn = FALSE) {
  if (!.file_exists2(path)) return(default)
  tryCatch(
    readLines(path, warn = warn, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(
        readLines(path, warn = warn),
        error = function(e2) default
      )
    }
  )
}

write_lines_safe <- function(lines, path) {
  .ensure_dir(dirname(path))
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(as.character(lines)), con = con)
  invisible(path)
}

write_txt_safe <- function(text, path) {
  if (length(text) == 1L) {
    write_lines_safe(text, path)
  } else {
    write_lines_safe(as.character(text), path)
  }
  invisible(path)
}

# ------------------------------------------------------------
# 7. common pipeline loaders
# ------------------------------------------------------------
read_pipeline_inputs <- function(
    cfg_path = PATH_CFG,
    dict_path = PATH_DICT,
    missing_path = PATH_MISSING,
    subsets_path = PATH_SUBSETS,
    data_path = PATH_DATA,
    required_cfg = TRUE,
    required_dict = TRUE,
    required_data = TRUE
) {
  cfg <- read_cfg(cfg_path, required = required_cfg)
  dict <- read_dictionary_csv(dict_path, required = required_dict)
  missing <- read_missing_yml(missing_path, required = FALSE)
  subsets <- read_subsets_yml(subsets_path, required = FALSE)
  raw_data <- read_data_file(data_path, required = required_data)

  list(
    CFG = cfg,
    DICT_RAW = dict,
    MISSING_SPEC = missing,
    SUBSETS_SPEC = subsets,
    RAW_DATA = raw_data
  )
}

save_step_rds <- function(object, name, dir_rds = DIR_RDS) {
  path <- file.path(dir_rds, paste0(name, ".rds"))
  save_rds_safe(object, path)
  invisible(path)
}

load_step_rds <- function(name, dir_rds = DIR_RDS, default = NULL, required = FALSE) {
  path <- file.path(dir_rds, paste0(name, ".rds"))
  read_rds_safe(path, default = default, required = required)
}

save_named_rds_list <- function(named_list, dir_rds = DIR_RDS) {
  if (length(named_list) == 0) return(invisible(character(0)))
  out_paths <- character(0)
  for (nm in names(named_list)) {
    out_paths <- c(out_paths, save_step_rds(named_list[[nm]], nm, dir_rds = dir_rds))
  }
  invisible(out_paths)
}


# ------------------------------------------------------------
# 7-1. input fingerprint helpers
# ------------------------------------------------------------
compute_file_fingerprint <- function(path, label = NA_character_) {
  path_chr <- as.character(path)[1]
  exists_i <- .file_exists2(path_chr)

  finfo <- if (exists_i) {
    tryCatch(file.info(path_chr), error = function(e) NULL)
  } else {
    NULL
  }

  size_i <- if (!is.null(finfo) && nrow(finfo) > 0) finfo$size[1] else NA_real_
  mtime_i <- if (!is.null(finfo) && nrow(finfo) > 0) as.character(finfo$mtime[1]) else NA_character_

  fingerprint_i <- if (exists_i && exists("safe_digest_file")) {
    safe_digest_file(path_chr)
  } else {
    NA_character_
  }

  data.frame(
    label = as.character(label)[1],
    file_name = basename(path_chr),
    file_path = .norm_path(path_chr),
    exists = exists_i,
    size_bytes = size_i,
    modified_time = mtime_i,
    fingerprint = fingerprint_i,
    stringsAsFactors = FALSE
  )
}

build_input_fingerprint_manifest <- function(cfg_path = PATH_CFG,
                                             dict_path = PATH_DICT,
                                             subsets_path = PATH_SUBSETS,
                                             missing_path = PATH_MISSING,
                                             data_path = PATH_DATA) {
  items <- list(
    cfg        = cfg_path,
    dictionary = dict_path,
    subsets    = subsets_path,
    missing    = missing_path,
    raw_data   = data_path
  )

  out_list <- lapply(names(items), function(nm) {
    compute_file_fingerprint(items[[nm]], label = nm)
  })

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

write_input_fingerprint_manifest <- function(x,
                                             dir_output = DIR_OUTPUT,
                                             file_name = "input_fingerprint_manifest.csv") {
  if (!is.data.frame(x)) {
    x <- data.frame(
      label = character(0),
      file_name = character(0),
      file_path = character(0),
      exists = logical(0),
      size_bytes = numeric(0),
      modified_time = character(0),
      fingerprint = character(0),
      stringsAsFactors = FALSE
    )
  }

  out_path <- file.path(dir_output, file_name)
  write_csv_safe(x, out_path)
  invisible(out_path)
}


# ------------------------------------------------------------
# 8. manifest / summary helpers
# ------------------------------------------------------------
make_simple_file_record <- function(path, label = NA_character_) {
  if (!.file_exists2(path)) {
    return(data.frame(
      label = label,
      file_name = basename(path),
      file_path = .norm_path(path),
      ext = tools::file_ext(path),
      size_bytes = NA_real_,
      modified_time = NA_character_,
      exists = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(path)
  data.frame(
    label = label,
    file_name = basename(path),
    file_path = .norm_path(path),
    ext = tools::file_ext(path),
    size_bytes = info$size,
    modified_time = as.character(info$mtime),
    exists = TRUE,
    stringsAsFactors = FALSE
  )
}

make_file_records <- function(paths, label = NA_character_) {
  paths <- as.character(paths)
  if (length(paths) == 0) return(data.frame())
  out <- do.call(rbind, lapply(paths, make_simple_file_record, label = label))
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 9. convenience wrappers used often in steps
# ------------------------------------------------------------
reload_settings_outputs <- function(dir_rds = DIR_RDS) {
  list(
    CFG = load_step_rds("CFG", dir_rds = dir_rds, required = TRUE),
    DICT = load_step_rds("DICT", dir_rds = dir_rds, required = TRUE),
    RAW_DATA = load_step_rds("RAW_DATA", dir_rds = dir_rds, required = TRUE),
    SETTINGS_SUMMARY = load_step_rds("SETTINGS_SUMMARY", dir_rds = dir_rds, required = TRUE),
    SURVEY_BUNDLE = load_step_rds("SURVEY_BUNDLE", dir_rds = dir_rds, default = list())
  )
}

reload_prep_outputs <- function(dir_rds = DIR_RDS) {
  list(
    CFG = load_step_rds("CFG", dir_rds = dir_rds, required = TRUE),
    DICT = load_step_rds("DICT", dir_rds = dir_rds, required = TRUE),
    SETTINGS_SUMMARY = load_step_rds("SETTINGS_SUMMARY", dir_rds = dir_rds, required = TRUE),
    ANALYSIS_DATA_SUB = load_step_rds("ANALYSIS_DATA_SUB", dir_rds = dir_rds, required = TRUE),
    DISPLAY_DATA_SUB = load_step_rds("DISPLAY_DATA_SUB", dir_rds = dir_rds, required = TRUE),
    MPLUS_DATA = load_step_rds("MPLUS_DATA", dir_rds = dir_rds, required = TRUE),
    SURVEY_BUNDLE = load_step_rds("SURVEY_BUNDLE", dir_rds = dir_rds, default = list())
  )
}

# ------------------------------------------------------------
# 10. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("04_io.R loaded\n")
cat("------------------------------------------------------------\n")
cat("I/O helpers registered\n")
cat("YAML available  : ", YAML_AVAILABLE, "\n", sep = "")
cat("readr available : ", READR_AVAILABLE, "\n", sep = "")
cat("============================================================\n\n")
