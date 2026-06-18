# ------------------------------------------------------------
# local safe helpers
# ------------------------------------------------------------
if (!exists("safe_df", mode = "function")) {
  safe_df <- function(x) {
    if (is.null(x)) return(data.frame())
    if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
    if (is.matrix(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
    out <- tryCatch(
      as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) data.frame()
    )
    if (is.null(out)) out <- data.frame()
    out
  }
}

# ============================================================
# 04d_bch.R
# Mplus-native BCH distal outcome analysis
# ============================================================

T0_BCH <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("BCH", "04d_bch.R")
log_info("Reloading best-solution / prep outputs ...")

# ------------------------------------------------------------
# common path guard
# ------------------------------------------------------------
if (!exists("DIR_COMMON", inherits = TRUE) ||
    is.null(DIR_COMMON) ||
    !nzchar(as.character(DIR_COMMON))) {

  if (exists("PROJECT_ROOT", inherits = TRUE) &&
      !is.null(PROJECT_ROOT) &&
      nzchar(as.character(PROJECT_ROOT))) {
    DIR_COMMON <- file.path(PROJECT_ROOT, "R", "common")
  } else if (exists("DIR_R", inherits = TRUE) &&
             !is.null(DIR_R) &&
             nzchar(as.character(DIR_R))) {
    DIR_COMMON <- file.path(DIR_R, "common")
  } else {
    stop("DIR_COMMON not found. Please ensure path settings are initialized before BCH step.")
  }
}

if (!dir.exists(DIR_COMMON)) {
  stop("DIR_COMMON does not exist: ", DIR_COMMON)
}

PATH_BCH_CORE <- file.path(DIR_COMMON, "10_bch_core.R")
if (file.exists(PATH_BCH_CORE)) {
  source(PATH_BCH_CORE, local = TRUE)
}

# ------------------------------------------------------------
# 1. local helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  as.data.frame(x, stringsAsFactors = FALSE)
}

flatten_df <- function(df) {
  if (!is.data.frame(df)) return(df)

  for (nm in names(df)) {
    if (is.list(df[[nm]])) {
      df[[nm]] <- vapply(
        df[[nm]],
        function(x) paste(capture.output(str(x)), collapse = " "),
        character(1)
      )
    }
  }

  rownames(df) <- NULL
  df
}

coerce_atomic_col <- function(x, prefer_numeric = FALSE) {
  if (!is.list(x)) {
    if (isTRUE(prefer_numeric)) {
      return(suppressWarnings(as.numeric(as.character(x))))
    }
    return(x)
  }

  out <- vapply(
    x,
    function(el) {
      if (is.null(el) || length(el) == 0) return(NA_character_)
      if (length(el) == 1) return(as.character(el[[1]]))
      paste(as.character(unlist(el, recursive = TRUE, use.names = FALSE)), collapse = "; ")
    },
    character(1)
  )

  if (isTRUE(prefer_numeric)) {
    return(suppressWarnings(as.numeric(out)))
  }

  out
}

sanitize_df_for_export <- function(df, numeric_cols = character(0)) {
  if (!is.data.frame(df)) return(df)

  for (nm in names(df)) {
    df[[nm]] <- coerce_atomic_col(df[[nm]], prefer_numeric = nm %in% numeric_cols)
  }

  rownames(df) <- NULL
  df
}

sort_class_cols_local <- function(nm) {
  cls <- grep("^class[0-9]+$", nm, value = TRUE)
  cls[order(as.numeric(gsub("^class", "", cls)))]
}

sort_se_class_cols_local <- function(nm) {
  cls <- grep("^se_class[0-9]+$", nm, value = TRUE)
  cls[order(as.numeric(gsub("^se_class", "", cls)))]
}

safe_num_local <- function(x) suppressWarnings(as.numeric(as.character(x)))

fmt_p_tbl <- function(p, digits = 3) {
  p <- safe_num_local(p)
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  out[ok] <- ifelse(p[ok] < .001, "<.001", formatC(p[ok], format = "f", digits = digits))
  out
}

# ----------------------------------------------------------
# p formatting helpers (🔥 추가)
# ----------------------------------------------------------
fmt_p3_strict <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  ifelse(
    is.na(p),
    NA_character_,
    ifelse(p < .001, "<.001", formatC(p, format = "f", digits = 3))
  )
}

sig_mark <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- rep("", length(p))
  out[!is.na(p) & p < .001] <- "***"
  out[!is.na(p) & p >= .001 & p < .01] <- "**"
  out[!is.na(p) & p >= .01 & p < .05] <- "*"
  out
}

p_to_sig_tbl <- function(p) {
  p <- safe_num_local(p)
  out <- rep("", length(p))
  out[!is.na(p) & p < .001] <- "***"
  out[!is.na(p) & p >= .001 & p < .01] <- "**"
  out[!is.na(p) & p >= .01 & p < .05] <- "*"
  out
}

resolve_out_file_from_inp <- function(inp_file) {
  sub("\\.inp$", ".out", inp_file, ignore.case = TRUE)
}

norm_space_collect <- function(x) {
  x <- as.character(x)
  x <- gsub("[\\r\\n\\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

extract_nums_collect <- function(x) {
  x <- norm_space_collect(x)
  m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?", x, perl = TRUE)
  out <- regmatches(x, m)[[1]]
  out <- gsub("D", "E", out, fixed = TRUE)
  out
}

extract_model_structure_from_tag <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  hit <- grepl("_model[0-9]+_k[0-9]+_", x, ignore.case = TRUE)
  out[hit] <- sub("^.*_(model[0-9]+)_k[0-9]+_.*$", "\\1", x[hit], ignore.case = TRUE)
  tolower(out)
}

mplus_wrap_statement <- function(keyword, vars, indent = "  ", width = 78) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (length(vars) == 0) return(character(0))

  line_prefix <- paste0(indent, keyword, " = ")
  cont_prefix <- paste0(indent, "  ")

  out <- character(0)
  current <- line_prefix

  for (v in vars) {
    candidate <- if (identical(current, line_prefix)) paste0(current, v) else paste(current, v)
    if (nchar(candidate, type = "width") > width) {
      out <- c(out, current)
      current <- paste0(cont_prefix, v)
    } else {
      current <- candidate
    }
  }

  c(out, paste0(current, ";"))
}

make_mplus_type_line <- function(weight_var = NULL, strata_var = NULL, cluster_var = NULL) {
  has_complex <- !is.null(weight_var) || !is.null(strata_var) || !is.null(cluster_var)
  if (has_complex) "  TYPE = MIXTURE COMPLEX;" else "  TYPE = MIXTURE;"
}

build_survey_variable_lines <- function(weight_var = NULL, strata_var = NULL, cluster_var = NULL) {
  out <- character(0)
  if (!is.null(weight_var) && nzchar(weight_var)) out <- c(out, paste0("  WEIGHT = ", weight_var, ";"))
  if (!is.null(strata_var) && nzchar(strata_var)) out <- c(out, paste0("  STRATIFICATION = ", strata_var, ";"))
  if (!is.null(cluster_var) && nzchar(cluster_var)) out <- c(out, paste0("  CLUSTER = ", cluster_var, ";"))
  out
}

make_output_lines_mplus <- function(CFG = NULL) {
  tech1    <- isTRUE(CFG$mplus$output$tech1 %||% FALSE)
  tech4    <- isTRUE(CFG$mplus$output$tech4 %||% FALSE)
  tech8    <- isTRUE(CFG$mplus$output$tech8 %||% FALSE)
  tech11   <- isTRUE(CFG$mplus$output$tech11 %||% FALSE)
  sampstat <- isTRUE(CFG$mplus$output$sampstat %||% FALSE)

  out <- c("OUTPUT:")
  if (sampstat) out <- c(out, "  SAMPSTAT;")
  if (tech1)    out <- c(out, "  TECH1;")
  if (tech4)    out <- c(out, "  TECH4;")
  if (tech11)   out <- c(out, "  TECH11;")
  if (tech8)    out <- c(out, "  TECH8;")

  if (length(out) == 1L) return(character(0))
  out
}

empty_bch_long <- function() {
  data.frame(
    analysis         = character(0),
    model_type       = character(0),
    method           = character(0),
    best_k           = integer(0),
    best_tag         = character(0),
    model_structure  = character(0),
    outcome          = character(0),
    var_name         = character(0),
    var_label        = character(0),
    outcome_type     = character(0),
    class            = character(0),
    class_num        = integer(0),
    estimate         = numeric(0),
    se               = numeric(0),
    stat             = numeric(0),
    df               = numeric(0),
    p                = numeric(0),
    p_fmt            = character(0),
    sig              = character(0),
    omnibus_p        = numeric(0),
    omnibus_p_fmt    = character(0),
    omnibus_sig      = character(0),
    inp_file         = character(0),
    out_file         = character(0),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 2. reload objects
# ------------------------------------------------------------

# ------------------------------------------------------------
# FORCE refresh stale BCH caches
# 정확한 위치: section 2 "reload objects" 시작 직전
# ------------------------------------------------------------
unlink(file.path(DIR_RDS, "BCH_POSTHOC.rds"))
unlink(file.path(DIR_RDS, "BCH_RESULTS_FULL.rds"))
unlink(file.path(DIR_RDS, "BCH_RESULTS.rds"))
unlink(file.path(DIR_RDS, "BCH_OMNIBUS_BASIC.rds"))
unlink(file.path(DIR_RDS, "BCH_INTERACTION.rds"))
unlink(file.path(DIR_RDS, "T6_bch.rds"))

BEST_K_SUMMARY           <- load_step_rds(
  "BEST_K_SUMMARY",
  dir_rds    = DIR_RDS,
  default    = NULL
)
if (is.null(BEST_K_SUMMARY)) {
  BEST_K_SUMMARY         <- load_step_rds(
    "SELECT_BEST_K_SUMMARY",
    dir_rds  = DIR_RDS,
    required = TRUE
  )
}

BEST_MODEL_ROW           <- load_step_rds(
  "BEST_MODEL_ROW",
  dir_rds    = DIR_RDS,
  default    = data.frame()
)

ESTIMATION_BUILD_SUMMARY <- load_step_rds(
  "ESTIMATION_BUILD_SUMMARY",
  dir_rds    = DIR_RDS,
  required   = TRUE
)

SETTINGS_SUMMARY         <- load_step_rds(
  "SETTINGS_SUMMARY",
  dir_rds    = DIR_RDS,
  required   = TRUE
)

DICT                     <- load_step_rds(
  "DICT",
  dir_rds    = DIR_RDS,
  default    = list()
)

MPLUS_EXPORT_DATA        <- load_step_rds(
  "MPLUS_EXPORT_DATA",
  dir_rds    = DIR_RDS,
  default    = NULL
)
if (is.null(MPLUS_EXPORT_DATA)) {
  MPLUS_EXPORT_DATA      <- load_step_rds(
    "MPLUS_DATA",
    dir_rds  = DIR_RDS,
    required = TRUE
  )
}

CFG                      <- load_step_rds(
  "CFG",
  dir_rds    = DIR_RDS,
  required   = TRUE
)
if (exists("PATH_CFG") && file.exists(PATH_CFG) && exists("read_cfg")) {
  CFG_CURRENT <- tryCatch(
    read_cfg(PATH_CFG, required = FALSE),
    error = function(e) NULL
  )
  if (is.list(CFG_CURRENT)) {
    CFG <- utils::modifyList(CFG, CFG_CURRENT)
  }
}

RAW_DATA                 <- load_step_rds(
  "RAW_DATA",
  dir_rds    = DIR_RDS,
  default    = data.frame()
)

REFERENCE_CLASS          <- load_step_rds(
  "REFERENCE_CLASS",
  dir_rds    = DIR_RDS,
  default    = NA_integer_
)

REFERENCE_CLASS_LABEL    <- load_step_rds(
  "REFERENCE_CLASS_LABEL",
  dir_rds    = DIR_RDS,
  default    = NA_character_
)

# ------------------------------------------------------------
# 3. normalize CFG / resolve basics
# ------------------------------------------------------------
if (!is.list(CFG)) CFG <- list()
if (is.null(CFG$data) || !is.list(CFG$data)) CFG$data <- list()
if (is.null(CFG$data$missing_code)) CFG$data$missing_code <- -9999

best_k <- as.integer(
  BEST_K_SUMMARY$best_k %||%
    BEST_K_SUMMARY$BEST_K %||%
    NA_integer_
)

best_tag <- as.character(
  BEST_K_SUMMARY$best_tag %||%
    BEST_K_SUMMARY$BEST_TAG %||%
    NA_character_
)

best_model_structure <- tolower(as.character(
  BEST_K_SUMMARY$best_model_structure %||%
    BEST_K_SUMMARY$BEST_MODEL_STRUCTURE %||%
    NA_character_
))

if (is.na(best_k) || !nzchar(best_tag)) {
  stop("BEST_K / BEST_TAG could not be resolved.", call. = FALSE)
}
if (is.na(best_model_structure) || !nzchar(best_model_structure)) {
  best_model_structure <- extract_model_structure_from_tag(best_tag)
}

MIXTURE_TYPE <- tolower(
  SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    "lpa"
)

INDICATORS             <- SETTINGS_SUMMARY$indicators %||% SETTINGS_SUMMARY$INDICATORS %||% character(0)
INDICATORS_CATEGORICAL <- SETTINGS_SUMMARY$indicators_categorical %||% SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% character(0)
INDICATORS_CONTINUOUS  <- SETTINGS_SUMMARY$indicators_continuous %||% SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% character(0)
OUTCOMES               <- SETTINGS_SUMMARY$outcomes %||% SETTINGS_SUMMARY$OUTCOMES %||% character(0)

DICT_OUTCOMES <- character(0)
if (is.list(DICT) && is.data.frame(DICT$meta) && all(c("var_name", "role") %in% names(DICT$meta))) {
  meta_bch <- DICT$meta
  if (!"use" %in% names(meta_bch)) meta_bch$use <- TRUE
  meta_bch$role <- tolower(trimws(as.character(meta_bch$role)))
  meta_bch$use <- as.logical(meta_bch$use)
  meta_bch$use[is.na(meta_bch$use)] <- TRUE
  DICT_OUTCOMES <- as.character(meta_bch$var_name[meta_bch$role %in% c("outcome", "outcomes") & meta_bch$use])
}
if (exists("PATH_DICT") && file.exists(PATH_DICT)) {
  dict_file_bch <- tryCatch(
    utils::read.csv(PATH_DICT, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
  if (is.data.frame(dict_file_bch) && all(c("var_name", "role") %in% names(dict_file_bch))) {
    if (!"use" %in% names(dict_file_bch)) dict_file_bch$use <- TRUE
    dict_file_bch$role <- tolower(trimws(as.character(dict_file_bch$role)))
    dict_file_bch$use <- as.logical(dict_file_bch$use)
    dict_file_bch$use[is.na(dict_file_bch$use)] <- TRUE
    DICT_OUTCOMES <- unique(c(
      DICT_OUTCOMES,
      as.character(dict_file_bch$var_name[dict_file_bch$role %in% c("outcome", "outcomes") & dict_file_bch$use])
    ))
  }
}

CFG_OUTCOMES <- unique(c(
  CFG$outcomes %||% character(0),
  CFG$bch$outcomes %||% character(0)
))
if (exists("PATH_CFG") && file.exists(PATH_CFG) && exists("read_cfg")) {
  CFG_OUTCOMES_CURRENT <- tryCatch(
    {
      cfg_current_bch <- read_cfg(PATH_CFG, required = FALSE)
      unique(c(
        cfg_current_bch$outcomes %||% character(0),
        cfg_current_bch$bch$outcomes %||% character(0)
      ))
    },
    error = function(e) character(0)
  )
  CFG_OUTCOMES <- unique(c(CFG_OUTCOMES, CFG_OUTCOMES_CURRENT))
}
OUTCOMES <- unique(c(OUTCOMES, DICT_OUTCOMES, CFG_OUTCOMES))

WEIGHT_VAR  <- SETTINGS_SUMMARY$weight_var %||% SETTINGS_SUMMARY$WEIGHT_VAR %||% NULL
STRATA_VAR  <- SETTINGS_SUMMARY$strata_var %||% SETTINGS_SUMMARY$STRATA_VAR %||% NULL
CLUSTER_VAR <- SETTINGS_SUMMARY$cluster_var %||% SETTINGS_SUMMARY$CLUSTER_VAR %||% NULL
ID_VAR      <- SETTINGS_SUMMARY$id_var %||% SETTINGS_SUMMARY$ID_VAR %||% "id"

if (!is.null(ID_VAR) && !ID_VAR %in% names(MPLUS_EXPORT_DATA)) {
  ID_VAR <- "id"
}

MISSING_OUTCOMES <- setdiff(unique_nz(OUTCOMES), names(MPLUS_EXPORT_DATA))
if (length(MISSING_OUTCOMES) > 0 &&
    is.data.frame(RAW_DATA) &&
    nrow(RAW_DATA) == nrow(MPLUS_EXPORT_DATA)) {

  ADD_OUTCOMES <- intersect(MISSING_OUTCOMES, names(RAW_DATA))
  if (length(ADD_OUTCOMES) > 0) {
    for (vv in ADD_OUTCOMES) {
      MPLUS_EXPORT_DATA[[vv]] <- RAW_DATA[[vv]]
    }
    log_info(
      "Added outcome columns from RAW_DATA for BCH = ",
      paste(ADD_OUTCOMES, collapse = ", ")
    )
  }
}

MISSING_OUTCOMES <- setdiff(unique_nz(OUTCOMES), names(MPLUS_EXPORT_DATA))
if (length(MISSING_OUTCOMES) > 0 &&
    exists("PATH_DATA") &&
    file.exists(PATH_DATA) &&
    exists("read_data_file")) {

  DATA_FILE_CURRENT <- tryCatch(
    as.data.frame(read_data_file(PATH_DATA, required = FALSE), stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  if (is.data.frame(DATA_FILE_CURRENT) &&
      nrow(DATA_FILE_CURRENT) == nrow(MPLUS_EXPORT_DATA)) {

    ADD_OUTCOMES <- intersect(MISSING_OUTCOMES, names(DATA_FILE_CURRENT))
    if (length(ADD_OUTCOMES) > 0) {
      for (vv in ADD_OUTCOMES) {
        MPLUS_EXPORT_DATA[[vv]] <- DATA_FILE_CURRENT[[vv]]
        RAW_DATA[[vv]] <- DATA_FILE_CURRENT[[vv]]
      }
      log_info(
        "Added outcome columns from PATH_DATA for BCH = ",
        paste(ADD_OUTCOMES, collapse = ", ")
      )
    }
  }
}

INDICATORS             <- intersect(unique_nz(INDICATORS), names(MPLUS_EXPORT_DATA))
INDICATORS_CATEGORICAL <- intersect(unique_nz(INDICATORS_CATEGORICAL), names(MPLUS_EXPORT_DATA))
INDICATORS_CONTINUOUS  <- intersect(unique_nz(INDICATORS_CONTINUOUS), names(MPLUS_EXPORT_DATA))
OUTCOMES               <- intersect(unique_nz(OUTCOMES), names(MPLUS_EXPORT_DATA))
OUTCOMES               <- OUTCOMES[!is.na(OUTCOMES) & nzchar(trimws(OUTCOMES))]

HAS_OUTCOME <- length(OUTCOMES) > 0

log_info("BEST_K               = ", best_k)
log_info("BEST_TAG             = ", best_tag)
log_info("BEST_MODEL_STRUCTURE = ", best_model_structure)
log_info("OUTCOMES             = ", paste(OUTCOMES, collapse = ", "))
log_info("HAS_OUTCOME          = ", HAS_OUTCOME)

MODERATOR_VARS <- unique(c(
  CFG$bch$moderators %||% character(0),
  CFG$bch$moderation$moderators %||% character(0),
  CFG$bch$moderation$moderator_var %||% character(0)
))
if (length(MODERATOR_VARS) == 0) MODERATOR_VARS <- "group"
MODERATOR_VARS <- unique(as.character(MODERATOR_VARS))
MODERATOR_VARS <- MODERATOR_VARS[nzchar(MODERATOR_VARS)]

if (!is.null(WEIGHT_VAR)  && !WEIGHT_VAR  %in% names(MPLUS_EXPORT_DATA)) WEIGHT_VAR  <- NULL
if (!is.null(STRATA_VAR)  && !STRATA_VAR  %in% names(MPLUS_EXPORT_DATA)) STRATA_VAR  <- NULL
if (!is.null(CLUSTER_VAR) && !CLUSTER_VAR %in% names(MPLUS_EXPORT_DATA)) CLUSTER_VAR <- NULL
if (!is.null(ID_VAR)      && !ID_VAR      %in% names(MPLUS_EXPORT_DATA)) ID_VAR      <- NULL

make_interaction_id <- function(df, id_var = NULL) {
  source_id <- NULL
  if ("id" %in% names(df)) {
    source_id <- df$id
  } else if (!is.null(id_var) && nzchar(id_var) && id_var %in% names(df)) {
    source_id <- df[[id_var]]
  }

  if (is.null(source_id)) {
    return(as.character(seq_len(nrow(df))))
  }

  source_id <- as.character(source_id)
  if (any(is.na(source_id)) || any(duplicated(source_id))) {
    return(as.character(seq_len(nrow(df))))
  }

  source_id
}

MPLUS_EXPORT_DATA$id <- make_interaction_id(MPLUS_EXPORT_DATA, ID_VAR)
INTERACTION_ID_USES_ROW_ORDER <- identical(MPLUS_EXPORT_DATA$id, as.character(seq_len(nrow(MPLUS_EXPORT_DATA))))
if (INTERACTION_ID_USES_ROW_ORDER) {
  log_warn("Interaction merge id uses row order because exported ID is missing or non-unique.")
}

MISSING_CODE <- ESTIMATION_BUILD_SUMMARY$missing_code %||% CFG$data$missing_code %||% CFG$missing_code %||% -9999
MPLUS_EXE <- resolve_mplus_exe(CFG, must_exist = FALSE)

# ------------------------------------------------------------
# 3B. interaction용 class key 로딩
# ------------------------------------------------------------
load_class_key_candidates <- function(dir_rds, id_var = "id", expected_n = NULL) {
  cand_names <- c(
    "CLASSIFIED_ANALYSIS",
    "ANALYSIS_DATA_CLASSIFIED",
    "CLASS_SUMMARY_FINAL",
    "CLASS_ASSIGNMENT",
    "CLASSIFICATION",
    "classified",
    "CLASS"
  )

  out_list <- list()

  for (nm in cand_names) {
    obj <- tryCatch(
      load_step_rds(nm, dir_rds = dir_rds, default = data.frame()),
      error = function(e) data.frame()
    )

    obj <- safe_df(obj)
    if (!is.data.frame(obj) || nrow(obj) == 0) next

    # id 보정
    if (!is.null(expected_n) && is.finite(expected_n) && nrow(obj) == expected_n) {
      source_id <- NULL
      if ("id" %in% names(obj)) {
        source_id <- obj$id
      } else if (!is.null(id_var) && id_var %in% names(obj)) {
        source_id <- obj[[id_var]]
      }

      if (is.null(source_id)) {
        obj$id <- seq_len(nrow(obj))
      } else {
        source_id <- as.character(source_id)
        if (any(is.na(source_id)) || any(duplicated(source_id))) {
          obj$id <- seq_len(nrow(obj))
        } else if (!"id" %in% names(obj) && !is.null(id_var) && id_var %in% names(obj)) {
          names(obj)[names(obj) == id_var] <- "id"
        }
      }
    } else if (!"id" %in% names(obj)) {
      if (!is.null(id_var) && id_var %in% names(obj)) {
        names(obj)[names(obj) == id_var] <- "id"
      }
    }

    # class_num 보정
    if (!"class_num" %in% names(obj)) {
      cand_class <- c("class_num", "Class_num", "class", "Class", "most_likely_class")
      cand_class <- cand_class[cand_class %in% names(obj)]
      if (length(cand_class) > 0) {
        cc <- cand_class[1]
        obj$class_num <- suppressWarnings(
          as.numeric(gsub("[^0-9]", "", as.character(obj[[cc]])))
        )
      }
    }

    keep <- intersect(c("id", "class_num"), names(obj))
    if (length(keep) < 2) next

    obj <- obj[, keep, drop = FALSE]
    obj <- obj[!is.na(obj$id), , drop = FALSE]
    obj$id <- as.character(obj$id)
    obj$class_num <- suppressWarnings(as.numeric(as.character(obj$class_num)))
    obj <- obj[!is.na(obj$class_num), , drop = FALSE]

    if (nrow(obj) == 0) next

    n_rows      <- nrow(obj)
    n_unique_id <- length(unique(obj$id))
    n_dup_id    <- sum(duplicated(obj$id))

    out_list[[nm]] <- list(
      data = obj,
      n_rows = n_rows,
      n_unique_id = n_unique_id,
      n_dup_id = n_dup_id
    )
  }

  if (length(out_list) == 0) return(data.frame())

  score_tbl <- do.call(
    rbind,
    lapply(names(out_list), function(nm) {
      x <- out_list[[nm]]
      data.frame(
        object_name = nm,
        n_rows = x$n_rows,
        n_unique_id = x$n_unique_id,
        n_dup_id = x$n_dup_id,
        stringsAsFactors = FALSE
      )
    })
  )

  # expected_n이 있으면, 거기에 가장 가까운 후보를 우선
  if (!is.null(expected_n) && is.finite(expected_n) && expected_n > 0) {
    score_tbl$dist_expected <- abs(score_tbl$n_unique_id - expected_n)
  } else {
    score_tbl$dist_expected <- 0
  }

  # 너무 작은 후보 제거: expected_n의 절반 미만이면 탈락
  if (!is.null(expected_n) && is.finite(expected_n) && expected_n > 0) {
    score_tbl <- score_tbl[score_tbl$n_unique_id >= max(10, floor(expected_n * 0.5)), , drop = FALSE]
    if (nrow(score_tbl) == 0) {
      score_tbl <- do.call(
        rbind,
        lapply(names(out_list), function(nm) {
          x <- out_list[[nm]]
          data.frame(
            object_name = nm,
            n_rows = x$n_rows,
            n_unique_id = x$n_unique_id,
            n_dup_id = x$n_dup_id,
            dist_expected = abs(x$n_unique_id - expected_n),
            stringsAsFactors = FALSE
          )
        })
      )
    }
  }

  score_tbl <- score_tbl[
    order(
      score_tbl$dist_expected,
      -score_tbl$n_unique_id,
      score_tbl$n_dup_id,
      -score_tbl$n_rows
    ),
    ,
    drop = FALSE
  ]

  best_name <- score_tbl$object_name[1]
  best_obj  <- out_list[[best_name]]$data

  # 중복 id는 제거하되, 이 시점에서는 best 후보가 개인단위여야 함
  best_obj <- best_obj[!duplicated(best_obj$id), , drop = FALSE]
  rownames(best_obj) <- NULL

  attr(best_obj, "selected_source") <- best_name
  attr(best_obj, "candidate_score") <- score_tbl
  best_obj
}

CLASS_KEY <- load_class_key_candidates(
  dir_rds    = DIR_RDS,
  id_var     = ID_VAR,
  expected_n = nrow(MPLUS_EXPORT_DATA)
)

if (nrow(CLASS_KEY) > 0) {
  log_info("CLASS_KEY source      = ", attr(CLASS_KEY, "selected_source"))
  log_info("CLASS_KEY n(rows)     = ", nrow(CLASS_KEY))
  log_info("CLASS_KEY n(unique id)= ", length(unique(CLASS_KEY$id)))
  log_info("CLASS_KEY dup(id)     = ", sum(duplicated(CLASS_KEY$id)))
} else {
  log_warn("CLASS_KEY could not be resolved from saved classify outputs.")
}

if ("id" %in% names(MPLUS_EXPORT_DATA)) {
  MPLUS_EXPORT_DATA$id <- as.character(MPLUS_EXPORT_DATA$id)
}

# ------------------------------------------------------------
# 4. directories
# ------------------------------------------------------------
DIR_MPLUS_BCH <- file.path(DIR_MPLUS, "bch")
DIR_MPLUS_BCH_DATA <- file.path(DIR_MPLUS_BCH, "data")
DIR_MPLUS_BCH_INP  <- file.path(DIR_MPLUS_BCH, "inp")
DIR_MPLUS_BCH_OUT  <- file.path(DIR_MPLUS_BCH, "out")
DIR_MPLUS_BCH_BAT  <- file.path(DIR_MPLUS_BCH, "bat")

ensure_dir2(DIR_MPLUS_BCH)
ensure_dir2(DIR_MPLUS_BCH_DATA)
ensure_dir2(DIR_MPLUS_BCH_INP)
ensure_dir2(DIR_MPLUS_BCH_OUT)
ensure_dir2(DIR_MPLUS_BCH_BAT)

# ------------------------------------------------------------
# 5. dictionary helpers
# ------------------------------------------------------------
dict_meta <- get_dict_meta_tbl(DICT)
if (exists("PATH_DICT") && file.exists(PATH_DICT)) {
  dict_meta_file <- tryCatch(
    utils::read.csv(PATH_DICT, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
  if (is.data.frame(dict_meta_file) && "var_name" %in% names(dict_meta_file)) {
    dict_meta <- dict_meta_file
  }
}

resolve_var_label_local <- function(v) {
  if (is.data.frame(dict_meta) && nrow(dict_meta) > 0) {
    hit <- dict_meta[dict_meta$var_name == v, , drop = FALSE]
    if (nrow(hit) > 0) {
      cand_cols <- c("label_en", "label_ko", "var_label", "var_name")
      cand_cols <- cand_cols[cand_cols %in% names(hit)]
      for (cc in cand_cols) {
        lab <- hit[[cc]][1]
        if (!is.na(lab) && nzchar(as.character(lab))) return(as.character(lab))
      }
    }
  }
  v
}

resolve_outcome_type_local <- function(v, x) {
  hit <- NULL
  if (is.data.frame(dict_meta) && nrow(dict_meta) > 0) {
    hit <- dict_meta[dict_meta$var_name == v, , drop = FALSE]
  }

  subtype <- if (!is.null(hit) && nrow(hit) > 0 && "subtype" %in% names(hit)) hit$subtype[1] else NA_character_
  type <- if (!is.null(hit) && nrow(hit) > 0 && "type" %in% names(hit)) hit$type[1] else NA_character_

  if (!is.na(subtype) && subtype == "categorical") return("categorical")
  if (!is.na(subtype) && subtype == "continuous") return("continuous")

  if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")

  if (is.numeric(x) || is.integer(x)) {
    ux <- unique(stats::na.omit(x))
    if (length(ux) <= 5 && all(abs(ux - round(ux)) < 1e-8)) {
      return("categorical")
    }
    return("continuous")
  }

  if (!is.na(type) && type %in% c("factor", "character")) return("categorical")
  "continuous"
}

resolve_bch_label_local <- function(v) {
  v <- as.character(v)
  out <- v
  for (i in seq_along(v)) {
    out[i] <- resolve_var_label_local(v[i])
  }
  out
}

# ------------------------------------------------------------
# 6. prepare outcome spec
# ------------------------------------------------------------
build_bch_outcome_spec <- function(data, outcomes) {
  rows <- list()
  idx <- 1L

  for (v in outcomes) {
    if (!v %in% names(data)) next

    x <- data[[v]]
    out_type <- resolve_outcome_type_local(v, x)

    if (out_type == "continuous") {
      x2 <- suppressWarnings(as.numeric(x))
    } else {
      if (is.factor(x)) x <- as.character(x)
      if (is.logical(x)) x <- as.integer(x)

      if (is.character(x)) {
        suppressWarnings(x_num <- as.numeric(x))
        if (sum(!is.na(x_num)) > 0 && sum(is.na(x_num)) < length(x_num)) {
          x2 <- x_num
        } else {
          x2 <- as.integer(factor(x))
        }
      } else {
        x2 <- suppressWarnings(as.numeric(x))
      }
    }

    data[[v]] <- x2

    rows[[idx]] <- data.frame(
      outcome = v,
      var_name = v,
      var_label = resolve_var_label_local(v),
      outcome_type = out_type,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  spec_df <- if (length(rows) == 0) data.frame() else do.call(rbind, rows)
  if (is.data.frame(spec_df) && nrow(spec_df) > 0) rownames(spec_df) <- NULL

  list(
    data = data,
    spec = spec_df
  )
}

log_info("Preparing outcomes for Mplus-native BCH ...")

bch_obj <- build_bch_outcome_spec(MPLUS_EXPORT_DATA, OUTCOMES)
BCH_DATA <- bch_obj$data
BCH_OUTCOME_SPEC <- bch_obj$spec

if (!HAS_OUTCOME || !is.data.frame(BCH_OUTCOME_SPEC) || nrow(BCH_OUTCOME_SPEC) == 0) {
  log_info("No outcomes found. Skipping BCH step and creating empty standardized outputs.")

  empty_bch <- empty_bch_long()

  BCH_RESULTS <- data.frame(
    var_name = character(0),
    label            = character(0),
    p                = numeric(0),
    display_order    = numeric(0),
    best_k           = integer(0),
    best_tag         = character(0),
    model_structure  = character(0),
    stringsAsFactors = FALSE
  )

  BCH_RESULTS_FULL <- empty_bch
  BCH_OMNIBUS_BASIC <- empty_bch_omnibus_basic()
  BCH_METADATA <- data.frame(
    best_k           = best_k,
    best_tag         = best_tag,
    model_structure  = best_model_structure,
    selected_model   = best_model_structure,
    method           = "Mplus-native BCH",
    n_outcomes       = 0L,
    created_at       = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  BCH_INTERACTION <- data.frame()
  T6_bch <- BCH_RESULTS_FULL

  log_info("Saving BCH outputs ...")

  save_named_rds_list(
    list(
      BCH_DATA          = BCH_DATA,
      BCH_OUTCOME_SPEC  = BCH_OUTCOME_SPEC,
      BCH_RESULTS       = BCH_RESULTS,
      BCH_RESULTS_FULL  = BCH_RESULTS_FULL,
      BCH_OMNIBUS_BASIC = BCH_OMNIBUS_BASIC,
      BCH_METADATA      = BCH_METADATA,
      BCH_INTERACTION   = BCH_INTERACTION,
      T6_bch            = T6_bch
    ),
    dir_rds = DIR_RDS
  )

  if (exists("DIR_TABLES") && !is.null(DIR_TABLES)) {
    write_csv_safe(BCH_RESULTS,       file.path(DIR_TABLES, "bch_results.csv"))
    write_csv_safe(BCH_RESULTS_FULL,  file.path(DIR_TABLES, "bch_results_full.csv"))
    write_csv_safe(BCH_OMNIBUS_BASIC, file.path(DIR_TABLES, "bch_omnibus_basic.csv"))
    write_csv_safe(BCH_METADATA,      file.path(DIR_TABLES, "bch_metadata.csv"))
    write_csv_safe(BCH_INTERACTION,   file.path(DIR_TABLES, "bch_interaction.csv"))
  }

  elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_BCH, units = "secs")), 2)

  log_info("04d_bch.R completed with empty standardized outputs.")
  log_info("method          = Mplus-native BCH")
  log_info("best_k          = ", best_k)
  log_info("best_tag        = ", best_tag)
  log_info("model_structure = ", best_model_structure)
  log_info("n(outcomes)     = 0")
  log_info("n(BCH_RESULTS)  = ", nrow(BCH_RESULTS))
  log_info("n(BCH_FULL)     = ", nrow(BCH_RESULTS_FULL))
  log_info("elapsed         = ", elapsed_sec, " sec")

  log_step_end("bch", elapsed_sec, ok = TRUE)

} else {

  for (nm in names(BCH_DATA)) {
    x <- BCH_DATA[[nm]]
    if (is.factor(x)) x  <- as.character(x)
    if (is.logical(x)) x <- as.integer(x)
    if (is.character(x)) suppressWarnings(x <- as.numeric(x))
    if (!is.numeric(x)) suppressWarnings(x <- as.numeric(x))
    x[is.na(x)]    <- MISSING_CODE
    BCH_DATA[[nm]] <- x
  }

  BCH_DATA_FILE <- file.path(
    DIR_MPLUS_BCH_DATA,
    paste0(tolower(DATASET_ID), "_bch_data.dat")
  )
  BCH_HEADER_FILE <- file.path(
    DIR_MPLUS_BCH_DATA,
    paste0(tolower(DATASET_ID), "_bch_header.txt")
  )

  write_mplus_data(BCH_DATA, BCH_DATA_FILE, missing_code = MISSING_CODE)
  write_mplus_header(names(BCH_DATA), BCH_HEADER_FILE)

  # ----------------------------------------------------------
  # 7. input builder
  # ----------------------------------------------------------
  build_bch_input <- function(outcome_name, outcome_type, model_tag) {
    inp_file       <- file.path(DIR_MPLUS_BCH_INP, paste0(model_tag, ".inp"))
    out_file       <- resolve_out_file_from_inp(inp_file)

    usevars        <- unique(c(
      INDICATORS,
      outcome_name,
      WEIGHT_VAR,
      STRATA_VAR,
      CLUSTER_VAR,
      ID_VAR
    ))
    usevars        <- usevars[!is.na(usevars) & nzchar(usevars)]
    usevars        <- intersect(usevars, names(BCH_DATA))

    cat_decl       <- character(0)
    if (identical(MIXTURE_TYPE, "lca") && length(INDICATORS) > 0) {
      cat_decl     <- INDICATORS
    } else if (MIXTURE_TYPE %in% c("lpa", "mixed") && length(INDICATORS_CATEGORICAL) > 0) {
      cat_decl     <- INDICATORS_CATEGORICAL
    }

    # 🔥 BCH에서는 outcome을 categorical에서 제거
    # categorical indicator만 유지
    cat_decl       <- setdiff(cat_decl, OUTCOMES)
    cat_decl       <- intersect(unique(cat_decl), names(BCH_DATA))

    variable_lines <- c(
      "VARIABLE:",
      mplus_wrap_statement("NAMES", names(BCH_DATA)),
      mplus_wrap_statement("USEVARIABLES", usevars)
    )

    if (length(cat_decl) > 0) {
      variable_lines <- c(variable_lines, mplus_wrap_statement("CATEGORICAL", cat_decl))
    }

    if (!is.null(ID_VAR) && nzchar(ID_VAR) && ID_VAR %in% names(BCH_DATA)) {
      variable_lines <- c(variable_lines, paste0("  IDVARIABLE = ", ID_VAR, ";"))
    }

    variable_lines <- c(
      variable_lines,
      paste0("  MISSING = ALL (", MISSING_CODE, ");"),
      paste0("  CLASSES = c(", best_k, ");"),
      build_survey_variable_lines(
        weight_var  = WEIGHT_VAR,
        strata_var  = STRATA_VAR,
        cluster_var = CLUSTER_VAR
      ),
      paste0("  AUXILIARY = ", outcome_name, " (BCH);")
    )

    analysis_lines <- c(
      "ANALYSIS:",
      make_mplus_type_line(
        weight_var  = WEIGHT_VAR,
        strata_var  = STRATA_VAR,
        cluster_var = CLUSTER_VAR
      ),
      paste0(
        "  ESTIMATOR = ",
        toupper(CFG$mplus$estimator %||% CFG$estimation$estimator %||% "MLR"),
        ";"
      ),
      paste0(
        "  STARTS = ",
        as.integer(CFG$mplus$starts %||% 500L),
        " ",
        as.integer(CFG$mplus$starts %||% 500L),
        ";"
      ),
      paste0("  STITERATIONS = ", as.integer(CFG$mplus$stiterations %||% 20L), ";"),
      paste0("  PROCESSORS = ", as.integer(CFG$mplus$processors %||% 2L), ";")
    )

    model_lines <- c("MODEL:", "  %OVERALL%")

    if (MIXTURE_TYPE %in% c("lpa", "mixed")) {
      cont_inds <- INDICATORS_CONTINUOUS
      if (length(cont_inds) == 0) cont_inds <- INDICATORS
      cont_inds <- intersect(unique(cont_inds), names(BCH_DATA))

      if (length(cont_inds) > 0) {
        model_lines <- c(
          model_lines,
          paste0("    [", paste(cont_inds, collapse = " "), "];"),
          paste0("    ", paste(cont_inds, collapse = " "), ";")
        )
      }
    }

    output_lines <- c("OUTPUT:", "  TECH4;")
    extra_output_lines <- make_output_lines_mplus(CFG)
    if (length(extra_output_lines) > 0) {
      extra_output_lines <- extra_output_lines[extra_output_lines != "OUTPUT:"]
      output_lines <- unique(c(output_lines, extra_output_lines))
    }

    inp_lines <- c(
      paste0("TITLE: BCH for ", outcome_name, " (", best_model_structure, ", k=", best_k, ");"),
      "",
      paste0("DATA: FILE = ", .norm_path2(BCH_DATA_FILE), ";"),
      "",
      variable_lines,
      "",
      analysis_lines,
      "",
      model_lines,
      "",
      output_lines
    )

    write_lines_safe(inp_lines, inp_file)

    list(
      inp_file = inp_file,
      out_file = out_file
    )
  }

  # ----------------------------------------------------------
  # 9. run models
  # ----------------------------------------------------------
  BCH_RUN_REGISTRY <- list()
  BCH_UNIVARIABLE  <- list()
  BCH_POSTHOC      <- list()

  log_info("Running Mplus-native BCH models ...")

  for (i in seq_len(nrow(BCH_OUTCOME_SPEC))) {
    spec_i <- BCH_OUTCOME_SPEC[i, , drop = FALSE]
    outcome_i <- spec_i$outcome[1]
    outcome_type_i <- spec_i$outcome_type[1]

    model_tag_i <- paste0(
      tolower(DATASET_ID), "_bch_",
      best_model_structure, "_k", best_k, "_",
      make_clean_names(outcome_i)
    )

    io_i <- build_bch_input(
      outcome_name = outcome_i,
      outcome_type = outcome_type_i,
      model_tag = model_tag_i
    )

    exec_res <- NULL
    if (isTRUE(RUN_MPLUS)) {
      exec_res <- run_mplus_model(
        inp_file  = io_i$inp_file,
        mplus_exe = MPLUS_EXE,
        workdir   = dirname(io_i$inp_file),
        wait      = TRUE,
        intern    = FALSE,
        quiet     = TRUE
      )
    } else {
      exec_res <- list(ok = TRUE, status = NA_integer_, output = NULL, error = NULL)
    }

    out_path_i <- io_i$out_file
    log_info("Using BCH OUT: ", out_path_i)

    if (!file.exists(out_path_i)) {
      log_warn("BCH out file not found: ", out_path_i)
      next
    }

    lines_i  <- read_mplus_out_lines(out_path_i)
    parsed_i <- parse_bch_model_results(
      lines           = lines_i,
      outcome_name    = outcome_i,
      outcome_meta    = spec_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure
    )

    posthoc_i <- attr(parsed_i, "posthoc")

    # --------------------------------------------------
    # PATCH (2차): posthoc_i 단계에서 즉시 정리
    # --------------------------------------------------
    if (is.data.frame(posthoc_i) && nrow(posthoc_i) > 0) {

      if ("Comparison" %in% names(posthoc_i)) {
        posthoc_i$Comparison <- trimws(as.character(posthoc_i$Comparison))

        # 자기비교 제거
        posthoc_i <- posthoc_i[
          !grepl("^Class\\s*([0-9]+)\\s*vs\\s*Class\\s*\\1$", posthoc_i$Comparison),
          ,
          drop = FALSE
        ]
      }

      # var_name 없으면 보정
      if (!"var_name" %in% names(posthoc_i)) {
        cand <- intersect(c("outcome", "variable", "var", "name"), names(posthoc_i))
        if (length(cand) > 0) {
          posthoc_i$var_name <- as.character(posthoc_i[[cand[1]]])
        } else {
          posthoc_i$var_name <- "value"
        }
      }

      # 중복 제거
      key_cols <- intersect(c("Comparison", "var_name"), names(posthoc_i))
      if (length(key_cols) == 2) {
        posthoc_i <- posthoc_i[
          !duplicated(posthoc_i[, key_cols, drop = FALSE]),
          ,
          drop = FALSE
        ]
      }
    }

    if (is.data.frame(posthoc_i) && nrow(posthoc_i) > 0) {
      BCH_POSTHOC[[length(BCH_POSTHOC) + 1L]] <- posthoc_i
    }

    if (is.data.frame(parsed_i) && nrow(parsed_i) > 0) {
      parsed_i$inp_file <- io_i$inp_file
      parsed_i$out_file <- out_path_i
      BCH_UNIVARIABLE[[length(BCH_UNIVARIABLE) + 1L]] <- parsed_i
    } else {
      log_warn("BCH parse yielded no rows for: ", outcome_i)
    }

    BCH_RUN_REGISTRY[[i]] <- data.frame(
      outcome          = outcome_i,
      outcome_type     = outcome_type_i,
      best_k           = best_k,
      best_tag         = best_tag,
      model_structure  = best_model_structure,
      inp_file         = io_i$inp_file,
      out_file         = out_path_i,
      exec_ok          = isTRUE(exec_res$ok),
      exec_status      = as.character(exec_res$status %||% ""),
      exec_error       = as.character(exec_res$error %||% ""),
      stringsAsFactors = FALSE
    )
  }

  BCH_UNIVARIABLE_DF <- if (length(BCH_UNIVARIABLE) == 0) {
    empty_bch_long()
  } else {
    do.call(rbind, BCH_UNIVARIABLE)
  }
  rownames(BCH_UNIVARIABLE_DF) <- NULL

  # ----------------------------------------------------------
  # 10. standardize BCH outputs
  # ----------------------------------------------------------
  BCH_RESULTS_FULL <- ensure_df(BCH_UNIVARIABLE_DF)

  BCH_POSTHOC_DF <- if (length(BCH_POSTHOC) == 0) {
    data.frame()
  } else {
    do.call(rbind, BCH_POSTHOC)
  }

  # ==========================================================
  # PATCH: BCH posthoc self-comparison / duplicate cleanup
  # 정확한 위치: BCH_POSTHOC_DF 생성 직후, rownames(BCH_POSTHOC_DF) <- NULL 직전
  # ==========================================================
  if (is.data.frame(BCH_POSTHOC_DF) && nrow(BCH_POSTHOC_DF) > 0) {

    # 1) class_i / class_j가 없으면 Comparison에서 추출
    if (!all(c("class_i", "class_j") %in% names(BCH_POSTHOC_DF)) &&
        "Comparison" %in% names(BCH_POSTHOC_DF)) {
      m  <- regexec("^Class\\s*([0-9]+)\\s*vs\\s*Class\\s*([0-9]+)$", BCH_POSTHOC_DF$Comparison)
      mm <- regmatches(BCH_POSTHOC_DF$Comparison, m)

      BCH_POSTHOC_DF$class_i <- rep(NA_integer_, nrow(BCH_POSTHOC_DF))
      BCH_POSTHOC_DF$class_j <- rep(NA_integer_, nrow(BCH_POSTHOC_DF))

      hit <- lengths(mm) >= 3
      if (any(hit)) {
        BCH_POSTHOC_DF$class_i[hit] <- suppressWarnings(as.integer(vapply(mm[hit], `[`, character(1), 2)))
        BCH_POSTHOC_DF$class_j[hit] <- suppressWarnings(as.integer(vapply(mm[hit], `[`, character(1), 3)))
      }
    }

    # 2) 자기비교 제거
    if (all(c("class_i", "class_j") %in% names(BCH_POSTHOC_DF))) {
      BCH_POSTHOC_DF <- BCH_POSTHOC_DF[
        is.na(BCH_POSTHOC_DF$class_i) | is.na(BCH_POSTHOC_DF$class_j) |
          BCH_POSTHOC_DF$class_i != BCH_POSTHOC_DF$class_j,
        , drop = FALSE
      ]
    }

    # 3) 비교 방향 표준화: 작은 class 번호를 앞에
    if (all(c("class_i", "class_j") %in% names(BCH_POSTHOC_DF))) {
      a <- pmin(BCH_POSTHOC_DF$class_i, BCH_POSTHOC_DF$class_j)
      b <- pmax(BCH_POSTHOC_DF$class_i, BCH_POSTHOC_DF$class_j)

      ok <- !is.na(a) & !is.na(b)
      BCH_POSTHOC_DF$class_i[ok] <- a[ok]
      BCH_POSTHOC_DF$class_j[ok] <- b[ok]

      if ("Comparison" %in% names(BCH_POSTHOC_DF)) {
        BCH_POSTHOC_DF$Comparison[ok] <- paste0("Class ", a[ok], " vs Class ", b[ok])
      }
    }

    # 4) var_name 없으면 보정
    if (!"var_name" %in% names(BCH_POSTHOC_DF)) {
      cand <- intersect(c("outcome", "variable", "var", "name"), names(BCH_POSTHOC_DF))
      BCH_POSTHOC_DF$var_name <- if (length(cand) > 0) {
        as.character(BCH_POSTHOC_DF[[cand[1]]])
      } else {
        "value"
      }
    }

    # 5) 중복 제거
    dedup_key <- intersect(c("Comparison", "var_name"), names(BCH_POSTHOC_DF))
    if (length(dedup_key) == 2) {
      BCH_POSTHOC_DF <- BCH_POSTHOC_DF[
        !duplicated(BCH_POSTHOC_DF[, dedup_key, drop = FALSE]),
        , drop = FALSE
      ]
    }
  }

  rownames(BCH_POSTHOC_DF) <- NULL

  BCH_OMNIBUS_BASIC <- if (nrow(BCH_RESULTS_FULL) == 0) {
    empty_bch_omnibus_basic()
  } else {
    om <- unique(BCH_RESULTS_FULL[, c("var_name", "var_label", "stat", "df", "omnibus_p", "omnibus_p_fmt", "omnibus_sig"), drop = FALSE])
    names(om) <- c("var_name", "var_label", "chi_sq", "df", "p", "p_fmt", "sig")
    om$display_order <- match(om$var_name, OUTCOMES)
    om <- om[order(om$display_order, om$var_name), , drop = FALSE]
    om$display_order <- NULL
    rownames(om) <- NULL
    om
  }

  # ----------------------------------------------------------
  # 10B. moderator-specific BCH
  # ----------------------------------------------------------
  BCH_MOD_RESULTS_FULL <- empty_bch_long()[0, , drop = FALSE]
  BCH_MOD_POSTHOC      <- data.frame()
  BCH_MOD_OMNIBUS      <- empty_bch_omnibus_basic()

  if (length(MODERATOR_VARS) > 0) {
    for (mv in MODERATOR_VARS) {
      x_mv <- MPLUS_EXPORT_DATA[[mv]]
      lvls <- unique(stats::na.omit(x_mv))

      for (lv in lvls) {
        data_sub <- MPLUS_EXPORT_DATA[!is.na(x_mv) & x_mv == lv, , drop = FALSE]
        if (!is.data.frame(data_sub) || nrow(data_sub) < best_k * 5) next

        sub_res <- run_bch_basic_subset(
          DATA_SUB                     = data_sub,
          DICT                         = DICT,
          CFG                          = CFG,
          OUTCOMES                     = OUTCOMES,
          best_k                       = best_k,
          best_tag                     = best_tag,
          best_model_structure         = best_model_structure,
          DATASET_ID                   = DATASET_ID,
          RUN_MPLUS                    = RUN_MPLUS,
          MPLUS_EXE                    = MPLUS_EXE,
          INDICATORS                   = INDICATORS,
          INDICATORS_CATEGORICAL       = INDICATORS_CATEGORICAL,
          INDICATORS_CONTINUOUS        = INDICATORS_CONTINUOUS,
          WEIGHT_VAR                   = WEIGHT_VAR,
          STRATA_VAR                   = STRATA_VAR,
          CLUSTER_VAR                  = CLUSTER_VAR,
          ID_VAR                       = ID_VAR,
          MISSING_CODE                 = MISSING_CODE,
          MIXTURE_TYPE                 = MIXTURE_TYPE,
          DIR_MPLUS_BCH_INP            = DIR_MPLUS_BCH_INP,
          BCH_DATA_FILE                = BCH_DATA_FILE,
          BCH_DATA                     = BCH_DATA,
          build_bch_input_fun          = build_bch_input,
          parse_bch_model_results_fun  = parse_bch_model_results,
          make_bch_outcome_spec_fun    = build_bch_outcome_spec,
          read_mplus_out_lines_fun     = read_mplus_out_lines,
          run_mplus_model_fun          = run_mplus_model,
          write_mplus_data_fun         = write_mplus_data
        )

        if (is.data.frame(sub_res$BCH_RESULTS_FULL) && nrow(sub_res$BCH_RESULTS_FULL) > 0) {
          tmp                  <- sub_res$BCH_RESULTS_FULL
          tmp$moderator        <- mv
          tmp$moderator_level  <- as.character(lv)
          BCH_MOD_RESULTS_FULL <- rbind(BCH_MOD_RESULTS_FULL, tmp)
        }

        # tmp_ph 초기화
        tmp_ph <- data.frame()

        if (is.data.frame(sub_res$BCH_POSTHOC) && nrow(sub_res$BCH_POSTHOC) > 0) {

          tmp_ph <- data.frame()

          if (is.data.frame(sub_res$BCH_POSTHOC) && nrow(sub_res$BCH_POSTHOC) > 0) {
            tmp_ph                 <- sub_res$BCH_POSTHOC
            tmp_ph                 <- flatten_df(tmp_ph)
            tmp_ph$moderator       <- mv
            tmp_ph$moderator_level <- as.character(lv)
            BCH_MOD_POSTHOC        <- rbind(BCH_MOD_POSTHOC, tmp_ph)
          }
        }

        if (is.data.frame(sub_res$BCH_OMNIBUS_BASIC) && nrow(sub_res$BCH_OMNIBUS_BASIC) > 0) {
          tmp_om <- sub_res$BCH_OMNIBUS_BASIC
        } else {
          tmp_om <- data.frame()
        }

        if (!is.data.frame(tmp_om) || nrow(tmp_om) == 0 ||
            !("p" %in% names(tmp_om)) || all(is.na(tmp_om$p))) {

          tmp_full <- safe_df(sub_res$BCH_RESULTS_FULL)

          if (nrow(tmp_full) > 0 && "var_name" %in% names(tmp_full)) {
            tmp_om <- unique(
              tmp_full[, c("var_name", "var_label", "stat", "df", "omnibus_p", "omnibus_p_fmt", "omnibus_sig"), drop = FALSE]
            )
            names(tmp_om) <- c("var_name", "var_label", "chi_sq", "df", "p", "p_fmt", "sig")
            rownames(tmp_om) <- NULL
          }
        }

        if (is.data.frame(tmp_om) && nrow(tmp_om) > 0) {
          if (!"p_fmt" %in% names(tmp_om) && "p" %in% names(tmp_om)) {
            tmp_om$p_fmt <- fmt_p_tbl(tmp_om$p)
          }
          if (!"sig" %in% names(tmp_om) && "p" %in% names(tmp_om)) {
            tmp_om$sig <- p_to_sig_tbl(tmp_om$p)
          }

          tmp_om$moderator <- mv
          tmp_om$moderator_level <- as.character(lv)
          BCH_MOD_OMNIBUS <- rbind(BCH_MOD_OMNIBUS, tmp_om)
        }
      }
    }

    if (nrow(BCH_MOD_RESULTS_FULL) > 0) rownames(BCH_MOD_RESULTS_FULL) <- NULL
    if (nrow(BCH_MOD_POSTHOC) > 0) rownames(BCH_MOD_POSTHOC) <- NULL
    if (nrow(BCH_MOD_OMNIBUS) > 0) rownames(BCH_MOD_OMNIBUS) <- NULL
  }

  if (nrow(BCH_RESULTS_FULL) > 0) {
    BCH_RESULTS_FULL <- BCH_RESULTS_FULL[
      order(match(BCH_RESULTS_FULL$var_name, OUTCOMES), BCH_RESULTS_FULL$class_num),
      ,
      drop = FALSE
    ]
    rownames(BCH_RESULTS_FULL) <- NULL
  }

  if (nrow(BCH_RESULTS_FULL) == 0) {
    BCH_RESULTS <- data.frame(
      var_name         = character(0),
      label            = character(0),
      p                = numeric(0),
      display_order    = numeric(0),
      best_k           = integer(0),
      best_tag         = character(0),
      model_structure  = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    bch_wide_src <- BCH_RESULTS_FULL[
      !is.na(BCH_RESULTS_FULL$class_num),
      c("var_name", "class_num", "estimate", "se"),
      drop = FALSE
    ]

    mean_wide <- reshape(
      bch_wide_src[, c("var_name", "class_num", "estimate"), drop = FALSE],
      idvar     = "var_name",
      timevar   = "class_num",
      direction = "wide"
    )

    se_wide <- reshape(
      bch_wide_src[, c("var_name", "class_num", "se"), drop = FALSE],
      idvar     = "var_name",
      timevar   = "class_num",
      direction = "wide"
    )

    p_df <- unique(BCH_RESULTS_FULL[, c("var_name", "omnibus_p"), drop = FALSE])
    p_df <- p_df[!duplicated(p_df$var_name), , drop = FALSE]
    names(p_df)[2] <- "p"
    p_df$p <- coerce_atomic_col(p_df$p, prefer_numeric = TRUE)

    BCH_RESULTS <- merge(mean_wide, se_wide, by = "var_name", all = TRUE, sort = FALSE)
    BCH_RESULTS <- merge(BCH_RESULTS, p_df, by = "var_name", all.x = TRUE, sort = FALSE)

    names(BCH_RESULTS) <- sub("^estimate\\.", "class", names(BCH_RESULTS))
    names(BCH_RESULTS) <- sub("^se\\.", "se_class", names(BCH_RESULTS))

    BCH_RESULTS$label           <- resolve_bch_label_local(BCH_RESULTS$var_name)
    BCH_RESULTS$display_order   <- match(BCH_RESULTS$var_name, OUTCOMES)
    BCH_RESULTS$best_k          <- best_k
    BCH_RESULTS$best_tag        <- best_tag
    BCH_RESULTS$model_structure <- best_model_structure

    class_cols <- sort_class_cols_local(names(BCH_RESULTS))
    se_cols    <- sort_se_class_cols_local(names(BCH_RESULTS))

    BCH_RESULTS <- BCH_RESULTS[
      order(BCH_RESULTS$display_order, BCH_RESULTS$var_name),
      c("var_name", "label", class_cols, se_cols, "p", "display_order", "best_k", "best_tag", "model_structure"),
      drop = FALSE
    ]

    BCH_RESULTS <- sanitize_df_for_export(
      BCH_RESULTS,
      numeric_cols = c(class_cols, se_cols, "p", "display_order", "best_k")
    )

    rownames(BCH_RESULTS) <- NULL
  }

  BCH_METADATA <- data.frame(
    best_k           = best_k,
    best_tag         = best_tag,
    model_structure  = best_model_structure,
    selected_model   = best_model_structure,
    method           = "Mplus-native BCH",
    n_outcomes       = length(unique(BCH_RESULTS_FULL$var_name)),
    created_at       = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  T6_bch <- BCH_RESULTS_FULL

  # ----------------------------------------------------------
  # 11. interaction test (class × moderator)
  # ----------------------------------------------------------
  log_info("Running interaction (class × moderator) analysis ...")

  BCH_INTERACTION <- data.frame()

  if (is.data.frame(CLASS_KEY) &&
      nrow(CLASS_KEY) > 0 &&
      "id" %in% names(CLASS_KEY) &&
      "class_num" %in% names(CLASS_KEY) &&
      length(MODERATOR_VARS) > 0 &&
      "id" %in% names(MPLUS_EXPORT_DATA)) {

    dat_src <- MPLUS_EXPORT_DATA
    if (is.data.frame(RAW_DATA) && nrow(RAW_DATA) > 0) {
      need_extra <- setdiff(MODERATOR_VARS, names(dat_src))
      if ("id" %in% names(RAW_DATA) && length(need_extra) > 0) {
        raw_add <- unique(RAW_DATA[, intersect(c("id", need_extra), names(RAW_DATA)), drop = FALSE])
        dat_src <- merge(dat_src, raw_add, by = "id", all.x = TRUE)
      } else if (length(need_extra) > 0 && nrow(RAW_DATA) == nrow(dat_src)) {
        add_cols <- intersect(need_extra, names(RAW_DATA))
        if (length(add_cols) > 0) {
          dat_src[, add_cols] <- RAW_DATA[, add_cols, drop = FALSE]
        }
      }
    }

    if (!"id" %in% names(dat_src)) {
      stop("Interaction analysis requires id in exported data.", call. = FALSE)
    }

    dat <- merge(
      dat_src,
      CLASS_KEY,
      by = "id",
      all.x = TRUE
    )

    if (nrow(dat) != nrow(MPLUS_EXPORT_DATA)) {
      warning("Merged interaction dataset row count differs from MPLUS_EXPORT_DATA. Check duplicated ids.")
    }

    if (sum(!is.na(dat$class_num)) == 0) {
      warning("No matched class assignments after merge. Check id/class key.")
    }

    ref_class_chr <- if (!is.na(REFERENCE_CLASS) && nzchar(as.character(REFERENCE_CLASS))) as.character(REFERENCE_CLASS) else NA_character_
    dat$class <- factor(dat$class_num)
    if (!is.na(ref_class_chr) && ref_class_chr %in% levels(dat$class)) {
      dat$class <- stats::relevel(dat$class, ref = ref_class_chr)
    }

    outcomes <- OUTCOMES
    moderators <- MODERATOR_VARS
    moderators <- moderators[moderators %in% names(dat)]

    dict_meta_local <- if (is.list(DICT) && is.data.frame(DICT$meta)) DICT$meta else data.frame()
    moderator_is_continuous <- function(mv, x) {
      if (is.data.frame(dict_meta_local) && nrow(dict_meta_local) > 0 && "var_name" %in% names(dict_meta_local)) {
        hit <- dict_meta_local[as.character(dict_meta_local$var_name) == mv, , drop = FALSE]
        if (nrow(hit) > 0 && "type" %in% names(hit)) {
          typ <- tolower(as.character(hit$type[1]))
          if (typ %in% c("continuous", "numeric", "scale")) return(TRUE)
          if (typ %in% c("categorical", "binary", "nominal", "ordinal")) return(FALSE)
        }
      }
      x_num <- suppressWarnings(as.numeric(as.character(x)))
      n_uniq <- length(unique(stats::na.omit(x_num)))
      all(!is.na(x_num)) && n_uniq > 5
    }
    class_term_label <- function(term_name) {
      cls_num <- suppressWarnings(as.integer(gsub("^class", "", term_name)))
      if (is.na(cls_num)) return(term_name)
      ref_lab <- if (!is.na(REFERENCE_CLASS_LABEL) && nzchar(as.character(REFERENCE_CLASS_LABEL))) as.character(REFERENCE_CLASS_LABEL) else paste0("Class ", ref_class_chr)
      paste0("Class ", cls_num, " vs ", ref_lab)
    }

    for (mv in moderators) {
      is_cont_mv <- moderator_is_continuous(mv, dat[[mv]])

      for (y in outcomes) {
        if (!y %in% names(dat)) next

        if (isTRUE(is_cont_mv)) {
          tmp <- dat[, c("class", y, mv), drop = FALSE]
          names(tmp)[names(tmp) == mv] <- "moderator_value"
          tmp$moderator_value <- suppressWarnings(as.numeric(as.character(tmp$moderator_value)))
          tmp <- tmp[
            !is.na(tmp[[y]]) &
              !is.na(tmp$class) &
              is.finite(tmp$moderator_value),
            ,
            drop = FALSE
          ]
          if (nrow(tmp) == 0) next
          if (length(unique(tmp$class)) < 2) next
          if (length(unique(tmp$moderator_value)) < 2) next

          fit <- tryCatch(
            stats::lm(stats::as.formula(paste0("`", y, "` ~ class * moderator_value")), data = tmp),
            error = function(e) NULL
          )
          if (is.null(fit)) next

          drop_tab <- tryCatch(stats::drop1(fit, test = "F"), error = function(e) NULL)
          if (is.data.frame(drop_tab) && nrow(drop_tab) > 0) {
            for (rn in intersect(c("class", "moderator_value", "class:moderator_value"), rownames(drop_tab))) {
              row <- drop_tab[rn, , drop = FALSE]
              eff_lab <- if (identical(rn, "moderator_value")) mv else if (identical(rn, "class:moderator_value")) paste0("class*", mv) else "class"
              BCH_INTERACTION <- rbind(
                BCH_INTERACTION,
                data.frame(
                  row_type = "omnibus",
                  moderator = mv,
                  moderator_type = "continuous",
                  outcome   = y,
                  effect    = eff_lab,
                  term      = eff_lab,
                  estimate  = NA_real_,
                  se        = NA_real_,
                  t_value   = NA_real_,
                  llci      = NA_real_,
                  ulci      = NA_real_,
                  stat      = suppressWarnings(as.numeric(row$`F value`)),
                  df        = suppressWarnings(as.numeric(row$Df)),
                  p         = suppressWarnings(as.numeric(row$`Pr(>F)`)),
                  p_fmt     = fmt_p3_strict(suppressWarnings(as.numeric(row$`Pr(>F)`))),
                  sig       = sig_mark(suppressWarnings(as.numeric(row$`Pr(>F)`))),
                  n         = nrow(tmp),
                  stringsAsFactors = FALSE
                )
              )
            }
          }

          sm <- tryCatch(summary(fit), error = function(e) NULL)
          cf <- if (!is.null(sm)) sm$coefficients else NULL
          ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
          if (is.matrix(cf) && nrow(cf) > 0) {
            term_names <- rownames(cf)
            keep_terms <- setdiff(term_names, "(Intercept)")
            for (term_i in keep_terms) {
              eff_block <- "moderator"
              term_label <- term_i
              if (grepl("^class", term_i) && grepl(":", term_i)) {
                eff_block <- "class*moderator"
                cls_part <- sub(":.*$", "", term_i)
                term_label <- paste0(class_term_label(cls_part), " x ", get_var_label(mv))
              } else if (grepl("^class", term_i)) {
                eff_block <- "class"
                term_label <- class_term_label(term_i)
              } else if (identical(term_i, "moderator_value")) {
                eff_block <- "moderator"
                term_label <- get_var_label(mv)
              }
              BCH_INTERACTION <- rbind(
                BCH_INTERACTION,
                data.frame(
                  row_type = "coefficient",
                  moderator = mv,
                  moderator_type = "continuous",
                  outcome   = y,
                  effect    = eff_block,
                  term      = term_label,
                  estimate  = suppressWarnings(as.numeric(cf[term_i, "Estimate"])),
                  se        = suppressWarnings(as.numeric(cf[term_i, "Std. Error"])),
                  t_value   = suppressWarnings(as.numeric(cf[term_i, "t value"])),
                  llci      = if (is.matrix(ci) && term_i %in% rownames(ci)) suppressWarnings(as.numeric(ci[term_i, 1])) else NA_real_,
                  ulci      = if (is.matrix(ci) && term_i %in% rownames(ci)) suppressWarnings(as.numeric(ci[term_i, 2])) else NA_real_,
                  stat      = suppressWarnings(as.numeric(cf[term_i, "t value"])),
                  df        = fit$df.residual,
                  p         = suppressWarnings(as.numeric(cf[term_i, "Pr(>|t|)"])),
                  p_fmt     = fmt_p3_strict(suppressWarnings(as.numeric(cf[term_i, "Pr(>|t|)"]))),
                  sig       = sig_mark(suppressWarnings(as.numeric(cf[term_i, "Pr(>|t|)"]))),
                  n         = nrow(tmp),
                  stringsAsFactors = FALSE
                )
              )
            }
          }

        } else {
          tmp <- dat[
            !is.na(dat[[y]]) &
              !is.na(dat$class) &
              !is.na(dat[[mv]]),
            ,
            drop = FALSE
          ]
          tmp[[mv]] <- factor(tmp[[mv]])
          if (nrow(tmp) == 0) next
          if (length(unique(tmp$class)) < 2) next
          if (length(unique(tmp[[mv]])) < 2) next

          xtab <- table(tmp$class, tmp[[mv]])
          if (nrow(xtab) < 2 || ncol(xtab) < 2) next

          fit <- tryCatch(
            stats::lm(stats::as.formula(paste0("`", y, "` ~ class * `", mv, "`")), data = tmp),
            error = function(e) NULL
          )
          if (is.null(fit)) next

          aov_tab <- tryCatch(
            stats::anova(fit),
            error = function(e) NULL
          )
          if (is.null(aov_tab)) next

          effect_rows <- c("class", mv, paste0("class:", mv))
          effect_labels <- c("class", mv, paste0("class*", mv))

          for (ii in seq_along(effect_rows)) {
            rn <- effect_rows[ii]
            lab <- effect_labels[ii]
            if (!rn %in% rownames(aov_tab)) next
            row <- aov_tab[rn, , drop = FALSE]
            p_val <- suppressWarnings(as.numeric(row$`Pr(>F)`))
            stat  <- suppressWarnings(as.numeric(row$`F value`))

            BCH_INTERACTION <- rbind(
              BCH_INTERACTION,
              data.frame(
                row_type = "omnibus",
                moderator = mv,
                moderator_type = "categorical",
                outcome   = y,
                effect    = lab,
                term      = lab,
                estimate  = NA_real_,
                se        = NA_real_,
                t_value   = NA_real_,
                llci      = NA_real_,
                ulci      = NA_real_,
                stat      = stat,
                df        = suppressWarnings(as.numeric(row$Df)),
                p         = p_val,
                p_fmt     = fmt_p3_strict(p_val),
                sig       = sig_mark(p_val),
                n         = nrow(tmp),
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }
    }
  }

  if (nrow(BCH_INTERACTION) > 0) rownames(BCH_INTERACTION) <- NULL

  BCH_MOD_RESULTS_FULL <- flatten_df(BCH_MOD_RESULTS_FULL)
  BCH_MOD_POSTHOC      <- flatten_df(BCH_MOD_POSTHOC)
  BCH_MOD_OMNIBUS      <- flatten_df(BCH_MOD_OMNIBUS)
  BCH_RESULTS          <- sanitize_df_for_export(
    BCH_RESULTS,
    numeric_cols = c(sort_class_cols_local(names(BCH_RESULTS)),
                     sort_se_class_cols_local(names(BCH_RESULTS)),
                     "p", "display_order", "best_k")
  )
  BCH_RESULTS_FULL     <- sanitize_df_for_export(
    BCH_RESULTS_FULL,
    numeric_cols = c("best_k", "class_num", "estimate", "se", "stat", "df", "p", "omnibus_p")
  )
  BCH_POSTHOC_DF       <- sanitize_df_for_export(
    BCH_POSTHOC_DF,
    numeric_cols = c("class_i", "class_j", "estimate", "se", "stat", "df", "p")
  )
  BCH_OMNIBUS_BASIC    <- sanitize_df_for_export(
    BCH_OMNIBUS_BASIC,
    numeric_cols = c("chi_sq", "df", "p")
  )
  BCH_MOD_RESULTS_FULL <- sanitize_df_for_export(
    BCH_MOD_RESULTS_FULL,
    numeric_cols = c("best_k", "class_num", "estimate", "se", "stat", "df", "p", "omnibus_p")
  )
  BCH_MOD_POSTHOC      <- sanitize_df_for_export(
    BCH_MOD_POSTHOC,
    numeric_cols = c("class_i", "class_j", "estimate", "se", "stat", "df", "p")
  )
  BCH_MOD_OMNIBUS      <- sanitize_df_for_export(
    BCH_MOD_OMNIBUS,
    numeric_cols = c("chi_sq", "df", "p")
  )
  BCH_METADATA         <- sanitize_df_for_export(
    BCH_METADATA,
    numeric_cols = c("best_k", "n_outcomes")
  )
  BCH_INTERACTION      <- sanitize_df_for_export(
    BCH_INTERACTION,
    numeric_cols = c("estimate", "se", "t_value", "llci", "ulci", "stat", "df", "p", "n")
  )
  CLASS_KEY            <- sanitize_df_for_export(
    CLASS_KEY,
    numeric_cols = c("class_num")
  )

  check_list_cols <- function(x, nm) {
    if (!is.data.frame(x)) {
      log_info("DEBUG ", nm, " : not data.frame")
      return(invisible(NULL))
    }

    is_list_col <- vapply(x, is.list, logical(1))
    bad_cols <- names(x)[is_list_col]

    if (length(bad_cols) == 0) {
      log_info("DEBUG ", nm, " : no list cols")
    } else {
      log_warn("DEBUG ", nm, " : list cols = ", paste(bad_cols, collapse = ", "))
    }
  }

  check_list_cols(BCH_RESULTS,          "BCH_RESULTS")
  check_list_cols(BCH_RESULTS_FULL,     "BCH_RESULTS_FULL")
  check_list_cols(BCH_POSTHOC_DF,       "BCH_POSTHOC_DF")
  check_list_cols(BCH_OMNIBUS_BASIC,    "BCH_OMNIBUS_BASIC")
  check_list_cols(BCH_MOD_RESULTS_FULL, "BCH_MOD_RESULTS_FULL")
  check_list_cols(BCH_MOD_POSTHOC,      "BCH_MOD_POSTHOC")
  check_list_cols(BCH_MOD_OMNIBUS,      "BCH_MOD_OMNIBUS")
  check_list_cols(BCH_METADATA,         "BCH_METADATA")
  check_list_cols(BCH_INTERACTION,      "BCH_INTERACTION")
  check_list_cols(CLASS_KEY,            "CLASS_KEY")
  check_list_cols(BCH_RUN_REGISTRY,     "BCH_RUN_REGISTRY")

  # ----------------------------------------------------------
  # 11B. save outputs
  # ----------------------------------------------------------
  log_info("Saving BCH outputs ...")

  if (length(BCH_RUN_REGISTRY) > 0) {
    BCH_RUN_REGISTRY           <- do.call(rbind, BCH_RUN_REGISTRY)
    rownames(BCH_RUN_REGISTRY) <- NULL
  } else {
    BCH_RUN_REGISTRY           <- data.frame()
  }

  save_named_rds_list(
    list(
      BCH_DATA             = BCH_DATA,
      BCH_OUTCOME_SPEC     = BCH_OUTCOME_SPEC,
      BCH_RESULTS          = BCH_RESULTS,
      BCH_RESULTS_FULL     = BCH_RESULTS_FULL,
      BCH_POSTHOC          = BCH_POSTHOC_DF,
      BCH_OMNIBUS_BASIC    = BCH_OMNIBUS_BASIC,
      BCH_MOD_RESULTS_FULL = BCH_MOD_RESULTS_FULL,
      BCH_MOD_POSTHOC      = BCH_MOD_POSTHOC,
      BCH_MOD_OMNIBUS      = BCH_MOD_OMNIBUS,
      BCH_METADATA         = BCH_METADATA,
      BCH_INTERACTION      = BCH_INTERACTION,
      BCH_RUN_REGISTRY     = BCH_RUN_REGISTRY,
      CLASS_KEY_BCH        = CLASS_KEY,
      T6_bch               = T6_bch
    ),
    dir_rds = DIR_RDS
  )

  if (exists("DIR_TABLES") && !is.null(DIR_TABLES)) {
    write_csv_safe(BCH_RESULTS,          file.path(DIR_TABLES, "bch_results.csv"))
    write_csv_safe(BCH_RESULTS_FULL,     file.path(DIR_TABLES, "bch_results_full.csv"))
    write_csv_safe(BCH_POSTHOC_DF,       file.path(DIR_TABLES, "bch_posthoc.csv"))
    write_csv_safe(BCH_OMNIBUS_BASIC,    file.path(DIR_TABLES, "bch_omnibus_basic.csv"))
    write_csv_safe(BCH_MOD_RESULTS_FULL, file.path(DIR_TABLES, "bch_mod_results_full.csv"))
    write_csv_safe(BCH_MOD_POSTHOC,      file.path(DIR_TABLES, "bch_mod_posthoc.csv"))
    write_csv_safe(BCH_MOD_OMNIBUS,      file.path(DIR_TABLES, "bch_mod_omnibus.csv"))
    write_csv_safe(BCH_METADATA,         file.path(DIR_TABLES, "bch_metadata.csv"))
    write_csv_safe(BCH_INTERACTION,      file.path(DIR_TABLES, "bch_interaction.csv"))
    write_csv_safe(CLASS_KEY,            file.path(DIR_TABLES, "bch_class_key_used.csv"))
  }

  # ----------------------------------------------------------
  # 12. finish
  # ----------------------------------------------------------
  elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_BCH, units = "secs")), 2)

  log_info("04d_bch.R completed.")
  log_info("method          = Mplus-native BCH")
  log_info("best_k          = ", best_k)
  log_info("best_tag        = ", best_tag)
  log_info("model_structure = ", best_model_structure)
  log_info("n(outcomes)     = ", length(OUTCOMES))
  log_info("n(parsed rows)  = ", nrow(BCH_UNIVARIABLE_DF))
  log_info("n(BCH_RESULTS)  = ", nrow(BCH_RESULTS))
  log_info("n(BCH_FULL)     = ", nrow(BCH_RESULTS_FULL))
  log_info("n(BCH_INTERACTION) = ", nrow(BCH_INTERACTION))
  log_info("elapsed         = ", elapsed_sec, " sec")

  log_step_end("bch", elapsed_sec, ok = TRUE)
}
