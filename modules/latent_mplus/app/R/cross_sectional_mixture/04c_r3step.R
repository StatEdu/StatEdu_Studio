# ============================================================
# 04c_r3step.R
# Mplus automatic R3STEP covariate analysis
# ------------------------------------------------------------
# 역할
# 1) best-solution / prep / settings 산출물 재로딩
# 2) covariate를 Mplus R3STEP용으로 준비
# 3) 모든 공변량을 동시에 포함한 Mplus-native R3STEP 실행
# 4) Mplus output(.out)에서 coefficient / SE / p / RRR 파싱
# 5) T5_rrr.rds 및 R3STEP_RESULTS.rds 저장
# ============================================================

T0_R3STEP <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("R3STEP", "04c_r3step.R")
log_info("Reloading classify / best-solution / prep outputs ...")

# ------------------------------------------------------------
# 1. local helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

safe_num_local <- function(x) suppressWarnings(as.numeric(as.character(x)))

safe_chr_local <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

fmt_p_tbl <- function(p, digits = 3) {
  p <- safe_num_local(p)
  out <- rep(NA_character_, length(p))
  ok <- !is.na(p)
  out[ok] <- ifelse(p[ok] < .001, "<.001", formatC(p[ok], format = "f", digits = digits))
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

norm_space_collect <- function(x) {
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x)
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

resolve_out_file_from_inp <- function(inp_file) {
  sub("\\.inp$", ".out", inp_file, ignore.case = TRUE)
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

empty_r3step_table <- function() {
  data.frame(
    analysis          = character(0),
    model_type        = character(0),
    method            = character(0),
    best_k            = integer(0),
    best_tag          = character(0),
    model_structure   = character(0),
    predictor         = character(0),
    source_var        = character(0),
    var_name          = character(0),
    var_label         = character(0),
    level             = character(0),
    value_label       = character(0),
    predictor_type    = character(0),
    outcome_class     = character(0),
    reference_class   = character(0),
    reference_level   = character(0),
    comparison        = character(0),
    estimate          = numeric(0),
    se                = numeric(0),
    stat              = numeric(0),
    p                 = numeric(0),
    p_fmt             = character(0),
    sig               = character(0),
    rrr               = numeric(0),
    rrr_fmt           = character(0),
    llci              = numeric(0),
    llci_fmt          = character(0),
    ulci              = numeric(0),
    ulci_fmt          = character(0),
    inp_file          = character(0),
    out_file          = character(0),
    stringsAsFactors  = FALSE
  )
}

# ------------------------------------------------------------
# 2. reload objects
# ------------------------------------------------------------
BEST_K_SUMMARY <- load_step_rds("BEST_K_SUMMARY", dir_rds = DIR_RDS, default = NULL)
if (is.null(BEST_K_SUMMARY)) {
  BEST_K_SUMMARY <- load_step_rds("SELECT_BEST_K_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
}

BEST_MODEL_ROW <- load_step_rds("BEST_MODEL_ROW", dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_BUILD_SUMMARY <- load_step_rds("ESTIMATION_BUILD_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
SETTINGS_SUMMARY <- load_step_rds("SETTINGS_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
SURVEY_BUNDLE <- load_step_rds("SURVEY_BUNDLE", dir_rds = DIR_RDS, default = list())
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, default = list())
MPLUS_EXPORT_DATA <- load_step_rds("MPLUS_EXPORT_DATA", dir_rds = DIR_RDS, default = NULL)
if (is.null(MPLUS_EXPORT_DATA)) {
  MPLUS_EXPORT_DATA <- load_step_rds("MPLUS_DATA", dir_rds = DIR_RDS, required = TRUE)
}
CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)

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
  ESTIMATION_BUILD_SUMMARY$mixture_type %||%
    ESTIMATION_BUILD_SUMMARY$MIXTURE_TYPE %||%
    SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    "lpa"
)

INDICATORS <- SETTINGS_SUMMARY$indicators %||% SETTINGS_SUMMARY$INDICATORS %||% character(0)
INDICATORS_CATEGORICAL <- SETTINGS_SUMMARY$indicators_categorical %||% SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% character(0)
INDICATORS_CONTINUOUS <- SETTINGS_SUMMARY$indicators_continuous %||% SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% character(0)
COVARIATES <- SETTINGS_SUMMARY$covariates %||% SETTINGS_SUMMARY$COVARIATES %||% character(0)

INDICATORS <- intersect(unique_nz(INDICATORS), names(MPLUS_EXPORT_DATA))
INDICATORS_CATEGORICAL <- intersect(unique_nz(INDICATORS_CATEGORICAL), names(MPLUS_EXPORT_DATA))
INDICATORS_CONTINUOUS <- intersect(unique_nz(INDICATORS_CONTINUOUS), names(MPLUS_EXPORT_DATA))
COVARIATES <- intersect(unique_nz(COVARIATES), names(MPLUS_EXPORT_DATA))

WEIGHT_VAR  <- SURVEY_BUNDLE$weight_var %||% SETTINGS_SUMMARY$weight_var %||% SETTINGS_SUMMARY$WEIGHT_VAR %||% NULL
STRATA_VAR  <- SURVEY_BUNDLE$strata_var %||% SETTINGS_SUMMARY$strata_var %||% SETTINGS_SUMMARY$STRATA_VAR %||% NULL
CLUSTER_VAR <- SURVEY_BUNDLE$cluster_var %||% SETTINGS_SUMMARY$cluster_var %||% SETTINGS_SUMMARY$CLUSTER_VAR %||% NULL
ID_VAR      <- SURVEY_BUNDLE$id_var %||% SETTINGS_SUMMARY$id_var %||% SETTINGS_SUMMARY$ID_VAR %||% "id"

if (!is.null(WEIGHT_VAR)  && !WEIGHT_VAR  %in% names(MPLUS_EXPORT_DATA)) WEIGHT_VAR  <- NULL
if (!is.null(STRATA_VAR)  && !STRATA_VAR  %in% names(MPLUS_EXPORT_DATA)) STRATA_VAR  <- NULL
if (!is.null(CLUSTER_VAR) && !CLUSTER_VAR %in% names(MPLUS_EXPORT_DATA)) CLUSTER_VAR <- NULL
if (!is.null(ID_VAR)      && !ID_VAR      %in% names(MPLUS_EXPORT_DATA)) ID_VAR      <- NULL

MISSING_CODE <- ESTIMATION_BUILD_SUMMARY$missing_code %||% CFG$data$missing_code %||% CFG$missing_code %||% -9999
MPLUS_EXE <- resolve_mplus_exe(CFG, must_exist = FALSE)

if (length(COVARIATES) == 0) {
  log_info("No covariates found. Creating empty R3STEP outputs.")

  empty_rrr <- empty_r3step_table()

  R3STEP_RESULTS <- list(
    method            = "Mplus automatic R3STEP (Vermunt correction)",
    best_k            = best_k,
    best_tag          = best_tag,
    model_structure   = best_model_structure,
    inference_available = FALSE,
    unavailable_reason = "No covariates were selected for R3STEP.",
    univariable       = empty_rrr,
    multivariable     = empty_rrr,
    T5_rrr            = empty_rrr
  )

  save_named_rds_list(
    list(
      R3STEP_RESULTS = R3STEP_RESULTS,
      T5_rrr = empty_rrr
    ),
    dir_rds = DIR_RDS
  )

  elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_R3STEP, units = "secs")), 2)
  log_info("04c_r3step.R completed with empty outputs.")
  log_info("elapsed = ", elapsed_sec, " sec")
  log_step_end("r3step", elapsed_sec, ok = TRUE)
  return(invisible(NULL))
}

# ------------------------------------------------------------
# 3. dirs
# ------------------------------------------------------------
DIR_MPLUS_R3STEP <- file.path(DIR_MPLUS, "r3step")
DIR_MPLUS_R3STEP_DATA <- file.path(DIR_MPLUS_R3STEP, "data")
DIR_MPLUS_R3STEP_INP <- file.path(DIR_MPLUS_R3STEP, "inp")
DIR_MPLUS_R3STEP_OUT <- file.path(DIR_MPLUS_R3STEP, "out")
DIR_MPLUS_R3STEP_BAT <- file.path(DIR_MPLUS_R3STEP, "bat")

ensure_dir2(DIR_MPLUS_R3STEP)
ensure_dir2(DIR_MPLUS_R3STEP_DATA)
ensure_dir2(DIR_MPLUS_R3STEP_INP)
ensure_dir2(DIR_MPLUS_R3STEP_OUT)
ensure_dir2(DIR_MPLUS_R3STEP_BAT)

# ------------------------------------------------------------
# 4. dictionary helpers
# ------------------------------------------------------------
dict_meta <- get_dict_meta_tbl(DICT)
dict_levels <- get_dict_levels_tbl(DICT)

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

resolve_reference_level_local <- function(v, x = NULL) {
  if (is.data.frame(dict_levels) && nrow(dict_levels) > 0 && "reference" %in% names(dict_levels)) {
    hit <- dict_levels[dict_levels$var_name == v, , drop = FALSE]
    ref <- hit$reference[!is.na(hit$reference) & nzchar(as.character(hit$reference))]
    if (length(ref) > 0) return(as.character(ref[1]))
  }

  if (!is.null(x)) {
    ux <- unique(stats::na.omit(as.character(x)))
    if (length(ux) > 0) return(sort(ux)[1])
  }

  NULL
}

resolve_value_label_local <- function(v, lev) {
  if (is.data.frame(dict_levels) && nrow(dict_levels) > 0) {
    hit <- dict_levels[
      dict_levels$var_name == v & as.character(dict_levels$value) == as.character(lev),
      ,
      drop = FALSE
    ]
    if (nrow(hit) > 0) {
      cand_cols <- c("label", "value_label", "label_en", "label_ko")
      cand_cols <- cand_cols[cand_cols %in% names(hit)]
      for (cc in cand_cols) {
        lab <- hit[[cc]][1]
        if (!is.na(lab) && nzchar(as.character(lab))) return(as.character(lab))
      }
    }
  }
  as.character(lev)
}

# ------------------------------------------------------------
# 5. expand covariates
# ------------------------------------------------------------
expand_r3step_covariates <- function(data, covariates) {
  out_data  <- data
  spec_rows <- list()
  idx <- 1L

  for (v in covariates) {
    if (!v %in% names(out_data)) next

    x         <- out_data[[v]]
    var_label <- resolve_var_label_local(v)
    is_cat    <- FALSE

    # --------------------------------------------------
    # helper: dictionary type
    # --------------------------------------------------
    hit_meta <- dict_meta[dict_meta$var_name == v, , drop = FALSE]

    type_v <- NA_character_
    force_continuous <- FALSE
    force_categorical <- FALSE
    if (nrow(hit_meta) > 0) {
      cand_type_cols <- intersect(c("measure_type", "type"), names(hit_meta))
      if (length(cand_type_cols) > 0) {
        all_type_vals <- unique(tolower(trimws(as.character(unlist(hit_meta[, cand_type_cols, drop = FALSE], use.names = FALSE)))))
        all_type_vals <- all_type_vals[!is.na(all_type_vals) & nzchar(all_type_vals)]
        force_continuous <- any(all_type_vals %in% c("continuous", "numeric", "scale"))
        force_categorical <- any(all_type_vals %in% c("categorical", "factor", "binary", "ordinal", "nominal"))
      }
      for (cc in cand_type_cols) {
        vv <- tolower(trimws(as.character(hit_meta[[cc]][1])))
        if (!is.na(vv) && nzchar(vv)) {
          type_v <- vv
          break
        }
      }
    }

    # --------------------------------------------------
    # helper: dictionary levels existence
    # --------------------------------------------------
    has_dict_levels <- FALSE

    if (is.data.frame(dict_levels) &&
        nrow(dict_levels) > 0 &&
        "var_name" %in% names(dict_levels)) {
      has_dict_levels <- any(as.character(dict_levels$var_name) == v)
    }

    if (!has_dict_levels && nrow(hit_meta) > 0) {
      value_cols <- grep("^value_[0-9]+$", names(hit_meta), value = TRUE)
      if (length(value_cols) > 0) {
        vv <- unlist(hit_meta[1, value_cols, drop = FALSE], use.names = FALSE)
        vv <- as.character(vv)
        vv <- vv[!is.na(vv) & nzchar(trimws(vv))]
        has_dict_levels <- length(vv) > 0
      }
    }

    # --------------------------------------------------
    # normalize user-defined missing codes before type decision
    # --------------------------------------------------
    x_chr_norm <- as.character(x)
    x_chr_norm[trimws(x_chr_norm) == ""] <- NA_character_
    miss_chr <- as.character(MISSING_CODE)
    x_chr_norm[!is.na(x_chr_norm) & trimws(x_chr_norm) == miss_chr] <- NA_character_

    if (is.numeric(x) || is.integer(x)) {
      x_num_norm <- suppressWarnings(as.numeric(x))
      x_num_norm[is.finite(x_num_norm) & x_num_norm == suppressWarnings(as.numeric(MISSING_CODE))] <- NA_real_
      x <- x_num_norm
    } else {
      x <- x_chr_norm
    }

    # --------------------------------------------------
    # variable type decision
    # --------------------------------------------------
    if (force_continuous || (!is.na(type_v) && type_v %in% c("continuous", "numeric", "scale"))) {
      suppressWarnings(x <- as.numeric(as.character(x)))
      is_cat <- FALSE

    } else if (force_categorical || (!is.na(type_v) && type_v %in% c("categorical", "factor", "binary", "ordinal", "nominal"))) {
      is_cat <- TRUE

    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      is_cat <- TRUE

    } else if (is.numeric(x) || is.integer(x)) {

      ux <- unique(stats::na.omit(x))
      ux <- ux[is.finite(ux)]
      n_ux <- length(ux)
      is_integer_like <- if (n_ux == 0) FALSE else all(abs(ux - round(ux)) < 1e-8)

      # 1) explicit dictionary type 최우선
      if (FALSE) {
        is_cat <- TRUE

      } else if (FALSE) {
        is_cat <- FALSE

        # 2) type 정보가 없을 때만 heuristic
      } else {
        # numeric + dict levels라고 해도
        # 수준 수가 매우 적을 때만 categorical로 본다.
        if (has_dict_levels && is_integer_like && n_ux <= 5) {
          is_cat <- TRUE
        } else if (is_integer_like && n_ux <= 2) {
          is_cat <- TRUE
        } else {
          is_cat <- FALSE
        }
      }

    } else {
      is_cat <- FALSE
    }

    # --------------------------------------------------
    # continuous
    # --------------------------------------------------
    if (!is_cat) {
      out_data[[v]] <- suppressWarnings(as.numeric(x))

      spec_rows[[idx]] <- data.frame(
        predictor        = v,
        source_var       = v,
        var_name         = v,
        var_label        = var_label,
        level            = NA_character_,
        value_label      = NA_character_,
        predictor_type   = "continuous",
        reference_level  = NA_character_,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
      next
    }

    # --------------------------------------------------
    # categorical / dummy expansion
    # --------------------------------------------------
    x_chr <- as.character(x)
    x_chr[trimws(x_chr) == ""] <- NA_character_
    x_chr[!is.na(x_chr) & trimws(x_chr) == miss_chr] <- NA_character_

    levs <- sort(unique(stats::na.omit(x_chr)))
    if (length(levs) == 0) next

    ref <- resolve_reference_level_local(v, x = x_chr)
    if (!is.null(ref)) ref <- as.character(ref)

    if (!is.null(ref) && nzchar(ref) && ref %in% levs) {
      levs <- c(ref, setdiff(levs, ref))
    } else {
      ref <- levs[1]
    }

    # binary -> one dummy
    if (length(levs) == 2) {
      lev1 <- setdiff(levs, ref)[1]
      new_name <- paste0(v, "__", make_clean_names(lev1))

      out_data[[new_name]] <- ifelse(
        is.na(x_chr), NA_real_,
        ifelse(x_chr == lev1, 1, 0)
      )

      spec_rows[[idx]] <- data.frame(
        predictor        = new_name,
        source_var       = v,
        var_name         = v,
        var_label        = var_label,
        level            = as.character(lev1),
        value_label      = resolve_value_label_local(v, lev1),
        predictor_type   = "binary_dummy",
        reference_level  = as.character(ref),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L

    } else {
      # multi-category -> one dummy per non-reference level
      nonref <- setdiff(levs, ref)

      for (lev in nonref) {
        new_name <- paste0(v, "__", make_clean_names(lev))

        out_data[[new_name]] <- ifelse(
          is.na(x_chr), NA_real_,
          ifelse(x_chr == lev, 1, 0)
        )

        spec_rows[[idx]] <- data.frame(
          predictor        = new_name,
          source_var       = v,
          var_name         = v,
          var_label        = var_label,
          level            = as.character(lev),
          value_label      = resolve_value_label_local(v, lev),
          predictor_type   = "multi_dummy",
          reference_level  = as.character(ref),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  spec_df <- if (length(spec_rows) == 0) {
    data.frame()
  } else {
    do.call(rbind, spec_rows)
  }

  if (is.data.frame(spec_df) && nrow(spec_df) > 0) {
    rownames(spec_df) <- NULL
  }

  list(
    data = out_data,
    spec = spec_df
  )
}

log_info("Preparing covariates for joint Mplus automatic R3STEP analysis ...")
exp_obj <- expand_r3step_covariates(MPLUS_EXPORT_DATA, COVARIATES)

continuous_covariates <- character(0)
if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && "var_name" %in% names(dict_meta)) {
  type_cols <- intersect(c("measure_type", "type"), names(dict_meta))
  if (length(type_cols) > 0) {
    type_mat <- lapply(type_cols, function(cc) tolower(trimws(as.character(dict_meta[[cc]]))))
    is_cont <- Reduce(`|`, lapply(type_mat, function(vv) vv %in% c("continuous", "numeric", "scale")))
    is_cat  <- Reduce(`|`, lapply(type_mat, function(vv) vv %in% c("categorical", "factor", "binary", "ordinal", "nominal")))
    continuous_covariates <- unique(as.character(dict_meta$var_name[is_cont & !is_cat]))
  }
}
continuous_covariates <- intersect(unique_nz(continuous_covariates), COVARIATES)

if (length(continuous_covariates) > 0) {
  spec_now <- safe_df(exp_obj$spec)
  data_now <- as.data.frame(exp_obj$data, stringsAsFactors = FALSE)

  for (v in continuous_covariates) {
    if (!v %in% names(MPLUS_EXPORT_DATA)) next

    var_rows <- which(spec_now$var_name == v)
    if (length(var_rows) == 0) next

    needs_override <- any(spec_now$predictor_type[var_rows] != "continuous", na.rm = TRUE)
    if (!needs_override) next

    dummy_preds <- intersect(as.character(spec_now$predictor[var_rows]), names(data_now))
    dummy_preds <- setdiff(dummy_preds, v)
    if (length(dummy_preds) > 0) data_now[dummy_preds] <- NULL

    data_now[[v]] <- suppressWarnings(as.numeric(as.character(MPLUS_EXPORT_DATA[[v]])))

    keep_rows <- setdiff(seq_len(nrow(spec_now)), var_rows)
    spec_now <- spec_now[keep_rows, , drop = FALSE]
    spec_now <- rbind(
      spec_now,
      data.frame(
        predictor = v,
        source_var = v,
        var_name = v,
        var_label = resolve_var_label_local(v),
        level = NA_character_,
        value_label = NA_character_,
        predictor_type = "continuous",
        reference_level = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }

  exp_obj$data <- data_now
  exp_obj$spec <- spec_now
}

R3STEP_DATA <- exp_obj$data
R3STEP_SPEC <- exp_obj$spec

continuous_covariates_final <- character(0)
if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && "var_name" %in% names(dict_meta)) {
  type_cols_final <- intersect(c("type", "measure_type"), names(dict_meta))
  if (length(type_cols_final) > 0) {
    type_mat_final <- lapply(type_cols_final, function(cc) tolower(trimws(as.character(dict_meta[[cc]]))))
    is_cont_final <- Reduce(`|`, lapply(type_mat_final, function(vv) vv %in% c("continuous", "numeric", "scale")))
    is_cat_final  <- Reduce(`|`, lapply(type_mat_final, function(vv) vv %in% c("categorical", "factor", "binary", "ordinal", "nominal")))
    continuous_covariates_final <- c(
      continuous_covariates_final,
      as.character(dict_meta$var_name[is_cont_final & !is_cat_final])
    )
  }
}
continuous_covariates_final <- intersect(unique_nz(c(continuous_covariates, continuous_covariates_final)), COVARIATES)

if (length(continuous_covariates_final) > 0 && is.data.frame(R3STEP_SPEC) && nrow(R3STEP_SPEC) > 0) {
  keep_idx <- rep(TRUE, nrow(R3STEP_SPEC))
  add_rows <- list()

  for (v in continuous_covariates_final) {
    hit <- which(as.character(R3STEP_SPEC$var_name) == v)
    if (length(hit) == 0) next

    keep_idx[hit] <- FALSE
    add_rows[[length(add_rows) + 1L]] <- data.frame(
      predictor = v,
      source_var = v,
      var_name = v,
      var_label = resolve_var_label_local(v),
      level = NA_character_,
      value_label = NA_character_,
      predictor_type = "continuous",
      reference_level = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  R3STEP_SPEC <- R3STEP_SPEC[keep_idx, , drop = FALSE]
  if (length(add_rows) > 0) {
    R3STEP_SPEC <- rbind(R3STEP_SPEC, do.call(rbind, add_rows))
  }
  rownames(R3STEP_SPEC) <- NULL
}

if (!is.data.frame(R3STEP_SPEC) || nrow(R3STEP_SPEC) == 0) {
  stop("R3STEP_SPEC is empty after covariate expansion.", call. = FALSE)
}

for (nm in names(R3STEP_DATA)) {
  x <- R3STEP_DATA[[nm]]
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) x <- as.integer(x)
  if (is.character(x)) suppressWarnings(x <- as.numeric(x))
  if (!is.numeric(x)) suppressWarnings(x <- as.numeric(x))
  x[is.na(x)] <- MISSING_CODE
  R3STEP_DATA[[nm]] <- x
}

R3STEP_DATA_FILE <- file.path(DIR_MPLUS_R3STEP_DATA, paste0(tolower(DATASET_ID), "_r3step_data.dat"))
R3STEP_HEADER_FILE <- file.path(DIR_MPLUS_R3STEP_DATA, paste0(tolower(DATASET_ID), "_r3step_header.txt"))

write_mplus_data(R3STEP_DATA, R3STEP_DATA_FILE, missing_code = MISSING_CODE)
write_mplus_header(names(R3STEP_DATA), R3STEP_HEADER_FILE)

# ------------------------------------------------------------
# 6. build the single joint native R3STEP data/input
# ------------------------------------------------------------
reserved_names <- unique(c(
  INDICATORS,
  WEIGHT_VAR,
  STRATA_VAR,
  CLUSTER_VAR,
  ID_VAR
))
reserved_names <- reserved_names[!is.na(reserved_names) & nzchar(reserved_names)]

R3STEP_SPEC <- r3step_make_alias_map(
  R3STEP_SPEC,
  reserved = reserved_names,
  prefix = "R3X"
)

R3STEP_EXPORT_DATA <- R3STEP_DATA[, FALSE, drop = FALSE]
for (nm in INDICATORS) {
  R3STEP_EXPORT_DATA[[nm]] <- R3STEP_DATA[[nm]]
}
for (i in seq_len(nrow(R3STEP_SPEC))) {
  predictor_i <- as.character(R3STEP_SPEC$predictor[i])
  alias_i <- as.character(R3STEP_SPEC$mplus_alias[i])
  if (!predictor_i %in% names(R3STEP_DATA)) {
    stop("Expanded R3STEP predictor is missing from export data: ", predictor_i, call. = FALSE)
  }
  R3STEP_EXPORT_DATA[[alias_i]] <- R3STEP_DATA[[predictor_i]]
}
for (nm in setdiff(reserved_names, INDICATORS)) {
  R3STEP_EXPORT_DATA[[nm]] <- R3STEP_DATA[[nm]]
}

write_mplus_data(R3STEP_EXPORT_DATA, R3STEP_DATA_FILE, missing_code = MISSING_CODE)
write_mplus_header(names(R3STEP_EXPORT_DATA), R3STEP_HEADER_FILE)

STARTS <- CFG$estimation$starts %||% CFG$mplus$starts %||% "500 100"
STITERATIONS <- as.integer(CFG$estimation$stiterations %||% CFG$mplus$stiterations %||% 20L)
PROCESSORS <- as.integer(CFG$estimation$processors %||% CFG$mplus$processors %||% 4L)
ESTIMATOR <- toupper(CFG$estimation$estimator %||% CFG$mplus$estimator %||% "MLR")
LRT_STARTS <- CFG$estimation$lrtstarts %||% CFG$estimation$lrt_starts %||% "0 0 200 40"

requested_reference <- load_step_rds(
  "REFERENCE_CLASS",
  dir_rds = DIR_RDS,
  default = CFG$three_step$reference_class %||%
    SETTINGS_SUMMARY$three_step_reference_class %||%
    best_k
)
requested_reference <- suppressWarnings(as.integer(requested_reference)[1])
if (!is.finite(requested_reference) ||
    requested_reference < 1L ||
    requested_reference > best_k) {
  requested_reference <- as.integer(best_k)
}

model_tag_safe <- gsub("[^A-Za-z0-9_-]+", "_", paste0(best_tag, "_native_r3step"))
R3STEP_INP_FILE <- file.path(DIR_MPLUS_R3STEP_INP, paste0(model_tag_safe, ".inp"))
R3STEP_OUT_FILE <- resolve_out_file_from_inp(R3STEP_INP_FILE)
R3STEP_LOG_FILE <- sub("\\.inp$", ".log", R3STEP_INP_FILE, ignore.case = TRUE)

output_cfg <- CFG$mplus$output %||% list()
output_options <- character(0)
if (isTRUE(output_cfg$sampstat %||% FALSE)) output_options <- c(output_options, "SAMPSTAT")
if (isTRUE(output_cfg$tech1 %||% FALSE)) output_options <- c(output_options, "TECH1")
if (isTRUE(output_cfg$tech4 %||% FALSE)) output_options <- c(output_options, "TECH4")
if (isTRUE(output_cfg$tech8 %||% FALSE)) output_options <- c(output_options, "TECH8")

R3STEP_INPUT_LINES <- r3step_build_input_lines(
  title = paste0(
    "Native joint R3STEP for ", best_tag,
    " (", best_model_structure, ", k=", best_k, ")"
  ),
  data_file = normalizePath(R3STEP_DATA_FILE, winslash = "/", mustWork = FALSE),
  data_names = names(R3STEP_EXPORT_DATA),
  indicators = INDICATORS,
  predictor_aliases = R3STEP_SPEC$mplus_alias,
  categorical = INDICATORS_CATEGORICAL,
  best_k = best_k,
  mixture_type = MIXTURE_TYPE,
  model_structure = best_model_structure,
  indicators_continuous = INDICATORS_CONTINUOUS,
  indicators_categorical = INDICATORS_CATEGORICAL,
  missing_code = MISSING_CODE,
  estimator = ESTIMATOR,
  starts = STARTS,
  stiterations = STITERATIONS,
  processors = PROCESSORS,
  lrtstarts = LRT_STARTS,
  id_var = ID_VAR,
  weight_var = WEIGHT_VAR,
  strata_var = STRATA_VAR,
  cluster_var = CLUSTER_VAR,
  output_options = output_options
)
write_lines_safe(R3STEP_INPUT_LINES, R3STEP_INP_FILE)

# ------------------------------------------------------------
# 7. execute Mplus and parse only the official R3STEP block
# ------------------------------------------------------------
R3STEP_METHOD <- "Mplus automatic R3STEP (Vermunt correction)"
R3STEP_MODEL_TYPE <- "mplus_native_r3step"

if (file.exists(R3STEP_OUT_FILE)) unlink(R3STEP_OUT_FILE, force = TRUE)
if (file.exists(R3STEP_LOG_FILE)) unlink(R3STEP_LOG_FILE, force = TRUE)

mplus_available <- is.character(MPLUS_EXE) &&
  length(MPLUS_EXE) == 1L &&
  !is.na(MPLUS_EXE) &&
  nzchar(MPLUS_EXE) &&
  file.exists(MPLUS_EXE)

run_result <- if (mplus_available) {
  run_mplus_model(
    inp_file = R3STEP_INP_FILE,
    mplus_exe = MPLUS_EXE,
    workdir = dirname(R3STEP_INP_FILE),
    wait = TRUE,
    quiet = TRUE
  )
} else {
  list(
    ok = FALSE,
    status = NA_integer_,
    error = "Mplus executable not found",
    out_file = R3STEP_OUT_FILE,
    log_file = R3STEP_LOG_FILE
  )
}

parse_result <- list(
  table = r3step_empty_table(),
  inference_available = FALSE,
  reason = if (mplus_available) {
    "Mplus R3STEP execution did not complete successfully."
  } else {
    "Mplus is required for automatic R3STEP; no modal-class regression fallback was used."
  },
  heading_found = FALSE,
  reference_class = NA_integer_
)

if (isTRUE(run_result$ok) && file.exists(R3STEP_OUT_FILE)) {
  output_lines <- readLines(
    R3STEP_OUT_FILE,
    warn = FALSE,
    encoding = "UTF-8",
    skipNul = TRUE
  )
  parse_result <- r3step_parse_native_output(
    lines = output_lines,
    spec = R3STEP_SPEC,
    best_k = best_k,
    best_tag = best_tag,
    model_structure = best_model_structure,
    inp_file = R3STEP_INP_FILE,
    out_file = R3STEP_OUT_FILE,
    reference_class = requested_reference,
    require_normal_termination = TRUE
  )
} else if (isTRUE(run_result$ok) && !file.exists(R3STEP_OUT_FILE)) {
  parse_result$reason <- "Mplus finished without creating the expected R3STEP output file."
} else if (!is.null(run_result$error) && nzchar(as.character(run_result$error))) {
  parse_result$reason <- paste0("Mplus R3STEP execution failed: ", as.character(run_result$error))
}

R3_MULTIVARIABLE_DF <- if (isTRUE(parse_result$inference_available)) {
  as.data.frame(parse_result$table, stringsAsFactors = FALSE)
} else {
  r3step_empty_table()
}
R3_UNIVARIABLE_DF <- r3step_empty_table()
T5_rrr <- R3_MULTIVARIABLE_DF

if (nrow(T5_rrr) > 0L) {
  ord_var <- match(T5_rrr$var_name, COVARIATES)
  ord_var[is.na(ord_var)] <- length(COVARIATES) + seq_len(sum(is.na(ord_var)))
  ord_class <- suppressWarnings(as.integer(gsub("[^0-9]", "", T5_rrr$outcome_class)))
  ord_level <- as.character(T5_rrr$level)
  T5_rrr <- T5_rrr[order(ord_class, ord_var, ord_level), , drop = FALSE]
  rownames(T5_rrr) <- NULL
}

unavailable_reason <- as.character(parse_result$reason %||% NA_character_)[1]
if (isTRUE(parse_result$inference_available)) unavailable_reason <- NA_character_

R3STEP_RUN_REGISTRY <- list(list(
  model_tag = model_tag_safe,
  best_k = as.integer(best_k),
  best_tag = as.character(best_tag),
  model_structure = as.character(best_model_structure),
  model_type = R3STEP_MODEL_TYPE,
  method = R3STEP_METHOD,
  joint_predictors = nrow(R3STEP_SPEC),
  requested_reference_class = requested_reference,
  reported_reference_class = suppressWarnings(as.integer(parse_result$reference_class)[1]),
  inp_file = R3STEP_INP_FILE,
  out_file = R3STEP_OUT_FILE,
  mplus_available = mplus_available,
  run_ok = isTRUE(run_result$ok),
  inference_available = isTRUE(parse_result$inference_available),
  unavailable_reason = unavailable_reason
))

R3STEP_PARSE_DEBUG_DF <- data.frame(
  model_tag = model_tag_safe,
  best_k = as.integer(best_k),
  best_tag = as.character(best_tag),
  model_structure = as.character(best_model_structure),
  requested_reference_class = requested_reference,
  reported_reference_class = suppressWarnings(as.integer(parse_result$reference_class)[1]),
  heading_found = isTRUE(parse_result$heading_found),
  run_ok = isTRUE(run_result$ok),
  inference_available = isTRUE(parse_result$inference_available),
  unavailable_reason = unavailable_reason,
  stringsAsFactors = FALSE
)

R3STEP_RESULTS <- list(
  method = R3STEP_METHOD,
  model_type = R3STEP_MODEL_TYPE,
  best_k = best_k,
  best_tag = best_tag,
  model_structure = best_model_structure,
  covariates_original = COVARIATES,
  predictors_expanded = R3STEP_SPEC,
  run_registry = R3STEP_RUN_REGISTRY,
  requested_reference_class = requested_reference,
  reported_reference_class = suppressWarnings(as.integer(parse_result$reference_class)[1]),
  inference_available = isTRUE(parse_result$inference_available),
  unavailable_reason = unavailable_reason,
  univariable = R3_UNIVARIABLE_DF,
  multivariable = R3_MULTIVARIABLE_DF,
  T5_rrr = T5_rrr
)

# ------------------------------------------------------------
# 8. save
# ------------------------------------------------------------
log_info("Saving native R3STEP outputs ...")
save_named_rds_list(
  list(
    R3STEP_DATA = R3STEP_DATA,
    R3STEP_EXPORT_DATA = R3STEP_EXPORT_DATA,
    R3STEP_SPEC = R3STEP_SPEC,
    R3STEP_INPUT_LINES = R3STEP_INPUT_LINES,
    R3STEP_RESULTS = R3STEP_RESULTS,
    R3STEP_PARSE_DEBUG = R3STEP_PARSE_DEBUG_DF,
    T5_rrr = T5_rrr
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_R3STEP, units = "secs")), 2)

log_info("04c_r3step.R completed.")
log_info("method            = ", R3STEP_METHOD)
log_info("best_k            = ", best_k)
log_info("best_tag          = ", best_tag)
log_info("model_structure   = ", best_model_structure)
log_info("reference_class   = Class ", parse_result$reference_class %||% requested_reference)
log_info("n(covariates)     = ", length(COVARIATES))
log_info("n(predictors)     = ", nrow(R3STEP_SPEC))
log_info("n(multivariable)  = ", nrow(R3_MULTIVARIABLE_DF))
log_info("inference ready   = ", isTRUE(parse_result$inference_available))
if (!isTRUE(parse_result$inference_available)) {
  log_warn("R3STEP unavailable: ", unavailable_reason)
}
log_info("elapsed           = ", elapsed_sec, " sec")

log_step_end("r3step", elapsed_sec, ok = TRUE)
