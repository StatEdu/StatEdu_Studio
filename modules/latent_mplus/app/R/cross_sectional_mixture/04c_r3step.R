# ============================================================
# 04c_r3step.R
# Manual 3-step multinomial covariate analysis
# ------------------------------------------------------------
# 역할
# 1) best-solution / prep / settings 산출물 재로딩
# 2) covariate를 Mplus R3STEP용으로 준비
# 3) 각 predictor별 Mplus-native R3STEP input 생성 및 실행
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

WEIGHT_VAR  <- SETTINGS_SUMMARY$weight_var %||% SETTINGS_SUMMARY$WEIGHT_VAR %||% NULL
STRATA_VAR  <- SETTINGS_SUMMARY$strata_var %||% SETTINGS_SUMMARY$STRATA_VAR %||% NULL
CLUSTER_VAR <- SETTINGS_SUMMARY$cluster_var %||% SETTINGS_SUMMARY$CLUSTER_VAR %||% NULL
ID_VAR      <- SETTINGS_SUMMARY$id_var %||% SETTINGS_SUMMARY$ID_VAR %||% "id"

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
    method            = "Manual 3-step multinomial (R)",
    best_k            = best_k,
    best_tag          = best_tag,
    model_structure   = best_model_structure,
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

log_info("Preparing covariates for manual 3-step multinomial analysis ...")
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
# 6. input builder
# ------------------------------------------------------------
build_r3step_input <- function(predictor_name, model_tag) {
  inp_file <- file.path(DIR_MPLUS_R3STEP_INP, paste0(model_tag, ".inp"))
  out_file <- resolve_out_file_from_inp(inp_file)

  usevars <- unique(c(INDICATORS, predictor_name, WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR, ID_VAR))
  usevars <- usevars[!is.na(usevars) & nzchar(usevars)]
  usevars <- intersect(usevars, names(R3STEP_DATA))

  cat_decl <- character(0)
  if (MIXTURE_TYPE == "lca" && length(INDICATORS) > 0) {
    cat_decl <- INDICATORS
  } else if (MIXTURE_TYPE %in% c("lpa", "mixed") && length(INDICATORS_CATEGORICAL) > 0) {
    cat_decl <- INDICATORS_CATEGORICAL
  }
  cat_decl <- intersect(unique(cat_decl), names(R3STEP_DATA))

  variable_lines <- c(
    "VARIABLE:",
    mplus_wrap_statement("NAMES", names(R3STEP_DATA)),
    mplus_wrap_statement("USEVARIABLES", usevars)
  )

  if (length(cat_decl) > 0) {
    variable_lines <- c(variable_lines, mplus_wrap_statement("CATEGORICAL", cat_decl))
  }

  if (!is.null(ID_VAR) && nzchar(ID_VAR) && ID_VAR %in% names(R3STEP_DATA)) {
    variable_lines <- c(variable_lines, paste0("  IDVARIABLE = ", ID_VAR, ";"))
  }

  variable_lines <- c(
    variable_lines,
    paste0("  MISSING = ALL (", MISSING_CODE, ");"),
    paste0("  CLASSES = c(", best_k, ");"),
    build_survey_variable_lines(WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR),
    paste0("  AUXILIARY = ", predictor_name, " (R3STEP);")
  )

  analysis_lines <- c(
    "ANALYSIS:",
    make_mplus_type_line(WEIGHT_VAR, STRATA_VAR, CLUSTER_VAR),
    paste0("  ESTIMATOR = ", toupper(CFG$mplus$estimator %||% CFG$estimation$estimator %||% "MLR"), ";"),
    paste0("  STARTS = ", as.integer(CFG$mplus$starts %||% 500L), " ", as.integer(CFG$mplus$starts %||% 500L), ";"),
    paste0("  STITERATIONS = ", as.integer(CFG$mplus$stiterations %||% 20L), ";"),
    paste0("  PROCESSORS = ", as.integer(CFG$mplus$processors %||% 2L), ";")
  )

  if (isTRUE(MIXTURE_TYPE %in% c("lpa", "mixed"))) {
    cont_inds <- INDICATORS_CONTINUOUS
    if (length(cont_inds) == 0) cont_inds <- INDICATORS
    cont_inds <- intersect(unique(cont_inds), names(R3STEP_DATA))

    model_lines <- c("MODEL:", "  %OVERALL%")
    if (length(cont_inds) > 0) {
      model_lines <- c(
        model_lines,
        paste0("    [", paste(cont_inds, collapse = " "), "];"),
        paste0("    ", paste(cont_inds, collapse = " "), ";")
      )
    }
  } else {
    model_lines <- c("MODEL:", "  %OVERALL%")
  }

  output_lines <- make_output_lines_mplus(CFG)

  inp_lines <- c(
    paste0("TITLE: R3STEP for ", predictor_name, " (", best_model_structure, ", k=", best_k, ");"),
    "",
    paste0("DATA: FILE = ", .norm_path2(R3STEP_DATA_FILE), ";"),
    "",
    variable_lines,
    "",
    analysis_lines,
    "",
    model_lines
  )

  if (length(output_lines) > 0) {
    inp_lines <- c(inp_lines, "", output_lines)
  }

  write_lines_safe(inp_lines, inp_file)

  list(inp_file = inp_file, out_file = out_file)
}

# ------------------------------------------------------------
# 7. parser
# ------------------------------------------------------------
parse_r3step_model_results <- function(
    lines,
    predictor_name = NA,
    predictor_meta = NULL,
    best_k = NA_integer_,
    best_tag = NA_character_,
    model_structure = NA_character_
) {

  out_empty <- empty_r3step_table()
  results <- list()

  if (length(lines) == 0) return(out_empty)

  lines2 <- vapply(lines, norm_space_collect, character(1))

  # 🔥 predictor 이름 정리
  base_var <- toupper(gsub("__.*$", "", predictor_name))

  # ----------------------------------------------------------
  # 1. predictor 포함된 라인 찾기
  # ----------------------------------------------------------
  target_lines <- lines2[
    grepl(base_var, toupper(lines2), fixed = TRUE)
  ]

  if (length(target_lines) == 0) return(out_empty)

  for (ln in target_lines) {

    parts <- unlist(strsplit(trimws(ln), "\\s+"))
    if (length(parts) < 3) next

    # 숫자 추출
    nums <- suppressWarnings(as.numeric(gsub("D", "E", parts, fixed = TRUE)))
    nums <- nums[!is.na(nums)]

    if (length(nums) < 2) next

    est <- nums[1]
    se  <- nums[2]

    if (is.na(est) || is.na(se) || se == 0) next

    stat_val <- est / se

    p_val <- if (length(nums) >= 4) nums[4] else 2 * (1 - pnorm(abs(stat_val)))

    rrr  <- exp(est)
    llci <- exp(est - 1.96 * se)
    ulci <- exp(est + 1.96 * se)

    results[[length(results)+1]] <- data.frame(
      analysis         = "univariable",
      model_type       = "manual_3step_r_multinom",
      method           = "Manual 3-step multinomial (R)",
      best_k           = best_k,
      best_tag         = best_tag,
      model_structure  = model_structure,
      predictor        = predictor_name,
      source_var       = predictor_name,
      var_name         = predictor_name,
      var_label        = predictor_name,
      level            = NA_character_,
      value_label      = NA_character_,
      predictor_type   = NA_character_,
      outcome_class    = NA_character_,
      reference_class  = "Class 1",
      comparison       = NA_character_,
      estimate         = est,
      se               = se,
      stat             = stat_val,
      p                = p_val,
      p_fmt            = fmt_p_tbl(p_val),
      sig              = p_to_sig_tbl(p_val),
      rrr              = rrr,
      rrr_fmt          = formatC(rrr, format = "f", digits = 3),
      llci             = llci,
      llci_fmt         = formatC(llci, format = "f", digits = 3),
      ulci             = ulci,
      ulci_fmt         = formatC(ulci, format = "f", digits = 3),
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0) return(out_empty)

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

# ----------------------------------------------------------
# resolve reference class
# ----------------------------------------------------------
three_step_reference_class <- SETTINGS_SUMMARY$three_step_reference_class %||%
  SETTINGS_SUMMARY$THREE_STEP_REFERENCE_CLASS %||%
  CFG$three_step$reference_class %||%
  NULL

resolve_reference_class_local <- function(class_summary_df, ref_class_cfg = NULL) {
  cls <- as.data.frame(class_summary_df, stringsAsFactors = FALSE)

  if (!is.data.frame(cls) || nrow(cls) == 0) {
    return(1L)
  }

  class_col <- c("class_num", "class", "Class")
  class_col <- class_col[class_col %in% names(cls)][1]
  if (is.na(class_col)) return(1L)

  prop_col <- c("estimated_prop", "weighted_prop", "prop", "proportion", "pct", "Percent")
  prop_col <- prop_col[prop_col %in% names(cls)][1]
  if (is.na(prop_col)) return(1L)

  if (identical(class_col, "class_num")) {
    class_num <- suppressWarnings(as.integer(cls[[class_col]]))
  } else {
    class_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(cls[[class_col]]))))
  }

  prop_num <- suppressWarnings(as.numeric(cls[[prop_col]]))

  ok <- !is.na(class_num) & !is.na(prop_num)
  if (!any(ok)) return(1L)

  class_num <- class_num[ok]
  prop_num  <- prop_num[ok]

  if (max(prop_num, na.rm = TRUE) > 1) {
    prop_num <- prop_num / 100
  }

  ref_cfg_num <- suppressWarnings(as.integer(ref_class_cfg))
  ref_cfg_num <- ref_cfg_num[!is.na(ref_cfg_num)]

  if (length(ref_cfg_num) >= 1 && ref_cfg_num[1] %in% class_num) {
    return(ref_cfg_num[1])
  }

  return(class_num[which.max(prop_num)])
}

CLASS_SUMMARY_FINAL   <- load_step_rds(
  "CLASS_SUMMARY_FINAL",
  dir_rds = DIR_RDS,
  default = data.frame()
)

REFERENCE_CLASS       <- load_step_rds(
  "REFERENCE_CLASS",
  dir_rds = DIR_RDS,
  default = NA_integer_
)

REFERENCE_CLASS_LABEL <- load_step_rds(
  "REFERENCE_CLASS_LABEL",
  dir_rds = DIR_RDS,
  default = NA_character_
)

if (is.na(REFERENCE_CLASS) || !nzchar(as.character(REFERENCE_CLASS_LABEL))) {
  if (is.data.frame(CLASS_SUMMARY_FINAL) && nrow(CLASS_SUMMARY_FINAL) > 0) {
    if ("reference_class" %in% names(CLASS_SUMMARY_FINAL)) {
      tmp_ref <- unique(CLASS_SUMMARY_FINAL$reference_class)
      tmp_ref <- tmp_ref[!is.na(tmp_ref)]
      if (length(tmp_ref) > 0) REFERENCE_CLASS <- as.integer(tmp_ref[1])
    }
    if ("reference_class_label" %in% names(CLASS_SUMMARY_FINAL)) {
      tmp_lab <- unique(CLASS_SUMMARY_FINAL$reference_class_label)
      tmp_lab <- tmp_lab[!is.na(tmp_lab) & nzchar(tmp_lab)]
      if (length(tmp_lab) > 0) REFERENCE_CLASS_LABEL <- as.character(tmp_lab[1])
    }
  }
}

if (is.na(REFERENCE_CLASS)) {
  stop("REFERENCE_CLASS could not be resolved from classify outputs.")
}

if (is.na(REFERENCE_CLASS_LABEL) || !nzchar(REFERENCE_CLASS_LABEL)) {
  REFERENCE_CLASS_LABEL <- paste0("Class ", REFERENCE_CLASS)
}

log_info("REFERENCE_CLASS       = ", REFERENCE_CLASS)
log_info("REFERENCE_CLASS_LABEL = ", REFERENCE_CLASS_LABEL)


# ------------------------------------------------------------
# 8. run models
# ------------------------------------------------------------
R3STEP_RUN_REGISTRY <- list()
R3STEP_PARSE_DEBUG <- list()
R3_UNIVARIABLE <- list()

log_info("Running R-based manual 3-step multinomial models ...")

CLASSIFIED_ANALYSIS <- load_step_rds(
  "CLASSIFIED_ANALYSIS",
  dir_rds = DIR_RDS,
  required = TRUE
)

if (!is.data.frame(CLASSIFIED_ANALYSIS) || nrow(CLASSIFIED_ANALYSIS) == 0) {
  stop("CLASSIFIED_ANALYSIS is missing or empty.", call. = FALSE)
}
if (!"class_num" %in% names(CLASSIFIED_ANALYSIS)) {
  stop("CLASSIFIED_ANALYSIS must contain 'class_num'.", call. = FALSE)
}

# ----------------------------------------------------------
# helper: predictor 분석용 데이터셋 만들기
# ----------------------------------------------------------
build_r3step_analysis_df <- function(
    classified_df,
    expanded_df,
    predictor_name,
    id_var = NULL,
    weight_var = NULL
) {
  cls_df <- classified_df

  keep_cls <- c("class_num")
  if (!is.null(id_var) && nzchar(id_var) && id_var %in% names(cls_df)) {
    keep_cls <- c(id_var, keep_cls)
  }
  if (!is.null(weight_var) && nzchar(weight_var) && weight_var %in% names(cls_df)) {
    keep_cls <- c(keep_cls, weight_var)
  }
  keep_cls <- unique(keep_cls[keep_cls %in% names(cls_df)])
  cls_df <- cls_df[, keep_cls, drop = FALSE]

  if (!predictor_name %in% names(expanded_df)) {
    return(list(
      data = data.frame(),
      merge_method = "predictor_not_found"
    ))
  }

  # 현재 파이프라인에서는 classify와 prep/export 데이터의 row order를 그대로 사용
  if (nrow(cls_df) == nrow(expanded_df)) {
    out <- cls_df
    out[[predictor_name]] <- expanded_df[[predictor_name]]

    return(list(
      data = out,
      merge_method = "row_bind_forced"
    ))
  }

  return(list(
    data = data.frame(),
    merge_method = "row_mismatch"
  ))
}

# ----------------------------------------------------------
# helper: multinom coefficient matrix normalize
# ----------------------------------------------------------
as_coef_matrix <- function(x, row_names = NULL, col_names = NULL) {
  if (is.null(dim(x))) {
    out <- matrix(x, nrow = 1)
    if (!is.null(row_names) && length(row_names) == 1) rownames(out) <- row_names
    if (!is.null(col_names) && length(col_names) == ncol(out)) colnames(out) <- col_names
    return(out)
  }
  x
}

# ----------------------------------------------------------
# helper: predictor type label
# ----------------------------------------------------------
resolve_predictor_type_local <- function(spec_i) {
  pt <- as.character(spec_i$predictor_type[1] %||% NA_character_)
  if (!is.na(pt) && nzchar(pt)) return(pt)

  lev_i <- as.character(spec_i$level[1] %||% NA_character_)
  if (!is.na(lev_i) && nzchar(lev_i)) return("dummy")

  "continuous"
}

# ----------------------------------------------------------
# run univariable multinomial models
# ----------------------------------------------------------
for (i in seq_len(nrow(R3STEP_SPEC))) {
  spec_i       <- R3STEP_SPEC[i, , drop = FALSE]

  predictor_i  <- as.character(spec_i$predictor[1])
  source_var_i <- as.character(spec_i$source_var[1] %||% predictor_i)

  prep_i       <- build_r3step_analysis_df(
    classified_df  = CLASSIFIED_ANALYSIS,
    expanded_df    = R3STEP_DATA,
    predictor_name = predictor_i,
    id_var         = ID_VAR,
    weight_var     = WEIGHT_VAR
  )

  dat_i <- prep_i$data
  merge_method_i <- prep_i$merge_method

  dbg_i <- data.frame(
    predictor       = predictor_i,
    source_var      = as.character(spec_i$source_var[1] %||% NA_character_),
    var_name        = as.character(spec_i$var_name[1] %||% NA_character_),
    merge_method    = merge_method_i,
    n_input         = if (is.data.frame(dat_i)) nrow(dat_i) else 0L,
    n_complete      = 0L,
    fit_ok          = FALSE,
    error_text      = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(dat_i) || nrow(dat_i) == 0) {
    dbg_i$error_text <- "analysis dataset empty after merge"
    R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

    R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
      predictor       = predictor_i,
      source_var      = spec_i$source_var[1],
      var_name        = spec_i$var_name[1],
      merge_method    = merge_method_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure,
      method          = "Manual 3-step multinomial (R)",
      fit_ok          = FALSE
    )
    next
  }

  # 필요한 열만 유지
  keep_cols   <- c("class_num", predictor_i)
  if (!is.null(WEIGHT_VAR) && nzchar(WEIGHT_VAR) && WEIGHT_VAR %in% names(dat_i)) {
    keep_cols <- c(keep_cols, WEIGHT_VAR)
  }
  keep_cols   <- unique(keep_cols[keep_cols %in% names(dat_i)])
  dat_i       <- dat_i[, keep_cols, drop = FALSE]

  # class_num / predictor 결측 제거
  cc_idx           <- complete.cases(dat_i[, c("class_num", predictor_i)])
  dat_i            <- dat_i[cc_idx, , drop = FALSE]
  rownames(dat_i)  <- NULL

  dbg_i$n_complete <- nrow(dat_i)

  if (nrow(dat_i) == 0) {
    dbg_i$error_text <- "no complete cases"
    R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

    R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
      predictor       = predictor_i,
      source_var      = spec_i$source_var[1],
      var_name        = spec_i$var_name[1],
      merge_method    = merge_method_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure,
      method          = "Manual 3-step multinomial (R)",
      fit_ok          = FALSE
    )
    next
  }

  # predictor variation check
  pred_vals <- dat_i[[predictor_i]]
  if (length(unique(stats::na.omit(pred_vals))) < 2) {
    dbg_i$error_text <- "predictor has <2 unique values"
    R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

    R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
      predictor       = predictor_i,
      source_var      = spec_i$source_var[1],
      var_name        = spec_i$var_name[1],
      merge_method    = merge_method_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure,
      method          = "Manual 3-step multinomial (R)",
      fit_ok          = FALSE
    )
    next
  }

  # outcome class factor: reference = Class 1
  class_vals       <- suppressWarnings(as.integer(as.character(dat_i$class_num)))
  class_vals_chr   <- as.character(class_vals)
  class_levels     <- sort(unique(stats::na.omit(class_vals)))
  class_levels_chr <- as.character(sort(unique(stats::na.omit(class_vals))))
  ref_chr          <- as.character(REFERENCE_CLASS)

  if (!(ref_chr %in% class_levels_chr)) {
    ref_chr <- class_levels_chr[1]
  }

  # reference class를 첫 번째 level로 이동
  class_levels_chr  <- c(ref_chr, setdiff(class_levels_chr, ref_chr))
  nonref_levels_chr <- setdiff(class_levels_chr, ref_chr)

  dat_i$class_num   <- factor(class_vals_chr, levels = class_levels_chr)

  if (length(class_levels_chr) < 2) {
    dbg_i$error_text <- "class_num has <2 levels"
    R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

    R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
      predictor       = predictor_i,
      source_var      = spec_i$source_var[1],
      var_name        = spec_i$var_name[1],
      merge_method    = merge_method_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure,
      method          = "Manual 3-step multinomial (R)",
      fit_ok          = FALSE
    )
    next
  }

  dat_i$class_num <- factor(class_vals_chr, levels = class_levels_chr)

  fit_i <- NULL
  fit_err <- NULL

  fit_i <- tryCatch(
    {
      form_i <- stats::reformulate(termlabels = predictor_i, response = "class_num")

      if (!is.null(WEIGHT_VAR) && nzchar(WEIGHT_VAR) && WEIGHT_VAR %in% names(dat_i)) {
        ww   <- suppressWarnings(as.numeric(dat_i[[WEIGHT_VAR]]))
        ww[!is.finite(ww) | is.na(ww) | ww <= 0] <- 0

        keep_w  <- ww > 0
        dat_fit <- dat_i[keep_w, c("class_num", predictor_i), drop = FALSE]
        ww_fit  <- ww[keep_w]

        if (!"class_num" %in% names(dat_fit)) stop("class_num missing in dat_fit")
        if (nrow(dat_fit) == 0) stop("no positive weights after filtering")

        dat_fit$class_num <- factor(dat_fit$class_num, levels = class_levels_chr)

        nnet::multinom(
          formula = form_i,
          data    = dat_fit,
          weights = ww_fit,
          trace   = FALSE
        )
      } else {
        dat_fit <- dat_i[, c("class_num", predictor_i), drop = FALSE]

        if (!"class_num" %in% names(dat_fit)) stop("class_num missing in dat_fit")
        if (nrow(dat_fit) == 0) stop("empty dat_fit")

        dat_fit$class_num <- factor(dat_fit$class_num, levels = class_levels_chr)

        nnet::multinom(
          formula = form_i,
          data    = dat_fit,
          trace   = FALSE
        )
      }
    },
    error = function(e) {
      fit_err <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(fit_i)) {
    dbg_i$error_text <- fit_err %||% "multinom fit failed"
    R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

    R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
      predictor       = predictor_i,
      source_var      = spec_i$source_var[1],
      var_name        = spec_i$var_name[1],
      merge_method    = merge_method_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure,
      method          = "Manual 3-step multinomial (R)",
      fit_ok          = FALSE,
      error_text      = fit_err
    )
    next
  }

  sm_i <- summary(fit_i)
  coef_i <- as_coef_matrix(
    sm_i$coefficients,
    row_names = class_levels_chr[-1],
    col_names = colnames(sm_i$coefficients %||% matrix(nrow = 0, ncol = 0))
  )
  se_i <- as_coef_matrix(
    sm_i$standard.errors,
    row_names = rownames(coef_i),
    col_names = colnames(coef_i)
  )

  # ----------------------------------------------------------
  # predictor coefficient column resolve (robust)
  # ----------------------------------------------------------
  coef_cols <- colnames(coef_i) %||% character(0)

  pred_col <- NA_character_

  log_info("DEBUG predictor=", predictor_i,
           " coef_cols=", paste(coef_cols, collapse=", "))
  log_info("CHECK predictor=", predictor_i,
           " unique=", paste(unique(dat_i[[predictor_i]]), collapse=", "))

  # 1차: exact match
  if (predictor_i %in% coef_cols) {
    pred_col <- predictor_i
  }

  # 2차: make.names 기반 match
  if (is.na(pred_col) || !nzchar(pred_col)) {
    coef_cols_clean <- make.names(coef_cols)
    pred_clean <- make.names(predictor_i)
    hit <- coef_cols[coef_cols_clean == pred_clean]
    if (length(hit) > 0) pred_col <- hit[1]
  }

  # 3차: source_var 기준 match
  if ((is.na(pred_col) || !nzchar(pred_col)) && "source_var" %in% names(spec_i)) {
    source_var_i <- as.character(spec_i$source_var[1] %||% "")
    if (nzchar(source_var_i)) {
      source_clean <- make.names(source_var_i)
      coef_cols_clean <- make.names(coef_cols)
      hit <- coef_cols[coef_cols_clean == source_clean]
      if (length(hit) > 0) pred_col <- hit[1]
    }
  }

  # 마지막에만 skip
  if (is.na(pred_col) || !nzchar(pred_col)) {
    log_warn("SKIP predictor (no coef): ", predictor_i,
             " | coef_cols=", paste(coef_cols, collapse=", "))
    next
  }

  # 2차: make.names 기반 match
  if (is.na(pred_col) || !nzchar(pred_col)) {
    coef_cols_clean <- make.names(coef_cols)
    pred_clean      <- make.names(predictor_i)
    hit             <- coef_cols[coef_cols_clean == pred_clean]
    if (length(hit) > 0) pred_col <- hit[1]
  }

  # 3차: source_var 기준 match
  if ((is.na(pred_col) || !nzchar(pred_col)) && "source_var" %in% names(spec_i)) {
    source_var_i      <- as.character(spec_i$source_var[1] %||% "")
    if (nzchar(source_var_i)) {
      source_clean    <- make.names(source_var_i)
      coef_cols_clean <- make.names(coef_cols)
      hit             <- coef_cols[coef_cols_clean == source_clean]
      if (length(hit) > 0) pred_col <- hit[1]
    }
  }

  if (is.na(pred_col) || !nzchar(pred_col)) {
    dbg_i$error_text <- paste0(
      "predictor coefficient not found in multinom output | predictor=",
      predictor_i,
      " | coef_cols=",
      paste(coef_cols, collapse = ", ")
    )
    R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

    R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
      predictor       = predictor_i,
      source_var      = spec_i$source_var[1],
      var_name        = spec_i$var_name[1],
      merge_method    = merge_method_i,
      best_k          = best_k,
      best_tag        = best_tag,
      model_structure = best_model_structure,
      method          = "Manual 3-step multinomial (R)",
      fit_ok          = FALSE,
      error_text      = dbg_i$error_text
    )
    next
  }

  dbg_i$fit_ok <- TRUE
  R3STEP_PARSE_DEBUG[[length(R3STEP_PARSE_DEBUG) + 1L]] <- dbg_i

  log_info("coef rows = ", paste(rownames(coef_i), collapse = ", "))

  for (r in seq_len(nrow(coef_i))) {

    est <- suppressWarnings(as.numeric(coef_i[r, pred_col]))
    se  <- suppressWarnings(as.numeric(se_i[r, pred_col]))

    if (is.na(est)) next

    # 🔥 SE 안정 처리
    if (is.na(se) || !is.finite(se) || se == 0) {
      se       <- NA_real_
      stat_val <- NA_real_
      p_val    <- NA_real_
    } else {
      stat_val <- est / se
      p_val    <- 2 * (1 - pnorm(abs(stat_val)))
    }

    rrr  <- exp(est)
    # 🔥 CI 계산 안정화 (Inf 방지)
    if (is.na(se) || !is.finite(se)) {
      llci <- NA_real_
      ulci <- NA_real_
    } else {
      llci <- exp(est - 1.96 * se)
      ulci <- exp(est + 1.96 * se)
    }

    row_class_chr <- rownames(coef_i)[r]

    # 🔥 숫자만 추출 (핵심 수정)
    class_num_i     <- suppressWarnings(
      as.integer(gsub("[^0-9]", "", row_class_chr))
    )

    if (is.na(class_num_i)) {
      # fallback
      if (r <= length(nonref_levels_chr)) {
        class_num_i <- suppressWarnings(as.integer(nonref_levels_chr[r]))
      }
    }

    log_info("ROWNAME=", row_class_chr, " → class=", class_num_i)

    if (is.na(class_num_i)) next
    if (class_num_i == REFERENCE_CLASS) next

    log_info("KEEP: predictor=", predictor_i, " class=", class_num_i, " est=", round(est,3))

    source_var_i      <- as.character(spec_i$source_var[1] %||% predictor_i)
    var_name_i        <- as.character(spec_i$var_name[1] %||% source_var_i)
    var_label_i       <- as.character(spec_i$var_label[1] %||% resolve_var_label_local(var_name_i))
    level_i           <- as.character(spec_i$level[1] %||% NA_character_)
    value_label_i     <- as.character(spec_i$value_label[1] %||% NA_character_)
    predictor_type_i  <- resolve_predictor_type_local(spec_i)
    reference_level_i <- as.character(spec_i$reference_level[1] %||% NA_character_)

    R3_UNIVARIABLE[[length(R3_UNIVARIABLE) + 1L]] <- data.frame(
      analysis         = "univariable",
      model_type       = "manual_3step_r_multinom",
      method           = "R multinomial",
      best_k           = as.integer(best_k),
      best_tag         = as.character(best_tag),
      model_structure  = as.character(best_model_structure),
      predictor        = source_var_i,
      source_var       = source_var_i,
      var_name         = var_name_i,
      var_label        = var_label_i,
      level            = level_i,
      value_label      = value_label_i,
      predictor_type   = predictor_type_i,
      outcome_class    = paste0("Class ", class_num_i),
      reference_class  = paste0("Class ", REFERENCE_CLASS),
      reference_level  = reference_level_i,
      comparison       = paste0("Class ", class_num_i, " vs Class ", REFERENCE_CLASS),
      estimate         = est,
      se               = se,
      stat             = stat_val,
      p                = p_val,
      p_fmt            = fmt_p_tbl(p_val),
      sig              = p_to_sig_tbl(p_val),
      rrr              = rrr,
      rrr_fmt          = formatC(rrr, format = "f", digits = 3),
      llci             = llci,
      llci_fmt         = formatC(llci, format = "f", digits = 3),
      ulci             = ulci,
      ulci_fmt         = formatC(ulci, format = "f", digits = 3),
      inp_file         = NA_character_,
      out_file         = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  R3STEP_RUN_REGISTRY[[length(R3STEP_RUN_REGISTRY) + 1L]] <- list(
    predictor         = predictor_i,
    source_var        = spec_i$source_var[1],
    var_name          = spec_i$var_name[1],
    var_label         = spec_i$var_label[1],
    merge_method      = merge_method_i,
    best_k            = best_k,
    best_tag          = best_tag,
    model_structure   = best_model_structure,
    method            = "Manual 3-step multinomial (R)",
    fit_ok            = TRUE,
    n_complete        = nrow(dat_i)
  )
}

R3STEP_PARSE_DEBUG_DF <- if (length(R3STEP_PARSE_DEBUG) == 0) {
  data.frame()
} else {
  do.call(rbind, R3STEP_PARSE_DEBUG)
}
rownames(R3STEP_PARSE_DEBUG_DF) <- NULL

R3_UNIVARIABLE_DF <- if (length(R3_UNIVARIABLE) == 0) {
  empty_r3step_table()
} else {
  do.call(rbind, R3_UNIVARIABLE)
}
rownames(R3_UNIVARIABLE_DF) <- NULL

# 현재는 multivariable 틀만 유지
R3_MULTIVARIABLE_DF <- empty_r3step_table()

T5_rrr <- safe_rbind(R3_UNIVARIABLE_DF, R3_MULTIVARIABLE_DF)

if (is.data.frame(T5_rrr) && nrow(T5_rrr) > 0) {
  ord_var <- match(T5_rrr$var_name, COVARIATES)
  ord_var[is.na(ord_var)] <- 999999 + seq_len(sum(is.na(ord_var)))
  T5_rrr <- T5_rrr[
    order(ord_var, T5_rrr$outcome_class, T5_rrr$level),
    ,
    drop = FALSE
  ]
  rownames(T5_rrr) <- NULL
}

R3STEP_RESULTS <- list(
  method              = "Manual 3-step multinomial (R)",
  best_k              = best_k,
  best_tag            = best_tag,
  model_structure     = best_model_structure,
  covariates_original = COVARIATES,
  predictors_expanded = R3STEP_SPEC,
  run_registry        = R3STEP_RUN_REGISTRY,
  univariable         = R3_UNIVARIABLE_DF,
  multivariable       = R3_MULTIVARIABLE_DF,
  T5_rrr              = T5_rrr
)


# ------------------------------------------------------------
# 9. save
# ------------------------------------------------------------
log_info("Saving R3STEP outputs ...")

save_named_rds_list(
  list(
    R3STEP_DATA           = R3STEP_DATA,
    R3STEP_SPEC           = R3STEP_SPEC,
    R3STEP_RESULTS        = R3STEP_RESULTS,
    R3STEP_PARSE_DEBUG    = R3STEP_PARSE_DEBUG_DF,
    T5_rrr                = T5_rrr
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_R3STEP, units = "secs")), 2)

log_info("04c_r3step.R completed.")
log_info("method            = Manual 3-step multinomial (R)")
log_info("best_k            = ", best_k)
log_info("best_tag          = ", best_tag)
log_info("model_structure   = ", best_model_structure)
log_info("reference_class   = Class ", REFERENCE_CLASS)
log_info("n(covariates)     = ", length(COVARIATES))
log_info("n(predictors)     = ", nrow(R3STEP_SPEC))
log_info("n(univariable)    = ", nrow(R3_UNIVARIABLE_DF))
log_info("elapsed           = ", elapsed_sec, " sec")
log_info("n(parse debug)    = ", nrow(R3STEP_PARSE_DEBUG_DF))

log_step_end("r3step", elapsed_sec, ok = TRUE)
