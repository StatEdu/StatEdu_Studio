# ============================================================
# 02_prep.R
# Prep stage for cross-sectional mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) settings 단계 산출물 재로딩
# 2) 분석 변수 유효성 점검
# 3) missing string / missing rule 적용
# 4) subset 규칙 적용
# 5) ANALYSIS_DATA / DISPLAY_DATA / MPLUS_DATA 생성
# 6) missing summary 및 prep summary 저장
# ============================================================

T0_PREP <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("PREP", "02_prep.R")
log_info("Reloading settings outputs ...")

# ------------------------------------------------------------
# 1. reload settings outputs
# ------------------------------------------------------------
obj <- reload_settings_outputs(dir_rds = DIR_RDS)

CFG              <- obj$CFG
DICT             <- obj$DICT
RAW_DATA         <- obj$RAW_DATA
SETTINGS_SUMMARY <- obj$SETTINGS_SUMMARY
SURVEY_BUNDLE    <- obj$SURVEY_BUNDLE

MISSING_SPEC <- load_step_rds("MISSING_SPEC", dir_rds = DIR_RDS, default = list())
SUBSETS_SPEC <- load_step_rds("SUBSETS_SPEC", dir_rds = DIR_RDS, default = list())

RAW_DATA <- as.data.frame(RAW_DATA, stringsAsFactors = FALSE)
RAW_DATA <- repair_names_if_needed(RAW_DATA)

# ------------------------------------------------------------
# 2. refresh DICT against RAW_DATA
# ------------------------------------------------------------
log_info("Refreshing DICT against RAW_DATA ...")

DICT <- enrich_dict_from_data(
  dict = DICT,
  raw_data = RAW_DATA
)

log_info(
  "DICT enriched from data: meta nrow = ",
  if (is.data.frame(DICT$meta)) nrow(DICT$meta) else 0L,
  ", levels nrow = ",
  if (is.data.frame(DICT$levels)) nrow(DICT$levels) else 0L
)

# ------------------------------------------------------------
# 3. resolve analysis variables
# ------------------------------------------------------------
vars <- extract_analysis_vars(DICT)

INDICATORS <- SETTINGS_SUMMARY$indicators %||% SETTINGS_SUMMARY$INDICATORS %||% vars$indicators %||% character(0)
INDICATORS_CONTINUOUS <- SETTINGS_SUMMARY$indicators_continuous %||% SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% character(0)
INDICATORS_CATEGORICAL <- SETTINGS_SUMMARY$indicators_categorical %||% SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% character(0)
COVARIATES <- SETTINGS_SUMMARY$covariates %||% SETTINGS_SUMMARY$COVARIATES %||% vars$covariates %||% character(0)
OUTCOMES   <- SETTINGS_SUMMARY$outcomes %||% SETTINGS_SUMMARY$OUTCOMES %||% vars$outcomes %||% character(0)

WEIGHT_VAR <- SURVEY_BUNDLE$weight_var %||% vars$weight_var %||% NULL
STRATA_VAR <- SURVEY_BUNDLE$strata_var %||% vars$strata_var %||% NULL
CLUSTER_VAR <- SURVEY_BUNDLE$cluster_var %||% vars$cluster_var %||% NULL
ID_VAR <- SURVEY_BUNDLE$id_var %||% vars$id_var %||% NULL

required_groups <- list(
  INDICATORS = INDICATORS,
  INDICATORS_CATEGORICAL = INDICATORS_CATEGORICAL,
  INDICATORS_CONTINUOUS = INDICATORS_CONTINUOUS,
  COVARIATES = COVARIATES,
  OUTCOMES = OUTCOMES
)

required_groups <- lapply(required_groups, function(x) intersect(unique_nz(x), names(RAW_DATA)))

INDICATORS <- required_groups$INDICATORS
INDICATORS_CATEGORICAL <- required_groups$INDICATORS_CATEGORICAL
INDICATORS_CONTINUOUS <- required_groups$INDICATORS_CONTINUOUS
COVARIATES <- required_groups$COVARIATES
OUTCOMES <- required_groups$OUTCOMES

if (!is.null(WEIGHT_VAR) && !WEIGHT_VAR %in% names(RAW_DATA)) WEIGHT_VAR <- NULL
if (!is.null(STRATA_VAR) && !STRATA_VAR %in% names(RAW_DATA)) STRATA_VAR <- NULL
if (!is.null(CLUSTER_VAR) && !CLUSTER_VAR %in% names(RAW_DATA)) CLUSTER_VAR <- NULL
if (!is.null(ID_VAR) && !ID_VAR %in% names(RAW_DATA)) ID_VAR <- NULL

# ------------------------------------------------------------
# validate variables against RAW_DATA / ANALYSIS_DATA
# ------------------------------------------------------------
INDICATORS <- intersect(unique(INDICATORS), names(RAW_DATA))
COVARIATES <- intersect(unique(COVARIATES), names(RAW_DATA))
OUTCOMES   <- intersect(unique(OUTCOMES),   names(RAW_DATA))

# CFG outcome 재확인 (settings에서 누락되었더라도 보완)
CFG_OUTCOMES <- CFG$outcomes %||% character(0)
CFG_OUTCOMES <- trimws(as.character(CFG_OUTCOMES))
CFG_OUTCOMES <- CFG_OUTCOMES[nzchar(CFG_OUTCOMES)]
CFG_OUTCOMES <- intersect(unique(CFG_OUTCOMES), names(RAW_DATA))

OUTCOMES <- unique(c(OUTCOMES, CFG_OUTCOMES))

log_info("Validated variables:")
log_info("  INDICATORS             = ", paste(INDICATORS, collapse = ", "))
log_info("  INDICATORS_CATEGORICAL = ", paste(INDICATORS_CATEGORICAL, collapse = ", "))
log_info("  INDICATORS_CONTINUOUS  = ", paste(INDICATORS_CONTINUOUS, collapse = ", "))
log_info("  COVARIATES             = ", paste(COVARIATES, collapse = ", "))
log_info("  OUTCOMES               = ", paste(OUTCOMES, collapse = ", "))

if (length(INDICATORS) == 0) {
  stop("No valid indicators found after prep validation.", call. = FALSE)
}

# ------------------------------------------------------------
# 4. clean missing strings / rules / basic coercion
# ------------------------------------------------------------
log_info("Cleaning missing strings, applying missing rules, and coercing basic types ...")

clean_data_by_dict <- function(data, dict, missing_spec = list()) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)

  meta <- dict$meta %||% data.frame()
  out <- data

  # global extra missing strings
  extra_missing_strings <- missing_spec$missing_strings %||% missing_spec$strings %||% character(0)

  for (v in names(out)) {
    x <- out[[v]]

    # 1) clean common missing strings
    x <- clean_missing_strings(x, extra_missing = extra_missing_strings)

    # 2) apply variable-specific missing rules if available
    var_missing <- NULL
    if (is.list(missing_spec) && !is.null(missing_spec[[v]])) {
      spec_v <- missing_spec[[v]]
      if (is.list(spec_v)) {
        var_missing <- spec_v$values %||% spec_v$missing_values %||% spec_v
      } else {
        var_missing <- spec_v
      }
    }
    x <- apply_missing_rule_vector(x, missing_values = var_missing)

    # 3) coerce basic type using DICT
    type_v <- NULL
    subtype_v <- NULL
    if (is.data.frame(meta) && nrow(meta) > 0 && "var_name" %in% names(meta)) {
      hit <- meta[meta$var_name == v, , drop = FALSE]
      if (nrow(hit) > 0) {
        type_v <- hit$type[1] %||% NULL
        subtype_v <- hit$subtype[1] %||% NULL
      }
    }

    if (!is.null(type_v) && identical(type_v, "numeric")) {
      x <- as_num_safely(x)
    } else if (!is.null(type_v) && identical(type_v, "logical")) {
      x <- as.logical(x)
    } else if (!is.null(type_v) && identical(type_v, "factor")) {
      x <- as_factor_safely(x)
    } else {
      # auto rule
      if (!is.null(subtype_v) && identical(subtype_v, "continuous")) {
        x <- as_num_safely(x)
      }
    }

    out[[v]] <- x
  }

  out
}

RAW_DATA_CLEAN <- clean_data_by_dict(
  data = RAW_DATA,
  dict = DICT,
  missing_spec = MISSING_SPEC
)

# ------------------------------------------------------------
# 5. build base analysis/display datasets
# ------------------------------------------------------------
analysis_vars <- unique(c(
  INDICATORS,
  COVARIATES,
  OUTCOMES,
  WEIGHT_VAR,
  STRATA_VAR,
  CLUSTER_VAR,
  ID_VAR,
  SURVEY_BUNDLE$subset_var %||% NULL
))
analysis_vars <- analysis_vars[!is.na(analysis_vars) & nzchar(analysis_vars)]
analysis_vars <- intersect(analysis_vars, names(RAW_DATA_CLEAN))

ANALYSIS_DATA <- RAW_DATA_CLEAN[, analysis_vars, drop = FALSE]
DISPLAY_DATA  <- RAW_DATA_CLEAN[, analysis_vars, drop = FALSE]

# ------------------------------------------------------------
# 6. apply subset rule
# ------------------------------------------------------------
log_info("Applying subset rule to analysis/display data ...")

subset_spec <- list(
  subset_name = SURVEY_BUNDLE$subset_name %||% NULL,
  subset_var = SURVEY_BUNDLE$subset_var %||% NULL,
  subset_value = SURVEY_BUNDLE$subset_value %||% NULL,
  subset_expr = SURVEY_BUNDLE$subset_expr %||% NULL
)

ANALYSIS_DATA_SUB <- apply_subset_filter(ANALYSIS_DATA, subset_spec = subset_spec)
DISPLAY_DATA_SUB  <- apply_subset_filter(DISPLAY_DATA, subset_spec = subset_spec)

# rownames reset
rownames(ANALYSIS_DATA_SUB) <- NULL
rownames(DISPLAY_DATA_SUB) <- NULL

log_info("ANALYSIS_DATA_SUB: data.frame [", nrow(ANALYSIS_DATA_SUB), " x ", ncol(ANALYSIS_DATA_SUB), "]")
log_info("DISPLAY_DATA_SUB: data.frame [", nrow(DISPLAY_DATA_SUB), " x ", ncol(DISPLAY_DATA_SUB), "]")

# ------------------------------------------------------------
# 7. build MPLUS_DATA
# ------------------------------------------------------------
mplus_vars <- unique(c(
  INDICATORS,
  COVARIATES,
  OUTCOMES,
  WEIGHT_VAR,
  STRATA_VAR,
  CLUSTER_VAR,
  ID_VAR
))
mplus_vars <- mplus_vars[!is.na(mplus_vars) & nzchar(mplus_vars)]
mplus_vars <- intersect(mplus_vars, names(ANALYSIS_DATA_SUB))

MPLUS_DATA <- ANALYSIS_DATA_SUB[, mplus_vars, drop = FALSE]

# mplus-friendly coercion
for (v in names(MPLUS_DATA)) {
  x <- MPLUS_DATA[[v]]

  if (is.logical(x)) x <- as.integer(x)
  if (is.factor(x)) x <- as.character(x)

  # categorical indicators / covariates도 수치형으로 보낼 수 있게 처리
  if (is.character(x)) {
    suppressWarnings(x_num <- as.numeric(x))
    # 숫자 문자열이면 그대로 numeric
    if (sum(!is.na(x_num)) > 0) {
      x <- x_num
    }
  }

  MPLUS_DATA[[v]] <- x
}

# id 없으면 임시 id 생성
if (is.null(ID_VAR) || !ID_VAR %in% names(MPLUS_DATA)) {
  if (!"id" %in% names(MPLUS_DATA)) {
    MPLUS_DATA$id <- seq_len(nrow(MPLUS_DATA))
    ANALYSIS_DATA_SUB$id <- seq_len(nrow(ANALYSIS_DATA_SUB))
    DISPLAY_DATA_SUB$id <- seq_len(nrow(DISPLAY_DATA_SUB))
    ID_VAR <- "id"
  } else {
    ID_VAR <- "id"
  }
}


# ------------------------------------------------------------
# 7-1. prep validation after subset / mplus data build
# ------------------------------------------------------------
log_info("Running prep validation ...")

prep_val_rows <- list()

add_prep_validation_row <- function(check_name, var_name = NA_character_, result = "ok", detail = "") {
  prep_val_rows[[length(prep_val_rows) + 1L]] <<- data.frame(
    check_name = as.character(check_name)[1],
    var_name   = as.character(var_name)[1],
    result     = as.character(result)[1],
    detail     = as.character(detail)[1],
    stringsAsFactors = FALSE
  )
}

# 1) analysis rows after subset
if (!is.data.frame(ANALYSIS_DATA_SUB) || nrow(ANALYSIS_DATA_SUB) == 0) {
  add_prep_validation_row(
    check_name = "analysis_rows_after_subset",
    result = "error",
    detail = "ANALYSIS_DATA_SUB has zero rows after subset."
  )
} else {
  add_prep_validation_row(
    check_name = "analysis_rows_after_subset",
    result = "ok",
    detail = paste0("n=", nrow(ANALYSIS_DATA_SUB))
  )
}

# 2) indicator presence
for (v in INDICATORS) {
  if (!v %in% names(ANALYSIS_DATA_SUB)) {
    add_prep_validation_row(
      check_name = "indicator_missing_after_subset",
      var_name = v,
      result = "error",
      detail = "Indicator not found in ANALYSIS_DATA_SUB."
    )
  }
}

# 3) indicator non-missing / variation
for (v in intersect(INDICATORS, names(ANALYSIS_DATA_SUB))) {
  x <- ANALYSIS_DATA_SUB[[v]]
  n_nonmiss <- sum(!is.na(x))
  n_uniq <- length(unique(stats::na.omit(x)))

  if (n_nonmiss == 0) {
    add_prep_validation_row(
      check_name = "indicator_all_missing",
      var_name = v,
      result = "error",
      detail = "Indicator is all missing after subset."
    )
  } else if (n_uniq <= 1) {
    add_prep_validation_row(
      check_name = "indicator_single_value",
      var_name = v,
      result = "warn",
      detail = paste0("Only ", n_uniq, " unique non-missing value after subset.")
    )
  } else {
    add_prep_validation_row(
      check_name = "indicator_ok",
      var_name = v,
      result = "ok",
      detail = paste0("nonmissing=", n_nonmiss, ", unique=", n_uniq)
    )
  }
}

# 4) categorical-like variables collapsed
cat_vars_check <- unique(c(INDICATORS_CATEGORICAL, COVARIATES, OUTCOMES))
cat_vars_check <- intersect(cat_vars_check, names(ANALYSIS_DATA_SUB))

for (v in cat_vars_check) {
  x <- ANALYSIS_DATA_SUB[[v]]
  ux <- unique(stats::na.omit(as.character(x)))
  if (length(ux) <= 1) {
    add_prep_validation_row(
      check_name = "categorical_collapsed",
      var_name = v,
      result = "warn",
      detail = paste0("Observed non-missing levels=", length(ux), " after subset.")
    )
  }
}

# 5) outcomes all missing
for (v in intersect(OUTCOMES, names(ANALYSIS_DATA_SUB))) {
  x <- ANALYSIS_DATA_SUB[[v]]
  if (sum(!is.na(x)) == 0) {
    add_prep_validation_row(
      check_name = "outcome_all_missing",
      var_name = v,
      result = "warn",
      detail = "Outcome is all missing after subset."
    )
  }
}

# 6) weight quality
if (!is.null(WEIGHT_VAR) && WEIGHT_VAR %in% names(ANALYSIS_DATA_SUB)) {
  w <- suppressWarnings(as.numeric(ANALYSIS_DATA_SUB[[WEIGHT_VAR]]))
  if (all(is.na(w))) {
    add_prep_validation_row(
      check_name = "weight_all_missing",
      var_name = WEIGHT_VAR,
      result = "error",
      detail = "Weight variable is all missing."
    )
  } else if (sum(!is.na(w) & w > 0) == 0) {
    add_prep_validation_row(
      check_name = "weight_no_positive",
      var_name = WEIGHT_VAR,
      result = "error",
      detail = "Weight variable has no positive values."
    )
  } else {
    add_prep_validation_row(
      check_name = "weight_ok",
      var_name = WEIGHT_VAR,
      result = "ok",
      detail = paste0(
        "nonmissing=", sum(!is.na(w)),
        ", positive=", sum(!is.na(w) & w > 0)
      )
    )
  }
}

# 7) design vars uniqueness
for (nm in c(STRATA_VAR, CLUSTER_VAR, ID_VAR)) {
  if (is.null(nm) || !nzchar(nm) || !nm %in% names(ANALYSIS_DATA_SUB)) next

  x <- ANALYSIS_DATA_SUB[[nm]]
  nunq <- length(unique(stats::na.omit(x)))

  add_prep_validation_row(
    check_name = "design_var_check",
    var_name = nm,
    result = ifelse(nunq <= 1, "warn", "ok"),
    detail = paste0("unique_nonmissing=", nunq)
  )
}

PREP_VALIDATION <- if (length(prep_val_rows) == 0) {
  data.frame(
    check_name = character(0),
    var_name   = character(0),
    result     = character(0),
    detail     = character(0),
    stringsAsFactors = FALSE
  )
} else {
  do.call(rbind, prep_val_rows)
}

rownames(PREP_VALIDATION) <- NULL

n_prep_err <- sum(PREP_VALIDATION$result == "error", na.rm = TRUE)
n_prep_warn <- sum(PREP_VALIDATION$result == "warn", na.rm = TRUE)

log_info("PREP_VALIDATION : errors=", n_prep_err, ", warnings=", n_prep_warn)

top_prep <- PREP_VALIDATION[PREP_VALIDATION$result != "ok", , drop = FALSE]
if (nrow(top_prep) > 0) {
  top_prep <- head(top_prep, 15)
  for (i in seq_len(nrow(top_prep))) {
    log_warn(
      "[PREP_VALIDATION] ",
      top_prep$result[i], " / ",
      top_prep$check_name[i], " / ",
      top_prep$var_name[i], " / ",
      top_prep$detail[i]
    )
  }
}

if (n_prep_err > 0) {
  stop("Prep validation failed. Check PREP_VALIDATION.", call. = FALSE)
}


# ------------------------------------------------------------
# 8. missing summaries
# ------------------------------------------------------------
log_info("Building missing summaries ...")

MISSING_SUMMARY_ANALYSIS <- missing_summary_df(ANALYSIS_DATA_SUB)
MISSING_SUMMARY_DISPLAY  <- missing_summary_df(DISPLAY_DATA_SUB)
MISSING_SUMMARY_MPLUS    <- missing_summary_df(MPLUS_DATA)

# ------------------------------------------------------------
# 9. prep summary
# ------------------------------------------------------------
PREP_SUMMARY <- list(
  analysis_n = nrow(ANALYSIS_DATA_SUB),
  analysis_p = ncol(ANALYSIS_DATA_SUB),
  display_n = nrow(DISPLAY_DATA_SUB),
  display_p = ncol(DISPLAY_DATA_SUB),
  mplus_n = nrow(MPLUS_DATA),
  mplus_p = ncol(MPLUS_DATA),

  indicators = INDICATORS,
  indicators_continuous = INDICATORS_CONTINUOUS,
  indicators_categorical = INDICATORS_CATEGORICAL,
  covariates = COVARIATES,
  outcomes = OUTCOMES,

  weight_var = WEIGHT_VAR,
  strata_var = STRATA_VAR,
  cluster_var = CLUSTER_VAR,
  id_var = ID_VAR,

  subset_var = subset_spec$subset_var %||% NULL,
  subset_name = subset_spec$subset_name %||% NULL,
  subset_expr = subset_spec$subset_expr %||% NULL,

  created_at = Sys.time()
)

PREP_SUMMARY$ANALYSIS_N <- PREP_SUMMARY$analysis_n
PREP_SUMMARY$ANALYSIS_P <- PREP_SUMMARY$analysis_p
PREP_SUMMARY$DISPLAY_N <- PREP_SUMMARY$display_n
PREP_SUMMARY$DISPLAY_P <- PREP_SUMMARY$display_p
PREP_SUMMARY$MPLUS_N <- PREP_SUMMARY$mplus_n
PREP_SUMMARY$MPLUS_P <- PREP_SUMMARY$mplus_p

# keep SETTINGS_SUMMARY synced for downstream compatibility
SETTINGS_SUMMARY$id_var <- ID_VAR
SETTINGS_SUMMARY$ID_VAR <- ID_VAR
SETTINGS_SUMMARY$weight_var <- WEIGHT_VAR
SETTINGS_SUMMARY$WEIGHT_VAR <- WEIGHT_VAR
SETTINGS_SUMMARY$strata_var <- STRATA_VAR
SETTINGS_SUMMARY$STRATA_VAR <- STRATA_VAR
SETTINGS_SUMMARY$cluster_var <- CLUSTER_VAR
SETTINGS_SUMMARY$CLUSTER_VAR <- CLUSTER_VAR

# ------------------------------------------------------------
# 10. save outputs
# ------------------------------------------------------------
log_info("Saving prep outputs ...")

save_named_rds_list(
  list(
    DICT = DICT,
    SETTINGS_SUMMARY = SETTINGS_SUMMARY,
    ANALYSIS_DATA = ANALYSIS_DATA,
    DISPLAY_DATA = DISPLAY_DATA,
    ANALYSIS_DATA_SUB = ANALYSIS_DATA_SUB,
    DISPLAY_DATA_SUB = DISPLAY_DATA_SUB,
    MPLUS_DATA = MPLUS_DATA,
    MISSING_SUMMARY_ANALYSIS = MISSING_SUMMARY_ANALYSIS,
    MISSING_SUMMARY_DISPLAY = MISSING_SUMMARY_DISPLAY,
    MISSING_SUMMARY_MPLUS = MISSING_SUMMARY_MPLUS,
    PREP_SUMMARY = PREP_SUMMARY,
    PREP_VALIDATION = PREP_VALIDATION
  ),
  dir_rds = DIR_RDS
)

# ------------------------------------------------------------
# 11. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_PREP, units = "secs")), 2)

log_info("02_prep.R completed.")
log_info("n(analysis) = ", nrow(ANALYSIS_DATA_SUB))
log_info("p(analysis) = ", ncol(ANALYSIS_DATA_SUB))
log_info("n(display)  = ", nrow(DISPLAY_DATA_SUB))
log_info("p(display)  = ", ncol(DISPLAY_DATA_SUB))
log_info("n(mplus)    = ", nrow(MPLUS_DATA))
log_info("p(mplus)    = ", ncol(MPLUS_DATA))
log_info("elapsed     = ", elapsed_sec, " sec")

log_step_end("prep", elapsed_sec, ok = TRUE)
