# ============================================================
# 01_settings.R
# Settings stage for cross-sectional mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) CFG / dictionary / missing / subsets / raw data 읽기
# 2) DICT 생성 및 data 기반 보강
# 3) 분석 변수군(indicators, covariates, outcomes 등) 추출
# 4) indicator subtype 분리
# 5) mixture type 추론
# 6) survey bundle 생성
# 7) settings 단계 산출물 저장
# ============================================================

T0_SETTINGS <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("SETTINGS", "01_settings.R")
log_info("Reading CFG / subsets / missing / raw data ...")

# ------------------------------------------------------------
# 1. read pipeline inputs
# ------------------------------------------------------------
inp <- read_pipeline_inputs(
  cfg_path      = PATH_CFG,
  dict_path     = PATH_DICT,
  missing_path  = PATH_MISSING,
  subsets_path  = PATH_SUBSETS,
  data_path     = PATH_DATA,
  required_cfg  = TRUE,
  required_dict = TRUE,
  required_data = TRUE
)

CFG          <- inp$CFG
DICT_RAW     <- inp$DICT_RAW
MISSING_SPEC <- inp$MISSING_SPEC
SUBSETS_SPEC <- inp$SUBSETS_SPEC
RAW_DATA     <- inp$RAW_DATA

RAW_DATA <- as.data.frame(RAW_DATA, stringsAsFactors = FALSE)
RAW_DATA <- repair_names_if_needed(RAW_DATA)

log_info("RAW_DATA loaded: nrow=", nrow(RAW_DATA), ", ncol=", ncol(RAW_DATA))


# ------------------------------------------------------------
# 1-1. input consistency checks
# ------------------------------------------------------------
log_info("Running input consistency checks ...")

survey_vars_for_validation <- resolve_survey_vars(
  cfg = CFG,
  raw_data = RAW_DATA
)

subset_spec_for_validation <- resolve_subset_spec(
  cfg = CFG,
  subsets_spec = SUBSETS_SPEC,
  survey_vars = survey_vars_for_validation
)

active_subsets_for_validation <- list()
has_active_subset <- (
  !is.null(subset_spec_for_validation$subset_name) &&
    nzchar(as.character(subset_spec_for_validation$subset_name))
) || (
  !is.null(subset_spec_for_validation$subset_var) &&
    nzchar(as.character(subset_spec_for_validation$subset_var))
) || (
  !is.null(subset_spec_for_validation$subset_expr) &&
    nzchar(as.character(subset_spec_for_validation$subset_expr))
)

if (isTRUE(has_active_subset)) {
  active_subset_name <- subset_spec_for_validation$subset_name %||% "active_subset"
  active_subsets_for_validation[[active_subset_name]] <- Filter(
    Negate(is.null),
    list(
      var = subset_spec_for_validation$subset_var %||% NULL,
      value = subset_spec_for_validation$subset_value %||% NULL,
      expr = subset_spec_for_validation$subset_expr %||% NULL
    )
  )
}

DICT_VALIDATION <- validate_dictionary_against_data(
  dict_raw = DICT_RAW,
  raw_data = RAW_DATA
)

CFG_VALIDATION <- validate_cfg_against_dict_and_data(
  cfg = CFG,
  dict_raw = DICT_RAW,
  raw_data = RAW_DATA
)

SUBSET_VALIDATION <- validate_subsets_against_data(
  subsets_spec = active_subsets_for_validation,
  raw_data = RAW_DATA
)

INPUT_FINGERPRINT <- build_input_fingerprint_manifest(
  cfg_path = PATH_CFG,
  dict_path = PATH_DICT,
  subsets_path = PATH_SUBSETS,
  missing_path = PATH_MISSING,
  data_path = PATH_DATA
)

dict_prob <- DICT_VALIDATION$problems %||% data.frame()
cfg_prob  <- CFG_VALIDATION$problems %||% data.frame()
sub_prob  <- SUBSET_VALIDATION %||% data.frame()

n_dict_err <- if (is.data.frame(dict_prob) && nrow(dict_prob) > 0) sum(dict_prob$severity == "error", na.rm = TRUE) else 0L
n_dict_warn <- if (is.data.frame(dict_prob) && nrow(dict_prob) > 0) sum(dict_prob$severity == "warn", na.rm = TRUE) else 0L

n_cfg_err <- if (is.data.frame(cfg_prob) && nrow(cfg_prob) > 0) sum(cfg_prob$severity == "error", na.rm = TRUE) else 0L
n_cfg_warn <- if (is.data.frame(cfg_prob) && nrow(cfg_prob) > 0) sum(cfg_prob$severity == "warn", na.rm = TRUE) else 0L

n_sub_err <- if (is.data.frame(sub_prob) && nrow(sub_prob) > 0) sum(sub_prob$severity == "error", na.rm = TRUE) else 0L
n_sub_warn <- if (is.data.frame(sub_prob) && nrow(sub_prob) > 0) sum(sub_prob$severity == "warn", na.rm = TRUE) else 0L

log_info("DICT_VALIDATION   : errors=", n_dict_err, ", warnings=", n_dict_warn)
log_info("CFG_VALIDATION    : errors=", n_cfg_err, ", warnings=", n_cfg_warn)
log_info("SUBSET_VALIDATION : errors=", n_sub_err, ", warnings=", n_sub_warn)

if (is.data.frame(dict_prob) && nrow(dict_prob) > 0) {
  top_dict <- head(dict_prob, 10)
  for (i in seq_len(nrow(top_dict))) {
    log_warn(
      "[DICT_VALIDATION] ",
      top_dict$severity[i], " / ",
      top_dict$issue_type[i], " / ",
      top_dict$var_name[i], " / ",
      top_dict$detail[i]
    )
  }
}

if (is.data.frame(cfg_prob) && nrow(cfg_prob) > 0) {
  top_cfg <- head(cfg_prob, 10)
  for (i in seq_len(nrow(top_cfg))) {
    log_warn(
      "[CFG_VALIDATION] ",
      top_cfg$severity[i], " / ",
      top_cfg$issue_type[i], " / ",
      top_cfg$var_name[i], " / ",
      top_cfg$detail[i]
    )
  }
}

if (is.data.frame(sub_prob) && nrow(sub_prob) > 0) {
  top_sub <- head(sub_prob, 10)
  for (i in seq_len(nrow(top_sub))) {
    log_warn(
      "[SUBSET_VALIDATION] ",
      top_sub$severity[i], " / ",
      top_sub$subset_name[i], " / ",
      top_sub$message[i]
    )
  }
}

if ((n_dict_err + n_cfg_err + n_sub_err) > 0) {
  stop(
    "Input consistency validation failed. Check DICT_VALIDATION / CFG_VALIDATION / SUBSET_VALIDATION.",
    call. = FALSE
  )
}



# ------------------------------------------------------------
# 2. build DICT
# ------------------------------------------------------------
log_info("Building DICT ...")

DICT <- build_dict(
  dict_raw = DICT_RAW,
  raw_data = RAW_DATA,
  cfg = CFG
)

log_info(
  "DICT built: meta nrow = ",
  if (is.data.frame(DICT$meta)) nrow(DICT$meta) else 0L,
  ", levels nrow = ",
  if (is.data.frame(DICT$levels)) nrow(DICT$levels) else 0L
)

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

DICT_SUMMARY <- dict_summary(DICT)

# ------------------------------------------------------------
# 3. extract analysis variables
# ------------------------------------------------------------
log_info("Extracting analysis variables from DICT ...")

VARS <- extract_analysis_vars(DICT)

meta <- DICT$meta

if (!is.data.frame(meta) || nrow(meta) == 0) {
  stop("DICT$meta is empty.", call. = FALSE)
}

if (!"var_name" %in% names(meta)) {
  stop("DICT$meta must contain 'var_name'.", call. = FALSE)
}

meta$var_name <- trimws(as.character(meta$var_name))

# role 보정: role 없으면 analysis_role 사용
if (!"role" %in% names(meta)) {
  if ("analysis_role" %in% names(meta)) {
    meta$role <- meta$analysis_role
  } else {
    meta$role <- NA_character_
  }
}

meta$role <- tolower(trimws(as.character(meta$role)))
meta$role[is.na(meta$role) | trimws(meta$role) == ""] <- "none"

if (!"use" %in% names(meta)) {
  meta$use <- TRUE
}
meta$use <- as.logical(meta$use)
meta$use[is.na(meta$use)] <- TRUE

# ------------------------------------------------------------
# role 표준화
# ------------------------------------------------------------
meta$role <- dplyr::case_when(
  meta$role %in% c("indicator", "indicators") ~ "indicator",
  meta$role %in% c("covariate", "covariates") ~ "covariate",
  meta$role %in% c("outcome", "outcomes")     ~ "outcome",
  TRUE                                        ~ meta$role
)

# DICT에도 반영해 두기
DICT$meta <- meta

# ------------------------------------------------------------
# DICT 기반 추출
# ------------------------------------------------------------
INDICATORS <- meta |>
  dplyr::filter(.data$role == "indicator", .data$use) |>
  dplyr::pull(.data$var_name) |>
  unique()

COVARIATES <- meta |>
  dplyr::filter(.data$role == "covariate", .data$use) |>
  dplyr::pull(.data$var_name) |>
  unique()

OUTCOMES <- meta |>
  dplyr::filter(.data$role == "outcome", .data$use) |>
  dplyr::pull(.data$var_name) |>
  unique()

# ------------------------------------------------------------
# CFG 기반 outcome 추가 병합
# ------------------------------------------------------------
CFG_OUTCOMES <- CFG$outcomes %||% character(0)
CFG_OUTCOMES <- trimws(as.character(CFG_OUTCOMES))
CFG_OUTCOMES <- CFG_OUTCOMES[nzchar(CFG_OUTCOMES)]

OUTCOMES <- unique(c(OUTCOMES, CFG_OUTCOMES))

# ------------------------------------------------------------
# 실제 데이터에 존재하는 변수만 유지
# ------------------------------------------------------------
INDICATORS <- intersect(INDICATORS, names(RAW_DATA))
COVARIATES <- intersect(COVARIATES, names(RAW_DATA))
OUTCOMES   <- intersect(OUTCOMES, names(RAW_DATA))

log_info("n(indicators)              = ", length(INDICATORS))
log_info("n(covariates)              = ", length(COVARIATES))
log_info("n(outcomes)                = ", length(OUTCOMES))

if (length(OUTCOMES) > 0) {
  log_info("OUTCOMES                  = ", paste(OUTCOMES, collapse = ", "))
} else {
  log_warn("No OUTCOMES detected after DICT + CFG merge.")
}

# ------------------------------------------------------------
# 4. split indicator subtypes
# ------------------------------------------------------------
log_info("Splitting indicator subtypes ...")

IND_SPLIT <- split_indicator_subtypes(
  dict = DICT,
  raw_data = RAW_DATA
)

INDICATORS_CONTINUOUS  <- intersect(
  IND_SPLIT$indicators_continuous %||% character(0),
  INDICATORS
)
INDICATORS_CATEGORICAL <- intersect(
  IND_SPLIT$indicators_categorical %||% character(0),
  INDICATORS
)

if (length(INDICATORS_CONTINUOUS) == 0 &&
    length(INDICATORS_CATEGORICAL) == 0 &&
    length(INDICATORS) > 0) {
  guessed_subtype <- vapply(
    INDICATORS,
    function(v) guess_subtype_from_data(RAW_DATA[[v]]),
    character(1)
  )

  INDICATORS_CONTINUOUS  <- INDICATORS[guessed_subtype == "continuous"]
  INDICATORS_CATEGORICAL <- INDICATORS[guessed_subtype == "categorical"]
}

log_info("INDICATORS total        = ", length(INDICATORS))
log_info("INDICATORS continuous   = ", length(INDICATORS_CONTINUOUS))
log_info("INDICATORS categorical  = ", length(INDICATORS_CATEGORICAL))

# ------------------------------------------------------------
# 5. infer mixture type
# ------------------------------------------------------------
log_info("Inferring MIXTURE_TYPE ...")

MIXTURE_TYPE <- tolower(
  CFG$analysis$mixture_type %||%
    CFG$mixture_type %||%
    NA_character_
)
if (!is.na(MIXTURE_TYPE) && MIXTURE_TYPE %in% c("auto", "mixed_model", "mixed-model")) {
  MIXTURE_TYPE <- NA_character_
}

if (is.na(MIXTURE_TYPE) || !nzchar(MIXTURE_TYPE)) {
  if (length(INDICATORS_CATEGORICAL) > 0 && length(INDICATORS_CONTINUOUS) == 0) {
    MIXTURE_TYPE <- "lca"
  } else if (length(INDICATORS_CONTINUOUS) > 0 && length(INDICATORS_CATEGORICAL) == 0) {
    MIXTURE_TYPE <- "lpa"
  } else if (length(INDICATORS_CONTINUOUS) > 0 && length(INDICATORS_CATEGORICAL) > 0) {
    MIXTURE_TYPE <- "mixed"
  } else {
    MIXTURE_TYPE <- "lpa"
  }
}

if (!MIXTURE_TYPE %in% c("lca", "lpa", "mixed")) {
  stop("Invalid MIXTURE_TYPE: ", MIXTURE_TYPE, call. = FALSE)
}

log_info("MIXTURE_TYPE = ", MIXTURE_TYPE)

# ------------------------------------------------------------
# 6. build survey bundle
# ------------------------------------------------------------
log_info("Building survey bundle ...")

SURVEY_BUNDLE <- build_survey_bundle(
  cfg = CFG,
  dict = DICT,
  raw_data = RAW_DATA,
  subsets_spec = SUBSETS_SPEC
)

SURVEY_CASE <- SURVEY_BUNDLE$survey_case %||% "none"
WEIGHT_VAR  <- SURVEY_BUNDLE$weight_var  %||% NULL
STRATA_VAR  <- SURVEY_BUNDLE$strata_var  %||% NULL
CLUSTER_VAR <- SURVEY_BUNDLE$cluster_var %||% NULL
ID_VAR      <- SURVEY_BUNDLE$id_var      %||% NULL
SUBSET_VAR  <- SURVEY_BUNDLE$subset_var  %||% NULL
SUBSET_NAME <- SURVEY_BUNDLE$subset_name %||% NULL

log_info("Survey bundle built:")
log_info("  survey_case      = ", SURVEY_CASE)
log_info("  weight_var       = ", WEIGHT_VAR %||% "NULL")
log_info("  strata_var       = ", STRATA_VAR %||% "NULL")
log_info("  cluster_var      = ", CLUSTER_VAR %||% "NULL")
log_info("  subset_var       = ", SUBSET_VAR %||% "NULL")
log_info("  subset_name      = ", SUBSET_NAME %||% "NULL")
log_info("  id_var           = ", ID_VAR %||% "NULL")
log_info("  replicate_method = ", SURVEY_BUNDLE$replicate_method %||% "NULL")

# ------------------------------------------------------------
# 7. build SETTINGS_SUMMARY
# ------------------------------------------------------------
log_info("Building SETTINGS_SUMMARY ...")

SETTINGS_SUMMARY <- list(
  project_root = PROJECT_ROOT,
  dataset_id   = DATASET_ID,
  analysis_id  = ANALYSIS_ID,

  mixture_type = MIXTURE_TYPE,

  indicators               = INDICATORS,
  indicators_continuous    = INDICATORS_CONTINUOUS,
  indicators_categorical   = INDICATORS_CATEGORICAL,
  covariates               = COVARIATES,
  outcomes                 = OUTCOMES,

  weight_var  = WEIGHT_VAR,
  strata_var  = STRATA_VAR,
  cluster_var = CLUSTER_VAR,
  id_var      = ID_VAR,
  subset_var  = SUBSET_VAR,
  subset_name = SUBSET_NAME,

  survey_case = SURVEY_CASE,

  raw_n = nrow(RAW_DATA),
  raw_p = ncol(RAW_DATA),

  path_cfg     = PATH_CFG,
  path_dict    = PATH_DICT,
  path_missing = PATH_MISSING,
  path_subsets = PATH_SUBSETS,
  path_data    = PATH_DATA,

  created_at = Sys.time()
)

# backward-friendly uppercase aliases
SETTINGS_SUMMARY$MIXTURE_TYPE <- SETTINGS_SUMMARY$mixture_type
SETTINGS_SUMMARY$INDICATORS <- SETTINGS_SUMMARY$indicators
SETTINGS_SUMMARY$INDICATORS_CONTINUOUS <- SETTINGS_SUMMARY$indicators_continuous
SETTINGS_SUMMARY$INDICATORS_CATEGORICAL <- SETTINGS_SUMMARY$indicators_categorical
SETTINGS_SUMMARY$COVARIATES <- SETTINGS_SUMMARY$covariates
SETTINGS_SUMMARY$OUTCOMES <- SETTINGS_SUMMARY$outcomes
SETTINGS_SUMMARY$WEIGHT_VAR <- SETTINGS_SUMMARY$weight_var
SETTINGS_SUMMARY$STRATA_VAR <- SETTINGS_SUMMARY$strata_var
SETTINGS_SUMMARY$CLUSTER_VAR <- SETTINGS_SUMMARY$cluster_var
SETTINGS_SUMMARY$ID_VAR <- SETTINGS_SUMMARY$id_var
SETTINGS_SUMMARY$SUBSET_VAR <- SETTINGS_SUMMARY$subset_var
SETTINGS_SUMMARY$SUBSET_NAME <- SETTINGS_SUMMARY$subset_name
SETTINGS_SUMMARY$SURVEY_CASE <- SETTINGS_SUMMARY$survey_case

# ------------------------------------------------------------
# 8. save outputs
# ------------------------------------------------------------
log_info("Saving settings outputs ...")

save_named_rds_list(
  list(
    CFG = CFG,
    DICT_RAW = DICT_RAW,
    DICT = DICT,
    DICT_SUMMARY = DICT_SUMMARY,
    MISSING_SPEC = MISSING_SPEC,
    SUBSETS_SPEC = SUBSETS_SPEC,
    RAW_DATA = RAW_DATA,
    SETTINGS_SUMMARY = SETTINGS_SUMMARY,
    SURVEY_BUNDLE = SURVEY_BUNDLE,
    DICT_VALIDATION = DICT_VALIDATION,
    CFG_VALIDATION = CFG_VALIDATION,
    SUBSET_VALIDATION = SUBSET_VALIDATION,
    INPUT_FINGERPRINT = INPUT_FINGERPRINT
  ),
  dir_rds = DIR_RDS
)

# ------------------------------------------------------------
# 9. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_SETTINGS, units = "secs")), 2)

log_info("01_settings.R completed.")
log_info("n(indicators)              = ", length(INDICATORS))
log_info("n(indicators_continuous)   = ", length(INDICATORS_CONTINUOUS))
log_info("n(indicators_categorical)  = ", length(INDICATORS_CATEGORICAL))
log_info("n(covariates)              = ", length(COVARIATES))
log_info("n(outcomes)                = ", length(OUTCOMES))
log_info("elapsed                    = ", elapsed_sec, " sec")

log_step_end("settings", elapsed_sec, ok = TRUE)
