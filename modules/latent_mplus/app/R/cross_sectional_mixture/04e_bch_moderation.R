# ============================================================
# 04e_bch_moderation.R
# Stratified BCH moderation
# ============================================================

T0_BCH_MOD <- Sys.time()
log_step_start("BCH_MODERATION", "04e_bch_moderation.R")
log_info("Reloading BCH moderation prerequisites ...")

PATH_BCH_CORE <- file.path(DIR_COMMON, "10_bch_core.R")
if (file.exists(PATH_BCH_CORE)) {
  source(PATH_BCH_CORE, local = TRUE)
} else {
  stop("Missing common BCH core file: ", PATH_BCH_CORE)
}

ensure_df2 <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  as.data.frame(x, stringsAsFactors = FALSE)
}

as_flag2 <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  if (is.logical(x)) return(isTRUE(x))
  x <- tolower(trimws(as.character(x)[1]))
  x %in% c("true", "t", "1", "yes", "y")
}

first_existing_col2 <- function(df, cand) {
  hit <- cand[cand %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

resolve_value_label_local2 <- function(var_name, value, DICT) {
  dict_levels              <- tryCatch(get_dict_levels_tbl(DICT), error = function(e) data.frame())
  if (!is.data.frame(dict_levels) || nrow(dict_levels) == 0) return(as.character(value))
  hit                      <- dict_levels[dict_levels$var_name == var_name, , drop = FALSE]
  if (nrow(hit) == 0) return(as.character(value))
  val_col                  <- first_existing_col2(hit, c("value", "level", "code"))
  lab_col                  <- first_existing_col2(hit, c("label_en", "label_ko", "label", "value_label"))
  if (is.na(val_col) || is.na(lab_col)) return(as.character(value))
  hit2                     <- hit[as.character(hit[[val_col]]) == as.character(value), , drop = FALSE]
  if (nrow(hit2) == 0) return(as.character(value))
  out                      <- hit2[[lab_col]][1]
  if (is.na(out) || !nzchar(as.character(out))) return(as.character(value))
  as.character(out)
}

empty_bch_stratified_full <- function() data.frame(
  moderator_var=character(0), moderator_level=character(0), moderator_label=character(0),
  analysis     =character(0), model_type=character(0), method=character(0), best_k=integer(0), best_tag=character(0), model_structure=character(0),
  outcome      =character(0), var_name=character(0), var_label=character(0), outcome_type=character(0), class=character(0), class_num=integer(0),
  estimate     =numeric(0), se=numeric(0), stat=numeric(0), df=numeric(0), p=numeric(0), p_fmt=character(0), sig=character(0),
  omnibus_p    =numeric(0), omnibus_p_fmt=character(0), omnibus_sig=character(0), inp_file=character(0), out_file=character(0), stringsAsFactors=FALSE)

empty_bch_stratified_posthoc <- function() data.frame(
  moderator_var=character(0), moderator_level=character(0), moderator_label=character(0),
  analysis     =character(0), model_type=character(0), method=character(0), best_k=integer(0), best_tag=character(0), model_structure=character(0),
  outcome      =character(0), var_name=character(0), var_label=character(0), outcome_type=character(0),
  class1       =integer(0), class2=integer(0), stat=numeric(0), p=numeric(0), p_fmt=character(0), sig=character(0), stringsAsFactors=FALSE)

empty_bch_stratified_omnibus <- function() data.frame(
  moderator_var=character(0), moderator_level=character(0), moderator_label=character(0),
  var_name     =character(0), var_label=character(0), chi_sq=numeric(0), df=numeric(0), p=numeric(0), p_fmt=character(0), sig=character(0), stringsAsFactors=FALSE)

BEST_K_SUMMARY                                    <- load_step_rds("BEST_K_SUMMARY", dir_rds = DIR_RDS, default = NULL)
if (is.null(BEST_K_SUMMARY)) BEST_K_SUMMARY       <- load_step_rds("SELECT_BEST_K_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
DICT                                              <- load_step_rds("DICT", dir_rds = DIR_RDS, default = list())
CFG                                               <- load_step_rds("CFG",  dir_rds = DIR_RDS, required = TRUE)
MPLUS_EXPORT_DATA                                 <- load_step_rds("MPLUS_EXPORT_DATA", dir_rds = DIR_RDS, default = NULL)
if (is.null(MPLUS_EXPORT_DATA)) MPLUS_EXPORT_DATA <- load_step_rds("MPLUS_DATA", dir_rds = DIR_RDS, required = TRUE)
SETTINGS_SUMMARY                                  <- load_step_rds("SETTINGS_SUMMARY", dir_rds = DIR_RDS, default = data.frame())

if (!is.list(CFG)) CFG                                    <- list()
if (is.null(CFG$data) || !is.list(CFG$data)) CFG$data     <- list()
if (is.null(CFG$data$missing_code)) CFG$data$missing_code <- -9999
best_k                                                    <- as.integer(BEST_K_SUMMARY$best_k %||% BEST_K_SUMMARY$BEST_K %||% NA_integer_)
best_tag                                                  <- as.character(BEST_K_SUMMARY$best_tag %||% BEST_K_SUMMARY$BEST_TAG %||% NA_character_)
best_model_structure                                      <- tolower(as.character(BEST_K_SUMMARY$best_model_structure %||% BEST_K_SUMMARY$BEST_MODEL_STRUCTURE %||% NA_character_))
BCH_CFG                                                   <- CFG$bch %||% list(); MOD_CFG <- BCH_CFG$moderation %||% list()
BCH_MODERATION_ENABLED                                    <- as_flag2(MOD_CFG$enabled, FALSE)
MODERATOR_VAR                                             <- trimws(as.character(MOD_CFG$moderator_var %||% ""))
RUN_STRATIFIED                                            <- as_flag2(MOD_CFG$run_stratified, TRUE)
OUTCOMES                                                  <- unique(as.character(BCH_CFG$distal_vars %||% BCH_CFG$outcomes %||% character(0)))
OUTCOMES                                                  <- OUTCOMES[nzchar(OUTCOMES)]
RAW_DATA                                                  <- ensure_df2(MPLUS_EXPORT_DATA)
MISSING_CODE                                              <- CFG$data$missing_code %||% -9999
DATASET_ID                                                <- if (is.data.frame(SETTINGS_SUMMARY) && "dataset_id" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) as.character(SETTINGS_SUMMARY$dataset_id[1]) else (CFG$dataset_id %||% "dataset")
MIXTURE_TYPE                                              <- if (is.data.frame(SETTINGS_SUMMARY) && "mixture_type" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) tolower(as.character(SETTINGS_SUMMARY$mixture_type[1])) else tolower(CFG$mixture_type %||% "lpa")
WEIGHT_VAR                                                <- if (is.data.frame(SETTINGS_SUMMARY) && "weight_var" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) SETTINGS_SUMMARY$weight_var[1] %||% NULL else CFG$survey_design$weight_var %||% NULL
STRATA_VAR                                                <- if (is.data.frame(SETTINGS_SUMMARY) && "strata_var" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) SETTINGS_SUMMARY$strata_var[1] %||% NULL else CFG$survey_design$strata_var %||% NULL
CLUSTER_VAR                                               <- if (is.data.frame(SETTINGS_SUMMARY) && "cluster_var" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) SETTINGS_SUMMARY$cluster_var[1] %||% NULL else CFG$survey_design$cluster_var %||% NULL
ID_VAR                                                    <- if (is.data.frame(SETTINGS_SUMMARY) && "id_var" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) SETTINGS_SUMMARY$id_var[1] %||% NULL else CFG$data$id_var %||% NULL
INDICATORS                                                <- if (is.data.frame(SETTINGS_SUMMARY) && "indicator_vars" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) unlist(SETTINGS_SUMMARY$indicator_vars[1], use.names = FALSE) else CFG$indicators %||% character(0)
INDICATORS                                                <- unique(as.character(INDICATORS)); INDICATORS <- INDICATORS[nzchar(INDICATORS)]
INDICATORS_CATEGORICAL                                    <- if (is.data.frame(SETTINGS_SUMMARY) && "indicator_categorical" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) unlist(SETTINGS_SUMMARY$indicator_categorical[1], use.names = FALSE) else character(0)
INDICATORS_CATEGORICAL                                    <- unique(as.character(INDICATORS_CATEGORICAL)); INDICATORS_CATEGORICAL <- INDICATORS_CATEGORICAL[nzchar(INDICATORS_CATEGORICAL)]
INDICATORS_CONTINUOUS                                     <- if (is.data.frame(SETTINGS_SUMMARY) && "indicator_continuous" %in% names(SETTINGS_SUMMARY) && nrow(SETTINGS_SUMMARY) > 0) unlist(SETTINGS_SUMMARY$indicator_continuous[1], use.names = FALSE) else character(0)
INDICATORS_CONTINUOUS                                     <- unique(as.character(INDICATORS_CONTINUOUS)); INDICATORS_CONTINUOUS <- INDICATORS_CONTINUOUS[nzchar(INDICATORS_CONTINUOUS)]
RUN_MPLUS                                                 <- isTRUE(CFG$run_mplus %||% TRUE)
MPLUS_EXE                                                 <- CFG$mplus$exe %||% Sys.getenv("MPLUS_EXE", unset = "")

DIR_MPLUS_BCH_MOD                                         <- file.path(DIR_MPLUS, "bch_moderation")
DIR_MPLUS_BCH_MOD_INP                                     <- file.path(DIR_MPLUS_BCH_MOD, "inp")
DIR_MPLUS_BCH_MOD_DATA                                    <- file.path(DIR_MPLUS_BCH_MOD, "data")
dir.create(DIR_MPLUS_BCH_MOD, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MPLUS_BCH_MOD_INP, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MPLUS_BCH_MOD_DATA, recursive = TRUE, showWarnings = FALSE)

if (!BCH_MODERATION_ENABLED || !RUN_STRATIFIED || !nzchar(MODERATOR_VAR)) {
  log_warn("BCH moderation disabled or moderator missing.")
  BCH_STRATIFIED_RESULTS_FULL <- empty_bch_stratified_full(); BCH_STRATIFIED_POSTHOC <- empty_bch_stratified_posthoc(); BCH_STRATIFIED_OMNIBUS <- empty_bch_stratified_omnibus(); BCH_STRATIFIED_RESULTS <- data.frame()
  BCH_MODERATION_METADATA <- data.frame(moderator_var=MODERATOR_VAR,best_k=best_k,best_tag=best_tag,model_structure=best_model_structure,enabled=BCH_MODERATION_ENABLED,run_stratified=RUN_STRATIFIED,n_levels=0,stringsAsFactors=FALSE)
} else if (!MODERATOR_VAR %in% names(RAW_DATA)) {
  stop("Moderator variable not found in data: ", MODERATOR_VAR)
} else {
  OUTCOMES                                                 <- intersect(OUTCOMES, names(RAW_DATA))
  moderator_levels                                         <- sort(unique(stats::na.omit(RAW_DATA[[MODERATOR_VAR]]))); moderator_levels <- as.character(moderator_levels)
  BCH_STRATIFIED_LIST                                      <- list(); BCH_STRATIFIED_POST <- list(); BCH_STRATIFIED_OM_LIST <- list()
  for (lv in moderator_levels) {
    log_info("Running stratified BCH: ", MODERATOR_VAR, " = ", lv)
    idx                                                    <- !is.na(RAW_DATA[[MODERATOR_VAR]]) & as.character(RAW_DATA[[MODERATOR_VAR]]) == lv
    DATA_SUB                                               <- RAW_DATA[idx, , drop = FALSE]
    if (!is.data.frame(DATA_SUB) || nrow(DATA_SUB) == 0) next
    level_stub                                             <- gsub("[^A-Za-z0-9_]+", "_", paste0(MODERATOR_VAR, "_", lv))
    BCH_DATA_FILE_SUB                                      <- file.path(DIR_MPLUS_BCH_MOD_DATA, paste0(tolower(DATASET_ID), "_bch_", level_stub, ".dat"))
    RES_SUB                                                <- run_bch_basic_subset(
      DATA_SUB                  = DATA_SUB, DICT = DICT, CFG = CFG, OUTCOMES = OUTCOMES,
      best_k                    = best_k, best_tag = best_tag, best_model_structure = best_model_structure,
      DATASET_ID                = paste0(DATASET_ID, "_", level_stub), RUN_MPLUS = RUN_MPLUS, MPLUS_EXE = MPLUS_EXE,
      INDICATORS                = INDICATORS, INDICATORS_CATEGORICAL = INDICATORS_CATEGORICAL, INDICATORS_CONTINUOUS = INDICATORS_CONTINUOUS,
      WEIGHT_VAR                = WEIGHT_VAR, STRATA_VAR = STRATA_VAR, CLUSTER_VAR = CLUSTER_VAR, ID_VAR = ID_VAR,
      MISSING_CODE              = MISSING_CODE, MIXTURE_TYPE = MIXTURE_TYPE,
      DIR_MPLUS_BCH_INP         = DIR_MPLUS_BCH_MOD_INP, BCH_DATA_FILE = BCH_DATA_FILE_SUB, BCH_DATA = DATA_SUB,
      build_bch_input_fun       = build_bch_input, parse_bch_model_results_fun = parse_bch_model_results,
      make_bch_outcome_spec_fun = build_bch_outcome_spec,
      read_mplus_out_lines_fun  = read_mplus_out_lines, run_mplus_model_fun = run_mplus_model, write_mplus_data_fun = write_mplus_data
    )
    df_full_sub                                            <- ensure_df2(RES_SUB$BCH_RESULTS_FULL); df_post_sub <- ensure_df2(RES_SUB$BCH_POSTHOC); df_om_sub <- ensure_df2(RES_SUB$BCH_OMNIBUS_BASIC)
    lv_label                                               <- resolve_value_label_local2(MODERATOR_VAR, lv, DICT)
    if (nrow(df_full_sub) > 0) { df_full_sub$moderator_var <- MODERATOR_VAR; df_full_sub$moderator_level <- lv; df_full_sub$moderator_label <- lv_label; BCH_STRATIFIED_LIST[[length(BCH_STRATIFIED_LIST)+1L]] <- df_full_sub }
    if (nrow(df_post_sub) > 0) { df_post_sub$moderator_var <- MODERATOR_VAR; df_post_sub$moderator_level <- lv; df_post_sub$moderator_label <- lv_label; BCH_STRATIFIED_POST[[length(BCH_STRATIFIED_POST)+1L]] <- df_post_sub }
    if (nrow(df_om_sub) > 0) { df_om_sub$moderator_var     <- MODERATOR_VAR; df_om_sub$moderator_level <- lv; df_om_sub$moderator_label <- lv_label; BCH_STRATIFIED_OM_LIST[[length(BCH_STRATIFIED_OM_LIST)+1L]] <- df_om_sub }
  }
  BCH_STRATIFIED_RESULTS_FULL <- if (length(BCH_STRATIFIED_LIST) == 0) empty_bch_stratified_full() else do.call(rbind, BCH_STRATIFIED_LIST)
  rownames(BCH_STRATIFIED_RESULTS_FULL) <- NULL
  BCH_STRATIFIED_POSTHOC <- if (length(BCH_STRATIFIED_POST) == 0) empty_bch_stratified_posthoc() else do.call(rbind, BCH_STRATIFIED_POST)
  if (!"sig" %in% names(BCH_STRATIFIED_POSTHOC) && "p" %in% names(BCH_STRATIFIED_POSTHOC)) BCH_STRATIFIED_POSTHOC$sig <- p_to_sig_tbl(BCH_STRATIFIED_POSTHOC$p)
  rownames(BCH_STRATIFIED_POSTHOC) <- NULL
  BCH_STRATIFIED_OMNIBUS <- if (length(BCH_STRATIFIED_OM_LIST) == 0) empty_bch_stratified_omnibus() else do.call(rbind, BCH_STRATIFIED_OM_LIST)
  if (!"sig" %in% names(BCH_STRATIFIED_OMNIBUS) && "p" %in% names(BCH_STRATIFIED_OMNIBUS)) BCH_STRATIFIED_OMNIBUS$sig <- p_to_sig_tbl(BCH_STRATIFIED_OMNIBUS$p)
  if (!"p_fmt" %in% names(BCH_STRATIFIED_OMNIBUS) && "p" %in% names(BCH_STRATIFIED_OMNIBUS)) BCH_STRATIFIED_OMNIBUS$p_fmt <- fmt_p_tbl(BCH_STRATIFIED_OMNIBUS$p)
  rownames(BCH_STRATIFIED_OMNIBUS)                                       <- NULL
  BCH_STRATIFIED_RESULTS                                                 <- if (nrow(BCH_STRATIFIED_RESULTS_FULL) == 0) data.frame() else {
    x                                                                    <- BCH_STRATIFIED_RESULTS_FULL
    if (!"class_num" %in% names(x) && "class" %in% names(x)) x$class_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", x$class)))
    x                                                                    <- x[!is.na(x$class_num), , drop = FALSE]
    if (nrow(x) == 0) {
      data.frame()
    } else {
    x$Profile                                                            <- ifelse(!is.na(x$class_num), paste0("Profile ", x$class_num), as.character(x$class))
    x$value_cell                                                         <- paste0(formatC(x$estimate, format = "f", digits = 2), "(", formatC(x$se, format = "f", digits = 2), ")")
    wide                                                                 <- reshape(x[, c("moderator_var", "moderator_level", "moderator_label", "var_name", "var_label", "Profile", "value_cell")], idvar = c("moderator_var", "moderator_level", "moderator_label", "var_name", "var_label"), timevar = "Profile", direction = "wide")
    names(wide)                                                          <- sub("^value_cell\\.", "", names(wide))
    if (nrow(BCH_STRATIFIED_OMNIBUS) > 0) {
      om                                                                 <- BCH_STRATIFIED_OMNIBUS[, c("moderator_var", "moderator_level", "var_name", "chi_sq", "df", "p", "p_fmt", "sig"), drop = FALSE]
      wide                                                               <- merge(wide, om, by = c("moderator_var", "moderator_level", "var_name"), all.x = TRUE)
    }
    rownames(wide)                                                       <- NULL
    wide
    }
  }
  BCH_MODERATION_METADATA <- data.frame(moderator_var=MODERATOR_VAR,best_k=best_k,best_tag=best_tag,model_structure=best_model_structure,enabled=BCH_MODERATION_ENABLED,run_stratified=RUN_STRATIFIED,n_levels=length(moderator_levels),stringsAsFactors=FALSE)
}

save_named_rds_list(list(BCH_STRATIFIED_RESULTS_FULL=BCH_STRATIFIED_RESULTS_FULL, BCH_STRATIFIED_RESULTS=BCH_STRATIFIED_RESULTS, BCH_STRATIFIED_POSTHOC=BCH_STRATIFIED_POSTHOC, BCH_STRATIFIED_OMNIBUS=BCH_STRATIFIED_OMNIBUS, BCH_MODERATION_METADATA=BCH_MODERATION_METADATA), dir_rds = DIR_RDS)
if (exists("DIR_TABLES") && !is.null(DIR_TABLES)) {
  write_csv_safe(BCH_STRATIFIED_RESULTS_FULL, file.path(DIR_TABLES, "bch_stratified_results_full.csv"))
  write_csv_safe(BCH_STRATIFIED_RESULTS,      file.path(DIR_TABLES, "bch_stratified_results.csv"))
  write_csv_safe(BCH_STRATIFIED_POSTHOC,      file.path(DIR_TABLES, "bch_stratified_posthoc.csv"))
  write_csv_safe(BCH_STRATIFIED_OMNIBUS,      file.path(DIR_TABLES, "bch_stratified_omnibus.csv"))
  write_csv_safe(BCH_MODERATION_METADATA,     file.path(DIR_TABLES, "bch_moderation_metadata.csv"))
}
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_BCH_MOD, units = "secs")), 2)
log_info("04e_bch_moderation.R completed.")
log_info("Elapsed seconds: ", elapsed_sec)
