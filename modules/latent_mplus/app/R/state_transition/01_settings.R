T0_SETTINGS <- Sys.time()

log_step_start("SETTINGS", "01_settings.R")
log_info("Loading configuration for state transition analysis ...")

CFG <- read_cfg(PATH_CFG, required = TRUE)
DICT_RAW <- read_dictionary_csv(PATH_DICT, required = FALSE)
MISSING_SPEC <- read_missing_yml(PATH_MISSING, required = FALSE)
SUBSETS_SPEC <- read_subsets_yml(PATH_SUBSETS, required = FALSE)

LONGITUDINAL_SPEC <- resolve_longitudinal_spec(
  cfg = CFG,
  raw_data = NULL,
  dir_data = DIR_DATA
)

needs_primary_data <- LONGITUDINAL_SPEC$data_layout %in% c("long", "wide")
RAW_DATA <- if (isTRUE(needs_primary_data)) {
  read_data_file(PATH_DATA, required = TRUE)
} else {
  data.frame()
}

DICT <- build_dict(
  dict_raw = DICT_RAW,
  raw_data = RAW_DATA,
  cfg = CFG
)

DICT_SUMMARY <- dict_summary(DICT)

DICT_META <- if (is.list(DICT) && is.data.frame(DICT$meta)) DICT$meta else data.frame()
DICT_COVARIATES <- character(0)
if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && "var_name" %in% names(DICT_META)) {
  role_col <- if ("analysis_role" %in% names(DICT_META)) "analysis_role" else if ("role" %in% names(DICT_META)) "role" else NULL
  use_col <- if ("use_var" %in% names(DICT_META)) "use_var" else if ("use" %in% names(DICT_META)) "use" else NULL
  if (!is.null(role_col)) {
    role_vals <- tolower(trimws(as.character(DICT_META[[role_col]])))
    use_vals <- if (!is.null(use_col)) as.logical(DICT_META[[use_col]]) else rep(TRUE, nrow(DICT_META))
    use_vals[is.na(use_vals)] <- TRUE
    DICT_COVARIATES <- unique(as.character(DICT_META$var_name[role_vals == "covariate" & use_vals]))
    DICT_COVARIATES <- DICT_COVARIATES[nzchar(DICT_COVARIATES)]
  }
}

CFG$longitudinal$invariant_vars <- DICT_COVARIATES
CFG$longitudinal$baseline_covariates <- DICT_COVARIATES
CFG$state_transition$baseline_covariates <- DICT_COVARIATES

LONGITUDINAL_SPEC$invariant_vars <- DICT_COVARIATES

SURVEY_BUNDLE <- build_survey_bundle(
  cfg = CFG,
  dict = DICT,
  raw_data = if (is.data.frame(RAW_DATA) && nrow(RAW_DATA) > 0) RAW_DATA else NULL,
  subsets_spec = SUBSETS_SPEC
)

SETTINGS_SUMMARY <- list(
  analysis_id = ANALYSIS_ID,
  analysis_type = "state_transition",
  data_layout = LONGITUDINAL_SPEC$data_layout,
  id_var = LONGITUDINAL_SPEC$id_var,
  time_var = LONGITUDINAL_SPEC$time_var,
  state_var = LONGITUDINAL_SPEC$state_var,
  wave_order = LONGITUDINAL_SPEC$wave_order,
  invariant_vars = LONGITUDINAL_SPEC$invariant_vars,
  path_data = PATH_DATA
)

save_named_rds_list(
  list(
    CFG = CFG,
    DICT_RAW = DICT_RAW,
    DICT = DICT,
    DICT_SUMMARY = DICT_SUMMARY,
    MISSING_SPEC = MISSING_SPEC,
    SUBSETS_SPEC = SUBSETS_SPEC,
    RAW_DATA = RAW_DATA,
    LONGITUDINAL_SPEC = LONGITUDINAL_SPEC,
    SURVEY_BUNDLE = SURVEY_BUNDLE,
    SETTINGS_SUMMARY = SETTINGS_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_info("State-transition settings saved.")
log_step_end("settings", round(as.numeric(difftime(Sys.time(), T0_SETTINGS, units = "secs")), 2), ok = TRUE)
