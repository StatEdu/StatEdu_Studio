T0_SETTINGS <- Sys.time()

log_step_start("SETTINGS", "01_settings.R")
log_info("Reading CFG / dictionary / raw data ...")

inputs <- read_pipeline_inputs(
  cfg_path = PATH_CFG,
  dict_path = PATH_DICT,
  missing_path = PATH_MISSING,
  subsets_path = PATH_SUBSETS,
  data_path = PATH_DATA,
  required_cfg = TRUE,
  required_dict = TRUE,
  required_data = TRUE
)

CFG <- inputs$CFG
DICT_RAW <- inputs$DICT_RAW
SUBSETS_SPEC <- inputs$SUBSETS_SPEC
RAW_DATA <- inputs$RAW_DATA

merge_cfg_lists_pm <- function(base, override) {
  if (is.null(base)) return(override)
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override)) return(override)
  out <- base
  for (nm in names(override)) {
    if (nm %in% names(out)) {
      out[[nm]] <- merge_cfg_lists_pm(out[[nm]], override[[nm]])
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

resolve_config_path_pm <- function(path, base_dir) {
  path <- as.character(path)[1]
  if (is.na(path) || !nzchar(path)) return(NA_character_)
  path <- gsub("\\\\", "/", path)
  if (grepl("^[A-Za-z]:/", path) || startsWith(path, "/")) return(path)
  file.path(base_dir, path)
}

process_cfg_path <- NA_character_
process_cfg_file <- as.character(CFG$process_config_file %||% "")[1]
if (nzchar(process_cfg_file)) {
  process_cfg_path <- resolve_config_path_pm(process_cfg_file, dirname(PATH_CFG))
  process_overlay_raw <- read_yaml_safe(process_cfg_path, default = list(), required = TRUE)
  process_overlay <- if (is.list(process_overlay_raw) && "process" %in% names(process_overlay_raw)) {
    process_overlay_raw$process
  } else {
    process_overlay_raw
  }
  CFG$process <- merge_cfg_lists_pm(CFG$process %||% list(), process_overlay %||% list())
}

apply_dictionary_aliases_pm <- function(raw_data, dict_raw) {
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0 || !is.data.frame(dict_raw) || nrow(dict_raw) == 0) {
    return(raw_data)
  }
  nms <- names(dict_raw)
  raw_col <- intersect(c("raw_var", "source_var", "raw_name", "rawname", "data_var", "original_var"), nms)
  alias_df <- data.frame(var_name = character(0), raw_var = character(0), stringsAsFactors = FALSE)
  if (length(raw_col) > 0 && "var_name" %in% nms) {
    alias_df <- dict_raw[, c("var_name", raw_col[1]), drop = FALSE]
    names(alias_df) <- c("var_name", "raw_var")
    alias_df$var_name <- trimws(as.character(alias_df$var_name))
    alias_df$raw_var <- trimws(as.character(alias_df$raw_var))
    alias_df <- alias_df[
      !is.na(alias_df$var_name) & nzchar(alias_df$var_name) &
        !is.na(alias_df$raw_var) & nzchar(alias_df$raw_var),
      ,
      drop = FALSE
    ]
  }
  if (identical(DATASET_ID, "07_complex")) {
    alias_df <- rbind(
      alias_df,
      data.frame(
        var_name = c("id", "sex", "marr", "edu", "religion", "group", "X", "W", "x3"),
        raw_var = c("pid", "P02", "P08", "P09", "P10", "SQ0", "x4", "x3", "x6"),
        stringsAsFactors = FALSE
      )
    )
    alias_df <- alias_df[!duplicated(alias_df$var_name), , drop = FALSE]
  }
  if (nrow(alias_df) == 0) return(raw_data)

  new_names <- names(raw_data)
  for (i in seq_len(nrow(alias_df))) {
    target_nm <- alias_df$var_name[i]
    source_nm <- alias_df$raw_var[i]
    hit <- match(source_nm, new_names)
    if (is.na(hit)) next
    if (target_nm %in% new_names && !identical(target_nm, source_nm)) next
    new_names[hit] <- target_nm
  }
  names(raw_data) <- new_names
  raw_data
}

pick_dict_vars_pm <- function(dict_meta, group_value = NULL, role_value = NULL) {
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0 || !"var_name" %in% names(dict_meta)) return(character(0))
  hit <- rep(TRUE, nrow(dict_meta))
  if (!is.null(group_value)) {
    group_hit <- rep(FALSE, nrow(dict_meta))
    if ("display_group" %in% names(dict_meta)) {
      group_hit <- group_hit | tolower(trimws(as.character(dict_meta$display_group))) %in% tolower(group_value)
    }
    role_col_group <- if ("role" %in% names(dict_meta)) "role" else if ("analysis_role" %in% names(dict_meta)) "analysis_role" else NULL
    if (!is.null(role_col_group)) {
      group_hit <- group_hit | tolower(trimws(as.character(dict_meta[[role_col_group]]))) %in% tolower(group_value)
    }
    hit <- hit & group_hit
  }
  if (!is.null(role_value)) {
    role_col <- if ("role" %in% names(dict_meta)) "role" else if ("analysis_role" %in% names(dict_meta)) "analysis_role" else NULL
    if (!is.null(role_col)) {
      hit <- hit & tolower(trimws(as.character(dict_meta[[role_col]]))) %in% tolower(role_value)
    }
  }
  out <- as.character(dict_meta$var_name[hit])
  unique(out[!is.na(out) & nzchar(out)])
}

RAW_DATA <- apply_dictionary_aliases_pm(RAW_DATA, DICT_RAW)

DICT <- build_dict(DICT_RAW, raw_data = RAW_DATA, cfg = CFG)
SURVEY_BUNDLE <- build_survey_bundle(
  cfg = CFG,
  dict = DICT,
  raw_data = RAW_DATA,
  subsets_spec = SUBSETS_SPEC
)

process_cfg <- CFG$process %||% list()
bch_cfg <- CFG$bch %||% list()
process_mplus_cfg <- process_cfg$mplus %||% list()
custom_model_cfg <- process_cfg$custom_model %||% list()
process_model_raw <- as.character(process_cfg$model %||% 1L)[1]
process_model_raw <- trimws(process_model_raw)
process_model_is_custom <- !is.na(process_model_raw) && nzchar(process_model_raw) &&
  tolower(process_model_raw) %in% c("custom", "user_defined", "user-defined", "manual")
custom_model_enabled <- isTRUE(custom_model_cfg$enabled %||% FALSE) || isTRUE(process_model_is_custom)
custom_model_label <- as.character(custom_model_cfg$label %||% "Custom")[1]
if (is.na(custom_model_label) || !nzchar(custom_model_label)) custom_model_label <- "Custom"

source_analysis_id <- as.character(process_cfg$source_analysis_id %||% "cross_sectional_mixture")[1]
process_model <- suppressWarnings(as.integer(process_model_raw))
if (isTRUE(process_model_is_custom)) {
  process_model <- 0L
} else if (is.na(process_model)) {
  process_model <- 1L
}
process_model_label <- if (isTRUE(process_model_is_custom)) "custom" else as.character(process_model)

moderator_source <- as.character(process_cfg$moderator_source %||% if (isTRUE(custom_model_enabled)) "observed" else if (process_model == 1L) "latent_class" else NA_character_)[1]
x_vars <- as.character(process_cfg$x %||% bch_cfg$moderators %||% character(0))
w_var <- as.character(process_cfg$w %||% if (identical(moderator_source, "latent_class")) "class" else character(0))[1]
y_vars <- as.character(process_cfg$y %||% bch_cfg$outcomes %||% character(0))
m_vars <- as.character(process_cfg$m %||% character(0))
covariates_mode <- as.character(process_cfg$covariates_mode %||% "dictionary")[1]
covariates_cfg <- as.character(process_cfg$covariates %||% character(0))
covariates_exclude <- as.character(process_cfg$covariates_exclude %||% character(0))
bootstrap_enabled <- isTRUE(process_cfg$bootstrap$enabled %||% TRUE)
bootstrap_n <- suppressWarnings(as.integer(process_cfg$bootstrap$n_boot %||% 5000L))
if (is.na(bootstrap_n) || !bootstrap_n %in% c(5000L, 50000L)) bootstrap_n <- 5000L
bootstrap_ci <- tolower(as.character(process_cfg$bootstrap$ci_type %||% "bca")[1])
if (!bootstrap_ci %in% c("bca", "percentile")) bootstrap_ci <- "bca"
id_var <- as.character(process_cfg$id_var %||% "id")[1]
observed_source <- (
  isTRUE(process_model == 1L) && !identical(moderator_source, "latent_class")
) || (
  isTRUE(process_model == 5L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 7L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 8L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 14L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 15L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 58L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 59L) && length(x_vars) > 0L && length(m_vars) > 0L && nzchar(w_var %||% "")
) || (
  isTRUE(process_model == 4L) && length(x_vars) > 0L && length(m_vars) > 0L
 ) || (
  isTRUE(process_model == 6L) && length(x_vars) > 0L && length(m_vars) >= 2L
)
if (isTRUE(custom_model_enabled)) observed_source <- TRUE
mplus_estimator <- toupper(as.character(
  process_mplus_cfg$estimator %||%
    CFG$mplus$estimator %||%
    CFG$estimation$estimator %||%
    "MLR"
)[1])
if (!nzchar(mplus_estimator) || is.na(mplus_estimator)) mplus_estimator <- "MLR"
centering_method <- tolower(as.character(process_mplus_cfg$center %||% "weighted_mean")[1])
if (!centering_method %in% c("weighted_mean", "mean", "none")) centering_method <- "weighted_mean"
probe_values <- tolower(as.character(process_mplus_cfg$probe_values %||% "mean_sd")[1])
if (!probe_values %in% c("mean_sd")) probe_values <- "mean_sd"
probe_sd_multiplier <- suppressWarnings(as.numeric(process_mplus_cfg$probe_sd_multiplier %||% 1))
if (is.na(probe_sd_multiplier) || probe_sd_multiplier <= 0) probe_sd_multiplier <- 1
use_subpopulation_in_mplus <- isTRUE(process_mplus_cfg$use_subpopulation %||% TRUE)
survey_case <- as.character(SURVEY_BUNDLE$survey_case %||% "none")[1]
variance_method <- if (identical(survey_case, "replicate")) {
  "replicate_design"
} else if (!identical(survey_case, "none")) {
  "survey_design"
} else {
  "bootstrap"
}
if (!identical(variance_method, "bootstrap")) bootstrap_enabled <- FALSE

x_vars <- unique(x_vars[!is.na(x_vars) & nzchar(x_vars)])
y_vars <- unique(y_vars[!is.na(y_vars) & nzchar(y_vars)])
m_vars <- unique(m_vars[!is.na(m_vars) & nzchar(m_vars)])

if (isTRUE(custom_model_enabled)) {
  custom_b_matrix <- custom_model_cfg$b_matrix %||% list()
  custom_row_vars <- names(custom_b_matrix)
  custom_row_vars <- as.character(custom_row_vars[!is.na(custom_row_vars) & nzchar(custom_row_vars)])
  custom_m_vars <- setdiff(custom_row_vars, y_vars)
  if (length(custom_m_vars) > 0L) {
    m_vars <- unique(c(custom_m_vars, m_vars))
  }
}

if (!isTRUE(custom_model_enabled) && isTRUE(process_model == 6L) && !length(m_vars) %in% c(2L, 3L)) {
  stop(
    "PROCESS model 6 requires 2 or 3 mediators in serial order. Current m count = ",
    length(m_vars),
    ".",
    call. = FALSE
  )
}
covariates_cfg <- unique(covariates_cfg[!is.na(covariates_cfg) & nzchar(covariates_cfg)])
covariates_exclude <- unique(covariates_exclude[!is.na(covariates_exclude) & nzchar(covariates_exclude)])

dict_meta <- DICT_RAW
if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0 || !all(c("var_name", "role") %in% names(dict_meta))) {
  dict_meta <- DICT$meta %||% DICT$META %||% data.frame()
}
if (length(x_vars) == 0L) x_vars <- pick_dict_vars_pm(dict_meta, group_value = "independent")
if (length(y_vars) == 0L) y_vars <- pick_dict_vars_pm(dict_meta, role_value = "outcome")
if (!nzchar(w_var %||% "")) {
  w_candidates <- pick_dict_vars_pm(dict_meta, group_value = "moderation")
  w_var <- as.character(w_candidates[1] %||% character(0))[1]
}
if (!nzchar(id_var %||% "") || identical(id_var, "id")) {
  id_candidates <- pick_dict_vars_pm(dict_meta, role_value = "id")
  if (length(id_candidates) > 0) id_var <- id_candidates[1]
}
dict_covariates <- character(0)
if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && all(c("var_name", "role") %in% names(dict_meta))) {
  dict_covariates <- as.character(dict_meta$var_name[tolower(as.character(dict_meta$role)) == "covariate"])
} else if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && all(c("var_name", "analysis_role") %in% names(dict_meta))) {
  dict_covariates <- as.character(dict_meta$var_name[tolower(as.character(dict_meta$analysis_role)) == "covariate"])
}
if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && all(c("var_name", "display_group") %in% names(dict_meta))) {
  dict_covariates <- unique(c(
    dict_covariates,
    as.character(dict_meta$var_name[tolower(as.character(dict_meta$display_group)) == "covariate"])
  ))
}
dict_covariates <- unique(dict_covariates[!is.na(dict_covariates) & nzchar(dict_covariates)])

if (identical(tolower(covariates_mode), "dictionary") && length(covariates_cfg) == 0L) {
  covariates <- dict_covariates
} else {
  covariates <- covariates_cfg
}
covariates <- setdiff(covariates, unique(c(x_vars, y_vars, m_vars, w_var, id_var)))
covariates <- setdiff(covariates, covariates_exclude)
covariates <- unique(covariates[!is.na(covariates) & nzchar(covariates)])

PROCESS_SETTINGS <- list(
  source_analysis_id = source_analysis_id,
  process_model_raw = process_model_raw,
  process_model = process_model,
  process_model_label = process_model_label,
  custom_model_enabled = custom_model_enabled,
  custom_model_label = custom_model_label,
  custom_model = custom_model_cfg,
  moderator_source = moderator_source,
  observed_source = observed_source,
  x_vars = x_vars,
  w_var = w_var,
  y_vars = y_vars,
  m_vars = m_vars,
  covariates_mode = covariates_mode,
  covariates = covariates,
  covariates_exclude = covariates_exclude,
  bootstrap_enabled = bootstrap_enabled,
  bootstrap_n = bootstrap_n,
  bootstrap_ci = bootstrap_ci,
  survey_case = survey_case,
  variance_method = variance_method,
  id_var = id_var,
  mplus_estimator = mplus_estimator,
  centering_method = centering_method,
  probe_values = probe_values,
  probe_sd_multiplier = probe_sd_multiplier,
  use_subpopulation_in_mplus = use_subpopulation_in_mplus,
  source_rds_dir = file.path(DIR_OUTPUT_BASE, source_analysis_id, "rds"),
  source_output_dir = file.path(DIR_OUTPUT_BASE, source_analysis_id)
)

SETTINGS_SUMMARY <- list(
  dataset_id = DATASET_ID,
  analysis_id = ANALYSIS_ID,
  source_analysis_id = source_analysis_id,
  process_model_raw = process_model_raw,
  process_model = process_model,
  process_model_label = process_model_label,
  custom_model_enabled = custom_model_enabled,
  custom_model_label = custom_model_label,
  moderator_source = moderator_source,
  observed_source = observed_source,
  n_x = length(x_vars),
  w_var = w_var,
  n_outcomes = length(y_vars),
  n_mediators = length(m_vars),
  covariates_mode = covariates_mode,
  n_covariates = length(covariates),
  survey_case = survey_case,
  variance_method = variance_method,
  bootstrap_enabled = bootstrap_enabled,
  bootstrap_n = bootstrap_n,
  id_var = id_var,
  mplus_estimator = mplus_estimator,
  centering_method = centering_method,
  use_subpopulation_in_mplus = use_subpopulation_in_mplus,
  created_at = Sys.time()
)

log_info("SOURCE_ANALYSIS_ID = ", source_analysis_id)
log_info("PROCESS_MODEL      = ", process_model_label)
log_info("CUSTOM_MODEL       = ", custom_model_enabled)
if (isTRUE(custom_model_enabled)) log_info("CUSTOM_LABEL       = ", custom_model_label)
log_info("MODERATOR_SOURCE   = ", moderator_source)
log_info("X                  = ", paste(x_vars, collapse = ", "))
log_info("W                  = ", w_var)
log_info("Y                  = ", paste(y_vars, collapse = ", "))
log_info("M                  = ", paste(m_vars, collapse = ", "))
log_info("COVARIATES_MODE    = ", covariates_mode)
log_info("COVARIATES         = ", paste(covariates, collapse = ", "))
log_info("SURVEY_CASE        = ", survey_case)
log_info("VARIANCE_METHOD    = ", variance_method)
log_info("OBSERVED_SOURCE    = ", observed_source)
log_info("BOOTSTRAP_ENABLED  = ", bootstrap_enabled)
log_info("BOOTSTRAP_N        = ", bootstrap_n)
log_info("MPLUS_ESTIMATOR    = ", mplus_estimator)
log_info("CENTERING_METHOD   = ", centering_method)
if (!is.na(process_cfg_path) && nzchar(process_cfg_path)) {
  log_info("PROCESS_CFG_FILE   = ", gsub("\\\\", "/", process_cfg_path))
}

save_named_rds_list(
  list(
    CFG = CFG,
    DICT = DICT,
    RAW_DATA = RAW_DATA,
    SURVEY_BUNDLE = SURVEY_BUNDLE,
    PROCESS_SETTINGS = PROCESS_SETTINGS,
    SETTINGS_SUMMARY = SETTINGS_SUMMARY
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_SETTINGS, units = "secs")), 2)
log_info("01_settings.R completed.")
log_info("n(raw rows)       = ", if (is.data.frame(RAW_DATA)) nrow(RAW_DATA) else 0L)
log_info("n(dict meta)      = ", if (is.data.frame(DICT$meta)) nrow(DICT$meta) else 0L)
log_info("n(x)              = ", length(x_vars))
log_info("n(outcomes)       = ", length(y_vars))
log_info("n(mediators)      = ", length(m_vars))
log_info("elapsed           = ", elapsed_sec, " sec")
log_step_end("settings", elapsed_sec, ok = TRUE)
