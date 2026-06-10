T0_SETTINGS <- Sys.time()

log_step_start("SETTINGS", "01_settings.R")
log_info("Loading configuration for latent transition analysis ...")

CFG <- read_cfg(PATH_CFG, required = TRUE)
DICT_RAW <- read_dictionary_csv(PATH_DICT, required = FALSE)
RAW_DATA <- read_data_file(PATH_DATA, required = TRUE)
DICT <- build_dict(
  dict_raw = DICT_RAW,
  raw_data = RAW_DATA,
  cfg = CFG
)
LONGITUDINAL_SPEC <- resolve_longitudinal_spec(cfg = CFG, raw_data = RAW_DATA, dir_data = DIR_DATA)
LTA_SPEC <- resolve_lta_spec(cfg = CFG, raw_data = RAW_DATA, longitudinal_spec = LONGITUDINAL_SPEC)

DICT_META <- if (is.list(DICT) && is.data.frame(DICT$meta)) DICT$meta else data.frame()
if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && all(c("var_name", "use_var") %in% names(DICT_META))) {
  use_vars <- unique(as.character(DICT_META$var_name[isTRUE(DICT_META$use_var) | DICT_META$use_var]))
  use_vars <- use_vars[!is.na(use_vars) & nzchar(use_vars)]
  LTA_SPEC$indicators_by_wave <- lapply(LTA_SPEC$indicators_by_wave, function(vars) {
    vars <- as.character(vars)
    vars[vars %in% use_vars]
  })
  LTA_SPEC$indicators_by_wave <- LTA_SPEC$indicators_by_wave[vapply(LTA_SPEC$indicators_by_wave, length, integer(1)) > 0L]
  LTA_SPEC$wave_order <- LTA_SPEC$wave_order[LTA_SPEC$wave_order %in% names(LTA_SPEC$indicators_by_wave)]
  LTA_SPEC$all_indicator_vars <- unique(unlist(LTA_SPEC$indicators_by_wave, use.names = FALSE))
  LTA_SPEC$missing_vars <- if (is.data.frame(RAW_DATA) && nrow(RAW_DATA) > 0) {
    setdiff(c(LTA_SPEC$id_var, LTA_SPEC$all_indicator_vars), names(RAW_DATA))
  } else {
    character(0)
  }
  if (identical(LTA_SPEC$measurement_mode, "lca") && is.data.frame(RAW_DATA) && nrow(RAW_DATA) > 0) {
    LTA_SPEC$category_counts <- stats::setNames(
      vapply(LTA_SPEC$all_indicator_vars, function(v) length(sort(unique(stats::na.omit(RAW_DATA[[v]])))), integer(1)),
      LTA_SPEC$all_indicator_vars
    )
    LTA_SPEC$threshold_counts <- stats::setNames(
      vapply(LTA_SPEC$all_indicator_vars, function(v) {
        u <- sort(unique(stats::na.omit(RAW_DATA[[v]])))
        max(length(u) - 1L, 1L)
      }, integer(1)),
      LTA_SPEC$all_indicator_vars
    )
  }
}

if (!isTRUE(LTA_SPEC$enabled)) {
  stop("CFG.yml latent_transition.enabled must be TRUE to run this pipeline.", call. = FALSE)
}
if (length(LTA_SPEC$wave_order) < 2L) {
  stop("latent_transition.wave_order must contain at least two waves.", call. = FALSE)
}
if (length(LTA_SPEC$all_indicator_vars) == 0L) {
  stop("latent_transition.indicators_by_wave is missing or empty.", call. = FALSE)
}
if (length(LTA_SPEC$missing_vars) > 0L) {
  stop("Missing latent-transition variables: ", paste(LTA_SPEC$missing_vars, collapse = ", "), call. = FALSE)
}
if (identical(LTA_SPEC$measurement_mode, "lca")) {
  bad_vars <- names(LTA_SPEC$category_counts)[LTA_SPEC$category_counts < 2L]
  if (length(bad_vars) > 0L) {
    bad_txt <- paste0(bad_vars, "(", LTA_SPEC$category_counts[bad_vars], " category)")
    stop(
      "LTA cannot run because some categorical indicators have fewer than 2 observed categories: ",
      paste(bad_txt, collapse = ", "),
      ". Use actual repeated indicators or remove non-varying waves/variables.",
      call. = FALSE
    )
  }
}

SETTINGS_SUMMARY <- list(
  analysis_id = ANALYSIS_ID,
  analysis_type = "latent_transition",
  id_var = LTA_SPEC$id_var,
  wave_order = LTA_SPEC$wave_order,
  n_classes = LTA_SPEC$n_classes,
  measurement_mode = LTA_SPEC$measurement_mode,
  invariance = LTA_SPEC$invariance
)

save_named_rds_list(
  list(
    CFG = CFG,
    DICT_RAW = DICT_RAW,
    DICT = DICT,
    RAW_DATA = RAW_DATA,
    LONGITUDINAL_SPEC = LONGITUDINAL_SPEC,
    LTA_SPEC = LTA_SPEC,
    SETTINGS_SUMMARY = SETTINGS_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("settings", round(as.numeric(difftime(Sys.time(), T0_SETTINGS, units = "secs")), 2), ok = TRUE)
