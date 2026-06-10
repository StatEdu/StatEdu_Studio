T0_PREP <- Sys.time()

log_step_start("PREP", "02_prep.R")
log_info("Preparing Mplus-ready latent transition data ...")

RAW_DATA <- load_step_rds("RAW_DATA", dir_rds = DIR_RDS, required = TRUE)
LTA_SPEC <- load_step_rds("LTA_SPEC", dir_rds = DIR_RDS, required = TRUE)

MPLUS_DATA <- build_lta_dataset(RAW_DATA, LTA_SPEC)

PREP_SUMMARY <- list(
  n_rows = nrow(MPLUS_DATA),
  n_ids = length(unique(stats::na.omit(MPLUS_DATA[[LTA_SPEC$id_var]]))),
  wave_order = LTA_SPEC$wave_order,
  n_classes = LTA_SPEC$n_classes,
  measurement_mode = LTA_SPEC$measurement_mode,
  indicator_count = length(LTA_SPEC$all_indicator_vars)
)

save_named_rds_list(
  list(
    MPLUS_DATA = MPLUS_DATA,
    PREP_SUMMARY = PREP_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_info("MPLUS_DATA rows = ", nrow(MPLUS_DATA), ", cols = ", ncol(MPLUS_DATA))
log_step_end("prep", round(as.numeric(difftime(Sys.time(), T0_PREP, units = "secs")), 2), ok = TRUE)
