T0_PANEL <- Sys.time()

log_step_start("PANEL_INGEST", "02_panel_ingest.R")
log_info("Building standardized panel objects ...")

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
RAW_DATA <- load_step_rds("RAW_DATA", dir_rds = DIR_RDS, default = data.frame())

PANEL_BUNDLE <- build_panel_bundle(
  cfg = CFG,
  raw_data = RAW_DATA,
  dir_data = DIR_DATA
)

save_named_rds_list(
  list(
    PANEL_LONG = PANEL_BUNDLE$PANEL_LONG,
    PANEL_WIDE = PANEL_BUNDLE$PANEL_WIDE,
    PANEL_META = PANEL_BUNDLE$PANEL_META
  ),
  dir_rds = DIR_RDS
)

log_info("PANEL_LONG rows = ", nrow(PANEL_BUNDLE$PANEL_LONG))
log_info("PANEL_WIDE rows = ", nrow(PANEL_BUNDLE$PANEL_WIDE))
log_step_end("panel_ingest", round(as.numeric(difftime(Sys.time(), T0_PANEL, units = "secs")), 2), ok = TRUE)
