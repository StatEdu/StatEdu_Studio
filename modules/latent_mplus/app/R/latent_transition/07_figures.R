T0_FIG <- Sys.time()

log_step_start("FIGURES", "07_figures.R")
log_info("No default figures are generated for latent transition skeleton.")

FIGURE_MANIFEST <- data.frame()
FIGURE_SUMMARY <- list(created = FALSE, figure_dir = DIR_FIGURES, n_figures = 0L, created_at = Sys.time())

save_named_rds_list(
  list(
    FIGURE_MANIFEST = FIGURE_MANIFEST,
    FIGURE_SUMMARY = FIGURE_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("figures", round(as.numeric(difftime(Sys.time(), T0_FIG, units = "secs")), 2), ok = TRUE)
