T0_FINAL <- Sys.time()

log_step_start("FINALIZE", "09_finalize.R")
log_info("Finalizing latent transition outputs ...")

PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, default = list())
ESTIMATION_COLLECT_SUMMARY <- load_step_rds("ESTIMATION_COLLECT_SUMMARY", dir_rds = DIR_RDS, default = list())
FIGURE_SUMMARY <- load_step_rds("FIGURE_SUMMARY", dir_rds = DIR_RDS, default = list())
TABLE_SUMMARY <- load_step_rds("TABLE_SUMMARY", dir_rds = DIR_RDS, default = list())
TABLE_MANIFEST <- load_step_rds("TABLE_MANIFEST", dir_rds = DIR_RDS, default = data.frame())

FINAL_SUMMARY <- list(
  analysis_id = ANALYSIS_ID,
  dataset_id = DATASET_ID,
  generated_at = as.character(Sys.time()),
  prep = PREP_SUMMARY,
  estimation = ESTIMATION_COLLECT_SUMMARY,
  figures = FIGURE_SUMMARY,
  tables = TABLE_SUMMARY
)

save_step_rds(FINAL_SUMMARY, "FINAL_SUMMARY", dir_rds = DIR_RDS)
write_csv_safe(
  data.frame(
    item = c("analysis_id", "dataset_id", "n_tables", "excel_file"),
    value = c(ANALYSIS_ID, DATASET_ID, TABLE_SUMMARY$n_tables %||% 0L, TABLE_SUMMARY$excel_file %||% PATH_FINAL_EXCEL),
    stringsAsFactors = FALSE
  ),
  file.path(DIR_FINAL, "final_summary.csv")
)

single_output_mode <- isTRUE(getOption("easyflow.single_output", TRUE))

if (!single_output_mode && file.exists(PATH_FINAL_EXCEL)) {
  file.copy(PATH_FINAL_EXCEL, file.path(DIR_FINAL_SUBMISSION, basename(PATH_FINAL_EXCEL)), overwrite = TRUE)
  file.copy(PATH_FINAL_EXCEL, file.path(DIR_FINAL_REVIEW, basename(PATH_FINAL_EXCEL)), overwrite = TRUE)
}
if (!single_output_mode && is.data.frame(TABLE_MANIFEST) && nrow(TABLE_MANIFEST) > 0) {
  write_csv_safe(TABLE_MANIFEST, file.path(DIR_FINAL_REVIEW, "TABLE_MANIFEST.csv"))
}

log_step_end("finalize", round(as.numeric(difftime(Sys.time(), T0_FINAL, units = "secs")), 2), ok = TRUE)
