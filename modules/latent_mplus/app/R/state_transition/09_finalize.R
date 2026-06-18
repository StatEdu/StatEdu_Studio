T0_FINAL <- Sys.time()

log_step_start("FINALIZE", "09_finalize.R")
log_info("Finalizing state-transition outputs ...")

if (exists("relocate_project_root_mplus_artifacts")) {
  stray_mplus_cleanup <- tryCatch(
    relocate_project_root_mplus_artifacts(PROJECT_ROOT, dir_mplus = DIR_MPLUS),
    error = function(e) data.frame()
  )
  if (is.data.frame(stray_mplus_cleanup) && nrow(stray_mplus_cleanup) > 0) {
    log_info("Relocated stray root-level Mplus artifacts: n = ", nrow(stray_mplus_cleanup))
  }
}

PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, default = list())
ESTIMATION_COLLECT_SUMMARY <- load_step_rds("ESTIMATION_COLLECT_SUMMARY", dir_rds = DIR_RDS, default = list())
FIGURE_SUMMARY <- load_step_rds("FIGURE_SUMMARY", dir_rds = DIR_RDS, default = list())
TABLE_SUMMARY <- load_step_rds("TABLE_SUMMARY", dir_rds = DIR_RDS, default = list())
TABLE_MANIFEST <- load_step_rds("TABLE_MANIFEST", dir_rds = DIR_RDS, default = data.frame())
FIGURE_MANIFEST <- load_step_rds("FIGURE_MANIFEST", dir_rds = DIR_RDS, default = data.frame())

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

summary_df <- data.frame(
  item = c("analysis_id", "dataset_id", "figure_created", "n_tables", "n_transition_rows", "excel_file"),
  value = c(
    ANALYSIS_ID,
    DATASET_ID,
    FIGURE_SUMMARY$created %||% FALSE,
    TABLE_SUMMARY$n_tables %||% 0L,
    PREP_SUMMARY$n_transition_rows %||% 0L,
    TABLE_SUMMARY$excel_file %||% PATH_FINAL_EXCEL
  ),
  stringsAsFactors = FALSE
)
write_csv_safe(summary_df, file.path(DIR_FINAL, "final_summary.csv"))

single_output_mode <- isTRUE(getOption("easyflow.single_output", TRUE))

if (!single_output_mode && file.exists(PATH_FINAL_EXCEL)) {
  file.copy(PATH_FINAL_EXCEL, file.path(DIR_FINAL_SUBMISSION, basename(PATH_FINAL_EXCEL)), overwrite = TRUE)
  file.copy(PATH_FINAL_EXCEL, file.path(DIR_FINAL_REVIEW, basename(PATH_FINAL_EXCEL)), overwrite = TRUE)
}

if (!single_output_mode && is.data.frame(TABLE_MANIFEST) && nrow(TABLE_MANIFEST) > 0) {
  write_csv_safe(TABLE_MANIFEST, file.path(DIR_FINAL_REVIEW, "TABLE_MANIFEST.csv"))
}

if (!single_output_mode && is.data.frame(FIGURE_MANIFEST) && nrow(FIGURE_MANIFEST) > 0) {
  write_csv_safe(FIGURE_MANIFEST, file.path(DIR_FINAL_REVIEW, "FIGURE_MANIFEST.csv"))
  figure_files <- unique(unlist(FIGURE_MANIFEST[, intersect(c("file_png", "file_tiff", "file_pdf"), names(FIGURE_MANIFEST)), drop = FALSE], use.names = FALSE))
  figure_files <- figure_files[!is.na(figure_files) & nzchar(figure_files) & file.exists(figure_files)]
  for (fp in figure_files) {
    file.copy(fp, file.path(DIR_FINAL_REVIEW, basename(fp)), overwrite = TRUE)
    file.copy(fp, file.path(DIR_FINAL_SUBMISSION, basename(fp)), overwrite = TRUE)
  }
}

log_step_end("finalize", round(as.numeric(difftime(Sys.time(), T0_FINAL, units = "secs")), 2), ok = TRUE)
