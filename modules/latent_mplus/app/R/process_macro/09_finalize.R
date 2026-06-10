T0_FINALIZE <- Sys.time()

log_step_start("FINALIZE", "09_finalize.R")
log_info("Reloading process finalization inputs ...")

`%||%` <- function(x, y) if (is.null(x)) y else x
ensure_dir_local <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

PROCESS_SETTINGS <- load_step_rds("PROCESS_SETTINGS", dir_rds = DIR_RDS, required = TRUE)
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, default = list())
MODEL_RUN_SUMMARY <- load_step_rds("MODEL_RUN_SUMMARY", dir_rds = DIR_RDS, default = list())
TABLE_SUMMARY <- load_step_rds("TABLE_SUMMARY", dir_rds = DIR_RDS, default = list())
TABLE_MANIFEST <- load_step_rds("TABLE_MANIFEST", dir_rds = DIR_RDS, default = data.frame())

ensure_dir_local(DIR_FINAL)
single_output_mode <- isTRUE(getOption("easyflow.single_output", TRUE))
if (!single_output_mode) {
  ensure_dir_local(DIR_FINAL_REVIEW)
  ensure_dir_local(DIR_FINAL_SUBMISSION)
  ensure_dir_local(DIR_FINAL_ARCHIVE)
}

asset_manifest <- data.frame(
  section = character(0),
  file_name = character(0),
  file_path = character(0),
  stringsAsFactors = FALSE
)

add_asset <- function(section, path) {
  if (!file.exists(path)) return(invisible(NULL))
  asset_manifest <<- rbind(
    asset_manifest,
    data.frame(
      section = section,
      file_name = basename(path),
      file_path = gsub("\\\\", "/", path),
      stringsAsFactors = FALSE
    )
  )
}

table_files <- list.files(DIR_TABLES, full.names = TRUE)
for (fp in table_files) {
  if (!single_output_mode) {
    file.copy(fp, file.path(DIR_FINAL_REVIEW, basename(fp)), overwrite = TRUE)
    file.copy(fp, file.path(DIR_FINAL_SUBMISSION, basename(fp)), overwrite = TRUE)
  }
  add_asset("table", fp)
}

if (file.exists(PATH_FINAL_EXCEL)) {
  if (!single_output_mode) {
    file.copy(PATH_FINAL_EXCEL, file.path(DIR_FINAL_REVIEW, basename(PATH_FINAL_EXCEL)), overwrite = TRUE)
    file.copy(PATH_FINAL_EXCEL, file.path(DIR_FINAL_SUBMISSION, basename(PATH_FINAL_EXCEL)), overwrite = TRUE)
  }
  add_asset("excel", PATH_FINAL_EXCEL)
}

FINAL_SUMMARY <- list(
  dataset_id = DATASET_ID,
  analysis_id = ANALYSIS_ID,
  source_analysis_id = PROCESS_SETTINGS$source_analysis_id,
  process_model = PROCESS_SETTINGS$process_model,
  predictor_source = PROCESS_SETTINGS$predictor_source,
  n_table_objects = TABLE_SUMMARY$n_tables %||% 0L,
  n_manifest_files = nrow(asset_manifest),
  source_reference_class = PREP_SUMMARY$source_reference_class %||% NA_integer_,
  source_reference_class_label = PREP_SUMMARY$source_reference_class_label %||% NA_character_,
  n_models = MODEL_RUN_SUMMARY$n_models %||% 0L,
  n_rows = MODEL_RUN_SUMMARY$n_rows %||% 0L,
  finalized_at = Sys.time()
)

write_csv_safe(asset_manifest, PATH_FINAL_ASSET_MANIFEST)
write_csv_safe(
  data.frame(
    name = names(FINAL_SUMMARY),
    value = vapply(FINAL_SUMMARY, function(x) paste(x, collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  ),
  PATH_REPRO_INFO
)

writeLines(
  c(
    paste0("Dataset ID      : ", DATASET_ID),
    paste0("Analysis ID     : ", ANALYSIS_ID),
    paste0("Source analysis : ", PROCESS_SETTINGS$source_analysis_id),
    paste0("Process model   : ", PROCESS_SETTINGS$process_model),
    paste0("Predictor       : ", PROCESS_SETTINGS$predictor_source),
    paste0("Model count     : ", MODEL_RUN_SUMMARY$n_models %||% 0L),
    paste0("Table count     : ", TABLE_SUMMARY$n_tables %||% 0L)
  ),
  con = PATH_FINAL_README,
  useBytes = TRUE
)

save_step_rds(FINAL_SUMMARY, "FINAL_SUMMARY", dir_rds = DIR_RDS)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_FINALIZE, units = "secs")), 2)
log_info("09_finalize.R completed.")
log_info("n_table_objects    = ", FINAL_SUMMARY$n_table_objects)
log_info("n_manifest_files   = ", FINAL_SUMMARY$n_manifest_files)
log_info("n_models           = ", FINAL_SUMMARY$n_models)
log_info("elapsed            = ", elapsed_sec, " sec")
log_step_end("finalize", elapsed_sec, ok = TRUE)
