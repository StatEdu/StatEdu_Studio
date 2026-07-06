T0_TABLES <- Sys.time()

log_step_start("TABLES", "05_tables.R")
log_info("Building latent transition tables ...")

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required.", call. = FALSE)
}

FIT_SUMMARY <- load_step_rds("FIT_SUMMARY", dir_rds = DIR_RDS, default = data.frame())
MEASUREMENT_MANIFEST <- load_step_rds("MEASUREMENT_MANIFEST", dir_rds = DIR_RDS, default = data.frame())
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, default = list())
LTA_SPEC <- load_step_rds("LTA_SPEC", dir_rds = DIR_RDS, required = TRUE)

T1 <- data.frame(
  Metric = c("Measurement mode", "Classes per wave", "Waves", "Indicators", "Rows", "Loglikelihood", "AIC", "BIC", "SABIC", "Entropy"),
  Value = c(
    LTA_SPEC$measurement_mode,
    LTA_SPEC$n_classes,
    length(LTA_SPEC$wave_order),
    length(LTA_SPEC$all_indicator_vars),
    PREP_SUMMARY$n_rows %||% NA_integer_,
    FIT_SUMMARY$ll[1] %||% NA_real_,
    FIT_SUMMARY$aic[1] %||% NA_real_,
    FIT_SUMMARY$bic[1] %||% NA_real_,
    FIT_SUMMARY$sabic[1] %||% NA_real_,
    FIT_SUMMARY$entropy[1] %||% NA_real_
  ),
  stringsAsFactors = FALSE
)

T2 <- MEASUREMENT_MANIFEST

TABLE_REGISTRY <- list(T1 = T1, T2 = T2)
TABLE_INDEX <- data.frame(
  table_name = names(TABLE_REGISTRY),
  nrow = vapply(TABLE_REGISTRY, nrow, integer(1)),
  ncol = vapply(TABLE_REGISTRY, ncol, integer(1)),
  stringsAsFactors = FALSE
)

for (nm in names(TABLE_REGISTRY)) {
  write_csv_safe(TABLE_REGISTRY[[nm]], file.path(DIR_TABLES, paste0(nm, ".csv")))
}
write_csv_safe(TABLE_INDEX, file.path(DIR_TABLES, "TABLE_INDEX.csv"))

wb <- openxlsx::createWorkbook()
for (nm in names(TABLE_REGISTRY)) {
  openxlsx::addWorksheet(wb, nm)
  openxlsx::writeData(wb, nm, TABLE_REGISTRY[[nm]])
  openxlsx::setColWidths(wb, nm, cols = 1:max(1, ncol(TABLE_REGISTRY[[nm]])), widths = "auto")
}
openxlsx::saveWorkbook(wb, PATH_FINAL_EXCEL, overwrite = TRUE)

TABLE_SUMMARY <- list(table_dir = DIR_TABLES, n_tables = length(TABLE_REGISTRY), excel_file = PATH_FINAL_EXCEL, created_at = Sys.time())
TABLE_MANIFEST <- data.frame(
  table_name = c(names(TABLE_REGISTRY), "TABLE_INDEX"),
  file_path = c(file.path(DIR_TABLES, paste0(names(TABLE_REGISTRY), ".csv")), file.path(DIR_TABLES, "TABLE_INDEX.csv")),
  stringsAsFactors = FALSE
)

save_named_rds_list(
  list(
    TABLE_REGISTRY = TABLE_REGISTRY,
    TABLE_INDEX = TABLE_INDEX,
    TABLE_MANIFEST = TABLE_MANIFEST,
    TABLE_SUMMARY = TABLE_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("tables", round(as.numeric(difftime(Sys.time(), T0_TABLES, units = "secs")), 2), ok = TRUE)
