# ============================================================
# 09_finalize.R
# Final packaging / archive / reproducibility summary
# ------------------------------------------------------------
# 역할
# 1) tables / figures / docx 결과물 재수집
# 2) final 폴더 구조 생성
# 3) submission / review / archive 자산 정리
# 4) manifest / README / session info / repro info 저장
#
# 생성 산출물
# - final/final_asset_manifest.csv
# - final/sessionInfo.txt
# - final/repro_info.csv
# - final/README.txt
# - final/submission/*
# - final/review/*
# - final/archive/*
# - FINAL_SUMMARY.rds
#
# 핵심 수정
# - best solution metadata(best_k, best_tag, model_structure) 반영
# - 05/07/08 summary 구조와 호환
# - source of truth = TABLE_REGISTRY / TABLE_MANIFEST / FIGURE_MANIFEST / DOCX_EXPORT_SUMMARY
# - 일부 자산이 비어 있어도 절대 실패하지 않음
# ============================================================

T0_FINALIZE <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

log_step_start("FINALIZE", "09_finalize.R")
log_info("Reloading finalization inputs ...")

if (exists("relocate_project_root_mplus_artifacts")) {
  stray_mplus_cleanup <- tryCatch(
    relocate_project_root_mplus_artifacts(PROJECT_ROOT, dir_mplus = DIR_MPLUS),
    error = function(e) data.frame()
  )
  if (is.data.frame(stray_mplus_cleanup) && nrow(stray_mplus_cleanup) > 0) {
    log_info("Relocated stray root-level Mplus artifacts: n = ", nrow(stray_mplus_cleanup))
  }
}

# ------------------------------------------------------------
# PATH FIX
# ------------------------------------------------------------
PATH_TABLE_MANIFEST_RDS <- get0(
  "PATH_TABLE_MANIFEST_RDS",
  ifnotfound = file.path(DIR_RDS, "TABLE_MANIFEST.rds"),
  inherits = TRUE
)

PATH_TABLE_MANIFEST_CSV <- get0(
  "PATH_TABLE_MANIFEST_CSV",
  ifnotfound = file.path(DIR_TABLES, "TABLE_MANIFEST.csv"),
  inherits = TRUE
)

PATH_FIGURE_MANIFEST_RDS <- get0(
  "PATH_FIGURE_MANIFEST_RDS",
  ifnotfound = file.path(DIR_RDS, "FIGURE_MANIFEST.rds"),
  inherits = TRUE
)

PATH_FIGURE_MANIFEST_CSV <- get0(
  "PATH_FIGURE_MANIFEST_CSV",
  ifnotfound = file.path(DIR_FIGURES, "FIGURE_MANIFEST.csv"),
  inherits = TRUE
)

PATH_INPUT_FINGERPRINT <- get0(
  "PATH_INPUT_FINGERPRINT",
  ifnotfound = file.path(DIR_FINAL, "input_fingerprint_manifest.csv"),
  inherits = TRUE
)

PATH_DICT_VALIDATION_PROBLEMS <- get0(
  "PATH_DICT_VALIDATION_PROBLEMS",
  ifnotfound = file.path(DIR_FINAL, "dict_validation_problems.csv"),
  inherits = TRUE
)

PATH_CFG_VALIDATION_PROBLEMS <- get0(
  "PATH_CFG_VALIDATION_PROBLEMS",
  ifnotfound = file.path(DIR_FINAL, "cfg_validation_problems.csv"),
  inherits = TRUE
)

PATH_SUBSET_VALIDATION <- get0(
  "PATH_SUBSET_VALIDATION",
  ifnotfound = file.path(DIR_FINAL, "subset_validation.csv"),
  inherits = TRUE
)

PATH_PREP_VALIDATION <- get0(
  "PATH_PREP_VALIDATION",
  ifnotfound = file.path(DIR_FINAL, "prep_validation.csv"),
  inherits = TRUE
)

# ------------------------------------------------------------
# 1. local helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

safe_read_rds_local <- function(path, default = NULL) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return(default)
  }
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(readRDS(path), error = function(e) default)
}

safe_read_csv_local <- function(path, default = data.frame()) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return(default)
  }
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) default)
}

file_exists2 <- function(path) {
  path <- as.character(path)
  ok <- !is.na(path) & nzchar(trimws(path))
  out <- rep(FALSE, length(path))
  out[ok] <- file.exists(path[ok])
  out
}

dir_exists2 <- function(path) {
  path <- as.character(path)
  ok <- !is.na(path) & nzchar(trimws(path))
  out <- rep(FALSE, length(path))
  out[ok] <- dir.exists(path[ok])
  out
}

first_existing <- function(paths) {
  paths <- as.character(paths)
  ok <- file_exists2(paths)
  hit <- paths[ok]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

collect_files_recursive <- function(path) {
  if (!dir_exists2(path)) return(character(0))
  list.files(path, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
}

copy_if_exists_local <- function(from, to, overwrite = TRUE) {
  from <- first_existing(from)
  if (is.na(from)) return(FALSE)

  ensure_dir2(dirname(to))
  ok <- tryCatch(file.copy(from, to, overwrite = overwrite), error = function(e) FALSE)
  isTRUE(ok)
}

add_manifest_rows <- function(manifest, files, section, role = NA_character_) {
  files <- unique(as.character(files))
  files <- files[file_exists2(files)]

  if (length(files) == 0) return(manifest)

  finfo <- file.info(files)

  add <- data.frame(
    section = section,
    role = role,
    file_name = basename(files),
    file_path = .norm_path2(files),
    ext = tools::file_ext(files),
    size_bytes = finfo$size,
    modified_time = as.character(finfo$mtime),
    stringsAsFactors = FALSE
  )

  if (is.null(manifest) || !is.data.frame(manifest) || nrow(manifest) == 0) {
    return(add)
  }

  out <- rbind(manifest, add)
  rownames(out) <- NULL
  out
}

safe_write_lines_local <- function(lines, path) {
  ensure_dir2(dirname(path))
  writeLines(enc2utf8(as.character(lines)), con = path, useBytes = TRUE)
  invisible(path)
}

safe_df_local <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (is.null(out)) out <- data.frame()
  out
}

get_table_local <- function(registry, nm) {
  if (!is.list(registry)) return(data.frame())
  obj <- registry[[nm]]
  if (is.null(obj) || !is.data.frame(obj)) return(data.frame())
  obj
}

extract_model_structure_from_tag <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  hit <- grepl("_model[0-9]+_k[0-9]+_", x, ignore.case = TRUE)
  out[hit] <- sub("^.*_(model[0-9]+)_k[0-9]+_.*$", "\\1", x[hit], ignore.case = TRUE)
  tolower(out)
}

# ------------------------------------------------------------
# 2. resolve directories
# ------------------------------------------------------------
DIR_FINAL <- DIR_FINAL %||% file.path(DIR_OUTPUT, "final")
DIR_FINAL_SUBMISSION <- DIR_FINAL_SUBMISSION %||% file.path(DIR_FINAL, "submission")
DIR_FINAL_REVIEW <- DIR_FINAL_REVIEW %||% file.path(DIR_FINAL, "review")
DIR_FINAL_ARCHIVE <- DIR_FINAL_ARCHIVE %||% file.path(DIR_FINAL, "archive")

DIR_TABLES <- DIR_TABLES %||% DIR_TABLE %||% file.path(DIR_OUTPUT, "tables")
DIR_FIGURES <- DIR_FIGURES %||% file.path(DIR_OUTPUT, "figures")
DIR_FIGURES_PNG <- DIR_FIGURES_PNG %||% file.path(DIR_FIGURES, "png")
DIR_FIGURES_TIFF <- DIR_FIGURES_TIFF %||% file.path(DIR_FIGURES, "tiff")
DIR_FIGURES_PDF <- DIR_FIGURES_PDF %||% file.path(DIR_FIGURES, "pdf")
DIR_DOCX <- DIR_DOCX %||% file.path(DIR_OUTPUT, "docx")

ensure_dir2(DIR_FINAL)
single_output_mode <- isTRUE(getOption("easyflow.single_output", TRUE))
if (!single_output_mode) {
  ensure_dir2(DIR_FINAL_SUBMISSION)
  ensure_dir2(DIR_FINAL_REVIEW)
  ensure_dir2(DIR_FINAL_ARCHIVE)
}

PATH_FINAL_ASSET_MANIFEST <- PATH_FINAL_ASSET_MANIFEST %||%
  file.path(DIR_FINAL, "final_asset_manifest.csv")

PATH_SESSION_INFO <- PATH_SESSION_INFO %||%
  file.path(DIR_FINAL, "sessionInfo.txt")

PATH_REPRO_INFO <- PATH_REPRO_INFO %||%
  file.path(DIR_FINAL, "repro_info.csv")

PATH_FINAL_README <- PATH_FINAL_README %||%
  file.path(DIR_FINAL, "README.txt")

PATH_FIGURE_MANIFEST_RDS <- PATH_FIGURE_MANIFEST_RDS %||%
  file.path(DIR_RDS, "FIGURE_MANIFEST.rds")

PATH_FIGURE_MANIFEST_CSV <- PATH_FIGURE_MANIFEST_CSV %||%
  file.path(DIR_FIGURES, "FIGURE_MANIFEST.csv")

# ------------------------------------------------------------
# 0-1. default paths (🔥 FIX)
# ------------------------------------------------------------
if (!exists("PATH_TABLE_MANIFEST_RDS")) {
  PATH_TABLE_MANIFEST_RDS <- file.path(DIR_RDS, "TABLE_MANIFEST.rds")
}

if (!exists("PATH_TABLE_MANIFEST_CSV")) {
  PATH_TABLE_MANIFEST_CSV <- file.path(DIR_OUTPUT, "TABLE_MANIFEST.csv")
}

PATH_FINAL_EXCEL <- PATH_FINAL_EXCEL %||%
  file.path(DIR_OUTPUT, paste0(DATASET_ID, "_", ANALYSIS_ID, "_final_results.xlsx"))

# ------------------------------------------------------------
# 3. reload main objects
# ------------------------------------------------------------
SETTINGS_SUMMARY <- load_step_rds(
  "SETTINGS_SUMMARY",
  dir_rds = DIR_RDS,
  required = TRUE
)

BEST_K_SUMMARY <- load_step_rds(
  "BEST_K_SUMMARY",
  dir_rds = DIR_RDS,
  default = list()
)

BEST_MODEL_ROW <- load_step_rds(
  "BEST_MODEL_ROW",
  dir_rds = DIR_RDS,
  default = data.frame()
)

TABLE_SUMMARY <- load_step_rds(
  "TABLE_SUMMARY",
  dir_rds = DIR_RDS,
  default = list()
)

FIGURE_SUMMARY <- load_step_rds(
  "FIGURE_SUMMARY",
  dir_rds = DIR_RDS,
  default = list()
)

DOCX_EXPORT_SUMMARY <- load_step_rds(
  "DOCX_EXPORT_SUMMARY",
  dir_rds = DIR_RDS,
  default = list()
)

DICT_VALIDATION <- load_step_rds(
  "DICT_VALIDATION",
  dir_rds = DIR_RDS,
  default = list()
)

CFG_VALIDATION <- load_step_rds(
  "CFG_VALIDATION",
  dir_rds = DIR_RDS,
  default = list()
)

SUBSET_VALIDATION <- load_step_rds(
  "SUBSET_VALIDATION",
  dir_rds = DIR_RDS,
  default = data.frame()
)

INPUT_FINGERPRINT <- load_step_rds(
  "INPUT_FINGERPRINT",
  dir_rds = DIR_RDS,
  default = data.frame()
)

PREP_VALIDATION <- load_step_rds(
  "PREP_VALIDATION",
  dir_rds = DIR_RDS,
  default = data.frame()
)

TABLE_REGISTRY <- load_step_rds(
  "TABLE_REGISTRY",
  dir_rds = DIR_RDS,
  default = list()
)

TABLE_MANIFEST <- safe_read_rds_local(PATH_TABLE_MANIFEST_RDS, default = data.frame())
if (!is.data.frame(TABLE_MANIFEST) || nrow(TABLE_MANIFEST) == 0) {
  TABLE_MANIFEST <- safe_read_csv_local(PATH_TABLE_MANIFEST_CSV, default = data.frame())
}

FIGURE_MANIFEST <- safe_read_rds_local(PATH_FIGURE_MANIFEST_RDS, default = data.frame())
if (!is.data.frame(FIGURE_MANIFEST) || nrow(FIGURE_MANIFEST) == 0) {
  FIGURE_MANIFEST <- safe_read_csv_local(PATH_FIGURE_MANIFEST_CSV, default = data.frame())
}

if (!is.list(TABLE_REGISTRY)) TABLE_REGISTRY <- list()

T1 <- get_table_local(TABLE_REGISTRY, "T1")
T2 <- get_table_local(TABLE_REGISTRY, "T2")
T3 <- get_table_local(TABLE_REGISTRY, "T3")
T4 <- get_table_local(TABLE_REGISTRY, "T4")
T5 <- get_table_local(TABLE_REGISTRY, "T5")
T6 <- get_table_local(TABLE_REGISTRY, "T6")
T7 <- get_table_local(TABLE_REGISTRY, "T7")
A3 <- get_table_local(TABLE_REGISTRY, "A3")
A4 <- get_table_local(TABLE_REGISTRY, "A4")
A5 <- get_table_local(TABLE_REGISTRY, "A5")
A6 <- get_table_local(TABLE_REGISTRY, "A6")
S6 <- get_table_local(TABLE_REGISTRY, "S6")

# ------------------------------------------------------------
# 4. resolve metadata
# ------------------------------------------------------------
dataset_id <- DATASET_ID %||% SETTINGS_SUMMARY$dataset_id %||% basename(dirname(DIR_OUTPUT))
analysis_id <- ANALYSIS_ID %||% SETTINGS_SUMMARY$analysis_id %||% basename(DIR_OUTPUT)

mixture_mode <- tolower(
  SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    TABLE_SUMMARY$mixture_mode %||%
    FIGURE_SUMMARY$mixture_mode %||%
    DOCX_EXPORT_SUMMARY$mixture_mode %||%
    "lpa"
)

best_k <- as.integer(
  BEST_K_SUMMARY$best_k %||%
    BEST_K_SUMMARY$BEST_K %||%
    TABLE_SUMMARY$best_k %||%
    FIGURE_SUMMARY$best_k %||%
    DOCX_EXPORT_SUMMARY$best_k %||%
    NA_integer_
)

best_tag <- as.character(
  BEST_K_SUMMARY$best_tag %||%
    BEST_K_SUMMARY$BEST_TAG %||%
    TABLE_SUMMARY$best_tag %||%
    FIGURE_SUMMARY$best_tag %||%
    DOCX_EXPORT_SUMMARY$best_tag %||%
    NA_character_
)

model_structure <- tolower(as.character(
  BEST_K_SUMMARY$best_model_structure %||%
    BEST_K_SUMMARY$BEST_MODEL_STRUCTURE %||%
    TABLE_SUMMARY$model_structure %||%
    FIGURE_SUMMARY$model_structure %||%
    DOCX_EXPORT_SUMMARY$model_structure %||%
    if (is.data.frame(BEST_MODEL_ROW) && nrow(BEST_MODEL_ROW) > 0 && "model_structure" %in% names(BEST_MODEL_ROW)) BEST_MODEL_ROW$model_structure[1] else NA_character_
))

if (is.na(model_structure) || !nzchar(model_structure)) {
  model_structure <- extract_model_structure_from_tag(best_tag)
}

best_rule <- as.character(
  BEST_K_SUMMARY$best_rule %||%
    BEST_K_SUMMARY$BEST_K_RULE %||%
    NA_character_
)

best_reason <- as.character(
  BEST_K_SUMMARY$best_reason_detail %||%
    BEST_K_SUMMARY$best_reason %||%
    NA_character_
)

# ------------------------------------------------------------
# 5. build final model info / class summary
# ------------------------------------------------------------
log_info("Building FINAL_MODEL_INFO ...")

FINAL_MODEL_INFO <- data.frame(
  dataset_id = dataset_id,
  analysis_id = analysis_id,
  mixture_mode = mixture_mode,
  best_k = best_k,
  best_tag = best_tag,
  model_structure = model_structure,
  best_rule = best_rule,
  best_reason = best_reason,
  n_T1 = nrow(T1),
  n_T2 = nrow(T2),
  n_T3 = nrow(T3),
  n_T4 = nrow(T4),
  n_T5 = nrow(T5),
  n_T6 = nrow(T6),
  n_T7 = nrow(T7),
  n_A3 = nrow(A3),
  n_A4 = nrow(A4),
  n_A5 = nrow(A5),
  n_A6 = nrow(A6),
  n_S6 = nrow(S6),
  n_table_objects = length(TABLE_REGISTRY),
  n_figures = if (is.data.frame(FIGURE_MANIFEST)) nrow(FIGURE_MANIFEST) else 0L,
  n_docx_files = length(DOCX_EXPORT_SUMMARY$created_files %||% character(0)),
  created_at = as.character(Sys.time()),
  stringsAsFactors = FALSE
)

FINAL_CLASS_SUMMARY <- T2
if (!is.data.frame(FINAL_CLASS_SUMMARY)) FINAL_CLASS_SUMMARY <- data.frame()

# ------------------------------------------------------------
# 6. collect source assets
# ------------------------------------------------------------
log_info("Collecting source assets ...")

docx_files <- collect_files_recursive(DIR_DOCX)

table_files <- collect_files_recursive(DIR_TABLES)
table_files <- table_files[file_exists2(table_files)]

figure_png <- collect_files_recursive(DIR_FIGURES_PNG)
figure_tiff <- collect_files_recursive(DIR_FIGURES_TIFF)
figure_pdf <- collect_files_recursive(DIR_FIGURES_PDF)
figure_files <- unique(c(figure_png, figure_tiff, figure_pdf))

excel_files <- collect_files_recursive(DIR_OUTPUT)
excel_files <- excel_files[grepl("\\.xlsx$", excel_files, ignore.case = TRUE)]

rds_files <- collect_files_recursive(DIR_RDS)

manifest_sources <- c(
  PATH_TABLE_MANIFEST_CSV,
  PATH_TABLE_MANIFEST_RDS,
  PATH_FIGURE_MANIFEST_CSV,
  PATH_FIGURE_MANIFEST_RDS
)
manifest_sources <- manifest_sources[file_exists2(manifest_sources)]

validation_sources <- c(
  PATH_INPUT_FINGERPRINT,
  PATH_DICT_VALIDATION_PROBLEMS,
  PATH_CFG_VALIDATION_PROBLEMS,
  PATH_SUBSET_VALIDATION,
  PATH_PREP_VALIDATION
)
validation_sources <- validation_sources[file_exists2(validation_sources)]

# ------------------------------------------------------------
# 7. register representative assets
# ------------------------------------------------------------
log_info("Registering final assets ...")

submission_targets <- character(0)
review_targets <- character(0)
archive_targets <- character(0)

if (!single_output_mode) {

# 7a. submission: 핵심 deliverables
main_excel <- first_existing(c(
  PATH_FINAL_EXCEL,
  excel_files
))

if (length(main_excel) == 1 && !is.na(main_excel)) {
  to_sub <- file.path(DIR_FINAL_SUBMISSION, basename(main_excel))
  if (copy_if_exists_local(main_excel, to_sub)) submission_targets <- c(submission_targets, to_sub)
}

if (length(docx_files) > 0) {
  for (f in docx_files) {
    to_sub <- file.path(DIR_FINAL_SUBMISSION, basename(f))
    if (copy_if_exists_local(f, to_sub)) submission_targets <- c(submission_targets, to_sub)
  }
}

# 7b. review: deliverables + figures + manifests + csv tables
for (f in unique(c(docx_files, figure_files, excel_files, manifest_sources, table_files, validation_sources))) {
  to_rev <- file.path(DIR_FINAL_REVIEW, basename(f))
  if (copy_if_exists_local(f, to_rev)) review_targets <- c(review_targets, to_rev)
}

# 7c. archive: 넓게 보관
archive_sources <- unique(c(
  docx_files,
  figure_files,
  excel_files,
  manifest_sources,
  table_files,
  rds_files,
  validation_sources
))

for (f in archive_sources) {
  to_arc <- file.path(DIR_FINAL_ARCHIVE, basename(f))
  if (copy_if_exists_local(f, to_arc)) archive_targets <- c(archive_targets, to_arc)
}

}

submission_targets <- unique(submission_targets)
review_targets <- unique(review_targets)
archive_targets <- unique(archive_targets)

# ------------------------------------------------------------
# 8. build final asset manifest
# ------------------------------------------------------------
log_info("Building FINAL_ASSET_MANIFEST ...")

FINAL_ASSET_MANIFEST <- data.frame()

if (!single_output_mode) {
  FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, submission_targets, "submission", "deliverable")
  FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, review_targets, "review", "review_material")
  FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, archive_targets, "archive", "archive_copy")
}

FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, excel_files, "source_output", "excel")
FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, docx_files, "source_output", "docx")
FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, figure_files, "source_output", "figure")
FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, table_files, "source_output", "table")
FINAL_ASSET_MANIFEST <- add_manifest_rows(FINAL_ASSET_MANIFEST, rds_files, "source_output", "rds")

if (is.data.frame(FINAL_ASSET_MANIFEST) && nrow(FINAL_ASSET_MANIFEST) > 0) {
  FINAL_ASSET_MANIFEST$dataset_id <- dataset_id
  FINAL_ASSET_MANIFEST$analysis_id <- analysis_id
  FINAL_ASSET_MANIFEST$mixture_mode <- mixture_mode
  FINAL_ASSET_MANIFEST$best_k <- best_k
  FINAL_ASSET_MANIFEST$best_tag <- best_tag
  FINAL_ASSET_MANIFEST$model_structure <- model_structure

  FINAL_ASSET_MANIFEST <- FINAL_ASSET_MANIFEST[
    !duplicated(paste(FINAL_ASSET_MANIFEST$section, FINAL_ASSET_MANIFEST$file_path, sep = "||")),
    ,
    drop = FALSE
  ]
  rownames(FINAL_ASSET_MANIFEST) <- NULL
}


# ------------------------------------------------------------
# 8-1. validation / fingerprint exports
# ------------------------------------------------------------
if (is.data.frame(INPUT_FINGERPRINT)) {
  write_csv_safe(INPUT_FINGERPRINT, PATH_INPUT_FINGERPRINT)
}

DICT_VALIDATION_PROBLEMS <- DICT_VALIDATION$problems %||% data.frame()
CFG_VALIDATION_PROBLEMS  <- CFG_VALIDATION$problems %||% data.frame()

if (is.data.frame(DICT_VALIDATION_PROBLEMS)) {
  write_csv_safe(DICT_VALIDATION_PROBLEMS, PATH_DICT_VALIDATION_PROBLEMS)
}

if (is.data.frame(CFG_VALIDATION_PROBLEMS)) {
  write_csv_safe(CFG_VALIDATION_PROBLEMS, PATH_CFG_VALIDATION_PROBLEMS)
}

if (is.data.frame(SUBSET_VALIDATION)) {
  write_csv_safe(SUBSET_VALIDATION, PATH_SUBSET_VALIDATION)
}

if (is.data.frame(PREP_VALIDATION)) {
  write_csv_safe(PREP_VALIDATION, PATH_PREP_VALIDATION)
}

# ------------------------------------------------------------
# 9. reproducibility files
# ------------------------------------------------------------
log_info("Writing reproducibility files ...")

session_lines <- utils::capture.output(sessionInfo())
safe_write_lines_local(session_lines, PATH_SESSION_INFO)

REPRO_INFO <- data.frame(
  key = c(
    "project_root",
    "dir_output",
    "dataset_id",
    "analysis_id",
    "mixture_mode",
    "best_k",
    "best_tag",
    "model_structure",
    "best_rule",
    "best_reason",
    "n_table_objects",
    "n_figures",
    "n_docx_files",
    "timestamp",
    "r_version",
    "platform"
  ),
  value = c(
    PROJECT_ROOT %||% NA_character_,
    DIR_OUTPUT,
    dataset_id,
    analysis_id,
    mixture_mode,
    ifelse(is.na(best_k), NA_character_, as.character(best_k)),
    best_tag,
    model_structure,
    best_rule,
    best_reason,
    as.character(length(TABLE_REGISTRY)),
    as.character(if (is.data.frame(FIGURE_MANIFEST)) nrow(FIGURE_MANIFEST) else 0L),
    as.character(length(docx_files)),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    paste(R.version$major, R.version$minor, sep = "."),
    R.version$platform
  ),
  stringsAsFactors = FALSE
)

write_csv_safe(REPRO_INFO, PATH_REPRO_INFO)

readme_lines <- c(
  "Mixture analysis final package",
  "",
  paste0("Dataset ID      : ", dataset_id),
  paste0("Analysis ID     : ", analysis_id),
  paste0("Mixture mode    : ", mixture_mode),
  paste0("Best k          : ", ifelse(is.na(best_k), "NA", best_k)),
  paste0("Best tag        : ", ifelse(is.na(best_tag) || !nzchar(best_tag), "NA", best_tag)),
  paste0("Model structure : ", ifelse(is.na(model_structure) || !nzchar(model_structure), "NA", model_structure)),
  paste0("Best rule       : ", ifelse(is.na(best_rule) || !nzchar(best_rule), "NA", best_rule)),
  paste0("Best reason     : ", ifelse(is.na(best_reason) || !nzchar(best_reason), "NA", best_reason)),
  paste0("Table count     : ", length(TABLE_REGISTRY)),
  paste0("Figure count    : ", ifelse(is.data.frame(FIGURE_MANIFEST), nrow(FIGURE_MANIFEST), 0)),
  paste0("Docx count      : ", length(docx_files)),
  "",
  "Folder guide",
  "-----------",
  "submission/ : 핵심 제출용 산출물(docx, xlsx 등)",
  "review/     : 검토용 산출물(docx, figures, tables, manifest 등)",
  "archive/    : 보관용 복사본 및 intermediate artifacts",
  "",
  "Main metadata files",
  "-------------------",
  "final_asset_manifest.csv : 최종 자산 목록",
  "sessionInfo.txt          : R 세션 정보",
  "repro_info.csv           : 재현성 관련 핵심 정보",
  "README.txt               : 이 안내 파일",
  "",
  "Input validation",
  "----------------",
  "dictionary/data consistency was checked at settings stage.",
  "cfg/dictionary/data/subsets consistency summaries were exported.",
  "input file fingerprint manifest was recorded for reproducibility.",
  "",
  "Analysis principle",
  "------------------",
  "This project prioritizes Mplus-native estimation for mixture modeling,",
  "R3STEP, and BCH, consistent with SCI-targeted analysis workflow.",
  "",
  "Selected final solution",
  "-----------------------",
  paste0("The retained solution is based on model_structure = ", ifelse(is.na(model_structure) || !nzchar(model_structure), "NA", model_structure),
         ", k = ", ifelse(is.na(best_k), "NA", best_k),
         ", tag = ", ifelse(is.na(best_tag) || !nzchar(best_tag), "NA", best_tag), "."),
  "",
  "Generated on:",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

safe_write_lines_local(readme_lines, PATH_FINAL_README)
write_csv_safe(FINAL_ASSET_MANIFEST, PATH_FINAL_ASSET_MANIFEST)

# ------------------------------------------------------------
# 10. final summary object
# ------------------------------------------------------------
log_info("Building FINAL_SUMMARY ...")

table_nrow_vec <- if (length(TABLE_REGISTRY) > 0) {
  vapply(TABLE_REGISTRY, function(x) if (is.data.frame(x)) nrow(x) else 0L, integer(1))
} else {
  integer(0)
}

FINAL_SUMMARY <- list(
  dataset_id          = dataset_id,
  analysis_id         = analysis_id,
  mixture_mode        = mixture_mode,
  best_k              = best_k,
  best_tag            = best_tag,
  model_structure     = model_structure,
  best_rule           = best_rule,
  best_reason         = best_reason,
  final_model_info    = FINAL_MODEL_INFO,
  final_class_summary = FINAL_CLASS_SUMMARY,
  table_summary       = TABLE_SUMMARY,
  table_manifest      = TABLE_MANIFEST,
  figure_summary      = FIGURE_SUMMARY,
  docx_export_summary = DOCX_EXPORT_SUMMARY,
  input_fingerprint = INPUT_FINGERPRINT,
  dict_validation = DICT_VALIDATION,
  cfg_validation = CFG_VALIDATION,
  subset_validation = SUBSET_VALIDATION,
  prep_validation = PREP_VALIDATION,
  n_table_objects     = length(TABLE_REGISTRY),
  n_table_rows_total  = sum(table_nrow_vec, na.rm = TRUE),
  n_figures           = if (is.data.frame(FIGURE_MANIFEST)) nrow(FIGURE_MANIFEST) else 0L,
  n_docx_files        = length(docx_files),
  n_manifest_files    = if (is.data.frame(FINAL_ASSET_MANIFEST)) nrow(FINAL_ASSET_MANIFEST) else 0L,
  submission_files    = submission_targets,
  review_files        = review_targets,
  archive_files       = archive_targets,
  session_info_file   = PATH_SESSION_INFO,
  repro_info_file     = PATH_REPRO_INFO,
  readme_file         = PATH_FINAL_README,
  final_manifest_file = PATH_FINAL_ASSET_MANIFEST,
  created_at          = Sys.time()
)

# ------------------------------------------------------------
# 11. save outputs
# ------------------------------------------------------------
log_info("Saving final outputs ...")

save_named_rds_list(
  list(
    FINAL_MODEL_INFO = FINAL_MODEL_INFO,
    FINAL_CLASS_SUMMARY = FINAL_CLASS_SUMMARY,
    FINAL_ASSET_MANIFEST = FINAL_ASSET_MANIFEST,
    FINAL_SUMMARY = FINAL_SUMMARY
  ),
  dir_rds = DIR_RDS
)

# ------------------------------------------------------------
# 12. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_FINALIZE, units = "secs")), 2)

log_info("09_finalize.R completed.")
log_info("best_k             = ", best_k)
log_info("best_tag           = ", best_tag)
log_info("model_structure    = ", model_structure)
log_info("n_table_objects    = ", FINAL_SUMMARY$n_table_objects)
log_info("n_table_rows_total = ", FINAL_SUMMARY$n_table_rows_total)
log_info("n_figures          = ", FINAL_SUMMARY$n_figures)
log_info("n_docx_files       = ", FINAL_SUMMARY$n_docx_files)
log_info("n_manifest_files   = ", FINAL_SUMMARY$n_manifest_files)
log_info("elapsed            = ", elapsed_sec, " sec")

log_step_end("finalize", elapsed_sec, ok = TRUE)
