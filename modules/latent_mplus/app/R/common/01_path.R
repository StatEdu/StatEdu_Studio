# ============================================================
# 01_path.R
# Path configuration for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) 프로젝트 공통 경로 설정
# 2) data / config / output / mplus / final 경로 생성
# 3) 이후 step에서 사용하는 PATH_* / DIR_* 객체 제공
#
# 핵심 원칙
# - 프로젝트 메인 구조는 합의한 구조 유지
# - 단, Mplus input/data/out 경로는 short path 사용
#   (Mplus 90-char line limitation 회피 목적)
# ============================================================

# ------------------------------------------------------------
# 0. local helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.norm_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- gsub("/+", "/", x)
  x
}

.ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

.path_join <- function(...) {
  .norm_path(file.path(...))
}

# ------------------------------------------------------------
# 1. required core objects
# ------------------------------------------------------------
if (!exists("PROJECT_ROOT") || is.null(PROJECT_ROOT) || !nzchar(PROJECT_ROOT)) {
  PROJECT_ROOT <- getwd()
}
if (!exists("DATASET_ID") || is.null(DATASET_ID) || !nzchar(DATASET_ID)) {
  DATASET_ID <- "KSWL"
}
if (!exists("ANALYSIS_ID") || is.null(ANALYSIS_ID) || !nzchar(ANALYSIS_ID)) {
  ANALYSIS_ID <- "cross_sectional_mixture"
}

PROJECT_ROOT <- .norm_path(PROJECT_ROOT)
DATASET_ID   <- as.character(DATASET_ID)[1]
ANALYSIS_ID  <- as.character(ANALYSIS_ID)[1]

# ------------------------------------------------------------
# 2. project-level roots
# ------------------------------------------------------------
DIR_PROJECT_DATA   <- .path_join(PROJECT_ROOT, "data")
DIR_PROJECT_OUTPUT <- .path_join(PROJECT_ROOT, "outputs")
DIR_PROJECT_R      <- .path_join(PROJECT_ROOT, "R")
DIR_PROJECT_RUN    <- .path_join(PROJECT_ROOT, "run")

DIR_DATASET_ROOT   <- .path_join(DIR_PROJECT_DATA, DATASET_ID)
DIR_OUTPUT_BASE    <- .path_join(DIR_PROJECT_OUTPUT, DATASET_ID)
DIR_OUTPUT         <- .path_join(DIR_OUTPUT_BASE, ANALYSIS_ID)

# ------------------------------------------------------------
# 3. input data/config structure
# ------------------------------------------------------------
DIR_DATA   <- .path_join(DIR_DATASET_ROOT, "data")
DIR_CONFIG <- .path_join(DIR_DATASET_ROOT, "config")

PATH_CFG     <- .path_join(DIR_CONFIG, "CFG.yml")
PATH_DICT    <- .path_join(DIR_CONFIG, "data_dictionary.csv")
PATH_MISSING <- .path_join(DIR_CONFIG, "missing.yml")
PATH_SUBSETS <- .path_join(DIR_CONFIG, "subsets.yml")

# data file 후보
PATH_DATA_CSV <- .path_join(DIR_DATA, paste0(DATASET_ID, ".csv"))
PATH_DATA_RDS <- .path_join(DIR_DATA, paste0(DATASET_ID, ".rds"))
PATH_DATA_SAV <- .path_join(DIR_DATA, paste0(DATASET_ID, ".sav"))
PATH_DATA_DTA <- .path_join(DIR_DATA, paste0(DATASET_ID, ".dta"))

PATH_DATA_FALLBACK_CSV <- .path_join(DIR_DATA, "data.csv")
PATH_DATA_FALLBACK_RDS <- .path_join(DIR_DATA, "data.rds")
PATH_DATA_FALLBACK_SAV <- .path_join(DIR_DATA, "data.sav")
PATH_DATA_FALLBACK_DTA <- .path_join(DIR_DATA, "data.dta")

if (file.exists(PATH_DATA_CSV)) {
  PATH_DATA <- PATH_DATA_CSV
} else if (file.exists(PATH_DATA_RDS)) {
  PATH_DATA <- PATH_DATA_RDS
} else if (file.exists(PATH_DATA_SAV)) {
  PATH_DATA <- PATH_DATA_SAV
} else if (file.exists(PATH_DATA_DTA)) {
  PATH_DATA <- PATH_DATA_DTA
} else if (file.exists(PATH_DATA_FALLBACK_CSV)) {
  PATH_DATA <- PATH_DATA_FALLBACK_CSV
} else if (file.exists(PATH_DATA_FALLBACK_RDS)) {
  PATH_DATA <- PATH_DATA_FALLBACK_RDS
} else if (file.exists(PATH_DATA_FALLBACK_SAV)) {
  PATH_DATA <- PATH_DATA_FALLBACK_SAV
} else if (file.exists(PATH_DATA_FALLBACK_DTA)) {
  PATH_DATA <- PATH_DATA_FALLBACK_DTA
} else {
  PATH_DATA <- PATH_DATA_CSV
}

# ------------------------------------------------------------
# 4. main output structure
# ------------------------------------------------------------
DIR_FIGURES      <- .path_join(DIR_OUTPUT, "figures")
DIR_FIGURES_PNG  <- .path_join(DIR_FIGURES, "png")
DIR_FIGURES_TIFF <- .path_join(DIR_FIGURES, "tiff")
DIR_FIGURES_PDF  <- .path_join(DIR_FIGURES, "pdf")

DIR_TABLES       <- .path_join(DIR_OUTPUT, "tables")
DIR_DOCX         <- .path_join(DIR_OUTPUT, "docx")
DIR_PAPER_BUNDLE <- .path_join(DIR_OUTPUT, "paper_bundle")

DIR_PAPER_BUNDLE_FIGURES      <- .path_join(DIR_PAPER_BUNDLE, "figures")
DIR_PAPER_BUNDLE_FIGURES_PNG  <- .path_join(DIR_PAPER_BUNDLE_FIGURES, "png")
DIR_PAPER_BUNDLE_FIGURES_TIFF <- .path_join(DIR_PAPER_BUNDLE_FIGURES, "tiff")
DIR_PAPER_BUNDLE_FIGURES_PDF  <- .path_join(DIR_PAPER_BUNDLE_FIGURES, "pdf")
DIR_PAPER_BUNDLE_TABLES       <- .path_join(DIR_PAPER_BUNDLE, "tables")

DIR_FINAL            <- .path_join(DIR_OUTPUT, "final")
DIR_FINAL_SUBMISSION <- .path_join(DIR_FINAL, "submission")
DIR_FINAL_ARCHIVE    <- .path_join(DIR_FINAL, "archive")
DIR_FINAL_REVIEW     <- .path_join(DIR_FINAL, "review")

DIR_LOGS <- .path_join(DIR_OUTPUT, "logs")
DIR_RDS  <- .path_join(DIR_OUTPUT, "rds")

# ------------------------------------------------------------
# 5. short Mplus path (very important)
# ------------------------------------------------------------
# Mplus는 input line 90자 제한이 있으므로
# data/inp/out/savedata 경로는 짧게 유지
DIR_MPLUS_SHORT <- get0("MPLUS_WORK_ROOT", ifnotfound = NULL)
if (is.null(DIR_MPLUS_SHORT) || !nzchar(as.character(DIR_MPLUS_SHORT))) {
  DIR_MPLUS_SHORT <- .path_join(PROJECT_ROOT, "mplus_tmp")
}
DIR_MPLUS_SHORT <- .norm_path(DIR_MPLUS_SHORT)

DIR_MPLUS          <- DIR_MPLUS_SHORT
DIR_MPLUS_DATA     <- .path_join(DIR_MPLUS_SHORT, "data")
DIR_MPLUS_INP      <- .path_join(DIR_MPLUS_SHORT, "inp")
DIR_MPLUS_OUT      <- .path_join(DIR_MPLUS_SHORT, "out")
DIR_MPLUS_SAVEDATA <- .path_join(DIR_MPLUS_SHORT, "savedata")
DIR_MPLUS_BAT      <- .path_join(DIR_MPLUS_SHORT, "bat")

# ------------------------------------------------------------
# 6. frequently used file paths
# ------------------------------------------------------------
PATH_RUN_LOG <- .path_join(DIR_LOGS, "run_log.txt")

PATH_FIGURE_MANIFEST_CSV <- .path_join(DIR_OUTPUT, "figure_manifest.csv")
PATH_FIGURE_MANIFEST_RDS <- .path_join(DIR_OUTPUT, "figure_manifest.rds")

PATH_PAPER_BUNDLE_MANIFEST <- .path_join(DIR_PAPER_BUNDLE, "manifest.csv")
PATH_PAPER_BUNDLE_SUMMARY  <- .path_join(DIR_PAPER_BUNDLE, "bundle_summary.csv")

PATH_FINAL_ASSET_MANIFEST <- .path_join(DIR_FINAL, "final_asset_manifest.csv")
PATH_SESSION_INFO         <- .path_join(DIR_FINAL, "sessionInfo.txt")
PATH_REPRO_INFO           <- .path_join(DIR_FINAL, "repro_info.csv")
PATH_FINAL_README         <- .path_join(DIR_FINAL, "README.txt")

PATH_FINAL_EXCEL <- .path_join(
  DIR_OUTPUT,
  paste0(DATASET_ID, "_", ANALYSIS_ID, "_final_results.xlsx")
)

# ------------------------------------------------------------
# 7. create directories
# ------------------------------------------------------------
dir_vec <- c(
  DIR_PROJECT_DATA,
  DIR_PROJECT_OUTPUT,
  DIR_DATASET_ROOT,
  DIR_OUTPUT_BASE,
  DIR_OUTPUT,
  DIR_DATA,
  DIR_CONFIG,
  DIR_FIGURES,
  DIR_FIGURES_PNG,
  DIR_TABLES,
  DIR_DOCX,
  DIR_FINAL,
  DIR_LOGS,
  DIR_RDS,
  DIR_MPLUS_SHORT,
  DIR_MPLUS_DATA,
  DIR_MPLUS_INP,
  DIR_MPLUS_OUT,
  DIR_MPLUS_SAVEDATA,
  DIR_MPLUS_BAT
)

for (d in unique(dir_vec)) {
  .ensure_dir(d)
}

# ------------------------------------------------------------
# 8. export path summary object
# ------------------------------------------------------------
PATHS <- list(
  PROJECT_ROOT = PROJECT_ROOT,
  DATASET_ID = DATASET_ID,
  ANALYSIS_ID = ANALYSIS_ID,

  DIR_DATASET_ROOT = DIR_DATASET_ROOT,
  DIR_DATA = DIR_DATA,
  DIR_CONFIG = DIR_CONFIG,

  DIR_OUTPUT_BASE = DIR_OUTPUT_BASE,
  DIR_OUTPUT = DIR_OUTPUT,
  DIR_RDS = DIR_RDS,
  DIR_LOGS = DIR_LOGS,

  DIR_TABLES = DIR_TABLES,
  DIR_DOCX = DIR_DOCX,

  DIR_FIGURES = DIR_FIGURES,
  DIR_FIGURES_PNG = DIR_FIGURES_PNG,
  DIR_FIGURES_TIFF = DIR_FIGURES_TIFF,
  DIR_FIGURES_PDF = DIR_FIGURES_PDF,

  DIR_PAPER_BUNDLE = DIR_PAPER_BUNDLE,
  DIR_PAPER_BUNDLE_TABLES = DIR_PAPER_BUNDLE_TABLES,
  DIR_PAPER_BUNDLE_FIGURES = DIR_PAPER_BUNDLE_FIGURES,
  DIR_PAPER_BUNDLE_FIGURES_PNG = DIR_PAPER_BUNDLE_FIGURES_PNG,
  DIR_PAPER_BUNDLE_FIGURES_TIFF = DIR_PAPER_BUNDLE_FIGURES_TIFF,
  DIR_PAPER_BUNDLE_FIGURES_PDF = DIR_PAPER_BUNDLE_FIGURES_PDF,

  DIR_FINAL = DIR_FINAL,
  DIR_FINAL_SUBMISSION = DIR_FINAL_SUBMISSION,
  DIR_FINAL_ARCHIVE = DIR_FINAL_ARCHIVE,
  DIR_FINAL_REVIEW = DIR_FINAL_REVIEW,

  DIR_MPLUS = DIR_MPLUS,
  DIR_MPLUS_SHORT = DIR_MPLUS_SHORT,
  DIR_MPLUS_DATA = DIR_MPLUS_DATA,
  DIR_MPLUS_INP = DIR_MPLUS_INP,
  DIR_MPLUS_OUT = DIR_MPLUS_OUT,
  DIR_MPLUS_SAVEDATA = DIR_MPLUS_SAVEDATA,
  DIR_MPLUS_BAT = DIR_MPLUS_BAT,

  PATH_CFG = PATH_CFG,
  PATH_DICT = PATH_DICT,
  PATH_MISSING = PATH_MISSING,
  PATH_SUBSETS = PATH_SUBSETS,
  PATH_DATA = PATH_DATA,

  PATH_RUN_LOG = PATH_RUN_LOG,
  PATH_FIGURE_MANIFEST_CSV = PATH_FIGURE_MANIFEST_CSV,
  PATH_FIGURE_MANIFEST_RDS = PATH_FIGURE_MANIFEST_RDS,
  PATH_PAPER_BUNDLE_MANIFEST = PATH_PAPER_BUNDLE_MANIFEST,
  PATH_PAPER_BUNDLE_SUMMARY = PATH_PAPER_BUNDLE_SUMMARY,
  PATH_FINAL_ASSET_MANIFEST = PATH_FINAL_ASSET_MANIFEST,
  PATH_SESSION_INFO = PATH_SESSION_INFO,
  PATH_REPRO_INFO = PATH_REPRO_INFO,
  PATH_FINAL_README = PATH_FINAL_README,
  PATH_FINAL_EXCEL = PATH_FINAL_EXCEL
)

# ------------------------------------------------------------
# 9. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("01_path.R loaded\n")
cat("------------------------------------------------------------\n")
cat("PROJECT_ROOT : ", PROJECT_ROOT, "\n", sep = "")
cat("DATASET_ID   : ", DATASET_ID, "\n", sep = "")
cat("ANALYSIS_ID  : ", ANALYSIS_ID, "\n", sep = "")
cat("DIR_DATA     : ", DIR_DATA, "\n", sep = "")
cat("DIR_CONFIG   : ", DIR_CONFIG, "\n", sep = "")
cat("DIR_OUTPUT   : ", DIR_OUTPUT, "\n", sep = "")
cat("DIR_MPLUS    : ", DIR_MPLUS, "\n", sep = "")
cat("PATH_CFG     : ", PATH_CFG, "\n", sep = "")
cat("PATH_DICT    : ", PATH_DICT, "\n", sep = "")
cat("PATH_DATA    : ", PATH_DATA, "\n", sep = "")
cat("============================================================\n\n")
