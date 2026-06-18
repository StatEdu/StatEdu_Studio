# ============================================================
# 08_export_docx.R
# Export publication-ready Word documents for mixture analysis
# ------------------------------------------------------------
# 역할
# 1) table / figure / validation 산출물 로드
# 2) SCI 제출용 docx 패키지 생성
# 3) summary / appendix / figure list 문서 작성
# 4) DOCX_EXPORT_SUMMARY 저장
#
# 생성 산출물
# - docx/<dataset>_<analysis>_summary.docx
# - docx/<dataset>_<analysis>_appendix_tables.docx
# - docx/<dataset>_<analysis>_figure_list.docx
# - DOCX_EXPORT_SUMMARY.rds
# ============================================================

T0_DOCX <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("EXPORT_DOCX", "08_export_docx.R")
log_info("Reloading table / figure outputs ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

extract_model_structure_from_tag <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  hit <- grepl("_model[0-9]+_k[0-9]+_", x, ignore.case = TRUE)
  out[hit] <- sub("^.*_(model[0-9]+)_k[0-9]+_.*$", "\\1", x[hit], ignore.case = TRUE)
  tolower(out)
}

# ------------------------------------------------------------
# 1. helpers
# ------------------------------------------------------------
safe_read_rds <- function(path, default = NULL) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return(default)
  }
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(readRDS(path), error = function(e) default)
}

safe_read_csv <- function(path, default = data.frame()) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return(default)
  }
  if (!file.exists(path)) {
    return(default)
  }
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) default)
}

safe_select_cols <- function(df, cols, warn = TRUE) {
  if (!is.data.frame(df)) return(data.frame())

  cols <- cols[!is.na(cols) & nzchar(cols)]
  cols_exist <- cols[cols %in% names(df)]
  cols_missing <- setdiff(cols, names(df))

  if (warn && length(cols_missing) > 0) {
    message("[safe_select_cols] Missing columns: ", paste(cols_missing, collapse = ", "))
  }

  if (length(cols_exist) == 0) {
    return(df[0, 0, drop = FALSE])
  }

  df[, cols_exist, drop = FALSE]
}

ensure_dir_local <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

safe_write_lines_local <- function(lines, path) {
  ensure_dir_local(dirname(path))
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

# ------------------------------------------------------------
# 2. reload objects
# ------------------------------------------------------------
T1  <- load_step_rds("T1", dir_rds = DIR_RDS, default = data.frame())
T2  <- load_step_rds("T2", dir_rds = DIR_RDS, default = data.frame())
T3  <- load_step_rds("T3", dir_rds = DIR_RDS, default = data.frame())
T4  <- load_step_rds("T4", dir_rds = DIR_RDS, default = data.frame())
T5  <- load_step_rds("T5", dir_rds = DIR_RDS, default = data.frame())
T5b <- load_step_rds("T5b", dir_rds = DIR_RDS, default = data.frame())
T5c <- load_step_rds("T5c", dir_rds = DIR_RDS, default = data.frame())
T5d <- load_step_rds("T5d", dir_rds = DIR_RDS, default = data.frame())
T6  <- load_step_rds("T6", dir_rds = DIR_RDS, default = data.frame())
T7  <- load_step_rds("T7", dir_rds = DIR_RDS, default = data.frame())

A3 <- load_step_rds("A3", dir_rds = DIR_RDS, default = data.frame())
A4 <- load_step_rds("A4", dir_rds = DIR_RDS, default = data.frame())
A5 <- load_step_rds("A5", dir_rds = DIR_RDS, default = data.frame())
A6 <- load_step_rds("A6", dir_rds = DIR_RDS, default = data.frame())

S1 <- load_step_rds("S1", dir_rds = DIR_RDS, default = data.frame())
S2 <- load_step_rds("S2", dir_rds = DIR_RDS, default = data.frame())
S3 <- load_step_rds("S3", dir_rds = DIR_RDS, default = data.frame())
S4 <- load_step_rds("S4", dir_rds = DIR_RDS, default = data.frame())
S5 <- load_step_rds("S5", dir_rds = DIR_RDS, default = data.frame())
S6 <- load_step_rds("S6", dir_rds = DIR_RDS, default = data.frame())

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

TABLE_VALIDATION <- load_step_rds(
  "TABLE_VALIDATION",
  dir_rds = DIR_RDS,
  default = data.frame()
)

FIGURE_VALIDATION <- load_step_rds(
  "FIGURE_VALIDATION",
  dir_rds = DIR_RDS,
  default = data.frame()
)

# ------------------------------------------------------------
# 3. resolve figure manifest
# ------------------------------------------------------------
PATH_FIGURE_MANIFEST_RDS <- PATH_FIGURE_MANIFEST_RDS %||%
  file.path(DIR_RDS, "FIGURE_MANIFEST.rds")

PATH_FIGURE_MANIFEST_CSV <- PATH_FIGURE_MANIFEST_CSV %||%
  file.path(DIR_FIGURES, "FIGURE_MANIFEST.csv")

FIGURE_MANIFEST <- safe_read_rds(
  PATH_FIGURE_MANIFEST_RDS,
  default = data.frame()
)

if (!is.data.frame(FIGURE_MANIFEST) || nrow(FIGURE_MANIFEST) == 0) {
  FIGURE_MANIFEST <- safe_read_csv(
    PATH_FIGURE_MANIFEST_CSV,
    default = data.frame()
  )
}

# ------------------------------------------------------------
# 4. core settings
# ------------------------------------------------------------
mixture_mode <- tolower(
  SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    TABLE_SUMMARY$mixture_mode %||%
    FIGURE_SUMMARY$mixture_mode %||%
    "lpa"
)

best_k <- as.integer(
  BEST_K_SUMMARY$best_k %||%
    BEST_K_SUMMARY$BEST_K %||%
    TABLE_SUMMARY$best_k %||%
    FIGURE_SUMMARY$best_k %||%
    NA_integer_
)

best_tag <- as.character(
  BEST_K_SUMMARY$best_tag %||%
    BEST_K_SUMMARY$BEST_TAG %||%
    TABLE_SUMMARY$best_tag %||%
    FIGURE_SUMMARY$best_tag %||%
    NA_character_
)

model_structure <- tolower(as.character(
  BEST_K_SUMMARY$best_model_structure %||%
    BEST_K_SUMMARY$BEST_MODEL_STRUCTURE %||%
    TABLE_SUMMARY$model_structure %||%
    FIGURE_SUMMARY$model_structure %||%
    if (is.data.frame(BEST_MODEL_ROW) && nrow(BEST_MODEL_ROW) > 0 && "model_structure" %in% names(BEST_MODEL_ROW)) BEST_MODEL_ROW$model_structure[1] else NA_character_
))

if (is.na(model_structure) || !nzchar(model_structure)) {
  model_structure <- extract_model_structure_from_tag(best_tag)
}

dataset_stub <- DATASET_ID %||% SETTINGS_SUMMARY$dataset_id %||% basename(dirname(DIR_OUTPUT))
analysis_stub <- ANALYSIS_ID %||% SETTINGS_SUMMARY$analysis_id %||% basename(DIR_OUTPUT)

file_stub <- paste0(
  tolower(as.character(dataset_stub)),
  "_",
  tolower(as.character(analysis_stub))
)

DIR_DOCX <- DIR_DOCX %||% file.path(DIR_OUTPUT, "docx")
ensure_dir_local(DIR_DOCX)

if (isTRUE(getOption("easyflow.single_output", TRUE))) {
  old_docx <- list.files(
    DIR_DOCX,
    pattern = "_(summary|appendix_tables|figure_list)\\.(docx|txt)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(old_docx) > 0) {
    unlink(old_docx, force = TRUE)
  }
}

summary_docx  <- file.path(DIR_DOCX, paste0(file_stub, "_summary.docx"))
appendix_docx <- file.path(DIR_DOCX, paste0(file_stub, "_appendix_tables.docx"))
figure_docx   <- file.path(DIR_DOCX, paste0(file_stub, "_figure_list.docx"))

# ------------------------------------------------------------
# 5. package support
# ------------------------------------------------------------
docx_supported <- requireNamespace("officer", quietly = TRUE) &&
  requireNamespace("flextable", quietly = TRUE) &&
  exists("body_add_flextable", where = asNamespace("flextable"), mode = "function")

# ------------------------------------------------------------
# 6. display helpers
# ------------------------------------------------------------
make_ft <- function(df) {
  ft <- flextable::flextable(df)
  ft <- flextable::autofit(ft)
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::theme_booktabs(ft)
  ft
}

add_df_section <- function(doc, title, df, note_empty = "No data available.") {
  doc <- officer::body_add_par(doc, title, style = "heading 2")

  if (!is.data.frame(df) || nrow(df) == 0) {
    doc <- officer::body_add_par(doc, note_empty, style = "Normal")
    return(doc)
  }

  ft <- make_ft(df)

  if (requireNamespace("flextable", quietly = TRUE) &&
      exists("body_add_flextable", where = asNamespace("flextable"), mode = "function")) {
    return(flextable::body_add_flextable(doc, value = ft))
  }

  if (requireNamespace("officer", quietly = TRUE) &&
      exists("body_add", where = asNamespace("officer"), mode = "function")) {
    return(officer::body_add(doc, value = ft))
  }

  txt <- utils::capture.output(print(utils::head(df, 40), row.names = FALSE))
  doc <- officer::body_add_par(doc, "Flextable insertion unavailable; text fallback used.", style = "Normal")
  for (ln in txt) {
    doc <- officer::body_add_par(doc, ln, style = "Normal")
  }
  doc
}

df_to_text_lines <- function(title, df, max_rows = 40) {
  out <- c(title, strrep("-", nchar(title)))
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(c(out, "No data available.", ""))
  }

  txt <- utils::capture.output(print(utils::head(df, max_rows), row.names = FALSE))
  c(out, txt, "")
}

write_txt_fallback <- function(lines, path) {
  safe_write_lines_local(lines, path)
}

short_figure_manifest <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  keep <- intersect(
    c(
      "figure_id",
      "file_stub",
      "figure_title",
      "png",
      "tiff",
      "pdf",
      "width",
      "height",
      "dpi",
      "dataset_id",
      "analysis_id",
      "mixture_mode",
      "best_k",
      "best_tag",
      "model_structure"
    ),
    names(df)
  )

  safe_select_cols(df, unique(keep))
}

trim_table_for_docx <- function(df, table_name) {
  df <- safe_df_local(df)
  if (nrow(df) == 0) return(df)

  if (table_name == "T1") return(df)
  if (table_name == "T2") return(df)
  if (table_name == "T3") return(df)
  if (table_name == "T4") return(df)
  if (table_name == "T5") return(df)
  if (table_name == "T5b") return(df)
  if (table_name == "T5c") return(df)
  if (table_name == "T5d") return(df)
  if (table_name == "T6") return(df)
  if (table_name == "T7") return(df)

  df
}

# ------------------------------------------------------------
# 7. prepare display tables
# ------------------------------------------------------------
T1_disp  <- trim_table_for_docx(T1,  "T1")
T2_disp  <- trim_table_for_docx(T2,  "T2")
T3_disp  <- trim_table_for_docx(T3,  "T3")
T4_disp  <- trim_table_for_docx(T4,  "T4")
T5_disp  <- trim_table_for_docx(T5,  "T5")
T5b_disp <- trim_table_for_docx(T5b, "T5b")
T5c_disp <- trim_table_for_docx(T5c, "T5c")
T5d_disp <- trim_table_for_docx(T5d, "T5d")
T6_disp  <- trim_table_for_docx(T6,  "T6")
T7_disp  <- trim_table_for_docx(T7,  "T7")

A3_disp <- trim_table_for_docx(A3, "A3")
A4_disp <- trim_table_for_docx(A4, "A4")
A5_disp <- trim_table_for_docx(A5, "A5")
A6_disp <- trim_table_for_docx(A6, "A6")

FIG_disp <- short_figure_manifest(FIGURE_MANIFEST)

meta_lines <- c(
  paste0("Dataset: ", dataset_stub),
  paste0("Analysis: ", analysis_stub),
  paste0("Mixture mode: ", mixture_mode),
  paste0("Best k: ", ifelse(is.na(best_k), "NA", best_k)),
  paste0("Model structure: ", ifelse(is.na(model_structure) || !nzchar(model_structure), "NA", model_structure)),
  paste0("Best tag: ", ifelse(is.na(best_tag) || !nzchar(best_tag), "NA", best_tag))
)

# ------------------------------------------------------------
# 8. export
# ------------------------------------------------------------
created_files <- character(0)

if (isTRUE(docx_supported)) {

  # ----------------------------------------------------------
  # 8a. summary docx
  # ----------------------------------------------------------
  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc, "Mixture Analysis Summary", style = "heading 1")

  for (ln in meta_lines) {
    doc <- officer::body_add_par(doc, ln, style = "Normal")
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "Core Results", style = "heading 2")
  doc <- officer::body_add_par(
    doc,
    paste0(
      "This document summarizes model fit, final class proportions, the retained solution, ",
      "table outputs, figure inventory, and validation results."
    ),
    style = "Normal"
  )

  doc <- add_df_section(doc, "T7. Analysis summary", T7_disp)
  doc <- add_df_section(doc, "T1. Retained solution summary", T1_disp)
  doc <- add_df_section(doc, "T2. Candidate model fit", T2_disp)
  doc <- add_df_section(doc, "Figure manifest", FIG_disp)
  doc <- add_df_section(doc, "Table validation", TABLE_VALIDATION)
  doc <- add_df_section(doc, "Figure validation", FIGURE_VALIDATION)

  print(doc, target = summary_docx)
  created_files <- c(created_files, summary_docx)

  # ----------------------------------------------------------
  # 8b. appendix tables docx
  # ----------------------------------------------------------
  doc2 <- officer::read_docx()

  doc2 <- officer::body_add_par(doc2, "Appendix Tables", style = "heading 1")
  for (ln in c(meta_lines, paste0("Exported: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))) {
    doc2 <- officer::body_add_par(doc2, ln, style = "Normal")
  }

  doc2 <- add_df_section(doc2, "T3. Profile sizes", T3_disp)
  doc2 <- add_df_section(doc2, "T4. Indicator means by profile", T4_disp)
  doc2 <- add_df_section(doc2, "T5. Covariates predicting profile membership", T5_disp)
  doc2 <- add_df_section(doc2, "T5b. Primary multinomial results", T5b_disp)
  doc2 <- add_df_section(doc2, "T5c. Sensitivity multinomial results", T5c_disp)
  doc2 <- add_df_section(doc2, "T5d. Comparison across approaches", T5d_disp)
  doc2 <- add_df_section(doc2, "T6. Distal outcomes by profile", T6_disp)
  doc2 <- add_df_section(doc2, "A3. Indicator means by profile (raw scale)", A3_disp)
  doc2 <- add_df_section(doc2, "A4. Indicator means by profile (standardized scale)", A4_disp)
  doc2 <- add_df_section(doc2, "A5. Classification summary", A5_disp)
  doc2 <- add_df_section(doc2, "A6. Classification quality by profile", A6_disp)
  doc2 <- add_df_section(doc2, "Table validation", TABLE_VALIDATION)
  doc2 <- add_df_section(doc2, "Figure validation", FIGURE_VALIDATION)

  print(doc2, target = appendix_docx)
  created_files <- c(created_files, appendix_docx)

  # ----------------------------------------------------------
  # 8c. figure list docx
  # ----------------------------------------------------------
  doc3 <- officer::read_docx()

  doc3 <- officer::body_add_par(doc3, "Figure List", style = "heading 1")
  for (ln in c(meta_lines, paste0("Number of figures: ", ifelse(is.data.frame(FIG_disp), nrow(FIG_disp), 0)))) {
    doc3 <- officer::body_add_par(doc3, ln, style = "Normal")
  }

  doc3 <- add_df_section(doc3, "Figures", FIG_disp)
  doc3 <- add_df_section(doc3, "Figure validation", FIGURE_VALIDATION)

  print(doc3, target = figure_docx)
  created_files <- c(created_files, figure_docx)

} else {

  # ----------------------------------------------------------
  # 9. fallback text export
  # ----------------------------------------------------------
  log_warn("officer/flextable not available; creating text fallback files instead of .docx")

  summary_txt  <- file.path(DIR_DOCX, paste0(file_stub, "_summary.txt"))
  appendix_txt <- file.path(DIR_DOCX, paste0(file_stub, "_appendix_tables.txt"))
  figure_txt   <- file.path(DIR_DOCX, paste0(file_stub, "_figure_list.txt"))

  write_txt_fallback(
    c(
      "Mixture Analysis Summary",
      meta_lines,
      "",
      "This document summarizes model fit, retained solution, figures, and validation results.",
      "",
      df_to_text_lines("T7. Analysis summary", T7_disp),
      df_to_text_lines("T1. Retained solution summary", T1_disp),
      df_to_text_lines("T2. Candidate model fit", T2_disp),
      df_to_text_lines("Figure manifest", FIG_disp),
      df_to_text_lines("Table validation", TABLE_VALIDATION),
      df_to_text_lines("Figure validation", FIGURE_VALIDATION)
    ),
    summary_txt
  )

  write_txt_fallback(
    c(
      "Appendix Tables",
      meta_lines,
      paste0("Exported: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      df_to_text_lines("T3. Profile sizes", T3_disp),
      df_to_text_lines("T4. Indicator means by profile", T4_disp),
      df_to_text_lines("T5. Covariates predicting profile membership", T5_disp),
      df_to_text_lines("T5b. Primary multinomial results", T5b_disp),
      df_to_text_lines("T5c. Sensitivity multinomial results", T5c_disp),
      df_to_text_lines("T5d. Comparison across approaches", T5d_disp),
      df_to_text_lines("T6. Distal outcomes by profile", T6_disp),
      df_to_text_lines("A3. Indicator means by profile (raw scale)", A3_disp),
      df_to_text_lines("A4. Indicator means by profile (standardized scale)", A4_disp),
      df_to_text_lines("A5. Classification summary", A5_disp),
      df_to_text_lines("A6. Classification quality by profile", A6_disp),
      df_to_text_lines("Table validation", TABLE_VALIDATION),
      df_to_text_lines("Figure validation", FIGURE_VALIDATION)
    ),
    appendix_txt
  )

  write_txt_fallback(
    c(
      "Figure List",
      meta_lines,
      paste0("Number of figures: ", ifelse(is.data.frame(FIG_disp), nrow(FIG_disp), 0)),
      "",
      df_to_text_lines("Figures", FIG_disp),
      df_to_text_lines("Figure validation", FIGURE_VALIDATION)
    ),
    figure_txt
  )

  created_files <- c(created_files, summary_txt, appendix_txt, figure_txt)
}

# ------------------------------------------------------------
# 10. summary object
# ------------------------------------------------------------
DOCX_EXPORT_SUMMARY <- list(
  dataset_id       = dataset_stub,
  analysis_id      = analysis_stub,
  mixture_mode     = mixture_mode,
  best_k           = best_k,
  best_tag         = best_tag,
  model_structure  = model_structure,
  docx_supported   = docx_supported,
  created_files    = created_files,
  n_created        = length(created_files),
  created_at       = Sys.time()
)

# ------------------------------------------------------------
# 11. save outputs
# ------------------------------------------------------------
save_step_rds(DOCX_EXPORT_SUMMARY, "DOCX_EXPORT_SUMMARY", dir_rds = DIR_RDS)

# ------------------------------------------------------------
# 12. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_DOCX, units = "secs")), 2)

log_info("08_export_docx.R completed.")
log_info("mixture_mode    = ", mixture_mode)
log_info("best_k          = ", best_k)
log_info("best_tag        = ", best_tag)
log_info("model_structure = ", model_structure)
log_info("docx_supported  = ", docx_supported)
log_info("n_created       = ", length(created_files))
log_info("elapsed         = ", elapsed_sec, " sec")

log_step_end("export_docx", elapsed_sec, ok = TRUE)
