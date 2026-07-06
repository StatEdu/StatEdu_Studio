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
T0  <- load_step_rds("T0", dir_rds = DIR_RDS, default = data.frame())
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
A7 <- load_step_rds("A7", dir_rds = DIR_RDS, default = data.frame())
A8 <- load_step_rds("A8", dir_rds = DIR_RDS, default = data.frame())

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

ESTIMATION_TABLES <- list()
if (dir.exists(DIR_TABLES)) {
  estimation_table_paths <- list.files(
    DIR_TABLES,
    pattern = "^estimation.*\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(estimation_table_paths) > 0) {
    for (path in estimation_table_paths) {
      table_id <- tools::file_path_sans_ext(basename(path))
      ESTIMATION_TABLES[[table_id]] <- safe_read_csv(path, default = data.frame())
    }
  }
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
docx_profile_count <- function(df) {
  df <- safe_df_local(df)
  if (nrow(df) == 0 || ncol(df) == 0) return(NA_integer_)
  values <- trimws(as.character(c(names(df), unlist(df, use.names = FALSE))))
  values <- values[nzchar(values)]
  if (length(values) == 0) return(NA_integer_)

  counts <- integer(0)
  profile_hits <- gregexpr("(?:Profile|Class)\\s*([0-9]+)\\b", values, perl = TRUE, ignore.case = TRUE)
  profile_text <- regmatches(values, profile_hits)
  profile_text <- unlist(profile_text, use.names = FALSE)
  if (length(profile_text) > 0) {
    counts <- c(counts, suppressWarnings(as.integer(gsub("[^0-9]+", "", profile_text))))
  }

  p_labels <- values[grepl("^P\\s*[0-9]+$", values, ignore.case = TRUE)]
  if (length(p_labels) > 0) {
    counts <- c(counts, suppressWarnings(as.integer(gsub("[^0-9]+", "", p_labels))))
  }

  counts <- counts[!is.na(counts) & counts > 0]
  if (length(counts) == 0) return(NA_integer_)
  max(counts)
}

docx_table_orientation <- function(table_name, df = NULL) {
  key <- toupper(as.character(table_name %||% ""))
  if (key %in% c("T3", "T4", "T6D", "T6E", "A3", "A4", "A5", "A6", "A7", "A8")) {
    return("portrait")
  }
  if (grepl("^ESTIMATION", key)) {
    return("landscape")
  }
  profile_count <- docx_profile_count(df)
  if (!is.na(profile_count)) {
    return(if (profile_count >= 5L) "landscape" else "portrait")
  }
  "portrait"
}

docx_b5_section_prop <- function(orientation = "portrait", type = "continuous") {
  orientation <- tolower(as.character(orientation %||% "portrait"))
  if (!orientation %in% c("portrait", "landscape")) {
    orientation <- docx_table_orientation(orientation)
  }
  page_size <- if (identical(orientation, "landscape")) {
    officer::page_size(width = 9.843, height = 6.929, orient = "landscape", unit = "in")
  } else {
    officer::page_size(width = 6.929, height = 9.843, orient = "portrait", unit = "in")
  }

  officer::prop_section(
    page_size = page_size,
    page_margins = officer::page_mar(
      top = 0.35,
      bottom = 0.35,
      left = 0.35,
      right = 0.35,
      header = 0.20,
      footer = 0.20
    ),
    type = type
  )
}

docx_b5_section_block <- function(table_name, df = NULL, type = "continuous") {
  officer::block_section(docx_b5_section_prop(docx_table_orientation(table_name, df = df), type = type))
}

make_ft <- function(df, table_name = NULL) {
  df <- safe_df_local(df)
  nm <- names(df)
  if (is.null(nm) || length(nm) != ncol(df)) {
    nm <- paste0("V", seq_len(ncol(df)))
  }
  nm <- trimws(as.character(nm))
  nm[!nzchar(nm) | is.na(nm)] <- paste0("V", which(!nzchar(nm) | is.na(nm)))
  names(df) <- make.unique(nm, sep = "_")

  orientation <- docx_table_orientation(table_name, df = df)
  font_size <- if (identical(orientation, "landscape")) 7 else 8
  page_width <- if (identical(orientation, "landscape")) 9.10 else 6.20

  ft <- flextable::flextable(df)
  ft <- flextable::autofit(ft)
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::theme_booktabs(ft)
  if (exists("fit_to_width", where = asNamespace("flextable"), mode = "function")) {
    ft <- tryCatch(flextable::fit_to_width(ft, max_width = page_width), error = function(e) ft)
  }
  ft
}

add_df_section <- function(doc, title, df, note_empty = "No data available.", table_name = NULL) {
  doc <- officer::body_add_par(doc, title, style = "heading 2")

  if (!is.data.frame(df) || nrow(df) == 0) {
    doc <- officer::body_add_par(doc, note_empty, style = "Normal")
    return(doc)
  }

  ft <- tryCatch(make_ft(df, table_name = table_name), error = function(e) e)
  if (inherits(ft, "error")) {
    doc <- officer::body_add_par(
      doc,
      paste0("Table rendering fallback: ", conditionMessage(ft)),
      style = "Normal"
    )
    txt <- utils::capture.output(print(utils::head(df, 40), row.names = FALSE))
    for (ln in txt) {
      doc <- officer::body_add_par(doc, ln, style = "Normal")
    }
    return(doc)
  }

  if (requireNamespace("flextable", quietly = TRUE) &&
      exists("body_add_flextable", where = asNamespace("flextable"), mode = "function")) {
    added <- tryCatch(flextable::body_add_flextable(doc, value = ft), error = function(e) e)
    if (!inherits(added, "error")) return(added)
  }

  if (requireNamespace("officer", quietly = TRUE) &&
      exists("body_add", where = asNamespace("officer"), mode = "function")) {
    added <- tryCatch(officer::body_add(doc, value = ft), error = function(e) e)
    if (!inherits(added, "error")) return(added)
  }

  txt <- utils::capture.output(print(utils::head(df, 40), row.names = FALSE))
  doc <- officer::body_add_par(doc, "Flextable insertion unavailable; text fallback used.", style = "Normal")
  for (ln in txt) {
    doc <- officer::body_add_par(doc, ln, style = "Normal")
  }
  doc
}

table_spec <- function(id, title, df, keep_empty = TRUE) {
  list(
    id = id,
    title = title,
    df = safe_df_local(df),
    keep_empty = isTRUE(keep_empty)
  )
}

has_table_rows <- function(spec) {
  isTRUE(spec$keep_empty) || (is.data.frame(spec$df) && nrow(spec$df) > 0)
}

add_docx_table_pages <- function(doc, specs) {
  specs <- Filter(has_table_rows, specs)
  if (length(specs) == 0) return(doc)

  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    section_type <- if (i < length(specs)) "nextPage" else "continuous"
    doc <- add_df_section(doc, spec$title, spec$df, table_name = spec$id)
    doc <- officer::body_end_block_section(
      doc,
      value = docx_b5_section_block(spec$id, df = spec$df, type = section_type)
    )
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
T0_disp  <- trim_table_for_docx(T0,  "T0")
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
A7_disp <- trim_table_for_docx(A7, "A7")
A8_disp <- trim_table_for_docx(A8, "A8")

S1_disp <- trim_table_for_docx(S1, "S1")
S5_disp <- trim_table_for_docx(S5, "S5")
S6_disp <- trim_table_for_docx(S6, "S6")

FIG_disp <- short_figure_manifest(FIGURE_MANIFEST)

meta_lines <- c(
  paste0("Dataset: ", dataset_stub),
  paste0("Analysis: ", analysis_stub),
  paste0("Mixture mode: ", mixture_mode),
  paste0("Best k: ", ifelse(is.na(best_k), "NA", best_k)),
  paste0("Model structure: ", ifelse(is.na(model_structure) || !nzchar(model_structure), "NA", model_structure)),
  paste0("Best tag: ", ifelse(is.na(best_tag) || !nzchar(best_tag), "NA", best_tag))
)

summary_table_specs <- list(
  table_spec("T0", "T0. Analysis overview", T0_disp),
  table_spec("T1", "T1. Retained solution summary", T1_disp),
  table_spec("T2", "T2. Candidate model fit", T2_disp),
  table_spec("T7", "T7. Analysis summary", T7_disp)
)

appendix_table_specs <- list(
  table_spec("T0", "T0. Analysis overview", T0_disp),
  table_spec("T1", "T1. Retained solution summary", T1_disp),
  table_spec("T2", "T2. Candidate model fit", T2_disp),
  table_spec("T3", "T3. Profile sizes", T3_disp),
  table_spec("T4", "T4. Indicator means by profile", T4_disp),
  table_spec("T5", "T5. Covariates predicting profile membership", T5_disp),
  table_spec("T5b", "T5b. Primary multinomial results", T5b_disp),
  table_spec("T5c", "T5c. Sensitivity multinomial results", T5c_disp),
  table_spec("T5d", "T5d. Comparison across approaches", T5d_disp),
  table_spec("T6", "T6. Distal outcomes by profile", T6_disp),
  table_spec("T7", "T7. Analysis summary", T7_disp),
  table_spec("A3", "A3. Indicator means by profile (raw scale)", A3_disp),
  table_spec("A4", "A4. Indicator means by profile (standardized scale)", A4_disp),
  table_spec("A5", "A5. Classification summary", A5_disp),
  table_spec("A6", "A6. Classification quality by profile", A6_disp),
  table_spec("A7", "A7. Auxiliary classification table", A7_disp, keep_empty = FALSE),
  table_spec("A8", "A8. Misclassification matrix", A8_disp),
  table_spec("S1", "S1. Overview of auxiliary analyses", S1_disp),
  table_spec("S5", "S5. Primary multinomial results", S5_disp),
  table_spec("S6", "S6. Multinomial model details", S6_disp)
)

if (length(ESTIMATION_TABLES) > 0) {
  for (nm in names(ESTIMATION_TABLES)) {
    appendix_table_specs[[length(appendix_table_specs) + 1L]] <- table_spec(
      nm,
      paste0(nm, ". Estimation table"),
      ESTIMATION_TABLES[[nm]],
      keep_empty = FALSE
    )
  }
}

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

  doc <- officer::body_end_block_section(
    doc,
    value = docx_b5_section_block("T0", type = "nextPage")
  )

  doc <- add_docx_table_pages(doc, summary_table_specs)
  doc <- officer::body_add_break(doc)
  doc <- add_df_section(doc, "Figure manifest", FIG_disp)
  doc <- add_df_section(doc, "Table validation", TABLE_VALIDATION)
  doc <- add_df_section(doc, "Figure validation", FIGURE_VALIDATION)

  print(doc, target = summary_docx)
  created_files <- c(created_files, summary_docx)

  # ----------------------------------------------------------
  # 8b. appendix tables docx
  # ----------------------------------------------------------
  doc2 <- officer::read_docx()
  doc2 <- add_docx_table_pages(doc2, appendix_table_specs)

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
