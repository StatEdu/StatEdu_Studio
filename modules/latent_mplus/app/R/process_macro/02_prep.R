T0_PREP <- Sys.time()

log_step_start("PREP", "02_prep.R")
log_info("Reloading settings outputs ...")

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, required = TRUE)
RAW_DATA <- load_step_rds("RAW_DATA", dir_rds = DIR_RDS, required = TRUE)
PROCESS_SETTINGS <- load_step_rds("PROCESS_SETTINGS", dir_rds = DIR_RDS, required = TRUE)
SURVEY_BUNDLE <- load_step_rds("SURVEY_BUNDLE", dir_rds = DIR_RDS, default = list())

source_rds_dir <- PROCESS_SETTINGS$source_rds_dir
observed_source <- isTRUE(PROCESS_SETTINGS$observed_source)
if (!observed_source && !dir.exists(source_rds_dir)) {
  stop("Source analysis RDS directory not found: ", source_rds_dir, call. = FALSE)
}

source_load <- function(name, default = NULL, required = FALSE) {
  load_step_rds(name, dir_rds = source_rds_dir, default = default, required = required)
}

SOURCE_CLASSIFIED_ANALYSIS <- if (observed_source) data.frame() else source_load("CLASSIFIED_ANALYSIS", required = TRUE)
SOURCE_REFERENCE_CLASS <- if (observed_source) NA_integer_ else source_load("REFERENCE_CLASS", default = NA_integer_)
SOURCE_REFERENCE_CLASS_LABEL <- if (observed_source) NA_character_ else source_load("REFERENCE_CLASS_LABEL", default = NA_character_)
SOURCE_CLASS_SUMMARY <- if (observed_source) data.frame() else source_load("CLASS_SUMMARY_FINAL", default = data.frame())
SOURCE_BEST_K_SUMMARY <- if (observed_source) list() else source_load("BEST_K_SUMMARY", default = list())

id_var <- PROCESS_SETTINGS$id_var %||% "id"
survey_vars <- unique(c(
  SURVEY_BUNDLE$weight_var,
  SURVEY_BUNDLE$strata_var,
  SURVEY_BUNDLE$cluster_var,
  SURVEY_BUNDLE$id_var,
  SURVEY_BUNDLE$rep_weight_vars
))
required_vars <- unique(c(PROCESS_SETTINGS$x_vars, PROCESS_SETTINGS$y_vars, PROCESS_SETTINGS$m_vars, PROCESS_SETTINGS$covariates, survey_vars))
required_vars <- required_vars[!is.na(required_vars) & nzchar(required_vars)]

if (observed_source) {
  prep_df <- RAW_DATA
  if (!is.data.frame(prep_df) || nrow(prep_df) == 0) {
    stop("RAW_DATA is empty for observed moderation source.", call. = FALSE)
  }
  required_vars <- unique(c(required_vars, PROCESS_SETTINGS$w_var, SURVEY_BUNDLE$subset_var))
  required_vars <- required_vars[!is.na(required_vars) & nzchar(required_vars)]
} else {
  prep_df <- SOURCE_CLASSIFIED_ANALYSIS
  if (!is.data.frame(prep_df) || nrow(prep_df) == 0) {
    stop("CLASSIFIED_ANALYSIS from source analysis is empty.", call. = FALSE)
  }

  if (!"class_num" %in% names(prep_df)) {
    stop("Source CLASSIFIED_ANALYSIS does not contain class_num.", call. = FALSE)
  }
  if (!"class_label" %in% names(prep_df)) {
    prep_df$class_label <- paste0("Class ", prep_df$class_num)
  }

  missing_vars <- setdiff(required_vars, names(prep_df))
  if (length(missing_vars) > 0 && is.data.frame(RAW_DATA) && nrow(RAW_DATA) > 0) {
    raw_keep <- intersect(c(id_var, missing_vars), names(RAW_DATA))
    if (length(raw_keep) > 0) {
      if (id_var %in% names(prep_df) && id_var %in% names(RAW_DATA) &&
          length(unique(prep_df[[id_var]])) == nrow(prep_df) &&
          length(unique(RAW_DATA[[id_var]])) == nrow(RAW_DATA)) {
        prep_df <- merge(
          prep_df,
          unique(RAW_DATA[, raw_keep, drop = FALSE]),
          by = id_var,
          all.x = TRUE,
          sort = FALSE
        )
      } else if (nrow(prep_df) == nrow(RAW_DATA)) {
        for (nm in setdiff(raw_keep, id_var)) {
          if (!nm %in% names(prep_df)) prep_df[[nm]] <- RAW_DATA[[nm]]
        }
      }
    }
  }
}

still_missing <- setdiff(required_vars, names(prep_df))
if (length(still_missing) > 0) {
  stop(
    "PROCESS input variables are missing after merge: ",
    paste(still_missing, collapse = ", "),
    call. = FALSE
  )
}

dict_meta <- DICT$meta %||% DICT$META %||% data.frame()
if (!is.data.frame(dict_meta)) dict_meta <- data.frame()

lookup_meta <- function(var_name) {
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0 || !("var_name" %in% names(dict_meta))) {
    return(list(var_label = var_name, type = NA_character_, display_order = NA_real_))
  }
  hit <- dict_meta[dict_meta$var_name == var_name, , drop = FALSE]
  if (nrow(hit) == 0) {
    return(list(var_label = var_name, type = NA_character_, display_order = NA_real_))
  }
  list(
    var_label = as.character(hit$var_label %||% hit$label_ko %||% hit$label_en %||% var_name)[1],
    type = as.character(hit$type %||% NA_character_)[1],
    display_order = suppressWarnings(as.numeric(hit$display_order %||% NA_real_))[1]
  )
}

is_continuous_like <- function(x, dict_type = NA_character_) {
  if (!is.na(dict_type) && tolower(dict_type) %in% c("continuous", "numeric", "scale")) return(TRUE)
  x_num <- suppressWarnings(as.numeric(x))
  n_unique <- length(unique(stats::na.omit(x_num)))
  !all(is.na(x_num)) && n_unique > 5
}

var_specs <- lapply(required_vars, function(v) {
  meta_i <- lookup_meta(v)
  list(
    var_name = v,
    var_label = meta_i$var_label,
    type = meta_i$type,
    display_order = meta_i$display_order,
    is_continuous = is_continuous_like(prep_df[[v]], meta_i$type)
  )
})

VAR_SPECS <- do.call(
  rbind,
  lapply(var_specs, function(x) as.data.frame(x, stringsAsFactors = FALSE))
)
rownames(VAR_SPECS) <- NULL

bad_y <- VAR_SPECS$var_name[VAR_SPECS$var_name %in% PROCESS_SETTINGS$y_vars & !VAR_SPECS$is_continuous]
bad_x <- VAR_SPECS$var_name[VAR_SPECS$var_name %in% PROCESS_SETTINGS$x_vars & !VAR_SPECS$is_continuous]
bad_m <- VAR_SPECS$var_name[VAR_SPECS$var_name %in% PROCESS_SETTINGS$m_vars & !VAR_SPECS$is_continuous]
bad_w <- VAR_SPECS$var_name[VAR_SPECS$var_name %in% PROCESS_SETTINGS$w_var & !VAR_SPECS$is_continuous]

if (length(bad_y) > 0) {
  stop("Outcomes for process_macro must be continuous-like: ", paste(bad_y, collapse = ", "), call. = FALSE)
}
if (PROCESS_SETTINGS$process_model == 1L && length(bad_x) > 0) {
  stop("Independent variables for process_macro Model 1 must be continuous-like: ", paste(bad_x, collapse = ", "), call. = FALSE)
}
if (PROCESS_SETTINGS$process_model == 4L && length(bad_m) > 0) {
  stop("Mediators for process_macro Model 4 must be continuous-like: ", paste(bad_m, collapse = ", "), call. = FALSE)
}
if (observed_source && length(bad_w) > 0) {
  if (isTRUE(PROCESS_SETTINGS$custom_model_enabled)) {
    stop("Observed moderator for process_macro custom model must be continuous-like: ", paste(bad_w, collapse = ", "), call. = FALSE)
  }
  stop("Observed moderator for process_macro Model 1 must be continuous-like: ", paste(bad_w, collapse = ", "), call. = FALSE)
}

PREP_SUMMARY <- list(
  source_analysis_id = PROCESS_SETTINGS$source_analysis_id,
  source_reference_class = SOURCE_REFERENCE_CLASS,
  source_reference_class_label = SOURCE_REFERENCE_CLASS_LABEL,
  best_k = SOURCE_BEST_K_SUMMARY$best_k %||% NA_integer_,
  n_rows = nrow(prep_df),
  n_x = length(PROCESS_SETTINGS$x_vars),
  n_outcomes = length(PROCESS_SETTINGS$y_vars),
  n_mediators = length(PROCESS_SETTINGS$m_vars),
  n_covariates = length(PROCESS_SETTINGS$covariates),
  created_at = Sys.time()
)

save_named_rds_list(
  list(
    PROCESS_DATA = prep_df,
    VAR_SPECS = VAR_SPECS,
    PREP_SUMMARY = PREP_SUMMARY,
    SOURCE_REFERENCE_CLASS = SOURCE_REFERENCE_CLASS,
    SOURCE_REFERENCE_CLASS_LABEL = SOURCE_REFERENCE_CLASS_LABEL,
    SOURCE_CLASS_SUMMARY = SOURCE_CLASS_SUMMARY
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_PREP, units = "secs")), 2)
log_info("02_prep.R completed.")
log_info("n(process rows)    = ", nrow(prep_df))
log_info("reference_class    = ", SOURCE_REFERENCE_CLASS)
log_info("reference_label    = ", SOURCE_REFERENCE_CLASS_LABEL)
log_info("elapsed            = ", elapsed_sec, " sec")
log_step_end("prep", elapsed_sec, ok = TRUE)
