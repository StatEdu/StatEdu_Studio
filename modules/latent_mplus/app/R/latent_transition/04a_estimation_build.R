T0_BUILD <- Sys.time()

log_step_start("ESTIMATION_BUILD", "04a_estimation_build.R")
log_info("Building Mplus input for latent transition model ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

wrap_stmt <- function(keyword, vars, indent = "  ", width = 78) {
  vars <- unique(as.character(vars))
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (length(vars) == 0) return(character(0))
  line_prefix <- paste0(indent, keyword, " = ")
  cont_prefix <- paste0(indent, "  ")
  out <- character(0)
  current <- line_prefix
  for (v in vars) {
    candidate <- if (identical(current, line_prefix)) paste0(current, v) else paste(current, v)
    if (nchar(candidate, type = "width") > width) {
      out <- c(out, current)
      current <- paste0(cont_prefix, v)
    } else {
      current <- candidate
    }
  }
  c(out, paste0(current, ";"))
}

make_transition_lines <- function(wave_order, n_classes) {
  lines <- character(0)
  for (i in seq_len(length(wave_order) - 1L)) {
    lines <- c(lines, paste0("  c", i + 1L, " ON c", i, ";"))
  }
  lines
}

make_lca_measurement_lines <- function(spec) {
  lines <- character(0)
  for (i in seq_along(spec$wave_order)) {
    wave_i <- spec$wave_order[i]
    vars_i <- spec$indicators_by_wave[[wave_i]] %||% character(0)
    lines <- c(lines, paste0("MODEL c", i, ":"))
    for (k in seq_len(spec$n_classes)) {
      lines <- c(lines, paste0("  %c", i, "#", k, "%"))
      for (j in seq_along(vars_i)) {
        var_j <- vars_i[j]
        n_th <- spec$threshold_counts[[var_j]] %||% 1L
        for (h in seq_len(max(1L, n_th))) {
          label_i <- if (spec$invariance %in% c("thresholds", "strong")) {
            paste0("th", j, "_c", k, "_h", h)
          } else {
            paste0("th", i, "_", j, "_c", k, "_h", h)
          }
          lines <- c(lines, paste0("    [", var_j, "$", h, "] (", label_i, ");"))
        }
      }
    }
  }
  lines
}

make_lpa_measurement_lines <- function(spec) {
  lines <- character(0)
  overall_vars <- unique(unlist(spec$indicators_by_wave, use.names = FALSE))
  if (spec$invariance %in% c("means", "strong")) {
    for (j in seq_along(overall_vars)) {
      lines <- c(lines, paste0("  ", overall_vars[j], " (v", j, ");"))
    }
  }
  for (i in seq_along(spec$wave_order)) {
    wave_i <- spec$wave_order[i]
    vars_i <- spec$indicators_by_wave[[wave_i]] %||% character(0)
    lines <- c(lines, paste0("MODEL c", i, ":"))
    for (k in seq_len(spec$n_classes)) {
      lines <- c(lines, paste0("  %c", i, "#", k, "%"))
      for (j in seq_along(vars_i)) {
        label_i <- if (spec$invariance %in% c("means", "strong")) paste0("mu", j, "_c", k) else paste0("mu", i, "_", j, "_c", k)
        lines <- c(lines, paste0("    [", vars_i[j], "] (", label_i, ");"))
      }
    }
  }
  lines
}

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
MPLUS_DATA <- load_step_rds("MPLUS_DATA", dir_rds = DIR_RDS, required = TRUE)
LTA_SPEC <- load_step_rds("LTA_SPEC", dir_rds = DIR_RDS, required = TRUE)

MISSING_CODE <- CFG$mplus_missing_code %||% CFG$missing_code %||% -9999
PROCESSORS <- as.integer(CFG$mplus$processors %||% CFG$estimation$processors %||% 2L)
ESTIMATOR <- toupper(as.character(CFG$mplus$estimator %||% CFG$estimation$estimator %||% "MLR")[1])
STARTS <- as.character(CFG$mplus$starts %||% CFG$estimation$starts %||% "500 100")[1]
STITERATIONS <- as.integer(CFG$mplus$stiterations %||% CFG$estimation$stiterations %||% 20L)
MPLUS_EXE <- resolve_mplus_exe(CFG, must_exist = FALSE)

MPLUS_EXPORT_DATA <- MPLUS_DATA
for (nm in names(MPLUS_EXPORT_DATA)) {
  x <- MPLUS_EXPORT_DATA[[nm]]
  if (is.factor(x)) x <- as.character(x)
  if (!identical(nm, LTA_SPEC$id_var)) suppressWarnings(x <- as.numeric(x))
  if (!is.numeric(x)) suppressWarnings(x <- as.numeric(x))
  x[is.na(x)] <- MISSING_CODE
  MPLUS_EXPORT_DATA[[nm]] <- x
}

MPLUS_DATA_FILE <- file.path(DIR_MPLUS_DATA, "latent_transition_data.dat")
write_mplus_data(MPLUS_EXPORT_DATA, MPLUS_DATA_FILE, missing_code = MISSING_CODE, sep = "\t")

all_names <- names(MPLUS_EXPORT_DATA)
indicator_vars <- LTA_SPEC$all_indicator_vars
latent_class_terms <- paste0("c", seq_along(LTA_SPEC$wave_order), "(", LTA_SPEC$n_classes, ")")

variable_lines <- c(
  wrap_stmt("NAMES", all_names),
  wrap_stmt("USEVARIABLES", c(LTA_SPEC$id_var, indicator_vars)),
  paste0("  CLASSES = ", paste(latent_class_terms, collapse = " "), ";"),
  paste0("  MISSING = ALL(", MISSING_CODE, ");"),
  paste0("  IDVARIABLE = ", LTA_SPEC$id_var, ";")
)

if (identical(LTA_SPEC$measurement_mode, "lca")) {
  variable_lines <- c(variable_lines, wrap_stmt("CATEGORICAL", indicator_vars))
}

analysis_lines <- c(
  "  TYPE = MIXTURE;",
  paste0("  ESTIMATOR = ", ESTIMATOR, ";"),
  paste0("  STARTS = ", STARTS, ";"),
  paste0("  STITERATIONS = ", STITERATIONS, ";"),
  paste0("  PROCESSORS = ", PROCESSORS, ";")
)

model_lines <- c("  %OVERALL%", make_transition_lines(LTA_SPEC$wave_order, LTA_SPEC$n_classes))
model_lines <- c(
  model_lines,
  if (identical(LTA_SPEC$measurement_mode, "lca")) make_lca_measurement_lines(LTA_SPEC) else make_lpa_measurement_lines(LTA_SPEC)
)

model_tag <- paste0(tolower(DATASET_ID), "_", ANALYSIS_ID, "_", LTA_SPEC$measurement_mode, "_k", LTA_SPEC$n_classes)
inp_file <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp"))
out_file <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
log_file <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".log"))
cprob_file <- file.path(DIR_MPLUS_SAVEDATA, paste0(model_tag, "_cprob.dat"))

inp_text <- c(
  paste0("TITLE: ", model_tag, ";"),
  "",
  "DATA:",
  paste0("  FILE = ", gsub("\\\\", "/", MPLUS_DATA_FILE), ";"),
  "",
  "VARIABLE:",
  variable_lines,
  "",
  "ANALYSIS:",
  analysis_lines,
  "",
  "MODEL:",
  model_lines,
  "",
  "OUTPUT:",
  "  TECH1;",
  "  TECH8;",
  "",
  "SAVEDATA:",
  paste0("  FILE = ", gsub("\\\\", "/", cprob_file), ";"),
  "  SAVE = CPROBABILITIES;",
  ""
)
writeLines(inp_text, con = inp_file, useBytes = TRUE)

ESTIMATION_REGISTRY <- data.frame(
  model_tag = model_tag,
  inp_file = inp_file,
  out_file = out_file,
  log_file = log_file,
  savedata_file = cprob_file,
  data_file = MPLUS_DATA_FILE,
  mplus_exe = MPLUS_EXE,
  measurement_mode = LTA_SPEC$measurement_mode,
  n_classes = LTA_SPEC$n_classes,
  stringsAsFactors = FALSE
)

ESTIMATION_BUILD_SUMMARY <- list(
  model_tag = model_tag,
  n_models = 1L,
  measurement_mode = LTA_SPEC$measurement_mode,
  n_classes = LTA_SPEC$n_classes,
  mplus_data_file = MPLUS_DATA_FILE,
  created_at = Sys.time()
)

save_named_rds_list(
  list(
    MPLUS_EXPORT_DATA = MPLUS_EXPORT_DATA,
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY,
    ESTIMATION_BUILD_SUMMARY = ESTIMATION_BUILD_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("estimation_build", round(as.numeric(difftime(Sys.time(), T0_BUILD, units = "secs")), 2), ok = TRUE)
