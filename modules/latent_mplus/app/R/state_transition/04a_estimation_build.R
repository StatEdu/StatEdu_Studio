T0_BUILD <- Sys.time()

log_step_start("ESTIMATION_BUILD", "04a_estimation_build.R")
log_info("Building Mplus input for state-transition model ...")

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

mplus_safe_names <- function(x) {
  x <- as.character(x)
  out <- gsub("[^A-Za-z0-9_]", "_", x)
  out <- gsub("_+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  out[is.na(out) | !nzchar(out)] <- "V"
  out <- ifelse(grepl("^[A-Za-z]", out), out, paste0("V_", out))
  out <- substr(out, 1L, 80L)

  used <- character(0)
  for (i in seq_along(out)) {
    base <- out[i]
    cand <- base
    suffix <- 1L
    while (toupper(cand) %in% toupper(used)) {
      suffix_txt <- paste0("_", suffix)
      cand <- paste0(substr(base, 1L, 80L - nchar(suffix_txt)), suffix_txt)
      suffix <- suffix + 1L
    }
    out[i] <- cand
    used <- c(used, cand)
  }
  out
}

filter_mplus_predictors <- function(data, predictors, missing_code = -9999) {
  predictors <- unique(as.character(predictors))
  predictors <- predictors[predictors %in% names(data)]
  dropped <- character(0)

  repeat {
    if (length(predictors) == 0) break
    xdat <- data[, predictors, drop = FALSE]
    observed <- as.data.frame(lapply(xdat, function(x) !is.na(x) & x != missing_code))
    complete <- Reduce(`&`, observed)
    if (!any(complete)) break

    varying <- vapply(predictors, function(v) {
      vals <- data[[v]][complete]
      vals <- vals[!is.na(vals) & vals != missing_code]
      length(unique(vals)) >= 2L
    }, logical(1))

    drop_now <- predictors[!varying]
    if (length(drop_now) == 0) break
    dropped <- unique(c(dropped, drop_now))
    predictors <- predictors[varying]
  }

  list(keep = predictors, dropped = dropped)
}

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
MPLUS_DATA <- load_step_rds("MPLUS_DATA", dir_rds = DIR_RDS, required = TRUE)
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, required = TRUE)

if (!is.data.frame(MPLUS_DATA) || nrow(MPLUS_DATA) == 0) {
  stop("MPLUS_DATA is missing or empty.", call. = FALSE)
}

wave_order <- PREP_SUMMARY$wave_order %||% character(0)
state_values <- PREP_SUMMARY$mplus_state_values %||% PREP_SUMMARY$state_values %||% integer(0)
reference_state <- PREP_SUMMARY$reference_state_mplus %||% max(state_values, na.rm = TRUE)
transition_specs <- PREP_SUMMARY$transition_specs %||% list()
active_baseline_covariates <- PREP_SUMMARY$active_baseline_covariates %||% character(0)
active_mediator_covariates <- PREP_SUMMARY$active_mediator_covariates %||% character(0)
covariate_specs <- PREP_SUMMARY$covariate_specs %||% list()

if (length(wave_order) < 2 || length(state_values) < 2) {
  stop("Insufficient wave/state specification for Mplus transition model.", call. = FALSE)
}

MISSING_CODE <- CFG$mplus_missing_code %||% CFG$missing_code %||% -9999
PROCESSORS <- as.integer(CFG$mplus$processors %||% CFG$estimation$processors %||% 2L)
ESTIMATOR <- toupper(as.character(CFG$mplus$estimator %||% CFG$estimation$estimator %||% "MLR")[1])
MPLUS_EXE <- resolve_mplus_exe(CFG, must_exist = FALSE)

MPLUS_EXPORT_DATA <- MPLUS_DATA
for (nm in names(MPLUS_EXPORT_DATA)) {
  x <- MPLUS_EXPORT_DATA[[nm]]
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) x <- as.integer(x)
  if (is.character(x)) suppressWarnings(x <- as.numeric(x))
  if (!is.numeric(x)) suppressWarnings(x <- as.numeric(x))
  x[is.na(x)] <- MISSING_CODE
  MPLUS_EXPORT_DATA[[nm]] <- x
}

MPLUS_NAME_MAP <- data.frame(
  original = names(MPLUS_EXPORT_DATA),
  mplus = mplus_safe_names(names(MPLUS_EXPORT_DATA)),
  stringsAsFactors = FALSE
)
mediator_name_idx <- match(active_mediator_covariates, MPLUS_NAME_MAP$original)
mediator_name_idx <- mediator_name_idx[!is.na(mediator_name_idx)]
if (length(mediator_name_idx) > 0) {
  existing_names <- toupper(MPLUS_NAME_MAP$mplus[-mediator_name_idx])
  mediator_names <- character(length(mediator_name_idx))
  for (i in seq_along(mediator_name_idx)) {
    cand <- sprintf("MED%03d", i)
    suffix <- i
    while (toupper(cand) %in% c(existing_names, toupper(mediator_names))) {
      cand <- sprintf("MD%04d", suffix)
      suffix <- suffix + 1L
    }
    mediator_names[i] <- cand
  }
  MPLUS_NAME_MAP$mplus[mediator_name_idx] <- mediator_names
}
name_lookup <- stats::setNames(MPLUS_NAME_MAP$mplus, MPLUS_NAME_MAP$original)
name_lookup_rev <- stats::setNames(MPLUS_NAME_MAP$original, MPLUS_NAME_MAP$mplus)
names(MPLUS_EXPORT_DATA) <- MPLUS_NAME_MAP$mplus

MPLUS_DATA_FILE <- file.path(DIR_MPLUS_DATA, "state_transition_data.dat")
write_mplus_data(MPLUS_EXPORT_DATA, MPLUS_DATA_FILE, missing_code = MISSING_CODE, sep = "\t")

all_names <- names(MPLUS_EXPORT_DATA)
nominal_vars <- unname(name_lookup[wave_order[-1]])
transition_specs_mplus <- lapply(transition_specs, function(x) {
  x$from_wave <- unname(name_lookup[x$from_wave] %||% x$from_wave)
  x$to_wave <- unname(name_lookup[x$to_wave] %||% x$to_wave)
  x$dummy_vars <- unname(name_lookup[x$dummy_vars] %||% x$dummy_vars)
  x$time_varying_covariates <- unname(name_lookup[x$time_varying_covariates] %||% x$time_varying_covariates)
  x
})
active_baseline_covariates_mplus <- unname(name_lookup[active_baseline_covariates])
active_baseline_covariates_mplus <- active_baseline_covariates_mplus[!is.na(active_baseline_covariates_mplus)]
active_mediator_covariates_mplus <- unname(name_lookup[active_mediator_covariates])
active_mediator_covariates_mplus <- active_mediator_covariates_mplus[!is.na(active_mediator_covariates_mplus)]
predictor_vars <- unlist(lapply(transition_specs_mplus, function(x) x$dummy_vars), use.names = FALSE)
time_varying_predictor_vars <- unlist(lapply(transition_specs_mplus, function(x) x$time_varying_covariates), use.names = FALSE)
predictor_vars <- unique(c(predictor_vars, active_baseline_covariates_mplus))
predictor_vars <- unique(c(predictor_vars, time_varying_predictor_vars))
predictor_vars <- unique(c(predictor_vars, active_mediator_covariates_mplus))
predictor_filter <- filter_mplus_predictors(MPLUS_EXPORT_DATA, predictor_vars, missing_code = MISSING_CODE)
predictor_vars <- predictor_filter$keep
active_baseline_covariates_mplus <- intersect(active_baseline_covariates_mplus, predictor_vars)
active_mediator_covariates_mplus <- intersect(active_mediator_covariates_mplus, predictor_vars)
transition_specs_mplus <- lapply(transition_specs_mplus, function(x) {
  keep_i <- x$dummy_vars %in% predictor_vars
  x$dummy_vars <- x$dummy_vars[keep_i]
  x$dummy_states <- (x$dummy_states %||% integer(0))[keep_i]
  x$time_varying_covariates <- intersect(x$time_varying_covariates %||% character(0), predictor_vars)
  x
})
if (length(predictor_filter$dropped) > 0) {
  log_info("Dropped zero-variance Mplus predictors: ", paste(predictor_filter$dropped, collapse = ", "))
}

make_variable_lines <- function(active_predictors) {
  c(
    wrap_stmt("NAMES", all_names),
    wrap_stmt("USEVARIABLES", c("panel_id", nominal_vars, active_predictors)),
    wrap_stmt("NOMINAL", nominal_vars),
    paste0("  MISSING = ALL(", MISSING_CODE, ");"),
    "  IDVARIABLE = panel_id;"
  )
}

variable_lines <- make_variable_lines(predictor_vars)

analysis_lines <- c(
  "  TYPE = GENERAL;",
  paste0("  ESTIMATOR = ", ESTIMATOR, ";"),
  paste0("  PROCESSORS = ", PROCESSORS, ";")
)

covariate_specs_df <- if (is.list(covariate_specs) && length(covariate_specs) > 0) {
  spec_dfs <- lapply(covariate_specs, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  spec_cols <- unique(unlist(lapply(spec_dfs, names), use.names = FALSE))
  spec_dfs <- lapply(spec_dfs, function(x) {
    missing_cols <- setdiff(spec_cols, names(x))
    for (nm in missing_cols) x[[nm]] <- NA
    x[, spec_cols, drop = FALSE]
  })
  do.call(rbind, spec_dfs)
} else {
  data.frame()
}

make_global_covariate_key <- function(mplus_name) {
  original_name <- unname(name_lookup_rev[mplus_name] %||% mplus_name)
  if (!is.data.frame(covariate_specs_df) || nrow(covariate_specs_df) == 0 || !"name" %in% names(covariate_specs_df)) {
    return(original_name)
  }
  hit <- covariate_specs_df[as.character(covariate_specs_df$name) == original_name, , drop = FALSE]
  if (nrow(hit) == 0) return(original_name)
  scope <- as.character(hit$covariate_scope[1] %||% "baseline")
  if (scope %in% c("time_varying", "mediator")) {
    paste(
      scope,
      as.character(hit$source_label[1] %||% hit$source_var[1] %||% original_name),
      as.character(hit$predictor_type[1] %||% ""),
      as.character(hit$level[1] %||% ""),
      sep = "|"
    )
  } else {
    original_name
  }
}

global_covariate_keys <- unique(vapply(
  predictor_vars,
  make_global_covariate_key,
  character(1)
))
global_covariate_labels <- data.frame(
  global_key = global_covariate_keys,
  label_base = paste0("g", seq_along(global_covariate_keys)),
  stringsAsFactors = FALSE
)

global_label_for <- function(mplus_name, k, prefix = "g") {
  key <- make_global_covariate_key(mplus_name)
  base <- global_covariate_labels$label_base[global_covariate_labels$global_key == key]
  if (length(base) == 0 || is.na(base[1])) base <- "gx"
  paste0(prefix, sub("^g", "", base[1]), "_", k)
}

mediation_transition_specs_mplus <- transition_specs_mplus
if (length(active_mediator_covariates_mplus) > 0 && is.data.frame(covariate_specs_df) && nrow(covariate_specs_df) > 0) {
  for (i in seq_along(mediation_transition_specs_mplus)) {
    spec_i <- mediation_transition_specs_mplus[[i]]
    from_original <- unname(name_lookup_rev[spec_i$from_wave] %||% spec_i$from_wave)
    to_original <- unname(name_lookup_rev[spec_i$to_wave] %||% spec_i$to_wave)
    mediator_original <- covariate_specs_df$name[
      as.character(covariate_specs_df$covariate_scope %||% "") == "mediator" &
        as.character(covariate_specs_df$from_wave %||% "") == from_original &
        as.character(covariate_specs_df$to_wave %||% "") == to_original
    ]
    mediator_mplus <- unname(name_lookup[mediator_original])
    mediator_mplus <- mediator_mplus[!is.na(mediator_mplus) & mediator_mplus %in% active_mediator_covariates_mplus]
    mediation_transition_specs_mplus[[i]]$time_varying_covariates <- mediator_mplus
  }
}

make_covariate_model_lines <- function(selected_baseline_covariates, selected_time_varying_covariates, invariant = FALSE, label_prefix = "g", transition_specs_model = transition_specs_mplus) {
  selected_baseline_covariates <- intersect(selected_baseline_covariates %||% character(0), active_baseline_covariates_mplus)
  selected_time_varying_covariates <- intersect(selected_time_varying_covariates %||% character(0), predictor_vars)
  out <- character(0)
  cov_lines <- character(0)

  for (i in seq_along(transition_specs_model)) {
    spec <- transition_specs_model[[i]]
    wave_idx <- i + 1L
    active_dummy_states <- spec$dummy_states %||% integer(0)
    spec_tv <- intersect(spec$time_varying_covariates %||% character(0), selected_time_varying_covariates)

    for (k in seq_len(length(state_values) - 1)) {
      out <- c(out, paste0("  [", spec$to_wave, "#", k, "] (a", wave_idx, k, ");"))
      for (j in seq_along(active_dummy_states)) {
        st <- active_dummy_states[j]
        line_j <- paste0("  ", spec$to_wave, "#", k, " ON ", spec$dummy_vars[j], " (b", wave_idx, k, st, ");")
        out <- c(out, line_j)
        if (invariant) cov_lines <- c(cov_lines, line_j)
      }
      for (cv in selected_baseline_covariates) {
        cov_lines <- c(
          cov_lines,
          if (invariant) {
            paste0("  ", spec$to_wave, "#", k, " ON ", cv, " (", global_label_for(cv, k, prefix = label_prefix), ");")
          } else {
            paste0("  ", spec$to_wave, "#", k, " ON ", cv, ";")
          }
        )
      }
      for (cv in spec_tv) {
        cov_lines <- c(
          cov_lines,
          if (invariant) {
            paste0("  ", spec$to_wave, "#", k, " ON ", cv, " (", global_label_for(cv, k, prefix = label_prefix), ");")
          } else {
            paste0("  ", spec$to_wave, "#", k, " ON ", cv, ";")
          }
        )
      }
    }
  }

  if (invariant) {
    c(out[!grepl(" ON ", out, fixed = TRUE)], cov_lines)
  } else {
    c(out, cov_lines)
  }
}

model_lines <- make_covariate_model_lines(active_baseline_covariates_mplus, time_varying_predictor_vars, invariant = FALSE)
global_model_lines <- make_covariate_model_lines(active_baseline_covariates_mplus, time_varying_predictor_vars, invariant = TRUE, label_prefix = "g")
constraint_lines <- character(0)

new_lines <- c(
  character(0)
)

for (i in seq_along(transition_specs)) {
  wave_from_idx <- i
  wave_to_idx <- i + 1L
  new_names <- character(0)
  eq_lines <- character(0)

  for (fs in state_values) {
    for (ts in state_values) {
      new_names <- c(new_names, paste0("tr", wave_from_idx, wave_to_idx, fs, ts))
    }

    eta1 <- paste0("a", wave_to_idx, "1")
    eta2 <- paste0("a", wave_to_idx, "2")
    if (fs %in% (transition_specs_mplus[[i]]$dummy_states %||% integer(0))) {
      eta1 <- paste0(eta1, "+b", wave_to_idx, "1", fs)
      eta2 <- paste0(eta2, "+b", wave_to_idx, "2", fs)
    }

    eq_lines <- c(
      eq_lines,
      paste0("  tr", wave_from_idx, wave_to_idx, fs, state_values[1], " = exp(", eta1, ")/(1+exp(", eta1, ")+exp(", eta2, "));"),
      paste0("  tr", wave_from_idx, wave_to_idx, fs, state_values[2], " = exp(", eta2, ")/(1+exp(", eta1, ")+exp(", eta2, "));"),
      paste0("  tr", wave_from_idx, wave_to_idx, fs, state_values[3], " = 1/(1+exp(", eta1, ")+exp(", eta2, "));")
    )
  }

  new_lines <- c(new_lines, paste0("  NEW(", paste(new_names, collapse = " "), ");"), eq_lines)
}

model_tag <- paste0(tolower(DATASET_ID), "_", ANALYSIS_ID, "_mplus")
global_model_tag <- paste0(tolower(DATASET_ID), "_", ANALYSIS_ID, "_global_covariates_mplus")
inp_file <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp"))
out_file <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
log_file <- file.path(DIR_MPLUS_INP, paste0(model_tag, ".log"))
global_inp_file <- file.path(DIR_MPLUS_INP, paste0(global_model_tag, ".inp"))
global_out_file <- file.path(DIR_MPLUS_INP, paste0(global_model_tag, ".out"))
global_log_file <- file.path(DIR_MPLUS_INP, paste0(global_model_tag, ".log"))
mediation_model_tag <- paste0(tolower(DATASET_ID), "_", ANALYSIS_ID, "_bmi_mediation_mplus")
mediation_inp_file <- file.path(DIR_MPLUS_INP, paste0(mediation_model_tag, ".inp"))
mediation_out_file <- file.path(DIR_MPLUS_INP, paste0(mediation_model_tag, ".out"))
mediation_log_file <- file.path(DIR_MPLUS_INP, paste0(mediation_model_tag, ".log"))

write_state_mplus_input <- function(model_tag_i, inp_file_i, model_lines_i, active_predictors_i) {
  inp_text_i <- c(
    paste0("TITLE: ", model_tag_i, ";"),
    "",
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", MPLUS_DATA_FILE), ";"),
    "",
    "VARIABLE:",
    make_variable_lines(active_predictors_i),
    "",
    "ANALYSIS:",
    analysis_lines,
    "",
    "MODEL:",
    model_lines_i,
    "",
    "MODEL CONSTRAINT:",
    new_lines,
    "",
    "OUTPUT:",
    "  TECH1;",
    ""
  )
  writeLines(inp_text_i, con = inp_file_i, useBytes = TRUE)
}

write_state_mplus_input(model_tag, inp_file, model_lines, predictor_vars)
write_state_mplus_input(global_model_tag, global_inp_file, global_model_lines, predictor_vars)
if (length(active_mediator_covariates_mplus) > 0) {
  mediation_predictors <- unique(c(
    unlist(lapply(mediation_transition_specs_mplus, function(x) x$dummy_vars), use.names = FALSE),
    active_baseline_covariates_mplus,
    active_mediator_covariates_mplus
  ))
  write_state_mplus_input(
    mediation_model_tag,
    mediation_inp_file,
    make_covariate_model_lines(
      active_baseline_covariates_mplus,
      active_mediator_covariates_mplus,
      invariant = FALSE,
      transition_specs_model = mediation_transition_specs_mplus
    ),
    mediation_predictors
  )
}

registry_rows <- list(
  data.frame(
    model_tag = model_tag,
    model_type = "interval_specific_covariates",
    covariate_key = NA_character_,
    inp_file = inp_file,
    out_file = out_file,
    log_file = log_file,
    stringsAsFactors = FALSE
  ),
  data.frame(
    model_tag = global_model_tag,
    model_type = "global_covariate_effects",
    covariate_key = NA_character_,
    inp_file = global_inp_file,
    out_file = global_out_file,
    log_file = global_log_file,
    stringsAsFactors = FALSE
  )
)

if (length(active_mediator_covariates_mplus) > 0) {
  registry_rows <- c(
    registry_rows,
    list(
      data.frame(
        model_tag = mediation_model_tag,
        model_type = "bmi_mediation_transition",
        covariate_key = NA_character_,
        inp_file = mediation_inp_file,
        out_file = mediation_out_file,
        log_file = mediation_log_file,
        stringsAsFactors = FALSE
      )
    )
  )
}

covariate_predictor_vars <- unique(c(active_baseline_covariates_mplus, time_varying_predictor_vars))
univariable_keys <- unique(vapply(covariate_predictor_vars, make_global_covariate_key, character(1)))
univariable_keys <- univariable_keys[!is.na(univariable_keys) & nzchar(univariable_keys)]

for (key_i in univariable_keys) {
  covs_i <- covariate_predictor_vars[
    vapply(covariate_predictor_vars, make_global_covariate_key, character(1)) == key_i
  ]
  baseline_i <- intersect(covs_i, active_baseline_covariates_mplus)
  time_varying_i <- intersect(covs_i, time_varying_predictor_vars)
  predictors_i <- unique(c(unlist(lapply(transition_specs_mplus, function(x) x$dummy_vars), use.names = FALSE), baseline_i, time_varying_i))
  key_tag_i <- mplus_safe_names(key_i)[1]
  uni_tag_i <- paste0(tolower(DATASET_ID), "_", ANALYSIS_ID, "_uni_", tolower(key_tag_i), "_mplus")
  uni_global_tag_i <- paste0(tolower(DATASET_ID), "_", ANALYSIS_ID, "_uni_", tolower(key_tag_i), "_global_mplus")
  uni_inp_i <- file.path(DIR_MPLUS_INP, paste0(uni_tag_i, ".inp"))
  uni_global_inp_i <- file.path(DIR_MPLUS_INP, paste0(uni_global_tag_i, ".inp"))

  write_state_mplus_input(
    uni_tag_i,
    uni_inp_i,
    make_covariate_model_lines(baseline_i, time_varying_i, invariant = FALSE),
    predictors_i
  )
  write_state_mplus_input(
    uni_global_tag_i,
    uni_global_inp_i,
    make_covariate_model_lines(baseline_i, time_varying_i, invariant = TRUE, label_prefix = "u"),
    predictors_i
  )

  registry_rows <- c(
    registry_rows,
    list(
      data.frame(
        model_tag = uni_tag_i,
        model_type = "univariable_interval_covariates",
        covariate_key = key_i,
        inp_file = uni_inp_i,
        out_file = file.path(DIR_MPLUS_INP, paste0(uni_tag_i, ".out")),
        log_file = file.path(DIR_MPLUS_INP, paste0(uni_tag_i, ".log")),
        stringsAsFactors = FALSE
      ),
      data.frame(
        model_tag = uni_global_tag_i,
        model_type = "univariable_global_covariate_effects",
        covariate_key = key_i,
        inp_file = uni_global_inp_i,
        out_file = file.path(DIR_MPLUS_INP, paste0(uni_global_tag_i, ".out")),
        log_file = file.path(DIR_MPLUS_INP, paste0(uni_global_tag_i, ".log")),
        stringsAsFactors = FALSE
      )
    )
  )
}

ESTIMATION_REGISTRY <- do.call(rbind, registry_rows)
ESTIMATION_REGISTRY <- cbind(
  ESTIMATION_REGISTRY,
  data.frame(
  data_file = MPLUS_DATA_FILE,
  mplus_exe = MPLUS_EXE,
  estimator = ESTIMATOR,
  n_waves = length(wave_order),
  n_states = length(state_values),
  stringsAsFactors = FALSE
  )
)

ESTIMATION_BUILD_SUMMARY <- list(
  model_tag = model_tag,
  n_models = 1L,
  n_waves = length(wave_order),
  n_states = length(state_values),
    reference_state = reference_state,
    baseline_covariates = active_baseline_covariates,
    baseline_covariates_mplus = active_baseline_covariates_mplus,
    time_varying_covariates_mplus = unique(unlist(lapply(transition_specs_mplus, function(x) x$time_varying_covariates %||% character(0)), use.names = FALSE)),
    mediator_covariates = active_mediator_covariates,
    mediator_covariates_mplus = active_mediator_covariates_mplus,
    dropped_mplus_predictors = predictor_filter$dropped,
    mplus_data_file = MPLUS_DATA_FILE,
    mplus_exe = MPLUS_EXE,
    created_at = Sys.time()
)

save_named_rds_list(
  list(
    MPLUS_EXPORT_DATA = MPLUS_EXPORT_DATA,
    MPLUS_NAME_MAP = MPLUS_NAME_MAP,
    GLOBAL_COVARIATE_LABELS = global_covariate_labels,
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY,
    ESTIMATION_BUILD_SUMMARY = ESTIMATION_BUILD_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("estimation_build", round(as.numeric(difftime(Sys.time(), T0_BUILD, units = "secs")), 2), ok = TRUE)
