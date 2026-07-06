T0_COLLECT <- Sys.time()

log_step_start("ESTIMATION_COLLECT", "04c_estimation_collect.R")
log_info("Collecting Mplus state-transition outputs ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_read_lines <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) return(character(0))
  tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0))
  )
}

extract_value_with_fallback <- function(pattern, txt, up, max_lookahead = 5L) {
  idx <- grep(pattern, up, perl = TRUE)
  if (length(idx) == 0) return(NA_real_)
  line <- trimws(txt[idx[1]])
  nums <- regmatches(line, gregexpr("-?[0-9]+\\.?[0-9]*", line, perl = TRUE))[[1]]
  vals <- suppressWarnings(as.numeric(nums))
  vals <- vals[!is.na(vals)]
  if (length(vals) > 0) return(vals[length(vals)])
  start <- idx[1] + 1L
  end <- min(length(txt), idx[1] + max_lookahead)
  for (j in seq.int(start, end)) {
    line_j <- trimws(txt[j])
    nums <- regmatches(line_j, gregexpr("-?[0-9]+\\.?[0-9]*", line_j, perl = TRUE))[[1]]
    vals <- suppressWarnings(as.numeric(nums))
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) return(vals[length(vals)])
  }
  NA_real_
}

parse_mplus_fit <- function(out_file, model_tag = NA_character_) {
  txt <- safe_read_lines(out_file)
  up <- toupper(txt)
  if (length(txt) == 0) {
    return(data.frame(
      model_tag = model_tag,
      out_file = out_file,
      parse_ok = FALSE,
      ll = NA_real_,
      aic = NA_real_,
      bic = NA_real_,
      sabic = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  ll_idx <- grep("^\\s*H0 VALUE\\s+", up)
  ll_val <- NA_real_
  if (length(ll_idx) > 0) {
    nums <- regmatches(txt[ll_idx[1]], gregexpr("-?[0-9]+\\.?[0-9]*", txt[ll_idx[1]], perl = TRUE))[[1]]
    vals <- suppressWarnings(as.numeric(nums))
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) ll_val <- vals[length(vals)]
  }

  out <- data.frame(
    model_tag = model_tag,
    out_file = out_file,
    parse_ok = TRUE,
    ll = ll_val,
    aic = extract_value_with_fallback("^\\s*AKAIKE \\(AIC\\)", txt, up),
    bic = extract_value_with_fallback("^\\s*BAYESIAN \\(BIC\\)", txt, up),
    sabic = extract_value_with_fallback("^\\s*SAMPLE-SIZE ADJUSTED BIC|SABIC|ADJUSTED BIC", txt, up),
    stringsAsFactors = FALSE
  )
  out$parse_ok <- !all(is.na(out[, c("ll", "aic", "bic", "sabic")]))
  out
}

parse_new_parameters <- function(out_file) {
  txt <- safe_read_lines(out_file)
  up <- toupper(txt)
  if (length(txt) == 0) return(data.frame())

  start_idx <- grep("NEW/ADDITIONAL PARAMETERS", up, fixed = TRUE)
  if (length(start_idx) == 0) start_idx <- grep("NEW ADDITIONAL PARAMETERS", up, fixed = TRUE)
  if (length(start_idx) == 0) return(data.frame())

  block <- txt[seq.int(start_idx[1], min(length(txt), start_idx[1] + 200L))]
  rows <- grep("^\\s*[A-Za-z][A-Za-z0-9_]*\\s+-?[0-9]", block, value = TRUE, perl = TRUE)
  if (length(rows) == 0) return(data.frame())

  out_list <- lapply(rows, function(ln) {
    parts <- strsplit(trimws(ln), "\\s+")[[1]]
    if (length(parts) < 2) return(NULL)
    data.frame(
      name = parts[1],
      estimate = suppressWarnings(as.numeric(parts[2])),
      se = if (length(parts) >= 3) suppressWarnings(as.numeric(parts[3])) else NA_real_,
      z = if (length(parts) >= 4) suppressWarnings(as.numeric(parts[4])) else NA_real_,
      p = if (length(parts) >= 5) suppressWarnings(as.numeric(parts[5])) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0) return(data.frame())
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

parse_model_results_nominal <- function(out_file) {
  txt <- safe_read_lines(out_file)
  up <- toupper(txt)
  start_idx <- grep("^MODEL RESULTS\\s*$", up)
  if (length(start_idx) == 0) return(data.frame())
  block <- txt[seq.int(start_idx[1] + 1L, length(txt))]
  block_up <- toupper(block)
  out_list <- list()
  current_outcome <- NA_character_
  idx <- 1L
  for (i in seq_along(block)) {
    ln <- block[i]
    up_i <- block_up[i]
    if (grepl("^\\s*INTERCEPTS\\s*$", up_i) || grepl("^\\s*NEW/ADDITIONAL PARAMETERS\\s*$", up_i) || grepl("^\\s*TECHNICAL", up_i)) break
    if (grepl("^\\s*[A-Z0-9_#]+\\s+ON\\s*$", up_i)) {
      current_outcome <- trimws(sub("\\s+ON\\s*$", "", up_i))
      next
    }
    if (is.na(current_outcome) || !nzchar(current_outcome)) next
    parts <- strsplit(trimws(ln), "\\s+")[[1]]
    if (length(parts) < 5) next
    vals <- suppressWarnings(as.numeric(parts[2:5]))
    if (any(is.na(vals))) next
    out_list[[idx]] <- data.frame(
      outcome = current_outcome,
      predictor = toupper(parts[1]),
      estimate = vals[1],
      se = vals[2],
      z = vals[3],
      p = vals[4],
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0) return(data.frame())
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

parse_model_odds_ratios <- function(out_file) {
  txt <- safe_read_lines(out_file)
  up <- toupper(txt)
  start_idx <- grep("^LOGISTIC REGRESSION ODDS RATIO RESULTS\\s*$", up)
  if (length(start_idx) == 0) return(data.frame())
  block <- txt[seq.int(start_idx[1] + 1L, length(txt))]
  block_up <- toupper(block)
  out_list <- list()
  current_outcome <- NA_character_
  idx <- 1L
  for (i in seq_along(block)) {
    ln <- block[i]
    up_i <- block_up[i]
    if (grepl("^\\s*TECHNICAL", up_i)) break
    if (grepl("^\\s*[A-Z0-9_#]+\\s+ON\\s*$", up_i)) {
      current_outcome <- trimws(sub("\\s+ON\\s*$", "", up_i))
      next
    }
    if (is.na(current_outcome) || !nzchar(current_outcome)) next
    parts <- strsplit(trimws(ln), "\\s+")[[1]]
    if (length(parts) < 5) next
    vals <- suppressWarnings(as.numeric(parts[2:5]))
    if (any(is.na(vals))) next
    out_list[[idx]] <- data.frame(
      outcome = current_outcome,
      predictor = toupper(parts[1]),
      odds_ratio = vals[1],
      or_se = vals[2],
      or_lcl = vals[3],
      or_ucl = vals[4],
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0) return(data.frame())
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

ESTIMATION_REGISTRY <- load_step_rds("ESTIMATION_REGISTRY", dir_rds = DIR_RDS, required = TRUE)
ESTIMATION_RUN_RESULTS <- load_step_rds("ESTIMATION_RUN_RESULTS", dir_rds = DIR_RDS, required = TRUE)
PREP_SUMMARY <- load_step_rds("PREP_SUMMARY", dir_rds = DIR_RDS, required = TRUE)
MPLUS_NAME_MAP <- load_step_rds("MPLUS_NAME_MAP", dir_rds = DIR_RDS, default = data.frame())
PANEL_LONG <- load_step_rds("PANEL_LONG", dir_rds = DIR_RDS, default = data.frame())
MPLUS_DATA <- load_step_rds("MPLUS_DATA", dir_rds = DIR_RDS, default = data.frame())

mplus_name_lookup <- character(0)
if (is.data.frame(MPLUS_NAME_MAP) && nrow(MPLUS_NAME_MAP) > 0 && all(c("original", "mplus") %in% names(MPLUS_NAME_MAP))) {
  mplus_name_lookup <- stats::setNames(as.character(MPLUS_NAME_MAP$mplus), as.character(MPLUS_NAME_MAP$original))
}

covariate_spec_df <- PREP_SUMMARY$covariate_specs %||% list()
if (is.list(covariate_spec_df) && length(covariate_spec_df) > 0) {
  spec_dfs <- lapply(covariate_spec_df, function(x) as.data.frame(x, stringsAsFactors = FALSE))
  spec_cols <- unique(unlist(lapply(spec_dfs, names), use.names = FALSE))
  spec_dfs <- lapply(spec_dfs, function(x) {
    missing_cols <- setdiff(spec_cols, names(x))
    for (nm in missing_cols) x[[nm]] <- NA
    x[, spec_cols, drop = FALSE]
  })
  covariate_spec_df <- do.call(rbind, spec_dfs)
} else {
  covariate_spec_df <- data.frame()
}
if (is.data.frame(covariate_spec_df) && nrow(covariate_spec_df) > 0) {
  covariate_spec_df$predictor_original <- as.character(covariate_spec_df$name %||% covariate_spec_df$predictor)
  covariate_spec_df$predictor <- mplus_name_lookup[covariate_spec_df$predictor_original]
  covariate_spec_df$predictor[is.na(covariate_spec_df$predictor)] <- covariate_spec_df$predictor_original[is.na(covariate_spec_df$predictor)]
  covariate_spec_df$predictor <- toupper(as.character(covariate_spec_df$predictor))
}

primary_idx <- which(as.character(ESTIMATION_REGISTRY$model_type %||% "") == "interval_specific_covariates")
if (length(primary_idx) == 0) primary_idx <- 1L
primary_idx <- primary_idx[1]
global_idx <- which(as.character(ESTIMATION_REGISTRY$model_type %||% "") == "global_covariate_effects")
global_idx <- global_idx[1]

out_file <- as.character(ESTIMATION_RUN_RESULTS$out_file[primary_idx] %||% ESTIMATION_REGISTRY$out_file[primary_idx])
fit_tbl <- parse_mplus_fit(out_file, model_tag = ESTIMATION_REGISTRY$model_tag[1])
new_params <- parse_new_parameters(out_file)
model_results <- parse_model_results_nominal(out_file)
odds_ratio_results <- parse_model_odds_ratios(out_file)
global_out_file <- if (!is.na(global_idx)) as.character(ESTIMATION_RUN_RESULTS$out_file[global_idx] %||% ESTIMATION_REGISTRY$out_file[global_idx]) else NA_character_
global_model_results <- if (!is.na(global_idx)) parse_model_results_nominal(global_out_file) else data.frame()
global_odds_ratio_results <- if (!is.na(global_idx)) parse_model_odds_ratios(global_out_file) else data.frame()

state_values <- PREP_SUMMARY$state_values %||% integer(0)
mplus_state_values <- PREP_SUMMARY$mplus_state_values %||% seq_along(state_values)
state_from_mplus <- PREP_SUMMARY$state_from_mplus %||% stats::setNames(mplus_state_values, as.character(mplus_state_values))
reference_state_original <- PREP_SUMMARY$reference_state %||% state_values[1]
wave_order <- PREP_SUMMARY$wave_order %||% character(0)
active_covariates_original <- unique(c(
  PREP_SUMMARY$active_baseline_covariates %||% character(0),
  PREP_SUMMARY$active_time_varying_covariates %||% character(0)
))
active_covariates <- mplus_name_lookup[active_covariates_original]
active_covariates[is.na(active_covariates)] <- active_covariates_original[is.na(active_covariates)]
active_covariates <- toupper(active_covariates)

baseline_params <- data.frame()
transition_params <- data.frame()
covariate_params <- data.frame()
global_covariate_params <- data.frame()
univariable_covariate_params <- data.frame()
univariable_global_covariate_params <- data.frame()
selected_covariate_params <- data.frame()
selected_global_covariate_params <- data.frame()
bmi_mediation_stage1 <- data.frame()
bmi_mediation_stage2 <- data.frame()
bmi_mediation_stage3 <- data.frame()

if (is.data.frame(new_params) && nrow(new_params) > 0) {
  name_lower <- tolower(new_params$name)
  baseline_params <- new_params[grepl("^pr1[0-9]+$", name_lower), , drop = FALSE]
  if (nrow(baseline_params) > 0) {
    baseline_params$wave <- toupper(wave_order[1])
    baseline_params$state <- suppressWarnings(as.integer(sub("^pr1([0-9]+)$", "\\1", tolower(baseline_params$name))))
    baseline_params$state <- unname(state_from_mplus[as.character(baseline_params$state)])
    baseline_params <- baseline_params[, c("wave", "state", "estimate", "se", "z", "p"), drop = FALSE]
    names(baseline_params)[names(baseline_params) == "estimate"] <- "prob"
  }

  transition_params <- new_params[grepl("^tr[0-9]{4}$", name_lower), , drop = FALSE]
  if (nrow(transition_params) > 0) {
    name_lower_i <- tolower(transition_params$name)
    transition_params$from_idx <- suppressWarnings(as.integer(substr(name_lower_i, 3, 3)))
    transition_params$to_idx <- suppressWarnings(as.integer(substr(name_lower_i, 4, 4)))
    transition_params$from_state <- suppressWarnings(as.integer(substr(name_lower_i, 5, 5)))
    transition_params$to_state <- suppressWarnings(as.integer(substr(name_lower_i, 6, 6)))
    transition_params$from_state <- unname(state_from_mplus[as.character(transition_params$from_state)])
    transition_params$to_state <- unname(state_from_mplus[as.character(transition_params$to_state)])
    transition_params$from_wave <- wave_order[transition_params$from_idx]
    transition_params$to_wave <- wave_order[transition_params$to_idx]
    transition_params$interval <- paste0(toupper(transition_params$from_wave), "_to_", toupper(transition_params$to_wave))
    transition_params <- transition_params[, c("interval", "from_wave", "to_wave", "from_state", "to_state", "estimate", "se", "z", "p"), drop = FALSE]
    names(transition_params)[names(transition_params) == "estimate"] <- "prob"
    transition_params <- transition_params[order(transition_params$interval, transition_params$from_state, transition_params$to_state), , drop = FALSE]
  }
}

if (is.data.frame(model_results) && nrow(model_results) > 0 && length(active_covariates) > 0) {
  covariate_params <- model_results[model_results$predictor %in% active_covariates, , drop = FALSE]
  if (is.data.frame(odds_ratio_results) && nrow(odds_ratio_results) > 0 && nrow(covariate_params) > 0) {
    covariate_params <- merge(
      covariate_params,
      odds_ratio_results,
      by = c("outcome", "predictor"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  if (nrow(covariate_params) > 0) {
    covariate_params$to_wave <- sub("(Y[0-9]+)#.*$", "\\1", covariate_params$outcome)
    covariate_params$to_state <- suppressWarnings(as.integer(sub("^.*#([0-9]+)$", "\\1", covariate_params$outcome)))
    covariate_params$to_state <- unname(state_from_mplus[as.character(covariate_params$to_state)])
    covariate_params$to_wave_idx <- match(toupper(covariate_params$to_wave), toupper(wave_order))
    covariate_params$from_wave <- ifelse(covariate_params$to_wave_idx > 1, wave_order[pmax(1, covariate_params$to_wave_idx - 1)], NA_character_)
    covariate_params$interval <- ifelse(!is.na(covariate_params$from_wave), paste0(toupper(covariate_params$from_wave), "_to_", toupper(covariate_params$to_wave)), toupper(covariate_params$to_wave))
    covariate_params$reference_state <- reference_state_original
    if (is.data.frame(covariate_spec_df) && nrow(covariate_spec_df) > 0) {
      keep_cols <- intersect(
        c("predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label"),
        names(covariate_spec_df)
      )
      if (length(keep_cols) > 0) {
        covariate_params <- merge(
          covariate_params,
          unique(covariate_spec_df[, keep_cols, drop = FALSE]),
          by = "predictor",
          all.x = TRUE,
          sort = FALSE
        )
      }
    }
    covariate_params <- covariate_params[, c("interval", "from_wave", "to_wave", "predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label", "to_state", "reference_state", "estimate", "se", "z", "p", "odds_ratio", "or_se", "or_lcl", "or_ucl"), drop = FALSE]
    covariate_params <- covariate_params[order(covariate_params$interval, covariate_params$predictor, covariate_params$to_state), , drop = FALSE]
  }
}

if (is.data.frame(global_model_results) && nrow(global_model_results) > 0 && length(active_covariates) > 0) {
  global_covariate_params <- global_model_results[global_model_results$predictor %in% active_covariates, , drop = FALSE]
  if (is.data.frame(global_odds_ratio_results) && nrow(global_odds_ratio_results) > 0 && nrow(global_covariate_params) > 0) {
    global_covariate_params <- merge(
      global_covariate_params,
      global_odds_ratio_results,
      by = c("outcome", "predictor"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  if (nrow(global_covariate_params) > 0) {
    global_covariate_params$to_wave <- sub("(Y[0-9]+)#.*$", "\\1", global_covariate_params$outcome)
    global_covariate_params$to_state <- suppressWarnings(as.integer(sub("^.*#([0-9]+)$", "\\1", global_covariate_params$outcome)))
    global_covariate_params$to_state <- unname(state_from_mplus[as.character(global_covariate_params$to_state)])
    global_covariate_params$reference_state <- reference_state_original
    if (is.data.frame(covariate_spec_df) && nrow(covariate_spec_df) > 0) {
      keep_cols <- intersect(
        c("predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label"),
        names(covariate_spec_df)
      )
      if (length(keep_cols) > 0) {
        global_covariate_params <- merge(
          global_covariate_params,
          unique(covariate_spec_df[, keep_cols, drop = FALSE]),
          by = "predictor",
          all.x = TRUE,
          sort = FALSE
        )
      }
    }
    global_covariate_params <- global_covariate_params[, c("predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label", "to_state", "reference_state", "estimate", "se", "z", "p", "odds_ratio", "or_se", "or_lcl", "or_ucl"), drop = FALSE]
    global_covariate_params <- unique(global_covariate_params)
    global_covariate_params$interval <- "Overall"
    global_covariate_params <- global_covariate_params[, c("interval", setdiff(names(global_covariate_params), "interval")), drop = FALSE]
    global_covariate_params <- global_covariate_params[order(global_covariate_params$predictor, global_covariate_params$to_state), , drop = FALSE]
  }
}

attach_covariate_metadata <- function(param_df, global = FALSE) {
  param_df <- safe_df(param_df)
  if (nrow(param_df) == 0) return(param_df)
  param_df$to_wave <- sub("(Y[0-9]+)#.*$", "\\1", param_df$outcome)
  param_df$to_state <- suppressWarnings(as.integer(sub("^.*#([0-9]+)$", "\\1", param_df$outcome)))
  param_df$to_state <- unname(state_from_mplus[as.character(param_df$to_state)])
  if (global) {
    param_df$interval <- "Overall"
  } else {
    param_df$to_wave_idx <- match(toupper(param_df$to_wave), toupper(wave_order))
    param_df$from_wave <- ifelse(param_df$to_wave_idx > 1, wave_order[pmax(1, param_df$to_wave_idx - 1)], NA_character_)
    param_df$interval <- ifelse(!is.na(param_df$from_wave), paste0(toupper(param_df$from_wave), "_to_", toupper(param_df$to_wave)), toupper(param_df$to_wave))
  }
  param_df$reference_state <- reference_state_original
  if (is.data.frame(covariate_spec_df) && nrow(covariate_spec_df) > 0) {
    keep_cols <- intersect(
      c("predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label"),
      names(covariate_spec_df)
    )
    if (length(keep_cols) > 0) {
      param_df <- merge(
        param_df,
        unique(covariate_spec_df[, keep_cols, drop = FALSE]),
        by = "predictor",
        all.x = TRUE,
        sort = FALSE
      )
    }
  }
  if (global) {
    param_df <- param_df[, c("interval", "predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label", "to_state", "reference_state", "estimate", "se", "z", "p", "odds_ratio", "or_se", "or_lcl", "or_ucl"), drop = FALSE]
    param_df <- unique(param_df)
    param_df <- param_df[order(param_df$predictor, param_df$to_state), , drop = FALSE]
  } else {
    param_df <- param_df[, c("interval", "from_wave", "to_wave", "predictor", "source_var", "source_label", "covariate_scope", "wave", "predictor_type", "level", "value_label", "reference_level", "reference_label", "to_state", "reference_state", "estimate", "se", "z", "p", "odds_ratio", "or_se", "or_lcl", "or_ucl"), drop = FALSE]
    param_df <- param_df[order(param_df$interval, param_df$predictor, param_df$to_state), , drop = FALSE]
  }
  param_df
}

collect_covariates_from_model <- function(out_file_i, covariates_i, global = FALSE) {
  covariates_i <- toupper(as.character(covariates_i))
  model_i <- parse_model_results_nominal(out_file_i)
  or_i <- parse_model_odds_ratios(out_file_i)
  if (!is.data.frame(model_i) || nrow(model_i) == 0 || length(covariates_i) == 0) return(data.frame())
  param_i <- model_i[model_i$predictor %in% covariates_i, , drop = FALSE]
  if (is.data.frame(or_i) && nrow(or_i) > 0 && nrow(param_i) > 0) {
    param_i <- merge(param_i, or_i, by = c("outcome", "predictor"), all.x = TRUE, sort = FALSE)
  }
  attach_covariate_metadata(param_i, global = global)
}

collect_covariate_registry_type <- function(model_type_i, global = FALSE) {
  idx <- which(as.character(ESTIMATION_REGISTRY$model_type %||% "") == model_type_i)
  if (length(idx) == 0) return(data.frame())
  rows <- lapply(idx, function(i) {
    out_i <- as.character(ESTIMATION_RUN_RESULTS$out_file[i] %||% ESTIMATION_REGISTRY$out_file[i])
    cov_key_i <- as.character(ESTIMATION_REGISTRY$covariate_key[i] %||% NA_character_)
    covs_i <- active_covariates
    if (!is.na(cov_key_i) && nzchar(cov_key_i) && is.data.frame(covariate_spec_df) && nrow(covariate_spec_df) > 0) {
      key_by_predictor <- stats::setNames(
        ifelse(
          as.character(covariate_spec_df$covariate_scope %||% "baseline") == "time_varying",
          paste(
            as.character(covariate_spec_df$covariate_scope %||% "time_varying"),
            as.character(covariate_spec_df$source_label %||% covariate_spec_df$source_var %||% covariate_spec_df$name),
            as.character(covariate_spec_df$predictor_type %||% ""),
            as.character(covariate_spec_df$level %||% ""),
            sep = "|"
          ),
          as.character(covariate_spec_df$name)
        ),
        toupper(as.character(covariate_spec_df$predictor))
      )
      covs_i <- names(key_by_predictor)[key_by_predictor == cov_key_i]
    }
    out <- collect_covariates_from_model(out_i, covs_i, global = global)
    if (nrow(out) > 0) {
      out$model_tag <- as.character(ESTIMATION_REGISTRY$model_tag[i])
      out$model_type <- model_type_i
      out$covariate_key <- cov_key_i
    }
    out
  })
  rows <- rows[vapply(rows, nrow, integer(1)) > 0]
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

univariable_covariate_params <- collect_covariate_registry_type("univariable_interval_covariates", global = FALSE)
univariable_global_covariate_params <- collect_covariate_registry_type("univariable_global_covariate_effects", global = TRUE)

add_covariate_key <- function(dat) {
  dat <- safe_df(dat)
  if (nrow(dat) == 0 || !"predictor" %in% names(dat)) return(dat)
  if ("covariate_key" %in% names(dat)) return(dat)
  dat$covariate_key <- as.character(dat$predictor)
  if (is.data.frame(covariate_spec_df) && nrow(covariate_spec_df) > 0 && "predictor" %in% names(covariate_spec_df)) {
    key_tbl <- covariate_spec_df
    key_tbl$covariate_key <- ifelse(
      as.character(key_tbl$covariate_scope %||% "baseline") == "time_varying",
      paste(
        as.character(key_tbl$covariate_scope %||% "time_varying"),
        as.character(key_tbl$source_label %||% key_tbl$source_var %||% key_tbl$name),
        as.character(key_tbl$predictor_type %||% ""),
        as.character(key_tbl$level %||% ""),
        sep = "|"
      ),
      as.character(key_tbl$name)
    )
    key_tbl <- unique(key_tbl[, c("predictor", "covariate_key"), drop = FALSE])
    dat <- merge(dat, key_tbl, by = "predictor", all.x = TRUE, sort = FALSE, suffixes = c("", ".from_spec"))
    if ("covariate_key.from_spec" %in% names(dat)) {
      hit <- as.character(dat$covariate_key.from_spec)
      fallback <- as.character(dat$covariate_key)
      dat$covariate_key <- ifelse(!is.na(hit) & nzchar(hit), hit, fallback)
      dat$covariate_key.from_spec <- NULL
    }
  }
  dat
}

covariate_params <- add_covariate_key(covariate_params)
global_covariate_params <- add_covariate_key(global_covariate_params)
univariable_covariate_params <- add_covariate_key(univariable_covariate_params)
univariable_global_covariate_params <- add_covariate_key(univariable_global_covariate_params)

selected_interval_keys <- unique(as.character(univariable_covariate_params$covariate_key[!is.na(univariable_covariate_params$p) & univariable_covariate_params$p < 0.05]))
selected_global_keys <- unique(as.character(univariable_global_covariate_params$covariate_key[!is.na(univariable_global_covariate_params$p) & univariable_global_covariate_params$p < 0.05]))
selected_covariate_params <- covariate_params[as.character(covariate_params$covariate_key) %in% selected_interval_keys, , drop = FALSE]
selected_global_covariate_params <- global_covariate_params[as.character(global_covariate_params$covariate_key) %in% selected_global_keys, , drop = FALSE]

bmi_mediation <- PREP_SUMMARY$bmi_mediation %||% list(enabled = FALSE)
mediator_specs <- bmi_mediation$mediator_specs %||% list()
if (isTRUE(bmi_mediation$enabled) && length(mediator_specs) > 0 && is.data.frame(MPLUS_DATA) && nrow(MPLUS_DATA) > 0) {
  mediator_df <- do.call(rbind, lapply(mediator_specs, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  mediator_names <- as.character(mediator_df$name)
  stage1_rows <- list()
  stage1_idx <- 1L
  for (med in mediator_names) {
    if (!med %in% names(MPLUS_DATA)) next
    interval_i <- paste0(
      toupper(as.character(mediator_df$from_wave[mediator_df$name == med][1])),
      "_to_",
      toupper(as.character(mediator_df$to_wave[mediator_df$name == med][1]))
    )
    med_type_i <- as.character(mediator_df$predictor_type[mediator_df$name == med][1] %||% "continuous")
    for (cv in PREP_SUMMARY$active_baseline_covariates %||% character(0)) {
      if (!cv %in% names(MPLUS_DATA)) next
      dat_i <- data.frame(y = suppressWarnings(as.numeric(MPLUS_DATA[[med]])), x = suppressWarnings(as.numeric(MPLUS_DATA[[cv]])))
      dat_i <- dat_i[stats::complete.cases(dat_i), , drop = FALSE]
      if (nrow(dat_i) < 3 || length(unique(dat_i$x)) < 2 || length(unique(dat_i$y)) < 2) next
      fit_i <- tryCatch(
        if (identical(med_type_i, "categorical")) summary(stats::glm(y ~ x, data = dat_i, family = stats::binomial()))$coefficients else summary(stats::lm(y ~ x, data = dat_i))$coefficients,
        error = function(e) NULL
      )
      if (is.null(fit_i) || !"x" %in% rownames(fit_i)) next
      spec_i <- covariate_spec_df[covariate_spec_df$predictor_original == cv | covariate_spec_df$name == cv, , drop = FALSE]
      est_i <- unname(fit_i["x", "Estimate"])
      se_i <- unname(fit_i["x", "Std. Error"])
      stat_col_i <- if (identical(med_type_i, "categorical")) "z value" else "t value"
      p_col_i <- if (identical(med_type_i, "categorical")) "Pr(>|z|)" else "Pr(>|t|)"
      if (!stat_col_i %in% colnames(fit_i) || !p_col_i %in% colnames(fit_i)) next
      stage1_rows[[stage1_idx]] <- data.frame(
        interval = interval_i,
        mediator = med,
        mediator_type = med_type_i,
        mediator_label = as.character(mediator_df$value_label[mediator_df$name == med][1] %||% "BMI change"),
        mediator_reference_label = as.character(mediator_df$reference_label[mediator_df$name == med][1] %||% NA_character_),
        predictor = toupper(mplus_name_lookup[cv] %||% cv),
        source_var = as.character(spec_i$source_var[1] %||% cv),
        source_label = as.character(spec_i$source_label[1] %||% cv),
        predictor_type = as.character(spec_i$predictor_type[1] %||% "continuous"),
        level = as.character(spec_i$level[1] %||% NA_character_),
        value_label = as.character(spec_i$value_label[1] %||% NA_character_),
        reference_level = as.character(spec_i$reference_level[1] %||% NA_character_),
        reference_label = as.character(spec_i$reference_label[1] %||% NA_character_),
        estimate = est_i,
        se = se_i,
        z = unname(fit_i["x", stat_col_i]),
        p = unname(fit_i["x", p_col_i]),
        odds_ratio = if (identical(med_type_i, "categorical")) exp(est_i) else NA_real_,
        or_lcl = if (identical(med_type_i, "categorical")) exp(est_i - 1.96 * se_i) else NA_real_,
        or_ucl = if (identical(med_type_i, "categorical")) exp(est_i + 1.96 * se_i) else NA_real_,
        n = nrow(dat_i),
        stringsAsFactors = FALSE
      )
      stage1_idx <- stage1_idx + 1L
    }
  }
  if (length(stage1_rows) > 0) {
    bmi_mediation_stage1 <- do.call(rbind, stage1_rows)
  }
}

mediation_idx <- which(as.character(ESTIMATION_REGISTRY$model_type %||% "") == "bmi_mediation_transition")
if (length(mediation_idx) > 0) {
  mediation_out_file <- as.character(ESTIMATION_RUN_RESULTS$out_file[mediation_idx[1]] %||% ESTIMATION_REGISTRY$out_file[mediation_idx[1]])
  mediator_original <- PREP_SUMMARY$active_mediator_covariates %||% character(0)
  mediation_covariates <- mplus_name_lookup[unique(c(PREP_SUMMARY$active_baseline_covariates %||% character(0), mediator_original))]
  mediation_covariates[is.na(mediation_covariates)] <- unique(c(PREP_SUMMARY$active_baseline_covariates %||% character(0), mediator_original))[is.na(mediation_covariates)]
  bmi_mediation_stage2 <- collect_covariates_from_model(mediation_out_file, mediation_covariates, global = FALSE)
  bmi_mediation_stage2 <- add_covariate_key(bmi_mediation_stage2)
}

if (isTRUE(bmi_mediation$enabled) && length(mediator_specs) > 0 && is.data.frame(MPLUS_DATA) && nrow(MPLUS_DATA) > 0) {
  mediator_df <- do.call(rbind, lapply(mediator_specs, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  stage3_rows <- list()
  stage3_idx <- 1L
  mediator_groups <- split(mediator_df, paste(as.character(mediator_df$predictor_type), as.character(mediator_df$level), sep = "\r"))
  for (mg in mediator_groups) {
    med_type_i <- as.character(mg$predictor_type[1] %||% "continuous")
    for (cv in PREP_SUMMARY$active_baseline_covariates %||% character(0)) {
      if (!cv %in% names(MPLUS_DATA)) next
      dat_list <- lapply(seq_len(nrow(mg)), function(i) {
        med_i <- as.character(mg$name[i])
        if (!med_i %in% names(MPLUS_DATA)) return(NULL)
        data.frame(
          y = suppressWarnings(as.numeric(MPLUS_DATA[[med_i]])),
          x = suppressWarnings(as.numeric(MPLUS_DATA[[cv]])),
          interval = paste0(toupper(as.character(mg$from_wave[i])), "_to_", toupper(as.character(mg$to_wave[i]))),
          stringsAsFactors = FALSE
        )
      })
      dat_list <- dat_list[!vapply(dat_list, is.null, logical(1))]
      if (length(dat_list) == 0) next
      dat_i <- do.call(rbind, dat_list)
      dat_i <- dat_i[stats::complete.cases(dat_i), , drop = FALSE]
      if (nrow(dat_i) < 3 || length(unique(dat_i$x)) < 2 || length(unique(dat_i$y)) < 2) next
      fit_i <- tryCatch(
        if (identical(med_type_i, "categorical")) {
          if (length(unique(dat_i$interval)) > 1) summary(stats::glm(y ~ x + factor(interval), data = dat_i, family = stats::binomial()))$coefficients else summary(stats::glm(y ~ x, data = dat_i, family = stats::binomial()))$coefficients
        } else {
          if (length(unique(dat_i$interval)) > 1) summary(stats::lm(y ~ x + factor(interval), data = dat_i))$coefficients else summary(stats::lm(y ~ x, data = dat_i))$coefficients
        },
        error = function(e) NULL
      )
      if (is.null(fit_i) || !"x" %in% rownames(fit_i)) next
      spec_i <- covariate_spec_df[covariate_spec_df$predictor_original == cv | covariate_spec_df$name == cv, , drop = FALSE]
      est_i <- unname(fit_i["x", "Estimate"])
      se_i <- unname(fit_i["x", "Std. Error"])
      stage3_rows[[stage3_idx]] <- data.frame(
        interval = "Overall W1-W4",
        mediator = as.character(mg$name[1]),
        mediator_type = med_type_i,
        mediator_label = as.character(mg$value_label[1] %||% "BMI change"),
        mediator_reference_label = as.character(mg$reference_label[1] %||% NA_character_),
        predictor = toupper(mplus_name_lookup[cv] %||% cv),
        source_var = as.character(spec_i$source_var[1] %||% cv),
        source_label = as.character(spec_i$source_label[1] %||% cv),
        predictor_type = as.character(spec_i$predictor_type[1] %||% "continuous"),
        level = as.character(spec_i$level[1] %||% NA_character_),
        value_label = as.character(spec_i$value_label[1] %||% NA_character_),
        reference_level = as.character(spec_i$reference_level[1] %||% NA_character_),
        reference_label = as.character(spec_i$reference_label[1] %||% NA_character_),
        estimate = est_i,
        se = se_i,
        z = unname(fit_i["x", if (identical(med_type_i, "categorical")) "z value" else "t value"]),
        p = unname(fit_i["x", if (identical(med_type_i, "categorical")) "Pr(>|z|)" else "Pr(>|t|)"]),
        odds_ratio = if (identical(med_type_i, "categorical")) exp(est_i) else NA_real_,
        or_lcl = if (identical(med_type_i, "categorical")) exp(est_i - 1.96 * se_i) else NA_real_,
        or_ucl = if (identical(med_type_i, "categorical")) exp(est_i + 1.96 * se_i) else NA_real_,
        n = nrow(dat_i),
        stringsAsFactors = FALSE
      )
      stage3_idx <- stage3_idx + 1L
    }
  }
  if (length(stage3_rows) > 0) {
    bmi_mediation_stage3 <- do.call(rbind, stage3_rows)
  }
}

wave_prevalence <- data.frame()
base_wave <- wave_order[1]
if (nrow(baseline_params) > 0) {
  current_prev <- baseline_params[order(match(baseline_params$state, state_values)), c("state", "prob"), drop = FALSE]
  current_prev$wave <- toupper(base_wave)
  wave_prevalence <- current_prev[, c("wave", "state", "prob"), drop = FALSE]
} else if (is.data.frame(PANEL_LONG) && nrow(PANEL_LONG) > 0) {
  base_df <- PANEL_LONG[toupper(as.character(PANEL_LONG$panel_wave)) == toupper(base_wave), , drop = FALSE]
  if (nrow(base_df) > 0) {
    tab <- table(factor(base_df$state, levels = state_values))
    probs <- as.numeric(tab) / sum(tab)
    wave_prevalence <- data.frame(
      wave = toupper(base_wave),
      state = state_values,
      prob = probs,
      stringsAsFactors = FALSE
    )
  }
}

if (nrow(wave_prevalence) > 0 && nrow(transition_params) > 0 && length(wave_order) >= 2) {
  current_prev <- wave_prevalence[wave_prevalence$wave == toupper(base_wave), c("state", "prob"), drop = FALSE]
  prev_vec <- stats::setNames(current_prev$prob, current_prev$state)
  for (i in seq_len(length(wave_order) - 1)) {
    from_wave <- wave_order[i]
    to_wave <- wave_order[i + 1]
    trans_i <- transition_params[
      toupper(transition_params$from_wave) == toupper(from_wave) &
        toupper(transition_params$to_wave) == toupper(to_wave),
      ,
      drop = FALSE
    ]
    if (nrow(trans_i) == 0) next
    next_probs <- vapply(
      state_values,
      function(ts) {
        sum(vapply(
          state_values,
          function(fs) {
            p_fs <- prev_vec[as.character(fs)] %||% 0
            p_tr <- trans_i$prob[trans_i$from_state == fs & trans_i$to_state == ts]
            if (length(p_tr) == 0 || is.na(p_fs)) return(0)
            p_fs * p_tr[1]
          },
          numeric(1)
        ))
      },
      numeric(1)
    )
    prev_vec <- stats::setNames(next_probs, as.character(state_values))
    wave_prevalence <- rbind(
      wave_prevalence,
      data.frame(wave = toupper(to_wave), state = state_values, prob = as.numeric(next_probs), stringsAsFactors = FALSE)
    )
  }
}

ESTIMATION_COLLECT_SUMMARY <- list(
  n_models = nrow(ESTIMATION_REGISTRY),
  parse_ok = fit_tbl$parse_ok[1] %||% FALSE,
  n_transition_cells = nrow(transition_params),
  n_prevalence_rows = nrow(wave_prevalence)
)

save_named_rds_list(
  list(
    FIT_SUMMARY = fit_tbl,
    MPLUS_NEW_PARAMS = new_params,
    ESTIMATION_TRANSITIONS = transition_params,
    ESTIMATION_PREVALENCE = wave_prevalence,
    ESTIMATION_COVARIATES = covariate_params,
    ESTIMATION_COVARIATES_GLOBAL = global_covariate_params,
    ESTIMATION_COVARIATES_UNIVARIABLE = univariable_covariate_params,
    ESTIMATION_COVARIATES_GLOBAL_UNIVARIABLE = univariable_global_covariate_params,
    ESTIMATION_COVARIATES_SELECTED = selected_covariate_params,
    ESTIMATION_COVARIATES_GLOBAL_SELECTED = selected_global_covariate_params,
    BMI_MEDIATION_STAGE1 = bmi_mediation_stage1,
    BMI_MEDIATION_STAGE2 = bmi_mediation_stage2,
    BMI_MEDIATION_STAGE3 = bmi_mediation_stage3,
    ESTIMATION_COLLECT_SUMMARY = ESTIMATION_COLLECT_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("estimation_collect", round(as.numeric(difftime(Sys.time(), T0_COLLECT, units = "secs")), 2), ok = TRUE)
