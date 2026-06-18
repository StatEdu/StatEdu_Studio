T0_MODEL <- Sys.time()

log_step_start("MODEL_RUN", "03_model_run.R")
log_info("Reloading process prep outputs ...")

PROCESS_SETTINGS <- load_step_rds("PROCESS_SETTINGS", dir_rds = DIR_RDS, required = TRUE)
PROCESS_DATA <- load_step_rds("PROCESS_DATA", dir_rds = DIR_RDS, required = TRUE)
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, required = TRUE)
CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
VAR_SPECS <- load_step_rds("VAR_SPECS", dir_rds = DIR_RDS, default = data.frame())
SOURCE_REFERENCE_CLASS <- load_step_rds("SOURCE_REFERENCE_CLASS", dir_rds = DIR_RDS, default = NA_integer_)
SOURCE_REFERENCE_CLASS_LABEL <- load_step_rds("SOURCE_REFERENCE_CLASS_LABEL", dir_rds = DIR_RDS, default = NA_character_)
SURVEY_BUNDLE <- load_step_rds("SURVEY_BUNDLE", dir_rds = DIR_RDS, default = list())

`%||%` <- function(x, y) if (is.null(x)) y else x
first_nonempty_pm <- function(...) {
  xs <- list(...)
  for (x in xs) {
    if (length(x) == 0) next
    val <- as.character(x[1])
    if (!is.na(val) && nzchar(val)) return(val)
  }
  NA_character_
}

if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) {
  custom_model_observed <- isTRUE(PROCESS_SETTINGS$custom_model_enabled)
  observed_model1 <- isTRUE(PROCESS_SETTINGS$process_model == 1L) &&
    !identical(PROCESS_SETTINGS$moderator_source %||% "latent_class", "latent_class")
  observed_model5 <- isTRUE(PROCESS_SETTINGS$process_model == 5L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model7 <- isTRUE(PROCESS_SETTINGS$process_model == 7L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model8 <- isTRUE(PROCESS_SETTINGS$process_model == 8L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model14 <- isTRUE(PROCESS_SETTINGS$process_model == 14L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model15 <- isTRUE(PROCESS_SETTINGS$process_model == 15L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model58 <- isTRUE(PROCESS_SETTINGS$process_model == 58L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model59 <- isTRUE(PROCESS_SETTINGS$process_model == 59L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  observed_model4 <- isTRUE(PROCESS_SETTINGS$process_model == 4L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L
  observed_model6 <- isTRUE(PROCESS_SETTINGS$process_model == 6L) &&
    length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) >= 2L
  if (!custom_model_observed && !observed_model1 && !observed_model5 && !observed_model7 && !observed_model8 && !observed_model14 && !observed_model15 && !observed_model58 && !observed_model59 && !observed_model4 && !observed_model6) {
    stop(
      "process_macro detected survey design settings and selected variance_method = '",
      PROCESS_SETTINGS$variance_method %||% "survey_design",
      "'. Ordinary bootstrap has been disabled for weighted/complex survey data. ",
      "Design-based fitting is currently implemented only for observed-variable custom models, Model 1 moderation, Model 4 mediation, Model 5 moderated mediation, Model 6 serial mediation, Model 7 first-stage moderated mediation, Model 8 moderated mediation, Model 14 second-stage moderated mediation, Model 15 moderated mediation, Model 58 moderated mediation, and Model 59 moderated mediation via Mplus.",
      call. = FALSE
    )
  }
}

safe_num_pm <- function(x) suppressWarnings(as.numeric(as.character(x)))

fmt_p_pm <- function(x, digits = 3) {
  x <- safe_num_pm(x)
  out <- rep("", length(x))
  ok <- !is.na(x)
  out[ok] <- ifelse(x[ok] < .001, "<.001", sub("^0", "", formatC(x[ok], format = "f", digits = digits)))
  out
}

sig_mark_pm <- function(p) {
  p <- safe_num_pm(p)
  out <- rep("", length(p))
  out[!is.na(p) & p < .001] <- "***"
  out[!is.na(p) & p >= .001 & p < .01] <- "**"
  out[!is.na(p) & p >= .01 & p < .05] <- "*"
  out
}

lookup_label <- function(var_name) {
  hit_meta <- lookup_dict_meta(var_name)
  if (nrow(hit_meta) > 0) {
    out <- first_nonempty_pm(hit_meta$label_en[1], hit_meta$var_label[1], hit_meta$label_ko[1], var_name)
    if (!is.na(out) && nzchar(out)) return(out)
  }
  if (!is.data.frame(VAR_SPECS) || nrow(VAR_SPECS) == 0) return(var_name)
  hit <- VAR_SPECS[VAR_SPECS$var_name == var_name, , drop = FALSE]
  if (nrow(hit) == 0) return(var_name)
  as.character(hit$var_label[1] %||% var_name)
}

dict_meta_pm <- DICT$meta %||% DICT$META %||% data.frame()
dict_levels_pm <- DICT$levels %||% DICT$LEVELS %||% data.frame()

lookup_dict_meta <- function(var_name) {
  if (!is.data.frame(dict_meta_pm) || nrow(dict_meta_pm) == 0 || !("var_name" %in% names(dict_meta_pm))) {
    return(data.frame())
  }
  hit <- dict_meta_pm[as.character(dict_meta_pm$var_name) == as.character(var_name), , drop = FALSE]
  rownames(hit) <- NULL
  hit
}

lookup_display_order <- function(var_name) {
  hit <- lookup_dict_meta(var_name)
  if (nrow(hit) == 0 || !"display_order" %in% names(hit)) return(Inf)
  out <- suppressWarnings(as.numeric(hit$display_order[1]))
  if (is.na(out)) Inf else out
}

lookup_subtype <- function(var_name) {
  hit <- lookup_dict_meta(var_name)
  if (nrow(hit) > 0) {
    type_i <- tolower(as.character(hit$type[1] %||% ""))
    subtype_i <- tolower(as.character(hit$subtype[1] %||% ""))
    if (type_i %in% c("continuous", "numeric", "scale")) return("continuous")
    if (type_i %in% c("categorical", "factor", "binary", "ordered", "ordinal")) return("categorical")
    if (subtype_i %in% c("continuous", "categorical")) return(subtype_i)
  }
  hit_vs <- VAR_SPECS[as.character(VAR_SPECS$var_name) == as.character(var_name), , drop = FALSE]
  if (nrow(hit_vs) > 0) {
    if (isTRUE(hit_vs$is_continuous[1])) return("continuous")
  }
  "categorical"
}

lookup_reference_value <- function(var_name) {
  hit <- lookup_dict_meta(var_name)
  if (nrow(hit) > 0 && "reference" %in% names(hit)) {
    ref <- as.character(hit$reference[1] %||% "")
    if (nzchar(ref)) return(ref)
  }
  if (is.data.frame(dict_levels_pm) && nrow(dict_levels_pm) > 0) {
    lv <- dict_levels_pm[as.character(dict_levels_pm$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(lv) > 0 && "reference" %in% names(lv)) {
      ref <- as.character(lv$reference[!is.na(lv$reference) & nzchar(as.character(lv$reference))][1] %||% "")
      if (nzchar(ref)) return(ref)
    }
  }
  NA_character_
}

lookup_value_label_pm <- function(var_name, value) {
  if (!is.data.frame(dict_levels_pm) || nrow(dict_levels_pm) == 0) return(as.character(value))
  hit <- dict_levels_pm[
    as.character(dict_levels_pm$var_name) == as.character(var_name) &
      as.character(dict_levels_pm$value) == as.character(value),
    ,
    drop = FALSE
  ]
  if (nrow(hit) == 0 || !"value_label" %in% names(hit)) return(as.character(value))
  out <- as.character(hit$value_label[1] %||% value)
  if (!nzchar(out)) as.character(value) else out
}

lookup_level_values <- function(var_name, observed_values = character(0)) {
  out <- character(0)
  if (is.data.frame(dict_levels_pm) && nrow(dict_levels_pm) > 0) {
    hit <- dict_levels_pm[as.character(dict_levels_pm$var_name) == as.character(var_name), , drop = FALSE]
    if (nrow(hit) > 0 && "value" %in% names(hit)) out <- as.character(hit$value)
  }
  out <- unique(c(out[!is.na(out) & nzchar(out)], as.character(observed_values[!is.na(observed_values) & nzchar(observed_values)])))
  out
}

prepare_covariates <- function(dat, covariates) {
  dat_out <- dat
  term_map <- data.frame(
    term = character(0),
    effect_type = character(0),
    effect = character(0),
    term_label = character(0),
    variable_label = character(0),
    category_label = character(0),
    reference_label = character(0),
    sort_key = numeric(0),
    covariate = character(0),
    covariate_type = character(0),
    stringsAsFactors = FALSE
  )
  rhs_terms <- character(0)

  for (v in covariates) {
    if (!v %in% names(dat_out)) next
    subtype_i <- lookup_subtype(v)
    display_order_i <- lookup_display_order(v)
    if (!is.finite(display_order_i)) display_order_i <- 999999
    var_label_i <- lookup_label(v)

    if (identical(subtype_i, "continuous")) {
      dat_out[[v]] <- safe_num_pm(dat_out[[v]])
      if (length(unique(stats::na.omit(dat_out[[v]]))) <= 1L) next
      rhs_terms <- c(rhs_terms, v)
      term_map <- rbind(
        term_map,
        data.frame(
          term = v,
          effect_type = "Continuous covariate",
          effect = "Continuous covariate",
          term_label = var_label_i,
          variable_label = var_label_i,
          category_label = "",
          reference_label = "",
          sort_key = 400000 + display_order_i * 100,
          covariate = v,
          covariate_type = "continuous",
          stringsAsFactors = FALSE
        )
      )
      next
    }

    raw_chr <- as.character(dat_out[[v]])
    raw_chr[is.na(dat_out[[v]])] <- NA_character_
    level_values <- lookup_level_values(v, observed_values = unique(stats::na.omit(raw_chr)))
    if (length(level_values) == 0L) next

    ref_value <- lookup_reference_value(v)
    if (is.na(ref_value) || !nzchar(ref_value) || !ref_value %in% level_values) {
      ref_value <- level_values[1]
    }
    level_values <- c(ref_value, setdiff(level_values, ref_value))
    compare_values <- setdiff(level_values, ref_value)

    kept_any <- FALSE
    for (i in seq_along(compare_values)) {
      lv <- compare_values[i]
      term_name <- paste0("cov__", v, "__", i)
      dat_out[[term_name]] <- ifelse(is.na(raw_chr), NA_real_, as.numeric(raw_chr == lv))
      if (length(unique(stats::na.omit(dat_out[[term_name]]))) <= 1L) {
        dat_out[[term_name]] <- NULL
        next
      }
      rhs_terms <- c(rhs_terms, term_name)
      kept_any <- TRUE
      term_map <- rbind(
        term_map,
        data.frame(
          term = term_name,
          effect_type = "Categorical covariate",
          effect = "Categorical covariate",
          term_label = paste0(var_label_i, ": ", lookup_value_label_pm(v, lv), " vs ", lookup_value_label_pm(v, ref_value)),
          variable_label = var_label_i,
          category_label = lookup_value_label_pm(v, lv),
          reference_label = lookup_value_label_pm(v, ref_value),
          sort_key = 400000 + display_order_i * 100 + i,
          covariate = v,
          covariate_type = "categorical",
          stringsAsFactors = FALSE
        )
      )
    }
    if (!kept_any) next
  }

  list(data = dat_out, rhs_terms = rhs_terms, term_map = term_map)
}

clean_term_label <- function(term, x_var, ref_class, cov_term_map = NULL) {
  term_chr <- as.character(term)[1]
  x_label <- lookup_label(x_var)

  if (identical(term_chr, "(Intercept)")) {
    return(list(
      effect_type = "Intercept",
      effect = "Intercept",
      term_label = "Intercept",
      variable_label = "Intercept",
      category_label = "",
      reference_label = "",
      sort_key = 10
    ))
  }

  class_hit <- regmatches(term_chr, regexec("^class_fac([0-9]+)$", term_chr))[[1]]
  if (length(class_hit) > 1) {
    cls <- suppressWarnings(as.integer(class_hit[2]))
    lbl <- paste0("Class ", cls, " vs Class ", ref_class)
    return(list(
      effect_type = "Moderator",
      effect = "Moderator main effect",
      term_label = lbl,
      variable_label = lbl,
      category_label = "",
      reference_label = "",
      sort_key = 120 + cls
    ))
  }

  if (identical(term_chr, x_var)) {
    return(list(
      effect_type = "Independent variable",
      effect = "Independent variable main effect",
      term_label = x_label,
      variable_label = x_label,
      category_label = "",
      reference_label = "",
      sort_key = 20
    ))
  }

  int_pat_1 <- regmatches(term_chr, regexec(paste0("^class_fac([0-9]+):", x_var, "$"), term_chr))[[1]]
  int_pat_2 <- regmatches(term_chr, regexec(paste0("^", x_var, ":class_fac([0-9]+)$"), term_chr))[[1]]
  int_hit <- if (length(int_pat_1) > 1) int_pat_1 else int_pat_2
  if (length(int_hit) > 1) {
    cls <- suppressWarnings(as.integer(int_hit[2]))
    lbl <- paste0("Class ", cls, " vs Class ", ref_class, " x ", x_label)
    return(list(
      effect_type = "Interaction",
      effect = "Interaction",
      term_label = lbl,
      variable_label = lbl,
      category_label = "",
      reference_label = "",
      sort_key = 220 + cls
    ))
  }

  if (is.data.frame(cov_term_map) && nrow(cov_term_map) > 0) {
    hit <- cov_term_map[as.character(cov_term_map$term) == term_chr, , drop = FALSE]
    if (nrow(hit) > 0) {
      return(list(
        effect_type = as.character(hit$effect_type[1]),
        effect = as.character(hit$effect[1]),
        term_label = as.character(hit$term_label[1]),
        variable_label = as.character(hit$variable_label[1]),
        category_label = as.character(hit$category_label[1]),
        reference_label = as.character(hit$reference_label[1]),
        sort_key = as.numeric(hit$sort_key[1])
      ))
    }
  }

  list(
    effect_type = "Covariate",
    effect = "Covariate",
    term_label = lookup_label(term_chr),
    variable_label = lookup_label(term_chr),
    category_label = "",
    reference_label = "",
    sort_key = 900000
  )
}

make_class_factor <- function(dat, ref_class) {
  dat$class_fac <- factor(dat$class_num, levels = sort(unique(dat$class_num)))
  if (!is.na(ref_class) && as.character(ref_class) %in% levels(dat$class_fac)) {
    dat$class_fac <- stats::relevel(dat$class_fac, ref = as.character(ref_class))
  }
  dat
}

make_subset_index_pm <- function(data, survey_bundle = list()) {
  if (!is.data.frame(data) || nrow(data) == 0) return(logical(0))
  subset_expr <- as.character(survey_bundle$subset_expr %||% "")[1]
  subset_var <- as.character(survey_bundle$subset_var %||% "")[1]
  subset_value <- survey_bundle$subset_value %||% NULL

  idx <- rep(TRUE, nrow(data))
  if (nzchar(subset_expr)) {
    idx <- if (exists("safe_eval_in_data")) {
      safe_eval_in_data(subset_expr, data)
    } else {
      tryCatch(
        eval(parse(text = subset_expr), envir = data, enclos = parent.frame()),
        error = function(e) rep(TRUE, nrow(data))
      )
    }
  } else if (nzchar(subset_var) && subset_var %in% names(data)) {
    vv <- data[[subset_var]]
    if (is.null(subset_value) || length(subset_value) == 0) {
      idx <- !is.na(vv)
    } else {
      sv_chr <- as.character(subset_value)
      if (is.numeric(vv) || is.integer(vv)) {
        idx <- vv %in% suppressWarnings(as.numeric(sv_chr))
      } else {
        idx <- as.character(vv) %in% sv_chr
      }
    }
  }
  if (!is.logical(idx) || length(idx) != nrow(data)) idx <- rep(TRUE, nrow(data))
  idx[is.na(idx)] <- FALSE
  idx
}

weighted_mean_pm <- function(x, w = NULL) {
  x <- safe_num_pm(x)
  if (is.null(w) || length(w) == 0) return(mean(x, na.rm = TRUE))
  w <- safe_num_pm(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(mean(x, na.rm = TRUE))
  stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

weighted_sd_pm <- function(x, w = NULL) {
  x <- safe_num_pm(x)
  if (is.null(w) || length(w) == 0) return(stats::sd(x, na.rm = TRUE))
  w <- safe_num_pm(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) <= 1L) return(stats::sd(x, na.rm = TRUE))
  mu <- stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
  sqrt(sum(w[ok] * (x[ok] - mu)^2, na.rm = TRUE) / sum(w[ok], na.rm = TRUE))
}

sanitize_token_pm <- function(x) {
  x <- tolower(as.character(x)[1])
  x <- gsub("[^a-z0-9]+", "", x)
  if (!nzchar(x)) "v" else x
}

wrap_mplus_statement_pm <- function(keyword, values, indent = "  ", width = 78) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) return(character(0))
  first_prefix <- paste0(indent, keyword, " = ")
  cont_prefix <- paste0(indent, "  ")
  out <- character(0)
  current <- first_prefix
  for (val in values) {
    candidate <- if (identical(current, first_prefix)) paste0(current, val) else paste(current, val)
    if (nchar(candidate, type = "width") > width) {
      out <- c(out, current)
      current <- paste0(cont_prefix, val)
    } else {
      current <- candidate
    }
  }
  c(out, paste0(current, ";"))
}

parse_last_number_pm <- function(x) {
  hit <- regmatches(x, gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?", x, perl = TRUE))[[1]]
  if (length(hit) == 0) return(NA_real_)
  suppressWarnings(as.numeric(gsub("D", "E", hit[length(hit)], fixed = TRUE)))
}

parse_mplus_numeric_row_pm <- function(line) {
  line <- trimws(as.character(line)[1])
  if (!nzchar(line)) return(NULL)
  m <- regexec(
    "^([A-Za-z][A-Za-z0-9_\\$#]*)\\s+([-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?)\\s+([-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?)\\s+([-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?)\\s+([-+]?[0-9]*\\.?[0-9]+(?:[EeDd][-+]?[0-9]+)?)$",
    line,
    perl = TRUE
  )
  hit <- regmatches(line, m)[[1]]
  if (length(hit) != 6) return(NULL)
  data.frame(
    name = toupper(hit[2]),
    estimate = suppressWarnings(as.numeric(gsub("D", "E", hit[3], fixed = TRUE))),
    se = suppressWarnings(as.numeric(gsub("D", "E", hit[4], fixed = TRUE))),
    z_value = suppressWarnings(as.numeric(gsub("D", "E", hit[5], fixed = TRUE))),
    p = suppressWarnings(as.numeric(gsub("D", "E", hit[6], fixed = TRUE))),
    stringsAsFactors = FALSE
  )
}

parse_mplus_main_sections_pm <- function(lines, outcome_name) {
  start <- grep("^\\s*MODEL RESULTS\\s*$", lines, ignore.case = TRUE, perl = TRUE)[1]
  if (is.na(start)) return(data.frame())
  end_hits <- grep(
    "^\\s*(R-SQUARE|QUALITY OF NUMERICAL RESULTS|CONFIDENCE INTERVALS OF MODEL RESULTS|TECHNICAL|PLOT INFORMATION|DIAGRAM INFORMATION|Beginning Time:|MODEL COMMAND WITH FINAL ESTIMATES)\\s*$",
    lines,
    ignore.case = TRUE,
    perl = TRUE
  )
  end <- end_hits[end_hits > start][1]
  if (is.na(end)) end <- length(lines) + 1L
  block <- lines[seq.int(start + 1L, end - 1L)]
  if (length(block) == 0) return(data.frame())

  current_section <- NA_character_
  out <- list()
  idx <- 1L
  outcome_name_up <- toupper(outcome_name)

  for (ln in block) {
    trim_ln <- trimws(ln)
    if (!nzchar(trim_ln)) next
    if (grepl(paste0("^", outcome_name_up, "\\s+ON$"), toupper(trim_ln), perl = TRUE)) {
      current_section <- "on"
      next
    }
    if (grepl("^INTERCEPTS$", toupper(trim_ln), perl = TRUE)) {
      current_section <- "intercepts"
      next
    }
    if (grepl("^RESIDUAL VARIANCES$", toupper(trim_ln), perl = TRUE)) {
      current_section <- "residual_variances"
      next
    }
    if (grepl("^NEW/ADDITIONAL PARAMETERS$", toupper(trim_ln), perl = TRUE)) {
      current_section <- "new_parameters"
      next
    }
    row_i <- parse_mplus_numeric_row_pm(trim_ln)
    if (is.null(row_i)) next
    row_i$section <- current_section
    out[[idx]] <- row_i
    idx <- idx + 1L
  }

  if (length(out) == 0) return(data.frame())
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

parse_mplus_mediation_sections_pm <- function(lines) {
  start <- grep("^\\s*MODEL RESULTS\\s*$", lines, ignore.case = TRUE, perl = TRUE)[1]
  if (is.na(start)) return(data.frame())
  end_hits <- grep(
    "^\\s*(R-SQUARE|QUALITY OF NUMERICAL RESULTS|CONFIDENCE INTERVALS OF MODEL RESULTS|TECHNICAL|PLOT INFORMATION|DIAGRAM INFORMATION|Beginning Time:|MODEL COMMAND WITH FINAL ESTIMATES)\\s*$",
    lines,
    ignore.case = TRUE,
    perl = TRUE
  )
  end <- end_hits[end_hits > start][1]
  if (is.na(end)) end <- length(lines) + 1L
  block <- lines[seq.int(start + 1L, end - 1L)]
  if (length(block) == 0) return(data.frame())

  current_section <- NA_character_
  current_lhs <- NA_character_
  out <- list()
  idx <- 1L

  for (ln in block) {
    trim_ln <- trimws(ln)
    if (!nzchar(trim_ln)) next
    on_hit <- regmatches(toupper(trim_ln), regexec("^([A-Z][A-Z0-9_]*)\\s+ON$", toupper(trim_ln), perl = TRUE))[[1]]
    if (length(on_hit) == 2) {
      current_lhs <- on_hit[2]
      if (identical(current_lhs, "M_PM")) {
        current_section <- "m_on"
      } else if (identical(current_lhs, "Y_PM")) {
        current_section <- "y_on"
      } else {
        current_section <- paste0(tolower(current_lhs), "_on")
      }
      next
    }
    if (grepl("^INTERCEPTS$", toupper(trim_ln), perl = TRUE)) {
      current_section <- "intercepts"
      current_lhs <- NA_character_
      next
    }
    if (grepl("^RESIDUAL VARIANCES$", toupper(trim_ln), perl = TRUE)) {
      current_section <- "residual_variances"
      current_lhs <- NA_character_
      next
    }
    if (grepl("^NEW/ADDITIONAL PARAMETERS$", toupper(trim_ln), perl = TRUE)) {
      current_section <- "new_parameters"
      current_lhs <- NA_character_
      next
    }
    row_i <- parse_mplus_numeric_row_pm(trim_ln)
    if (is.null(row_i)) next
    row_i$section <- current_section
    row_i$lhs <- current_lhs
    out[[idx]] <- row_i
    idx <- idx + 1L
  }

  if (length(out) == 0) return(data.frame())
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

parse_mplus_rsquare_pm <- function(lines) {
  start <- grep("^\\s*R-SQUARE\\s*$", lines, ignore.case = TRUE, perl = TRUE)[1]
  if (is.na(start)) return(data.frame())
  end_hits <- grep(
    "^\\s*(QUALITY OF NUMERICAL RESULTS|CONFIDENCE INTERVALS OF MODEL RESULTS|TECHNICAL|PLOT INFORMATION|DIAGRAM INFORMATION|Beginning Time:)\\s*$",
    lines,
    ignore.case = TRUE,
    perl = TRUE
  )
  end <- end_hits[end_hits > start][1]
  if (is.na(end)) end <- length(lines) + 1L
  block <- lines[seq.int(start + 1L, end - 1L)]
  out <- list()
  idx <- 1L
  for (ln in block) {
    row_i <- parse_mplus_numeric_row_pm(ln)
    if (is.null(row_i)) next
    out[[idx]] <- row_i
    idx <- idx + 1L
  }
  if (length(out) == 0) return(data.frame())
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

parse_mplus_wald_test_pm <- function(lines) {
  start <- grep("^\\s*WALD TEST OF PARAMETER CONSTRAINTS\\s*$", lines, ignore.case = TRUE, perl = TRUE)[1]
  if (is.na(start)) return(data.frame())

  block <- lines[seq.int(start + 1L, min(length(lines), start + 20L))]
  value_line <- grep("^\\s*Value\\s+", block, value = TRUE, ignore.case = TRUE, perl = TRUE)[1]
  df_line <- grep("^\\s*Degrees of Freedom\\s+", block, value = TRUE, ignore.case = TRUE, perl = TRUE)[1]
  p_line <- grep("^\\s*P-Value\\s+", block, value = TRUE, ignore.case = TRUE, perl = TRUE)[1]

  data.frame(
    value = parse_last_number_pm(value_line),
    df = parse_last_number_pm(df_line),
    p = parse_last_number_pm(p_line),
    stringsAsFactors = FALSE
  )
}

parse_mplus_nobs_pm <- function(lines) {
  hit <- grep("Number of observations", lines, value = TRUE, ignore.case = TRUE)
  if (length(hit) == 0) return(NA_real_)
  parse_last_number_pm(hit[1])
}

build_observed_moderation_mplus_data_pm <- function(df, outcome_var, x_var, w_var, covariates, survey_bundle, process_settings) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  needed <- unique(c(
    outcome_var,
    x_var,
    w_var,
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_var, w_var) %in% names(dat))) return(NULL)

  if ("P03" %in% names(dat)) {
    p03_num <- safe_num_pm(dat$P03)
    if (stats::median(p03_num, na.rm = TRUE) > 10000) {
      yy <- floor(p03_num / 10000)
      current_yy <- suppressWarnings(as.integer(format(Sys.Date(), "%y")))
      current_year <- suppressWarnings(as.integer(format(Sys.Date(), "%Y")))
      birth_year <- ifelse(yy <= current_yy, 2000 + yy, 1900 + yy)
      dat$P03 <- current_year - birth_year
    }
  }

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  dat[[w_var]] <- safe_num_pm(dat[[w_var]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_var, w_var, cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  weight_vec <- if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    safe_num_pm(dat[[survey_bundle$weight_var]])
  } else {
    rep(1, nrow(dat))
  }

  center_x <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[x_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[x_var]][analysis_idx], weight_vec[analysis_idx])
  }

  center_w <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }

  dat$x_pm <- dat[[x_var]] - center_x
  dat$w_pm <- dat[[w_var]] - center_w
  dat$xw_pm <- dat$x_pm * dat$w_pm
  dat$y_pm <- dat[[outcome_var]]

  w_sd <- if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_sd_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }
  if (is.na(w_sd) || w_sd <= 0) w_sd <- stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  if (is.na(w_sd) || w_sd <= 0) w_sd <- 1

  probe_mult <- suppressWarnings(as.numeric(process_settings$probe_sd_multiplier %||% 1))
  if (is.na(probe_mult) || probe_mult <= 0) probe_mult <- 1
  low_raw <- center_w - probe_mult * w_sd
  mean_raw <- center_w
  high_raw <- center_w + probe_mult * w_sd
  low_centered <- low_raw - center_w
  mean_centered <- mean_raw - center_w
  high_centered <- high_raw - center_w

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "y_pm", "x_pm", "w_pm", "xw_pm",
    cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]
  export_data <- dat[, export_names, drop = FALSE]

  list(
    export_data = export_data,
    cov_term_map = cov_term_map,
    cov_rhs = cov_rhs,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    center_x = center_x,
    center_w = center_w,
    w_sd = w_sd,
    low_raw = low_raw,
    mean_raw = mean_raw,
    high_raw = high_raw,
    low_centered = low_centered,
    mean_centered = mean_centered,
    high_centered = high_centered,
    analysis_n = sum(analysis_idx, na.rm = TRUE)
  )
}

build_observed_moderation_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, w_var) {
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = CFG$mplus_missing_code %||% CFG$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  rhs_terms <- c("x_pm (b_x)", "w_pm (b_w)", "xw_pm (b_int)")
  if (length(prep_obj$cov_export_names) > 0) {
    rhs_terms <- c(rhs_terms, paste0(prep_obj$cov_export_names, " (b_", prep_obj$cov_export_names, ")"))
  }
  model_on_lines <- c("  y_pm ON", paste0("    ", rhs_terms))
  model_on_lines[length(model_on_lines)] <- paste0(model_on_lines[length(model_on_lines)], ";")
  omnibus_labels <- c("b_x", "b_w", "b_int", if (length(prep_obj$cov_export_names) > 0) paste0("b_", prep_obj$cov_export_names) else character(0))
  model_test_lines <- c("MODEL TEST:", paste0("  ", omnibus_labels, " = 0;"))

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed moderation for ", outcome_var, " on ", x_var, " by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    model_on_lines,
    "  [y_pm] (b0);",
    "MODEL CONSTRAINT:",
    "  NEW(w_low w_mean w_high eff_low eff_mean eff_high);",
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = b_x + b_int*w_low;",
    "  eff_mean = b_x + b_int*w_mean;",
    "  eff_high = b_x + b_int*w_high;",
    model_test_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model1 <- function(df, outcome_var, x_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_moderation_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm1obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_moderation_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    w_var = w_var
  )

  mplus_exe <- resolve_mplus_exe(CFG, default = CFG$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed moderation run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_main_sections_pm(out_lines, outcome_name = "y_pm")
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  parsed_wald <- parse_mplus_wald_test_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section &
      as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  coef_rows <- list()
  idx <- 1L

  intercept_row <- find_row("intercepts", "y_pm")
  if (nrow(intercept_row) == 1) {
    ci_i <- make_ci(intercept_row$estimate[1], intercept_row$se[1])
    coef_rows[[idx]] <- data.frame(
      term = "(Intercept)",
      estimate = intercept_row$estimate[1],
      se = intercept_row$se[1],
      t_value = intercept_row$z_value[1],
      p = intercept_row$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = "Intercept",
      effect = "Intercept",
      term_label = "Intercept",
      variable_label = "Intercept",
      category_label = "",
      reference_label = "",
      sort_key = 10,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  core_specs <- list(
    list(export_name = "x_pm", term = x_var, effect_type = "Independent variable", effect = "Independent variable main effect", label = lookup_label(x_var), sort_key = 20),
    list(export_name = "w_pm", term = w_var, effect_type = "Moderator", effect = "Moderator main effect", label = lookup_label(w_var), sort_key = 120),
    list(export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", effect = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 220)
  )

  for (spec in core_specs) {
    row_i <- find_row("on", spec$export_name)
    if (nrow(row_i) == 0) next
    ci_i <- make_ci(row_i$estimate[1], row_i$se[1])
    coef_rows[[idx]] <- data.frame(
      term = spec$term,
      estimate = row_i$estimate[1],
      se = row_i$se[1],
      t_value = row_i$z_value[1],
      p = row_i$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = spec$effect_type,
      effect = spec$effect,
      term_label = spec$label,
      variable_label = spec$label,
      category_label = "",
      reference_label = "",
      sort_key = spec$sort_key,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("on", map_i$export_name[1])
      if (nrow(row_i) == 0) next
      ci_i <- make_ci(row_i$estimate[1], row_i$se[1])
      coef_rows[[idx]] <- data.frame(
        term = as.character(map_i$term[1]),
        estimate = row_i$estimate[1],
        se = row_i$se[1],
        t_value = row_i$z_value[1],
        p = row_i$p[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        effect_type = as.character(map_i$effect_type[1]),
        effect = as.character(map_i$effect[1]),
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1]),
        sort_key = as.numeric(map_i$sort_key[1]),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- outcome_var
    coef_df$outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  r2_val <- NA_real_
  if (is.data.frame(parsed_r2) && nrow(parsed_r2) > 0) {
    r2_hit <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
    if (nrow(r2_hit) == 1) r2_val <- r2_hit$estimate[1]
  }
  resid_var <- NA_real_
  resid_row <- find_row("residual_variances", "y_pm")
  if (nrow(resid_row) == 1) resid_var <- safe_num_pm(resid_row$estimate[1])
  if (is.na(r2_val) && !is.na(resid_var) && "y_pm" %in% names(prep_obj$export_data)) {
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    y_var <- if (is.null(wt_vec)) {
      stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE)
    } else {
      weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    }
    if (!is.na(y_var) && y_var > 0) r2_val <- max(0, min(1, 1 - resid_var / y_var))
  }

  f_val <- NA_real_
  df1_val <- NA_real_
  p_val <- NA_real_
  if (is.data.frame(parsed_wald) && nrow(parsed_wald) > 0) {
    df1_val <- safe_num_pm(parsed_wald$df[1])
    p_val <- safe_num_pm(parsed_wald$p[1])
    if (!is.na(df1_val) && df1_val > 0) {
      f_val <- safe_num_pm(parsed_wald$value[1]) / df1_val
    }
  }

  model_summary <- data.frame(
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    x_var = x_var,
    x_label = lookup_label(x_var),
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    model_component = "Outcome model",
    n = nobs,
    r2 = r2_val,
    adj_r2 = NA_real_,
    f_value = f_val,
    df1 = df1_val,
    df2 = NA_real_,
    p = p_val,
    delta_r2 = NA_real_,
    delta_f = NA_real_,
    delta_p = NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    stringsAsFactors = FALSE
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  cond_specs <- data.frame(
    name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  idx_c <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    hit <- new_params[as.character(new_params$name) == cond_specs$name[i], , drop = FALSE]
    if (nrow(hit) == 0) next
    ci_i <- make_ci(hit$estimate[1], hit$se[1])
    conditional_list[[idx_c]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      class_num = i,
      class_contrast = cond_specs$class_contrast[i],
      conditional_effect = hit$estimate[1],
      se = hit$se[1],
      t_value = hit$z_value[1],
      p = hit$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      n = nobs,
      stringsAsFactors = FALSE
    )
    idx_c <- idx_c + 1L
  }
  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_moderation_mplus_data_multi_pm <- function(df, outcome_var, x_vars, w_var, covariates, survey_bundle, process_settings) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  x_vars <- unique(as.character(x_vars))
  x_vars <- x_vars[!is.na(x_vars) & nzchar(x_vars) & x_vars %in% names(df)]
  if (length(x_vars) < 2L) return(NULL)

  needed <- unique(c(
    outcome_var,
    x_vars,
    w_var,
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_vars, w_var) %in% names(dat))) return(NULL)

  if ("P03" %in% names(dat)) {
    p03_num <- safe_num_pm(dat$P03)
    if (stats::median(p03_num, na.rm = TRUE) > 10000) {
      yy <- floor(p03_num / 10000)
      current_yy <- suppressWarnings(as.integer(format(Sys.Date(), "%y")))
      current_year <- suppressWarnings(as.integer(format(Sys.Date(), "%Y")))
      birth_year <- ifelse(yy <= current_yy, 2000 + yy, 1900 + yy)
      dat$P03 <- current_year - birth_year
    }
  }

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[w_var]] <- safe_num_pm(dat[[w_var]])
  for (xv in x_vars) dat[[xv]] <- safe_num_pm(dat[[xv]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_vars, w_var, cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  weight_vec <- if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    safe_num_pm(dat[[survey_bundle$weight_var]])
  } else {
    rep(1, nrow(dat))
  }

  center_w <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }

  x_map <- data.frame(
    x_var = x_vars,
    x_label = vapply(x_vars, lookup_label, character(1)),
    x_export = sprintf("x%02d_pm", seq_along(x_vars)),
    int_export = sprintf("xw%02d_pm", seq_along(x_vars)),
    bx_label = sprintf("b_x%02d", seq_along(x_vars)),
    bint_label = sprintf("b_i%02d", seq_along(x_vars)),
    eff_low = sprintf("e%02dl", seq_along(x_vars)),
    eff_mean = sprintf("e%02dm", seq_along(x_vars)),
    eff_high = sprintf("e%02dh", seq_along(x_vars)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(x_map))) {
    xv <- x_map$x_var[i]
    center_x <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
      0
    } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
      mean(dat[[xv]][analysis_idx], na.rm = TRUE)
    } else {
      weighted_mean_pm(dat[[xv]][analysis_idx], weight_vec[analysis_idx])
    }
    dat[[x_map$x_export[i]]] <- dat[[xv]] - center_x
    dat[[x_map$int_export[i]]] <- dat[[x_map$x_export[i]]] * (dat[[w_var]] - center_w)
    x_map$center_x[i] <- center_x
  }

  dat$w_pm <- dat[[w_var]] - center_w
  dat$y_pm <- dat[[outcome_var]]

  w_sd <- if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_sd_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }
  if (is.na(w_sd) || w_sd <= 0) w_sd <- stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  if (is.na(w_sd) || w_sd <= 0) w_sd <- 1

  probe_mult <- suppressWarnings(as.numeric(process_settings$probe_sd_multiplier %||% 1))
  if (is.na(probe_mult) || probe_mult <= 0) probe_mult <- 1
  low_raw <- center_w - probe_mult * w_sd
  mean_raw <- center_w
  high_raw <- center_w + probe_mult * w_sd
  low_centered <- low_raw - center_w
  mean_centered <- mean_raw - center_w
  high_centered <- high_raw - center_w

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "y_pm",
    x_map$x_export,
    "w_pm",
    x_map$int_export,
    cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]
  export_data <- dat[, export_names, drop = FALSE]

  list(
    export_data = export_data,
    cov_term_map = cov_term_map,
    cov_rhs = cov_rhs,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    x_map = x_map,
    center_w = center_w,
    w_sd = w_sd,
    low_raw = low_raw,
    mean_raw = mean_raw,
    high_raw = high_raw,
    low_centered = low_centered,
    mean_centered = mean_centered,
    high_centered = high_centered,
    analysis_n = sum(analysis_idx, na.rm = TRUE)
  )
}

build_observed_moderation_input_multi_pm <- function(prep_obj, model_tag, outcome_var, x_vars, w_var) {
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = CFG$mplus_missing_code %||% CFG$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  rhs_terms <- c(
    paste0(prep_obj$x_map$x_export, " (", prep_obj$x_map$bx_label, ")"),
    "w_pm (b_w)",
    paste0(prep_obj$x_map$int_export, " (", prep_obj$x_map$bint_label, ")")
  )
  if (length(prep_obj$cov_export_names) > 0) {
    rhs_terms <- c(rhs_terms, paste0(prep_obj$cov_export_names, " (b_", prep_obj$cov_export_names, ")"))
  }
  model_on_lines <- c("  y_pm ON", paste0("    ", rhs_terms))
  model_on_lines[length(model_on_lines)] <- paste0(model_on_lines[length(model_on_lines)], ";")

  new_terms <- c("w_low", "w_mean", "w_high", prep_obj$x_map$eff_low, prep_obj$x_map$eff_mean, prep_obj$x_map$eff_high)
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    paste0("  NEW(", paste(new_terms, collapse = " "), ");"),
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";")
  )
  for (i in seq_len(nrow(prep_obj$x_map))) {
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", prep_obj$x_map$eff_low[i], " = ", prep_obj$x_map$bx_label[i], " + ", prep_obj$x_map$bint_label[i], "*w_low;"),
      paste0("  ", prep_obj$x_map$eff_mean[i], " = ", prep_obj$x_map$bx_label[i], " + ", prep_obj$x_map$bint_label[i], "*w_mean;"),
      paste0("  ", prep_obj$x_map$eff_high[i], " = ", prep_obj$x_map$bx_label[i], " + ", prep_obj$x_map$bint_label[i], "*w_high;")
    )
  }

  omnibus_labels <- c(prep_obj$x_map$bx_label, "b_w", prep_obj$x_map$bint_label, if (length(prep_obj$cov_export_names) > 0) paste0("b_", prep_obj$cov_export_names) else character(0))
  model_test_lines <- c("MODEL TEST:", paste0("  ", omnibus_labels, " = 0;"))

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed moderation for ", outcome_var, " on ", paste(x_vars, collapse = ", "), " by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    model_on_lines,
    "  [y_pm] (b0);",
    constraint_lines,
    model_test_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model1_multi <- function(df, outcome_var, x_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_moderation_mplus_data_multi_pm(
    df = df,
    outcome_var = outcome_var,
    x_vars = x_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm1obs2x",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(paste(x_vars, collapse = "_")),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_moderation_input_multi_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_vars = x_vars,
    w_var = w_var
  )

  mplus_exe <- resolve_mplus_exe(CFG, default = CFG$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed moderation run failed for outcome = ", outcome_var,
      ", x = ", paste(x_vars, collapse = ", "), ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_main_sections_pm(out_lines, outcome_name = "y_pm")
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  parsed_wald <- parse_mplus_wald_test_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  coef_rows <- list()
  idx <- 1L

  intercept_row <- find_row("intercepts", "y_pm")
  if (nrow(intercept_row) == 1) {
    ci_i <- make_ci(intercept_row$estimate[1], intercept_row$se[1])
    coef_rows[[idx]] <- data.frame(
      term = "(Intercept)",
      estimate = intercept_row$estimate[1],
      se = intercept_row$se[1],
      t_value = intercept_row$z_value[1],
      p = intercept_row$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = "Intercept",
      effect = "Intercept",
      term_label = "Intercept",
      variable_label = "Intercept",
      category_label = "",
      reference_label = "",
      sort_key = 10,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  for (i in seq_len(nrow(prep_obj$x_map))) {
    xm <- prep_obj$x_map[i, , drop = FALSE]
    row_x <- find_row("on", xm$x_export[1])
    if (nrow(row_x) == 1) {
      ci_i <- make_ci(row_x$estimate[1], row_x$se[1])
      coef_rows[[idx]] <- data.frame(
        term = xm$x_var[1],
        estimate = row_x$estimate[1],
        se = row_x$se[1],
        t_value = row_x$z_value[1],
        p = row_x$p[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        effect_type = "Independent variable",
        effect = "Independent variable main effect",
        term_label = xm$x_label[1],
        variable_label = xm$x_label[1],
        category_label = "",
        reference_label = "",
        sort_key = 20 + i - 1,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  row_w <- find_row("on", "w_pm")
  if (nrow(row_w) == 1) {
    ci_i <- make_ci(row_w$estimate[1], row_w$se[1])
    coef_rows[[idx]] <- data.frame(
      term = w_var,
      estimate = row_w$estimate[1],
      se = row_w$se[1],
      t_value = row_w$z_value[1],
      p = row_w$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = "Moderator",
      effect = "Moderator main effect",
      term_label = lookup_label(w_var),
      variable_label = lookup_label(w_var),
      category_label = "",
      reference_label = "",
      sort_key = 120,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  for (i in seq_len(nrow(prep_obj$x_map))) {
    xm <- prep_obj$x_map[i, , drop = FALSE]
    row_i <- find_row("on", xm$int_export[1])
    if (nrow(row_i) == 0) next
    ci_i <- make_ci(row_i$estimate[1], row_i$se[1])
    coef_rows[[idx]] <- data.frame(
      term = paste0(xm$x_var[1], ":", w_var),
      estimate = row_i$estimate[1],
      se = row_i$se[1],
      t_value = row_i$z_value[1],
      p = row_i$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = "Interaction",
      effect = "Interaction",
      term_label = paste0(xm$x_label[1], " x ", lookup_label(w_var)),
      variable_label = paste0(xm$x_label[1], " x ", lookup_label(w_var)),
      category_label = "",
      reference_label = "",
      sort_key = 220 + i - 1,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("on", map_i$export_name[1])
      if (nrow(row_i) == 0) next
      ci_i <- make_ci(row_i$estimate[1], row_i$se[1])
      coef_rows[[idx]] <- data.frame(
        term = as.character(map_i$term[1]),
        estimate = row_i$estimate[1],
        se = row_i$se[1],
        t_value = row_i$z_value[1],
        p = row_i$p[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        effect_type = as.character(map_i$effect_type[1]),
        effect = as.character(map_i$effect[1]),
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1]),
        sort_key = as.numeric(map_i$sort_key[1]),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- outcome_var
    coef_df$outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- paste(x_vars, collapse = "|")
    coef_df$x_label <- paste(vapply(x_vars, lookup_label, character(1)), collapse = ", ")
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  r2_val <- NA_real_
  if (is.data.frame(parsed_r2) && nrow(parsed_r2) > 0) {
    r2_hit <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
    if (nrow(r2_hit) == 1) r2_val <- r2_hit$estimate[1]
  }
  resid_var <- NA_real_
  resid_row <- find_row("residual_variances", "y_pm")
  if (nrow(resid_row) == 1) resid_var <- safe_num_pm(resid_row$estimate[1])
  if (is.na(r2_val) && !is.na(resid_var) && "y_pm" %in% names(prep_obj$export_data)) {
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    y_var <- if (is.null(wt_vec)) {
      stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE)
    } else {
      weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    }
    if (!is.na(y_var) && y_var > 0) r2_val <- max(0, min(1, 1 - resid_var / y_var))
  }

  f_val <- NA_real_
  df1_val <- NA_real_
  p_val <- NA_real_
  if (is.data.frame(parsed_wald) && nrow(parsed_wald) > 0) {
    df1_val <- safe_num_pm(parsed_wald$df[1])
    p_val <- safe_num_pm(parsed_wald$p[1])
    if (!is.na(df1_val) && df1_val > 0) {
      f_val <- safe_num_pm(parsed_wald$value[1]) / df1_val
    }
  }

  model_summary <- data.frame(
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    x_var = paste(x_vars, collapse = "|"),
    x_label = paste(vapply(x_vars, lookup_label, character(1)), collapse = ", "),
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    model_component = "Outcome model",
    n = nobs,
    r2 = r2_val,
    adj_r2 = NA_real_,
    f_value = f_val,
    df1 = df1_val,
    df2 = NA_real_,
    p = p_val,
    delta_r2 = NA_real_,
    delta_f = NA_real_,
    delta_p = NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    stringsAsFactors = FALSE
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  conditional_list <- list()
  idx_c <- 1L
  for (i in seq_len(nrow(prep_obj$x_map))) {
    xm <- prep_obj$x_map[i, , drop = FALSE]
    cond_specs <- data.frame(
      name = c(xm$eff_low[1], xm$eff_mean[1], xm$eff_high[1]),
      class_contrast = c(
        paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
      ),
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(cond_specs))) {
      hit <- new_params[as.character(new_params$name) == toupper(cond_specs$name[j]), , drop = FALSE]
      if (nrow(hit) == 0) next
      ci_i <- make_ci(hit$estimate[1], hit$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        x_var = xm$x_var[1],
        x_label = xm$x_label[1],
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        class_num = j,
        class_contrast = cond_specs$class_contrast[j],
        conditional_effect = hit$estimate[1],
        se = hit$se[1],
        t_value = hit$z_value[1],
        p = hit$p[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        n = nobs,
        stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
  }
  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      n_x = length(x_vars),
      stringsAsFactors = FALSE
    )
  )
}

build_observed_mediation_mplus_data_pm <- function(df, outcome_var, x_var, mediator_var, covariates, survey_bundle, process_settings) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  needed <- unique(c(
    outcome_var,
    x_var,
    mediator_var,
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_var, mediator_var) %in% names(dat))) return(NULL)

  if ("P03" %in% names(dat)) {
    p03_num <- safe_num_pm(dat$P03)
    if (stats::median(p03_num, na.rm = TRUE) > 10000) {
      yy <- floor(p03_num / 10000)
      current_yy <- suppressWarnings(as.integer(format(Sys.Date(), "%y")))
      current_year <- suppressWarnings(as.integer(format(Sys.Date(), "%Y")))
      birth_year <- ifelse(yy <= current_yy, 2000 + yy, 1900 + yy)
      dat$P03 <- current_year - birth_year
    }
  }

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  dat[[mediator_var]] <- safe_num_pm(dat[[mediator_var]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_var, mediator_var, cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  dat$x_pm <- safe_num_pm(dat[[x_var]])
  dat$m_pm <- safe_num_pm(dat[[mediator_var]])
  dat$y_pm <- safe_num_pm(dat[[outcome_var]])

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "x_pm", "m_pm", "y_pm", cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]
  export_data <- dat[, export_names, drop = FALSE]

  list(
    export_data = export_data,
    cov_term_map = cov_term_map,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    analysis_n = sum(analysis_idx, na.rm = TRUE),
    x_var = x_var,
    x_label = lookup_label(x_var),
    mediator_var = mediator_var,
    mediator_label = lookup_label(mediator_var),
    outcome_var = outcome_var,
    outcome_label = lookup_label(outcome_var)
  )
}

build_observed_mediation_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var) {
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = CFG$mplus_missing_code %||% CFG$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, paste0(prep_obj$cov_export_names, " (a_", prep_obj$cov_export_names, ")"))
  out_rhs <- c("x_pm (cp1)", "m_pm (b1)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, paste0(prep_obj$cov_export_names, " (b_", prep_obj$cov_export_names, ")"))

  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")
  model_lines <- c(
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);"
  )

  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    "  NEW(ind total);",
    "  ind = a1*b1;",
    "  total = cp1 + ind;"
  )

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed mediation for ", outcome_var, " from ", x_var, " through ", mediator_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model4 <- function(df, outcome_var, x_var, mediator_var, covariates, survey_bundle) {
  prep_obj <- build_observed_mediation_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm4obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sep = "_"
  )
  paths <- build_observed_mediation_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var
  )

  mplus_exe <- resolve_mplus_exe(CFG, default = CFG$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed mediation run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) {
      return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    }

    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) {
      dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    }
    if (!nrow(dat_fit)) {
      return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    }

    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) {
          stats::as.formula(paste0("~", prep_obj$design_map$cluster_var))
        } else {
          ~1
        }
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) {
          stats::as.formula(paste0("~", prep_obj$design_map$strata_var))
        } else {
          NULL
        }
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) {
          stats::as.formula(paste0("~", prep_obj$design_map$weight_var))
        } else {
          NULL
        }

        des <- survey::svydesign(
          ids = ids_formula,
          strata = strata_formula,
          weights = weights_formula,
          data = dat_fit,
          nest = TRUE
        )
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(
          fit,
          stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))),
          method = "Wald"
        )

        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) {
        NULL
      })
      if (!is.null(survey_res)) return(survey_res)
    }

    lm_res <- tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) {
        stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
      } else {
        NA_real_
      }
      list(
        f_value = safe_num_pm(fstat[1]),
        df1 = safe_num_pm(fstat[2]),
        p = safe_num_pm(p_i)
      )
    }, error = function(e) {
      list(f_value = NA_real_, df1 = NA_real_, p = NA_real_)
    })
    lm_res
  }

  coef_rows <- list()
  idx <- 1L

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      stringsAsFactors = FALSE
    )
  }

  row_im <- find_row("intercepts", "m_pm")
  tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) {
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  row_a <- find_row("m_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_a, "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model")
  if (!is.null(tmp_row)) {
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("m_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Mediator model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 400000 + i,
        model_component = "Mediator model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) {
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  row_cp <- find_row("y_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_cp, "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model")
  if (!is.null(tmp_row)) {
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  row_b <- find_row("y_on", "m_pm")
  tmp_row <- add_coef_row(mediator_var, row_b, "Mediator", "Outcome model", lookup_label(mediator_var), lookup_label(mediator_var), 1000030, "Outcome model")
  if (!is.null(tmp_row)) {
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + i,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  r2_m <- NA_real_
  r2_y <- NA_real_
  if (is.data.frame(parsed_r2) && nrow(parsed_r2) > 0) {
    hit_m <- parsed_r2[as.character(parsed_r2$name) == "M_PM", , drop = FALSE]
    hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
    if (nrow(hit_m) == 1) r2_m <- hit_m$estimate[1]
    if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  }

  resid_m <- find_row("residual_variances", "m_pm")
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_m <- if (nrow(resid_m) == 1) safe_num_pm(resid_m$estimate[1]) else NA_real_
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  if (is.na(r2_m) && !is.na(resid_var_m) && "m_pm" %in% names(prep_obj$export_data)) {
    m_var <- if (is.null(wt_vec)) {
      stats::var(safe_num_pm(prep_obj$export_data$m_pm), na.rm = TRUE)
    } else {
      weighted_sd_pm(prep_obj$export_data$m_pm, wt_vec)^2
    }
    if (!is.na(m_var) && m_var > 0) r2_m <- max(0, min(1, 1 - resid_var_m / m_var))
  }
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) {
      stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE)
    } else {
      weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    }
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", prep_obj$cov_export_names))

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  row_ind <- new_params[as.character(new_params$name) == "IND", , drop = FALSE]
  row_total <- new_params[as.character(new_params$name) == "TOTAL", , drop = FALSE]

  a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mediator_var, , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]

  indirect_df <- data.frame()
  if (nrow(a_row) == 1 && nrow(b_row) == 1 && nrow(row_ind) == 1) {
    ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
    indirect_df <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      class_contrast = "Indirect effect",
      a = a_row$estimate[1],
      a_se = a_row$se[1],
      a_p = a_row$p[1],
      b = b_row$estimate[1],
      b_se = b_row$se[1],
      b_p = b_row$p[1],
      direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
      total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
      indirect = row_ind$estimate[1],
      se = row_ind$se[1],
      z_value = row_ind$z_value[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      p = row_ind$p[1],
      p_fmt = fmt_p_pm(row_ind$p[1]),
      sig = sig_mark_pm(row_ind$p[1]),
      stringsAsFactors = FALSE
    )
  }

  model_summary <- rbind(
    data.frame(
      model_component = "Mediator model",
      outcome = mediator_var,
      outcome_label = lookup_label(mediator_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = r2_m,
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    ),
    data.frame(
      model_component = "Outcome model",
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = r2_y,
      adj_r2 = NA_real_,
      f_value = out_test$f_value %||% NA_real_,
      df1 = out_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = out_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    )
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_model5_mplus_data_pm <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle, process_settings) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  needed <- unique(c(
    outcome_var,
    x_var,
    mediator_var,
    w_var,
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_var, mediator_var, w_var) %in% names(dat))) return(NULL)

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  dat[[mediator_var]] <- safe_num_pm(dat[[mediator_var]])
  dat[[w_var]] <- safe_num_pm(dat[[w_var]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_var, mediator_var, w_var, cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  weight_vec <- if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    safe_num_pm(dat[[survey_bundle$weight_var]])
  } else {
    rep(1, nrow(dat))
  }

  center_x <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[x_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[x_var]][analysis_idx], weight_vec[analysis_idx])
  }
  center_w <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }

  w_sd <- if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_sd_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }
  if (is.na(w_sd) || w_sd <= 0) w_sd <- stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  if (is.na(w_sd) || w_sd <= 0) w_sd <- 1

  probe_mult <- suppressWarnings(as.numeric(process_settings$probe_sd_multiplier %||% 1))
  if (is.na(probe_mult) || probe_mult <= 0) probe_mult <- 1
  low_raw <- center_w - probe_mult * w_sd
  mean_raw <- center_w
  high_raw <- center_w + probe_mult * w_sd
  low_centered <- low_raw - center_w
  mean_centered <- mean_raw - center_w
  high_centered <- high_raw - center_w

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  dat$x_pm <- dat[[x_var]] - center_x
  dat$w_pm <- dat[[w_var]] - center_w
  dat$xw_pm <- dat$x_pm * dat$w_pm
  dat$m_pm <- dat[[mediator_var]]
  dat$y_pm <- dat[[outcome_var]]

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "x_pm", "w_pm", "xw_pm", "m_pm", "y_pm", cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]

  list(
    export_data = dat[, export_names, drop = FALSE],
    cov_term_map = cov_term_map,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    center_x = center_x,
    center_w = center_w,
    w_sd = w_sd,
    low_raw = low_raw,
    mean_raw = mean_raw,
    high_raw = high_raw,
    low_centered = low_centered,
    mean_centered = mean_centered,
    high_centered = high_centered,
    analysis_n = sum(analysis_idx, na.rm = TRUE),
    x_var = x_var,
    x_label = lookup_label(x_var),
    mediator_var = mediator_var,
    mediator_label = lookup_label(mediator_var),
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    outcome_var = outcome_var,
    outcome_label = lookup_label(outcome_var)
  )
}

build_observed_model5_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = CFG$mplus_missing_code %||% CFG$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)", "w_pm (bw)", "xw_pm (bint)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 5 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with direct effect moderated by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    "  NEW(w_low w_mean w_high eff_low eff_mean eff_high ind);",
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + bint*w_low;",
    "  eff_mean = cp1 + bint*w_mean;",
    "  eff_high = cp1 + bint*w_high;",
    "  ind = a1*b1;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model5 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm5obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model5_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var
  )

  mplus_exe <- resolve_mplus_exe(CFG, default = CFG$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 5 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L

  row_im <- find_row("intercepts", "m_pm")
  tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  row_a <- find_row("m_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_a, "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("m_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Mediator model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 400000 + i,
        model_component = "Mediator model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  core_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", effect = "Outcome model", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", effect = "Outcome model", label = lookup_label(mediator_var), sort_key = 1000030),
    list(section = "y_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", effect = "Outcome model", label = lookup_label(w_var), sort_key = 1000040),
    list(section = "y_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", effect = "Outcome model", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 1000050)
  )
  for (spec in core_specs) {
    row_i <- find_row(spec$section, spec$export_name)
    tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, spec$effect, spec$label, spec$label, spec$sort_key, "Outcome model")
    if (is.null(tmp_row)) next
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + i,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))

  model_summary <- rbind(
    data.frame(
      model_component = "Mediator model",
      outcome = mediator_var,
      outcome_label = lookup_label(mediator_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm("m_pm"),
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    ),
    data.frame(
      model_component = "Outcome model",
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm("y_pm"),
      adj_r2 = NA_real_,
      f_value = out_test$f_value %||% NA_real_,
      df1 = out_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = out_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    )
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  row_ind <- new_params[as.character(new_params$name) == "IND", , drop = FALSE]

  a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mediator_var, , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]

  indirect_df <- data.frame()
  if (nrow(a_row) == 1 && nrow(b_row) == 1 && nrow(row_ind) == 1) {
    ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
    indirect_df <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      class_contrast = "Indirect effect",
      a = a_row$estimate[1],
      a_se = a_row$se[1],
      a_p = a_row$p[1],
      b = b_row$estimate[1],
      b_se = b_row$se[1],
      b_p = b_row$p[1],
      direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
      total = NA_real_,
      indirect = row_ind$estimate[1],
      se = row_ind$se[1],
      z_value = row_ind$z_value[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      p = row_ind$p[1],
      p_fmt = fmt_p_pm(row_ind$p[1]),
      sig = sig_mark_pm(row_ind$p[1]),
      stringsAsFactors = FALSE
    )
  }

  cond_specs <- data.frame(
    name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )
  conditional_list <- list()
  idx_c <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    hit <- new_params[as.character(new_params$name) == cond_specs$name[i], , drop = FALSE]
    if (nrow(hit) == 0) next
    ci_i <- make_ci(hit$estimate[1], hit$se[1])
    conditional_list[[idx_c]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      class_num = i,
      class_contrast = cond_specs$class_contrast[i],
      conditional_effect = hit$estimate[1],
      se = hit$se[1],
      t_value = hit$z_value[1],
      p = hit$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      n = nobs,
      stringsAsFactors = FALSE
    )
    idx_c <- idx_c + 1L
  }
  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_parallel_model5_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle, process_settings) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  mediator_vars <- unique(as.character(mediator_vars))
  mediator_vars <- mediator_vars[!is.na(mediator_vars) & nzchar(mediator_vars)]
  if (!length(mediator_vars)) return(NULL)

  needed <- unique(c(
    outcome_var,
    x_var,
    mediator_vars,
    w_var,
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_var, mediator_vars, w_var) %in% names(dat))) return(NULL)

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  dat[[w_var]] <- safe_num_pm(dat[[w_var]])
  for (mv in mediator_vars) dat[[mv]] <- safe_num_pm(dat[[mv]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_var, mediator_vars, w_var, cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  weight_vec <- if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    safe_num_pm(dat[[survey_bundle$weight_var]])
  } else {
    rep(1, nrow(dat))
  }

  center_x <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[x_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[x_var]][analysis_idx], weight_vec[analysis_idx])
  }
  center_w <- if (identical(process_settings$centering_method %||% "weighted_mean", "none")) {
    0
  } else if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    mean(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_mean_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }

  w_sd <- if (identical(process_settings$centering_method %||% "weighted_mean", "mean")) {
    stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  } else {
    weighted_sd_pm(dat[[w_var]][analysis_idx], weight_vec[analysis_idx])
  }
  if (is.na(w_sd) || w_sd <= 0) w_sd <- stats::sd(dat[[w_var]][analysis_idx], na.rm = TRUE)
  if (is.na(w_sd) || w_sd <= 0) w_sd <- 1

  probe_mult <- suppressWarnings(as.numeric(process_settings$probe_sd_multiplier %||% 1))
  if (is.na(probe_mult) || probe_mult <= 0) probe_mult <- 1
  low_raw <- center_w - probe_mult * w_sd
  mean_raw <- center_w
  high_raw <- center_w + probe_mult * w_sd

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  mediator_map <- data.frame(
    mediator_var = mediator_vars,
    mediator_label = vapply(mediator_vars, lookup_label, character(1)),
    export_name = sprintf("m%02d_pm", seq_along(mediator_vars)),
    a_label = paste0("a", seq_along(mediator_vars)),
    b_label = paste0("b", seq_along(mediator_vars)),
    intercept_label = paste0("im", seq_along(mediator_vars)),
    indirect_label = paste0("ind", seq_along(mediator_vars)),
    block_order = seq_along(mediator_vars),
    stringsAsFactors = FALSE
  )

  dat$x_pm <- dat[[x_var]] - center_x
  dat$w_pm <- dat[[w_var]] - center_w
  dat$xw_pm <- dat$x_pm * dat$w_pm
  for (i in seq_len(nrow(mediator_map))) {
    dat[[mediator_map$export_name[i]]] <- safe_num_pm(dat[[mediator_map$mediator_var[i]]])
  }
  dat$y_pm <- safe_num_pm(dat[[outcome_var]])

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "x_pm", "w_pm", "xw_pm",
    mediator_map$export_name,
    "y_pm",
    cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]

  list(
    export_data = dat[, export_names, drop = FALSE],
    cov_term_map = cov_term_map,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    center_x = center_x,
    center_w = center_w,
    w_sd = w_sd,
    low_raw = low_raw,
    mean_raw = mean_raw,
    high_raw = high_raw,
    low_centered = low_raw - center_w,
    mean_centered = mean_raw - center_w,
    high_centered = high_raw - center_w,
    analysis_n = sum(analysis_idx, na.rm = TRUE),
    x_var = x_var,
    x_label = lookup_label(x_var),
    mediator_map = mediator_map,
    mediator_set_id = paste(mediator_map$mediator_var, collapse = "|"),
    mediator_set_label = paste(mediator_map$mediator_label, collapse = ", "),
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    outcome_var = outcome_var,
    outcome_label = lookup_label(outcome_var)
  )
}

build_observed_parallel_model5_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    med_rhs <- c(paste0("x_pm (", mm$a_label[1], ")"), prep_obj$cov_export_names)
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(
      model_lines,
      med_lines,
      paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");")
    )
  }

  out_rhs <- c(
    "x_pm (cp1)",
    paste0(prep_obj$mediator_map$export_name, " (", prep_obj$mediator_map$b_label, ")"),
    "w_pm (bw)",
    "xw_pm (bint)",
    prep_obj$cov_export_names
  )
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")
  model_lines <- c(model_lines, out_model_lines, "  [y_pm] (iy);")

  diff_map <- build_indirect_difference_map_pm(prep_obj$mediator_map)
  indirect_names <- prep_obj$mediator_map$indirect_label
  diff_names <- diff_map$param_name %||% character(0)
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    paste0("  NEW(w_low w_mean w_high eff_low eff_mean eff_high ", paste(c(indirect_names, diff_names, "ind_total", "total"), collapse = " "), ");")
  )
  constraint_lines <- c(
    constraint_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + bint*w_low;",
    "  eff_mean = cp1 + bint*w_mean;",
    "  eff_high = cp1 + bint*w_high;"
  )
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", prep_obj$mediator_map$indirect_label[i], " = ", prep_obj$mediator_map$a_label[i], "*", prep_obj$mediator_map$b_label[i], ";")
    )
  }
  if (is.data.frame(diff_map) && nrow(diff_map) > 0) {
    for (k in seq_len(nrow(diff_map))) {
      constraint_lines <- c(
        constraint_lines,
        paste0("  ", diff_map$param_name[k], " = ", prep_obj$mediator_map$indirect_label[diff_map$i[k]], " - ", prep_obj$mediator_map$indirect_label[diff_map$j[k]], ";")
      )
    }
  }
  constraint_lines <- c(
    constraint_lines,
    paste0("  ind_total = ", paste(indirect_names, collapse = " + "), ";"),
    "  total = cp1 + ind_total;"
  )

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 5 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with direct effect moderated by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model5_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm5obspar",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_parallel_model5_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 5 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      mediator = mediator,
      mediator_label = mediator_label,
      table_block_key = table_block_key,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    row_im <- find_row("intercepts", mm$export_name[1])
    tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    row_a <- find_row(paste0(tolower(mm$export_name[1]), "_on"), "x_pm", lhs = mm$export_name[1])
    tmp_row <- add_coef_row(x_var, row_a, "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1])
        tmp_row <- add_coef_row(
          term = as.character(map_i$term[1]),
          row_obj = row_i,
          effect_type = as.character(map_i$effect_type[1]),
          effect = "Mediator model",
          term_label = as.character(map_i$term_label[1]),
          variable_label = as.character(map_i$variable_label[1]),
          sort_key = 400000 + j,
          model_component = "Mediator model",
          category_label = as.character(map_i$category_label[1]),
          reference_label = as.character(map_i$reference_label[1]),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          table_block_key = block_key,
          block_order = mm$block_order[1]
        )
        if (is.null(tmp_row)) next
        coef_rows[[idx]] <- tmp_row
        idx <- idx + 1L
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  row_cp <- find_row("y_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_cp, "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    row_b <- find_row("y_on", mm$export_name[1])
    tmp_row <- add_coef_row(mm$mediator_var[1], row_b, "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000030 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  row_w <- find_row("y_on", "w_pm")
  tmp_row <- add_coef_row(w_var, row_w, "Moderator", "Outcome model", lookup_label(w_var), lookup_label(w_var), 1000200, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  row_int <- find_row("y_on", "xw_pm")
  tmp_row <- add_coef_row(paste0(x_var, ":", w_var), row_int, "Interaction", "Outcome model", paste0(lookup_label(x_var), " x ", lookup_label(w_var)), paste0(lookup_label(x_var), " x ", lookup_label(w_var)), 1000210, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + j,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1]),
        mediator = mediator_set_id,
        mediator_label = mediator_set_label,
        table_block_key = outcome_block_key,
        block_order = outcome_block_order
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(
      model_component = "Mediator model",
      outcome = mm$mediator_var[1],
      outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1],
      mediator_label = mm$mediator_label[1],
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = r2_i,
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]),
      block_order = mm$block_order[1],
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
    sidx <- sidx + 1L
  }

  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", prep_obj$mediator_map$export_name, "w_pm", "xw_pm", prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(
    model_component = "Outcome model",
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    mediator = mediator_set_id,
    mediator_label = mediator_set_label,
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    x_var = x_var,
    x_label = lookup_label(x_var),
    target_outcome = outcome_var,
    target_outcome_label = lookup_label(outcome_var),
    n = nobs,
    r2 = r2_y,
    adj_r2 = NA_real_,
    f_value = out_test$f_value %||% NA_real_,
    df1 = out_test$df1 %||% NA_real_,
    df2 = NA_real_,
    p = out_test$p %||% NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key,
    block_order = outcome_block_order,
    analysis_key = analysis_key,
    stringsAsFactors = FALSE
  )
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  diff_map <- build_indirect_difference_map_pm(prep_obj$mediator_map)
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  row_total <- new_params[as.character(new_params$name) == "TOTAL", , drop = FALSE]
  indirect_rows <- list()
  iidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$mediator) == mm$mediator_var[1] & as.character(coef_df$term) == x_var, , drop = FALSE]
    b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mm$mediator_var[1], , drop = FALSE]
    row_ind <- new_params[as.character(new_params$name) == toupper(mm$indirect_label[1]), , drop = FALSE]
    if (nrow(a_row) == 1 && nrow(b_row) == 1 && nrow(row_ind) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[iidx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = mm$mediator_var[1],
        mediator_label = mm$mediator_label[1],
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Indirect effect",
        a = a_row$estimate[1],
        a_se = a_row$se[1],
        a_p = a_row$p[1],
        b = b_row$estimate[1],
        b_se = b_row$se[1],
        b_p = b_row$p[1],
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
        indirect = row_ind$estimate[1],
        se = row_ind$se[1],
        z_value = row_ind$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_ind$p[1],
        p_fmt = fmt_p_pm(row_ind$p[1]),
        sig = sig_mark_pm(row_ind$p[1]),
        contrast_order = i,
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      iidx <- iidx + 1L
    }
  }
  if (is.data.frame(diff_map) && nrow(diff_map) > 0) {
    for (k in seq_len(nrow(diff_map))) {
      row_diff <- new_params[as.character(new_params$name) == toupper(diff_map$param_name[k]), , drop = FALSE]
      if (nrow(row_diff) != 1) next
      ci_i <- make_ci(row_diff$estimate[1], row_diff$se[1])
      indirect_rows[[iidx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = paste0(diff_map$mediator1[k], " - ", diff_map$mediator2[k]),
        mediator_label = paste0(diff_map$mediator1_label[k], " - ", diff_map$mediator2_label[k]),
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Indirect effect difference",
        a = NA_real_,
        a_se = NA_real_,
        a_p = NA_real_,
        b = NA_real_,
        b_se = NA_real_,
        b_p = NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
        indirect = row_diff$estimate[1],
        se = row_diff$se[1],
        z_value = row_diff$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_diff$p[1],
        p_fmt = fmt_p_pm(row_diff$p[1]),
        sig = sig_mark_pm(row_diff$p[1]),
        contrast_order = 100 + k,
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      iidx <- iidx + 1L
    }
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  if (is.data.frame(indirect_df) && nrow(indirect_df) > 0 && "contrast_order" %in% names(indirect_df)) {
    indirect_df <- indirect_df[order(indirect_df$contrast_order, indirect_df$mediator_label), , drop = FALSE]
    rownames(indirect_df) <- NULL
  }

  cond_specs <- data.frame(
    name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )
  conditional_list <- list()
  cidx <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    hit <- new_params[as.character(new_params$name) == cond_specs$name[i], , drop = FALSE]
    if (nrow(hit) == 0) next
    ci_i <- make_ci(hit$estimate[1], hit$se[1])
    conditional_list[[cidx]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      mediator = mediator_set_id,
      mediator_label = mediator_set_label,
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      class_num = i,
      class_contrast = cond_specs$class_contrast[i],
      conditional_effect = hit$estimate[1],
      se = hit$se[1],
      t_value = hit$z_value[1],
      p = hit$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      n = nobs,
      stringsAsFactors = FALSE
    )
    cidx <- cidx + 1L
  }
  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_model7_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)", "w_pm (aw)", "xw_pm (aint)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 7 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with first-stage moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    "  NEW(w_low w_mean w_high a_low a_mean a_high ind_low ind_mean ind_high imm);",
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  a_low = a1 + aint*w_low;",
    "  a_mean = a1 + aint*w_mean;",
    "  a_high = a1 + aint*w_high;",
    "  ind_low = a_low*b1;",
    "  ind_mean = a_mean*b1;",
    "  ind_high = a_high*b1;",
    "  imm = aint*b1;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model7 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm7obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model7_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 7 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L

  row_im <- find_row("intercepts", "m_pm")
  tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  core_med_specs <- list(
    list(section = "m_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
    list(section = "m_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
    list(section = "m_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
  )
  for (spec in core_med_specs) {
    row_i <- find_row(spec$section, spec$export_name)
    tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model")
    if (is.null(tmp_row)) next
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("m_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Mediator model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 400000 + i,
        model_component = "Mediator model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  core_out_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", label = lookup_label(mediator_var), sort_key = 1000030)
  )
  for (spec in core_out_specs) {
    row_i <- find_row(spec$section, spec$export_name)
    tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Outcome model", spec$label, spec$label, spec$sort_key, "Outcome model")
    if (is.null(tmp_row)) next
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + i,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", prep_obj$cov_export_names))

  model_summary <- rbind(
    data.frame(
      model_component = "Mediator model",
      outcome = mediator_var,
      outcome_label = lookup_label(mediator_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm("m_pm"),
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    ),
    data.frame(
      model_component = "Outcome model",
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm("y_pm"),
      adj_r2 = NA_real_,
      f_value = out_test$f_value %||% NA_real_,
      df1 = out_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = out_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    )
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mediator_var, , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]

  cond_specs <- data.frame(
    name = c("A_LOW", "A_MEAN", "A_HIGH"),
    ind_name = c("IND_LOW", "IND_MEAN", "IND_HIGH"),
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  indirect_rows <- list()
  idx_c <- 1L
  idx_i <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    row_aeff <- get_new_param(cond_specs$name[i])
    row_ind <- get_new_param(cond_specs$ind_name[i])
    if (nrow(row_aeff) == 1) {
      ci_i <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = mediator_var,
        outcome_label = lookup_label(mediator_var),
        target_outcome = outcome_var,
        target_outcome_label = lookup_label(outcome_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        mediator = mediator_var,
        mediator_label = lookup_label(mediator_var),
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        class_num = i,
        class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_aeff$estimate[1],
        se = row_aeff$se[1],
        t_value = row_aeff$z_value[1],
        p = row_aeff$p[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        n = nobs,
        stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_ind) == 1 && nrow(b_row) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[idx_i]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = mediator_var,
        mediator_label = lookup_label(mediator_var),
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = paste0("Conditional indirect effect at ", cond_specs$class_contrast[i]),
        a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_,
        a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_,
        a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_,
        b = b_row$estimate[1],
        b_se = b_row$se[1],
        b_p = b_row$p[1],
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_,
        indirect = row_ind$estimate[1],
        se = row_ind$se[1],
        z_value = row_ind$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_ind$p[1],
        p_fmt = fmt_p_pm(row_ind$p[1]),
        sig = sig_mark_pm(row_ind$p[1]),
        stringsAsFactors = FALSE
      )
      idx_i <- idx_i + 1L
    }
  }
  row_imm <- get_new_param("IMM")
  if (nrow(row_imm) == 1) {
    ci_i <- make_ci(row_imm$estimate[1], row_imm$se[1])
    indirect_rows[[idx_i]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      class_contrast = "Index of moderated mediation",
      a = NA_real_,
      a_se = NA_real_,
      a_p = NA_real_,
      b = if (nrow(b_row) == 1) b_row$estimate[1] else NA_real_,
      b_se = if (nrow(b_row) == 1) b_row$se[1] else NA_real_,
      b_p = if (nrow(b_row) == 1) b_row$p[1] else NA_real_,
      direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
      total = NA_real_,
      indirect = row_imm$estimate[1],
      se = row_imm$se[1],
      z_value = row_imm$z_value[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      p = row_imm$p[1],
      p_fmt = fmt_p_pm(row_imm$p[1]),
      sig = sig_mark_pm(row_imm$p[1]),
      stringsAsFactors = FALSE
    )
  }

  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_parallel_model7_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_map <- prep_obj$mediator_map
  med_map$w_label <- paste0("aw", seq_len(nrow(med_map)))
  med_map$int_label <- paste0("aiw", seq_len(nrow(med_map)))
  med_map$a_low_label <- paste0("al", seq_len(nrow(med_map)))
  med_map$a_mean_label <- paste0("am", seq_len(nrow(med_map)))
  med_map$a_high_label <- paste0("ah", seq_len(nrow(med_map)))
  med_map$ind_low_label <- paste0("il", seq_len(nrow(med_map)))
  med_map$ind_mean_label <- paste0("imn", seq_len(nrow(med_map)))
  med_map$ind_high_label <- paste0("ih", seq_len(nrow(med_map)))
  med_map$imm_label <- paste0("imm", seq_len(nrow(med_map)))

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(med_map))) {
    mm <- med_map[i, , drop = FALSE]
    med_rhs <- c(
      paste0("x_pm (", mm$a_label[1], ")"),
      paste0("w_pm (", mm$w_label[1], ")"),
      paste0("xw_pm (", mm$int_label[1], ")"),
      prep_obj$cov_export_names
    )
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(model_lines, med_lines, paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");"))
  }

  out_rhs <- c("x_pm (cp1)", paste0(med_map$export_name, " (", med_map$b_label, ")"), prep_obj$cov_export_names)
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_lines[length(out_lines)] <- paste0(out_lines[length(out_lines)], ";")
  model_lines <- c(model_lines, out_lines, "  [y_pm] (iy);")

  new_terms <- c("w_low", "w_mean", "w_high", unlist(med_map[, c("a_low_label", "a_mean_label", "a_high_label", "ind_low_label", "ind_mean_label", "ind_high_label", "imm_label"), drop = FALSE], use.names = FALSE))
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    paste0("  NEW(", paste(new_terms, collapse = " "), ");"),
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";")
  )
  for (i in seq_len(nrow(med_map))) {
    mm <- med_map[i, , drop = FALSE]
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", mm$a_low_label[1], " = ", mm$a_label[1], " + ", mm$int_label[1], "*w_low;"),
      paste0("  ", mm$a_mean_label[1], " = ", mm$a_label[1], " + ", mm$int_label[1], "*w_mean;"),
      paste0("  ", mm$a_high_label[1], " = ", mm$a_label[1], " + ", mm$int_label[1], "*w_high;"),
      paste0("  ", mm$ind_low_label[1], " = ", mm$a_low_label[1], "*", mm$b_label[1], ";"),
      paste0("  ", mm$ind_mean_label[1], " = ", mm$a_mean_label[1], "*", mm$b_label[1], ";"),
      paste0("  ", mm$ind_high_label[1], " = ", mm$a_high_label[1], "*", mm$b_label[1], ";"),
      paste0("  ", mm$imm_label[1], " = ", mm$int_label[1], "*", mm$b_label[1], ";")
    )
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 7 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with first-stage moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model7_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  prep_obj$mediator_map$w_label <- paste0("aw", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$int_label <- paste0("aiw", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_low_label <- paste0("al", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_mean_label <- paste0("am", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_high_label <- paste0("ah", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_low_label <- paste0("il", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_mean_label <- paste0("imn", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_high_label <- paste0("ih", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$imm_label <- paste0("imm", seq_len(nrow(prep_obj$mediator_map)))

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm7obspar",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_parallel_model7_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 7 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      mediator = mediator,
      mediator_label = mediator_label,
      table_block_key = table_block_key,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", mm$export_name[1]), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    med_specs <- list(
      list(export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
      list(export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
      list(export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
    )
    for (spec in med_specs) {
      row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), spec$export_name, lhs = mm$export_name[1])
      tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1])
        tmp_row <- add_coef_row(
          term = as.character(map_i$term[1]),
          row_obj = row_i,
          effect_type = as.character(map_i$effect_type[1]),
          effect = "Mediator model",
          term_label = as.character(map_i$term_label[1]),
          variable_label = as.character(map_i$variable_label[1]),
          sort_key = 400000 + j,
          model_component = "Mediator model",
          category_label = as.character(map_i$category_label[1]),
          reference_label = as.character(map_i$reference_label[1]),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          table_block_key = block_key,
          block_order = mm$block_order[1]
        )
        if (is.null(tmp_row)) next
        coef_rows[[idx]] <- tmp_row
        idx <- idx + 1L
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(x_var, find_row("y_on", "x_pm"), "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    tmp_row <- add_coef_row(mm$mediator_var[1], find_row("y_on", mm$export_name[1]), "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000030 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + j,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1]),
        mediator = mediator_set_id,
        mediator_label = mediator_set_label,
        table_block_key = outcome_block_key,
        block_order = outcome_block_order
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(
      model_component = "Mediator model",
      outcome = mm$mediator_var[1],
      outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1],
      mediator_label = mm$mediator_label[1],
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = r2_i,
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]),
      block_order = mm$block_order[1],
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
    sidx <- sidx + 1L
  }

  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", prep_obj$mediator_map$export_name, prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(
    model_component = "Outcome model",
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    mediator = mediator_set_id,
    mediator_label = mediator_set_label,
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    x_var = x_var,
    x_label = lookup_label(x_var),
    target_outcome = outcome_var,
    target_outcome_label = lookup_label(outcome_var),
    n = nobs,
    r2 = r2_y,
    adj_r2 = NA_real_,
    f_value = out_test$f_value %||% NA_real_,
    df1 = out_test$df1 %||% NA_real_,
    df2 = NA_real_,
    p = out_test$p %||% NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key,
    block_order = outcome_block_order,
    analysis_key = analysis_key,
    stringsAsFactors = FALSE
  )
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  indirect_rows <- list()
  conditional_rows <- list()
  ridx <- 1L
  cidx <- 1L
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mm$mediator_var[1], , drop = FALSE]
    probe_map <- data.frame(
      a_name = c(mm$a_low_label[1], mm$a_mean_label[1], mm$a_high_label[1]),
      ind_name = c(mm$ind_low_label[1], mm$ind_mean_label[1], mm$ind_high_label[1]),
      class_num = 1:3,
      class_contrast = c(
        paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
      ),
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(probe_map))) {
      row_aeff <- get_new_param(probe_map$a_name[j])
      row_ind <- get_new_param(probe_map$ind_name[j])
      if (nrow(row_aeff) == 1) {
        ci_j <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
        conditional_rows[[cidx]] <- data.frame(
          outcome = mm$mediator_var[1],
          outcome_label = mm$mediator_label[1],
          target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var),
          x_var = x_var,
          x_label = lookup_label(x_var),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          moderator = w_var,
          moderator_label = lookup_label(w_var),
          class_num = probe_map$class_num[j],
          class_contrast = probe_map$class_contrast[j],
          conditional_effect = row_aeff$estimate[1],
          se = row_aeff$se[1],
          t_value = row_aeff$z_value[1],
          p = row_aeff$p[1],
          llci = ci_j["llci"],
          ulci = ci_j["ulci"],
          n = nobs,
          stringsAsFactors = FALSE
        )
        cidx <- cidx + 1L
      }
      if (nrow(row_ind) == 1 && nrow(b_row) == 1) {
        ci_j <- make_ci(row_ind$estimate[1], row_ind$se[1])
        indirect_rows[[ridx]] <- data.frame(
          outcome = outcome_var,
          outcome_label = lookup_label(outcome_var),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          moderator = w_var,
          moderator_label = lookup_label(w_var),
          x_var = x_var,
          x_label = lookup_label(x_var),
          class_contrast = paste0("Conditional indirect effect at ", probe_map$class_contrast[j]),
          a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_,
          a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_,
          a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_,
          b = b_row$estimate[1],
          b_se = b_row$se[1],
          b_p = b_row$p[1],
          direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
          total = NA_real_,
          indirect = row_ind$estimate[1],
          se = row_ind$se[1],
          z_value = row_ind$z_value[1],
          llci = ci_j["llci"],
          ulci = ci_j["ulci"],
          p = row_ind$p[1],
          p_fmt = fmt_p_pm(row_ind$p[1]),
          sig = sig_mark_pm(row_ind$p[1]),
          analysis_key = analysis_key,
          stringsAsFactors = FALSE
        )
        ridx <- ridx + 1L
      }
    }

    row_imm <- get_new_param(mm$imm_label[1])
    if (nrow(row_imm) == 1) {
      ci_j <- make_ci(row_imm$estimate[1], row_imm$se[1])
      indirect_rows[[ridx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = mm$mediator_var[1],
        mediator_label = mm$mediator_label[1],
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Index of moderated mediation",
        a = NA_real_,
        a_se = NA_real_,
        a_p = NA_real_,
        b = if (nrow(b_row) == 1) b_row$estimate[1] else NA_real_,
        b_se = if (nrow(b_row) == 1) b_row$se[1] else NA_real_,
        b_p = if (nrow(b_row) == 1) b_row$p[1] else NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_,
        indirect = row_imm$estimate[1],
        se = row_imm$se[1],
        z_value = row_imm$z_value[1],
        llci = ci_j["llci"],
        ulci = ci_j["ulci"],
        p = row_imm$p[1],
        p_fmt = fmt_p_pm(row_imm$p[1]),
        sig = sig_mark_pm(row_imm$p[1]),
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      ridx <- ridx + 1L
    }
  }

  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  conditional_df <- if (length(conditional_rows) > 0) do.call(rbind, conditional_rows) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_model14_mplus_data_pm <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle, process_settings) {
  prep_obj <- build_observed_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
  if (is.null(prep_obj)) return(NULL)

  prep_obj$export_data$mw_pm <- prep_obj$export_data$m_pm * prep_obj$export_data$w_pm
  export_cols <- c(names(prep_obj$export_data), "mw_pm")
  export_cols <- export_cols[!duplicated(export_cols)]
  prep_obj$export_data <- prep_obj$export_data[, export_cols, drop = FALSE]
  prep_obj
}

build_observed_model14_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)", "w_pm (bw)", "mw_pm (bint)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 14 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with second-stage moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    "  NEW(w_low w_mean w_high b_low b_mean b_high ind_low ind_mean ind_high imm);",
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  b_low = b1 + bint*w_low;",
    "  b_mean = b1 + bint*w_mean;",
    "  b_high = b1 + bint*w_high;",
    "  ind_low = a1*b_low;",
    "  ind_mean = a1*b_mean;",
    "  ind_high = a1*b_high;",
    "  imm = a1*bint;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model14 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model14_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm14obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model14_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 14 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L

  row_im <- find_row("intercepts", "m_pm")
  tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  row_a <- find_row("m_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_a, "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("m_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Mediator model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 400000 + i,
        model_component = "Mediator model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  core_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", effect = "Outcome model", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", effect = "Outcome model", label = lookup_label(mediator_var), sort_key = 1000030),
    list(section = "y_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", effect = "Outcome model", label = lookup_label(w_var), sort_key = 1000040),
    list(section = "y_on", export_name = "mw_pm", term = paste0(mediator_var, ":", w_var), effect_type = "Interaction", effect = "Outcome model", label = paste0(lookup_label(mediator_var), " x ", lookup_label(w_var)), sort_key = 1000050)
  )
  for (spec in core_specs) {
    row_i <- find_row(spec$section, spec$export_name)
    tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, spec$effect, spec$label, spec$label, spec$sort_key, "Outcome model")
    if (is.null(tmp_row)) next
    coef_rows[[idx]] <- tmp_row
    idx <- idx + 1L
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + i,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1])
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", "w_pm", "mw_pm", prep_obj$cov_export_names))

  model_summary <- rbind(
    data.frame(
      model_component = "Mediator model",
      outcome = mediator_var,
      outcome_label = lookup_label(mediator_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm("m_pm"),
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    ),
    data.frame(
      model_component = "Outcome model",
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm("y_pm"),
      adj_r2 = NA_real_,
      f_value = out_test$f_value %||% NA_real_,
      df1 = out_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = out_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      stringsAsFactors = FALSE
    )
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]

  a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  m_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mediator_var, , drop = FALSE]
  int_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(mediator_var, ":", w_var), paste0(w_var, ":", mediator_var)), , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]

  cond_specs <- data.frame(
    name = c("B_LOW", "B_MEAN", "B_HIGH"),
    ind_name = c("IND_LOW", "IND_MEAN", "IND_HIGH"),
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  indirect_rows <- list()
  idx_c <- 1L
  idx_i <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    row_beff <- get_new_param(cond_specs$name[i])
    row_ind <- get_new_param(cond_specs$ind_name[i])
    if (nrow(row_beff) == 1) {
      ci_i <- make_ci(row_beff$estimate[1], row_beff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        target_outcome = outcome_var,
        target_outcome_label = lookup_label(outcome_var),
        x_var = mediator_var,
        x_label = lookup_label(mediator_var),
        mediator = mediator_var,
        mediator_label = lookup_label(mediator_var),
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        class_num = i,
        class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_beff$estimate[1],
        se = row_beff$se[1],
        t_value = row_beff$z_value[1],
        p = row_beff$p[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        n = nobs,
        stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_ind) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[idx_i]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = mediator_var,
        mediator_label = lookup_label(mediator_var),
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = paste0("Conditional indirect effect at ", cond_specs$class_contrast[i]),
        a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_,
        a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_,
        a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
        b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_,
        b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_,
        b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_,
        indirect = row_ind$estimate[1],
        se = row_ind$se[1],
        z_value = row_ind$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_ind$p[1],
        p_fmt = fmt_p_pm(row_ind$p[1]),
        sig = sig_mark_pm(row_ind$p[1]),
        stringsAsFactors = FALSE
      )
      idx_i <- idx_i + 1L
    }
  }

  row_imm <- get_new_param("IMM")
  if (nrow(row_imm) == 1) {
    ci_i <- make_ci(row_imm$estimate[1], row_imm$se[1])
    indirect_rows[[idx_i]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      class_contrast = "Index of moderated mediation",
      a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_,
      a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_,
      a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
      b = if (nrow(int_row) == 1) int_row$estimate[1] else NA_real_,
      b_se = if (nrow(int_row) == 1) int_row$se[1] else NA_real_,
      b_p = if (nrow(int_row) == 1) int_row$p[1] else NA_real_,
      direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
      total = NA_real_,
      indirect = row_imm$estimate[1],
      se = row_imm$se[1],
      z_value = row_imm$z_value[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      p = row_imm$p[1],
      p_fmt = fmt_p_pm(row_imm$p[1]),
      sig = sig_mark_pm(row_imm$p[1]),
      stringsAsFactors = FALSE
    )
  }

  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_parallel_model14_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle, process_settings) {
  prep_obj <- build_observed_parallel_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
  if (is.null(prep_obj)) return(NULL)

  prep_obj$mediator_map$mw_export <- sprintf("mw%02d_pm", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$b_low_label <- paste0("bl", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$b_mean_label <- paste0("bm", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$b_high_label <- paste0("bh", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_low_label <- paste0("il", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_mean_label <- paste0("imn", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_high_label <- paste0("ih", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$imm_label <- paste0("imm", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$int_label <- paste0("biw", seq_len(nrow(prep_obj$mediator_map)))

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    prep_obj$export_data[[prep_obj$mediator_map$mw_export[i]]] <- prep_obj$export_data[[prep_obj$mediator_map$export_name[i]]] * prep_obj$export_data$w_pm
  }

  export_cols <- c(names(prep_obj$export_data), prep_obj$mediator_map$mw_export)
  export_cols <- export_cols[!duplicated(export_cols)]
  prep_obj$export_data <- prep_obj$export_data[, export_cols, drop = FALSE]
  prep_obj
}

build_observed_parallel_model14_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    med_rhs <- c(paste0("x_pm (", mm$a_label[1], ")"), prep_obj$cov_export_names)
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(model_lines, med_lines, paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");"))
  }

  out_rhs <- c(
    "x_pm (cp1)",
    "w_pm (bw)",
    paste0(prep_obj$mediator_map$export_name, " (", prep_obj$mediator_map$b_label, ")"),
    paste0(prep_obj$mediator_map$mw_export, " (", prep_obj$mediator_map$int_label, ")"),
    prep_obj$cov_export_names
  )
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_lines[length(out_lines)] <- paste0(out_lines[length(out_lines)], ";")
  model_lines <- c(model_lines, out_lines, "  [y_pm] (iy);")

  new_terms <- c(
    "w_low", "w_mean", "w_high",
    unlist(prep_obj$mediator_map[, c("b_low_label", "b_mean_label", "b_high_label", "ind_low_label", "ind_mean_label", "ind_high_label", "imm_label"), drop = FALSE], use.names = FALSE)
  )
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    paste0("  NEW(", paste(new_terms, collapse = " "), ");"),
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";")
  )
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", mm$b_low_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_low;"),
      paste0("  ", mm$b_mean_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_mean;"),
      paste0("  ", mm$b_high_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_high;"),
      paste0("  ", mm$ind_low_label[1], " = ", mm$a_label[1], "*", mm$b_low_label[1], ";"),
      paste0("  ", mm$ind_mean_label[1], " = ", mm$a_label[1], "*", mm$b_mean_label[1], ";"),
      paste0("  ", mm$ind_high_label[1], " = ", mm$a_label[1], "*", mm$b_high_label[1], ";"),
      paste0("  ", mm$imm_label[1], " = ", mm$a_label[1], "*", mm$int_label[1], ";")
    )
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 14 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with second-stage moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model14_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model14_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm14obspar",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_parallel_model14_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 14 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      mediator = mediator,
      mediator_label = mediator_label,
      table_block_key = table_block_key,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", mm$export_name[1]), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    tmp_row <- add_coef_row(x_var, find_row(paste0(tolower(mm$export_name[1]), "_on"), "x_pm", lhs = mm$export_name[1]), "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1])
        tmp_row <- add_coef_row(
          term = as.character(map_i$term[1]),
          row_obj = row_i,
          effect_type = as.character(map_i$effect_type[1]),
          effect = "Mediator model",
          term_label = as.character(map_i$term_label[1]),
          variable_label = as.character(map_i$variable_label[1]),
          sort_key = 400000 + j,
          model_component = "Mediator model",
          category_label = as.character(map_i$category_label[1]),
          reference_label = as.character(map_i$reference_label[1]),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          table_block_key = block_key,
          block_order = mm$block_order[1]
        )
        if (is.null(tmp_row)) next
        coef_rows[[idx]] <- tmp_row
        idx <- idx + 1L
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(x_var, find_row("y_on", "x_pm"), "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(w_var, find_row("y_on", "w_pm"), "Moderator", "Outcome model", lookup_label(w_var), lookup_label(w_var), 1000030, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    tmp_row <- add_coef_row(mm$mediator_var[1], find_row("y_on", mm$export_name[1]), "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000040 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    tmp_row <- add_coef_row(paste0(mm$mediator_var[1], ":", w_var), find_row("y_on", mm$mw_export[1]), "Interaction", "Outcome model", paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), 1000140 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + j,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1]),
        mediator = mediator_set_id,
        mediator_label = mediator_set_label,
        table_block_key = outcome_block_key,
        block_order = outcome_block_order
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(
      model_component = "Mediator model",
      outcome = mm$mediator_var[1],
      outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1],
      mediator_label = mm$mediator_label[1],
      moderator = w_var,
      moderator_label = lookup_label(w_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = r2_i,
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]),
      block_order = mm$block_order[1],
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
    sidx <- sidx + 1L
  }

  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "w_pm", prep_obj$mediator_map$export_name, prep_obj$mediator_map$mw_export, prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(
    model_component = "Outcome model",
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    mediator = mediator_set_id,
    mediator_label = mediator_set_label,
    moderator = w_var,
    moderator_label = lookup_label(w_var),
    x_var = x_var,
    x_label = lookup_label(x_var),
    target_outcome = outcome_var,
    target_outcome_label = lookup_label(outcome_var),
    n = nobs,
    r2 = r2_y,
    adj_r2 = NA_real_,
    f_value = out_test$f_value %||% NA_real_,
    df1 = out_test$df1 %||% NA_real_,
    df2 = NA_real_,
    p = out_test$p %||% NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key,
    block_order = outcome_block_order,
    analysis_key = analysis_key,
    stringsAsFactors = FALSE
  )
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  indirect_rows <- list()
  conditional_rows <- list()
  ridx <- 1L
  cidx <- 1L
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$mediator) == mm$mediator_var[1] & as.character(coef_df$term) == x_var, , drop = FALSE]
    int_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(mm$mediator_var[1], ":", w_var), paste0(w_var, ":", mm$mediator_var[1])), , drop = FALSE]
    probe_map <- data.frame(
      b_name = c(mm$b_low_label[1], mm$b_mean_label[1], mm$b_high_label[1]),
      ind_name = c(mm$ind_low_label[1], mm$ind_mean_label[1], mm$ind_high_label[1]),
      class_num = 1:3,
      class_contrast = c(
        paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
      ),
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(probe_map))) {
      row_beff <- get_new_param(probe_map$b_name[j])
      row_ind <- get_new_param(probe_map$ind_name[j])
      if (nrow(row_beff) == 1) {
        ci_j <- make_ci(row_beff$estimate[1], row_beff$se[1])
        conditional_rows[[cidx]] <- data.frame(
          outcome = outcome_var,
          outcome_label = lookup_label(outcome_var),
          target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var),
          x_var = mm$mediator_var[1],
          x_label = mm$mediator_label[1],
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          moderator = w_var,
          moderator_label = lookup_label(w_var),
          class_num = probe_map$class_num[j],
          class_contrast = probe_map$class_contrast[j],
          conditional_effect = row_beff$estimate[1],
          se = row_beff$se[1],
          t_value = row_beff$z_value[1],
          p = row_beff$p[1],
          llci = ci_j["llci"],
          ulci = ci_j["ulci"],
          n = nobs,
          analysis_key = analysis_key,
          stringsAsFactors = FALSE
        )
        cidx <- cidx + 1L
      }
      if (nrow(row_ind) == 1) {
        ci_j <- make_ci(row_ind$estimate[1], row_ind$se[1])
        indirect_rows[[ridx]] <- data.frame(
          outcome = outcome_var,
          outcome_label = lookup_label(outcome_var),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          moderator = w_var,
          moderator_label = lookup_label(w_var),
          x_var = x_var,
          x_label = lookup_label(x_var),
          class_contrast = paste0("Conditional indirect effect at ", probe_map$class_contrast[j]),
          a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_,
          a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_,
          a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
          b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_,
          b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_,
          b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
          direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
          total = NA_real_,
          indirect = row_ind$estimate[1],
          se = row_ind$se[1],
          z_value = row_ind$z_value[1],
          llci = ci_j["llci"],
          ulci = ci_j["ulci"],
          p = row_ind$p[1],
          p_fmt = fmt_p_pm(row_ind$p[1]),
          sig = sig_mark_pm(row_ind$p[1]),
          analysis_key = analysis_key,
          stringsAsFactors = FALSE
        )
        ridx <- ridx + 1L
      }
    }

    row_imm <- get_new_param(mm$imm_label[1])
    if (nrow(row_imm) == 1) {
      ci_j <- make_ci(row_imm$estimate[1], row_imm$se[1])
      indirect_rows[[ridx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = mm$mediator_var[1],
        mediator_label = mm$mediator_label[1],
        moderator = w_var,
        moderator_label = lookup_label(w_var),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Index of moderated mediation",
        a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_,
        a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_,
        a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
        b = if (nrow(int_row) == 1) int_row$estimate[1] else NA_real_,
        b_se = if (nrow(int_row) == 1) int_row$se[1] else NA_real_,
        b_p = if (nrow(int_row) == 1) int_row$p[1] else NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_,
        indirect = row_imm$estimate[1],
        se = row_imm$se[1],
        z_value = row_imm$z_value[1],
        llci = ci_j["llci"],
        ulci = ci_j["ulci"],
        p = row_imm$p[1],
        p_fmt = fmt_p_pm(row_imm$p[1]),
        sig = sig_mark_pm(row_imm$p[1]),
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      ridx <- ridx + 1L
    }
  }

  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  conditional_df <- if (length(conditional_rows) > 0) do.call(rbind, conditional_rows) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_model8_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)", "w_pm (aw)", "xw_pm (aint)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)", "w_pm (bw)", "xw_pm (cpint)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 8 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with first-stage and direct-effect moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    "  NEW(w_low w_mean w_high a_low a_mean a_high eff_low eff_mean eff_high ind_low ind_mean ind_high imm);",
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  a_low = a1 + aint*w_low;",
    "  a_mean = a1 + aint*w_mean;",
    "  a_high = a1 + aint*w_high;",
    "  eff_low = cp1 + cpint*w_low;",
    "  eff_mean = cp1 + cpint*w_mean;",
    "  eff_high = cp1 + cpint*w_high;",
    "  ind_low = a_low*b1;",
    "  ind_mean = a_mean*b1;",
    "  ind_high = a_high*b1;",
    "  imm = aint*b1;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model8 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm8obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model8_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 8 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label, sort_key = sort_key,
      model_component = model_component, stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "m_pm"), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  med_specs <- list(
    list(section = "m_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
    list(section = "m_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
    list(section = "m_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
  )
  for (spec in med_specs) {
    tmp_row <- add_coef_row(spec$term, find_row(spec$section, spec$export_name), spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("m_on", map_i$export_name[1]),
        as.character(map_i$effect_type[1]), "Mediator model", as.character(map_i$term_label[1]),
        as.character(map_i$variable_label[1]), 400000 + i, "Mediator model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  out_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", label = lookup_label(mediator_var), sort_key = 1000030),
    list(section = "y_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 1000040),
    list(section = "y_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 1000050)
  )
  for (spec in out_specs) {
    tmp_row <- add_coef_row(spec$term, find_row(spec$section, spec$export_name), spec$effect_type, "Outcome model", spec$label, spec$label, spec$sort_key, "Outcome model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("y_on", map_i$export_name[1]),
        as.character(map_i$effect_type[1]), "Outcome model", as.character(map_i$term_label[1]),
        as.character(map_i$variable_label[1]), 1400000 + i, "Outcome model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
  model_summary <- rbind(
    data.frame(model_component = "Mediator model", outcome = mediator_var, outcome_label = lookup_label(mediator_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("m_pm"), adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_, df1 = med_test$df1 %||% NA_real_, df2 = NA_real_,
      p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE),
    data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("y_pm"), adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_, df1 = out_test$df1 %||% NA_real_, df2 = NA_real_,
      p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE)
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mediator_var, , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  cpint_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var)), , drop = FALSE]
  aint_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$term) %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var)), , drop = FALSE]

  cond_specs <- data.frame(
    a_name = c("A_LOW", "A_MEAN", "A_HIGH"),
    eff_name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    ind_name = c("IND_LOW", "IND_MEAN", "IND_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  indirect_rows <- list()
  idx_c <- 1L
  idx_i <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    row_aeff <- get_new_param(cond_specs$a_name[i])
    row_eff <- get_new_param(cond_specs$eff_name[i])
    row_ind <- get_new_param(cond_specs$ind_name[i])
    if (nrow(row_aeff) == 1) {
      ci_i <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = mediator_var, outcome_label = lookup_label(mediator_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_aeff$estimate[1], se = row_aeff$se[1], t_value = row_aeff$z_value[1], p = row_aeff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_eff) == 1) {
      ci_i <- make_ci(row_eff$estimate[1], row_eff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_eff$estimate[1], se = row_eff$se[1], t_value = row_eff$z_value[1], p = row_eff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_ind) == 1 && nrow(b_row) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[idx_i]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
        class_contrast = paste0("Conditional indirect effect at ", cond_specs$class_contrast[i]),
        a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_, a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_, a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_,
        b = b_row$estimate[1], b_se = b_row$se[1], b_p = b_row$p[1],
        direct = if (nrow(row_eff) == 1) row_eff$estimate[1] else if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_,
        indirect = row_ind$estimate[1], se = row_ind$se[1], z_value = row_ind$z_value[1], llci = ci_i["llci"], ulci = ci_i["ulci"],
        p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]), sig = sig_mark_pm(row_ind$p[1]), stringsAsFactors = FALSE
      )
      idx_i <- idx_i + 1L
    }
  }
  row_imm <- get_new_param("IMM")
  if (nrow(row_imm) == 1) {
    ci_i <- make_ci(row_imm$estimate[1], row_imm$se[1])
    indirect_rows[[idx_i]] <- data.frame(
      outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
      moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
      class_contrast = "Index of moderated mediation",
      a = if (nrow(aint_row) == 1) aint_row$estimate[1] else NA_real_, a_se = if (nrow(aint_row) == 1) aint_row$se[1] else NA_real_, a_p = if (nrow(aint_row) == 1) aint_row$p[1] else NA_real_,
      b = b_row$estimate[1], b_se = b_row$se[1], b_p = b_row$p[1],
      direct = if (nrow(cpint_row) == 1) cpint_row$estimate[1] else if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_,
      indirect = row_imm$estimate[1], se = row_imm$se[1], z_value = row_imm$z_value[1], llci = ci_i["llci"], ulci = ci_i["ulci"],
      p = row_imm$p[1], p_fmt = fmt_p_pm(row_imm$p[1]), sig = sig_mark_pm(row_imm$p[1]), stringsAsFactors = FALSE
    )
  }

  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_parallel_model8_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_map <- prep_obj$mediator_map
  med_map$w_label <- paste0("aw", seq_len(nrow(med_map)))
  med_map$int_label <- paste0("aiw", seq_len(nrow(med_map)))
  med_map$a_low_label <- paste0("al", seq_len(nrow(med_map)))
  med_map$a_mean_label <- paste0("am", seq_len(nrow(med_map)))
  med_map$a_high_label <- paste0("ah", seq_len(nrow(med_map)))
  med_map$ind_low_label <- paste0("il", seq_len(nrow(med_map)))
  med_map$ind_mean_label <- paste0("imn", seq_len(nrow(med_map)))
  med_map$ind_high_label <- paste0("ih", seq_len(nrow(med_map)))
  med_map$imm_label <- paste0("imm", seq_len(nrow(med_map)))

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(med_map))) {
    mm <- med_map[i, , drop = FALSE]
    med_rhs <- c(
      paste0("x_pm (", mm$a_label[1], ")"),
      paste0("w_pm (", mm$w_label[1], ")"),
      paste0("xw_pm (", mm$int_label[1], ")"),
      prep_obj$cov_export_names
    )
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(model_lines, med_lines, paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");"))
  }

  out_rhs <- c("x_pm (cp1)", "w_pm (bw)", "xw_pm (cpint)", paste0(med_map$export_name, " (", med_map$b_label, ")"), prep_obj$cov_export_names)
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_lines[length(out_lines)] <- paste0(out_lines[length(out_lines)], ";")
  model_lines <- c(model_lines, out_lines, "  [y_pm] (iy);")

  new_terms <- c("w_low", "w_mean", "w_high", "eff_low", "eff_mean", "eff_high",
    unlist(med_map[, c("a_low_label", "a_mean_label", "a_high_label", "ind_low_label", "ind_mean_label", "ind_high_label", "imm_label"), drop = FALSE], use.names = FALSE))
  new_lines <- wrap_mplus_statement_pm(keyword = "NEW", values = new_terms)
  new_lines[1] <- sub("^\\s*NEW\\s*=\\s*", "  NEW(", new_lines[1])
  new_lines[length(new_lines)] <- sub(";$", ");", new_lines[length(new_lines)])
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    new_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + cpint*w_low;",
    "  eff_mean = cp1 + cpint*w_mean;",
    "  eff_high = cp1 + cpint*w_high;"
  )
  for (i in seq_len(nrow(med_map))) {
    mm <- med_map[i, , drop = FALSE]
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", mm$a_low_label[1], " = ", mm$a_label[1], " + ", mm$int_label[1], "*w_low;"),
      paste0("  ", mm$a_mean_label[1], " = ", mm$a_label[1], " + ", mm$int_label[1], "*w_mean;"),
      paste0("  ", mm$a_high_label[1], " = ", mm$a_label[1], " + ", mm$int_label[1], "*w_high;"),
      paste0("  ", mm$ind_low_label[1], " = ", mm$a_low_label[1], "*", mm$b_label[1], ";"),
      paste0("  ", mm$ind_mean_label[1], " = ", mm$a_mean_label[1], "*", mm$b_label[1], ";"),
      paste0("  ", mm$ind_high_label[1], " = ", mm$a_high_label[1], "*", mm$b_label[1], ";"),
      paste0("  ", mm$imm_label[1], " = ", mm$int_label[1], "*", mm$b_label[1], ";")
    )
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 8 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with first-stage and direct-effect moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model8_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  prep_obj$mediator_map$w_label <- paste0("aw", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$int_label <- paste0("aiw", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_low_label <- paste0("al", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_mean_label <- paste0("am", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_high_label <- paste0("ah", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_low_label <- paste0("il", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_mean_label <- paste0("imn", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_high_label <- paste0("ih", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$imm_label <- paste0("imm", seq_len(nrow(prep_obj$mediator_map)))

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm8obspar",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_parallel_model8_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 8 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }
  make_ci <- function(est, se) c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se), ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label,
      sort_key = sort_key, model_component = model_component, mediator = mediator, mediator_label = mediator_label,
      table_block_key = table_block_key, block_order = block_order, stringsAsFactors = FALSE)
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", mm$export_name[1]), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    med_specs <- list(
      list(export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
      list(export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
      list(export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
    )
    for (spec in med_specs) {
      tmp_row <- add_coef_row(spec$term, find_row(paste0(tolower(mm$export_name[1]), "_on"), spec$export_name, lhs = mm$export_name[1]),
        spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model",
        mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1]),
          as.character(map_i$effect_type[1]), "Mediator model", as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + j,
          "Mediator model", as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
        if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  out_specs <- list(
    list(export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 1000020),
    list(export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 1000030),
    list(export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 1000040)
  )
  for (spec in out_specs) {
    tmp_row <- add_coef_row(spec$term, find_row("y_on", spec$export_name), spec$effect_type, "Outcome model", spec$label, spec$label, spec$sort_key, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    tmp_row <- add_coef_row(mm$mediator_var[1], find_row("y_on", mm$export_name[1]), "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000100 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("y_on", map_i$export_name[1]),
        as.character(map_i$effect_type[1]), "Outcome model", as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + j,
        "Outcome model", as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mediator_set_id,
        mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(model_component = "Mediator model", outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
      r2 = r2_i, adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_, df1 = med_test$df1 %||% NA_real_, df2 = NA_real_,
      p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]), block_order = mm$block_order[1], analysis_key = analysis_key,
      stringsAsFactors = FALSE)
    sidx <- sidx + 1L
  }
  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", prep_obj$mediator_map$export_name, "w_pm", "xw_pm", prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
    mediator = mediator_set_id, mediator_label = mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var),
    x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
    r2 = r2_y, adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_, df1 = out_test$df1 %||% NA_real_, df2 = NA_real_,
    p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key, block_order = outcome_block_order, analysis_key = analysis_key, stringsAsFactors = FALSE)
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  cpint_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var)), , drop = FALSE]
  conditional_rows <- list()
  indirect_rows <- list()
  ridx <- 1L
  cidx <- 1L
  probe_map <- data.frame(
    eff_name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(nrow(probe_map))) {
    row_eff <- get_new_param(probe_map$eff_name[j])
    if (nrow(row_eff) != 1) next
    ci_j <- make_ci(row_eff$estimate[1], row_eff$se[1])
    conditional_rows[[cidx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var), x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_set_id,
      mediator_label = mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map$class_num[j],
      class_contrast = probe_map$class_contrast[j], conditional_effect = row_eff$estimate[1], se = row_eff$se[1], t_value = row_eff$z_value[1],
      p = row_eff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
    cidx <- cidx + 1L
  }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    aint_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$mediator) == mm$mediator_var[1] & as.character(coef_df$term) %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var)), , drop = FALSE]
    b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mm$mediator_var[1], , drop = FALSE]
    probe_map_i <- data.frame(
      a_name = c(mm$a_low_label[1], mm$a_mean_label[1], mm$a_high_label[1]),
      ind_name = c(mm$ind_low_label[1], mm$ind_mean_label[1], mm$ind_high_label[1]),
      class_num = 1:3,
      class_contrast = probe_map$class_contrast,
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(probe_map_i))) {
      row_aeff <- get_new_param(probe_map_i$a_name[j])
      row_ind <- get_new_param(probe_map_i$ind_name[j])
      if (nrow(row_aeff) == 1) {
        ci_j <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
        conditional_rows[[cidx]] <- data.frame(outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1], target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var), x_var = x_var, x_label = lookup_label(x_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map_i$class_num[j],
          class_contrast = probe_map_i$class_contrast[j], conditional_effect = row_aeff$estimate[1], se = row_aeff$se[1], t_value = row_aeff$z_value[1],
          p = row_aeff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
        cidx <- cidx + 1L
      }
      if (nrow(row_ind) == 1 && nrow(b_row) == 1) {
        ci_j <- make_ci(row_ind$estimate[1], row_ind$se[1])
        indirect_rows[[ridx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
          class_contrast = paste0("Conditional indirect effect at ", probe_map_i$class_contrast[j]),
          a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_, a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_, a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_,
          b = b_row$estimate[1], b_se = b_row$se[1], b_p = b_row$p[1],
          direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_, indirect = row_ind$estimate[1], se = row_ind$se[1],
          z_value = row_ind$z_value[1], llci = ci_j["llci"], ulci = ci_j["ulci"], p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]),
          sig = sig_mark_pm(row_ind$p[1]), analysis_key = analysis_key, stringsAsFactors = FALSE)
        ridx <- ridx + 1L
      }
    }
    row_imm <- get_new_param(mm$imm_label[1])
    if (nrow(row_imm) == 1) {
      ci_j <- make_ci(row_imm$estimate[1], row_imm$se[1])
      indirect_rows[[ridx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mm$mediator_var[1],
        mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
        class_contrast = "Index of moderated mediation",
        a = if (nrow(aint_row) == 1) aint_row$estimate[1] else NA_real_, a_se = if (nrow(aint_row) == 1) aint_row$se[1] else NA_real_, a_p = if (nrow(aint_row) == 1) aint_row$p[1] else NA_real_,
        b = b_row$estimate[1], b_se = b_row$se[1], b_p = b_row$p[1],
        direct = if (nrow(cpint_row) == 1) cpint_row$estimate[1] else if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_, indirect = row_imm$estimate[1], se = row_imm$se[1], z_value = row_imm$z_value[1], llci = ci_j["llci"], ulci = ci_j["ulci"],
        p = row_imm$p[1], p_fmt = fmt_p_pm(row_imm$p[1]), sig = sig_mark_pm(row_imm$p[1]), analysis_key = analysis_key, stringsAsFactors = FALSE)
      ridx <- ridx + 1L
    }
  }

  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  conditional_df <- if (length(conditional_rows) > 0) do.call(rbind, conditional_rows) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_observed_model15_mplus_data_pm <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle, process_settings) {
  build_observed_model14_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
}

build_observed_model15_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)", "w_pm (bw)", "xw_pm (cpint)", "mw_pm (bint)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 15 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with moderated direct and second-stage effects by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    "  NEW(w_low w_mean w_high eff_low eff_mean eff_high b_low b_mean b_high ind_low ind_mean ind_high imm);",
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + cpint*w_low;",
    "  eff_mean = cp1 + cpint*w_mean;",
    "  eff_high = cp1 + cpint*w_high;",
    "  b_low = b1 + bint*w_low;",
    "  b_mean = b1 + bint*w_mean;",
    "  b_high = b1 + bint*w_high;",
    "  ind_low = a1*b_low;",
    "  ind_mean = a1*b_mean;",
    "  ind_high = a1*b_high;",
    "  imm = a1*bint;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model15 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model15_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm15obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model15_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 15 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label, sort_key = sort_key,
      model_component = model_component, stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "m_pm"), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(x_var, find_row("m_on", "x_pm"), "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("m_on", map_i$export_name[1]),
        as.character(map_i$effect_type[1]), "Mediator model", as.character(map_i$term_label[1]),
        as.character(map_i$variable_label[1]), 400000 + i, "Mediator model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  core_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", label = lookup_label(mediator_var), sort_key = 1000030),
    list(section = "y_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 1000040),
    list(section = "y_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 1000050),
    list(section = "y_on", export_name = "mw_pm", term = paste0(mediator_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(mediator_var), " x ", lookup_label(w_var)), sort_key = 1000060)
  )
  for (spec in core_specs) {
    tmp_row <- add_coef_row(spec$term, find_row(spec$section, spec$export_name), spec$effect_type, "Outcome model", spec$label, spec$label, spec$sort_key, "Outcome model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("y_on", map_i$export_name[1]),
        as.character(map_i$effect_type[1]), "Outcome model", as.character(map_i$term_label[1]),
        as.character(map_i$variable_label[1]), 1400000 + i, "Outcome model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", "w_pm", "xw_pm", "mw_pm", prep_obj$cov_export_names))
  model_summary <- rbind(
    data.frame(model_component = "Mediator model", outcome = mediator_var, outcome_label = lookup_label(mediator_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("m_pm"), adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_, df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_, p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE),
    data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("y_pm"), adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_, df1 = out_test$df1 %||% NA_real_,
      df2 = NA_real_, p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE)
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  cpint_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var)), , drop = FALSE]
  int_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(mediator_var, ":", w_var), paste0(w_var, ":", mediator_var)), , drop = FALSE]

  cond_specs <- data.frame(
    eff_name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    b_name = c("B_LOW", "B_MEAN", "B_HIGH"),
    ind_name = c("IND_LOW", "IND_MEAN", "IND_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  indirect_rows <- list()
  idx_c <- 1L
  idx_i <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    row_eff <- get_new_param(cond_specs$eff_name[i])
    row_beff <- get_new_param(cond_specs$b_name[i])
    row_ind <- get_new_param(cond_specs$ind_name[i])
    if (nrow(row_eff) == 1) {
      ci_i <- make_ci(row_eff$estimate[1], row_eff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_eff$estimate[1], se = row_eff$se[1], t_value = row_eff$z_value[1], p = row_eff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_beff) == 1) {
      ci_i <- make_ci(row_beff$estimate[1], row_beff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = mediator_var, x_label = lookup_label(mediator_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_beff$estimate[1], se = row_beff$se[1], t_value = row_beff$z_value[1], p = row_beff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_ind) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[idx_i]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
        class_contrast = paste0("Conditional indirect effect at ", cond_specs$class_contrast[i]),
        a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_, a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_, a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
        b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_, b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_, b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
        direct = if (nrow(row_eff) == 1) row_eff$estimate[1] else if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_,
        indirect = row_ind$estimate[1], se = row_ind$se[1], z_value = row_ind$z_value[1], llci = ci_i["llci"], ulci = ci_i["ulci"],
        p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]), sig = sig_mark_pm(row_ind$p[1]), stringsAsFactors = FALSE
      )
      idx_i <- idx_i + 1L
    }
  }

  row_imm <- get_new_param("IMM")
  if (nrow(row_imm) == 1) {
    ci_i <- make_ci(row_imm$estimate[1], row_imm$se[1])
    indirect_rows[[idx_i]] <- data.frame(
      outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
      moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
      class_contrast = "Index of moderated mediation",
      a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_, a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_, a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
      b = if (nrow(int_row) == 1) int_row$estimate[1] else NA_real_, b_se = if (nrow(int_row) == 1) int_row$se[1] else NA_real_, b_p = if (nrow(int_row) == 1) int_row$p[1] else NA_real_,
      direct = if (nrow(cpint_row) == 1) cpint_row$estimate[1] else if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_,
      indirect = row_imm$estimate[1], se = row_imm$se[1], z_value = row_imm$z_value[1], llci = ci_i["llci"], ulci = ci_i["ulci"],
      p = row_imm$p[1], p_fmt = fmt_p_pm(row_imm$p[1]), sig = sig_mark_pm(row_imm$p[1]), stringsAsFactors = FALSE
    )
  }

  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_parallel_model15_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle, process_settings) {
  build_observed_parallel_model14_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
}

build_observed_parallel_model15_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(data = prep_obj$export_data, path = paths$data_file, missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999)
  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c("VARIABLE:", wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)), wrap_mplus_statement_pm("USEVARIABLES", usevars_vec))
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))
  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) "  TYPE = COMPLEX;" else "  TYPE = GENERAL;"

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    med_rhs <- c(paste0("x_pm (", mm$a_label[1], ")"), prep_obj$cov_export_names)
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(model_lines, med_lines, paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");"))
  }

  out_rhs <- c(
    "x_pm (cp1)",
    "w_pm (bw)",
    "xw_pm (cpint)",
    paste0(prep_obj$mediator_map$export_name, " (", prep_obj$mediator_map$b_label, ")"),
    paste0(prep_obj$mediator_map$mw_export, " (", prep_obj$mediator_map$int_label, ")"),
    prep_obj$cov_export_names
  )
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_lines[length(out_lines)] <- paste0(out_lines[length(out_lines)], ";")
  model_lines <- c(model_lines, out_lines, "  [y_pm] (iy);")

  new_terms <- c(
    "w_low", "w_mean", "w_high", "eff_low", "eff_mean", "eff_high",
    unlist(prep_obj$mediator_map[, c("b_low_label", "b_mean_label", "b_high_label", "ind_low_label", "ind_mean_label", "ind_high_label", "imm_label"), drop = FALSE], use.names = FALSE)
  )
  new_lines <- wrap_mplus_statement_pm(keyword = "NEW", values = new_terms)
  new_lines[1] <- sub("^\\s*NEW\\s*=\\s*", "  NEW(", new_lines[1])
  new_lines[length(new_lines)] <- sub(";$", ");", new_lines[length(new_lines)])
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    new_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + cpint*w_low;",
    "  eff_mean = cp1 + cpint*w_mean;",
    "  eff_high = cp1 + cpint*w_high;"
  )
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", mm$b_low_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_low;"),
      paste0("  ", mm$b_mean_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_mean;"),
      paste0("  ", mm$b_high_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_high;"),
      paste0("  ", mm$ind_low_label[1], " = ", mm$a_label[1], "*", mm$b_low_label[1], ";"),
      paste0("  ", mm$ind_mean_label[1], " = ", mm$a_label[1], "*", mm$b_mean_label[1], ";"),
      paste0("  ", mm$ind_high_label[1], " = ", mm$a_label[1], "*", mm$b_high_label[1], ";"),
      paste0("  ", mm$imm_label[1], " = ", mm$a_label[1], "*", mm$int_label[1], ";")
    )
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 15 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with moderated direct and second-stage effects by ", w_var, ";"),
    "DATA:", paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"), variable_lines, "ANALYSIS:", type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines, constraint_lines, "OUTPUT:", "  SAMPSTAT CINTERVAL;"
  )
  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model15_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model15_mplus_data_pm(
    df = df, outcome_var = outcome_var, x_var = x_var, mediator_vars = mediator_vars, w_var = w_var,
    covariates = covariates, survey_bundle = survey_bundle, process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(sanitize_token_pm(DATASET_ID), "pm15obspar", sanitize_token_pm(outcome_var), sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""), sanitize_token_pm(w_var), sep = "_")
  paths <- build_observed_parallel_model15_input_pm(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var)

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 15 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }
  make_ci <- function(est, se) c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se), ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label,
      sort_key = sort_key, model_component = model_component, mediator = mediator, mediator_label = mediator_label,
      table_block_key = table_block_key, block_order = block_order, stringsAsFactors = FALSE)
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", mm$export_name[1]), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    tmp_row <- add_coef_row(x_var, find_row(paste0(tolower(mm$export_name[1]), "_on"), "x_pm", lhs = mm$export_name[1]), "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1]),
          as.character(map_i$effect_type[1]), "Mediator model", as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + j,
          "Mediator model", as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
        if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(x_var, find_row("y_on", "x_pm"), "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(w_var, find_row("y_on", "w_pm"), "Moderator", "Outcome model", lookup_label(w_var), lookup_label(w_var), 1000030, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(paste0(x_var, ":", w_var), find_row("y_on", "xw_pm"), "Interaction", "Outcome model", paste0(lookup_label(x_var), " x ", lookup_label(w_var)), paste0(lookup_label(x_var), " x ", lookup_label(w_var)), 1000040, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    tmp_row <- add_coef_row(mm$mediator_var[1], find_row("y_on", mm$export_name[1]), "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000100 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    tmp_row <- add_coef_row(paste0(mm$mediator_var[1], ":", w_var), find_row("y_on", mm$mw_export[1]), "Interaction", "Outcome model", paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), 1000200 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("y_on", map_i$export_name[1]),
        as.character(map_i$effect_type[1]), "Outcome model", as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + j,
        "Outcome model", as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mediator_set_id,
        mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(model_component = "Mediator model", outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
      r2 = r2_i, adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_, df1 = med_test$df1 %||% NA_real_, df2 = NA_real_,
      p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]), block_order = mm$block_order[1], analysis_key = analysis_key,
      stringsAsFactors = FALSE)
    sidx <- sidx + 1L
  }
  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "w_pm", "xw_pm", prep_obj$mediator_map$export_name, prep_obj$mediator_map$mw_export, prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
    mediator = mediator_set_id, mediator_label = mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var),
    x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
    r2 = r2_y, adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_, df1 = out_test$df1 %||% NA_real_, df2 = NA_real_,
    p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key, block_order = outcome_block_order, analysis_key = analysis_key, stringsAsFactors = FALSE)
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  cpint_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(x_var, ":", w_var), paste0(w_var, ":", x_var)), , drop = FALSE]
  indirect_rows <- list()
  conditional_rows <- list()
  ridx <- 1L
  cidx <- 1L

  probe_map <- data.frame(
    eff_name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(nrow(probe_map))) {
    row_eff <- get_new_param(probe_map$eff_name[j])
    if (nrow(row_eff) != 1) next
    ci_j <- make_ci(row_eff$estimate[1], row_eff$se[1])
    conditional_rows[[cidx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var), x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_set_id,
      mediator_label = mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map$class_num[j],
      class_contrast = probe_map$class_contrast[j], conditional_effect = row_eff$estimate[1], se = row_eff$se[1], t_value = row_eff$z_value[1],
      p = row_eff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
    cidx <- cidx + 1L
  }

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$mediator) == mm$mediator_var[1] & as.character(coef_df$term) == x_var, , drop = FALSE]
    int_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) %in% c(paste0(mm$mediator_var[1], ":", w_var), paste0(w_var, ":", mm$mediator_var[1])), , drop = FALSE]
    probe_map_i <- data.frame(
      b_name = c(mm$b_low_label[1], mm$b_mean_label[1], mm$b_high_label[1]),
      ind_name = c(mm$ind_low_label[1], mm$ind_mean_label[1], mm$ind_high_label[1]),
      class_num = 1:3,
      class_contrast = probe_map$class_contrast,
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(probe_map_i))) {
      row_beff <- get_new_param(probe_map_i$b_name[j])
      row_ind <- get_new_param(probe_map_i$ind_name[j])
      if (nrow(row_beff) == 1) {
        ci_j <- make_ci(row_beff$estimate[1], row_beff$se[1])
        conditional_rows[[cidx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var), x_var = mm$mediator_var[1], x_label = mm$mediator_label[1], mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map_i$class_num[j],
          class_contrast = probe_map_i$class_contrast[j], conditional_effect = row_beff$estimate[1], se = row_beff$se[1], t_value = row_beff$z_value[1],
          p = row_beff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
        cidx <- cidx + 1L
      }
      if (nrow(row_ind) == 1) {
        ci_j <- make_ci(row_ind$estimate[1], row_ind$se[1])
        indirect_rows[[ridx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
          class_contrast = paste0("Conditional indirect effect at ", probe_map_i$class_contrast[j]),
          a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_, a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_, a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
          b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_, b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_, b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
          direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_, indirect = row_ind$estimate[1], se = row_ind$se[1],
          z_value = row_ind$z_value[1], llci = ci_j["llci"], ulci = ci_j["ulci"], p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]),
          sig = sig_mark_pm(row_ind$p[1]), analysis_key = analysis_key, stringsAsFactors = FALSE)
        ridx <- ridx + 1L
      }
    }
    row_imm <- get_new_param(mm$imm_label[1])
    if (nrow(row_imm) == 1) {
      ci_j <- make_ci(row_imm$estimate[1], row_imm$se[1])
      indirect_rows[[ridx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mm$mediator_var[1],
        mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
        class_contrast = "Index of moderated mediation",
        a = if (nrow(a_row) == 1) a_row$estimate[1] else NA_real_, a_se = if (nrow(a_row) == 1) a_row$se[1] else NA_real_, a_p = if (nrow(a_row) == 1) a_row$p[1] else NA_real_,
        b = if (nrow(int_row) == 1) int_row$estimate[1] else NA_real_, b_se = if (nrow(int_row) == 1) int_row$se[1] else NA_real_, b_p = if (nrow(int_row) == 1) int_row$p[1] else NA_real_,
        direct = if (nrow(cpint_row) == 1) cpint_row$estimate[1] else if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_, indirect = row_imm$estimate[1], se = row_imm$se[1], z_value = row_imm$z_value[1], llci = ci_j["llci"], ulci = ci_j["ulci"],
        p = row_imm$p[1], p_fmt = fmt_p_pm(row_imm$p[1]), sig = sig_mark_pm(row_imm$p[1]), analysis_key = analysis_key, stringsAsFactors = FALSE)
      ridx <- ridx + 1L
    }
  }

  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  conditional_df <- if (length(conditional_rows) > 0) do.call(rbind, conditional_rows) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_model58_mplus_data_pm <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle, process_settings) {
  prep_obj <- build_observed_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
  if (is.null(prep_obj)) return(NULL)
  prep_obj$export_data$mw_pm <- prep_obj$export_data$m_pm * prep_obj$export_data$w_pm
  prep_obj
}

build_observed_model58_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_rhs <- c("x_pm (a1)", "w_pm (aw)", "xw_pm (aint)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)", "w_pm (bw)", "mw_pm (bint)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  new_lines <- wrap_mplus_statement_pm(
    keyword = "NEW",
    values = c("w_low", "w_mean", "w_high", "a_low", "a_mean", "a_high", "b_low", "b_mean", "b_high", "ind_low", "ind_mean", "ind_high")
  )
  new_lines[1] <- sub("^\\s*NEW\\s*=\\s*", "  NEW(", new_lines[1])
  new_lines[length(new_lines)] <- sub(";$", ");", new_lines[length(new_lines)])

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 58 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with first- and second-stage moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    new_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  a_low = a1 + aint*w_low;",
    "  a_mean = a1 + aint*w_mean;",
    "  a_high = a1 + aint*w_high;",
    "  b_low = b1 + bint*w_low;",
    "  b_mean = b1 + bint*w_mean;",
    "  b_high = b1 + bint*w_high;",
    "  ind_low = a_low*b_low;",
    "  ind_mean = a_mean*b_mean;",
    "  ind_high = a_high*b_high;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model58 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model58_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm58obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model58_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 58 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label, sort_key = sort_key,
      model_component = model_component, stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  row_im <- find_row("intercepts", "m_pm")
  tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  med_specs <- list(
    list(section = "m_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
    list(section = "m_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
    list(section = "m_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
  )
  for (spec in med_specs) {
    row_i <- find_row(spec$section, spec$export_name)
    tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("m_on", map_i$export_name[1])
      tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Mediator model",
        as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + i, "Mediator model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  out_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", label = lookup_label(mediator_var), sort_key = 1000030),
    list(section = "y_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 1000040),
    list(section = "y_on", export_name = "mw_pm", term = paste0(mediator_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(mediator_var), " x ", lookup_label(w_var)), sort_key = 1000050)
  )
  for (spec in out_specs) {
    row_i <- find_row(spec$section, spec$export_name)
    tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Outcome model", spec$label, spec$label, spec$sort_key, "Outcome model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Outcome model",
        as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + i, "Outcome model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", "w_pm", "mw_pm", prep_obj$cov_export_names))

  model_summary <- rbind(
    data.frame(model_component = "Mediator model", outcome = mediator_var, outcome_label = lookup_label(mediator_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("m_pm"), adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_, df2 = NA_real_, p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_, bootstrap_ci = NA_character_, estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE),
    data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("y_pm"), adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_,
      df1 = out_test$df1 %||% NA_real_, df2 = NA_real_, p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_, bootstrap_ci = NA_character_, estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE)
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  cond_specs <- data.frame(
    a_name = c("A_LOW", "A_MEAN", "A_HIGH"),
    b_name = c("B_LOW", "B_MEAN", "B_HIGH"),
    ind_name = c("IND_LOW", "IND_MEAN", "IND_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  indirect_rows <- list()
  idx_c <- 1L
  idx_i <- 1L
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  for (i in seq_len(nrow(cond_specs))) {
    row_aeff <- get_new_param(cond_specs$a_name[i])
    row_beff <- get_new_param(cond_specs$b_name[i])
    row_ind <- get_new_param(cond_specs$ind_name[i])
    if (nrow(row_aeff) == 1) {
      ci_i <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = mediator_var, outcome_label = lookup_label(mediator_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_aeff$estimate[1], se = row_aeff$se[1], t_value = row_aeff$z_value[1], p = row_aeff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_beff) == 1) {
      ci_i <- make_ci(row_beff$estimate[1], row_beff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = mediator_var, x_label = lookup_label(mediator_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_beff$estimate[1], se = row_beff$se[1], t_value = row_beff$z_value[1], p = row_beff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_ind) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[idx_i]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
        class_contrast = paste0("Conditional indirect effect at ", cond_specs$class_contrast[i]),
        a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_,
        a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_,
        a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_,
        b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_,
        b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_,
        b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = NA_real_, indirect = row_ind$estimate[1], se = row_ind$se[1], z_value = row_ind$z_value[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]), sig = sig_mark_pm(row_ind$p[1]),
        stringsAsFactors = FALSE
      )
      idx_i <- idx_i + 1L
    }
  }

  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_parallel_model58_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle, process_settings) {
  prep_obj <- build_observed_parallel_model5_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
  if (is.null(prep_obj)) return(NULL)
  prep_obj$mediator_map$mw_export <- sprintf("mw%02d_pm", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_low_label <- paste0("al", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_mean_label <- paste0("am", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$a_high_label <- paste0("ah", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$b_low_label <- paste0("bl", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$b_mean_label <- paste0("bm", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$b_high_label <- paste0("bh", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_low_label <- paste0("il", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_mean_label <- paste0("imn", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$ind_high_label <- paste0("ih", seq_len(nrow(prep_obj$mediator_map)))
  prep_obj$mediator_map$int_label <- paste0("biw", seq_len(nrow(prep_obj$mediator_map)))
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    prep_obj$export_data[[prep_obj$mediator_map$mw_export[i]]] <- prep_obj$export_data[[prep_obj$mediator_map$export_name[i]]] * prep_obj$export_data$w_pm
  }
  prep_obj
}

build_observed_parallel_model58_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(data = prep_obj$export_data, path = paths$data_file, missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999)
  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c("VARIABLE:", wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)), wrap_mplus_statement_pm("USEVARIABLES", usevars_vec))
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))
  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) "  TYPE = COMPLEX;" else "  TYPE = GENERAL;"

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    med_rhs <- c(paste0("x_pm (", mm$a_label[1], ")"), "w_pm (aw)", "xw_pm (aint)", prep_obj$cov_export_names)
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(model_lines, med_lines, paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");"))
  }
  out_rhs <- c("x_pm (cp1)", "w_pm (bw)", paste0(prep_obj$mediator_map$export_name, " (", prep_obj$mediator_map$b_label, ")"), paste0(prep_obj$mediator_map$mw_export, " (", prep_obj$mediator_map$int_label, ")"), prep_obj$cov_export_names)
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_lines[length(out_lines)] <- paste0(out_lines[length(out_lines)], ";")
  model_lines <- c(model_lines, out_lines, "  [y_pm] (iy);")

  new_terms <- c("w_low", "w_mean", "w_high",
    unlist(prep_obj$mediator_map[, c("a_low_label", "a_mean_label", "a_high_label", "b_low_label", "b_mean_label", "b_high_label", "ind_low_label", "ind_mean_label", "ind_high_label"), drop = FALSE], use.names = FALSE))
  new_lines <- wrap_mplus_statement_pm(keyword = "NEW", values = new_terms)
  new_lines[1] <- sub("^\\s*NEW\\s*=\\s*", "  NEW(", new_lines[1])
  new_lines[length(new_lines)] <- sub(";$", ");", new_lines[length(new_lines)])
  constraint_lines <- c("MODEL CONSTRAINT:", new_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"))
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", mm$a_low_label[1], " = ", mm$a_label[1], " + aint*w_low;"),
      paste0("  ", mm$a_mean_label[1], " = ", mm$a_label[1], " + aint*w_mean;"),
      paste0("  ", mm$a_high_label[1], " = ", mm$a_label[1], " + aint*w_high;"),
      paste0("  ", mm$b_low_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_low;"),
      paste0("  ", mm$b_mean_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_mean;"),
      paste0("  ", mm$b_high_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_high;"),
      paste0("  ", mm$ind_low_label[1], " = ", mm$a_low_label[1], "*", mm$b_low_label[1], ";"),
      paste0("  ", mm$ind_mean_label[1], " = ", mm$a_mean_label[1], "*", mm$b_mean_label[1], ";"),
      paste0("  ", mm$ind_high_label[1], " = ", mm$a_high_label[1], "*", mm$b_high_label[1], ";")
    )
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 58 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with first- and second-stage moderation by ", w_var, ";"),
    "DATA:", paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"), variable_lines, "ANALYSIS:", type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines, constraint_lines, "OUTPUT:", "  SAMPSTAT CINTERVAL;"
  )
  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model58_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model58_mplus_data_pm(
    df = df, outcome_var = outcome_var, x_var = x_var, mediator_vars = mediator_vars, w_var = w_var,
    covariates = covariates, survey_bundle = survey_bundle, process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(sanitize_token_pm(DATASET_ID), "pm58obspar", sanitize_token_pm(outcome_var), sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""), sanitize_token_pm(w_var), sep = "_")
  paths <- build_observed_parallel_model58_input_pm(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var)

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 58 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }
  make_ci <- function(est, se) c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se), ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label,
      sort_key = sort_key, model_component = model_component, mediator = mediator, mediator_label = mediator_label,
      table_block_key = table_block_key, block_order = block_order, stringsAsFactors = FALSE)
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", mm$export_name[1]), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    med_specs <- list(
      list(export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
      list(export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
      list(export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
    )
    for (spec in med_specs) {
      row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), spec$export_name, lhs = mm$export_name[1])
      tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1])
        tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Mediator model",
          as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + j, "Mediator model",
          as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
        if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(x_var, find_row("y_on", "x_pm"), "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(w_var, find_row("y_on", "w_pm"), "Moderator", "Outcome model", lookup_label(w_var), lookup_label(w_var), 1000030, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    tmp_row <- add_coef_row(mm$mediator_var[1], find_row("y_on", mm$export_name[1]), "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000040 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    tmp_row <- add_coef_row(paste0(mm$mediator_var[1], ":", w_var), find_row("y_on", mm$mw_export[1]), "Interaction", "Outcome model", paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), 1000140 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Outcome model",
        as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + j, "Outcome model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mediator_set_id,
        mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(model_component = "Mediator model", outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
      r2 = r2_i, adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_, df1 = med_test$df1 %||% NA_real_, df2 = NA_real_,
      p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]), block_order = mm$block_order[1], analysis_key = analysis_key,
      stringsAsFactors = FALSE)
    sidx <- sidx + 1L
  }
  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "w_pm", prep_obj$mediator_map$export_name, prep_obj$mediator_map$mw_export, prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
    mediator = prep_obj$mediator_set_id, mediator_label = prep_obj$mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var),
    x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
    r2 = r2_y, adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_, df1 = out_test$df1 %||% NA_real_, df2 = NA_real_,
    p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key, block_order = outcome_block_order, analysis_key = analysis_key, stringsAsFactors = FALSE)
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  indirect_rows <- list()
  conditional_rows <- list()
  ridx <- 1L
  cidx <- 1L
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    probe_map <- data.frame(
      a_name = c(mm$a_low_label[1], mm$a_mean_label[1], mm$a_high_label[1]),
      b_name = c(mm$b_low_label[1], mm$b_mean_label[1], mm$b_high_label[1]),
      ind_name = c(mm$ind_low_label[1], mm$ind_mean_label[1], mm$ind_high_label[1]),
      class_num = 1:3,
      class_contrast = c(
        paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
        paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
      ),
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(probe_map))) {
      row_aeff <- get_new_param(probe_map$a_name[j])
      row_beff <- get_new_param(probe_map$b_name[j])
      row_ind <- get_new_param(probe_map$ind_name[j])
      if (nrow(row_aeff) == 1) {
        ci_j <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
        conditional_rows[[cidx]] <- data.frame(outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1], target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var), x_var = x_var, x_label = lookup_label(x_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map$class_num[j],
          class_contrast = probe_map$class_contrast[j], conditional_effect = row_aeff$estimate[1], se = row_aeff$se[1], t_value = row_aeff$z_value[1],
          p = row_aeff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
        cidx <- cidx + 1L
      }
      if (nrow(row_beff) == 1) {
        ci_j <- make_ci(row_beff$estimate[1], row_beff$se[1])
        conditional_rows[[cidx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var), x_var = mm$mediator_var[1], x_label = mm$mediator_label[1], mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map$class_num[j],
          class_contrast = probe_map$class_contrast[j], conditional_effect = row_beff$estimate[1], se = row_beff$se[1], t_value = row_beff$z_value[1],
          p = row_beff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
        cidx <- cidx + 1L
      }
      if (nrow(row_ind) == 1) {
        ci_j <- make_ci(row_ind$estimate[1], row_ind$se[1])
        indirect_rows[[ridx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
          class_contrast = paste0("Conditional indirect effect at ", probe_map$class_contrast[j]),
          a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_, a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_,
          a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_, b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_,
          b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_, b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
          direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_, total = NA_real_, indirect = row_ind$estimate[1], se = row_ind$se[1],
          z_value = row_ind$z_value[1], llci = ci_j["llci"], ulci = ci_j["ulci"], p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]),
          sig = sig_mark_pm(row_ind$p[1]), analysis_key = analysis_key, stringsAsFactors = FALSE)
        ridx <- ridx + 1L
      }
    }
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  conditional_df <- if (length(conditional_rows) > 0) do.call(rbind, conditional_rows) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_model59_mplus_data_pm <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle, process_settings) {
  build_observed_model58_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
}

build_observed_model59_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))
  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) "  TYPE = COMPLEX;" else "  TYPE = GENERAL;"

  med_rhs <- c("x_pm (a1)", "w_pm (aw)", "xw_pm (aint)")
  if (length(prep_obj$cov_export_names) > 0) med_rhs <- c(med_rhs, prep_obj$cov_export_names)
  med_model_lines <- c("  m_pm ON", paste0("    ", med_rhs))
  med_model_lines[length(med_model_lines)] <- paste0(med_model_lines[length(med_model_lines)], ";")

  out_rhs <- c("x_pm (cp1)", "m_pm (b1)", "w_pm (bw)", "xw_pm (cpint)", "mw_pm (bint)")
  if (length(prep_obj$cov_export_names) > 0) out_rhs <- c(out_rhs, prep_obj$cov_export_names)
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")

  new_lines <- wrap_mplus_statement_pm(
    keyword = "NEW",
    values = c("w_low", "w_mean", "w_high", "eff_low", "eff_mean", "eff_high", "a_low", "a_mean", "a_high", "b_low", "b_mean", "b_high", "ind_low", "ind_mean", "ind_high")
  )
  new_lines[1] <- sub("^\\s*NEW\\s*=\\s*", "  NEW(", new_lines[1])
  new_lines[length(new_lines)] <- sub(";$", ");", new_lines[length(new_lines)])

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 59 for ", outcome_var, " from ", x_var, " through ", mediator_var, " with first-, second-stage, and direct-effect moderation by ", w_var, ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    "MODEL:",
    med_model_lines,
    out_model_lines,
    "  [m_pm] (im);",
    "  [y_pm] (iy);",
    "MODEL CONSTRAINT:",
    new_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + cpint*w_low;",
    "  eff_mean = cp1 + cpint*w_mean;",
    "  eff_high = cp1 + cpint*w_high;",
    "  a_low = a1 + aint*w_low;",
    "  a_mean = a1 + aint*w_mean;",
    "  a_high = a1 + aint*w_high;",
    "  b_low = b1 + bint*w_low;",
    "  b_mean = b1 + bint*w_mean;",
    "  b_high = b1 + bint*w_high;",
    "  ind_low = a_low*b_low;",
    "  ind_mean = a_mean*b_mean;",
    "  ind_high = a_high*b_high;",
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model59 <- function(df, outcome_var, x_var, mediator_var, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_model59_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_var = mediator_var,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm59obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    sanitize_token_pm(mediator_var),
    sanitize_token_pm(w_var),
    sep = "_"
  )
  paths <- build_observed_model59_input_pm(prep_obj, model_tag, outcome_var, x_var, mediator_var, w_var)

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed Model 59 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediator = ", mediator_var, ", w = ", w_var,
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }
  make_ci <- function(est, se) c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se), ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "") {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label, sort_key = sort_key,
      model_component = model_component, stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "m_pm"), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  med_specs <- list(
    list(section = "m_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
    list(section = "m_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
    list(section = "m_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
  )
  for (spec in med_specs) {
    tmp_row <- add_coef_row(spec$term, find_row(spec$section, spec$export_name), spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("m_on", map_i$export_name[1]), as.character(map_i$effect_type[1]), "Mediator model",
        as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + i, "Mediator model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model")
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  out_specs <- list(
    list(section = "y_on", export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 1000020),
    list(section = "y_on", export_name = "m_pm", term = mediator_var, effect_type = "Mediator", label = lookup_label(mediator_var), sort_key = 1000030),
    list(section = "y_on", export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 1000040),
    list(section = "y_on", export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 1000050),
    list(section = "y_on", export_name = "mw_pm", term = paste0(mediator_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(mediator_var), " x ", lookup_label(w_var)), sort_key = 1000060)
  )
  for (spec in out_specs) {
    tmp_row <- add_coef_row(spec$term, find_row(spec$section, spec$export_name), spec$effect_type, "Outcome model", spec$label, spec$label, spec$sort_key, "Outcome model")
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (i in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[i, , drop = FALSE]
      tmp_row <- add_coef_row(as.character(map_i$term[1]), find_row("y_on", map_i$export_name[1]), as.character(map_i$effect_type[1]), "Outcome model",
        as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + i, "Outcome model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]))
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", mediator_var, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", lookup_label(mediator_var), lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$mediator <- mediator_var
    coef_df$mediator_label <- lookup_label(mediator_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  med_test <- compute_step_test_pm("m_pm", c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "m_pm", "w_pm", "xw_pm", "mw_pm", prep_obj$cov_export_names))

  model_summary <- rbind(
    data.frame(model_component = "Mediator model", outcome = mediator_var, outcome_label = lookup_label(mediator_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("m_pm"), adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_, df2 = NA_real_, p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_, bootstrap_ci = NA_character_, estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE),
    data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
      mediator = mediator_var, mediator_label = lookup_label(mediator_var), moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
      n = nobs, r2 = get_r2_pm("y_pm"), adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_,
      df1 = out_test$df1 %||% NA_real_, df2 = NA_real_, p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_, bootstrap_ci = NA_character_, estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design", stringsAsFactors = FALSE)
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  cond_specs <- data.frame(
    eff_name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    a_name = c("A_LOW", "A_MEAN", "A_HIGH"),
    b_name = c("B_LOW", "B_MEAN", "B_HIGH"),
    ind_name = c("IND_LOW", "IND_MEAN", "IND_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )

  conditional_list <- list()
  indirect_rows <- list()
  idx_c <- 1L
  idx_i <- 1L
  for (i in seq_len(nrow(cond_specs))) {
    row_eff <- get_new_param(cond_specs$eff_name[i])
    row_aeff <- get_new_param(cond_specs$a_name[i])
    row_beff <- get_new_param(cond_specs$b_name[i])
    row_ind <- get_new_param(cond_specs$ind_name[i])
    if (nrow(row_eff) == 1) {
      ci_i <- make_ci(row_eff$estimate[1], row_eff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_eff$estimate[1], se = row_eff$se[1], t_value = row_eff$z_value[1], p = row_eff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_aeff) == 1) {
      ci_i <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = mediator_var, outcome_label = lookup_label(mediator_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_aeff$estimate[1], se = row_aeff$se[1], t_value = row_aeff$z_value[1], p = row_aeff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_beff) == 1) {
      ci_i <- make_ci(row_beff$estimate[1], row_beff$se[1])
      conditional_list[[idx_c]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var),
        x_var = mediator_var, x_label = lookup_label(mediator_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), class_num = cond_specs$class_num[i], class_contrast = cond_specs$class_contrast[i],
        conditional_effect = row_beff$estimate[1], se = row_beff$se[1], t_value = row_beff$z_value[1], p = row_beff$p[1],
        llci = ci_i["llci"], ulci = ci_i["ulci"], n = nobs, stringsAsFactors = FALSE
      )
      idx_c <- idx_c + 1L
    }
    if (nrow(row_ind) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[idx_i]] <- data.frame(
        outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mediator_var, mediator_label = lookup_label(mediator_var),
        moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
        class_contrast = paste0("Conditional indirect effect at ", cond_specs$class_contrast[i]),
        a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_, a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_, a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_,
        b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_, b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_, b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
        direct = if (nrow(row_eff) == 1) row_eff$estimate[1] else NA_real_, total = NA_real_,
        indirect = row_ind$estimate[1], se = row_ind$se[1], z_value = row_ind$z_value[1], llci = ci_i["llci"], ulci = ci_i["ulci"],
        p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]), sig = sig_mark_pm(row_ind$p[1]), stringsAsFactors = FALSE
      )
      idx_i <- idx_i + 1L
    }
  }

  conditional_df <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_parallel_model59_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle, process_settings) {
  prep_obj <- build_observed_parallel_model58_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    w_var = w_var,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = process_settings
  )
  if (is.null(prep_obj)) return(NULL)
  prep_obj
}

build_observed_parallel_model59_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(data = prep_obj$export_data, path = paths$data_file, missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999)
  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c("VARIABLE:", wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)), wrap_mplus_statement_pm("USEVARIABLES", usevars_vec))
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))
  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) "  TYPE = COMPLEX;" else "  TYPE = GENERAL;"

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    med_rhs <- c(paste0("x_pm (", mm$a_label[1], ")"), "w_pm (aw)", "xw_pm (aint)", prep_obj$cov_export_names)
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", mm$export_name[1], " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(model_lines, med_lines, paste0("  [", mm$export_name[1], "] (", mm$intercept_label[1], ");"))
  }
  out_rhs <- c("x_pm (cp1)", "w_pm (bw)", "xw_pm (cpint)", paste0(prep_obj$mediator_map$export_name, " (", prep_obj$mediator_map$b_label, ")"), paste0(prep_obj$mediator_map$mw_export, " (", prep_obj$mediator_map$int_label, ")"), prep_obj$cov_export_names)
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_lines[length(out_lines)] <- paste0(out_lines[length(out_lines)], ";")
  model_lines <- c(model_lines, out_lines, "  [y_pm] (iy);")

  new_terms <- c(
    "w_low", "w_mean", "w_high", "eff_low", "eff_mean", "eff_high",
    unlist(prep_obj$mediator_map[, c("a_low_label", "a_mean_label", "a_high_label", "b_low_label", "b_mean_label", "b_high_label", "ind_low_label", "ind_mean_label", "ind_high_label"), drop = FALSE], use.names = FALSE)
  )
  new_lines <- wrap_mplus_statement_pm(keyword = "NEW", values = new_terms)
  new_lines[1] <- sub("^\\s*NEW\\s*=\\s*", "  NEW(", new_lines[1])
  new_lines[length(new_lines)] <- sub(";$", ");", new_lines[length(new_lines)])
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    new_lines,
    paste0("  w_low = ", sprintf("%.10f", prep_obj$low_centered), ";"),
    paste0("  w_mean = ", sprintf("%.10f", prep_obj$mean_centered), ";"),
    paste0("  w_high = ", sprintf("%.10f", prep_obj$high_centered), ";"),
    "  eff_low = cp1 + cpint*w_low;",
    "  eff_mean = cp1 + cpint*w_mean;",
    "  eff_high = cp1 + cpint*w_high;"
  )
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", mm$a_low_label[1], " = ", mm$a_label[1], " + aint*w_low;"),
      paste0("  ", mm$a_mean_label[1], " = ", mm$a_label[1], " + aint*w_mean;"),
      paste0("  ", mm$a_high_label[1], " = ", mm$a_label[1], " + aint*w_high;"),
      paste0("  ", mm$b_low_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_low;"),
      paste0("  ", mm$b_mean_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_mean;"),
      paste0("  ", mm$b_high_label[1], " = ", mm$b_label[1], " + ", mm$int_label[1], "*w_high;"),
      paste0("  ", mm$ind_low_label[1], " = ", mm$a_low_label[1], "*", mm$b_low_label[1], ";"),
      paste0("  ", mm$ind_mean_label[1], " = ", mm$a_mean_label[1], "*", mm$b_mean_label[1], ";"),
      paste0("  ", mm$ind_high_label[1], " = ", mm$a_high_label[1], "*", mm$b_high_label[1], ";")
    )
  }

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed Model 59 parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), " with first-, second-stage, and direct-effect moderation by ", w_var, ";"),
    "DATA:", paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"), variable_lines, "ANALYSIS:", type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines, constraint_lines, "OUTPUT:", "  SAMPSTAT CINTERVAL;"
  )
  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model59_parallel <- function(df, outcome_var, x_var, mediator_vars, w_var, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_model59_mplus_data_pm(
    df = df, outcome_var = outcome_var, x_var = x_var, mediator_vars = mediator_vars, w_var = w_var,
    covariates = covariates, survey_bundle = survey_bundle, process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(sanitize_token_pm(DATASET_ID), "pm59obspar", sanitize_token_pm(outcome_var), sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""), sanitize_token_pm(w_var), sep = "_")
  paths <- build_observed_parallel_model59_input_pm(prep_obj, model_tag, outcome_var, x_var, mediator_vars, w_var)

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)
  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel Model 59 run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ", w = ", w_var, ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[as.character(parsed_main$section) == section & as.character(parsed_main$name) == toupper(name), , drop = FALSE]
    if (!is.null(lhs) && "lhs" %in% names(hit)) hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }
  make_ci <- function(est, se) c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se), ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(term = term, estimate = row_obj$estimate[1], se = row_obj$se[1], t_value = row_obj$z_value[1], p = row_obj$p[1],
      llci = ci_i["llci"], ulci = ci_i["ulci"], effect_type = effect_type, effect = effect, term_label = term_label,
      variable_label = variable_label, category_label = category_label, reference_label = reference_label,
      sort_key = sort_key, model_component = model_component, mediator = mediator, mediator_label = mediator_label,
      table_block_key = table_block_key, block_order = block_order, stringsAsFactors = FALSE)
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), w_var, sep = " | ")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", mm$export_name[1]), "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    med_specs <- list(
      list(export_name = "x_pm", term = x_var, effect_type = "Independent variable", label = lookup_label(x_var), sort_key = 20),
      list(export_name = "w_pm", term = w_var, effect_type = "Moderator", label = lookup_label(w_var), sort_key = 30),
      list(export_name = "xw_pm", term = paste0(x_var, ":", w_var), effect_type = "Interaction", label = paste0(lookup_label(x_var), " x ", lookup_label(w_var)), sort_key = 40)
    )
    for (spec in med_specs) {
      row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), spec$export_name, lhs = mm$export_name[1])
      tmp_row <- add_coef_row(spec$term, row_i, spec$effect_type, "Mediator model", spec$label, spec$label, spec$sort_key, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1])
        tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Mediator model",
          as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + j, "Mediator model",
          as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
        if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_id <- prep_obj$mediator_set_id
  mediator_set_label <- prep_obj$mediator_set_label
  tmp_row <- add_coef_row("(Intercept)", find_row("intercepts", "y_pm"), "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(x_var, find_row("y_on", "x_pm"), "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(w_var, find_row("y_on", "w_pm"), "Moderator", "Outcome model", lookup_label(w_var), lookup_label(w_var), 1000030, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  tmp_row <- add_coef_row(paste0(x_var, ":", w_var), find_row("y_on", "xw_pm"), "Interaction", "Outcome model", paste0(lookup_label(x_var), " x ", lookup_label(w_var)), paste0(lookup_label(x_var), " x ", lookup_label(w_var)), 1000040, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    tmp_row <- add_coef_row(mm$mediator_var[1], find_row("y_on", mm$export_name[1]), "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000100 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    tmp_row <- add_coef_row(paste0(mm$mediator_var[1], ":", w_var), find_row("y_on", mm$mw_export[1]), "Interaction", "Outcome model", paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), paste0(mm$mediator_label[1], " x ", lookup_label(w_var)), 1000200 + i, "Outcome model", mediator = mediator_set_id, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Outcome model",
        as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + j, "Outcome model",
        as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mediator_set_id,
        mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
      if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$moderator <- w_var
    coef_df$moderator_label <- lookup_label(w_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", "w_pm", "xw_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(model_component = "Mediator model", outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var),
      x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
      r2 = r2_i, adj_r2 = NA_real_, f_value = med_test$f_value %||% NA_real_, df1 = med_test$df1 %||% NA_real_, df2 = NA_real_,
      p = med_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]), block_order = mm$block_order[1], analysis_key = analysis_key,
      stringsAsFactors = FALSE)
    sidx <- sidx + 1L
  }
  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", "w_pm", "xw_pm", prep_obj$mediator_map$export_name, prep_obj$mediator_map$mw_export, prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(model_component = "Outcome model", outcome = outcome_var, outcome_label = lookup_label(outcome_var),
    mediator = prep_obj$mediator_set_id, mediator_label = prep_obj$mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var),
    x_var = x_var, x_label = lookup_label(x_var), target_outcome = outcome_var, target_outcome_label = lookup_label(outcome_var), n = nobs,
    r2 = r2_y, adj_r2 = NA_real_, f_value = out_test$f_value %||% NA_real_, df1 = out_test$df1 %||% NA_real_, df2 = NA_real_,
    p = out_test$p %||% NA_real_, bootstrap_enabled = FALSE, bootstrap_n = NA_real_, bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR", variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key, block_order = outcome_block_order, analysis_key = analysis_key, stringsAsFactors = FALSE)
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]
  indirect_rows <- list()
  conditional_rows <- list()
  ridx <- 1L
  cidx <- 1L
  probe_map <- data.frame(
    eff_name = c("EFF_LOW", "EFF_MEAN", "EFF_HIGH"),
    class_num = 1:3,
    class_contrast = c(
      paste0(lookup_label(w_var), " = M - 1 SD (", formatC(prep_obj$low_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M (", formatC(prep_obj$mean_raw, format = "f", digits = 2), ")"),
      paste0(lookup_label(w_var), " = M + 1 SD (", formatC(prep_obj$high_raw, format = "f", digits = 2), ")")
    ),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(nrow(probe_map))) {
    row_eff <- get_new_param(probe_map$eff_name[j])
    if (nrow(row_eff) != 1) next
    ci_j <- make_ci(row_eff$estimate[1], row_eff$se[1])
    conditional_rows[[cidx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var), x_var = x_var, x_label = lookup_label(x_var), mediator = mediator_set_id,
      mediator_label = mediator_set_label, moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map$class_num[j],
      class_contrast = probe_map$class_contrast[j], conditional_effect = row_eff$estimate[1], se = row_eff$se[1], t_value = row_eff$z_value[1],
      p = row_eff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
    cidx <- cidx + 1L
  }

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    probe_map_i <- data.frame(
      a_name = c(mm$a_low_label[1], mm$a_mean_label[1], mm$a_high_label[1]),
      b_name = c(mm$b_low_label[1], mm$b_mean_label[1], mm$b_high_label[1]),
      ind_name = c(mm$ind_low_label[1], mm$ind_mean_label[1], mm$ind_high_label[1]),
      class_num = 1:3,
      class_contrast = probe_map$class_contrast,
      stringsAsFactors = FALSE
    )
    for (j in seq_len(nrow(probe_map_i))) {
      row_aeff <- get_new_param(probe_map_i$a_name[j])
      row_beff <- get_new_param(probe_map_i$b_name[j])
      row_ind <- get_new_param(probe_map_i$ind_name[j])
      row_eff <- get_new_param(probe_map$eff_name[j])
      if (nrow(row_aeff) == 1) {
        ci_j <- make_ci(row_aeff$estimate[1], row_aeff$se[1])
        conditional_rows[[cidx]] <- data.frame(outcome = mm$mediator_var[1], outcome_label = mm$mediator_label[1], target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var), x_var = x_var, x_label = lookup_label(x_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map_i$class_num[j],
          class_contrast = probe_map_i$class_contrast[j], conditional_effect = row_aeff$estimate[1], se = row_aeff$se[1], t_value = row_aeff$z_value[1],
          p = row_aeff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
        cidx <- cidx + 1L
      }
      if (nrow(row_beff) == 1) {
        ci_j <- make_ci(row_beff$estimate[1], row_beff$se[1])
        conditional_rows[[cidx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), target_outcome = outcome_var,
          target_outcome_label = lookup_label(outcome_var), x_var = mm$mediator_var[1], x_label = mm$mediator_label[1], mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), class_num = probe_map_i$class_num[j],
          class_contrast = probe_map_i$class_contrast[j], conditional_effect = row_beff$estimate[1], se = row_beff$se[1], t_value = row_beff$z_value[1],
          p = row_beff$p[1], llci = ci_j["llci"], ulci = ci_j["ulci"], n = nobs, analysis_key = analysis_key, stringsAsFactors = FALSE)
        cidx <- cidx + 1L
      }
      if (nrow(row_ind) == 1) {
        ci_j <- make_ci(row_ind$estimate[1], row_ind$se[1])
        indirect_rows[[ridx]] <- data.frame(outcome = outcome_var, outcome_label = lookup_label(outcome_var), mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1], moderator = w_var, moderator_label = lookup_label(w_var), x_var = x_var, x_label = lookup_label(x_var),
          class_contrast = paste0("Conditional indirect effect at ", probe_map_i$class_contrast[j]),
          a = if (nrow(row_aeff) == 1) row_aeff$estimate[1] else NA_real_, a_se = if (nrow(row_aeff) == 1) row_aeff$se[1] else NA_real_,
          a_p = if (nrow(row_aeff) == 1) row_aeff$p[1] else NA_real_, b = if (nrow(row_beff) == 1) row_beff$estimate[1] else NA_real_,
          b_se = if (nrow(row_beff) == 1) row_beff$se[1] else NA_real_, b_p = if (nrow(row_beff) == 1) row_beff$p[1] else NA_real_,
          direct = if (nrow(row_eff) == 1) row_eff$estimate[1] else NA_real_, total = NA_real_, indirect = row_ind$estimate[1], se = row_ind$se[1],
          z_value = row_ind$z_value[1], llci = ci_j["llci"], ulci = ci_j["ulci"], p = row_ind$p[1], p_fmt = fmt_p_pm(row_ind$p[1]),
          sig = sig_mark_pm(row_ind$p[1]), analysis_key = analysis_key, stringsAsFactors = FALSE)
        ridx <- ridx + 1L
      }
    }
  }

  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  conditional_df <- if (length(conditional_rows) > 0) do.call(rbind, conditional_rows) else data.frame()
  if (nrow(conditional_df) > 0) {
    conditional_df$p_fmt <- fmt_p_pm(conditional_df$p)
    conditional_df$sig <- sig_mark_pm(conditional_df$p)
  }
  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    conditional_effects = conditional_df,
    run_info = data.frame(model_tag = model_tag, inp_file = paths$inp_file, out_file = paths$out_file, data_file = paths$data_file, stringsAsFactors = FALSE)
  )
}

build_observed_parallel_mediation_mplus_data_pm <- function(df, outcome_var, x_var, mediator_vars, covariates, survey_bundle, process_settings) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  mediator_vars <- unique(as.character(mediator_vars))
  mediator_vars <- mediator_vars[!is.na(mediator_vars) & nzchar(mediator_vars)]
  if (!length(mediator_vars)) return(NULL)

  needed <- unique(c(
    outcome_var,
    x_var,
    mediator_vars,
    covariates,
    survey_bundle$weight_var,
    survey_bundle$strata_var,
    survey_bundle$cluster_var,
    survey_bundle$subset_var,
    process_settings$id_var
  ))
  needed <- needed[!is.na(needed) & nzchar(needed) & needed %in% names(df)]
  dat <- df[, needed, drop = FALSE]
  if (!all(c(outcome_var, x_var, mediator_vars) %in% names(dat))) return(NULL)

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  for (mv in mediator_vars) dat[[mv]] <- safe_num_pm(dat[[mv]])

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_term_map <- cov_prep$term_map
  cov_rhs <- cov_prep$rhs_terms

  subset_idx <- make_subset_index_pm(dat, survey_bundle)
  if (length(subset_idx) == 0) subset_idx <- rep(TRUE, nrow(dat))

  design_terms <- c(survey_bundle$weight_var, survey_bundle$strata_var, survey_bundle$cluster_var)
  design_terms <- design_terms[!is.na(design_terms) & nzchar(design_terms) & design_terms %in% names(dat)]
  model_terms <- c(outcome_var, x_var, mediator_vars, cov_rhs)
  model_terms <- model_terms[model_terms %in% names(dat)]
  cc_idx <- stats::complete.cases(dat[, unique(c(model_terms, design_terms)), drop = FALSE])

  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    ww <- safe_num_pm(dat[[survey_bundle$weight_var]])
    cc_idx <- cc_idx & !is.na(ww) & ww > 0
  }

  analysis_idx <- cc_idx & subset_idx
  if (!any(analysis_idx)) return(NULL)

  cov_export_names <- if (length(cov_rhs) > 0) sprintf("c%02d", seq_along(cov_rhs)) else character(0)
  for (i in seq_along(cov_rhs)) dat[[cov_export_names[i]]] <- safe_num_pm(dat[[cov_rhs[i]]])
  if (length(cov_export_names) > 0 && nrow(cov_term_map) > 0) {
    cov_term_map$export_name <- cov_export_names[match(cov_term_map$term, cov_rhs)]
  } else {
    cov_term_map$export_name <- character(0)
  }
  if (length(cov_export_names) > 0) {
    keep_cov <- vapply(cov_export_names, function(nm) {
      if (!nm %in% names(dat)) return(FALSE)
      x <- safe_num_pm(dat[[nm]])
      ux <- unique(stats::na.omit(x))
      length(ux) > 1L
    }, logical(1))
    cov_export_names <- cov_export_names[keep_cov]
    if (nrow(cov_term_map) > 0) {
      cov_term_map <- cov_term_map[as.character(cov_term_map$export_name) %in% cov_export_names, , drop = FALSE]
    }
  }

  mediator_map <- data.frame(
    mediator_var = mediator_vars,
    mediator_label = vapply(mediator_vars, lookup_label, character(1)),
    export_name = sprintf("m%02d_pm", seq_along(mediator_vars)),
    a_label = paste0("a", seq_along(mediator_vars)),
    b_label = paste0("b", seq_along(mediator_vars)),
    intercept_label = paste0("im", seq_along(mediator_vars)),
    indirect_label = paste0("ind", seq_along(mediator_vars)),
    block_order = seq_along(mediator_vars),
    stringsAsFactors = FALSE
  )

  dat$x_pm <- safe_num_pm(dat[[x_var]])
  for (i in seq_len(nrow(mediator_map))) {
    dat[[mediator_map$export_name[i]]] <- safe_num_pm(dat[[mediator_map$mediator_var[i]]])
  }
  dat$y_pm <- safe_num_pm(dat[[outcome_var]])

  has_subpop_spec <- nzchar(as.character(survey_bundle$subset_var %||% "")[1]) ||
    nzchar(as.character(survey_bundle$subset_expr %||% "")[1])
  use_subpopulation <- isTRUE(process_settings$use_subpopulation_in_mplus) && isTRUE(has_subpop_spec)
  if (use_subpopulation) {
    dat$sb_pm <- as.integer(analysis_idx)
  } else {
    dat <- dat[analysis_idx, , drop = FALSE]
    analysis_idx <- rep(TRUE, nrow(dat))
  }

  design_map <- list()
  if (!is.null(survey_bundle$weight_var) && survey_bundle$weight_var %in% names(dat)) {
    dat$wt_pm <- safe_num_pm(dat[[survey_bundle$weight_var]])
    design_map$weight_var <- "wt_pm"
  } else {
    design_map$weight_var <- NULL
  }
  if (!is.null(survey_bundle$strata_var) && survey_bundle$strata_var %in% names(dat)) {
    dat$st_pm <- safe_num_pm(dat[[survey_bundle$strata_var]])
    design_map$strata_var <- "st_pm"
  } else {
    design_map$strata_var <- NULL
  }
  if (!is.null(survey_bundle$cluster_var) && survey_bundle$cluster_var %in% names(dat)) {
    dat$cl_pm <- safe_num_pm(dat[[survey_bundle$cluster_var]])
    design_map$cluster_var <- "cl_pm"
  } else {
    design_map$cluster_var <- NULL
  }

  export_names <- c(
    if (use_subpopulation) "sb_pm" else character(0),
    design_map$weight_var %||% character(0),
    design_map$strata_var %||% character(0),
    design_map$cluster_var %||% character(0),
    "x_pm",
    mediator_map$export_name,
    "y_pm",
    cov_export_names
  )
  export_names <- export_names[!is.na(export_names) & nzchar(export_names)]

  list(
    export_data = dat[, export_names, drop = FALSE],
    cov_term_map = cov_term_map,
    cov_export_names = cov_export_names,
    use_subpopulation = use_subpopulation,
    design_map = design_map,
    analysis_n = sum(analysis_idx, na.rm = TRUE),
    x_var = x_var,
    x_label = lookup_label(x_var),
    mediator_map = mediator_map,
    mediator_set_label = paste(mediator_map$mediator_label, collapse = ", "),
    outcome_var = outcome_var,
    outcome_label = lookup_label(outcome_var)
  )
}

build_indirect_difference_map_pm <- function(mediator_map) {
  if (!is.data.frame(mediator_map) || nrow(mediator_map) < 2) {
    return(data.frame())
  }
  cmb <- utils::combn(seq_len(nrow(mediator_map)), 2)
  out <- vector("list", ncol(cmb))
  for (k in seq_len(ncol(cmb))) {
    i <- cmb[1, k]
    j <- cmb[2, k]
    out[[k]] <- data.frame(
      i = i,
      j = j,
      mediator1 = mediator_map$mediator_var[i],
      mediator1_label = mediator_map$mediator_label[i],
      mediator2 = mediator_map$mediator_var[j],
      mediator2_label = mediator_map$mediator_label[j],
      param_name = paste0("d", i, "_", j),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

build_observed_parallel_mediation_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  model_lines <- c("MODEL:")
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    med_export <- prep_obj$mediator_map$export_name[i]
    med_rhs <- c(
      paste0("x_pm (", prep_obj$mediator_map$a_label[i], ")"),
      prep_obj$cov_export_names
    )
    med_rhs <- med_rhs[nzchar(med_rhs)]
    med_lines <- c(paste0("  ", med_export, " ON"), paste0("    ", med_rhs))
    med_lines[length(med_lines)] <- paste0(med_lines[length(med_lines)], ";")
    model_lines <- c(
      model_lines,
      med_lines,
      paste0("  [", med_export, "] (", prep_obj$mediator_map$intercept_label[i], ");")
    )
  }

  out_rhs <- c(
    "x_pm (cp1)",
    paste0(prep_obj$mediator_map$export_name, " (", prep_obj$mediator_map$b_label, ")"),
    prep_obj$cov_export_names
  )
  out_rhs <- out_rhs[nzchar(out_rhs)]
  out_model_lines <- c("  y_pm ON", paste0("    ", out_rhs))
  out_model_lines[length(out_model_lines)] <- paste0(out_model_lines[length(out_model_lines)], ";")
  model_lines <- c(model_lines, out_model_lines, "  [y_pm] (iy);")

  indirect_names <- prep_obj$mediator_map$indirect_label
  diff_map <- build_indirect_difference_map_pm(prep_obj$mediator_map)
  diff_names <- diff_map$param_name %||% character(0)
  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    paste0("  NEW(", paste(c(indirect_names, diff_names, "ind_total", "total"), collapse = " "), ");")
  )
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    constraint_lines <- c(
      constraint_lines,
      paste0("  ", prep_obj$mediator_map$indirect_label[i], " = ", prep_obj$mediator_map$a_label[i], "*", prep_obj$mediator_map$b_label[i], ";")
    )
  }
  if (is.data.frame(diff_map) && nrow(diff_map) > 0) {
    for (k in seq_len(nrow(diff_map))) {
      constraint_lines <- c(
        constraint_lines,
        paste0("  ", diff_map$param_name[k], " = ", prep_obj$mediator_map$indirect_label[diff_map$i[k]], " - ", prep_obj$mediator_map$indirect_label[diff_map$j[k]], ";")
      )
    }
  }
  constraint_lines <- c(
    constraint_lines,
    paste0("  ind_total = ", paste(indirect_names, collapse = " + "), ";"),
    "  total = cp1 + ind_total;"
  )

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed parallel mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = ", "), ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model4_parallel <- function(df, outcome_var, x_var, mediator_vars, covariates, survey_bundle) {
  prep_obj <- build_observed_parallel_mediation_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm4obspar",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    sep = "_"
  )
  paths <- build_observed_parallel_mediation_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed parallel mediation run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = ", "),
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) {
      return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    }
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) {
      dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    }
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))

    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }

    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      mediator = mediator,
      mediator_label = mediator_label,
      table_block_key = table_block_key,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  analysis_key <- paste(outcome_var, x_var, paste(prep_obj$mediator_map$mediator_var, collapse = "|"), sep = " | ")

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    block_key <- paste0(analysis_key, " | med | ", mm$mediator_var[1])
    row_im <- find_row("intercepts", mm$export_name[1])
    tmp_row <- add_coef_row("(Intercept)", row_im, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

    row_a <- find_row(paste0(tolower(mm$export_name[1]), "_on"), "x_pm", lhs = mm$export_name[1])
    tmp_row <- add_coef_row(x_var, row_a, "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model", mediator = mm$mediator_var[1], mediator_label = mm$mediator_label[1], table_block_key = block_key, block_order = mm$block_order[1])
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(mm$export_name[1]), "_on"), map_i$export_name[1], lhs = mm$export_name[1])
        tmp_row <- add_coef_row(
          term = as.character(map_i$term[1]),
          row_obj = row_i,
          effect_type = as.character(map_i$effect_type[1]),
          effect = "Mediator model",
          term_label = as.character(map_i$term_label[1]),
          variable_label = as.character(map_i$variable_label[1]),
          sort_key = 400000 + j,
          model_component = "Mediator model",
          category_label = as.character(map_i$category_label[1]),
          reference_label = as.character(map_i$reference_label[1]),
          mediator = mm$mediator_var[1],
          mediator_label = mm$mediator_label[1],
          table_block_key = block_key,
          block_order = mm$block_order[1]
        )
        if (is.null(tmp_row)) next
        coef_rows[[idx]] <- tmp_row
        idx <- idx + 1L
      }
    }
  }

  outcome_block_order <- nrow(prep_obj$mediator_map) + 1L
  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_label <- prep_obj$mediator_set_label

  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = paste(prep_obj$mediator_map$mediator_var, collapse = "|"), mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

  row_cp <- find_row("y_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_cp, "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = paste(prep_obj$mediator_map$mediator_var, collapse = "|"), mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    row_b <- find_row("y_on", mm$export_name[1])
    tmp_row <- add_coef_row(mm$mediator_var[1], row_b, "Mediator", "Outcome model", mm$mediator_label[1], mm$mediator_label[1], 1000030 + i, "Outcome model", mediator = paste(prep_obj$mediator_map$mediator_var, collapse = "|"), mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = outcome_block_order)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }

  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(
        term = as.character(map_i$term[1]),
        row_obj = row_i,
        effect_type = as.character(map_i$effect_type[1]),
        effect = "Outcome model",
        term_label = as.character(map_i$term_label[1]),
        variable_label = as.character(map_i$variable_label[1]),
        sort_key = 1400000 + j,
        model_component = "Outcome model",
        category_label = as.character(map_i$category_label[1]),
        reference_label = as.character(map_i$reference_label[1]),
        mediator = paste(prep_obj$mediator_map$mediator_var, collapse = "|"),
        mediator_label = mediator_set_label,
        table_block_key = outcome_block_key,
        block_order = outcome_block_order
      )
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row
      idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  summary_rows <- list()
  sidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(mm$export_name[1]), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", mm$export_name[1])
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && mm$export_name[1] %in% names(prep_obj$export_data)) {
      med_var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[mm$export_name[1]]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[mm$export_name[1]]], wt_vec)^2
      if (!is.na(med_var_i) && med_var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / med_var_i))
    }
    med_test <- compute_step_test_pm(mm$export_name[1], c("x_pm", prep_obj$cov_export_names))
    summary_rows[[sidx]] <- data.frame(
      model_component = "Mediator model",
      outcome = mm$mediator_var[1],
      outcome_label = mm$mediator_label[1],
      mediator = mm$mediator_var[1],
      mediator_label = mm$mediator_label[1],
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = r2_i,
      adj_r2 = NA_real_,
      f_value = med_test$f_value %||% NA_real_,
      df1 = med_test$df1 %||% NA_real_,
      df2 = NA_real_,
      p = med_test$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = paste0(analysis_key, " | med | ", mm$mediator_var[1]),
      block_order = mm$block_order[1],
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
    sidx <- sidx + 1L
  }

  r2_y <- NA_real_
  hit_y <- parsed_r2[as.character(parsed_r2$name) == "Y_PM", , drop = FALSE]
  if (nrow(hit_y) == 1) r2_y <- hit_y$estimate[1]
  resid_y <- find_row("residual_variances", "y_pm")
  resid_var_y <- if (nrow(resid_y) == 1) safe_num_pm(resid_y$estimate[1]) else NA_real_
  if (is.na(r2_y) && !is.na(resid_var_y) && "y_pm" %in% names(prep_obj$export_data)) {
    y_var <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data$y_pm), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data$y_pm, wt_vec)^2
    if (!is.na(y_var) && y_var > 0) r2_y <- max(0, min(1, 1 - resid_var_y / y_var))
  }
  out_test <- compute_step_test_pm("y_pm", c("x_pm", prep_obj$mediator_map$export_name, prep_obj$cov_export_names))
  summary_rows[[sidx]] <- data.frame(
    model_component = "Outcome model",
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    mediator = paste(prep_obj$mediator_map$mediator_var, collapse = "|"),
    mediator_label = mediator_set_label,
    x_var = x_var,
    x_label = lookup_label(x_var),
    target_outcome = outcome_var,
    target_outcome_label = lookup_label(outcome_var),
    n = nobs,
    r2 = r2_y,
    adj_r2 = NA_real_,
    f_value = out_test$f_value %||% NA_real_,
    df1 = out_test$df1 %||% NA_real_,
    df2 = NA_real_,
    p = out_test$p %||% NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key,
    block_order = outcome_block_order,
    analysis_key = analysis_key,
    stringsAsFactors = FALSE
  )
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)
  model_summary <- model_summary[order(model_summary$block_order), , drop = FALSE]

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  diff_map <- build_indirect_difference_map_pm(prep_obj$mediator_map)
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  row_total <- new_params[as.character(new_params$name) == "TOTAL", , drop = FALSE]
  indirect_rows <- list()
  iidx <- 1L
  for (i in seq_len(nrow(prep_obj$mediator_map))) {
    mm <- prep_obj$mediator_map[i, , drop = FALSE]
    a_row <- coef_df[coef_df$model_component == "Mediator model" & as.character(coef_df$mediator) == mm$mediator_var[1] & as.character(coef_df$term) == x_var, , drop = FALSE]
    b_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == mm$mediator_var[1], , drop = FALSE]
    row_ind <- new_params[as.character(new_params$name) == toupper(mm$indirect_label[1]), , drop = FALSE]
    if (nrow(a_row) == 1 && nrow(b_row) == 1 && nrow(row_ind) == 1) {
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[iidx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = mm$mediator_var[1],
        mediator_label = mm$mediator_label[1],
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Indirect effect",
        a = a_row$estimate[1],
        a_se = a_row$se[1],
        a_p = a_row$p[1],
        b = b_row$estimate[1],
        b_se = b_row$se[1],
        b_p = b_row$p[1],
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
        indirect = row_ind$estimate[1],
        se = row_ind$se[1],
        z_value = row_ind$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_ind$p[1],
        p_fmt = fmt_p_pm(row_ind$p[1]),
        sig = sig_mark_pm(row_ind$p[1]),
        contrast_order = i,
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      iidx <- iidx + 1L
    }
  }
  if (is.data.frame(diff_map) && nrow(diff_map) > 0) {
    for (k in seq_len(nrow(diff_map))) {
      row_diff <- new_params[as.character(new_params$name) == toupper(diff_map$param_name[k]), , drop = FALSE]
      if (nrow(row_diff) != 1) next
      ci_i <- make_ci(row_diff$estimate[1], row_diff$se[1])
      indirect_rows[[iidx]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = paste0(diff_map$mediator1[k], " - ", diff_map$mediator2[k]),
        mediator_label = paste0(diff_map$mediator1_label[k], " - ", diff_map$mediator2_label[k]),
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Indirect effect difference",
        a = NA_real_,
        a_se = NA_real_,
        a_p = NA_real_,
        b = NA_real_,
        b_se = NA_real_,
        b_p = NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
        indirect = row_diff$estimate[1],
        se = row_diff$se[1],
        z_value = row_diff$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_diff$p[1],
        p_fmt = fmt_p_pm(row_diff$p[1]),
        sig = sig_mark_pm(row_diff$p[1]),
        contrast_order = 100 + k,
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
      iidx <- iidx + 1L
    }
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()
  if (is.data.frame(indirect_df) && nrow(indirect_df) > 0 && "contrast_order" %in% names(indirect_df)) {
    indirect_df <- indirect_df[order(indirect_df$contrast_order, indirect_df$mediator_label), , drop = FALSE]
    rownames(indirect_df) <- NULL
  }

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

build_model6_effect_defs_pm <- function(med_map) {
  n_med <- nrow(med_map)
  defs <- lapply(seq_len(n_med), function(i) {
    list(
      label = med_map$mediator_label[i],
      param = paste0("ie", i),
      expr = paste0("a", i, "*b", i)
    )
  })
  if (n_med >= 2L) {
    defs[[length(defs) + 1L]] <- list(
      label = paste0(med_map$mediator_label[1], " -> ", med_map$mediator_label[2]),
      param = "ie12",
      expr = "a1*d21*b2"
    )
  }
  if (n_med == 3L) {
    defs[[length(defs) + 1L]] <- list(
      label = paste0(med_map$mediator_label[1], " -> ", med_map$mediator_label[3]),
      param = "ie13",
      expr = "a1*d31*b3"
    )
    defs[[length(defs) + 1L]] <- list(
      label = paste0(med_map$mediator_label[2], " -> ", med_map$mediator_label[3]),
      param = "ie23",
      expr = "a2*d32*b3"
    )
    defs[[length(defs) + 1L]] <- list(
      label = paste0(med_map$mediator_label[1], " -> ", med_map$mediator_label[2], " -> ", med_map$mediator_label[3]),
      param = "ie123",
      expr = "a1*d21*d32*b3"
    )
  }
  defs
}

build_observed_serial_mediation_input_pm <- function(prep_obj, model_tag, outcome_var, x_var, mediator_vars) {
  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  wrap_new_statement_pm <- function(values, indent = "  ", width = 78) {
    values <- as.character(values)
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) return(character(0))
    prefix <- paste0(indent, "NEW(")
    cont_prefix <- paste0(indent, "  ")
    out <- character(0)
    current <- prefix
    for (val in values) {
      candidate <- if (identical(current, prefix)) paste0(current, val) else paste(current, val)
      if (nchar(candidate, type = "width") > width) {
        out <- c(out, current)
        current <- paste0(cont_prefix, val)
      } else {
        current <- candidate
      }
    }
    c(out, paste0(current, ");"))
  }
  paths <- list(
    data_file = file.path(DIR_MPLUS_DATA, paste0(model_tag, ".dat")),
    inp_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".inp")),
    out_file = file.path(DIR_MPLUS_INP, paste0(model_tag, ".out"))
  )

  write_mplus_data(
    data = prep_obj$export_data,
    path = paths$data_file,
    missing_code = cfg_local$mplus_missing_code %||% cfg_local$missing_code %||% -9999
  )

  usevars_vec <- setdiff(names(prep_obj$export_data), "sb_pm")
  variable_lines <- c(
    "VARIABLE:",
    wrap_mplus_statement_pm("NAMES", names(prep_obj$export_data)),
    wrap_mplus_statement_pm("USEVARIABLES", usevars_vec)
  )
  if (prep_obj$use_subpopulation) variable_lines <- c(variable_lines, "  SUBPOPULATION = sb_pm EQ 1;")
  if (!is.null(prep_obj$design_map$weight_var)) variable_lines <- c(variable_lines, paste0("  WEIGHT = ", prep_obj$design_map$weight_var, ";"))
  if (!is.null(prep_obj$design_map$strata_var)) variable_lines <- c(variable_lines, paste0("  STRATIFICATION = ", prep_obj$design_map$strata_var, ";"))
  if (!is.null(prep_obj$design_map$cluster_var)) variable_lines <- c(variable_lines, paste0("  CLUSTER = ", prep_obj$design_map$cluster_var, ";"))

  type_line <- if (!is.null(prep_obj$design_map$weight_var) || !is.null(prep_obj$design_map$strata_var) || !is.null(prep_obj$design_map$cluster_var)) {
    "  TYPE = COMPLEX;"
  } else {
    "  TYPE = GENERAL;"
  }

  med_map <- prep_obj$mediator_map
  n_med <- nrow(med_map)
  if (!n_med %in% c(2L, 3L)) {
    stop("PROCESS model 6 currently supports 2 or 3 mediators in serial order.", call. = FALSE)
  }

  model_lines <- c("MODEL:")

  for (i in seq_len(n_med)) {
    rhs_i <- c(paste0("x_pm (a", i, ")"))
    if (i > 1) {
      for (j in seq_len(i - 1L)) {
        rhs_i <- c(rhs_i, paste0(med_map$export_name[j], " (d", i, j, ")"))
      }
    }
    rhs_i <- c(rhs_i, prep_obj$cov_export_names)
    rhs_i <- rhs_i[nzchar(rhs_i)]
    model_lines <- c(
      model_lines,
      paste0("  ", med_map$export_name[i], " ON"),
      paste0("    ", rhs_i)
    )
    model_lines[length(model_lines)] <- paste0(model_lines[length(model_lines)], ";")
  }

  rhs_y <- c("x_pm (cp1)")
  for (i in seq_len(n_med)) {
    rhs_y <- c(rhs_y, paste0(med_map$export_name[i], " (b", i, ")"))
  }
  rhs_y <- c(rhs_y, prep_obj$cov_export_names)
  rhs_y <- rhs_y[nzchar(rhs_y)]
  model_lines <- c(
    model_lines,
    "  y_pm ON",
    paste0("    ", rhs_y)
  )
  model_lines[length(model_lines)] <- paste0(model_lines[length(model_lines)], ";")
  for (i in seq_len(n_med)) {
    model_lines <- c(model_lines, paste0("  [", med_map$export_name[i], "] (", med_map$intercept_label[i], ");"))
  }
  model_lines <- c(model_lines, "  [y_pm] (iy);")

  effect_defs <- build_model6_effect_defs_pm(med_map)
  indirect_terms <- vapply(effect_defs, `[[`, character(1), "param")
  diff_defs <- list()
  if (length(effect_defs) >= 2L) {
    pair_idx <- utils::combn(seq_along(effect_defs), 2, simplify = FALSE)
    diff_defs <- lapply(seq_along(pair_idx), function(k) {
      ij <- pair_idx[[k]]
      list(
        label = paste0(effect_defs[[ij[1]]]$label, " - ", effect_defs[[ij[2]]]$label),
        param = sprintf("de%02d%02d", ij[1], ij[2]),
        expr = paste0(effect_defs[[ij[1]]]$param, " - ", effect_defs[[ij[2]]]$param)
      )
    })
  }
  diff_terms <- if (length(diff_defs) > 0) vapply(diff_defs, `[[`, character(1), "param") else character(0)

  constraint_lines <- c(
    "MODEL CONSTRAINT:",
    wrap_new_statement_pm(c(indirect_terms, diff_terms, "itot", "total"))
  )
  for (def_i in effect_defs) {
    constraint_lines <- c(constraint_lines, paste0("  ", def_i$param, " = ", def_i$expr, ";"))
  }
  if (length(diff_defs) > 0) {
    for (def_i in diff_defs) {
      constraint_lines <- c(constraint_lines, paste0("  ", def_i$param, " = ", def_i$expr, ";"))
    }
  }
  constraint_lines <- c(
    constraint_lines,
    paste0("  itot = ", paste(indirect_terms, collapse = " + "), ";"),
    "  total = cp1 + itot;"
  )

  input_lines <- c(
    paste0("TITLE: PROCESS-like observed serial mediation for ", outcome_var, " from ", x_var, " through ", paste(mediator_vars, collapse = " -> "), ";"),
    "DATA:",
    paste0("  FILE = ", gsub("\\\\", "/", paths$data_file), ";"),
    variable_lines,
    "ANALYSIS:",
    type_line,
    paste0("  ESTIMATOR = ", PROCESS_SETTINGS$mplus_estimator %||% "MLR", ";"),
    model_lines,
    constraint_lines,
    "OUTPUT:",
    "  SAMPSTAT CINTERVAL;"
  )

  writeLines(input_lines, con = paths$inp_file, useBytes = TRUE)
  paths
}

fit_observed_mplus_model6 <- function(df, outcome_var, x_var, mediator_vars, covariates, survey_bundle) {
  mediator_vars <- unique(as.character(mediator_vars))
  mediator_vars <- mediator_vars[!is.na(mediator_vars) & nzchar(mediator_vars)]
  if (!length(mediator_vars) %in% c(2L, 3L)) {
    stop("PROCESS model 6 currently requires 2 or 3 mediators in serial order.", call. = FALSE)
  }

  prep_obj <- build_observed_parallel_mediation_mplus_data_pm(
    df = df,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars,
    covariates = covariates,
    survey_bundle = survey_bundle,
    process_settings = PROCESS_SETTINGS
  )
  if (is.null(prep_obj)) return(NULL)

  model_tag <- paste(
    sanitize_token_pm(DATASET_ID),
    "pm6obs",
    sanitize_token_pm(outcome_var),
    sanitize_token_pm(x_var),
    paste(vapply(mediator_vars, sanitize_token_pm, character(1)), collapse = ""),
    sep = "_"
  )
  paths <- build_observed_serial_mediation_input_pm(
    prep_obj = prep_obj,
    model_tag = model_tag,
    outcome_var = outcome_var,
    x_var = x_var,
    mediator_vars = mediator_vars
  )

  cfg_local <- if (exists("CFG", inherits = TRUE)) get("CFG", inherits = TRUE) else list()
  mplus_exe <- resolve_mplus_exe(cfg_local, default = cfg_local$mplus_exe %||% NULL, must_exist = TRUE)
  run_res <- run_mplus_model(paths$inp_file, mplus_exe = mplus_exe, workdir = dirname(paths$inp_file), quiet = TRUE)
  out_lines <- read_mplus_out_lines(paths$out_file)

  if (!isTRUE(run_res$ok) || !detect_mplus_normal_termination(out_lines)) {
    stop(
      "Mplus observed serial mediation run failed for outcome = ", outcome_var,
      ", x = ", x_var, ", mediators = ", paste(mediator_vars, collapse = " -> "),
      ". Check: ", paths$out_file,
      call. = FALSE
    )
  }

  parsed_main <- parse_mplus_mediation_sections_pm(out_lines)
  parsed_r2 <- parse_mplus_rsquare_pm(out_lines)
  nobs <- parse_mplus_nobs_pm(out_lines)
  if (is.na(nobs)) nobs <- prep_obj$analysis_n

  find_row <- function(section, name, lhs = NULL) {
    if (!is.data.frame(parsed_main) || nrow(parsed_main) == 0) return(data.frame())
    hit <- parsed_main[
      as.character(parsed_main$section) == section &
        as.character(parsed_main$name) == toupper(name),
      ,
      drop = FALSE
    ]
    if (!is.null(lhs) && "lhs" %in% names(hit)) {
      hit <- hit[as.character(hit$lhs) == toupper(lhs), , drop = FALSE]
    }
    if (nrow(hit) == 0) data.frame() else hit[1, , drop = FALSE]
  }

  make_ci <- function(est, se) {
    c(llci = ifelse(is.na(est) || is.na(se), NA_real_, est - 1.96 * se),
      ulci = ifelse(is.na(est) || is.na(se), NA_real_, est + 1.96 * se))
  }

  compute_step_test_pm <- function(dep_var, rhs_terms) {
    rhs_terms <- rhs_terms[rhs_terms %in% names(prep_obj$export_data)]
    if (!length(rhs_terms) || !dep_var %in% names(prep_obj$export_data)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    dat_fit <- prep_obj$export_data
    if (prep_obj$use_subpopulation && "sb_pm" %in% names(dat_fit)) dat_fit <- dat_fit[dat_fit$sb_pm == 1, , drop = FALSE]
    if (!nrow(dat_fit)) return(list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
    form_chr <- paste(dep_var, "~", paste(rhs_terms, collapse = " + "))
    if (requireNamespace("survey", quietly = TRUE)) {
      survey_res <- tryCatch({
        ids_formula <- if (!is.null(prep_obj$design_map$cluster_var) && prep_obj$design_map$cluster_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$cluster_var)) else ~1
        strata_formula <- if (!is.null(prep_obj$design_map$strata_var) && prep_obj$design_map$strata_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$strata_var)) else NULL
        weights_formula <- if (!is.null(prep_obj$design_map$weight_var) && prep_obj$design_map$weight_var %in% names(dat_fit)) stats::as.formula(paste0("~", prep_obj$design_map$weight_var)) else NULL
        des <- survey::svydesign(ids = ids_formula, strata = strata_formula, weights = weights_formula, data = dat_fit, nest = TRUE)
        fit <- survey::svyglm(stats::as.formula(form_chr), design = des)
        tst <- survey::regTermTest(fit, stats::as.formula(paste0("~", paste(rhs_terms, collapse = " + "))), method = "Wald")
        df1_i <- safe_num_pm(if (!is.null(tst$df)) tst$df[1] else NA_real_)
        p_i <- safe_num_pm(if (!is.null(tst$p)) tst$p[1] else NA_real_)
        f_i <- safe_num_pm(if (!is.null(tst$Ftest)) tst$Ftest[1] else NA_real_)
        chi_i <- safe_num_pm(if (!is.null(tst$chisq)) tst$chisq[1] else NA_real_)
        if (is.na(f_i) && !is.na(chi_i) && !is.na(df1_i) && df1_i > 0) f_i <- chi_i / df1_i
        list(f_value = f_i, df1 = df1_i, p = p_i)
      }, error = function(e) NULL)
      if (!is.null(survey_res)) return(survey_res)
    }
    tryCatch({
      fit <- stats::lm(stats::as.formula(form_chr), data = dat_fit)
      fstat <- tryCatch(unname(summary(fit)$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
      p_i <- if (length(fstat) >= 3 && all(!is.na(fstat[1:3]))) stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
      list(f_value = safe_num_pm(fstat[1]), df1 = safe_num_pm(fstat[2]), p = safe_num_pm(p_i))
    }, error = function(e) list(f_value = NA_real_, df1 = NA_real_, p = NA_real_))
  }

  add_coef_row <- function(term, row_obj, effect_type, effect, term_label, variable_label = term_label, sort_key, model_component, category_label = "", reference_label = "", mediator = NA_character_, mediator_label = NA_character_, table_block_key = "", block_order = NA_real_) {
    if (!is.data.frame(row_obj) || nrow(row_obj) == 0) return(NULL)
    ci_i <- make_ci(row_obj$estimate[1], row_obj$se[1])
    data.frame(
      term = term,
      estimate = row_obj$estimate[1],
      se = row_obj$se[1],
      t_value = row_obj$z_value[1],
      p = row_obj$p[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      effect_type = effect_type,
      effect = effect,
      term_label = term_label,
      variable_label = variable_label,
      category_label = category_label,
      reference_label = reference_label,
      sort_key = sort_key,
      model_component = model_component,
      mediator = mediator,
      mediator_label = mediator_label,
      table_block_key = table_block_key,
      block_order = block_order,
      stringsAsFactors = FALSE
    )
  }

  coef_rows <- list()
  idx <- 1L
  med_map <- prep_obj$mediator_map
  n_med <- nrow(med_map)
  analysis_key <- paste(outcome_var, x_var, paste(med_map$mediator_var, collapse = "|"), sep = " | ")

  for (i in seq_len(n_med)) {
    med_i <- med_map[i, , drop = FALSE]
    block_key_i <- paste0(analysis_key, " | med | ", med_i$mediator_var[1])
    row_int_i <- find_row("intercepts", med_i$export_name[1])
    tmp_row <- add_coef_row("(Intercept)", row_int_i, "Intercept", "Mediator model", "Intercept", "Intercept", 10, "Mediator model", mediator = med_i$mediator_var[1], mediator_label = med_i$mediator_label[1], table_block_key = block_key_i, block_order = i)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

    row_a_i <- find_row(paste0(tolower(med_i$export_name[1]), "_on"), "x_pm", lhs = med_i$export_name[1])
    tmp_row <- add_coef_row(x_var, row_a_i, "Independent variable", "Mediator model", lookup_label(x_var), lookup_label(x_var), 20, "Mediator model", mediator = med_i$mediator_var[1], mediator_label = med_i$mediator_label[1], table_block_key = block_key_i, block_order = i)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }

    if (i > 1L) {
      for (j in seq_len(i - 1L)) {
        prev_j <- med_map[j, , drop = FALSE]
        row_d_ij <- find_row(paste0(tolower(med_i$export_name[1]), "_on"), prev_j$export_name[1], lhs = med_i$export_name[1])
        tmp_row <- add_coef_row(prev_j$mediator_var[1], row_d_ij, "Mediator", "Mediator model", prev_j$mediator_label[1], prev_j$mediator_label[1], 20 + j, "Mediator model", mediator = med_i$mediator_var[1], mediator_label = med_i$mediator_label[1], table_block_key = block_key_i, block_order = i)
        if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
      }
    }

    if (nrow(prep_obj$cov_term_map) > 0) {
      for (j in seq_len(nrow(prep_obj$cov_term_map))) {
        map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
        row_i <- find_row(paste0(tolower(med_i$export_name[1]), "_on"), map_i$export_name[1], lhs = med_i$export_name[1])
        tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Mediator model", as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 400000 + j, "Mediator model", as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = med_i$mediator_var[1], mediator_label = med_i$mediator_label[1], table_block_key = block_key_i, block_order = i)
        if (is.null(tmp_row)) next
        coef_rows[[idx]] <- tmp_row; idx <- idx + 1L
      }
    }
  }

  outcome_block_key <- paste0(analysis_key, " | outcome")
  mediator_set_label <- paste(med_map$mediator_label, collapse = " -> ")
  mediator_set_var <- paste(med_map$mediator_var, collapse = "|")
  row_iy <- find_row("intercepts", "y_pm")
  tmp_row <- add_coef_row("(Intercept)", row_iy, "Intercept", "Outcome model", "Intercept", "Intercept", 1000010, "Outcome model", mediator = mediator_set_var, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = n_med + 1L)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  row_cp <- find_row("y_on", "x_pm")
  tmp_row <- add_coef_row(x_var, row_cp, "Independent variable", "Outcome model", lookup_label(x_var), lookup_label(x_var), 1000020, "Outcome model", mediator = mediator_set_var, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = n_med + 1L)
  if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  for (i in seq_len(n_med)) {
    med_i <- med_map[i, , drop = FALSE]
    row_b_i <- find_row("y_on", med_i$export_name[1])
    tmp_row <- add_coef_row(med_i$mediator_var[1], row_b_i, "Mediator", "Outcome model", med_i$mediator_label[1], med_i$mediator_label[1], 1000030 + i, "Outcome model", mediator = mediator_set_var, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = n_med + 1L)
    if (!is.null(tmp_row)) { coef_rows[[idx]] <- tmp_row; idx <- idx + 1L }
  }
  if (nrow(prep_obj$cov_term_map) > 0) {
    for (j in seq_len(nrow(prep_obj$cov_term_map))) {
      map_i <- prep_obj$cov_term_map[j, , drop = FALSE]
      row_i <- find_row("y_on", map_i$export_name[1])
      tmp_row <- add_coef_row(as.character(map_i$term[1]), row_i, as.character(map_i$effect_type[1]), "Outcome model", as.character(map_i$term_label[1]), as.character(map_i$variable_label[1]), 1400000 + j, "Outcome model", as.character(map_i$category_label[1]), as.character(map_i$reference_label[1]), mediator = mediator_set_var, mediator_label = mediator_set_label, table_block_key = outcome_block_key, block_order = n_med + 1L)
      if (is.null(tmp_row)) next
      coef_rows[[idx]] <- tmp_row; idx <- idx + 1L
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  if (nrow(coef_df) > 0) {
    coef_df$outcome <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator, outcome_var)
    coef_df$outcome_label <- ifelse(coef_df$model_component == "Mediator model", coef_df$mediator_label, lookup_label(outcome_var))
    coef_df$target_outcome <- outcome_var
    coef_df$target_outcome_label <- lookup_label(outcome_var)
    coef_df$x_var <- x_var
    coef_df$x_label <- lookup_label(x_var)
    coef_df$analysis_key <- analysis_key
    coef_df$n <- nobs
    coef_df$p_fmt <- fmt_p_pm(coef_df$p)
    coef_df$sig <- sig_mark_pm(coef_df$p)
    coef_df <- coef_df[order(coef_df$block_order, coef_df$sort_key, coef_df$term_label), , drop = FALSE]
    rownames(coef_df) <- NULL
  }

  wt_vec <- if ("wt_pm" %in% names(prep_obj$export_data)) safe_num_pm(prep_obj$export_data$wt_pm) else NULL
  get_r2_pm <- function(var_name) {
    r2_i <- NA_real_
    hit_i <- parsed_r2[as.character(parsed_r2$name) == toupper(var_name), , drop = FALSE]
    if (nrow(hit_i) == 1) r2_i <- hit_i$estimate[1]
    resid_i <- find_row("residual_variances", var_name)
    resid_var_i <- if (nrow(resid_i) == 1) safe_num_pm(resid_i$estimate[1]) else NA_real_
    if (is.na(r2_i) && !is.na(resid_var_i) && var_name %in% names(prep_obj$export_data)) {
      var_i <- if (is.null(wt_vec)) stats::var(safe_num_pm(prep_obj$export_data[[var_name]]), na.rm = TRUE) else weighted_sd_pm(prep_obj$export_data[[var_name]], wt_vec)^2
      if (!is.na(var_i) && var_i > 0) r2_i <- max(0, min(1, 1 - resid_var_i / var_i))
    }
    r2_i
  }

  summary_rows <- list()
  for (i in seq_len(n_med)) {
    med_i <- med_map[i, , drop = FALSE]
    rhs_i <- c("x_pm")
    if (i > 1L) rhs_i <- c(rhs_i, med_map$export_name[seq_len(i - 1L)])
    rhs_i <- c(rhs_i, prep_obj$cov_export_names)
    test_i <- compute_step_test_pm(med_i$export_name[1], rhs_i)
    block_key_i <- paste0(analysis_key, " | med | ", med_i$mediator_var[1])
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      model_component = "Mediator model",
      outcome = med_i$mediator_var[1],
      outcome_label = med_i$mediator_label[1],
      mediator = med_i$mediator_var[1],
      mediator_label = med_i$mediator_label[1],
      x_var = x_var,
      x_label = lookup_label(x_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = nobs,
      r2 = get_r2_pm(med_i$export_name[1]),
      adj_r2 = NA_real_,
      f_value = test_i$f_value %||% NA_real_,
      df1 = test_i$df1 %||% NA_real_,
      df2 = NA_real_,
      p = test_i$p %||% NA_real_,
      bootstrap_enabled = FALSE,
      bootstrap_n = NA_real_,
      bootstrap_ci = NA_character_,
      estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
      variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
      table_block_key = block_key_i,
      block_order = i,
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
  }
  rhs_y_test <- c("x_pm", med_map$export_name, prep_obj$cov_export_names)
  test_y <- compute_step_test_pm("y_pm", rhs_y_test)
  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    model_component = "Outcome model",
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    mediator = mediator_set_var,
    mediator_label = mediator_set_label,
    x_var = x_var,
    x_label = lookup_label(x_var),
    target_outcome = outcome_var,
    target_outcome_label = lookup_label(outcome_var),
    n = nobs,
    r2 = get_r2_pm("y_pm"),
    adj_r2 = NA_real_,
    f_value = test_y$f_value %||% NA_real_,
    df1 = test_y$df1 %||% NA_real_,
    df2 = NA_real_,
    p = test_y$p %||% NA_real_,
    bootstrap_enabled = FALSE,
    bootstrap_n = NA_real_,
    bootstrap_ci = NA_character_,
    estimator = PROCESS_SETTINGS$mplus_estimator %||% "MLR",
    variance_method = PROCESS_SETTINGS$variance_method %||% "survey_design",
    table_block_key = outcome_block_key,
    block_order = n_med + 1L,
    analysis_key = analysis_key,
    stringsAsFactors = FALSE
  )
  model_summary <- do.call(rbind, summary_rows)
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  new_params <- parsed_main[as.character(parsed_main$section) == "new_parameters", , drop = FALSE]
  cp_row <- coef_df[coef_df$model_component == "Outcome model" & as.character(coef_df$term) == x_var, , drop = FALSE]
  row_total <- new_params[as.character(new_params$name) == "TOTAL", , drop = FALSE]
  get_new_param <- function(name) new_params[as.character(new_params$name) == toupper(name), , drop = FALSE]

  indirect_rows <- list()
  rows_spec <- build_model6_effect_defs_pm(med_map)
  for (i in seq_along(rows_spec)) {
    spec <- rows_spec[[i]]
    row_ind <- get_new_param(spec$param)
    if (nrow(row_ind) != 1) next
    ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
    indirect_rows[[i]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = spec$label,
      mediator_label = spec$label,
      x_var = x_var,
      x_label = lookup_label(x_var),
      class_contrast = "Indirect effect",
      a = NA_real_,
      a_se = NA_real_,
      a_p = NA_real_,
      b = NA_real_,
      b_se = NA_real_,
      b_p = NA_real_,
      direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
      total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
      indirect = row_ind$estimate[1],
      se = row_ind$se[1],
      z_value = row_ind$z_value[1],
      llci = ci_i["llci"],
      ulci = ci_i["ulci"],
      p = row_ind$p[1],
      p_fmt = fmt_p_pm(row_ind$p[1]),
      sig = sig_mark_pm(row_ind$p[1]),
      analysis_key = analysis_key,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows_spec) >= 2L) {
    pair_idx <- utils::combn(seq_along(rows_spec), 2, simplify = FALSE)
    for (k in seq_along(pair_idx)) {
      ij <- pair_idx[[k]]
      spec <- list(
        label = paste0(rows_spec[[ij[1]]]$label, " - ", rows_spec[[ij[2]]]$label),
        param = sprintf("de%02d%02d", ij[1], ij[2])
      )
      row_ind <- get_new_param(spec$param)
      if (nrow(row_ind) != 1) next
      ci_i <- make_ci(row_ind$estimate[1], row_ind$se[1])
      indirect_rows[[length(indirect_rows) + 1L]] <- data.frame(
        outcome = outcome_var,
        outcome_label = lookup_label(outcome_var),
        mediator = spec$label,
        mediator_label = spec$label,
        x_var = x_var,
        x_label = lookup_label(x_var),
        class_contrast = "Indirect effect difference",
        a = NA_real_,
        a_se = NA_real_,
        a_p = NA_real_,
        b = NA_real_,
        b_se = NA_real_,
        b_p = NA_real_,
        direct = if (nrow(cp_row) == 1) cp_row$estimate[1] else NA_real_,
        total = if (nrow(row_total) == 1) row_total$estimate[1] else NA_real_,
        indirect = row_ind$estimate[1],
        se = row_ind$se[1],
        z_value = row_ind$z_value[1],
        llci = ci_i["llci"],
        ulci = ci_i["ulci"],
        p = row_ind$p[1],
        p_fmt = fmt_p_pm(row_ind$p[1]),
        sig = sig_mark_pm(row_ind$p[1]),
        analysis_key = analysis_key,
        stringsAsFactors = FALSE
      )
    }
  }
  indirect_df <- if (length(indirect_rows) > 0) do.call(rbind, indirect_rows) else data.frame()

  list(
    coefficients = coef_df,
    summary = model_summary,
    indirect = indirect_df,
    run_info = data.frame(
      model_tag = model_tag,
      inp_file = paths$inp_file,
      out_file = paths$out_file,
      data_file = paths$data_file,
      stringsAsFactors = FALSE
    )
  )
}

clean_term_label_model4 <- function(term, mediator_var, ref_class, cov_term_map = NULL) {
  term_chr <- as.character(term)[1]
  med_label <- lookup_label(mediator_var)

  if (identical(term_chr, "(Intercept)")) {
    return(list(
      effect_type = "Intercept",
      effect = "Intercept",
      term_label = "Intercept",
      variable_label = "Intercept",
      category_label = "",
      reference_label = "",
      sort_key = 10
    ))
  }

  class_hit <- regmatches(term_chr, regexec("^class_fac([0-9]+)$", term_chr))[[1]]
  if (length(class_hit) > 1) {
    cls <- suppressWarnings(as.integer(class_hit[2]))
    lbl <- paste0("Class ", cls, " vs Class ", ref_class)
    return(list(
      effect_type = "Class",
      effect = "Class main effect",
      term_label = lbl,
      variable_label = lbl,
      category_label = "",
      reference_label = "",
      sort_key = 20 + cls
    ))
  }

  if (identical(term_chr, mediator_var)) {
    return(list(
      effect_type = "Mediator",
      effect = "Mediator effect",
      term_label = med_label,
      variable_label = med_label,
      category_label = "",
      reference_label = "",
      sort_key = 200
    ))
  }

  if (is.data.frame(cov_term_map) && nrow(cov_term_map) > 0) {
    hit <- cov_term_map[as.character(cov_term_map$term) == term_chr, , drop = FALSE]
    if (nrow(hit) > 0) {
      return(list(
        effect_type = as.character(hit$effect_type[1]),
        effect = as.character(hit$effect[1]),
        term_label = as.character(hit$term_label[1]),
        variable_label = as.character(hit$variable_label[1]),
        category_label = as.character(hit$category_label[1]),
        reference_label = as.character(hit$reference_label[1]),
        sort_key = as.numeric(hit$sort_key[1])
      ))
    }
  }

  list(
    effect_type = "Covariate",
    effect = "Covariate",
    term_label = lookup_label(term_chr),
    variable_label = lookup_label(term_chr),
    category_label = "",
    reference_label = "",
    sort_key = 900000
  )
}

extract_coef_df <- function(fit) {
  coef_mat <- summary(fit)$coefficients
  coef_df <- data.frame(
    term = rownames(coef_mat),
    estimate = coef_mat[, "Estimate"],
    se = coef_mat[, "Std. Error"],
    t_value = coef_mat[, "t value"],
    p = coef_mat[, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)
  if (!is.null(ci)) {
    ci_df <- data.frame(term = rownames(ci), llci = ci[, 1], ulci = ci[, 2], stringsAsFactors = FALSE, row.names = NULL)
    coef_df <- merge(coef_df, ci_df, by = "term", all.x = TRUE, sort = FALSE)
  } else {
    coef_df$llci <- NA_real_
    coef_df$ulci <- NA_real_
  }
  coef_df
}

bootstrap_coef_ci <- function(dat, formula_obj, term_names, t0 = NULL, n_boot = 5000L, ci_type = "bca") {
  if (!is.data.frame(dat) || nrow(dat) == 0 || length(term_names) == 0) {
    return(data.frame(term = term_names, llci = NA_real_, ulci = NA_real_, stringsAsFactors = FALSE))
  }
  n_boot <- suppressWarnings(as.integer(n_boot))
  if (is.na(n_boot) || n_boot < 1L) {
    return(data.frame(term = term_names, llci = NA_real_, ulci = NA_real_, stringsAsFactors = FALSE))
  }
  ci_type <- tolower(as.character(ci_type %||% "bca")[1])
  if (!ci_type %in% c("bca", "percentile")) ci_type <- "bca"

  stat_fun <- function(data, idx) {
    fit_b <- tryCatch(stats::lm(formula_obj, data = data[idx, , drop = FALSE]), error = function(e) NULL)
    out <- rep(NA_real_, length(term_names))
    names(out) <- term_names
    if (is.null(fit_b)) return(out)
    coef_b <- stats::coef(fit_b)
    hit <- intersect(names(coef_b), term_names)
    if (length(hit) > 0) out[hit] <- coef_b[hit]
    out
  }

  if (requireNamespace("boot", quietly = TRUE)) {
    boot_res <- tryCatch(
      boot::boot(data = dat, statistic = stat_fun, R = n_boot),
      error = function(e) NULL
    )
    if (!is.null(boot_res)) {
      return(data.frame(
        term = term_names,
        llci = vapply(seq_along(term_names), function(i) {
          ci <- tryCatch(boot::boot.ci(boot_res, type = if (ci_type == "bca") "bca" else "perc", index = i), error = function(e) NULL)
          if (is.null(ci)) return(NA_real_)
          vals <- if (ci_type == "bca") ci$bca else ci$percent
          if (is.null(vals)) return(NA_real_)
          as.numeric(vals[4])
        }, numeric(1)),
        ulci = vapply(seq_along(term_names), function(i) {
          ci <- tryCatch(boot::boot.ci(boot_res, type = if (ci_type == "bca") "bca" else "perc", index = i), error = function(e) NULL)
          if (is.null(ci)) return(NA_real_)
          vals <- if (ci_type == "bca") ci$bca else ci$percent
          if (is.null(vals)) return(NA_real_)
          as.numeric(vals[5])
        }, numeric(1)),
        stringsAsFactors = FALSE
      ))
    }
  }

  boot_mat <- replicate(n_boot, stat_fun(dat, sample.int(nrow(dat), size = nrow(dat), replace = TRUE)))
  if (is.vector(boot_mat)) boot_mat <- matrix(boot_mat, nrow = length(term_names))
  boot_mat <- t(boot_mat)
  colnames(boot_mat) <- term_names
  data.frame(
    term = term_names,
    llci = vapply(term_names, function(tt) {
      x <- stats::na.omit(boot_mat[, tt])
      if (length(x) < 50L) NA_real_ else as.numeric(stats::quantile(x, probs = 0.025, na.rm = TRUE, type = 6))
    }, numeric(1)),
    ulci = vapply(term_names, function(tt) {
      x <- stats::na.omit(boot_mat[, tt])
      if (length(x) < 50L) NA_real_ else as.numeric(stats::quantile(x, probs = 0.975, na.rm = TRUE, type = 6))
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
}

bootstrap_indirect_ci <- function(dat, fit_m_formula, fit_y_formula, class_terms, mediator_var, n_boot = 5000L, ci_type = "bca") {
  out <- data.frame(
    class_term = class_terms,
    llci = NA_real_,
    ulci = NA_real_,
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(dat) || nrow(dat) == 0 || length(class_terms) == 0) return(out)
  n_boot <- suppressWarnings(as.integer(n_boot))
  if (is.na(n_boot) || n_boot < 1L) return(out)
  ci_type <- tolower(as.character(ci_type %||% "bca")[1])
  if (!ci_type %in% c("bca", "percentile")) ci_type <- "bca"

  stat_fun <- function(data, idx) {
    dat_b <- data[idx, , drop = FALSE]
    fit_m_b <- tryCatch(stats::lm(fit_m_formula, data = dat_b), error = function(e) NULL)
    fit_y_b <- tryCatch(stats::lm(fit_y_formula, data = dat_b), error = function(e) NULL)
    out_b <- rep(NA_real_, length(class_terms))
    names(out_b) <- class_terms
    if (is.null(fit_m_b) || is.null(fit_y_b)) return(out_b)
    coef_m_b <- stats::coef(fit_m_b)
    coef_y_b <- stats::coef(fit_y_b)
    if (!mediator_var %in% names(coef_y_b)) return(out_b)
    b_est <- coef_y_b[[mediator_var]]
    for (tt in class_terms) {
      if (tt %in% names(coef_m_b)) out_b[tt] <- coef_m_b[[tt]] * b_est
    }
    out_b
  }

  if (requireNamespace("boot", quietly = TRUE)) {
    boot_res <- tryCatch(
      boot::boot(data = dat, statistic = stat_fun, R = n_boot),
      error = function(e) NULL
    )
    if (!is.null(boot_res)) {
      out$llci <- vapply(seq_along(class_terms), function(i) {
        ci <- tryCatch(boot::boot.ci(boot_res, type = if (ci_type == "bca") "bca" else "perc", index = i), error = function(e) NULL)
        if (is.null(ci)) return(NA_real_)
        vals <- if (ci_type == "bca") ci$bca else ci$percent
        if (is.null(vals)) return(NA_real_)
        as.numeric(vals[4])
      }, numeric(1))
      out$ulci <- vapply(seq_along(class_terms), function(i) {
        ci <- tryCatch(boot::boot.ci(boot_res, type = if (ci_type == "bca") "bca" else "perc", index = i), error = function(e) NULL)
        if (is.null(ci)) return(NA_real_)
        vals <- if (ci_type == "bca") ci$bca else ci$percent
        if (is.null(vals)) return(NA_real_)
        as.numeric(vals[5])
      }, numeric(1))
      return(out)
    }
  }

  boot_mat <- replicate(n_boot, stat_fun(dat, sample.int(nrow(dat), size = nrow(dat), replace = TRUE)))
  if (is.vector(boot_mat)) boot_mat <- matrix(boot_mat, nrow = length(class_terms))
  boot_mat <- t(boot_mat)
  colnames(boot_mat) <- class_terms
  out$llci <- vapply(class_terms, function(tt) {
    x <- stats::na.omit(boot_mat[, tt])
    if (length(x) < 50L) NA_real_ else as.numeric(stats::quantile(x, probs = 0.025, na.rm = TRUE, type = 6))
  }, numeric(1))
  out$ulci <- vapply(class_terms, function(tt) {
    x <- stats::na.omit(boot_mat[, tt])
    if (length(x) < 50L) NA_real_ else as.numeric(stats::quantile(x, probs = 0.975, na.rm = TRUE, type = 6))
  }, numeric(1))
  out
}

compute_conditional_effects_model1 <- function(fit, x_var, ref_class, dat, outcome_var) {
  coef_fit <- stats::coef(fit)
  vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(vc) || !x_var %in% names(coef_fit)) return(data.frame())

  classes <- sort(unique(stats::na.omit(dat$class_num)))
  if (length(classes) == 0L) return(data.frame())
  df_resid <- tryCatch(stats::df.residual(fit), error = function(e) NA_real_)

  out_list <- vector("list", length(classes))
  for (i in seq_along(classes)) {
    cls <- classes[i]
    l_vec <- rep(0, length(coef_fit))
    names(l_vec) <- names(coef_fit)
    l_vec[x_var] <- 1

    int_term_1 <- paste0("class_fac", cls, ":", x_var)
    int_term_2 <- paste0(x_var, ":class_fac", cls)
    int_term <- intersect(c(int_term_1, int_term_2), names(coef_fit))
    if (!identical(cls, ref_class) && length(int_term) > 0) {
      l_vec[int_term[1]] <- 1
    }

    est <- sum(l_vec * coef_fit)
    se <- sqrt(as.numeric(t(l_vec) %*% vc %*% l_vec))
    t_val <- ifelse(is.na(se) || se == 0, NA_real_, est / se)
    p_val <- ifelse(
      is.na(t_val) || is.na(df_resid),
      NA_real_,
      2 * stats::pt(abs(t_val), df = df_resid, lower.tail = FALSE)
    )
    crit <- ifelse(is.na(df_resid), NA_real_, stats::qt(0.975, df = df_resid))
    llci <- ifelse(is.na(se) || is.na(crit), NA_real_, est - crit * se)
    ulci <- ifelse(is.na(se) || is.na(crit), NA_real_, est + crit * se)

    out_list[[i]] <- data.frame(
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      x_var = x_var,
      x_label = lookup_label(x_var),
      moderator = PROCESS_SETTINGS$w_var %||% "class",
      moderator_label = if (identical(PROCESS_SETTINGS$moderator_source, "latent_class")) "Profile" else (PROCESS_SETTINGS$w_var %||% "Moderator"),
      class_num = cls,
      class_contrast = if (identical(cls, ref_class)) {
        paste0("Class ", cls, " (reference)")
      } else {
        paste0("Class ", cls, " vs Class ", ref_class)
      },
      conditional_effect = est,
      se = se,
      t_value = t_val,
      p = p_val,
      llci = llci,
      ulci = ulci,
      n = stats::nobs(fit),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, out_list)
  out$p_fmt <- fmt_p_pm(out$p)
  out$sig <- sig_mark_pm(out$p)
  rownames(out) <- NULL
  out
}

fit_one_model <- function(df, outcome_var, x_var, covariates, ref_class, bootstrap_enabled = TRUE, bootstrap_n = 5000L, bootstrap_ci = "bca") {
  needed <- unique(c("class_num", outcome_var, x_var, covariates))
  dat <- df[, intersect(needed, names(df)), drop = FALSE]
  dat <- dat[stats::complete.cases(dat[, intersect(c("class_num", outcome_var, x_var), names(dat)), drop = FALSE]), , drop = FALSE]

  if (nrow(dat) == 0) return(NULL)

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[x_var]] <- safe_num_pm(dat[[x_var]])
  dat <- dat[!is.na(dat[[outcome_var]]) & !is.na(dat[[x_var]]) & !is.na(dat$class_num), , drop = FALSE]

  if (nrow(dat) == 0) return(NULL)

  dat <- make_class_factor(dat, ref_class)

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_rhs <- cov_prep$rhs_terms
  cov_term_map <- cov_prep$term_map

  rhs <- c("class_fac", x_var, paste0("class_fac:", x_var), cov_rhs)
  formula_txt <- paste(outcome_var, "~", paste(rhs, collapse = " + "))
  fit <- stats::lm(stats::as.formula(formula_txt), data = dat)
  reduced_rhs <- c("class_fac", x_var, cov_rhs)
  reduced_formula_txt <- paste(outcome_var, "~", paste(reduced_rhs, collapse = " + "))
  fit_reduced <- stats::lm(stats::as.formula(reduced_formula_txt), data = dat)

  coef_df <- extract_coef_df(fit)
  if (isTRUE(bootstrap_enabled)) {
    boot_ci <- bootstrap_coef_ci(dat = dat, formula_obj = stats::as.formula(formula_txt), term_names = coef_df$term, n_boot = bootstrap_n, ci_type = bootstrap_ci)
    coef_df <- merge(coef_df[, setdiff(names(coef_df), c("llci", "ulci")), drop = FALSE], boot_ci, by = "term", all.x = TRUE, sort = FALSE)
  }

  term_info <- lapply(coef_df$term, clean_term_label, x_var = x_var, ref_class = ref_class, cov_term_map = cov_term_map)
  coef_df$effect_type <- vapply(term_info, `[[`, character(1), "effect_type")
  coef_df$effect <- vapply(term_info, `[[`, character(1), "effect")
  coef_df$term_label <- vapply(term_info, `[[`, character(1), "term_label")
  coef_df$variable_label <- vapply(term_info, `[[`, character(1), "variable_label")
  coef_df$category_label <- vapply(term_info, `[[`, character(1), "category_label")
  coef_df$reference_label <- vapply(term_info, `[[`, character(1), "reference_label")
  coef_df$sort_key <- vapply(term_info, `[[`, numeric(1), "sort_key")

  coef_df$outcome <- outcome_var
  coef_df$outcome_label <- lookup_label(outcome_var)
  coef_df$x_var <- x_var
  coef_df$x_label <- lookup_label(x_var)
  coef_df$moderator <- PROCESS_SETTINGS$w_var %||% "class"
  coef_df$moderator_label <- if (identical(PROCESS_SETTINGS$moderator_source, "latent_class")) "Profile" else (PROCESS_SETTINGS$w_var %||% "Moderator")
  coef_df$n <- stats::nobs(fit)
  coef_df$p_fmt <- fmt_p_pm(coef_df$p)
  coef_df$sig <- sig_mark_pm(coef_df$p)
  coef_df <- coef_df[order(coef_df$sort_key, coef_df$term_label), , drop = FALSE]
  rownames(coef_df) <- NULL

  fit_sum <- summary(fit)
  fstat <- tryCatch(unname(fit_sum$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
  delta_test <- tryCatch(stats::anova(fit_reduced, fit), error = function(e) NULL)
  delta_f <- if (is.data.frame(delta_test) && nrow(delta_test) >= 2L && "F" %in% names(delta_test)) delta_test$F[2] else NA_real_
  delta_p <- if (is.data.frame(delta_test) && nrow(delta_test) >= 2L && "Pr(>F)" %in% names(delta_test)) delta_test[["Pr(>F)"]][2] else NA_real_
  model_summary <- data.frame(
    outcome = outcome_var,
    outcome_label = lookup_label(outcome_var),
    x_var = x_var,
    x_label = lookup_label(x_var),
    moderator = PROCESS_SETTINGS$w_var %||% "class",
    moderator_label = if (identical(PROCESS_SETTINGS$moderator_source, "latent_class")) "Profile" else (PROCESS_SETTINGS$w_var %||% "Moderator"),
    n = stats::nobs(fit),
    r2 = fit_sum$r.squared,
    adj_r2 = fit_sum$adj.r.squared,
    f_value = fstat[1],
    df1 = fstat[2],
    df2 = fstat[3],
    p = tryCatch(stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE), error = function(e) NA_real_),
    delta_r2 = fit_sum$r.squared - summary(fit_reduced)$r.squared,
    delta_f = delta_f,
    delta_p = delta_p,
    bootstrap_enabled = isTRUE(bootstrap_enabled),
    bootstrap_n = bootstrap_n,
    bootstrap_ci = bootstrap_ci,
    stringsAsFactors = FALSE
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  conditional_effects <- compute_conditional_effects_model1(
    fit = fit,
    x_var = x_var,
    ref_class = ref_class,
    dat = dat,
    outcome_var = outcome_var
  )

  list(coefficients = coef_df, summary = model_summary, conditional_effects = conditional_effects)
}

fit_mediation_model4 <- function(df, outcome_var, mediator_var, covariates, ref_class, bootstrap_enabled = TRUE, bootstrap_n = 5000L, bootstrap_ci = "bca") {
  needed <- unique(c("class_num", outcome_var, mediator_var, covariates))
  dat <- df[, intersect(needed, names(df)), drop = FALSE]
  dat <- dat[stats::complete.cases(dat[, intersect(c("class_num", outcome_var, mediator_var), names(dat)), drop = FALSE]), , drop = FALSE]
  if (nrow(dat) == 0) return(NULL)

  dat[[outcome_var]] <- safe_num_pm(dat[[outcome_var]])
  dat[[mediator_var]] <- safe_num_pm(dat[[mediator_var]])
  dat <- dat[!is.na(dat[[outcome_var]]) & !is.na(dat[[mediator_var]]) & !is.na(dat$class_num), , drop = FALSE]
  if (nrow(dat) == 0) return(NULL)

  dat <- make_class_factor(dat, ref_class)

  cov_keep <- covariates[covariates %in% names(dat)]
  cov_prep <- prepare_covariates(dat, cov_keep)
  dat <- cov_prep$data
  cov_rhs <- cov_prep$rhs_terms
  cov_term_map <- cov_prep$term_map

  form_m <- paste(mediator_var, "~", paste(c("class_fac", cov_rhs), collapse = " + "))
  fit_m <- stats::lm(stats::as.formula(form_m), data = dat)

  form_y <- paste(outcome_var, "~", paste(c("class_fac", mediator_var, cov_rhs), collapse = " + "))
  fit_y <- stats::lm(stats::as.formula(form_y), data = dat)

  coef_m <- extract_coef_df(fit_m)
  coef_y <- extract_coef_df(fit_y)
  if (isTRUE(bootstrap_enabled)) {
    boot_ci_m <- bootstrap_coef_ci(dat = dat, formula_obj = stats::as.formula(form_m), term_names = coef_m$term, n_boot = bootstrap_n, ci_type = bootstrap_ci)
    coef_m <- merge(coef_m[, setdiff(names(coef_m), c("llci", "ulci")), drop = FALSE], boot_ci_m, by = "term", all.x = TRUE, sort = FALSE)
    boot_ci_y <- bootstrap_coef_ci(dat = dat, formula_obj = stats::as.formula(form_y), term_names = coef_y$term, n_boot = bootstrap_n, ci_type = bootstrap_ci)
    coef_y <- merge(coef_y[, setdiff(names(coef_y), c("llci", "ulci")), drop = FALSE], boot_ci_y, by = "term", all.x = TRUE, sort = FALSE)
  }

  info_m <- lapply(coef_m$term, clean_term_label_model4, mediator_var = mediator_var, ref_class = ref_class, cov_term_map = cov_term_map)
  coef_m$effect_type <- vapply(info_m, `[[`, character(1), "effect_type")
  coef_m$effect <- vapply(info_m, `[[`, character(1), "effect")
  coef_m$term_label <- vapply(info_m, `[[`, character(1), "term_label")
  coef_m$variable_label <- vapply(info_m, `[[`, character(1), "variable_label")
  coef_m$category_label <- vapply(info_m, `[[`, character(1), "category_label")
  coef_m$reference_label <- vapply(info_m, `[[`, character(1), "reference_label")
  coef_m$sort_key <- vapply(info_m, `[[`, numeric(1), "sort_key")
  coef_m$model_component <- "Mediator model"
  coef_m$outcome <- mediator_var
  coef_m$outcome_label <- lookup_label(mediator_var)
  coef_m$mediator <- mediator_var
  coef_m$mediator_label <- lookup_label(mediator_var)
  coef_m$target_outcome <- outcome_var
  coef_m$target_outcome_label <- lookup_label(outcome_var)
  coef_m$n <- stats::nobs(fit_m)
  coef_m$p_fmt <- fmt_p_pm(coef_m$p)
  coef_m$sig <- sig_mark_pm(coef_m$p)

  info_y <- lapply(coef_y$term, clean_term_label_model4, mediator_var = mediator_var, ref_class = ref_class, cov_term_map = cov_term_map)
  coef_y$effect_type <- vapply(info_y, `[[`, character(1), "effect_type")
  coef_y$effect <- vapply(info_y, `[[`, character(1), "effect")
  coef_y$term_label <- vapply(info_y, `[[`, character(1), "term_label")
  coef_y$variable_label <- vapply(info_y, `[[`, character(1), "variable_label")
  coef_y$category_label <- vapply(info_y, `[[`, character(1), "category_label")
  coef_y$reference_label <- vapply(info_y, `[[`, character(1), "reference_label")
  coef_y$sort_key <- vapply(info_y, `[[`, numeric(1), "sort_key")
  coef_y$model_component <- "Outcome model"
  coef_y$outcome <- outcome_var
  coef_y$outcome_label <- lookup_label(outcome_var)
  coef_y$mediator <- mediator_var
  coef_y$mediator_label <- lookup_label(mediator_var)
  coef_y$target_outcome <- outcome_var
  coef_y$target_outcome_label <- lookup_label(outcome_var)
  coef_y$n <- stats::nobs(fit_y)
  coef_y$p_fmt <- fmt_p_pm(coef_y$p)
  coef_y$sig <- sig_mark_pm(coef_y$p)

  a_rows <- coef_m[grepl("^class_fac[0-9]+$", coef_m$term), , drop = FALSE]
  b_row <- coef_y[coef_y$term == mediator_var, , drop = FALSE]

  indirect_df <- data.frame()
  if (nrow(a_rows) > 0 && nrow(b_row) == 1) {
    b_est <- b_row$estimate[1]
    b_se <- b_row$se[1]
    b_p <- b_row$p[1]
    for (i in seq_len(nrow(a_rows))) {
      a_est <- a_rows$estimate[i]
      a_se <- a_rows$se[i]
      indirect <- a_est * b_est
      ind_se <- sqrt((b_est^2 * a_se^2) + (a_est^2 * b_se^2))
      z_val <- ifelse(is.na(ind_se) || ind_se == 0, NA_real_, indirect / ind_se)
      p_val <- ifelse(is.na(z_val), NA_real_, 2 * stats::pnorm(abs(z_val), lower.tail = FALSE))
      llci <- ifelse(is.na(ind_se), NA_real_, indirect - 1.96 * ind_se)
      ulci <- ifelse(is.na(ind_se), NA_real_, indirect + 1.96 * ind_se)
      cls_num <- suppressWarnings(as.integer(sub("^class_fac", "", a_rows$term[i])))
      indirect_df <- rbind(
        indirect_df,
        data.frame(
          outcome = outcome_var,
          outcome_label = lookup_label(outcome_var),
          mediator = mediator_var,
          mediator_label = lookup_label(mediator_var),
          class_term = a_rows$term[i],
          class_contrast = paste0("Class ", cls_num, " vs Class ", ref_class),
          a = a_est,
          a_se = a_se,
          a_p = a_rows$p[i],
          b = b_est,
          b_se = b_se,
          b_p = b_p,
          indirect = indirect,
          se = ind_se,
          z_value = z_val,
          llci = llci,
          ulci = ulci,
          p = p_val,
          p_fmt = fmt_p_pm(p_val),
          sig = sig_mark_pm(p_val),
          stringsAsFactors = FALSE
        )
      )
    }
    if (isTRUE(bootstrap_enabled) && nrow(indirect_df) > 0) {
      boot_ind <- bootstrap_indirect_ci(
        dat = dat,
        fit_m_formula = stats::as.formula(form_m),
        fit_y_formula = stats::as.formula(form_y),
        class_terms = a_rows$term,
        mediator_var = mediator_var,
        n_boot = bootstrap_n,
        ci_type = bootstrap_ci
      )
      indirect_df <- merge(
        indirect_df[, setdiff(names(indirect_df), c("llci", "ulci")), drop = FALSE],
        boot_ind,
        by = "class_term",
        all.x = TRUE,
        sort = FALSE
      )
    }
  }

  sum_m <- summary(fit_m)
  fstat_m <- tryCatch(unname(sum_m$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))
  sum_y <- summary(fit_y)
  fstat_y <- tryCatch(unname(sum_y$fstatistic), error = function(e) c(NA_real_, NA_real_, NA_real_))

  model_summary <- rbind(
    data.frame(
      model_component = "Mediator model",
      outcome = mediator_var,
      outcome_label = lookup_label(mediator_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = stats::nobs(fit_m),
      r2 = sum_m$r.squared,
      adj_r2 = sum_m$adj.r.squared,
      f_value = fstat_m[1],
      df1 = fstat_m[2],
      df2 = fstat_m[3],
      p = tryCatch(stats::pf(fstat_m[1], fstat_m[2], fstat_m[3], lower.tail = FALSE), error = function(e) NA_real_),
      bootstrap_enabled = isTRUE(bootstrap_enabled),
      bootstrap_n = bootstrap_n,
      bootstrap_ci = bootstrap_ci,
      stringsAsFactors = FALSE
    ),
    data.frame(
      model_component = "Outcome model",
      outcome = outcome_var,
      outcome_label = lookup_label(outcome_var),
      mediator = mediator_var,
      mediator_label = lookup_label(mediator_var),
      target_outcome = outcome_var,
      target_outcome_label = lookup_label(outcome_var),
      n = stats::nobs(fit_y),
      r2 = sum_y$r.squared,
      adj_r2 = sum_y$adj.r.squared,
      f_value = fstat_y[1],
      df1 = fstat_y[2],
      df2 = fstat_y[3],
      p = tryCatch(stats::pf(fstat_y[1], fstat_y[2], fstat_y[3], lower.tail = FALSE), error = function(e) NA_real_),
      bootstrap_enabled = isTRUE(bootstrap_enabled),
      bootstrap_n = bootstrap_n,
      bootstrap_ci = bootstrap_ci,
      stringsAsFactors = FALSE
    )
  )
  model_summary$p_fmt <- fmt_p_pm(model_summary$p)
  model_summary$sig <- sig_mark_pm(model_summary$p)

  list(
    coefficients = rbind(coef_m, coef_y),
    summary = model_summary,
    indirect = indirect_df
  )
}

custom_helper_file_pm <- file.path(PROJECT_ROOT, "R", ANALYSIS_ID, "custom_model_helpers.R")
if (file.exists(custom_helper_file_pm)) {
  sys.source(custom_helper_file_pm, envir = environment())
}

results_list <- list()
summary_list <- list()
indirect_list <- list()
conditional_list <- list()
run_info_list <- list()
idx <- 0L

if (isTRUE(PROCESS_SETTINGS$custom_model_enabled)) {
  observed_custom <- length(PROCESS_SETTINGS$x_vars %||% character(0)) == 1L &&
    length(PROCESS_SETTINGS$y_vars %||% character(0)) == 1L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) >= 1L
  if (!observed_custom) {
    stop("Custom process model currently requires one X, one Y, and at least one mediator.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      idx <- idx + 1L
      log_info(
        "Fitting custom PROCESS model via Mplus",
        if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
        ": outcome = ", yy,
        ", x = ", xx,
        ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
        if (nzchar(PROCESS_SETTINGS$w_var %||% "")) paste0(", moderator = ", PROCESS_SETTINGS$w_var) else ""
      )
      fit_i <- fit_observed_mplus_custom_model(
        df = PROCESS_DATA,
        outcome_var = yy,
        x_var = xx,
        mediator_vars = PROCESS_SETTINGS$m_vars,
        w_var = PROCESS_SETTINGS$w_var,
        covariates = PROCESS_SETTINGS$covariates,
        survey_bundle = SURVEY_BUNDLE
      )
      if (is.null(fit_i)) next
      results_list[[idx]] <- fit_i$coefficients
      summary_list[[idx]] <- fit_i$summary
      indirect_list[[idx]] <- fit_i$indirect
      conditional_list[[idx]] <- fit_i$conditional_effects
      run_info_list[[idx]] <- fit_i$run_info
    }
  }
} else if (PROCESS_SETTINGS$process_model == 1L && !identical(PROCESS_SETTINGS$moderator_source, "latent_class")) {
  for (yy in PROCESS_SETTINGS$y_vars) {
    if (length(PROCESS_SETTINGS$x_vars) > 1L) {
      idx <- idx + 1L
      log_info(
        "Fitting Model 1 via Mplus COMPLEX: outcome = ", yy,
        ", x = ", paste(PROCESS_SETTINGS$x_vars, collapse = ", "),
        ", moderator = ", PROCESS_SETTINGS$w_var
      )
      fit_i <- fit_observed_mplus_model1_multi(
        df = PROCESS_DATA,
        outcome_var = yy,
        x_vars = PROCESS_SETTINGS$x_vars,
        w_var = PROCESS_SETTINGS$w_var,
        covariates = PROCESS_SETTINGS$covariates,
        survey_bundle = SURVEY_BUNDLE
      )
      if (is.null(fit_i)) next
      results_list[[idx]] <- fit_i$coefficients
      summary_list[[idx]] <- fit_i$summary
      conditional_list[[idx]] <- fit_i$conditional_effects
      run_info_list[[idx]] <- fit_i$run_info
    } else {
      for (xx in PROCESS_SETTINGS$x_vars) {
        idx <- idx + 1L
        log_info("Fitting Model 1 via Mplus COMPLEX: outcome = ", yy, ", x = ", xx, ", moderator = ", PROCESS_SETTINGS$w_var)
        fit_i <- fit_observed_mplus_model1(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 1L) {
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      idx <- idx + 1L
      log_info("Fitting Model 1: outcome = ", yy, ", x = ", xx, ", moderator = latent profile")
      fit_i <- fit_one_model(
        df = PROCESS_DATA,
        outcome_var = yy,
        x_var = xx,
        covariates = PROCESS_SETTINGS$covariates,
        ref_class = SOURCE_REFERENCE_CLASS,
        bootstrap_enabled = PROCESS_SETTINGS$bootstrap_enabled,
        bootstrap_n = PROCESS_SETTINGS$bootstrap_n,
        bootstrap_ci = PROCESS_SETTINGS$bootstrap_ci
      )
      if (is.null(fit_i)) next
      results_list[[idx]] <- fit_i$coefficients
      summary_list[[idx]] <- fit_i$summary
      conditional_list[[idx]] <- fit_i$conditional_effects
    }
  }
} else if (PROCESS_SETTINGS$process_model == 4L) {
  observed_model4 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L
  if (observed_model4) {
    for (yy in PROCESS_SETTINGS$y_vars) {
      for (xx in PROCESS_SETTINGS$x_vars) {
        if (length(PROCESS_SETTINGS$m_vars) > 1L) {
          idx <- idx + 1L
          log_info("Fitting Model 4 via Mplus", if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "", ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "))
          fit_i <- fit_observed_mplus_model4_parallel(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_vars = PROCESS_SETTINGS$m_vars,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          run_info_list[[idx]] <- fit_i$run_info
        } else {
          for (mm in PROCESS_SETTINGS$m_vars) {
            idx <- idx + 1L
            log_info("Fitting Model 4 via Mplus", if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "", ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm)
            fit_i <- fit_observed_mplus_model4(
              df = PROCESS_DATA,
              outcome_var = yy,
              x_var = xx,
              mediator_var = mm,
              covariates = PROCESS_SETTINGS$covariates,
              survey_bundle = SURVEY_BUNDLE
            )
            if (is.null(fit_i)) next
            results_list[[idx]] <- fit_i$coefficients
            summary_list[[idx]] <- fit_i$summary
            indirect_list[[idx]] <- fit_i$indirect
            run_info_list[[idx]] <- fit_i$run_info
          }
        }
      }
    }
  } else {
    for (yy in PROCESS_SETTINGS$y_vars) {
      for (mm in PROCESS_SETTINGS$m_vars) {
        idx <- idx + 1L
        log_info("Fitting Model 4: outcome = ", yy, ", mediator = ", mm)
        fit_i <- fit_mediation_model4(
          df = PROCESS_DATA,
          outcome_var = yy,
          mediator_var = mm,
          covariates = PROCESS_SETTINGS$covariates,
          ref_class = SOURCE_REFERENCE_CLASS,
          bootstrap_enabled = PROCESS_SETTINGS$bootstrap_enabled,
          bootstrap_n = PROCESS_SETTINGS$bootstrap_n,
          bootstrap_ci = PROCESS_SETTINGS$bootstrap_ci
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 5L) {
  observed_model5 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model5) {
    stop("Current PROCESS model 5 implementation supports observed-variable moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 5 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model5_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 5 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model5(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 7L) {
  observed_model7 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model7) {
    stop("Current PROCESS model 7 implementation supports observed-variable first-stage moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 7 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model7_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 7 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model7(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 8L) {
  observed_model8 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model8) {
    stop("Current PROCESS model 8 implementation supports observed-variable first-stage and direct-effect moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 8 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model8_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 8 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model8(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 14L) {
  observed_model14 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model14) {
    stop("Current PROCESS model 14 implementation supports observed-variable second-stage moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 14 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model14_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 14 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model14(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 15L) {
  observed_model15 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model15) {
    stop("Current PROCESS model 15 implementation supports observed-variable moderated direct and second-stage moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 15 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model15_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 15 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model15(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 58L) {
  observed_model58 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model58) {
    stop("Current PROCESS model 58 implementation supports observed-variable first- and second-stage moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 58 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model58_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 58 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model58(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 59L) {
  observed_model59 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L &&
    length(PROCESS_SETTINGS$m_vars %||% character(0)) > 0L &&
    nzchar(PROCESS_SETTINGS$w_var %||% "")
  if (!observed_model59) {
    stop("Current PROCESS model 59 implementation supports observed-variable first-, second-stage, and direct-effect moderated mediation only.", call. = FALSE)
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      if (length(PROCESS_SETTINGS$m_vars %||% character(0)) > 1L) {
        idx <- idx + 1L
        log_info(
          "Fitting Model 59 via Mplus",
          if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
          ": outcome = ", yy, ", x = ", xx, ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = ", "),
          ", moderator = ", PROCESS_SETTINGS$w_var
        )
        fit_i <- fit_observed_mplus_model59_parallel(
          df = PROCESS_DATA,
          outcome_var = yy,
          x_var = xx,
          mediator_vars = PROCESS_SETTINGS$m_vars,
          w_var = PROCESS_SETTINGS$w_var,
          covariates = PROCESS_SETTINGS$covariates,
          survey_bundle = SURVEY_BUNDLE
        )
        if (is.null(fit_i)) next
        results_list[[idx]] <- fit_i$coefficients
        summary_list[[idx]] <- fit_i$summary
        indirect_list[[idx]] <- fit_i$indirect
        conditional_list[[idx]] <- fit_i$conditional_effects
        run_info_list[[idx]] <- fit_i$run_info
      } else {
        for (mm in PROCESS_SETTINGS$m_vars) {
          idx <- idx + 1L
          log_info(
            "Fitting Model 59 via Mplus",
            if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
            ": outcome = ", yy, ", x = ", xx, ", mediator = ", mm, ", moderator = ", PROCESS_SETTINGS$w_var
          )
          fit_i <- fit_observed_mplus_model59(
            df = PROCESS_DATA,
            outcome_var = yy,
            x_var = xx,
            mediator_var = mm,
            w_var = PROCESS_SETTINGS$w_var,
            covariates = PROCESS_SETTINGS$covariates,
            survey_bundle = SURVEY_BUNDLE
          )
          if (is.null(fit_i)) next
          results_list[[idx]] <- fit_i$coefficients
          summary_list[[idx]] <- fit_i$summary
          indirect_list[[idx]] <- fit_i$indirect
          conditional_list[[idx]] <- fit_i$conditional_effects
          run_info_list[[idx]] <- fit_i$run_info
        }
      }
    }
  }
} else if (PROCESS_SETTINGS$process_model == 6L) {
  observed_model6 <- length(PROCESS_SETTINGS$x_vars %||% character(0)) > 0L
  if (!observed_model6) {
    stop("Current PROCESS model 6 implementation supports observed serial mediation only.", call. = FALSE)
  }
  if (!length(PROCESS_SETTINGS$m_vars %||% character(0)) %in% c(2L, 3L)) {
    stop(
      "PROCESS model 6 requires 2 or 3 mediators in serial order. Current m count = ",
      length(PROCESS_SETTINGS$m_vars %||% character(0)),
      ".",
      call. = FALSE
    )
  }
  for (yy in PROCESS_SETTINGS$y_vars) {
    for (xx in PROCESS_SETTINGS$x_vars) {
      idx <- idx + 1L
      log_info(
        "Fitting Model 6 via Mplus",
        if (!identical(PROCESS_SETTINGS$variance_method %||% "bootstrap", "bootstrap")) " COMPLEX" else "",
        ": outcome = ", yy,
        ", x = ", xx,
        ", mediators = ", paste(PROCESS_SETTINGS$m_vars, collapse = " -> ")
      )
      fit_i <- fit_observed_mplus_model6(
        df = PROCESS_DATA,
        outcome_var = yy,
        x_var = xx,
        mediator_vars = PROCESS_SETTINGS$m_vars,
        covariates = PROCESS_SETTINGS$covariates,
        survey_bundle = SURVEY_BUNDLE
      )
      if (is.null(fit_i)) next
      results_list[[idx]] <- fit_i$coefficients
      summary_list[[idx]] <- fit_i$summary
      indirect_list[[idx]] <- fit_i$indirect
      run_info_list[[idx]] <- fit_i$run_info
    }
  }
} else {
  stop("Current process_macro implementation supports Model 1, Model 4, Model 5, Model 6, Model 7, Model 8, Model 14, Model 15, Model 58, and Model 59.", call. = FALSE)
}

PROCESS_MODEL_RESULTS <- if (length(results_list) > 0) do.call(rbind, results_list) else data.frame()
PROCESS_MODEL_SUMMARY <- if (length(summary_list) > 0) do.call(rbind, summary_list) else data.frame()
PROCESS_INDIRECT_RESULTS <- if (length(indirect_list) > 0) do.call(rbind, indirect_list) else data.frame()
PROCESS_CONDITIONAL_EFFECTS <- if (length(conditional_list) > 0) do.call(rbind, conditional_list) else data.frame()
PROCESS_MPLUS_RUN_INFO <- if (length(run_info_list) > 0) do.call(rbind, run_info_list) else data.frame()

MODEL_RUN_SUMMARY <- list(
  process_model = PROCESS_SETTINGS$process_model,
  source_analysis_id = PROCESS_SETTINGS$source_analysis_id,
  moderator_source = PROCESS_SETTINGS$moderator_source,
  bootstrap_enabled = PROCESS_SETTINGS$bootstrap_enabled,
  bootstrap_n = PROCESS_SETTINGS$bootstrap_n,
  bootstrap_ci = PROCESS_SETTINGS$bootstrap_ci,
  reference_class = SOURCE_REFERENCE_CLASS,
  reference_class_label = SOURCE_REFERENCE_CLASS_LABEL,
  n_models = nrow(PROCESS_MODEL_SUMMARY),
  n_rows = nrow(PROCESS_MODEL_RESULTS),
  n_indirect_rows = nrow(PROCESS_INDIRECT_RESULTS),
  n_conditional_rows = nrow(PROCESS_CONDITIONAL_EFFECTS),
  n_mplus_runs = nrow(PROCESS_MPLUS_RUN_INFO),
  created_at = Sys.time()
)

save_named_rds_list(
  list(
    PROCESS_MODEL_RESULTS = PROCESS_MODEL_RESULTS,
    PROCESS_MODEL_SUMMARY = PROCESS_MODEL_SUMMARY,
    PROCESS_INDIRECT_RESULTS = PROCESS_INDIRECT_RESULTS,
    PROCESS_CONDITIONAL_EFFECTS = PROCESS_CONDITIONAL_EFFECTS,
    PROCESS_MPLUS_RUN_INFO = PROCESS_MPLUS_RUN_INFO,
    MODEL_RUN_SUMMARY = MODEL_RUN_SUMMARY
  ),
  dir_rds = DIR_RDS
)

elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_MODEL, units = "secs")), 2)
log_info("03_model_run.R completed.")
log_info("n(models)          = ", nrow(PROCESS_MODEL_SUMMARY))
log_info("n(coef rows)       = ", nrow(PROCESS_MODEL_RESULTS))
log_info("n(indirect rows)   = ", nrow(PROCESS_INDIRECT_RESULTS))
log_info("n(conditional rows)= ", nrow(PROCESS_CONDITIONAL_EFFECTS))
log_info("bootstrap_enabled  = ", PROCESS_SETTINGS$bootstrap_enabled)
log_info("bootstrap_n        = ", PROCESS_SETTINGS$bootstrap_n)
log_info("bootstrap_ci       = ", PROCESS_SETTINGS$bootstrap_ci)
log_info("elapsed            = ", elapsed_sec, " sec")
log_step_end("model_run", elapsed_sec, ok = TRUE)
