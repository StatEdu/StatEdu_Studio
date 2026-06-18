T0_PREP <- Sys.time()

log_step_start("PREP", "03_prep.R")
log_info("Preparing Mplus-ready state-transition data ...")

CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)
DICT <- load_step_rds("DICT", dir_rds = DIR_RDS, default = list())
LONGITUDINAL_SPEC <- load_step_rds("LONGITUDINAL_SPEC", dir_rds = DIR_RDS, required = TRUE)
PANEL_LONG <- load_step_rds("PANEL_LONG", dir_rds = DIR_RDS, required = TRUE)
PANEL_WIDE <- load_step_rds("PANEL_WIDE", dir_rds = DIR_RDS, required = TRUE)
PANEL_META <- load_step_rds("PANEL_META", dir_rds = DIR_RDS, required = TRUE)

make_state_label_map <- function(dict = NULL, cfg = NULL, state_var_names = character(0)) {
  dict_levels <- if (is.list(dict) && is.data.frame(dict$levels)) dict$levels else data.frame()
  state_var_names <- unique(as.character(state_var_names))
  state_var_names <- state_var_names[!is.na(state_var_names) & nzchar(state_var_names)]

  if (is.data.frame(dict_levels) && nrow(dict_levels) > 0 &&
      all(c("var_name", "value", "value_label") %in% names(dict_levels))) {
    lv <- dict_levels[dict_levels$var_name %in% state_var_names, c("value", "value_label"), drop = FALSE]
    lv <- lv[!is.na(lv$value) & nzchar(as.character(lv$value)), , drop = FALSE]
    lv <- lv[!duplicated(as.character(lv$value)), , drop = FALSE]
    if (nrow(lv) > 0) {
      vals <- as.character(lv$value_label)
      nms <- as.character(lv$value)
      keep <- !is.na(vals) & nzchar(vals) & !is.na(nms) & nzchar(nms)
      if (any(keep)) return(stats::setNames(vals[keep], nms[keep]))
    }
  }

  x <- cfg$longitudinal$state_labels %||% cfg$state_labels %||% NULL
  if (is.null(x)) return(setNames(character(0), character(0)))
  if (is.list(x)) {
    vals <- vapply(x, function(z) as.character(z %||% NA_character_)[1], character(1))
    nms <- names(vals) %||% character(0)
    keep <- !is.na(vals) & nzchar(vals) & !is.na(nms) & nzchar(nms)
    return(stats::setNames(vals[keep], as.character(nms[keep])))
  }
  x <- as.character(x)
  stats::setNames(x, names(x))
}

resolve_reference_state <- function(cfg, dict, state_values, state_label_map, state_var_names = character(0)) {
  dict_meta <- if (is.list(dict) && is.data.frame(dict$meta)) dict$meta else data.frame()
  dict_levels <- if (is.list(dict) && is.data.frame(dict$levels)) dict$levels else data.frame()

  state_var_names <- unique(as.character(state_var_names))
  state_var_names <- state_var_names[!is.na(state_var_names) & nzchar(state_var_names)]

  if (is.data.frame(dict_levels) && nrow(dict_levels) > 0 &&
      all(c("var_name", "value", "reference") %in% names(dict_levels))) {
    lv <- dict_levels[dict_levels$var_name %in% state_var_names, , drop = FALSE]
    ref_rows <- lv[
      !is.na(lv$reference) &
        nzchar(trimws(as.character(lv$reference))) &
        tolower(trimws(as.character(lv$reference))) %in% c("true", "t", "1", "yes", "y", "ref"),
      ,
      drop = FALSE
    ]
    if (nrow(ref_rows) > 0) {
      ref_num <- suppressWarnings(as.integer(as.character(ref_rows$value[1])))
      if (!is.na(ref_num) && ref_num %in% state_values) return(ref_num)
    }
  }

  if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && "reference" %in% names(dict_meta)) {
    meta_hit <- dict_meta[dict_meta$var_name %in% state_var_names, , drop = FALSE]
    ref_meta <- meta_hit$reference[!is.na(meta_hit$reference) & nzchar(trimws(as.character(meta_hit$reference)))]
    if (length(ref_meta) > 0) {
      ref_chr0 <- trimws(as.character(ref_meta[1]))
      ref_num0 <- suppressWarnings(as.integer(ref_chr0))
      if (!is.na(ref_num0) && ref_num0 %in% state_values) return(ref_num0)
      if (length(state_label_map) > 0) {
        hit0 <- names(state_label_map)[tolower(as.character(state_label_map)) == tolower(ref_chr0)]
        hit_num0 <- suppressWarnings(as.integer(hit0))
        hit_num0 <- hit_num0[!is.na(hit_num0) & hit_num0 %in% state_values]
        if (length(hit_num0) > 0) return(hit_num0[1])
      }
    }
  }

  ref_raw <- cfg$state_transition$reference_state %||%
    cfg$state_transition$reference_label %||%
    cfg$longitudinal$reference_state %||%
    cfg$longitudinal$reference_label %||%
    NULL

  if (is.null(ref_raw) || length(ref_raw) == 0 || is.na(ref_raw[1]) || !nzchar(trimws(as.character(ref_raw[1])))) {
    return(state_values[1])
  }

  ref_chr <- trimws(as.character(ref_raw[1]))
  ref_num <- suppressWarnings(as.integer(ref_chr))
  if (!is.na(ref_num) && ref_num %in% state_values) {
    return(ref_num)
  }

  if (length(state_label_map) > 0) {
    label_vals <- as.character(state_label_map)
    hit <- names(state_label_map)[tolower(label_vals) == tolower(ref_chr)]
    hit_num <- suppressWarnings(as.integer(hit))
    hit_num <- hit_num[!is.na(hit_num) & hit_num %in% state_values]
    if (length(hit_num) > 0) return(hit_num[1])
  }

  stop(
    "Configured reference_state/reference_label was not found in observed states: ",
    ref_chr,
    call. = FALSE
  )
}

ANALYSIS_PANEL_LONG <- PANEL_LONG
ANALYSIS_PANEL_LONG <- ANALYSIS_PANEL_LONG[!is.na(ANALYSIS_PANEL_LONG$panel_id), , drop = FALSE]
ANALYSIS_PANEL_LONG <- ANALYSIS_PANEL_LONG[!is.na(ANALYSIS_PANEL_LONG$panel_wave_index), , drop = FALSE]
ANALYSIS_PANEL_LONG <- ANALYSIS_PANEL_LONG[order(ANALYSIS_PANEL_LONG$panel_id, ANALYSIS_PANEL_LONG$panel_wave_index), , drop = FALSE]
rownames(ANALYSIS_PANEL_LONG) <- NULL

TRANSITION_DATA <- build_transition_pairs(ANALYSIS_PANEL_LONG)

wave_order <- PANEL_META$wave_order %||% LONGITUDINAL_SPEC$wave_order %||% character(0)
state_wide_map <- LONGITUDINAL_SPEC$state_wide_map %||% character(0)
state_wide_map <- state_wide_map[names(state_wide_map) %in% wave_order]
state_label_map <- make_state_label_map(DICT, CFG, unname(state_wide_map))

if (!is.data.frame(PANEL_WIDE) || nrow(PANEL_WIDE) == 0) {
  stop("PANEL_WIDE is missing or empty.", call. = FALSE)
}

id_candidates <- intersect(c("panel_id", LONGITUDINAL_SPEC$id_var %||% "panel_id"), names(PANEL_WIDE))
if (length(id_candidates) == 0) {
  stop("No valid ID variable found in PANEL_WIDE.", call. = FALSE)
}
id_var_wide <- id_candidates[1]

state_values <- sort(unique(stats::na.omit(ANALYSIS_PANEL_LONG$state)))
state_values <- state_values[!is.na(state_values)]
if (length(state_values) < 2) {
  stop("At least two observed states are required for transition modeling.", call. = FALSE)
}
reference_state <- resolve_reference_state(
  cfg = CFG,
  dict = DICT,
  state_values = state_values,
  state_label_map = state_label_map,
  state_var_names = unname(state_wide_map)
)
mplus_state_order <- c(setdiff(state_values, reference_state), reference_state)
state_to_mplus <- stats::setNames(seq_along(mplus_state_order), as.character(mplus_state_order))
state_from_mplus <- stats::setNames(mplus_state_order, as.character(seq_along(mplus_state_order)))
reference_state_mplus <- unname(state_to_mplus[as.character(reference_state)])

state_vars <- unname(state_wide_map)
state_vars <- state_vars[state_vars %in% names(PANEL_WIDE)]
if (length(state_vars) < 2) {
  stop("At least two wide-form state variables are required for Mplus transition modeling.", call. = FALSE)
}

dict_meta <- if (is.list(DICT) && is.data.frame(DICT$meta)) DICT$meta else data.frame()
dict_levels <- if (is.list(DICT) && is.data.frame(DICT$levels)) DICT$levels else data.frame()

normalize_var_key <- function(x) {
  gsub("[^a-z0-9]+", "", tolower(as.character(x)))
}

resolve_panel_var_name <- function(var_name) {
  var_name <- as.character(var_name)
  if (var_name %in% names(PANEL_WIDE)) return(var_name)
  panel_keys <- normalize_var_key(names(PANEL_WIDE))
  hit <- which(panel_keys == normalize_var_key(var_name))
  if (length(hit) > 0) return(names(PANEL_WIDE)[hit[1]])
  NA_character_
}

covariate_dict_aliases <- new.env(parent = emptyenv())

resolve_dict_var_name <- function(var_name) {
  var_name <- as.character(var_name)
  if (exists(var_name, envir = covariate_dict_aliases, inherits = FALSE)) {
    return(get(var_name, envir = covariate_dict_aliases, inherits = FALSE))
  }
  var_name
}

get_covariate_meta <- function(var_name) {
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0 || !"var_name" %in% names(dict_meta)) return(data.frame())
  dict_var <- resolve_dict_var_name(var_name)
  dict_meta[as.character(dict_meta$var_name) == dict_var, , drop = FALSE]
}

get_covariate_levels <- function(var_name) {
  if (!is.data.frame(dict_levels) || nrow(dict_levels) == 0 || !"var_name" %in% names(dict_levels)) return(data.frame())
  dict_var <- resolve_dict_var_name(var_name)
  dict_levels[as.character(dict_levels$var_name) == dict_var, , drop = FALSE]
}

resolve_covariate_type <- function(var_name) {
  hit <- get_covariate_meta(var_name)
  if (!is.data.frame(hit) || nrow(hit) == 0) return("continuous")
  cand_cols <- intersect(c("type", "subtype"), names(hit))
  vals <- unique(tolower(trimws(as.character(unlist(hit[, cand_cols, drop = FALSE], use.names = FALSE)))))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (any(vals %in% c("categorical", "factor", "binary", "ordinal", "nominal"))) return("categorical")
  "continuous"
}

resolve_covariate_reference <- function(var_name, observed_levels = character(0)) {
  lv <- get_covariate_levels(var_name)
  if (is.data.frame(lv) && nrow(lv) > 0 && all(c("value", "reference") %in% names(lv))) {
    ref_rows <- lv[
      !is.na(lv$reference) &
        nzchar(trimws(as.character(lv$reference))) &
        trimws(as.character(lv$reference)) %in% c("1", "TRUE", "true", "T", "t", "YES", "yes", "Y", "y", "ref", "REF"),
      ,
      drop = FALSE
    ]
    if (nrow(ref_rows) > 0) return(as.character(ref_rows$value[1]))
  }
  hit <- get_covariate_meta(var_name)
  if (is.data.frame(hit) && nrow(hit) > 0 && "reference" %in% names(hit)) {
    ref <- trimws(as.character(hit$reference[1]))
    if (!is.na(ref) && nzchar(ref)) return(ref)
  }
  observed_levels <- as.character(observed_levels)
  observed_levels <- observed_levels[!is.na(observed_levels) & nzchar(observed_levels)]
  if (length(observed_levels) == 0) return(NA_character_)
  observed_levels[1]
}

resolve_covariate_label <- function(var_name) {
  hit <- get_covariate_meta(var_name)
  if (is.data.frame(hit) && nrow(hit) > 0 && "var_label" %in% names(hit)) {
    val <- as.character(hit$var_label[1])
    if (!is.na(val) && nzchar(val)) return(val)
  }
  var_name
}

resolve_value_label <- function(var_name, value) {
  lv <- get_covariate_levels(var_name)
  if (is.data.frame(lv) && nrow(lv) > 0 && all(c("value", "value_label") %in% names(lv))) {
    hit <- lv[as.character(lv$value) == as.character(value), , drop = FALSE]
    if (nrow(hit) > 0) {
      val <- as.character(hit$value_label[1])
      if (!is.na(val) && nzchar(val)) return(val)
    }
  }
  as.character(value)
}

resolve_time_varying_covariate_map <- function(dict_meta, covariates, wave_order, alias_map = NULL) {
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0 || length(covariates) == 0 || length(wave_order) == 0) {
    return(data.frame())
  }
  if (!"var_name" %in% names(dict_meta)) return(data.frame())

  covariates <- unique(as.character(covariates))
  if (is.null(alias_map)) {
    alias_map <- stats::setNames(covariates, covariates)
  }
  alias_map <- alias_map[names(alias_map) %in% covariates]
  dict_names <- unique(c(covariates, unname(alias_map)))

  meta <- dict_meta[as.character(dict_meta$var_name) %in% dict_names, , drop = FALSE]
  if (nrow(meta) == 0) return(data.frame())

  display_group <- if ("display_group" %in% names(meta)) tolower(trimws(as.character(meta$display_group))) else rep("", nrow(meta))
  description <- if ("description" %in% names(meta)) tolower(trimws(as.character(meta$description))) else rep("", nrow(meta))
  label <- if ("var_label" %in% names(meta)) trimws(as.character(meta$var_label)) else as.character(meta$var_name)

  is_time_varying <- display_group %in% c("time_varying", "time-varying", "time varying", "future", "wave", "longitudinal") |
    grepl("time[-_ ]*depend|time[-_ ]*vary|time[-_ ]*variant|wave[-_ ]*vary|wave[-_ ]*variant", description)
  meta <- meta[is_time_varying, , drop = FALSE]
  label <- label[is_time_varying]
  if (nrow(meta) == 0) return(data.frame())

  group_key <- gsub("[^a-z0-9]+", "_", tolower(label))
  group_key[is.na(group_key) | !nzchar(group_key)] <- tolower(as.character(meta$var_name[is.na(group_key) | !nzchar(group_key)]))
  meta$time_varying_group <- group_key

  actual_by_dict <- stats::setNames(names(alias_map), unname(alias_map))

  out_list <- list()
  idx <- 1L
  for (grp in unique(meta$time_varying_group)) {
    sub <- meta[meta$time_varying_group == grp, , drop = FALSE]
    ord <- if ("display_order" %in% names(sub)) suppressWarnings(as.numeric(sub$display_order)) else seq_len(nrow(sub))
    ord[is.na(ord)] <- seq_len(nrow(sub))[is.na(ord)]
    sub <- sub[order(ord, as.character(sub$var_name)), , drop = FALSE]
    n_map <- min(nrow(sub), length(wave_order))
    if (n_map == 0) next
    out_list[[idx]] <- data.frame(
      var_name = unname(actual_by_dict[as.character(sub$var_name[seq_len(n_map)])]),
      dict_var_name = as.character(sub$var_name[seq_len(n_map)]),
      wave = as.character(wave_order[seq_len(n_map)]),
      time_varying_group = grp,
      stringsAsFactors = FALSE
    )
    missing_actual <- is.na(out_list[[idx]]$var_name) | !nzchar(out_list[[idx]]$var_name)
    out_list[[idx]]$var_name[missing_actual] <- out_list[[idx]]$dict_var_name[missing_actual]
    idx <- idx + 1L
  }

  if (length(out_list) == 0) return(data.frame())
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

dict_covariates <- character(0)
if (is.data.frame(dict_meta) && nrow(dict_meta) > 0 && "var_name" %in% names(dict_meta)) {
  role_col <- if ("analysis_role" %in% names(dict_meta)) "analysis_role" else if ("role" %in% names(dict_meta)) "role" else NULL
  use_col <- if ("use_var" %in% names(dict_meta)) "use_var" else if ("use" %in% names(dict_meta)) "use" else NULL
  if (!is.null(role_col)) {
    role_vals <- tolower(trimws(as.character(dict_meta[[role_col]])))
    use_vals <- if (!is.null(use_col)) as.logical(dict_meta[[use_col]]) else rep(TRUE, nrow(dict_meta))
    use_vals[is.na(use_vals)] <- TRUE
    dict_covariates <- unique(as.character(dict_meta$var_name[role_vals == "covariate" & use_vals]))
    dict_covariates <- dict_covariates[nzchar(dict_covariates)]
  }
}

baseline_covariates <- if (length(dict_covariates) > 0) {
  dict_covariates
} else {
  CFG$state_transition$baseline_covariates %||%
    CFG$longitudinal$baseline_covariates %||%
    CFG$longitudinal$invariant_vars %||%
    character(0)
}
requested_covariates <- unique(as.character(unlist(baseline_covariates, use.names = FALSE)))
requested_covariates <- requested_covariates[!is.na(requested_covariates) & nzchar(requested_covariates)]
resolved_covariates <- vapply(requested_covariates, resolve_panel_var_name, character(1))
keep_covariates <- !is.na(resolved_covariates) & nzchar(resolved_covariates)
covariate_alias_map <- stats::setNames(requested_covariates[keep_covariates], resolved_covariates[keep_covariates])
for (actual_name in names(covariate_alias_map)) {
  assign(actual_name, unname(covariate_alias_map[[actual_name]]), envir = covariate_dict_aliases)
}
baseline_covariates <- unique(unname(resolved_covariates[keep_covariates]))

time_varying_covariate_map <- resolve_time_varying_covariate_map(
  dict_meta,
  baseline_covariates,
  wave_order,
  alias_map = covariate_alias_map
)
time_varying_covariates <- if (is.data.frame(time_varying_covariate_map) && nrow(time_varying_covariate_map) > 0) {
  unique(as.character(time_varying_covariate_map$var_name))
} else {
  character(0)
}
baseline_covariates <- setdiff(baseline_covariates, time_varying_covariates)
model_covariates <- unique(c(baseline_covariates, time_varying_covariates))

MPLUS_DATA <- data.frame(panel_id = as.character(PANEL_WIDE[[id_var_wide]]), stringsAsFactors = FALSE)

for (wv in names(state_wide_map)) {
  col_i <- state_wide_map[[wv]]
  if (!col_i %in% names(PANEL_WIDE)) next
  raw_state_i <- suppressWarnings(as.integer(as.numeric(PANEL_WIDE[[col_i]])))
  MPLUS_DATA[[wv]] <- unname(state_to_mplus[as.character(raw_state_i)])
  MPLUS_DATA[[wv]][is.na(raw_state_i)] <- NA_integer_
}

covariate_specs <- list()
if (length(model_covariates) > 0) {
  spec_idx <- 1L
  for (cv in model_covariates) {
    x <- PANEL_WIDE[[cv]]
    cv_type <- resolve_covariate_type(cv)
    cv_label <- resolve_covariate_label(cv)
    is_time_varying_cv <- cv %in% time_varying_covariates
    wave_i <- if (is_time_varying_cv && is.data.frame(time_varying_covariate_map) && nrow(time_varying_covariate_map) > 0) {
      hit <- time_varying_covariate_map$wave[time_varying_covariate_map$var_name == cv]
      if (length(hit) > 0) as.character(hit[1]) else NA_character_
    } else {
      NA_character_
    }
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) x <- as.integer(x)

    if (identical(cv_type, "categorical")) {
      x_chr <- as.character(x)
      x_chr[trimws(x_chr) == ""] <- NA_character_
      obs_levels <- unique(x_chr[!is.na(x_chr)])
      lv <- get_covariate_levels(cv)
      dict_level_values <- if (is.data.frame(lv) && nrow(lv) > 0 && "value" %in% names(lv)) as.character(lv$value) else character(0)
      dict_level_values <- dict_level_values[!is.na(dict_level_values) & nzchar(dict_level_values)]
      level_values <- unique(c(dict_level_values, obs_levels))
      ref_level <- resolve_covariate_reference(cv, observed_levels = level_values)
      level_values <- level_values[!is.na(level_values) & nzchar(level_values)]
      if (!is.na(ref_level) && nzchar(ref_level) && ref_level %in% level_values) {
        level_values <- c(ref_level, setdiff(level_values, ref_level))
      }
      nonref_levels <- setdiff(level_values, ref_level)
      for (lev in nonref_levels) {
        dummy_name <- paste0(cv, "__", make.names(lev))
        MPLUS_DATA[[dummy_name]] <- ifelse(
          is.na(x_chr), NA_real_,
          ifelse(x_chr == lev, 1, 0)
        )
        dummy_nonmiss <- MPLUS_DATA[[dummy_name]][!is.na(MPLUS_DATA[[dummy_name]])]
        covariate_specs[[spec_idx]] <- list(
          name = dummy_name,
          source_var = cv,
          source_label = cv_label,
          covariate_scope = if (is_time_varying_cv) "time_varying" else "baseline",
          wave = wave_i,
          predictor_type = "categorical",
          level = as.character(lev),
          value_label = resolve_value_label(cv, lev),
          reference_level = as.character(ref_level),
          reference_label = resolve_value_label(cv, ref_level),
          unique_n = length(unique(dummy_nonmiss)),
          is_active = length(unique(dummy_nonmiss)) >= 2
        )
        spec_idx <- spec_idx + 1L
      }
    } else {
      suppressWarnings(x_num <- as.numeric(x))
      x_num[is.infinite(x_num)] <- NA_real_
      MPLUS_DATA[[cv]] <- x_num
      x_nonmiss <- x_num[!is.na(x_num)]
      covariate_specs[[spec_idx]] <- list(
        name = cv,
        source_var = cv,
        source_label = cv_label,
        covariate_scope = if (is_time_varying_cv) "time_varying" else "baseline",
        wave = wave_i,
        predictor_type = "continuous",
        level = NA_character_,
        value_label = NA_character_,
        reference_level = NA_character_,
        reference_label = NA_character_,
        unique_n = length(unique(x_nonmiss)),
        is_active = length(unique(x_nonmiss)) >= 2
      )
      spec_idx <- spec_idx + 1L
    }
  }
}
covariate_specs <- covariate_specs[!vapply(covariate_specs, is.null, logical(1))]

bmi_mediation <- list(enabled = FALSE)
if (is.data.frame(time_varying_covariate_map) && nrow(time_varying_covariate_map) > 0) {
  bmi_rows <- time_varying_covariate_map[
    tolower(as.character(time_varying_covariate_map$time_varying_group)) %in% c("bmi", "body_mass_index") |
      grepl("bmi|body.*mass", tolower(as.character(time_varying_covariate_map$time_varying_group))),
    ,
    drop = FALSE
  ]
  if (nrow(bmi_rows) == 0) {
    bmi_rows <- time_varying_covariate_map[
      grepl("bmi|body.*mass", tolower(as.character(time_varying_covariate_map$var_name))),
      ,
      drop = FALSE
    ]
  }
  if (nrow(bmi_rows) >= 2 && length(wave_order) >= 2) {
    bmi_by_wave <- stats::setNames(as.character(bmi_rows$var_name), as.character(bmi_rows$wave))
    mediator_specs <- list()
    med_idx <- 1L
    for (i in seq_len(length(wave_order) - 1L)) {
      from_wave <- wave_order[i]
      to_wave <- wave_order[i + 1L]
      from_bmi <- unname(bmi_by_wave[from_wave])
      to_bmi <- unname(bmi_by_wave[to_wave])
      if (is.na(from_bmi) || is.na(to_bmi) || !from_bmi %in% names(PANEL_WIDE) || !to_bmi %in% names(PANEL_WIDE)) next
      bmi_type <- resolve_covariate_type(from_bmi)
      if (identical(bmi_type, "categorical")) {
        from_chr <- as.character(PANEL_WIDE[[from_bmi]])
        to_chr <- as.character(PANEL_WIDE[[to_bmi]])
        med_raw <- ifelse(is.na(from_chr) | is.na(to_chr) | !nzchar(from_chr) | !nzchar(to_chr), NA_character_, paste0(from_chr, "_to_", to_chr))
        ref_from <- resolve_covariate_reference(from_bmi, observed_levels = unique(from_chr[!is.na(from_chr)]))
        ref_to <- resolve_covariate_reference(to_bmi, observed_levels = unique(to_chr[!is.na(to_chr)]))
        ref_med <- paste0(ref_from, "_to_", ref_to)
        med_levels <- unique(med_raw[!is.na(med_raw) & nzchar(med_raw)])
        med_levels <- c(ref_med, setdiff(med_levels, ref_med))
        for (lev in setdiff(med_levels, ref_med)) {
          med_name <- paste0("bmi_tr_", from_wave, "_", to_wave, "__", make.names(lev))
          MPLUS_DATA[[med_name]] <- ifelse(is.na(med_raw), NA_real_, ifelse(med_raw == lev, 1, 0))
          med_nonmiss <- MPLUS_DATA[[med_name]][!is.na(MPLUS_DATA[[med_name]])]
          parts <- strsplit(lev, "_to_", fixed = TRUE)[[1]]
          ref_parts <- strsplit(ref_med, "_to_", fixed = TRUE)[[1]]
          lev_label <- paste0(resolve_value_label(from_bmi, parts[1]), " -> ", resolve_value_label(to_bmi, parts[2]))
          ref_label <- paste0(resolve_value_label(from_bmi, ref_parts[1]), " -> ", resolve_value_label(to_bmi, ref_parts[2]))
          mediator_specs[[med_idx]] <- list(
            name = med_name,
            source_var = paste0("BMI_transition_", from_wave, "_", to_wave),
            source_label = "BMI transition",
            covariate_scope = "mediator",
            from_wave = from_wave,
            to_wave = to_wave,
            predictor_type = "categorical",
            level = lev,
            value_label = lev_label,
            reference_level = ref_med,
            reference_label = ref_label,
            unique_n = length(unique(med_nonmiss)),
            is_active = length(unique(med_nonmiss)) >= 2
          )
          med_idx <- med_idx + 1L
        }
      } else {
        med_name <- paste0("bmi_delta_", from_wave, "_", to_wave)
        suppressWarnings(from_x <- as.numeric(PANEL_WIDE[[from_bmi]]))
        suppressWarnings(to_x <- as.numeric(PANEL_WIDE[[to_bmi]]))
        MPLUS_DATA[[med_name]] <- to_x - from_x
        med_nonmiss <- MPLUS_DATA[[med_name]][!is.na(MPLUS_DATA[[med_name]])]
        mediator_specs[[med_idx]] <- list(
          name = med_name,
          source_var = "BMI_change",
          source_label = "BMI change",
          covariate_scope = "mediator",
          from_wave = from_wave,
          to_wave = to_wave,
          predictor_type = "continuous",
          level = NA_character_,
          value_label = NA_character_,
          reference_level = NA_character_,
          reference_label = NA_character_,
          unique_n = length(unique(med_nonmiss)),
          is_active = length(unique(med_nonmiss)) >= 2
        )
        med_idx <- med_idx + 1L
      }
    }
    mediator_specs <- mediator_specs[!vapply(mediator_specs, is.null, logical(1))]
    bmi_mediation <- list(
      enabled = length(mediator_specs) > 0,
      mediator_specs = mediator_specs,
      source_vars = unique(as.character(bmi_rows$var_name))
    )
  }
}

if (isTRUE(bmi_mediation$enabled)) {
  covariate_specs <- c(covariate_specs, bmi_mediation$mediator_specs)
}

active_baseline_covariates <- vapply(
  covariate_specs[vapply(covariate_specs, function(x) isTRUE(x$is_active) && identical(x$covariate_scope, "baseline"), logical(1))],
  function(x) x$name,
  character(1)
)
active_time_varying_covariates <- vapply(
  covariate_specs[vapply(covariate_specs, function(x) isTRUE(x$is_active) && identical(x$covariate_scope, "time_varying"), logical(1))],
  function(x) x$name,
  character(1)
)
active_mediator_covariates <- vapply(
  covariate_specs[vapply(covariate_specs, function(x) isTRUE(x$is_active) && identical(x$covariate_scope, "mediator"), logical(1))],
  function(x) x$name,
  character(1)
)

transition_specs <- vector("list", max(0, length(wave_order) - 1))

if (length(wave_order) >= 2) {
  for (i in seq_len(length(wave_order) - 1)) {
    from_wave <- wave_order[i]
    to_wave <- wave_order[i + 1]
    if (!from_wave %in% names(MPLUS_DATA) || !to_wave %in% names(MPLUS_DATA)) next

    dummy_vars <- character(0)
    dummy_states_used <- integer(0)
    for (st in seq_len(length(mplus_state_order) - 1L)) {
      dummy_name <- paste0(tolower(from_wave), "_s", st)
      MPLUS_DATA[[dummy_name]] <- ifelse(MPLUS_DATA[[from_wave]] == st, 1L, ifelse(is.na(MPLUS_DATA[[from_wave]]), NA_integer_, 0L))
      dummy_values <- MPLUS_DATA[[dummy_name]]
      dummy_values <- dummy_values[!is.na(dummy_values)]
      if (length(unique(dummy_values)) >= 2) {
        dummy_vars <- c(dummy_vars, dummy_name)
        dummy_states_used <- c(dummy_states_used, st)
      }
    }

    transition_specs[[i]] <- list(
      from_wave = from_wave,
      to_wave = to_wave,
      dummy_vars = dummy_vars,
      dummy_states = dummy_states_used,
      time_varying_covariates = active_time_varying_covariates[
        vapply(active_time_varying_covariates, function(v) {
          hit <- covariate_specs[vapply(covariate_specs, function(x) identical(x$name, v), logical(1))]
          length(hit) > 0 && identical(hit[[1]]$wave, from_wave)
        }, logical(1))
      ]
    )
  }
}
transition_specs <- transition_specs[!vapply(transition_specs, is.null, logical(1))]

PREP_SUMMARY <- list(
  n_panel_rows = nrow(ANALYSIS_PANEL_LONG),
  n_ids = length(unique(stats::na.omit(ANALYSIS_PANEL_LONG$panel_id))),
  n_transition_rows = nrow(TRANSITION_DATA),
  wave_order = wave_order,
  state_values = state_values,
  mplus_state_values = seq_along(mplus_state_order),
  reference_state = reference_state,
  reference_state_mplus = reference_state_mplus,
  state_to_mplus = state_to_mplus,
  state_from_mplus = state_from_mplus,
  state_label_map = state_label_map,
  state_vars = state_vars,
  transition_specs = transition_specs,
  baseline_covariates = baseline_covariates,
  time_varying_covariate_map = time_varying_covariate_map,
  time_varying_covariates = time_varying_covariates,
  active_baseline_covariates = active_baseline_covariates,
  active_time_varying_covariates = active_time_varying_covariates,
  active_mediator_covariates = active_mediator_covariates,
  bmi_mediation = bmi_mediation,
  covariate_specs = covariate_specs
)


save_named_rds_list(
  list(
    ANALYSIS_PANEL_LONG = ANALYSIS_PANEL_LONG,
    TRANSITION_DATA = TRANSITION_DATA,
    MPLUS_DATA = MPLUS_DATA,
    PREP_SUMMARY = PREP_SUMMARY,
    BASELINE_WIDE = if (length(c(id_var_wide, baseline_covariates)) > 0) PANEL_WIDE[, unique(c(id_var_wide, baseline_covariates)), drop = FALSE] else data.frame()
  ),
  dir_rds = DIR_RDS
)

log_info("Transition rows = ", nrow(TRANSITION_DATA))
log_info("MPLUS_DATA rows = ", nrow(MPLUS_DATA), ", cols = ", ncol(MPLUS_DATA))
log_step_end("prep", round(as.numeric(difftime(Sys.time(), T0_PREP, units = "secs")), 2), ok = TRUE)
