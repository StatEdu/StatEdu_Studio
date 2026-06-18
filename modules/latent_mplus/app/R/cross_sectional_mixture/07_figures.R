# ============================================================
# 07_figures.R
# Build publication-ready figures for cross_sectional_mixture
# ------------------------------------------------------------
# 역할
# 1) table / summary 산출물 로드
# 2) SCI 스타일 publication-ready figure 생성
# 3) png / tiff / pdf 동시 저장
# 4) figure manifest / summary 저장
# 5) figure validation / checklist 저장
#
# LPA figure architecture (aligned to LCA figure system)
# - Fig1   : model fit indices
# - Fig2   : profile size
# - Fig3   : entropy / classification quality
# - Fig4   : indicator mean heatmaps
# - Fig5   : posterior maximum probability
# - Fig6   : result figures (R3STEP / BCH / interaction)
# - Fig7   : profile line plots
# - Fig8   : radar plot
# - Fig9   : profile line plot with CI
# ============================================================

T0_FIGURES <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("FIGURES", "07_figures.R")
log_info("Reloading figure inputs ...")

# ------------------------------------------------------------
# 0-1. registry init
# ------------------------------------------------------------
FIGURE_REGISTRY <- list()

# ------------------------------------------------------------
# 0-2. basic helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

safe_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  if (is.matrix(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  out <- tryCatch(
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
  if (is.null(out)) out <- data.frame()
  out
}

safe_chr <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x <- trimws(x)
  x[!is.na(x) & nzchar(x)]
}

safe_num <- function(x) suppressWarnings(as.numeric(x))
safe_int <- function(x) suppressWarnings(as.integer(x))

safe_read_lines <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) {
    return(character(0))
  }
  tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      tryCatch(readLines(path, warn = FALSE), error = function(e2) character(0))
    }
  )
}

extract_model_structure_from_tag <- function(x) {
  x        <- as.character(x)
  out      <- rep(NA_character_, length(x))
  hit      <- grepl("_model[0-9]+_k[0-9]+_", x, ignore.case = TRUE)
  out[hit] <- sub("^.*_(model[0-9]+)_k[0-9]+_.*$", "\\1", x[hit], ignore.case = TRUE)
  tolower(out)
}

parse_lpa_indicator_profile <- function(out_file, indicators, model_tag = NA_character_) {
  txt <- safe_read_lines(out_file)
  if (length(txt) == 0 || length(indicators) == 0) return(data.frame())

  lines <- trimws(txt)
  out_list <- list()
  idx <- 1L

  in_model_results <- FALSE
  in_means_block   <- FALSE
  current_class    <- NA_integer_

  indicators_up <- toupper(as.character(indicators))

  for (ln in lines) {
    if (!nzchar(ln)) next

    if (grepl("MODEL RESULTS", toupper(ln))) {
      in_model_results <- TRUE
      in_means_block   <- FALSE
      current_class    <- NA_integer_
      next
    }

    if (!in_model_results) next

    if (grepl("LATENT CLASS\\s+[0-9]+", toupper(ln))) {
      current_class <- suppressWarnings(
        as.integer(gsub(".*LATENT CLASS\\s+([0-9]+).*", "\\1", toupper(ln)))
      )
      in_means_block <- FALSE
      next
    }

    if (grepl("^\\s*MEANS\\s*$", toupper(ln))) {
      in_means_block <- TRUE
      next
    }

    if (grepl("^(VARIANCES|INTERCEPTS|THRESHOLDS|CATEGORICAL LATENT VARIABLES|LATENT CLASS|QUALITY OF NUMERICAL RESULTS)",
              toupper(ln))) {
      in_means_block <- FALSE
    }

    if (!in_means_block || is.na(current_class)) next

    parts <- unlist(strsplit(ln, "\\s+"))
    if (length(parts) < 3) next

    var_i <- toupper(parts[1])
    if (!(var_i %in% indicators_up)) next

    est_i <- suppressWarnings(as.numeric(parts[2]))
    se_i  <- suppressWarnings(as.numeric(parts[3]))
    if (is.na(est_i)) next

    out_list[[idx]] <- data.frame(
      model_tag = as.character(model_tag),
      var_name  = tolower(var_i),
      Class     = paste0("Class ", current_class),
      Mean      = est_i,
      SE        = se_i,
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (length(out_list) == 0) return(data.frame())
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 0-3. dictionary / label helpers
# ------------------------------------------------------------
DICT <- load_step_rds(
  "DICT",
  dir_rds = DIR_RDS,
  default = list()
)

DICT_META <- if (is.list(DICT) && is.data.frame(DICT$meta)) DICT$meta else data.frame()

get_var_label <- function(v) {
  v <- as.character(v)

  if (!is.data.frame(DICT_META) || nrow(DICT_META) == 0 || !"var_name" %in% names(DICT_META)) {
    return(v)
  }

  out <- v
  for (i in seq_along(v)) {
    hit <- DICT_META[DICT_META$var_name == v[i], , drop = FALSE]
    if (nrow(hit) == 0) next

    cand <- character(0)
    if ("label_en" %in% names(hit)) cand <- c(cand, as.character(hit$label_en[1]))
    if ("label_ko" %in% names(hit)) cand <- c(cand, as.character(hit$label_ko[1]))
    if ("var_label" %in% names(hit)) cand <- c(cand, as.character(hit$var_label[1]))
    if ("var_name" %in% names(hit)) cand <- c(cand, as.character(hit$var_name[1]))

    cand <- cand[!is.na(cand) & nzchar(trimws(cand))]
    if (length(cand) > 0) out[i] <- cand[1]
  }

  out
}

get_var_display_order <- function(vars) {
  vars <- as.character(vars)

  if (!is.data.frame(DICT_META) || nrow(DICT_META) == 0 || !"var_name" %in% names(DICT_META)) {
    return(seq_along(vars))
  }

  ord_tbl <- DICT_META[, intersect(c("var_name", "display_order"), names(DICT_META)), drop = FALSE]
  if (!all(c("var_name", "display_order") %in% names(ord_tbl))) {
    return(seq_along(vars))
  }

  out <- ord_tbl$display_order[match(vars, ord_tbl$var_name)]
  out <- suppressWarnings(as.numeric(out))

  miss <- is.na(out)
  if (any(miss)) {
    out[miss] <- max(out, na.rm = TRUE) + seq_len(sum(miss))
  }
  out
}

DICT_LEVELS <- if (is.list(DICT) && is.data.frame(DICT$levels)) DICT$levels else data.frame()

normalize_code_key <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_

  out <- x

  suppressWarnings({
    numx <- as.numeric(x)
    is_num <- !is.na(numx)
    out[is_num] <- as.character(numx[is_num])
  })

  out
}

get_value_label_map_safe <- function(var_name) {
  var_name <- as.character(var_name)[1]

  if (!is.data.frame(DICT_LEVELS) || nrow(DICT_LEVELS) == 0) {
    return(data.frame(code_key = character(0), label = character(0), stringsAsFactors = FALSE))
  }
  if (!"var_name" %in% names(DICT_LEVELS)) {
    return(data.frame(code_key = character(0), label = character(0), stringsAsFactors = FALSE))
  }

  lv <- DICT_LEVELS[DICT_LEVELS$var_name == var_name, , drop = FALSE]
  if (nrow(lv) == 0) {
    return(data.frame(code_key = character(0), label = character(0), stringsAsFactors = FALSE))
  }

  code_candidates  <- intersect(c("value", "level", "code", "category"), names(lv))
  label_candidates <- intersect(c("label", "value_label", "label_en", "label_ko"), names(lv))

  if (length(code_candidates) == 0 || length(label_candidates) == 0) {
    return(data.frame(code_key = character(0), label = character(0), stringsAsFactors = FALSE))
  }

  code_col  <- code_candidates[1]
  label_col <- label_candidates[1]

  code_raw <- lv[[code_col]]
  lab_raw  <- as.character(lv[[label_col]])

  code_key <- normalize_code_key(code_raw)
  keep <- !is.na(code_key) & nzchar(code_key) & !is.na(lab_raw) & nzchar(trimws(lab_raw))

  out <- data.frame(
    code_key = code_key[keep],
    label    = trimws(lab_raw[keep]),
    stringsAsFactors = FALSE
  )

  if (nrow(out) == 0) return(out)

  out <- out[!duplicated(out$code_key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

map_value_labels_for_var <- function(values, var_name) {
  values_chr <- as.character(values)
  values_key <- normalize_code_key(values_chr)

  map_df <- get_value_label_map_safe(var_name)
  if (!is.data.frame(map_df) || nrow(map_df) == 0) {
    return(values_chr)
  }

  hit <- map_df$label[match(values_key, map_df$code_key)]

  # 1차: 그대로 매칭 실패하면 0/1 vs 1/2 이진변수 보정 시도
  miss <- is.na(hit) | !nzchar(trimws(hit))
  uniq_val <- sort(unique(values_key[!is.na(values_key)]))
  uniq_map <- sort(unique(map_df$code_key[!is.na(map_df$code_key)]))

  if (any(miss) && length(uniq_val) == 2 && length(uniq_map) == 2) {
    suppressWarnings({
      num_val <- as.numeric(uniq_val)
      num_map <- as.numeric(uniq_map)
    })

    if (all(!is.na(num_val)) && all(!is.na(num_map))) {
      # 예: values=0/1, dict=1/2 인 경우 순서기반 매핑
      ord_val <- order(num_val)
      ord_map <- order(num_map)

      remap <- stats::setNames(map_df$label[match(uniq_map[ord_map], map_df$code_key)], uniq_val[ord_val])
      hit2 <- unname(remap[values_key])

      keep2 <- miss & !is.na(hit2) & nzchar(trimws(hit2))
      hit[keep2] <- hit2[keep2]
    }
  }

  # 그래도 없으면 원값 유지
  miss <- is.na(hit) | !nzchar(trimws(hit))
  hit[miss] <- values_chr[miss]

  hit
}

map_display_codes_for_var <- function(values, var_name) {
  values_chr <- as.character(values)
  values_key <- normalize_code_key(values_chr)

  map_df <- get_value_label_map_safe(var_name)
  if (!is.data.frame(map_df) || nrow(map_df) == 0) {
    return(values_chr)
  }

  dict_codes <- unique(map_df$code_key[!is.na(map_df$code_key) & nzchar(map_df$code_key)])
  obs_codes  <- unique(values_key[!is.na(values_key) & nzchar(values_key)])

  # 그대로 맞으면 그대로
  if (all(obs_codes %in% dict_codes)) {
    return(values_key)
  }

  # 수준 수가 같고 numeric이면 순서기반 매핑
  suppressWarnings({
    obs_num  <- as.numeric(obs_codes)
    dict_num <- as.numeric(dict_codes)
  })

  if (length(obs_codes) == length(dict_codes) &&
      all(!is.na(obs_num)) &&
      all(!is.na(dict_num))) {

    obs_ord  <- obs_codes[order(obs_num)]
    dict_ord <- dict_codes[order(dict_num)]

    recode_map <- stats::setNames(dict_ord, obs_ord)
    out <- unname(recode_map[values_key])

    miss <- is.na(out) | !nzchar(trimws(out))
    out[miss] <- values_chr[miss]
    return(out)
  }

  values_chr
}

map_display_codes_for_var <- function(values, var_name) {
  values_chr <- as.character(values)
  values_key <- normalize_code_key(values_chr)

  map_df <- get_value_label_map_safe(var_name)
  if (!is.data.frame(map_df) || nrow(map_df) == 0) {
    return(values_chr)
  }

  dict_codes <- unique(map_df$code_key[!is.na(map_df$code_key) & nzchar(map_df$code_key)])
  obs_codes  <- unique(values_key[!is.na(values_key) & nzchar(values_key)])

  # 1차: observed 코드가 dict 코드와 그대로 맞으면 그대로 사용
  if (all(obs_codes %in% dict_codes)) {
    return(values_key)
  }

  # 2차: 코드값이 다르지만 수준 수가 같고 모두 numeric이면 순서기반 매핑
  suppressWarnings({
    obs_num  <- as.numeric(obs_codes)
    dict_num <- as.numeric(dict_codes)
  })

  if (length(obs_codes) == length(dict_codes) &&
      all(!is.na(obs_num)) &&
      all(!is.na(dict_num))) {

    obs_ord  <- obs_codes[order(obs_num)]
    dict_ord <- dict_codes[order(dict_num)]

    recode_map <- stats::setNames(dict_ord, obs_ord)
    out <- unname(recode_map[values_key])

    miss <- is.na(out) | !nzchar(trimws(out))
    out[miss] <- values_chr[miss]
    return(out)
  }

  # 3차: 매핑 실패 시 원값 유지
  values_chr
}


DICT_LEVELS <- if (is.list(DICT) && is.data.frame(DICT$levels)) DICT$levels else data.frame()

# ------------------------------------------------------------
# 1. validation helpers
# ------------------------------------------------------------
make_skip_validation <- function(figure_name, reason = "skipped by design") {
  data.frame(
    figure_name = figure_name,
    valid = TRUE,
    issue = reason,
    stringsAsFactors = FALSE
  )
}

validate_figure_object <- function(figure_name, plot_obj) {
  out <- data.frame(
    figure_name = figure_name,
    valid = TRUE,
    issue = "",
    stringsAsFactors = FALSE
  )

  if (is.null(plot_obj)) {
    out$valid <- FALSE
    out$issue <- "plot object is NULL"
    return(out)
  }

  if (!inherits(plot_obj, "ggplot")) {
    out$valid <- FALSE
    out$issue <- "plot object is not a ggplot"
    return(out)
  }

  out
}

validate_required_data_fig1 <- function(df) {
  out <- data.frame(
    figure_name = "Fig1_model_fit",
    valid = TRUE,
    issue = "",
    stringsAsFactors = FALSE
  )

  df <- if (is.data.frame(df)) df else data.frame()
  if (nrow(df) == 0) {
    out$valid <- FALSE
    out$issue <- "fit data is empty"
    return(out)
  }

  need <- c("k")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    out$valid <- FALSE
    out$issue <- paste0("missing columns: ", paste(miss, collapse = ", "))
    return(out)
  }

  metric_ok <- any(names(df) %in% c("bic", "aic", "sabic", "dbic", "BIC", "AIC", "SABIC", "DBIC"))
  if (!metric_ok) {
    out$valid <- FALSE
    out$issue <- "no fit metric columns found"
  }

  out
}

validate_required_data_fig2 <- function(df) {
  out <- data.frame(
    figure_name = "Fig2_class_proportion",
    valid = TRUE,
    issue = "",
    stringsAsFactors = FALSE
  )

  df <- if (is.data.frame(df)) df else data.frame()
  if (nrow(df) == 0) {
    out$valid <- FALSE
    out$issue <- "class summary data is empty"
    return(out)
  }

  has_class <- any(names(df) %in% c("class", "Class"))
  has_prop  <- any(names(df) %in% c("prop", "pct", "n"))

  if (!has_class || !has_prop) {
    out$valid <- FALSE
    out$issue <- "requires class and prop/pct/n columns"
  }

  out
}

validate_required_data_fig_profile <- function(df, figure_name = "Fig7", mixture_mode = "lpa") {
  out <- data.frame(
    figure_name = figure_name,
    valid = TRUE,
    issue = "",
    stringsAsFactors = FALSE
  )

  df <- if (is.data.frame(df)) df else data.frame()
  if (nrow(df) == 0) {
    out$valid <- FALSE
    out$issue <- "profile data is empty"
    return(out)
  }

  nm <- tolower(names(df))
  if (tolower(mixture_mode) == "lpa") {
    if (!all(c("class", "mean") %in% nm)) {
      out$valid <- FALSE
      out$issue <- "LPA figure requires class and mean columns"
    }
  } else {
    if (!("class" %in% nm) || !("mean" %in% nm)) {
      out$valid <- FALSE
      out$issue <- "LCA figure requires class and mean columns"
    }
  }

  out
}

bind_figure_validation <- function(...) {
  xs <- list(...)
  xs <- xs[vapply(xs, is.data.frame, logical(1))]
  if (length(xs) == 0) return(data.frame())
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 2. package check
# ------------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 package is required for 07_figures.R", call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("dplyr package is required for 07_figures.R", call. = FALSE)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("tidyr package is required for 07_figures.R", call. = FALSE)
}
if (!requireNamespace("ggh4x", quietly = TRUE)) {
  stop("ggh4x package is required for repeated facet axes in 07_figures.R", call. = FALSE)
}

# ------------------------------------------------------------
# 3. load core objects
# ------------------------------------------------------------
FIT_SUMMARY <- load_step_rds(
  "FIT_SUMMARY",
  dir_rds = DIR_RDS,
  default = data.frame()
)

CLASS_SUMMARY_FINAL <- load_step_rds(
  "CLASS_SUMMARY_FINAL",
  dir_rds = DIR_RDS,
  default = data.frame()
)
T3_indicator_profile <- load_step_rds(
  "T3_indicator_profile",
  dir_rds = DIR_RDS,
  default = data.frame()
)

T3_indicator_profile_z <- load_step_rds(
  "T3_indicator_profile_z",
  dir_rds = DIR_RDS,
  default = data.frame()
)

TABLE_A4_Z <- load_step_rds(
  "TABLE_A4_Z",
  dir_rds = DIR_RDS,
  default = data.frame()
)

A4_indicator_profile_z <- load_step_rds(
  "A4_indicator_profile_z",
  dir_rds = DIR_RDS,
  default = data.frame()
)

POSTERIOR_MAX_DF <- load_step_rds(
  "POSTERIOR_MAX_DF",
  dir_rds = DIR_RDS,
  default = data.frame()
)

CLASSIFIED_ANALYSIS <- load_step_rds(
  "CLASSIFIED_ANALYSIS",
  dir_rds = DIR_RDS,
  default = data.frame()
)

ANALYSIS_DATA_CLASSIFIED <- load_step_rds(
  "ANALYSIS_DATA_CLASSIFIED",
  dir_rds = DIR_RDS,
  default = data.frame()
)

CLASSIFICATION_QUALITY <- load_step_rds(
  "CLASSIFICATION_QUALITY",
  dir_rds = DIR_RDS,
  default = data.frame()
)

A5_CLASSIFICATION_SUMMARY <- load_step_rds(
  "A5",
  dir_rds = DIR_RDS,
  default = data.frame()
)

TABLE_SUMMARY <- load_step_rds(
  "TABLE_SUMMARY",
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

CFG <- load_step_rds(
  "CFG",
  dir_rds = DIR_RDS,
  default = list()
)

BCH_MOD_RESULTS_FULL <- load_step_rds(
  "BCH_MOD_RESULTS_FULL",
  dir_rds = DIR_RDS,
  default = data.frame()
)

T5B_TABLE <- load_step_rds(
  "T5b",
  dir_rds = DIR_RDS,
  default = data.frame()
)

# ------------------------------------------------------------
# 4. resolve settings
# ------------------------------------------------------------
mixture_mode <- tolower(
  SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    TABLE_SUMMARY$mixture_mode %||%
    "lpa"
)

best_k <- as.integer(
  BEST_K_SUMMARY$best_k %||%
    BEST_K_SUMMARY$BEST_K %||%
    TABLE_SUMMARY$best_k %||%
    NA_integer_
)

best_tag <- as.character(
  BEST_K_SUMMARY$best_tag %||%
    BEST_K_SUMMARY$BEST_TAG %||%
    TABLE_SUMMARY$best_tag %||%
    NA_character_
)

model_structure <- tolower(as.character(
  BEST_K_SUMMARY$best_model_structure %||%
    BEST_K_SUMMARY$BEST_MODEL_STRUCTURE %||%
    TABLE_SUMMARY$model_structure %||%
    if (is.data.frame(BEST_MODEL_ROW) && nrow(BEST_MODEL_ROW) > 0 && "model_structure" %in% names(BEST_MODEL_ROW)) BEST_MODEL_ROW$model_structure[1] else NA_character_
))

if (is.na(model_structure) || !nzchar(model_structure)) {
  model_structure <- extract_model_structure_from_tag(best_tag)
}

has_mixed_indicators_fig <- function() {
  length(SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% SETTINGS_SUMMARY$indicators_continuous %||% character(0)) > 0 &&
    length(SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% SETTINGS_SUMMARY$indicators_categorical %||% character(0)) > 0
}

latent_group_term <- function() {
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") "Class" else "Profile"
}

latent_group_term_lower <- function() tolower(latent_group_term())

latent_group_label <- function(x) {
  paste0(latent_group_term(), " ", safe_int(x))
}

normalize_latent_group_display <- function(x) {
  x <- as.character(x)
  term <- latent_group_term()
  x <- gsub("Profile\\s+([0-9]+)", paste0(term, " \\1"), x, ignore.case = TRUE)
  x <- gsub("Class\\s+([0-9]+)", paste0(term, " \\1"), x, ignore.case = TRUE)
  x <- gsub("Profile([0-9]+)", paste0(term, " \\1"), x, ignore.case = TRUE)
  x <- gsub("Class([0-9]+)", paste0(term, " \\1"), x, ignore.case = TRUE)
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
    x <- gsub("\\bProfiles\\b", "Classes", x)
    x <- gsub("\\bProfile\\b", "Class", x)
    x <- gsub("\\bprofiles\\b", "classes", x)
    x <- gsub("\\bprofile\\b", "class", x)
  } else {
    x <- gsub("\\bClasses\\b", "Profiles", x)
    x <- gsub("\\bClass\\b", "Profile", x)
    x <- gsub("\\bclasses\\b", "profiles", x)
    x <- gsub("\\bclass\\b", "profile", x)
  }
  x
}

normalize_latent_group_text <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("^(class|profile)\\s*:?\\s*([0-9]+)$", paste0(latent_group_term(), " ", "\\2"), x, ignore.case = TRUE)
  x <- gsub("^(class|profile)([0-9]+)$", paste0(latent_group_term(), " ", "\\2"), x, ignore.case = TRUE)
  x
}

get_reference_group_num <- function() {
  src <- safe_df(CLASS_SUMMARY_FINAL)
  if (nrow(src) == 0) return(NA_integer_)
  if (!"class_num" %in% names(src)) return(NA_integer_)
  prop_col <- if ("weighted_percent" %in% names(src)) {
    "weighted_percent"
  } else if ("percent" %in% names(src)) {
    "percent"
  } else {
    NA_character_
  }
  if (is.na(prop_col)) return(NA_integer_)
  prop <- safe_num(src[[prop_col]])
  cls <- safe_int(src$class_num)
  if (all(is.na(prop)) || all(is.na(cls))) return(NA_integer_)
  cls[which.max(prop)[1]]
}

order_latent_group_nums <- function(x) {
  nums <- unique(stats::na.omit(safe_int(x)))
  if (length(nums) == 0) return(nums)
  sort(nums)
}

order_latent_group_labels <- function(x) {
  x <- normalize_latent_group_text(x)
  nums <- safe_int(gsub("[^0-9]", "", x))
  ordered_nums <- order_latent_group_nums(nums)
  labs <- latent_group_label(ordered_nums)
  labs[nzchar(labs)]
}

apply_latent_group_order <- function(df, class_col = "class", class_num_col = "class_num") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)
  if (!(class_col %in% names(df))) return(df)

  df[[class_col]] <- normalize_latent_group_text(df[[class_col]])
  if (class_num_col %in% names(df)) {
    df[[class_num_col]] <- safe_int(df[[class_num_col]])
  } else {
    df[[class_num_col]] <- safe_int(gsub("[^0-9]", "", as.character(df[[class_col]])))
  }

  levs <- order_latent_group_labels(df[[class_col]])
  if (length(levs) == 0) levs <- unique(as.character(df[[class_col]]))
  df[[class_col]] <- factor(as.character(df[[class_col]]), levels = levs)
  df
}

latent_group_style_scales <- function(levels_in = NULL) {
  levs <- as.character(levels_in %||% character(0))
  levs <- levs[nzchar(levs)]
  if (length(levs) == 0) return(list())
  ref_lab <- latent_group_label(get_reference_group_num())
  line_vals <- rep(c("dashed", "dotdash", "longdash", "twodash"), length.out = length(levs))
  shape_vals <- rep(c(17, 15, 18, 8, 3, 7), length.out = length(levs))
  if (ref_lab %in% levs) {
    ref_idx <- match(ref_lab, levs)
    line_vals[ref_idx] <- "solid"
    shape_vals[ref_idx] <- 16
  }
  list(
    ggplot2::scale_linetype_manual(values = stats::setNames(line_vals, levs)),
    ggplot2::scale_shape_manual(values = stats::setNames(shape_vals, levs))
  )
}

latent_group_color_scale <- function(levels_in = NULL) {
  levs <- as.character(levels_in %||% character(0))
  levs <- levs[nzchar(levs)]
  if (length(levs) == 0) return(list())
  palette <- c(
    "#0072B2", "#D55E00", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442", "#000000"
  )
  ggplot2::scale_colour_manual(values = stats::setNames(rep(palette, length.out = length(levs)), levs))
}

# ------------------------------------------------------------
# 4-1. enforce retained solution only for profile-level objects
# ------------------------------------------------------------
filter_retained_profile <- function(df, best_k, best_tag = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if ("model_tag" %in% names(df) && !is.null(best_tag) && nzchar(best_tag)) {
    df <- df[as.character(df$model_tag) == as.character(best_tag), , drop = FALSE]
  }

  if ("class_num" %in% names(df)) {
    df <- df[!is.na(df$class_num) & df$class_num <= best_k, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}

extract_class_ids_from_text <- function(x) {
  x <- as.character(x %||% "")
  lapply(x, function(one) {
    hit <- gregexpr("Class\\s*([0-9]+)", one, perl = TRUE, ignore.case = TRUE)
    vals <- regmatches(one, hit)[[1]]
    if (length(vals) == 0) return(integer(0))
    suppressWarnings(as.integer(gsub("[^0-9]", "", vals)))
  })
}

filter_retained_solution_df <- function(df, best_k, best_tag = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if ("best_tag" %in% names(df) && !is.null(best_tag) && nzchar(best_tag)) {
    tag_chr <- as.character(df$best_tag)
    keep_tag <- is.na(tag_chr) | !nzchar(tag_chr) | tag_chr == as.character(best_tag)
    df <- df[keep_tag, , drop = FALSE]
  } else if ("model_tag" %in% names(df) && !is.null(best_tag) && nzchar(best_tag)) {
    tag_chr <- as.character(df$model_tag)
    keep_tag <- is.na(tag_chr) | !nzchar(tag_chr) | tag_chr == as.character(best_tag)
    df <- df[keep_tag, , drop = FALSE]
  }

  if ("best_k" %in% names(df)) {
    bk <- suppressWarnings(as.integer(df$best_k))
    keep_bk <- is.na(bk) | bk == as.integer(best_k)
    df <- df[keep_bk, , drop = FALSE]
  }

  if ("class_num" %in% names(df)) {
    cls <- suppressWarnings(as.integer(df$class_num))
    keep_cls <- is.na(cls) | cls <= as.integer(best_k)
    df <- df[keep_cls, , drop = FALSE]
  }

  if ("comparison" %in% names(df)) {
    cmp_ids <- extract_class_ids_from_text(df$comparison)
    keep_cmp <- vapply(cmp_ids, function(ids) length(ids) == 0 || all(is.na(ids) | ids <= as.integer(best_k)), logical(1))
    df <- df[keep_cmp, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}

# 적용
T3_indicator_profile   <- filter_retained_profile(T3_indicator_profile, best_k, best_tag)
T3_indicator_profile_z <- filter_retained_profile(T3_indicator_profile_z, best_k, best_tag)
TABLE_A4_Z             <- filter_retained_profile(TABLE_A4_Z, best_k, best_tag)
A4_indicator_profile_z <- filter_retained_profile(A4_indicator_profile_z, best_k, best_tag)
CLASS_SUMMARY_FINAL    <- filter_retained_solution_df(CLASS_SUMMARY_FINAL, best_k, best_tag)
BCH_MOD_RESULTS_FULL   <- filter_retained_solution_df(BCH_MOD_RESULTS_FULL, best_k, best_tag)

# ------------------------------------------------------------
# 5. directories / paths
# ------------------------------------------------------------
DIR_FIGURES <- get0(
  "DIR_FIGURES",
  ifnotfound = file.path(DIR_OUTPUT, "figures"),
  inherits = TRUE
)

DIR_FIGURES_PNG <- get0(
  "DIR_FIGURES_PNG",
  ifnotfound = file.path(DIR_FIGURES, "png"),
  inherits = TRUE
)

DIR_FIGURES_TIFF <- get0(
  "DIR_FIGURES_TIFF",
  ifnotfound = file.path(DIR_FIGURES, "tiff"),
  inherits = TRUE
)

DIR_FIGURES_PDF <- get0(
  "DIR_FIGURES_PDF",
  ifnotfound = file.path(DIR_FIGURES, "pdf"),
  inherits = TRUE
)

ensure_dir2(DIR_FIGURES)
ensure_dir2(DIR_FIGURES_PNG)
if (!isTRUE(getOption("easyflow.single_output", TRUE))) {
  ensure_dir2(DIR_FIGURES_TIFF)
  ensure_dir2(DIR_FIGURES_PDF)
}

PATH_FIGURE_SUMMARY_CSV <- get0(
  "PATH_FIGURE_SUMMARY_CSV",
  ifnotfound = file.path(DIR_FIGURES, "FIGURE_SUMMARY.csv"),
  inherits = TRUE
)

PATH_FIGURE_MANIFEST_CSV <- get0(
  "PATH_FIGURE_MANIFEST_CSV",
  ifnotfound = file.path(DIR_FIGURES, "FIGURE_MANIFEST.csv"),
  inherits = TRUE
)

PATH_FIGURE_MANIFEST_RDS <- get0(
  "PATH_FIGURE_MANIFEST_RDS",
  ifnotfound = file.path(DIR_RDS, "FIGURE_MANIFEST.rds"),
  inherits = TRUE
)

PATH_FIGURE_CHECKLIST_CSV <- file.path(DIR_FIGURES, "FIGURE_CHECKLIST.csv")
PATH_FIGURE_VALIDATION_CSV <- file.path(DIR_FIGURES, "FIGURE_VALIDATION.csv")

# ------------------------------------------------------------
# 6. titles
# ------------------------------------------------------------
fit_title <- "Model fit across candidate solutions"

class_title <- paste0(latent_group_term(), " proportions")

profile_title_raw <- if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
  paste0("Indicator response by ", latent_group_term_lower())
} else {
  paste0("Indicator profile by ", latent_group_term_lower())
}

profile_title_z <- if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
  paste0("Indicator response by ", latent_group_term_lower(), " standardized")
} else {
  paste0("Indicator profile by ", latent_group_term_lower(), " standardized")
}

# ------------------------------------------------------------
# 7. style helper
# ------------------------------------------------------------
# ------------------------------------------------------------
# journal style profile
# ------------------------------------------------------------
theme_sci_ref <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = "black"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}


get_journal_style <- function() {
  # 1순위: CFG
  j <- NULL
  if (exists("CFG") && is.list(CFG)) {
    j <- CFG$paper$journal_style %||% CFG$journal_style %||% NULL
  }

  # 2순위: SETTINGS_SUMMARY
  if (is.null(j) && exists("SETTINGS_SUMMARY") && is.list(SETTINGS_SUMMARY)) {
    j <- SETTINGS_SUMMARY$journal_style %||% SETTINGS_SUMMARY$JOURNAL_STYLE %||% NULL
  }

  # 기본값
  j <- tolower(as.character(j %||% "generic_sci"))
  if (!nzchar(j)) j <- "generic_sci"
  j
}

get_journal_base_spec <- function(journal_style = get_journal_style()) {
  journal_style <- tolower(as.character(journal_style))

  switch(
    journal_style,

    # Elsevier 계열: 비교적 넓은 본문/2단 컬럼 대응
    elsevier = list(
      base_width   = 7.2,
      base_height  = 5.0,
      wide_width   = 8.6,
      wide_height  = 5.8,
      facet_width  = 9.2,
      facet_height = 6.2,
      square       = 7.0,
      dpi          = 600
    ),

    # Springer 계열: 약간 컴팩트
    springer       = list(
      base_width   = 6.8,
      base_height  = 4.8,
      wide_width   = 8.2,
      wide_height  = 5.5,
      facet_width  = 8.8,
      facet_height = 6.0,
      square       = 6.8,
      dpi          = 600
    ),

    # APA 계열: 비교적 절제된 폭
    apa            = list(
      base_width   = 6.5,
      base_height  = 4.8,
      wide_width   = 7.8,
      wide_height  = 5.3,
      facet_width  = 8.4,
      facet_height = 5.8,
      square       = 6.8,
      dpi          = 600
    ),

    # 기본 SCI
    generic_sci    = list(
      base_width   = 7.0,
      base_height  = 5.0,
      wide_width   = 8.5,
      wide_height  = 5.5,
      facet_width  = 9.0,
      facet_height = 6.0,
      square       = 7.0,
      dpi          = 600
    )
  )
}

get_fig_spec <- function(stub, journal_style = get_journal_style()) {
  js <- get_journal_base_spec(journal_style)

  specs <- list(
    Fig1_model_fit                = list(width = js$base_width, height = js$base_height, dpi = js$dpi),
    Fig2_class_proportion         = list(width = 6.5, height = 4.8, dpi = js$dpi),
    Fig3_entropy                  = list(width = 6.5, height = 4.8, dpi = js$dpi),
    Fig3_classification_quality   = list(width = js$base_width, height = js$base_height, dpi = js$dpi),

    Fig4_1_indicator_heatmap_raw  = list(width = js$wide_width, height = js$wide_height, dpi = js$dpi),
    Fig4_2_indicator_heatmap_z    = list(width = js$wide_width, height = js$wide_height, dpi = js$dpi),

    Fig5_posterior_max            = list(width = js$base_width, height = js$base_height, dpi = js$dpi),
    Fig5_2_app_occ_by_profile     = list(width = js$wide_width, height = 6.2, dpi = js$dpi),
    Fig6_2_bch_interaction        = list(width = js$wide_width, height = 6.0, dpi = js$dpi),
    Fig6_3_moderator_profile_means = list(width = js$wide_width, height = 5.6, dpi = js$dpi),

    Fig7_1_profile_line_raw       = list(width = js$wide_width, height = js$wide_height, dpi = js$dpi),
    Fig7_2_profile_line_z         = list(width = js$wide_width, height = js$wide_height, dpi = js$dpi),
    Fig7_1_profile_line_raw_color = list(width = js$wide_width, height = js$wide_height, dpi = 600),
    Fig7_2_profile_line_z_color   = list(width = js$wide_width, height = js$wide_height, dpi = 600),
    Fig7_3_profile_line_raw_facet = list(width = js$facet_width, height = js$facet_height, dpi = js$dpi),
    Fig7_4_profile_line_z_facet   = list(width = js$facet_width, height = js$facet_height, dpi = js$dpi),
    Fig7_5_categorical_profile_distribution = list(width = js$wide_width, height = js$facet_height, dpi = js$dpi),
    Fig7_6_categorical_profile_distribution_bw = list(width = js$wide_width, height = js$facet_height, dpi = js$dpi),

    Fig8_radar_raw                = list(width = js$square, height = js$square, dpi = js$dpi),
    Fig9_profile_line_ci          = list(width = js$wide_width, height = js$wide_height, dpi = js$dpi)
  )

  specs[[stub]] %||% list(width = js$base_width, height = js$base_height, dpi = js$dpi)
}

scale_y_percent_sci_local <- function(...) {
  if (exists("scale_y_percent_sci", inherits = TRUE)) {
    return(scale_y_percent_sci(...))
  }
  ggplot2::scale_y_continuous(
    labels = function(x) paste0(sprintf("%.0f", x * 100), "%"),
    ...
  )
}

wrap_label_text <- function(x, width = 18) {
  x <- as.character(x)
  vapply(
    x,
    function(s) {
      s <- trimws(s)
      if (is.na(s) || !nzchar(s)) return("")
      paste(strwrap(s, width = width), collapse = "\n")
    },
    character(1)
  )
}

make_wrapped_factor <- function(x, width = 18) {
  x_chr <- as.character(x)
  lev0  <- unique(x_chr)
  lab0  <- wrap_label_text(lev0, width = width)
  factor(x_chr, levels = lev0, labels = lab0)
}


# ------------------------------------------------------------
# 8. save helpers
# ------------------------------------------------------------
save_fig <- function(plot_obj, file_stub, figure_title, width, height, dpi = 600) {
  save_plot_all_formats(
    plot_obj     = plot_obj,
    file_stub    = file_stub,
    dir_png      = DIR_FIGURES_PNG,
    dir_tiff     = DIR_FIGURES_TIFF,
    dir_pdf      = DIR_FIGURES_PDF,
    width        = width,
    height       = height,
    dpi          = dpi,
    figure_title = figure_title
  )
}

FIGURE_MANIFEST <- empty_figure_manifest()

register_figure_paths <- function(registry, stub) {
  registry[[paste0(stub, "_png")]]  <- file.path(DIR_FIGURES_PNG,  paste0(stub, ".png"))
  if (!isTRUE(getOption("easyflow.single_output", TRUE))) {
    registry[[paste0(stub, "_tiff")]] <- file.path(DIR_FIGURES_TIFF, paste0(stub, ".tiff"))
    registry[[paste0(stub, "_pdf")]]  <- file.path(DIR_FIGURES_PDF,  paste0(stub, ".pdf"))
  }
  registry
}

# ------------------------------------------------------------
# 9. profile data prep
# ------------------------------------------------------------
resolve_profile_df <- function(df, mixture_mode = "lpa") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  nm <- names(df)
  low <- tolower(nm)

  get_col_name <- function(targets) {
    hit <- nm[match(targets, low, nomatch = 0)]
    hit <- hit[nzchar(hit)]
    if (length(hit) == 0) return(NA_character_)
    hit[1]
  }

  class_col <- get_col_name(c("class", "profile"))
  class_num_col <- get_col_name(c("class_num", "profile_num"))
  mean_col <- get_col_name(c("mean", "estimate", "prop", "pct"))
  se_col <- get_col_name(c("se", "std_error"))
  label_col <- get_col_name(c("label", "var_label", "variable", "var_name"))
  var_col <- get_col_name(c("var_name", "variable_raw", "variable"))

  if (!is.na(class_col) && !"class" %in% names(df)) df$class <- as.character(df[[class_col]])
  if (!is.na(class_num_col) && !"class_num" %in% names(df)) df$class_num <- safe_int(df[[class_num_col]])
  if (!is.na(mean_col) && !"mean" %in% names(df)) df$mean <- safe_num(df[[mean_col]])
  if (!is.na(se_col) && !"se" %in% names(df)) df$se <- safe_num(df[[se_col]])
  if (!is.na(label_col) && !"label" %in% names(df)) df$label <- as.character(df[[label_col]])
  if (!is.na(var_col) && !"var_name" %in% names(df)) df$var_name <- as.character(df[[var_col]])

  if (!"class" %in% names(df) && "class_num" %in% names(df)) df$class <- latent_group_label(df$class_num)
  if (!"class_num" %in% names(df) && "class" %in% names(df)) df$class_num <- safe_int(gsub("[^0-9]", "", as.character(df$class)))

  if (!"var_name" %in% names(df) && "label" %in% names(df)) df$var_name <- as.character(df$label)
  if (!"label" %in% names(df) && "var_name" %in% names(df)) df$label <- get_var_label(df$var_name)

  if ("pct" %in% low && !("prop" %in% low) && "mean" %in% names(df) && mixture_mode != "lpa") {
    df$mean <- df$mean / 100
  }

  df$class_num                   <- safe_int(df$class_num)
  df$mean                        <- safe_num(df$mean)
  if ("se" %in% names(df)) df$se <- safe_num(df$se)

  df                             <- df[!is.na(df$class) & !is.na(df$mean), , drop = FALSE]
  if (nrow(df) == 0) return(df)

  if (all(is.na(df$class_num))) {
    df$class_num                 <- seq_len(length(unique(df$class)))[match(df$class, unique(df$class))]
  }

  class_levels <- order_latent_group_labels(df$class)
  if (length(class_levels) == 0) class_levels <- unique(as.character(df$class))
  df$class               <- factor(normalize_latent_group_text(df$class), levels = class_levels)

  if ("var_name" %in% names(df)) {
    ord_var              <- unique(as.character(df$var_name))
    lab_map              <- stats::setNames(get_var_label(ord_var), ord_var)

    if ("label" %in% names(df)) {
      miss_lab           <- !nzchar(trimws(as.character(df$label))) | is.na(df$label)
      df$label[miss_lab] <- lab_map[as.character(df$var_name[miss_lab])]
    } else {
      df$label           <- unname(lab_map[as.character(df$var_name)])
    }

    ord_label            <- unique(as.character(df$label[match(ord_var, df$var_name)]))
    df$label             <- factor(as.character(df$label), levels = ord_label)
  } else if ("label" %in% names(df)) {
    df$label             <- factor(as.character(df$label), levels = unique(as.character(df$label)))
  }

  df
}

derive_profile_z_df <- function(df_raw) {
  df_raw <- resolve_profile_df(df_raw, mixture_mode = "lpa")
  if (nrow(df_raw) == 0) return(data.frame())
  if (!all(c("class", "label", "mean") %in% names(df_raw))) return(data.frame())

  out <- df_raw
  out$mean <- safe_num(out$mean)
  out <- out[!is.na(out$mean), , drop = FALSE]
  if (nrow(out) == 0) return(data.frame())

  out <- dplyr::group_by(out, .data$label)
  out <- dplyr::mutate(
    out,
    mean = {
      x <- safe_num(.data$mean)
      s <- stats::sd(x, na.rm = TRUE)
      if (is.finite(s) && s > 0) as.numeric(scale(x)) else rep(0, length(x))
    }
  )
  out <- dplyr::ungroup(out)
  out
}

build_profile_line_plot <- function(df, title, ylab = "Mean", use_errorbar = TRUE, color = FALSE) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  df <- apply_latent_group_order(df, class_col = "class", class_num_col = "class_num")
  if (!"label" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$label <- as.character(df$var_label)
    } else if ("var_name" %in% names(df)) {
      df$label <- get_var_label(df$var_name)
    }
  }
  if (!"label" %in% names(df)) return(NULL)

  df$mean <- safe_num(df$mean)
  if ("se" %in% names(df)) df$se <- safe_num(df$se)

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- df$mean - df$se
    df$ymax <- df$mean + df$se
  }

  df$label_wrapped <- make_wrapped_factor(df$label, width = 18)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$label_wrapped,
      y = .data$mean,
      group = .data$class,
      linetype = .data$class,
      shape = .data$class
    )
  )
  if (isTRUE(color)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(colour = .data$class), linewidth = 0.7) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$class), size = 2.3)
  } else {
    p <- p +
      ggplot2::geom_line(linewidth = 0.7, colour = "black") +
      ggplot2::geom_point(size = 2.3, colour = "black")
  }

  if (use_errorbar && has_se) {
    errorbar_mapping <- if (isTRUE(color)) {
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax, colour = .data$class)
    } else {
      ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax)
    }
    p <- p +
      ggplot2::geom_errorbar(
        errorbar_mapping,
        width = 0.08,
        linewidth = 0.35
      )
  }

  p <- p +
    latent_group_style_scales(levels(df$class)) +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 1,
        face = "plain",
        family = "sans",
        lineheight = 0.95
      )
    )
  if (isTRUE(color)) {
    p <- p + latent_group_color_scale(levels(df$class))
  }
  p
}

build_profile_line_plot_varname <- function(df,
                                            title = NULL,
                                            ylab = "Mean",
                                            use_errorbar = TRUE) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)

  if (!"mean" %in% names(df) && "Mean" %in% names(df)) df$mean <- safe_num(df$Mean)
  if (!"se"   %in% names(df) && "SE"   %in% names(df)) df$se   <- safe_num(df$SE)
  if (!"class" %in% names(df) && "Class" %in% names(df)) df$class <- as.character(df$Class)

  if (!"var_name" %in% names(df)) {
    if ("label" %in% names(df)) df$var_name <- as.character(df$label)
  }

  need <- c("class", "var_name", "mean")
  if (!all(need %in% names(df))) return(NULL)

  df$class    <- as.character(df$class)
  df$var_name <- as.character(df$var_name)
  df$mean     <- safe_num(df$mean)
  if ("se" %in% names(df)) df$se <- safe_num(df$se) else df$se <- NA_real_
  df <- apply_latent_group_order(df, class_col = "class", class_num_col = "class_num")

  df <- df[!is.na(df$class) & !is.na(df$var_name) & !is.na(df$mean), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  wrap_axis_labels <- function(x, width = 10) {
    vapply(as.character(x), function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1))
  }

  df$var_name_wrap <- factor(
    wrap_axis_labels(df$var_name, width = 10),
    levels = unique(wrap_axis_labels(df$var_name, width = 10))
  )

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- df$mean - df$se
    df$ymax <- df$mean + df$se
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = var_name_wrap,
      y = mean,
      group = class,
      linetype = class,
      shape = class
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.2)

  if (isTRUE(use_errorbar) && has_se) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0.08,
      linewidth = 0.35
    )
  }

  p +
    latent_group_style_scales(levels(df$class)) +
    ggplot2::labs(
      title = title %||% "",
      x = NULL,
      y = ylab,
      linetype = NULL,
      shape = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8, lineheight = 0.9)
    )
}


build_profile_facet_plot <- function(df, title, ylab = "Mean") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (!"label" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$label <- as.character(df$var_label)
    } else if ("var_name" %in% names(df)) {
      df$label <- get_var_label(df$var_name)
    }
  }
  if (!"label" %in% names(df)) return(NULL)

  df$mean <- safe_num(df$mean)
  if ("se" %in% names(df)) df$se <- safe_num(df$se)

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- df$mean - df$se
    df$ymax <- df$mean + df$se
  }

  df$label_wrapped <- make_wrapped_factor(df$label, width = 18)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$class, y = .data$mean)
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 0.5)

  if (has_se) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
        width     = 0.08,
        linewidth = 0.35
      )
  }

  p +
    ggplot2::facet_wrap(~ label_wrapped, scales = "free_y") +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(
        angle      = 0,
        hjust      = 0.5,
        face       = "plain",
        family     = "sans"
      ),
      strip.text   = ggplot2::element_text(
        face       = "plain",
        family     = "sans",
        lineheight = 0.95
      )
    )
}

build_lca_item_response_plot <- function(df, title = NULL, ylab = "Item-response probability") {
  df <- prepare_lca_irp_df(df)
  if (nrow(df) == 0) return(NULL)

  n_var <- length(levels(df$var_label))
  wrap_width <- if (n_var >= 16) 10 else if (n_var >= 12) 12 else 18
  strip_size <- if (n_var >= 16) 9 else if (n_var >= 12) 10 else 11

  df$item_wrapped <- make_wrapped_factor(df$var_label, width = wrap_width)

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- pmax(0, df$mean - df$se)
    df$ymax <- pmin(1, df$mean + df$se)
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x        = resp_label,
      y        = mean,
      group    = class,
      linetype = class,
      shape    = class
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 2.0)

  if (has_se) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width             = 0.08,
      linewidth         = 0.30
    )
  }

  p +
    latent_group_style_scales(levels(df2$class)) +
    ggh4x::facet_wrap2(
      ~ item_wrapped,
      scales        = "free_y",
      axes          = "all",
      remove_labels = "none"
    ) +
    ggplot2::scale_y_continuous(
      limits        = c(0, 1),
      breaks        = seq(0, 1, by = 0.25)
    ) +
    ggplot2::labs(
      title         = title %||% "",
      x             = "Response category",
      y             = ylab,
      linetype      = NULL,
      shape         = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 7),
      strip.text  = ggplot2::element_text(size = 5.8, lineheight = 0.88)
    )
}


build_profile_heatmap <- function(df, title, fill_lab = "Mean") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)

  if (!"class" %in% names(df) && "class_num" %in% names(df)) {
    df$class <- latent_group_label(df$class_num)
  }
  if (!"class_num" %in% names(df) && "class" %in% names(df)) {
    df$class_num <- safe_int(gsub("[^0-9]", "", as.character(df$class)))
  }
  if (!"var_label" %in% names(df) && "var_name" %in% names(df)) {
    df$var_label <- get_var_label(df$var_name)
  }
  if (!"label" %in% names(df)) {
    if (all(c("var_label", "category") %in% names(df))) {
      df$label <- paste0(as.character(df$var_label), " = ", as.character(df$category))
    } else if ("var_label" %in% names(df)) {
      df$label <- as.character(df$var_label)
    } else if ("var_name" %in% names(df)) {
      df$label <- get_var_label(df$var_name)
    }
  }

  df$mean <- safe_num(df$mean)
  df$class <- as.character(df$class)
  df$label <- as.character(df$label)
  df <- df[!is.na(df$mean) & nzchar(df$class) & nzchar(df$label), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  if ("class_num" %in% names(df) && any(!is.na(df$class_num))) {
    class_levels <- latent_group_label(order_latent_group_nums(df$class_num))
    df$class <- factor(df$class, levels = class_levels)
  } else {
    df$class <- factor(normalize_latent_group_text(df$class), levels = order_latent_group_labels(df$class))
  }
  df$label <- factor(df$label, levels = rev(unique(df$label)))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$class, y = .data$label, fill = .data$mean)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$mean)), size = 3) +
    ggplot2::labs(title = title, x = latent_group_term(), y = NULL, fill = fill_lab) +
    theme_sci_ref()
}

build_posterior_max_plot <- function(df, title = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (!all(c("Class_num", "PosteriorMax") %in% names(df))) return(NULL)
  if (is.null(title) || !nzchar(title)) title <- paste0("Posterior max probability by ", latent_group_term_lower())

  df$Class_num <- safe_int(df$Class_num)
  df$PosteriorMax <- safe_num(df$PosteriorMax)
  df <- df[!is.na(df$Class_num) & !is.na(df$PosteriorMax), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  df$class <- factor(
    latent_group_label(df$Class_num),
    levels = latent_group_label(order_latent_group_nums(df$Class_num))
  )

  n_by_class <- table(df$class)
  use_violin <- all(n_by_class >= 2)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = class, y = PosteriorMax))

  if (use_violin) {
    p <- p + ggplot2::geom_violin(trim = FALSE, linewidth = 0.3)
  }

  p +
    ggplot2::geom_boxplot(
      width = 0.16,
      outlier.shape = NA,
      linewidth = 0.35,
      fill = "white"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1)
    ) +
    ggplot2::labs(
      title = title,
      x = latent_group_term(),
      y = "Maximum posterior probability"
    ) +
    theme_sci_ref()
}

build_app_occ_plot <- function(df, title = NULL, monochrome = FALSE) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (!all(c("Profile", "APP", "OCC") %in% names(df))) return(NULL)
  if (is.null(title) || !nzchar(title)) title <- paste0("Classification quality by ", latent_group_term_lower())

  df$Profile <- normalize_latent_group_text(df$Profile)
  df$APP <- safe_num(df$APP)
  df$OCC <- safe_num(df$OCC)
  df <- df[!is.na(df$APP) & nzchar(df$Profile), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  prof_num <- suppressWarnings(as.integer(gsub("^.*?([0-9]+)$", "\\1", df$Profile)))
  ord <- order(match(df$Profile, order_latent_group_labels(df$Profile)), prof_num, df$Profile, na.last = TRUE)
  df <- df[ord, , drop = FALSE]
  df$Profile <- factor(df$Profile, levels = order_latent_group_labels(df$Profile))

  long_df <- tidyr::pivot_longer(
    df[, c("Profile", "APP", "OCC"), drop = FALSE],
    cols = c("APP", "OCC"),
    names_to = "metric",
    values_to = "value"
  )
  long_df <- long_df[!is.na(long_df$value), , drop = FALSE]
  if (nrow(long_df) == 0) return(NULL)
  long_df$metric <- factor(long_df$metric, levels = c("APP", "OCC"))

  if (isTRUE(monochrome)) {
    p <- ggplot2::ggplot(
      long_df,
      ggplot2::aes(x = .data$Profile, y = .data$value)
    ) +
      ggplot2::geom_col(
        width = 0.65,
        fill = "grey75",
        colour = "black",
        linewidth = 0.25,
        show.legend = FALSE
      )
  } else {
    p <- ggplot2::ggplot(
      long_df,
      ggplot2::aes(x = .data$Profile, y = .data$value, fill = .data$Profile)
    ) +
      ggplot2::geom_col(
        width = 0.65,
        alpha = 0.9,
        colour = "black",
        linewidth = 0.25,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2")
  }

  p <- p +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(.data$metric == "APP", sprintf("%.2f", .data$value), sprintf("%.3f", .data$value))),
      vjust = -0.35,
      size = 3.1,
      color = "black"
    ) +
    ggplot2::facet_wrap(~metric, ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      title = title,
      x = latent_group_term(),
      y = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )

  p
}

build_fig3_entropy <- function(df, title = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (is.null(title) || !nzchar(title)) title <- paste0("Entropy across candidate ", latent_group_term_lower(), " solutions")

  if ("Entropy" %in% names(df) && !"entropy" %in% names(df)) df$entropy <- safe_num(df$Entropy)
  if (!"entropy" %in% names(df)) return(NULL)

  if ("k" %in% names(df)) {
    df$k <- safe_num(df$k)
  } else if ("K" %in% names(df)) {
    df$k <- safe_num(df$K)
  } else {
    return(NULL)
  }

  df <- df[!is.na(df$k) & !is.na(df$entropy), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  ggplot2::ggplot(df, ggplot2::aes(x = k, y = entropy)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$k))) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::labs(title = title, x = paste0("Number of ", latent_group_term_lower(), "s"), y = "Entropy") +
    theme_sci_ref()
}

build_categorical_profile_df <- function(df, indicators, weight_var = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0 || !"class_num" %in% names(df)) return(data.frame())

  indicators <- intersect(as.character(indicators), names(df))
  if (length(indicators) == 0) return(data.frame())

  df$class_num <- safe_int(df$class_num)
  df <- df[!is.na(df$class_num), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  weight_ok <- !is.null(weight_var) && nzchar(as.character(weight_var)) && weight_var %in% names(df)
  out <- list()
  idx <- 1L

  for (v in indicators) {
    x_all <- df[[v]]
    cats <- sort(unique(x_all[!is.na(x_all)]))
    if (length(cats) == 0) next

    for (cl in sort(unique(df$class_num))) {
      sub <- df[df$class_num == cl & !is.na(df[[v]]), , drop = FALSE]
      if (nrow(sub) == 0) next

      if (isTRUE(weight_ok)) {
        w_sub <- safe_num(sub[[weight_var]])
        valid_w <- is.finite(w_sub) & w_sub > 0
        use_weight <- any(valid_w)
      } else {
        w_sub <- NULL
        valid_w <- rep(FALSE, nrow(sub))
        use_weight <- FALSE
      }

      for (catv in cats) {
        if (isTRUE(use_weight)) {
          denom <- sum(w_sub[valid_w], na.rm = TRUE)
          num <- sum(w_sub[valid_w & as.character(sub[[v]]) == as.character(catv)], na.rm = TRUE)
          prop_i <- if (is.finite(denom) && denom > 0) num / denom else NA_real_
        } else {
          prop_i <- mean(as.character(sub[[v]]) == as.character(catv), na.rm = TRUE)
        }
        if (!is.finite(prop_i) || is.na(prop_i)) next

        out[[idx]] <- data.frame(
          class_num = as.integer(cl),
          Profile = latent_group_label(cl),
          var_name = as.character(v),
          var_label = get_var_label(v),
          category = as.character(catv),
          category_label = map_value_labels_for_var(catv, v),
          prop = as.numeric(prop_i),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  if (length(out) == 0) return(data.frame())
  out <- do.call(rbind, out)
  out$var_order <- get_var_display_order(out$var_name)
  out <- out[order(out$var_order, out$var_name, out$class_num, out$category_label), , drop = FALSE]
  rownames(out) <- NULL
  out
}

build_categorical_profile_plot <- function(df,
                                           title = NULL,
                                           monochrome = FALSE,
                                           ylab = "Weighted %") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (!all(c("Profile", "var_label", "category_label", "prop") %in% names(df))) return(NULL)
  if (is.null(title) || !nzchar(title)) title <- paste0("Categorical indicator distributions by ", latent_group_term_lower())

  df$Profile <- factor(normalize_latent_group_text(df$Profile), levels = latent_group_label(order_latent_group_nums(df$class_num)))
  df$prop <- safe_num(df$prop)
  df <- df[!is.na(df$prop) & !is.na(df$Profile), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  vars <- unique(as.character(df$var_name))
  out_list <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    vv <- vars[i]
    sub <- df[as.character(df$var_name) == vv, , drop = FALSE]
    sub$x_cat <- paste0(vv, "::", as.character(sub$category_label))
    out_list[[i]] <- sub
  }
  df2 <- do.call(rbind, out_list)
  rownames(df2) <- NULL

  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(x = .data$x_cat, y = .data$prop, group = .data$Profile)
  )

  if (isTRUE(monochrome)) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(shape = .data$Profile),
        size = 2.4,
        colour = "black"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(linetype = .data$Profile),
        linewidth = 0.45,
        colour = "black"
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$Profile, shape = .data$Profile),
        size = 2.4
      ) +
      ggplot2::geom_line(
        ggplot2::aes(colour = .data$Profile, linetype = .data$Profile),
        linewidth = 0.45
      )
  }

  p +
    latent_group_style_scales(levels(df2$Profile)) +
    ggh4x::facet_wrap2(~var_label, scales = "free_x", ncol = 2) +
    ggplot2::scale_x_discrete(labels = function(brks) sub("^.*::", "", brks), drop = TRUE) +
    scale_y_percent_sci_local(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = ylab,
      colour = NULL,
      shape = NULL,
      linetype = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold")
    )
}

build_fig3_classification_quality <- function(df, title = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (is.null(title) || !nzchar(title)) title <- paste0("Classification quality bands by ", latent_group_term_lower())

  need <- c("Class_num", "n_lt_70", "n_70_79", "n_80_89", "n_90_94", "n_ge_95")
  if (!all(need %in% names(df))) return(NULL)

  plot_df <- df |>
    dplyr::mutate(Class_num = safe_int(Class_num)) |>
    tidyr::pivot_longer(
      cols = c("n_lt_70", "n_70_79", "n_80_89", "n_90_94", "n_ge_95"),
      names_to = "Band",
      values_to = "n"
    ) |>
    dplyr::group_by(Class_num) |>
    dplyr::mutate(pct = 100 * n / sum(n, na.rm = TRUE)) |>
    dplyr::ungroup()

  if (nrow(plot_df) == 0) return(NULL)

  plot_df$Band <- factor(
    plot_df$Band,
    levels = c("n_lt_70", "n_70_79", "n_80_89", "n_90_94", "n_ge_95"),
    labels = c("<.70", ".70-.79", ".80-.89", ".90-.94", ">=.95")
  )

  plot_df$class <- factor(
    latent_group_label(plot_df$Class_num),
    levels = latent_group_label(order_latent_group_nums(plot_df$Class_num))
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = class, y = pct, fill = Band)) +
    ggplot2::geom_col(width = 0.7, colour = "black", linewidth = 0.2) +
    ggplot2::labs(title = title, x = latent_group_term(), y = "Percent") +
    theme_sci_ref()
}

build_profile_line_ci_plot <- function(df, title) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)
  if (!"label" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$label <- as.character(df$var_label)
    } else if ("var_name" %in% names(df)) {
      df$label <- get_var_label(df$var_name)
    }
  }
  if (!all(c("class", "label", "mean", "se") %in% names(df))) return(NULL)
  df <- apply_latent_group_order(df, class_col = "class", class_num_col = "class_num")

  df$mean <- safe_num(df$mean)
  df$se   <- safe_num(df$se)
  df <- df[!is.na(df$mean) & !is.na(df$se), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  df$ymin <- df$mean - 1.96 * df$se
  df$ymax <- df$mean + 1.96 * df$se
  df$label_wrapped <- make_wrapped_factor(df$label, width = 18)

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$label_wrapped,
      y = .data$mean,
      group = .data$class,
      linetype = .data$class,
      shape = .data$class
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0.08,
      linewidth = 0.35
    ) +
    latent_group_style_scales(levels(df$class)) +
    ggplot2::labs(title = title, x = NULL, y = "Mean (95% CI)") +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 1,
        face = "plain",
        family = "sans",
        lineheight = 0.95
      )
    )
}

build_F6_bch_interaction_plot <- function(
    df,
    outcome_filter = NULL,
    moderator_filter = NULL,
    use_ci = FALSE
) {
  df <- safe_df(df)
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  need_cols <- c("moderator", "moderator_level", "class_num", "estimate", "se", "var_name")
  if (!all(need_cols %in% names(df))) return(NULL)

  df$class_num <- as.integer(df$class_num)
  df$estimate  <- as.numeric(df$estimate)
  df$se        <- as.numeric(df$se)

  df <- df[!is.na(df$class_num) & !is.na(df$estimate), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  if (!is.null(outcome_filter)) df <- df[df$var_name %in% outcome_filter, , drop = FALSE]
  if (!is.null(moderator_filter)) df <- df[df$moderator %in% moderator_filter, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  df$Profile <- factor(
    latent_group_label(df$class_num),
    levels = latent_group_label(order_latent_group_nums(df$class_num))
  )
  df$Outcome <- get_var_label(df$var_name)
  df$Group   <- as.character(df$moderator_level)

  if (use_ci) {
    df$ymin <- df$estimate - 1.96 * df$se
    df$ymax <- df$estimate + 1.96 * df$se
  } else {
    df$ymin <- df$estimate - df$se
    df$ymax <- df$estimate + df$se
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Profile,
      y = estimate,
      group = Group,
      linetype = Group,
      shape = Group
    )
  ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0.10,
      linewidth = 0.35
    ) +
    ggplot2::facet_wrap(~ Outcome, scales = "free_y") +
    ggplot2::labs(title = paste0("BCH interaction plot by ", latent_group_term_lower()), x = NULL, y = "Estimate") +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )
}

build_F6_moderator_profile_mean_plot <- function(df) {
  df <- safe_df(df)
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  if (all(c("class_num", "moderator", "moderator_level", "estimate") %in% names(df))) {
    summ <- df[, c("class_num", "moderator", "moderator_level", "estimate"), drop = FALSE]
    summ$class_num <- safe_int(summ$class_num)
    summ$estimate <- safe_num(summ$estimate)
    summ$moderator <- as.character(summ$moderator)
    summ$moderator_level <- as.character(summ$moderator_level)
    summ <- summ[
      !is.na(summ$class_num) &
        !is.na(summ$estimate) &
        nzchar(trimws(summ$moderator)) &
        nzchar(trimws(summ$moderator_level)),
      ,
      drop = FALSE
    ]
    if (nrow(summ) == 0) return(NULL)

    summ$group <- paste0(summ$moderator, "::", summ$moderator_level)
    summ$group_label <- vapply(
      seq_len(nrow(summ)),
      function(i) {
        mod_lab <- if (exists("get_var_label")) get_var_label(summ$moderator[i]) else summ$moderator[i]
        lv_lab <- if (exists("get_value_label")) get_value_label(summ$moderator[i], summ$moderator_level[i]) else summ$moderator_level[i]
        if (is.na(lv_lab) || !nzchar(trimws(lv_lab))) lv_lab <- summ$moderator_level[i]
        paste(mod_lab, lv_lab)
      },
      character(1)
    )
    summ$mean <- summ$estimate
  } else {
    if (!all(c("class_num", "group", "y") %in% names(df))) return(NULL)

    dat <- df[, c("class_num", "group", "y"), drop = FALSE]
    dat$class_num <- safe_int(dat$class_num)
    dat$group <- as.character(dat$group)
    dat$y <- safe_num(dat$y)
    dat <- dat[!is.na(dat$class_num) & !is.na(dat$group) & nzchar(trimws(dat$group)) & !is.na(dat$y), , drop = FALSE]
    if (nrow(dat) == 0) return(NULL)

    summ <- stats::aggregate(
      y ~ class_num + group,
      data = dat,
      FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE))
    )
    if (!is.data.frame(summ) || nrow(summ) == 0) return(NULL)

    y_mat <- summ$y
    if (is.list(y_mat)) {
      y_mat <- do.call(rbind, y_mat)
    }
    y_df <- as.data.frame(y_mat, stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(y_df) < 2) return(NULL)
    summ$mean <- y_df[[1]]
    summ$sd <- y_df[[2]]
    summ$y <- NULL

    grp_num <- suppressWarnings(as.numeric(summ$group))
    if (all(!is.na(grp_num))) {
      grp_levels <- as.character(sort(unique(grp_num)))
      summ$group_label <- paste("group", grp_levels[match(summ$group, grp_levels)])
    } else {
      grp_levels <- sort(unique(summ$group))
      summ$group_label <- summ$group
    }
  }

  group_order_df <- unique(summ[, c("group", "group_label"), drop = FALSE])
  group_order_df$..ord.. <- suppressWarnings(as.numeric(sub("^.*::", "", group_order_df$group)))
  group_order_df$..ord..[is.na(group_order_df$..ord..)] <- seq_len(sum(is.na(group_order_df$..ord..))) + 9999
  group_order_df <- group_order_df[order(group_order_df$..ord.., group_order_df$group_label), , drop = FALSE]

  prof_levels <- order_latent_group_nums(summ$class_num)
  summ$Profile <- factor(latent_group_label(summ$class_num), levels = latent_group_label(prof_levels))
  summ$group_label <- factor(summ$group_label, levels = unique(group_order_df$group_label))

  ggplot2::ggplot(
    summ,
    ggplot2::aes(
      x = Profile,
      y = mean,
      group = group_label,
      linetype = group_label,
      shape = group_label
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::labs(
      title = paste0("Distal outcome means by ", latent_group_term_lower(), " across moderator levels"),
      x = NULL,
      y = "Mean"
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )
}

# ------------------------------------------------------------
# LPA radar chart
# ------------------------------------------------------------
save_fig8_radar_lpa <- function(
    df,
    file_stub_filled  = "Fig8_1_radar_filled",
    file_stub_outline = "Fig8_2_radar_outline",
    y_min             = NULL,
    y_max             = NULL,
    y_mode            = c("auto", "fixed"),
    n_seg             = 5
) {
  y_mode <- match.arg(y_mode)
  df <- safe_df(df)
  if (nrow(df) == 0) return(NULL)

  if (!requireNamespace("fmsb", quietly = TRUE)) {
    log_warn("Fig8 radar skipped: package 'fmsb' not installed.")
    return(NULL)
  }
  if (!all(c("label", "class", "mean", "var_name") %in% names(df))) return(NULL)

  radar_src <- df[, c("var_name", "label", "class", "mean"), drop = FALSE]
  radar_src <- radar_src[!duplicated(radar_src[, c("class", "var_name")]), , drop = FALSE]
  radar_src$var_name <- as.character(radar_src$var_name)
  radar_src$label    <- as.character(radar_src$label)
  radar_src$class    <- as.character(radar_src$class)
  radar_src$mean     <- safe_num(radar_src$mean)
  radar_src          <- radar_src[!is.na(radar_src$mean), , drop = FALSE]
  if (nrow(radar_src) == 0) return(NULL)

  ord_map <- get_var_display_order(unique(radar_src$var_name))
  names(ord_map) <- unique(radar_src$var_name)

  var_order <- unique(radar_src$var_name)
  var_order <- var_order[order(ord_map[var_order])]

  lab_map <- stats::setNames(
    wrap_label_text(get_var_label(var_order), width = 14),
    var_order
  )

  wide <- tryCatch(
    tidyr::pivot_wider(
      radar_src,
      id_cols     = class,
      names_from  = var_name,
      values_from = mean
    ),
    error = function(e) NULL
  )
  if (is.null(wide) || nrow(wide) == 0) return(NULL)

  keep_cols <- intersect(var_order, names(wide))
  if (length(keep_cols) == 0) return(NULL)

  radar_mat <- as.data.frame(
    wide[, keep_cols, drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(radar_mat) <- as.character(wide$class)

  for (j in seq_len(ncol(radar_mat))) {
    radar_mat[[j]] <- safe_num(radar_mat[[j]])
  }

  keep_non_na <- vapply(radar_mat, function(x) any(is.finite(x)), logical(1))
  radar_mat <- radar_mat[, keep_non_na, drop = FALSE]

  if (!is.null(rownames(radar_mat))) {
    radar_mat <- radar_mat[!duplicated(rownames(radar_mat)), , drop = FALSE]
  }

  if (ncol(radar_mat) < 3 || nrow(radar_mat) < 1) return(NULL)

  legend_labels <- rownames(radar_mat)
  if (length(legend_labels) != nrow(radar_mat) || all(!nzchar(legend_labels))) {
    legend_labels <- latent_group_label(seq_len(nrow(radar_mat)))
  }
  legend_labels <- normalize_latent_group_display(legend_labels)

  colnames(radar_mat) <- unname(lab_map[colnames(radar_mat)])

  vals <- unlist(radar_mat, use.names = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)

  if (y_mode == "fixed") {
    if (is.null(y_min) || is.null(y_max)) {
      stop("When y_mode = 'fixed', both y_min and y_max must be supplied.")
    }
    vmin <- as.numeric(y_min)
    vmax <- as.numeric(y_max)
  } else {
    vmin <- floor(min(vals, na.rm = TRUE) * 10) / 10
    vmax <- ceiling(max(vals, na.rm = TRUE) * 10) / 10
    if ((vmax - vmin) < 1) {
      mid  <- mean(c(vmin, vmax))
      vmin <- floor((mid - 0.6) * 10) / 10
      vmax <- ceiling((mid + 0.6) * 10) / 10
    }
  }

  if (!is.finite(vmin) || !is.finite(vmax) || vmin >= vmax) {
    stop("Invalid radar axis range: check y_min / y_max or source values.")
  }

  axis_vals <- round(seq(vmin, vmax, length.out = n_seg + 1), 1)

  max_row <- as.data.frame(
    setNames(as.list(rep(vmax, ncol(radar_mat))), names(radar_mat)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  min_row <- as.data.frame(
    setNames(as.list(rep(vmin, ncol(radar_mat))), names(radar_mat)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  radar_plot_df <- base::rbind.data.frame(
    max_row, min_row, radar_mat,
    stringsAsFactors = FALSE
  )
  rownames(radar_plot_df)[1:2] <- c("max", "min")

  n_cls     <- nrow(radar_mat)
  line_cols <- grDevices::gray(seq(0.10, 0.65, length.out = n_cls))
  fill_cols <- grDevices::adjustcolor(line_cols, alpha.f = 0.10)

  draw_radar <- function(filled = TRUE) {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(mar = c(3.5, 3.5, 3.5, 6.5))

    fmsb::radarchart(
      radar_plot_df,
      axistype    = 1,
      seg         = n_seg,
      caxislabels = axis_vals,
      pcol        = line_cols,
      pfcol       = if (filled) fill_cols else rep(NA, n_cls),
      plwd        = 2.2,
      plty        = seq_len(n_cls),
      cglcol      = "grey88",
      cglty       = 1,
      cglwd       = 0.6,
      axislabcol  = "grey45",
      vlcex       = 0.95,
      calcex      = 0.65,
      centerzero  = FALSE,
      title       = if (filled) {
        paste0("Radar ", latent_group_term_lower(), " plot")
      } else {
        paste0("Radar ", latent_group_term_lower(), " plot (outline)")
      }
    )

    graphics::legend(
      "topright",
      inset    = c(-0.22, 0.02),
      legend   = legend_labels,
      col      = line_cols,
      lty      = seq_len(n_cls),
      lwd      = 2.2,
      bty      = "n",
      cex      = 0.95,
      xpd      = NA,
      text.col = "black"
    )
  }

  sp <- get_fig_spec("Fig8_radar_raw")

  save_one <- function(stub, filled = TRUE) {
    png_path  <- file.path(DIR_FIGURES_PNG,  paste0(stub, ".png"))
    single_output_mode <- isTRUE(getOption("easyflow.single_output", TRUE))
    tiff_path <- if (single_output_mode) NA_character_ else file.path(DIR_FIGURES_TIFF, paste0(stub, ".tiff"))
    pdf_path  <- if (single_output_mode) NA_character_ else file.path(DIR_FIGURES_PDF,  paste0(stub, ".pdf"))

    grDevices::png(png_path, width = sp$width, height = sp$height, units = "in", res = sp$dpi)
    draw_radar(filled = filled)
    grDevices::dev.off()

    if (!single_output_mode) {
      grDevices::tiff(tiff_path, width = sp$width, height = sp$height, units = "in", res = sp$dpi, compression = "lzw")
      draw_radar(filled = filled)
      grDevices::dev.off()

      grDevices::pdf(pdf_path, width = sp$width, height = sp$height)
      draw_radar(filled = filled)
      grDevices::dev.off()
    }

    data.frame(
      figure_name = stub,
      file_png    = png_path,
      file_tiff   = tiff_path,
      file_pdf    = pdf_path,
      stringsAsFactors = FALSE
    )
  }

  out1 <- save_one(file_stub_filled, filled = TRUE)
  out2 <- save_one(file_stub_outline, filled = FALSE)

  base::rbind.data.frame(out1, out2, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------
# LCA item-response helpers
# ------------------------------------------------------------
prepare_lca_irp_df <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  need <- c("class", "class_num", "var_name", "mean")
  if (!all(need %in% names(df))) return(data.frame())

  if (!"var_label" %in% names(df)) {
    df$var_label <- get_var_label(df$var_name)
  }

  df$class             <- as.character(df$class)
  df$class_num         <- safe_int(df$class_num)
  df$var_name          <- as.character(df$var_name)
  df$var_label         <- as.character(df$var_label)
  df$resp_value        <- if ("category" %in% names(df)) as.character(df$category) else ""
  df$resp_code_display <- df$resp_value
  df$resp_label_value  <- df$resp_value
  df$mean              <- safe_num(df$mean)
  if ("se" %in% names(df)) {
    df$se <- safe_num(df$se)
  } else {
    df$se <- NA_real_
  }

  df <- df[!is.na(df$class_num) & !is.na(df$mean) & nzchar(df$var_label), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  cls_levels <- latent_group_label(order_latent_group_nums(df$class_num))
  df$class <- factor(latent_group_label(df$class_num), levels = cls_levels)

  ord <- get_var_display_order(unique(df$var_name))
  names(ord) <- unique(df$var_name)

  var_ord <- unique(df$var_name)
  var_ord <- var_ord[order(ord[var_ord])]

  lab_map <- stats::setNames(get_var_label(var_ord), var_ord)
  df$var_label <- factor(as.character(df$var_label), levels = unname(lab_map[var_ord]))

  vars <- unique(as.character(df$var_name))
  for (vv in vars) {
    idx <- which(as.character(df$var_name) == vv)

    # 7-7용 표시 코드
    df$resp_code_display[idx] <- map_display_codes_for_var(
      values   = df$resp_value[idx],
      var_name = vv
    )

    # 7-8용 표시 라벨
    df$resp_label_value[idx] <- map_value_labels_for_var(
      values   = df$resp_code_display[idx],
      var_name = vv
    )
  }

  df
}

order_response_codes <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
  if (length(x_chr) == 0) return(character(0))
  x_chr <- unique(x_chr)

  x_num <- suppressWarnings(as.numeric(x_chr))
  if (all(!is.na(x_num))) {
    return(x_chr[order(x_num)])
  }

  sort(unique(x_chr))
}

wrap_axis_labels <- function(x, width = 12) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- ""
  vapply(
    x_chr,
    function(one) {
      one <- trimws(one)
      if (!nzchar(one)) return("")
      paste(strwrap(one, width = width), collapse = "\n")
    },
    character(1)
  )
}

build_lca_item_response_plot <- function(df, title = NULL, ylab = "Item-response probability") {
  df <- prepare_lca_irp_df(df)
  if (nrow(df) == 0) return(NULL)

  n_var <- length(levels(df$var_label))
  wrap_width <- if (n_var >= 16) 10 else if (n_var >= 12) 12 else 18
  strip_size <- if (n_var >= 16) 9 else if (n_var >= 12) 10 else 11

  df$item_wrapped <- make_wrapped_factor(df$var_label, width = wrap_width)

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- pmax(0, df$mean - df$se)
    df$ymax <- pmin(1, df$mean + df$se)
  }

  df$x_code <- as.character(df$resp_code_display)

  vars            <- unique(as.character(df$var_name))
  out_list        <- vector("list", length(vars))

  for (i in seq_along(vars)) {
    vv            <- vars[i]
    sub           <- df[as.character(df$var_name) == vv, , drop = FALSE]

    ord_val       <- order_response_codes(sub$resp_code_display)
    panel_levels  <- paste0(vv, "::", ord_val)
    sub$x_code    <- factor(
      paste0(vv, "::", as.character(sub$resp_code_display)),
      levels = panel_levels
    )
    out_list[[i]] <- sub
  }

  df2 <- do.call(rbind, out_list)
  rownames(df2) <- NULL

  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(
      x        = x_code,
      y        = mean,
      group    = class,
      linetype = class,
      shape    = class
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 2.0)

  if (has_se) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0.08,
      linewidth = 0.30
    )
  }

  p +
    latent_group_style_scales(levels(df2$class)) +
    ggh4x::facet_wrap2(
      ~ item_wrapped,
      scales        = "free_x",
      axes          = "all",
      remove_labels = "none"
    ) +
    ggplot2::scale_x_discrete(
      drop = TRUE,
      labels = function(brks) sub("^.*::", "", brks)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ) +
    ggplot2::labs(
      title    = title %||% "",
      x        = "Response category",
      y        = ylab,
      linetype = NULL,
      shape    = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.spacing = grid::unit(1.2, "lines"),
      strip.text = ggplot2::element_text(
        size = strip_size,
        lineheight = 0.8,
        margin = ggplot2::margin(t = 2, b = 2)
      ),
      strip.background = ggplot2::element_blank()
    )
}

build_lca_item_response_plot_value_label <- function(df, title = NULL, ylab = "Item-response probability") {
  df <- prepare_lca_irp_df(df)
  if (nrow(df) == 0) return(NULL)

  n_var <- length(unique(as.character(df$var_label)))
  wrap_width <- if (n_var >= 16) 10 else if (n_var >= 12) 12 else 18
  strip_size <- if (n_var >= 16) 9 else if (n_var >= 12) 10 else 11

  df$item_wrapped <- make_wrapped_factor(df$var_label, width = wrap_width)

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- pmax(0, df$mean - df$se)
    df$ymax <- pmin(1, df$mean + df$se)
  }

  df$x_val_label <- as.character(df$resp_label_value)

  vars <- unique(as.character(df$var_name))
  out_list <- vector("list", length(vars))

  for (i in seq_along(vars)) {
    vv  <- vars[i]
    sub <- df[as.character(df$var_name) == vv, , drop = FALSE]

    ord_code <- order_response_codes(sub$resp_code_display)
    ord_lab  <- vapply(
      ord_code,
      function(z) {
        hit <- unique(as.character(sub$x_val_label[as.character(sub$resp_code_display) == z]))
        hit <- hit[!is.na(hit) & nzchar(trimws(hit))]
        if (length(hit) > 0) hit[1] else z
      },
      character(1)
    )

    ord_lab_wrapped <- wrap_axis_labels(ord_lab, width = 12)
    label_map <- stats::setNames(ord_lab_wrapped, ord_lab)
    panel_levels <- paste0(vv, "::", ord_lab_wrapped)
    sub$x_val_label <- factor(
      paste0(vv, "::", unname(label_map[as.character(sub$x_val_label)])),
      levels = panel_levels
    )
    out_list[[i]] <- sub
  }

  df2 <- do.call(rbind, out_list)
  rownames(df2) <- NULL

  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(
      x        = x_val_label,
      y        = mean,
      group    = class,
      linetype = class,
      shape    = class
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 2.0)

  if (has_se) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0.08,
      linewidth = 0.30
    )
  }

  p +
    latent_group_style_scales(levels(df2$class)) +
    ggh4x::facet_wrap2(
      ~ item_wrapped,
      scales        = "free_x",
      axes          = "all",
      remove_labels = "none"
    ) +
    ggplot2::scale_x_discrete(
      drop = TRUE,
      labels = function(brks) sub("^.*::", "", brks)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ) +
    ggplot2::labs(
      title    = title %||% "",
      x        = "Response label",
      y        = ylab,
      linetype = NULL,
      shape    = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 10, lineheight = 0.9),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.spacing = grid::unit(1.2, "lines"),
      strip.text = ggplot2::element_text(
        size = strip_size,
        lineheight = 0.8,
        margin = ggplot2::margin(t = 2, b = 2)
      ),
      strip.background = ggplot2::element_blank()
    )
}

build_lca_item_response_facet_plot <- function(df, title = NULL, ylab = "Item-response probability") {
  df <- prepare_lca_irp_df(df)
  if (nrow(df) == 0) return(NULL)

  n_var <- length(levels(df$var_label))
  wrap_width <- if (n_var >= 16) 10 else if (n_var >= 12) 12 else 18
  strip_size <- if (n_var >= 16) 9 else if (n_var >= 12) 10 else 11

  df$item_wrapped <- make_wrapped_factor(df$var_label, width = wrap_width)

  has_se <- "se" %in% names(df) && any(!is.na(df$se))
  if (has_se) {
    df$ymin <- pmax(0, df$mean - df$se)
    df$ymax <- pmin(1, df$mean + df$se)
  }

  df$x_code <- as.character(df$resp_value)

  vars <- unique(as.character(df$var_name))
  out_list <- vector("list", length(vars))

  for (i in seq_along(vars)) {
    vv  <- vars[i]
    sub <- df[as.character(df$var_name) == vv, , drop = FALSE]

    ord_val <- sort(unique(as.character(sub$resp_value)))
    sub$x_code <- factor(as.character(sub$resp_value), levels = ord_val)
    out_list[[i]] <- sub
  }

  df2 <- do.call(rbind, out_list)
  rownames(df2) <- NULL

  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(
      x        = x_code,
      y        = mean,
      group    = class,
      linetype = class,
      shape    = class
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_point(size = 2.0)

  if (has_se) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ymin, ymax = ymax),
      width = 0.08,
      linewidth = 0.30
    )
  }

  p +
    latent_group_style_scales(levels(df2$class)) +
    ggh4x::facet_wrap2(
      ~ item_wrapped,
      scales        = "free_x",
      axes          = "all",
      remove_labels = "none"
    ) +
    ggplot2::scale_x_discrete(drop = TRUE) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ) +
    ggplot2::labs(
      title    = title %||% "",
      x        = "Response category",
      y        = ylab,
      linetype = NULL,
      shape    = NULL
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8),
      strip.text  = ggplot2::element_text(size = strip_size, lineheight = 0.95)
    )
}

build_lca_item_response_horizontal_plot <- function(df, title = NULL, xlab = "Probability") {
  df <- prepare_lca_irp_df(df)
  if (nrow(df) == 0) return(NULL)

  df$var_label <- factor(df$var_label, levels = rev(levels(df$var_label)))

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = mean,
      y = var_label,
      shape = resp_label_value
    )
  ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.45),
      size = 2.2
    ) +
    ggplot2::facet_wrap(~ class) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(
      title = title %||% "",
      x = xlab,
      y = NULL,
      shape = "Response"
    ) +
    theme_sci_ref() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8.5, lineheight = 0.9)
    )

  p
}

# ------------------------------------------------------------
# prep profile inputs
# ------------------------------------------------------------
if (mixture_mode == "lpa") {
  best_out_file <- BEST_MODEL_ROW$out_file[1]
  if (is.null(best_out_file) || is.na(best_out_file) || !nzchar(best_out_file) || !file.exists(best_out_file)) {
    best_out_file <- file.path(DIR_MPLUS_OUT, paste0(best_tag, ".out"))
  }

  indicators_use <- SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||%
    SETTINGS_SUMMARY$indicators_continuous %||%
    SETTINGS_SUMMARY$INDICATORS %||%
    SETTINGS_SUMMARY$indicators %||%
    character(0)

  T3_indicator_profile <- parse_lpa_indicator_profile(
    out_file   = best_out_file,
    indicators = indicators_use,
    model_tag  = best_tag
  )

  if (is.data.frame(T3_indicator_profile) && nrow(T3_indicator_profile) > 0) {
    T3_indicator_profile$class <- T3_indicator_profile$Class
    T3_indicator_profile$var_label <- get_var_label(T3_indicator_profile$var_name)
  }

  t3_raw <- resolve_profile_df(T3_indicator_profile, mixture_mode = mixture_mode)
} else {
  t3_raw <- safe_df(T3_indicator_profile)

  if (nrow(t3_raw) > 0) {
    if (!"class" %in% names(t3_raw) && "class_num" %in% names(t3_raw)) {
      t3_raw$class <- latent_group_label(t3_raw$class_num)
    }
    if (!"class_num" %in% names(t3_raw) && "class" %in% names(t3_raw)) {
      t3_raw$class_num <- safe_int(gsub("[^0-9]", "", as.character(t3_raw$class)))
    }
    if (!"var_label" %in% names(t3_raw) && "var_name" %in% names(t3_raw)) {
      t3_raw$var_label <- get_var_label(t3_raw$var_name)
    }
    if (!"category" %in% names(t3_raw) && "label" %in% names(t3_raw)) {
      t3_raw$category <- as.character(t3_raw$label)
    }

    t3_raw$class     <- as.character(t3_raw$class)
    t3_raw$class_num <- safe_int(t3_raw$class_num)
    t3_raw$var_name  <- as.character(t3_raw$var_name)
    t3_raw$var_label <- as.character(t3_raw$var_label)
    t3_raw$category  <- as.character(t3_raw$category)
    t3_raw$mean      <- safe_num(t3_raw$mean)
    if ("se" %in% names(t3_raw)) t3_raw$se <- safe_num(t3_raw$se)

    t3_raw <- t3_raw[!is.na(t3_raw$class_num) & !is.na(t3_raw$mean), , drop = FALSE]
  }
}

t3_z <- resolve_profile_df(T3_indicator_profile_z, mixture_mode = mixture_mode)
if (nrow(t3_z) == 0) t3_z <- resolve_profile_df(TABLE_A4_Z, mixture_mode = mixture_mode)
if (nrow(t3_z) == 0) t3_z <- resolve_profile_df(A4_indicator_profile_z, mixture_mode = mixture_mode)
if (nrow(t3_z) == 0 && mixture_mode == "lpa") t3_z <- derive_profile_z_df(t3_raw)

mixed_cat_profile_df <- data.frame()
indicators_categorical <- SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||%
  SETTINGS_SUMMARY$indicators_categorical %||%
  character(0)
weight_var_fig <- SETTINGS_SUMMARY$WEIGHT_VAR %||% SETTINGS_SUMMARY$weight_var %||% NULL

if (length(indicators_categorical) > 0) {
  classified_src_fig <- safe_df(CLASSIFIED_ANALYSIS)
  if (nrow(classified_src_fig) == 0) classified_src_fig <- safe_df(ANALYSIS_DATA_CLASSIFIED)
  mixed_cat_profile_df <- build_categorical_profile_df(
    df = classified_src_fig,
    indicators = indicators_categorical,
    weight_var = weight_var_fig
  )
}

# ------------------------------------------------------------
# 10. Fig1 model fit
# ------------------------------------------------------------
log_info("Building Fig1_model_fit ...")

p1 <- NULL
fit_df_fig1 <- data.frame()

if (is.data.frame(FIT_SUMMARY) && nrow(FIT_SUMMARY) > 0) {
  fit_df_fig1 <- FIT_SUMMARY

  if ("BIC" %in% names(fit_df_fig1) && !"bic" %in% names(fit_df_fig1)) fit_df_fig1$bic <- safe_num(fit_df_fig1$BIC)
  if ("AIC" %in% names(fit_df_fig1) && !"aic" %in% names(fit_df_fig1)) fit_df_fig1$aic <- safe_num(fit_df_fig1$AIC)
  if ("SABIC" %in% names(fit_df_fig1) && !"sabic" %in% names(fit_df_fig1)) fit_df_fig1$sabic <- safe_num(fit_df_fig1$SABIC)
  if ("DBIC" %in% names(fit_df_fig1) && !"dbic" %in% names(fit_df_fig1)) fit_df_fig1$dbic <- safe_num(fit_df_fig1$DBIC)
  if ("Entropy" %in% names(fit_df_fig1) && !"entropy" %in% names(fit_df_fig1)) fit_df_fig1$entropy <- safe_num(fit_df_fig1$Entropy)
  if (!"model_structure" %in% names(fit_df_fig1)) fit_df_fig1$model_structure <- model_structure

  p1 <- tryCatch(
    build_fig_model_fit(
      fit_df  = fit_df_fig1,
      metrics = c("dbic", "bic", "aic", "sabic"),
      title   = fit_title
    ) +
      ggplot2::labs(x = paste0("Number of ", tolower(latent_group_term()), "s (k)")) +
      theme_sci_ref(),
    error = function(e) {
      log_warn("Fig1_model_fit build failed: ", conditionMessage(e))
      NULL
    }
  )
}


FIG1_DATA_VALIDATION <- validate_required_data_fig1(fit_df_fig1)
FIG1_OBJ_VALIDATION  <- validate_figure_object("Fig1_model_fit", p1)

sp <- get_fig_spec("Fig1_model_fit")

if (!is.null(p1)) {
  m1 <- save_fig(
    plot_obj     = p1,
    file_stub    = "Fig1_model_fit",
    figure_title = fit_title,
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m1)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig1_model_fit")
} else {
  log_warn("Fig1_model_fit skipped.")
}

# ------------------------------------------------------------
# 11. Fig2 profile size
# ------------------------------------------------------------
log_info("Building Fig2_class_proportion ...")

p2 <- NULL
class_df_fig2 <- data.frame()

if (is.data.frame(CLASS_SUMMARY_FINAL) && nrow(CLASS_SUMMARY_FINAL) > 0) {
  class_df_fig2 <- CLASS_SUMMARY_FINAL

  # --------------------------------------------------
  # FIX: Fig2는 CLASS_SUMMARY_FINAL의 실제 percent를 우선 사용
  # 정확한 위치: class_df_fig2 <- CLASS_SUMMARY_FINAL 바로 아래
  # --------------------------------------------------
  if ("class_num" %in% names(class_df_fig2)) {
    class_df_fig2$class_num <- safe_int(class_df_fig2$class_num)
    class_df_fig2 <- class_df_fig2[order(class_df_fig2$class_num), , drop = FALSE]
  }

  if ("percent" %in% names(class_df_fig2)) {
    class_df_fig2$prop <- safe_num(class_df_fig2$percent) / 100
  } else if ("weighted_percent" %in% names(class_df_fig2)) {
    class_df_fig2$prop <- safe_num(class_df_fig2$weighted_percent) / 100
  }

  if ("Class" %in% names(class_df_fig2) && !"class" %in% names(class_df_fig2)) {
    class_df_fig2$class <- as.character(class_df_fig2$Class)
  }

  if ("class_num" %in% names(class_df_fig2)) {
    class_df_fig2$class_num <- safe_int(class_df_fig2$class_num)
  } else if ("Class_num" %in% names(class_df_fig2)) {
    class_df_fig2$class_num <- safe_int(class_df_fig2$Class_num)
  } else {
    class_df_fig2$class_num <- safe_int(gsub("[^0-9]", "", as.character(class_df_fig2$class)))
  }

  if ("prop" %in% names(class_df_fig2)) {
    prop_num <- safe_num(class_df_fig2$prop)
    if (all(is.na(prop_num))) {
      class_df_fig2$prop <- NA_real_
    } else if (max(prop_num, na.rm = TRUE) > 1) {
      class_df_fig2$prop <- prop_num / 100
    } else {
      class_df_fig2$prop <- prop_num
    }
  } else if ("pct" %in% names(class_df_fig2)) {
    pct_num <- safe_num(class_df_fig2$pct)
    class_df_fig2$prop <- ifelse(
      is.na(pct_num),
      NA_real_,
      ifelse(max(pct_num, na.rm = TRUE) > 1, pct_num / 100, pct_num)
    )
  } else if ("percent" %in% names(class_df_fig2)) {
    pct_num <- safe_num(class_df_fig2$percent)
    class_df_fig2$prop <- ifelse(
      is.na(pct_num),
      NA_real_,
      ifelse(max(pct_num, na.rm = TRUE) > 1, pct_num / 100, pct_num)
    )
  } else if ("weighted_percent" %in% names(class_df_fig2)) {
    pct_num <- safe_num(class_df_fig2$weighted_percent)
    class_df_fig2$prop <- ifelse(
      is.na(pct_num),
      NA_real_,
      ifelse(max(pct_num, na.rm = TRUE) > 1, pct_num / 100, pct_num)
    )
  } else if ("n" %in% names(class_df_fig2)) {
    n_num <- safe_num(class_df_fig2$n)
    class_df_fig2$prop <- n_num / sum(n_num, na.rm = TRUE)
  }

  sprop <- sum(class_df_fig2$prop, na.rm = TRUE)
  if (is.finite(sprop) && sprop > 0) class_df_fig2$prop <- class_df_fig2$prop / sprop

  class_df_fig2$class <- factor(
    normalize_latent_group_text(as.character(class_df_fig2$class)),
    levels = latent_group_label(order_latent_group_nums(class_df_fig2$class_num))
  )

  p2 <- tryCatch({
    ggplot2::ggplot(class_df_fig2, ggplot2::aes(x = class, y = prop)) +
      ggplot2::geom_col(width = 0.65, colour = "black", linewidth = 0.25) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(sprintf("%.1f", prop * 100), "%")),
        vjust = -0.30,
        size = 3.2
      ) +
      scale_y_percent_sci_local(expand = ggplot2::expansion(mult = c(0, 0.08))) +
      ggplot2::labs(title = class_title, x = NULL, y = "Proportion") +
      theme_sci_ref()
  }, error = function(e) {
    log_warn("Fig2_class_proportion build failed: ", conditionMessage(e))
    NULL
  })
}

FIG2_DATA_VALIDATION <- validate_required_data_fig2(class_df_fig2)
FIG2_OBJ_VALIDATION  <- validate_figure_object("Fig2_class_proportion", p2)

sp <- get_fig_spec("Fig2_class_proportion")

if (!is.null(p2)) {
  m2 <- save_fig(
    plot_obj     = p2,
    file_stub    = "Fig2_class_proportion",
    figure_title = class_title,
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m2)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig2_class_proportion")
} else {
  log_warn("Fig2_class_proportion skipped.")
}

# ------------------------------------------------------------
# 12. Fig3 entropy / classification quality
# ------------------------------------------------------------
log_info("Building Fig3 entropy / classification quality ...")

p3_entropy <- tryCatch(
  build_fig3_entropy(FIT_SUMMARY),
  error = function(e) {
    log_warn("Fig3_entropy build failed: ", conditionMessage(e))
    NULL
  }
)

p3_quality <- tryCatch(
  build_fig3_classification_quality(CLASSIFICATION_QUALITY),
  error = function(e) {
    log_warn("Fig3_classification_quality build failed: ", conditionMessage(e))
    NULL
  }
)

FIG3_ENTROPY_DATA_VALIDATION <- data.frame(
  figure_name = "Fig3_entropy",
  valid = is.data.frame(FIT_SUMMARY) && nrow(FIT_SUMMARY) > 0 &&
    (("entropy" %in% names(FIT_SUMMARY)) || ("Entropy" %in% names(FIT_SUMMARY))),
  issue = if (!(is.data.frame(FIT_SUMMARY) && nrow(FIT_SUMMARY) > 0 &&
                (("entropy" %in% names(FIT_SUMMARY)) || ("Entropy" %in% names(FIT_SUMMARY))))) {
    "entropy data is unavailable"
  } else "",
  stringsAsFactors = FALSE
)

FIG3_ENTROPY_OBJ_VALIDATION <- validate_figure_object("Fig3_entropy", p3_entropy)

FIG3_QUALITY_DATA_VALIDATION <- data.frame(
  figure_name = "Fig3_classification_quality",
  valid = is.data.frame(CLASSIFICATION_QUALITY) && nrow(CLASSIFICATION_QUALITY) > 0,
  issue = if (!(is.data.frame(CLASSIFICATION_QUALITY) && nrow(CLASSIFICATION_QUALITY) > 0)) {
    "classification quality data is unavailable"
  } else "",
  stringsAsFactors = FALSE
)

FIG3_QUALITY_OBJ_VALIDATION <- validate_figure_object("Fig3_classification_quality", p3_quality)

sp <- get_fig_spec("Fig3_entropy")

if (!is.null(p3_entropy)) {
  m3_entropy <- save_fig(
    plot_obj     = p3_entropy,
    file_stub    = "Fig3_entropy",
    figure_title = paste0("Entropy across candidate ", latent_group_term_lower(), " solutions"),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m3_entropy)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig3_entropy")
} else {
  log_warn("Fig3_entropy skipped.")
}

sp <- get_fig_spec("Fig3_classification_quality")

if (!is.null(p3_quality)) {
  m3_quality <- save_fig(
    plot_obj     = p3_quality,
    file_stub    = "Fig3_classification_quality",
    figure_title = paste0("Classification quality bands by ", latent_group_term_lower()),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m3_quality)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig3_classification_quality")
} else {
  log_warn("Fig3_classification_quality skipped.")
}

# ------------------------------------------------------------
# 13. Fig4 heatmaps
# ------------------------------------------------------------
log_info("Building Fig4 heatmaps ...")

p4_1 <- NULL
p4_2 <- NULL

if (nrow(t3_raw) > 0) {
  p4_1 <- tryCatch(
    build_profile_heatmap(
      df       = t3_raw,
      title    = "Indicator mean heatmap (raw scale)",
      fill_lab = "Mean"
    ),
    error      = function(e) {
      log_warn("Fig4_1_indicator_heatmap_raw build failed: ", conditionMessage(e))
      NULL
    }
  )
}

if (nrow(t3_z) > 0) {
  p4_2 <- tryCatch(
    build_profile_heatmap(
      df       = t3_z,
      title    = "Indicator mean heatmap (standardized)",
      fill_lab = "Z"
    ),
    error      = function(e) {
      log_warn("Fig4_2_indicator_heatmap_z build failed: ", conditionMessage(e))
      NULL
    }
  )
}

FIG4_1_DATA_VALIDATION <- data.frame(
  figure_name      = "Fig4_1_indicator_heatmap_raw",
  valid            = is.data.frame(t3_raw) && nrow(t3_raw) > 0,
  issue            = if (!(is.data.frame(t3_raw) && nrow(t3_raw) > 0)) "raw profile data is unavailable" else "",
  stringsAsFactors = FALSE
)
FIG4_1_OBJ_VALIDATION <- validate_figure_object("Fig4_1_indicator_heatmap_raw", p4_1)

FIG4_2_DATA_VALIDATION <- data.frame(
  figure_name      = "Fig4_2_indicator_heatmap_z",
  valid            = is.data.frame(t3_z) && nrow(t3_z) > 0,
  issue            = if (!(is.data.frame(t3_z) && nrow(t3_z) > 0)) "z profile data is unavailable" else "",
  stringsAsFactors = FALSE
)
FIG4_2_OBJ_VALIDATION <- validate_figure_object("Fig4_2_indicator_heatmap_z", p4_2)

sp <- get_fig_spec("Fig4_1_indicator_heatmap_raw")

if (!is.null(p4_1)) {
  m4_1 <- save_fig(
    plot_obj     = p4_1,
    file_stub    = "Fig4_1_indicator_heatmap_raw",
    figure_title = "Indicator mean heatmap (raw scale)",
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m4_1)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig4_1_indicator_heatmap_raw")
} else {
  log_warn("Fig4_1_indicator_heatmap_raw skipped.")
}

sp <- get_fig_spec("Fig4_2_indicator_heatmap_z")

if (!is.null(p4_2)) {
  m4_2 <- save_fig(
    plot_obj     = p4_2,
    file_stub    = "Fig4_2_indicator_heatmap_z",
    figure_title = "Indicator mean heatmap (standardized)",
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m4_2)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig4_2_indicator_heatmap_z")
} else {
  log_warn("Fig4_2_indicator_heatmap_z skipped.")
}

# ------------------------------------------------------------
# 14. Fig5 posterior max
# ------------------------------------------------------------
log_info("Building Fig5_posterior_max ...")

p5 <- tryCatch(
  build_posterior_max_plot(POSTERIOR_MAX_DF),
  error = function(e) {
    log_warn("Fig5_posterior_max build failed: ", conditionMessage(e))
    NULL
  }
)

FIG5_DATA_VALIDATION <- data.frame(
  figure_name = "Fig5_posterior_max",
  valid = is.data.frame(POSTERIOR_MAX_DF) && nrow(POSTERIOR_MAX_DF) > 0,
  issue = if (!(is.data.frame(POSTERIOR_MAX_DF) && nrow(POSTERIOR_MAX_DF) > 0)) "posterior max data is unavailable" else "",
  stringsAsFactors = FALSE
)
FIG5_OBJ_VALIDATION <- validate_figure_object("Fig5_posterior_max", p5)

sp <- get_fig_spec("Fig5_posterior_max")

if (!is.null(p5)) {
  m5 <- save_fig(
    plot_obj     = p5,
    file_stub    = "Fig5_posterior_max",
    figure_title = paste0("Posterior max probability by ", latent_group_term_lower()),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m5)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig5_posterior_max")
} else {
  log_warn("Fig5_posterior_max skipped.")
}

p5_2 <- tryCatch(
  build_app_occ_plot(A5_CLASSIFICATION_SUMMARY),
  error = function(e) {
    log_warn("Fig5_2_app_occ_by_profile build failed: ", conditionMessage(e))
    NULL
  }
)

p5_2_bw <- tryCatch(
  build_app_occ_plot(
    A5_CLASSIFICATION_SUMMARY,
    title = paste0("Classification quality by ", latent_group_term_lower(), " (grayscale)"),
    monochrome = TRUE
  ),
  error = function(e) {
    log_warn("Fig5_2_app_occ_by_profile_bw build failed: ", conditionMessage(e))
    NULL
  }
)

FIG5_2_DATA_VALIDATION <- data.frame(
  figure_name = "Fig5_2_app_occ_by_profile",
  valid = is.data.frame(A5_CLASSIFICATION_SUMMARY) &&
    nrow(A5_CLASSIFICATION_SUMMARY) > 0 &&
    all(c("Profile", "APP", "OCC") %in% names(A5_CLASSIFICATION_SUMMARY)),
  issue = if (!(is.data.frame(A5_CLASSIFICATION_SUMMARY) &&
    nrow(A5_CLASSIFICATION_SUMMARY) > 0 &&
    all(c("Profile", "APP", "OCC") %in% names(A5_CLASSIFICATION_SUMMARY)))) {
      "A5 classification summary with APP/OCC is unavailable"
    } else "",
  stringsAsFactors = FALSE
)

FIG5_2_OBJ_VALIDATION <- validate_figure_object("Fig5_2_app_occ_by_profile", p5_2)

sp <- get_fig_spec("Fig5_2_app_occ_by_profile")

if (!is.null(p5_2)) {
  m5_2 <- save_fig(
    plot_obj     = p5_2,
    file_stub    = "Fig5_2_app_occ_by_profile",
    figure_title = paste0("APP and OCC by ", latent_group_term_lower()),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m5_2)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig5_2_app_occ_by_profile")
} else {
  log_warn("Fig5_2_app_occ_by_profile skipped.")
}

if (!is.null(p5_2_bw)) {
  m5_2_bw <- save_fig(
    plot_obj     = p5_2_bw,
    file_stub    = "Fig5_2_app_occ_by_profile_bw",
    figure_title = paste0("APP and OCC by ", latent_group_term_lower(), " (grayscale)"),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m5_2_bw)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig5_2_app_occ_by_profile_bw")
} else {
  log_warn("Fig5_2_app_occ_by_profile_bw skipped.")
}

# ------------------------------------------------------------
# 15. Fig6 result figures
# ------------------------------------------------------------
log_info("Building Fig6 result figures ...")

fill_down_chr_local <- function(x) {
  x <- as.character(x)
  last <- ""
  for (i in seq_along(x)) {
    xi <- trimws(x[i])
    if (nzchar(xi)) {
      last <- xi
      x[i] <- xi
    } else {
      x[i] <- last
    }
  }
  x
}

extract_profile_num_local <- function(x) {
  suppressWarnings(as.integer(gsub("^.*(Profile|Class)\\s*([0-9]+).*$", "\\2", as.character(x))))
}

prepare_r3step_forest_df <- function(tbl) {
  tbl <- safe_df(tbl)
  if (nrow(tbl) == 0) return(data.frame())
  if (!all(c("Variable", "Category") %in% names(tbl))) return(data.frame())

  var_raw  <- as.character(tbl$Variable)
  var_fill <- fill_down_chr_local(var_raw)
  cat_chr  <- as.character(tbl$Category)
  cat_chr[is.na(cat_chr)] <- ""

  rrr_cols <- grep("^RRR__", names(tbl), value = TRUE)
  if (length(rrr_cols) == 0) return(data.frame())

  out_list <- vector("list", length(rrr_cols))

  for (i in seq_along(rrr_cols)) {
    col_rrr  <- rrr_cols[i]
    comp_i   <- sub("^RRR__", "", col_rrr)
    col_llci <- paste0("LLCI__", comp_i)
    col_ulci <- paste0("ULCI__", comp_i)
    col_p    <- paste0("p__", comp_i)
    col_sig  <- paste0("sig__", comp_i)

    llci_i <- if (col_llci %in% names(tbl)) safe_num(tbl[[col_llci]]) else rep(NA_real_, nrow(tbl))
    ulci_i <- if (col_ulci %in% names(tbl)) safe_num(tbl[[col_ulci]]) else rep(NA_real_, nrow(tbl))
    p_i    <- if (col_p    %in% names(tbl)) as.character(tbl[[col_p]]) else rep("", nrow(tbl))
    sig_i  <- if (col_sig  %in% names(tbl)) as.character(tbl[[col_sig]]) else rep("", nrow(tbl))

    out_list[[i]] <- data.frame(
      Variable_raw = var_raw,
      Variable     = var_fill,
      Category     = cat_chr,
      display_label = ifelse(nzchar(trimws(cat_chr)), trimws(cat_chr), trimws(var_fill)),
      Comparison   = comp_i,
      odds_ratio   = safe_num(tbl[[col_rrr]]),
      or_lcl       = llci_i,
      or_ucl       = ulci_i,
      p_raw        = p_i,
      sig_raw      = sig_i,
      row_order    = seq_len(nrow(tbl)),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, out_list)
  if (!is.data.frame(out) || nrow(out) == 0) return(data.frame())

  out$Comparison <- normalize_latent_group_display(trimws(as.character(out$Comparison)))
  out$Variable   <- trimws(as.character(out$Variable))
  out$display_label <- trimws(as.character(out$display_label))

  keep <- !(is.na(out$odds_ratio) & is.na(out$or_lcl) & is.na(out$or_ucl))
  out <- out[keep, , drop = FALSE]
  if (nrow(out) == 0) return(data.frame())

  ref_like <- (is.na(out$or_lcl) | is.na(out$or_ucl)) &
    (!nzchar(trimws(out$p_raw %||% ""))) &
    (!nzchar(trimws(out$sig_raw %||% "")))
  out <- out[!ref_like, , drop = FALSE]
  if (nrow(out) == 0) return(data.frame())

  comp_levels <- unique(out$Comparison[order(extract_profile_num_local(out$Comparison), out$Comparison)])
  var_levels  <- unique(out$Variable[order(out$row_order, out$Variable)])

  out$Comparison     <- factor(out$Comparison, levels = comp_levels)
  out$covariate_block <- factor(out$Variable, levels = var_levels)
  out$sig_dir <- ifelse(
    !is.na(out$or_ucl) & out$or_ucl < 1,
    "sig_lt1",
    ifelse(!is.na(out$or_lcl) & out$or_lcl > 1, "sig_gt1", "ns")
  )
  out$comp_short <- gsub("\\s+", " ", gsub(latent_group_term(), substr(latent_group_term(), 1, 1), as.character(out$Comparison), ignore.case = TRUE))

  out
}

build_category_position_df <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  split_list <- split(df, df$covariate_block, drop = TRUE)
  out_list <- vector("list", length(split_list))
  ii <- 1L

  for (nm in names(split_list)) {
    sub <- split_list[[nm]]
    cat_levels <- unique(sub$display_label[order(sub$row_order, sub$display_label)])
    base_map <- stats::setNames(rev(seq_along(cat_levels)), cat_levels)
    comp_levels <- levels(df$Comparison)
    comp_levels <- comp_levels[comp_levels %in% unique(as.character(sub$Comparison))]
    offs <- seq(from = 0.24, to = -0.24, length.out = max(length(comp_levels), 1L))
    off_map <- stats::setNames(offs, comp_levels)
    sub$y_base <- unname(base_map[sub$display_label])
    sub$y_pos  <- sub$y_base + unname(off_map[as.character(sub$Comparison)])
    sub$cat_levels <- I(list(cat_levels))
    out_list[[ii]] <- sub
    ii <- ii + 1L
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

r3step_title <- paste0("Baseline Covariate Effects on ", latent_group_term(), " Membership Risk")
r3step_df <- prepare_r3step_forest_df(T5B_TABLE)

if (nrow(r3step_df) > 0) {
  comparison_levels_r3 <- levels(r3step_df$Comparison)
  comp_colour_map <- stats::setNames(
    c("#0f4c5c", "#5f0f40", "#e36414", "#9a031e", "#437f97", "#6a994e")[seq_along(comparison_levels_r3)],
    comparison_levels_r3
  )
  comp_shape_map <- stats::setNames(c(16, 15, 17, 18, 8, 7)[seq_along(comparison_levels_r3)], comparison_levels_r3)
  comp_fill_shape_map <- stats::setNames(c(21, 22, 24, 25, 23, 21)[seq_along(comparison_levels_r3)], comparison_levels_r3)
  sig_colour_map_r3 <- c("ns" = "#4D4D4D", "sig_lt1" = "#D55E00", "sig_gt1" = "#0072B2")
  sig_bw_colour_map_r3 <- c("ns" = "#4D4D4D", "sig_lt1" = "#9E9E9E", "sig_gt1" = "#000000")
  sig_colour_labels_r3 <- c(
    "ns" = "Not significant",
    "sig_lt1" = "Significant, RRR < 1",
    "sig_gt1" = "Significant, RRR > 1"
  )
  sig_breaks_present_r3 <- names(sig_colour_labels_r3)[names(sig_colour_labels_r3) %in% unique(as.character(r3step_df$sig_dir))]

  row_levels_r31 <- unique(paste0(sprintf("%03d", r3step_df$row_order), "||", r3step_df$display_label))
  r3step_df$row_factor <- factor(
    paste0(sprintf("%03d", r3step_df$row_order), "||", r3step_df$display_label),
    levels = rev(row_levels_r31)
  )

  p6_1_1 <- ggplot2::ggplot(r3step_df, ggplot2::aes(x = odds_ratio, y = row_factor)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), height = 0, linewidth = 0.65, colour = "#264653") +
    ggplot2::geom_point(size = 2.2, colour = "#264653") +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_discrete(labels = function(x) sub("^\\d+\\|\\|", "", x)) +
    ggplot2::facet_grid(covariate_block ~ Comparison, scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(title = r3step_title, x = "RRR", y = NULL) +
    theme_sci_ref(base_size = 11) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(face = "plain", size = 12),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 11),
      axis.ticks.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0.05, "lines"),
      panel.spacing.x = ggplot2::unit(0.20, "lines"),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
    )

  m6_1_1 <- save_fig(
    plot_obj = p6_1_1,
    file_stub = "Fig6_1_r3step_forest_1_overview",
    figure_title = "R3STEP forest overview",
    width = 11,
    height = max(7.0, 1.15 * length(levels(r3step_df$covariate_block)) + 2.5),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_1)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_1_overview")

  r3step_df2 <- build_category_position_df(r3step_df)
  p6_1_2 <- ggplot2::ggplot(r3step_df2, ggplot2::aes(x = odds_ratio, y = y_pos, colour = Comparison, shape = Comparison)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), height = 0, linewidth = 0.65) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(values = comp_colour_map, name = "Comparison") +
    ggplot2::scale_shape_manual(values = comp_shape_map, name = "Comparison") +
    ggplot2::facet_grid(covariate_block ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(title = r3step_title, x = "RRR", y = NULL) +
    theme_sci_ref(base_size = 11) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11),
      panel.border = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0.0, "lines"),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
    )

  m6_1_2 <- save_fig(
    plot_obj = p6_1_2,
    file_stub = "Fig6_1_r3step_forest_2_by_covariate",
    figure_title = "R3STEP forest by covariate",
    width = 11,
    height = max(6.5, 1.10 * length(levels(r3step_df$covariate_block)) + 2.0),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_2)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_2_by_covariate")

  r3step_df3 <- r3step_df
  r3step_df3$rrr_ci_label <- paste0(
    r3step_df3$display_label, "  ",
    sprintf("%.2f", r3step_df3$odds_ratio), " (",
    sprintf("%.2f", r3step_df3$or_lcl), "-",
    sprintf("%.2f", r3step_df3$or_ucl), ")"
  )
  row_levels_r33 <- unique(paste0(sprintf("%03d", r3step_df3$row_order), "||", r3step_df3$rrr_ci_label))
  r3step_df3$row_factor <- factor(
    paste0(sprintf("%03d", r3step_df3$row_order), "||", r3step_df3$rrr_ci_label),
    levels = rev(row_levels_r33)
  )

  p6_1_3 <- ggplot2::ggplot(r3step_df3, ggplot2::aes(x = odds_ratio, y = row_factor, colour = sig_dir)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), height = 0, linewidth = 0.75) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_discrete(labels = function(x) sub("^\\d+\\|\\|", "", x)) +
    ggplot2::scale_colour_manual(values = sig_colour_map_r3, breaks = sig_breaks_present_r3, labels = unname(sig_colour_labels_r3[sig_breaks_present_r3]), name = NULL) +
    ggplot2::facet_grid(covariate_block ~ Comparison, scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(title = r3step_title, x = "RRR", y = NULL) +
    theme_sci_ref(base_size = 11) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(face = "plain", size = 12),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11),
      panel.border = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0.05, "lines"),
      panel.spacing.x = ggplot2::unit(0.20, "lines"),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
    )

  m6_1_3 <- save_fig(
    plot_obj = p6_1_3,
    file_stub = "Fig6_1_r3step_forest_3_rrr_ci",
    figure_title = "R3STEP forest with RRR and CI labels",
    width = 11,
    height = max(7.5, 1.20 * length(levels(r3step_df$covariate_block)) + 2.8),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_3)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_3_rrr_ci")

  r3step_df4 <- r3step_df
  r3step_df4$rrr_ci_label <- paste0(
    r3step_df4$comp_short, "  ",
    r3step_df4$display_label, "  ",
    sprintf("%.2f", r3step_df4$odds_ratio), " (",
    sprintf("%.2f", r3step_df4$or_lcl), "-",
    sprintf("%.2f", r3step_df4$or_ucl), ")"
  )
  row_levels_r34 <- unique(paste0(sprintf("%03d", r3step_df4$row_order), "__", sprintf("%02d", match(r3step_df4$Comparison, comparison_levels_r3)), "__", r3step_df4$rrr_ci_label))
  r3step_df4$row_factor <- factor(
    paste0(sprintf("%03d", r3step_df4$row_order), "__", sprintf("%02d", match(r3step_df4$Comparison, comparison_levels_r3)), "__", r3step_df4$rrr_ci_label),
    levels = rev(row_levels_r34)
  )

  p6_1_4 <- ggplot2::ggplot(r3step_df4, ggplot2::aes(x = odds_ratio, y = row_factor, colour = sig_dir, shape = Comparison, linetype = Comparison)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), height = 0, linewidth = 0.75) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_discrete(labels = function(x) sub("^\\d+__\\d+__", "", x)) +
    ggplot2::scale_colour_manual(values = sig_colour_map_r3, breaks = sig_breaks_present_r3, labels = unname(sig_colour_labels_r3[sig_breaks_present_r3]), name = NULL) +
    ggplot2::scale_shape_manual(values = comp_shape_map, name = "Comparison") +
    ggplot2::scale_linetype_manual(values = stats::setNames(rep("solid", length(comparison_levels_r3)), comparison_levels_r3), name = "Comparison") +
    ggplot2::facet_grid(covariate_block ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(title = r3step_title, x = "RRR", y = NULL) +
    theme_sci_ref(base_size = 11) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11),
      panel.border = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0.05, "lines"),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
    )

  m6_1_4 <- save_fig(
    plot_obj = p6_1_4,
    file_stub = "Fig6_1_r3step_forest_4_shape_legend",
    figure_title = "R3STEP forest with shape legend",
    width = 10.5,
    height = max(8.0, 1.35 * length(levels(r3step_df$covariate_block)) + 3.0),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_4)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_4_shape_legend")

  p6_1_5 <- p6_1_2 +
    ggplot2::scale_colour_manual(values = stats::setNames(c("#1A1A1A", "#4D4D4D", "#7A7A7A", "#A6A6A6", "#262626", "#595959")[seq_along(comparison_levels_r3)], comparison_levels_r3), name = "Comparison") +
    ggplot2::scale_shape_manual(values = comp_shape_map, name = "Comparison")

  m6_1_5 <- save_fig(
    plot_obj = p6_1_5,
    file_stub = "Fig6_1_r3step_forest_5_by_covariate_bw",
    figure_title = "R3STEP forest by covariate (grayscale)",
    width = 11,
    height = max(6.5, 1.10 * length(levels(r3step_df$covariate_block)) + 2.0),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_5)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_5_by_covariate_bw")

  p6_1_6 <- p6_1_3 +
    ggplot2::scale_colour_manual(values = sig_bw_colour_map_r3, breaks = sig_breaks_present_r3, labels = unname(sig_colour_labels_r3[sig_breaks_present_r3]), name = NULL)

  m6_1_6 <- save_fig(
    plot_obj = p6_1_6,
    file_stub = "Fig6_1_r3step_forest_6_rrr_ci_bw",
    figure_title = "R3STEP forest with RRR and CI labels (grayscale)",
    width = 11,
    height = max(7.5, 1.20 * length(levels(r3step_df$covariate_block)) + 2.8),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_6)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_6_rrr_ci_bw")

  r3step_df7 <- r3step_df4
  r3step_df7$sig_bw <- ifelse(as.character(r3step_df7$sig_dir) == "ns", "ns", "sig")
  p6_1_7 <- ggplot2::ggplot(
    r3step_df7,
    ggplot2::aes(
      x = odds_ratio,
      y = row_factor,
      shape = Comparison,
      linetype = Comparison,
      fill = sig_bw,
      colour = sig_bw
    )
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, colour = "grey40") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_lcl, xmax = or_ucl), height = 0, linewidth = 0.75, colour = "#333333") +
    ggplot2::geom_point(size = 2.8, stroke = 0.8) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_discrete(labels = function(x) sub("^\\d+__\\d+__", "", x)) +
    ggplot2::scale_shape_manual(values = comp_fill_shape_map, name = "Comparison") +
    ggplot2::scale_linetype_manual(values = stats::setNames(rep("solid", length(comparison_levels_r3)), comparison_levels_r3), name = "Comparison") +
    ggplot2::scale_fill_manual(values = c("ns" = "white", "sig" = "#333333"), breaks = c("ns", "sig"), labels = c("Not significant", "Significant"), name = NULL) +
    ggplot2::scale_colour_manual(values = c("ns" = "#333333", "sig" = "#333333"), guide = "none") +
    ggplot2::facet_grid(covariate_block ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(title = r3step_title, x = "RRR", y = NULL) +
    theme_sci_ref(base_size = 11) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, face = "plain", size = 12),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 11),
      panel.border = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0.05, "lines"),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.margin = ggplot2::margin(t = 4, r = 6, b = 4, l = 4)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2),
      shape = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 1)
    )

  m6_1_7 <- save_fig(
    plot_obj = p6_1_7,
    file_stub = "Fig6_1_r3step_forest_7_shape_legend_bw",
    figure_title = "R3STEP forest with shape legend (grayscale)",
    width = 10.5,
    height = max(8.0, 1.35 * length(levels(r3step_df$covariate_block)) + 3.0),
    dpi = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_1_7)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_1_r3step_forest_7_shape_legend_bw")
} else {
  log_warn("Fig6_1_r3step_forest skipped: T5b table is unavailable or empty.")
}

F6_BCH_INTERACTION <- build_F6_bch_interaction_plot(
  df = BCH_MOD_RESULTS_FULL,
  outcome_filter = NULL,
  moderator_filter = NULL,
  use_ci = FALSE
)

FIG6_2_DATA_VALIDATION <- data.frame(
  figure_name = "Fig6_2_bch_interaction",
  valid = !is.null(F6_BCH_INTERACTION),
  issue = if (is.null(F6_BCH_INTERACTION)) "BCH interaction plot could not be built" else "",
  stringsAsFactors = FALSE
)

FIG6_2_OBJ_VALIDATION <- validate_figure_object("Fig6_2_bch_interaction", F6_BCH_INTERACTION)

sp <- get_fig_spec("Fig6_2_bch_interaction")

if (!is.null(F6_BCH_INTERACTION)) {
  m6_2 <- save_fig(
    plot_obj     = F6_BCH_INTERACTION,
    file_stub    = "Fig6_2_bch_interaction",
    figure_title = "BCH interaction plot",
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_2)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_2_bch_interaction")
} else {
  log_warn("Fig6_2_bch_interaction skipped.")
}

F6_MODERATOR_PROFILE_MEANS_SRC <- safe_df(BCH_MOD_RESULTS_FULL)
if (nrow(F6_MODERATOR_PROFILE_MEANS_SRC) == 0) {
  F6_MODERATOR_PROFILE_MEANS_SRC <- safe_df(CLASSIFIED_ANALYSIS)
}
if (nrow(F6_MODERATOR_PROFILE_MEANS_SRC) == 0) {
  F6_MODERATOR_PROFILE_MEANS_SRC <- safe_df(ANALYSIS_DATA_CLASSIFIED)
}

F6_MODERATOR_PROFILE_MEANS <- build_F6_moderator_profile_mean_plot(
  df = F6_MODERATOR_PROFILE_MEANS_SRC
)

FIG6_3_DATA_VALIDATION <- data.frame(
  figure_name = "Fig6_3_moderator_profile_means",
  valid = !is.null(F6_MODERATOR_PROFILE_MEANS),
  issue = if (is.null(F6_MODERATOR_PROFILE_MEANS)) "moderator profile mean plot could not be built" else "",
  stringsAsFactors = FALSE
)

FIG6_3_OBJ_VALIDATION <- validate_figure_object("Fig6_3_moderator_profile_means", F6_MODERATOR_PROFILE_MEANS)

sp <- get_fig_spec("Fig6_3_moderator_profile_means")

if (!is.null(F6_MODERATOR_PROFILE_MEANS)) {
  m6_3 <- save_fig(
    plot_obj     = F6_MODERATOR_PROFILE_MEANS,
    file_stub    = "Fig6_3_moderator_profile_means",
    figure_title = paste0("Distal outcome means by ", latent_group_term_lower(), " across moderator levels"),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m6_3)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig6_3_moderator_profile_means")
} else {
  log_warn("Fig6_3_moderator_profile_means skipped.")
}

# ------------------------------------------------------------
# 16. Fig7 profile line figures
# ------------------------------------------------------------
log_info("Building Fig7 profile line figures ...")

p7_1 <- NULL
p7_2 <- NULL
p7_1_color <- NULL
p7_2_color <- NULL
p7_3 <- NULL
p7_4 <- NULL
p7_5 <- NULL
p7_6 <- NULL
p7_7 <- NULL
p7_8 <- NULL

if (nrow(t3_raw) > 0) {
  p7_1 <- tryCatch(
    build_profile_line_plot(
      df           = t3_raw,
      title        = profile_title_raw,
      ylab         = if (mixture_mode == "lpa") "Mean" else "Probability",
      use_errorbar = TRUE
    ),
    error          = function(e) {
      log_warn("Fig7_1_profile_line_raw build failed: ", conditionMessage(e))
      NULL
    }
  )
  p7_1_color <- tryCatch(
    build_profile_line_plot(
      df           = t3_raw,
      title        = paste0(profile_title_raw, " - color"),
      ylab         = if (mixture_mode == "lpa") "Mean" else "Probability",
      use_errorbar = TRUE,
      color        = TRUE
    ),
    error          = function(e) {
      log_warn("Fig7_1_profile_line_raw_color build failed: ", conditionMessage(e))
      NULL
    }
  )

  p7_1_varname <- tryCatch(
    build_profile_line_plot_varname(
      df           = t3_raw,
      title        = paste0(profile_title_raw, " - var_name"),
      ylab         = if (mixture_mode == "lpa") "Mean" else "Probability",
      use_errorbar = TRUE
    ),
    error          = function(e) {
      log_warn("Fig7_1_profile_line_raw_varname build failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(p7_1_varname)) {
    sp_var <- get_fig_spec("Fig7_1_profile_line_raw")
    m7_1_var <- save_fig(
      plot_obj     = p7_1_varname,
      file_stub    = "Fig7_1_profile_line_raw_varname",
      figure_title = paste0(profile_title_raw, " - var_name"),
      width        = sp_var$width,
      height       = sp_var$height,
      dpi          = sp_var$dpi
    )
    FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_1_var)
    FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_1_profile_line_raw_varname")
  } else {
    log_warn("Fig7_1_profile_line_raw_varname skipped.")
  }

  p7_3 <- tryCatch(
    if (mixture_mode == "lpa") {
      build_profile_facet_plot(
        df         = t3_raw,
        title      = paste0(profile_title_raw, " - faceted"),
        ylab       = "Mean"
      )
    } else {
      build_lca_item_response_facet_plot(
        df         = t3_raw,
        title      = paste0("Item-response probabilities by ", latent_group_term_lower(), " (", model_structure, ", k=", best_k, ") - faceted"),
        ylab       = "Probability"
      )
    },
    error          = function(e) {
      log_warn("Fig7_3_profile_line_raw_facet build failed: ", conditionMessage(e))
      NULL
    }
  )

  if (mixture_mode == "lca") {
    p7_7 <- tryCatch(
      build_lca_item_response_plot(
        df         = t3_raw,
        title      = paste0("Item-response probabilities by ", latent_group_term_lower()),
        ylab       = "Item-response probability"
      ),
      error = function(e) {
        log_warn("Fig7_7_item_response_probability build failed: ", conditionMessage(e))
        NULL
      }
    )

    p7_8 <- tryCatch(
      build_lca_item_response_plot_value_label(
        df    = t3_raw,
        title = paste0("Item-response probabilities by ", latent_group_term_lower(), " (value labels)"),
        ylab  = "Item-response probability"
      ),
      error = function(e) {
        log_warn("Fig7_8_item_response_probability_value_label build failed: ", conditionMessage(e))
        NULL
      }
    )

  }
}

if (nrow(t3_z) > 0) {
  p7_2 <- tryCatch(
    build_profile_line_plot(
      df           = t3_z,
      title        = profile_title_z,
      ylab         = if (mixture_mode == "lpa") "Z score" else "Standardized probability",
      use_errorbar = FALSE
    ),
    error          = function(e) {
      log_warn("Fig7_2_profile_line_z build failed: ", conditionMessage(e))
      NULL
    }
  )
  p7_2_color <- tryCatch(
    build_profile_line_plot(
      df           = t3_z,
      title        = paste0(profile_title_z, " - color"),
      ylab         = if (mixture_mode == "lpa") "Z score" else "Standardized probability",
      use_errorbar = FALSE,
      color        = TRUE
    ),
    error          = function(e) {
      log_warn("Fig7_2_profile_line_z_color build failed: ", conditionMessage(e))
      NULL
    }
  )

  p7_4 <- tryCatch(
    build_profile_facet_plot(
      df           = t3_z,
      title        = paste0(profile_title_z, " - faceted"),
      ylab         = if (mixture_mode == "lpa") "Z score" else "Standardized probability"
    ),
    error          = function(e) {
      log_warn("Fig7_4_profile_line_z_facet build failed: ", conditionMessage(e))
      NULL
    }
  )
}

if (nrow(mixed_cat_profile_df) > 0) {
  p7_5 <- tryCatch(
    build_categorical_profile_plot(
      df = mixed_cat_profile_df,
      title = paste0("Categorical indicator distributions by ", latent_group_term_lower()),
      monochrome = FALSE,
      ylab = if (!is.null(weight_var_fig) && nzchar(as.character(weight_var_fig))) "Weighted %" else "%"
    ),
    error = function(e) {
      log_warn("Fig7_5_categorical_profile_distribution build failed: ", conditionMessage(e))
      NULL
    }
  )
  p7_6 <- tryCatch(
    build_categorical_profile_plot(
      df = mixed_cat_profile_df,
      title = paste0("Categorical indicator distributions by ", latent_group_term_lower()),
      monochrome = TRUE,
      ylab = if (!is.null(weight_var_fig) && nzchar(as.character(weight_var_fig))) "Weighted %" else "%"
    ),
    error = function(e) {
      log_warn("Fig7_6_categorical_profile_distribution_bw build failed: ", conditionMessage(e))
      NULL
    }
  )
}

FIG7_1_DATA_VALIDATION <- validate_required_data_fig_profile(t3_raw, "Fig7_1_profile_line_raw", mixture_mode)
FIG7_1_OBJ_VALIDATION  <- validate_figure_object("Fig7_1_profile_line_raw", p7_1)
FIG7_1_COLOR_OBJ_VALIDATION <- validate_figure_object("Fig7_1_profile_line_raw_color", p7_1_color)

FIG7_2_DATA_VALIDATION <- validate_required_data_fig_profile(t3_z, "Fig7_2_profile_line_z", mixture_mode)
FIG7_2_OBJ_VALIDATION  <- validate_figure_object("Fig7_2_profile_line_z", p7_2)
FIG7_2_COLOR_OBJ_VALIDATION <- validate_figure_object("Fig7_2_profile_line_z_color", p7_2_color)

FIG7_3_DATA_VALIDATION <- validate_required_data_fig_profile(t3_raw, "Fig7_3_profile_line_raw_facet", mixture_mode)
FIG7_3_OBJ_VALIDATION  <- validate_figure_object("Fig7_3_profile_line_raw_facet", p7_3)

FIG7_4_DATA_VALIDATION <- validate_required_data_fig_profile(t3_z, "Fig7_4_profile_line_z_facet", mixture_mode)
FIG7_4_OBJ_VALIDATION  <- validate_figure_object("Fig7_4_profile_line_z_facet", p7_4)

FIG7_5_DATA_VALIDATION <- data.frame(
  figure_name = "Fig7_5_categorical_profile_distribution",
  valid = is.data.frame(mixed_cat_profile_df) && nrow(mixed_cat_profile_df) > 0,
  issue = if (!(is.data.frame(mixed_cat_profile_df) && nrow(mixed_cat_profile_df) > 0)) {
    "categorical profile data is unavailable"
  } else "",
  stringsAsFactors = FALSE
)
FIG7_5_OBJ_VALIDATION <- validate_figure_object("Fig7_5_categorical_profile_distribution", p7_5)
FIG7_6_DATA_VALIDATION <- data.frame(
  figure_name = "Fig7_6_categorical_profile_distribution_bw",
  valid = is.data.frame(mixed_cat_profile_df) && nrow(mixed_cat_profile_df) > 0,
  issue = if (!(is.data.frame(mixed_cat_profile_df) && nrow(mixed_cat_profile_df) > 0)) {
    "categorical profile data is unavailable"
  } else "",
  stringsAsFactors = FALSE
)
FIG7_6_OBJ_VALIDATION <- validate_figure_object("Fig7_6_categorical_profile_distribution_bw", p7_6)

sp <- get_fig_spec("Fig7_1_profile_line_raw")

if (!is.null(p7_1)) {
  m7_1 <- save_fig(
    plot_obj     = p7_1,
    file_stub    = "Fig7_1_profile_line_raw",
    figure_title = profile_title_raw,
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_1)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_1_profile_line_raw")
} else {
  log_warn("Fig7_1_profile_line_raw skipped.")
}

sp <- get_fig_spec("Fig7_1_profile_line_raw_color")

if (!is.null(p7_1_color)) {
  m7_1_color <- save_fig(
    plot_obj     = p7_1_color,
    file_stub    = "Fig7_1_profile_line_raw_color",
    figure_title = paste0(profile_title_raw, " - color"),
    width        = sp$width,
    height       = sp$height,
    dpi          = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_1_color)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_1_profile_line_raw_color")
} else {
  log_warn("Fig7_1_profile_line_raw_color skipped.")
}

sp <- get_fig_spec("Fig7_2_profile_line_z")

if (!is.null(p7_2)) {
  m7_2 <- save_fig(
    plot_obj     = p7_2,
    file_stub    = "Fig7_2_profile_line_z",
    figure_title = profile_title_z,
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_2)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_2_profile_line_z")
} else {
  log_warn("Fig7_2_profile_line_z skipped.")
}

sp <- get_fig_spec("Fig7_2_profile_line_z_color")

if (!is.null(p7_2_color)) {
  m7_2_color <- save_fig(
    plot_obj     = p7_2_color,
    file_stub    = "Fig7_2_profile_line_z_color",
    figure_title = paste0(profile_title_z, " - color"),
    width        = sp$width,
    height       = sp$height,
    dpi          = 600
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_2_color)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_2_profile_line_z_color")
} else {
  log_warn("Fig7_2_profile_line_z_color skipped.")
}

sp <- get_fig_spec("Fig7_3_profile_line_raw_facet")

if (!is.null(p7_3)) {
  m7_3 <- save_fig(
    plot_obj     = p7_3,
    file_stub    = "Fig7_3_profile_line_raw_facet",
    figure_title = paste0(profile_title_raw, " - faceted"),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_3)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_3_profile_line_raw_facet")
} else {
  log_warn("Fig7_3_profile_line_raw_facet skipped.")
}

sp <- get_fig_spec("Fig7_4_profile_line_z_facet")

if (!is.null(p7_4)) {
  m7_4 <- save_fig(
    plot_obj     = p7_4,
    file_stub    = "Fig7_4_profile_line_z_facet",
    figure_title = paste0(profile_title_z, " - faceted"),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_4)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_4_profile_line_z_facet")
} else {
  log_warn("Fig7_4_profile_line_z_facet skipped.")
}

sp <- get_fig_spec("Fig7_5_categorical_profile_distribution")

if (!is.null(p7_5)) {
  m7_5 <- save_fig(
    plot_obj     = p7_5,
    file_stub    = "Fig7_5_categorical_profile_distribution",
    figure_title = paste0("Categorical indicator distributions by ", latent_group_term_lower()),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_5)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_5_categorical_profile_distribution")
} else if (length(indicators_categorical) > 0) {
  log_warn("Fig7_5_categorical_profile_distribution skipped.")
}

sp <- get_fig_spec("Fig7_6_categorical_profile_distribution_bw")

if (!is.null(p7_6)) {
  m7_6 <- save_fig(
    plot_obj     = p7_6,
    file_stub    = "Fig7_6_categorical_profile_distribution_bw",
    figure_title = paste0("Categorical indicator distributions by ", latent_group_term_lower()),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_6)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_6_categorical_profile_distribution_bw")
} else if (length(indicators_categorical) > 0) {
  log_warn("Fig7_6_categorical_profile_distribution_bw skipped.")
}

if (!is.null(p7_7)) {
  sp <- list(width = 12, height = 10, dpi = 600)
  m7_7 <- save_fig(
    plot_obj     = p7_7,
    file_stub    = "Fig7_7_item_response_probability",
    figure_title = paste0("Item-response probabilities by ", latent_group_term_lower()),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_7)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_7_item_response_probability")
} else {
  if (mixture_mode == "lca") log_warn("Fig7_7_item_response_probability skipped.")
}

if (!is.null(p7_8)) {
  sp <- list(width = 12, height = 10, dpi = 600)
  m7_8 <- save_fig(
    plot_obj     = p7_8,
    file_stub    = "Fig7_8_item_response_probability_value_label",
    figure_title = paste0("Item-response probabilities by ", latent_group_term_lower(), " (value labels)"),
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m7_8)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig7_8_item_response_probability_value_label")
} else {
  if (mixture_mode == "lca") log_warn("Fig7_8_item_response_probability_value_label skipped.")
}


if (!is.null(p7_1)) {
  assign("Fig3_indicator_profile",  p7_1, envir = .GlobalEnv)
  assign("Fig7_1_profile_line_raw", p7_1, envir = .GlobalEnv)
}
if (!is.null(p7_2)) assign("Fig7_2_profile_line_z", p7_2, envir = .GlobalEnv)
if (!is.null(p7_1_color)) assign("Fig7_1_profile_line_raw_color", p7_1_color, envir = .GlobalEnv)
if (!is.null(p7_2_color)) assign("Fig7_2_profile_line_z_color", p7_2_color, envir = .GlobalEnv)
if (!is.null(p7_3)) assign("Fig7_3_profile_line_raw_facet", p7_3, envir = .GlobalEnv)
if (!is.null(p7_4)) assign("Fig7_4_profile_line_z_facet", p7_4, envir = .GlobalEnv)
if (!is.null(p7_5)) assign("Fig7_5_categorical_profile_distribution", p7_5, envir = .GlobalEnv)
if (!is.null(p7_6)) assign("Fig7_6_categorical_profile_distribution_bw", p7_6, envir = .GlobalEnv)

# ------------------------------------------------------------
# 17. Fig8 radar
# ------------------------------------------------------------

# ==========================================================
# Fig8 helpers
# ==========================================================

clean_fig8_pairwise <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(data.frame())
  }

  out <- df

  # -----------------------------
  # 1. Comparison 확보
  # -----------------------------
  if (!"Comparison" %in% names(out)) {
    if (all(c("class_i", "class_j") %in% names(out))) {
      out$Comparison <- paste0(latent_group_label(out$class_i), " vs ", latent_group_label(out$class_j))
    } else if (all(c("class", "ref_class") %in% names(out))) {
      out$Comparison <- paste0(as.character(out$class), " vs ", as.character(out$ref_class))
    } else if (all(c("target_class", "reference_class") %in% names(out))) {
      out$Comparison <- paste0(latent_group_label(out$target_class), " vs ", latent_group_label(out$reference_class))
    }
  }

  if (!"Comparison" %in% names(out)) {
    return(data.frame())
  }

  out$Comparison <- normalize_latent_group_display(trimws(as.character(out$Comparison)))

  # -----------------------------
  # 2. class_i / class_j 추출
  # -----------------------------
  if (!"class_i" %in% names(out) || !"class_j" %in% names(out)) {
    m <- regexec("^(Class|Profile)\\s*([0-9]+)\\s*vs\\s*(Class|Profile)\\s*([0-9]+)$", out$Comparison)
    mm <- regmatches(out$Comparison, m)

    tmp_i <- rep(NA_integer_, length(mm))
    tmp_j <- rep(NA_integer_, length(mm))

    hit <- lengths(mm) >= 5
    if (any(hit)) {
      tmp_i[hit] <- suppressWarnings(as.integer(vapply(mm[hit], `[`, character(1), 3)))
      tmp_j[hit] <- suppressWarnings(as.integer(vapply(mm[hit], `[`, character(1), 5)))
    }

    if (!"class_i" %in% names(out)) out$class_i <- tmp_i
    if (!"class_j" %in% names(out)) out$class_j <- tmp_j
  }

  # -----------------------------
  # 3. 자기 자신 비교 제거
  # -----------------------------
  if (all(c("class_i", "class_j") %in% names(out))) {
    out <- out[is.na(out$class_i) | is.na(out$class_j) | out$class_i != out$class_j, , drop = FALSE]
  } else {
    out <- out[!grepl("^(Class|Profile)\\s*([0-9]+)\\s*vs\\s*(Class|Profile)\\s*\\2$", out$Comparison), , drop = FALSE]
  }

  if (nrow(out) == 0) return(out)

  # -----------------------------
  # 4. 변수명 통일
  # -----------------------------
  if (!"var_name" %in% names(out)) {
    cand_var <- c("outcome", "variable", "var", "name")
    cand_var <- cand_var[cand_var %in% names(out)]
    if (length(cand_var) > 0) {
      out$var_name <- as.character(out[[cand_var[1]]])
    } else {
      out$var_name <- "value"
    }
  }
  out$var_name <- as.character(out$var_name)

  # -----------------------------
  # 5. 값 컬럼 통일
  # -----------------------------
  value_candidates <- c("estimate", "est", "mean_diff", "diff", "value")
  value_col <- value_candidates[value_candidates %in% names(out)][1]

  if (length(value_col) == 0 || is.na(value_col)) {
    out$value <- NA_real_
  } else {
    out$value <- suppressWarnings(as.numeric(out[[value_col]]))
  }

  # -----------------------------
  # 6. p 컬럼 보조 생성
  # -----------------------------
  if ("p" %in% names(out)) {
    out$p_num <- suppressWarnings(as.numeric(out$p))
  } else if ("p_value" %in% names(out)) {
    out$p_num <- suppressWarnings(as.numeric(out$p_value))
  } else {
    out$p_num <- NA_real_
  }

  # -----------------------------
  # 7. 중복 제거
  # 우선순위: p 있는 행 -> p 작은 행 -> |value| 큰 행
  # -----------------------------
  out$abs_value <- abs(out$value)

  ord <- order(
    out$Comparison,
    out$var_name,
    is.na(out$p_num),
    out$p_num,
    -out$abs_value
  )
  out <- out[ord, , drop = FALSE]

  key_dup <- duplicated(out[, c("Comparison", "var_name")])
  out <- out[!key_dup, , drop = FALSE]

  # -----------------------------
  # 8. reshape용 최소 컬럼만 유지
  # -----------------------------
  out <- out[, unique(c(
    intersect(c("Comparison", "var_name", "value", "class_i", "class_j"), names(out))
  )), drop = FALSE]

  rownames(out) <- NULL
  out
}


align_rbind_fill <- function(dflist) {
  if (length(dflist) == 0) return(data.frame())

  keep <- vapply(dflist, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))
  dflist <- dflist[keep]

  if (length(dflist) == 0) return(data.frame())

  all_cols <- unique(unlist(lapply(dflist, names)))

  dflist2 <- lapply(dflist, function(x) {
    miss <- setdiff(all_cols, names(x))
    if (length(miss) > 0) {
      for (nm in miss) x[[nm]] <- NA
    }
    x <- x[, all_cols, drop = FALSE]
    rownames(x) <- NULL
    x
  })

  do.call(rbind, dflist2)
}


make_fig8_radar_wide <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(data.frame())
  }

  x <- clean_fig8_pairwise(df)
  if (nrow(x) == 0) return(data.frame())

  # reshape 전 중복 재검사
  dup_chk <- aggregate(
    rep(1, nrow(x)),
    by = list(Comparison = x$Comparison, var_name = x$var_name),
    FUN = length
  )
  dup_chk <- dup_chk[dup_chk$x > 1, , drop = FALSE]
  if (nrow(dup_chk) > 0) {
    stop("Fig8 reshape blocked: duplicate Comparison-var_name remained.")
  }

  wide <- reshape(
    x[, c("Comparison", "var_name", "value"), drop = FALSE],
    idvar = "Comparison",
    timevar = "var_name",
    direction = "wide"
  )

  rownames(wide) <- NULL
  wide
}

# ==========================================================
# Fig8 radar figures
# ==========================================================
log_info("Building Fig8 radar figures ...")

FIG8_LIST <- list()

# ----------------------------------------------------------
# source 선택
# 우선순위:
# 1) BCH_POSTHOC
# 2) BCH_INTERACTION
# 3) T6C
# ----------------------------------------------------------
radar_src_candidates <- list()

if (exists("BCH_POSTHOC")) {
  radar_src_candidates[["BCH_POSTHOC"]] <- BCH_POSTHOC
}
if (exists("BCH_INTERACTION")) {
  radar_src_candidates[["BCH_INTERACTION"]] <- BCH_INTERACTION
}
if (exists("T6C")) {
  radar_src_candidates[["T6C"]] <- T6C
}

for (src_name in names(radar_src_candidates)) {
  src <- radar_src_candidates[[src_name]]

  if (!is.data.frame(src) || nrow(src) == 0) {
    log_warn(paste0("Fig8 source skipped: ", src_name, " is empty."))
    next
  }

  cleaned <- clean_fig8_pairwise(src)

  if (!is.data.frame(cleaned) || nrow(cleaned) == 0) {
    log_warn(paste0("Fig8 source skipped after cleaning: ", src_name))
    next
  }

  radar_wide <- tryCatch(
    make_fig8_radar_wide(cleaned),
    error = function(e) {
      log_warn(paste0("Fig8 reshape skipped for ", src_name, ": ", e$message))
      data.frame()
    }
  )

  if (!is.data.frame(radar_wide) || nrow(radar_wide) == 0) {
    next
  }

  radar_wide$source_name <- src_name
  FIG8_LIST[[src_name]] <- radar_wide
}

FIG8_DATA <- align_rbind_fill(FIG8_LIST)

if (!is.data.frame(FIG8_DATA) || nrow(FIG8_DATA) == 0) {
  log_warn("Fig8_radar skipped: no valid radar data after cleaning / reshape.")

  FIG8_DATA_VALIDATION <- data.frame(
    figure_name      = "Fig8_radar",
    valid            = FALSE,
    issue            = "no valid radar data after cleaning",
    stringsAsFactors = FALSE
  )

  FIG8_OBJ_VALIDATION <- data.frame(
    figure_name      = "Fig8_radar",
    valid            = TRUE,
    issue            = "skipped",
    stringsAsFactors = FALSE
  )

} else {

  # ----------------------------------------
  # value.* 컬럼 찾기
  # ----------------------------------------
  radar_value_cols <- grep("^value\\.", names(FIG8_DATA), value = TRUE)

  if (length(radar_value_cols) < 3) {
    log_warn("Fig8_radar skipped: fewer than 3 radar axes available.")

    FIG8_DATA_VALIDATION <- data.frame(
      figure_name      = "Fig8_radar",
      valid            = FALSE,
      issue            = "fewer than 3 radar axes available",
      stringsAsFactors = FALSE
    )

    FIG8_OBJ_VALIDATION <- data.frame(
      figure_name      = "Fig8_radar",
      valid            = TRUE,
      issue            = "skipped",
      stringsAsFactors = FALSE
    )

  } else {

    # 긴 형식으로 변환
    fig8_long <- data.frame(
      Comparison = rep(FIG8_DATA$Comparison, each = length(radar_value_cols)),
      Axis       = rep(sub("^value\\.", "", radar_value_cols), times = nrow(FIG8_DATA)),
      Value      = as.numeric(unlist(FIG8_DATA[, radar_value_cols, drop = FALSE])),
      source_name = rep(FIG8_DATA$source_name, each = length(radar_value_cols)),
      stringsAsFactors = FALSE
    )

    fig8_long <- fig8_long[!is.na(fig8_long$Value), , drop = FALSE]

    if (nrow(fig8_long) == 0) {
      log_warn("Fig8_radar skipped: all radar values are NA.")

      FIG8_DATA_VALIDATION <- data.frame(
        figure_name      = "Fig8_radar",
        valid            = FALSE,
        issue            = "all radar values are NA",
        stringsAsFactors = FALSE
      )

      FIG8_OBJ_VALIDATION <- data.frame(
        figure_name      = "Fig8_radar",
        valid            = TRUE,
        issue            = "skipped",
        stringsAsFactors = FALSE
      )

    } else {

      FIG8_DATA_VALIDATION <- data.frame(
        figure_name      = "Fig8_radar",
        valid            = TRUE,
        issue            = "",
        stringsAsFactors = FALSE
      )

      # 축 순서: display_order가 있으면 그 순서 사용
      if (exists("DICT") && is.list(DICT) && "meta" %in% names(DICT) &&
          is.data.frame(DICT$meta) && nrow(DICT$meta) > 0 &&
          "var_name" %in% names(DICT$meta)) {

        meta0 <- DICT$meta

        axis_order <- meta0$var_name
        if ("display_order" %in% names(meta0)) {
          oo <- order(meta0$display_order, na.last = TRUE)
          axis_order <- meta0$var_name[oo]
        }

        axis_order <- unique(as.character(axis_order))
        axis_order <- intersect(axis_order, unique(fig8_long$Axis))

        if (length(axis_order) > 0) {
          fig8_long$Axis <- factor(fig8_long$Axis, levels = axis_order)
        }
      }

      # --------------------------------------------------
      # 저장용 객체
      # --------------------------------------------------
      Fig8_radar_data <- fig8_long
      saveRDS(Fig8_radar_data, file.path(DIR_RDS, "Fig8_radar_data.rds"))

      # --------------------------------------------------
      # 실제 그림
      # ggplot2가 이미 로드되어 있다는 전제
      # --------------------------------------------------
      p_fig8 <- ggplot(fig8_long, aes(x = Axis, y = Value, group = Comparison, color = Comparison)) +
        geom_polygon(fill = NA, linewidth = 0.5, show.legend = TRUE) +
        geom_line(linewidth = 0.7, show.legend = TRUE) +
        geom_point(size = 1.8, show.legend = FALSE) +
        coord_polar() +
        facet_wrap(~source_name) +
        labs(
          title = paste0("Fig8. Radar comparison across pairwise ", latent_group_term_lower(), " contrasts"),
          x = NULL,
          y = NULL,
          color = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 9),
          legend.position = "bottom"
        )

      ggsave(
        filename = file.path(DIR_FIGURES_PNG, "Fig8_radar.png"),
        plot = p_fig8,
        width = 10,
        height = 8,
        dpi = 600
      )

      assign("Fig8_radar", p_fig8, envir = .GlobalEnv)
      log_info("Fig8_radar saved.")
    }
  }
}

# ------------------------------------------------------------
# 18. Fig9 profile line with CI
# ------------------------------------------------------------
log_info("Building Fig9_profile_line_ci ...")

p9 <- NULL
if (nrow(t3_raw) > 0 && "se" %in% names(t3_raw)) {
  p9 <- tryCatch(
    build_profile_line_ci_plot(
      df = t3_raw,
      title = if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
        paste0("Indicator response by ", latent_group_term_lower(), " with 95% CI")
      } else {
        paste0("Indicator profile by ", latent_group_term_lower(), " with 95% CI")
      }
    ),
    error = function(e) {
      log_warn("Fig9_profile_line_ci build failed: ", conditionMessage(e))
      NULL
    }
  )
}

FIG9_DATA_VALIDATION <- data.frame(
  figure_name = "Fig9_profile_line_ci",
  valid = is.data.frame(t3_raw) && nrow(t3_raw) > 0 && "se" %in% names(t3_raw) && any(!is.na(t3_raw$se)),
  issue = if (!(is.data.frame(t3_raw) && nrow(t3_raw) > 0 && "se" %in% names(t3_raw) && any(!is.na(t3_raw$se)))) {
    paste0("raw ", latent_group_term_lower(), " data with se is unavailable")
  } else "",
  stringsAsFactors = FALSE
)

FIG9_OBJ_VALIDATION <- validate_figure_object("Fig9_profile_line_ci", p9)

sp <- get_fig_spec("Fig9_profile_line_ci")

if (!is.null(p9)) {
  m9 <- save_fig(
    plot_obj     = p9,
    file_stub    = "Fig9_profile_line_ci",
    figure_title = if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
      paste0("Indicator response by ", latent_group_term_lower(), " with 95% CI")
    } else {
      paste0("Indicator profile by ", latent_group_term_lower(), " with 95% CI")
    },
    width        = sp$width,
    height       = sp$height,
    dpi          = sp$dpi
  )
  FIGURE_MANIFEST <- append_figure_manifest(FIGURE_MANIFEST, m9)
  FIGURE_REGISTRY <- register_figure_paths(FIGURE_REGISTRY, "Fig9_profile_line_ci")
} else {
  log_warn("Fig9_profile_line_ci skipped.")
}

# ------------------------------------------------------------
# 19. validation summary
# ------------------------------------------------------------
FIGURE_VALIDATION <- bind_figure_validation(
  FIG1_DATA_VALIDATION, FIG1_OBJ_VALIDATION,
  FIG2_DATA_VALIDATION, FIG2_OBJ_VALIDATION,
  FIG3_ENTROPY_DATA_VALIDATION, FIG3_ENTROPY_OBJ_VALIDATION,
  FIG3_QUALITY_DATA_VALIDATION, FIG3_QUALITY_OBJ_VALIDATION,
  FIG4_1_DATA_VALIDATION, FIG4_1_OBJ_VALIDATION,
  FIG4_2_DATA_VALIDATION, FIG4_2_OBJ_VALIDATION,
  FIG5_DATA_VALIDATION, FIG5_OBJ_VALIDATION,
  FIG5_2_DATA_VALIDATION, FIG5_2_OBJ_VALIDATION,
  FIG6_2_DATA_VALIDATION, FIG6_2_OBJ_VALIDATION,
  FIG6_3_DATA_VALIDATION, FIG6_3_OBJ_VALIDATION,
  FIG7_1_DATA_VALIDATION, FIG7_1_OBJ_VALIDATION,
  FIG7_2_DATA_VALIDATION, FIG7_2_OBJ_VALIDATION,
  FIG7_3_DATA_VALIDATION, FIG7_3_OBJ_VALIDATION,
  FIG7_4_DATA_VALIDATION, FIG7_4_OBJ_VALIDATION,
  FIG7_5_DATA_VALIDATION, FIG7_5_OBJ_VALIDATION,
  FIG7_6_DATA_VALIDATION, FIG7_6_OBJ_VALIDATION,
  FIG8_DATA_VALIDATION, FIG8_OBJ_VALIDATION,
  FIG9_DATA_VALIDATION, FIG9_OBJ_VALIDATION
)

if (is.data.frame(FIGURE_VALIDATION) && nrow(FIGURE_VALIDATION) > 0) {
  bad_fig <- FIGURE_VALIDATION[!FIGURE_VALIDATION$valid, , drop = FALSE]

  if (nrow(bad_fig) > 0) {
    for (i in seq_len(nrow(bad_fig))) {
      issue_i <- as.character(bad_fig$issue[i] %||% "")
      if (grepl("skip|skipped|not available|empty|unavailable", issue_i, ignore.case = TRUE)) {
        log_info("[FIGURE_VALIDATION] ", bad_fig$figure_name[i], " / ", issue_i)
      } else {
        log_warn("[FIGURE_VALIDATION] ", bad_fig$figure_name[i], " / ", issue_i)
      }
    }
  } else {
    log_info("FIGURE_VALIDATION: all figures passed structure checks.")
  }
}

# ------------------------------------------------------------
# 20. checklist
# ------------------------------------------------------------
expected_figures <- c(
  "Fig1_model_fit",
  "Fig2_class_proportion",
  "Fig3_entropy",
  "Fig3_classification_quality",
  "Fig4_1_indicator_heatmap_raw",
  "Fig4_2_indicator_heatmap_z",
  "Fig5_posterior_max",
  "Fig5_2_app_occ_by_profile",
  "Fig6_1_r3step_forest",
  "Fig6_2_bch_interaction",
  "Fig6_3_moderator_profile_means",
  "Fig7_1_profile_line_raw",
  "Fig7_2_profile_line_z",
  "Fig7_3_profile_line_raw_facet",
  "Fig7_4_profile_line_z_facet",
  "Fig7_5_categorical_profile_distribution",
  "Fig7_6_categorical_profile_distribution_bw",
  "Fig7_7_item_response_probability",
  "Fig7_8_item_response_probability_value_label",
  "Fig8_1_radar_filled",
  "Fig8_2_radar_outline"
  # "Fig9_profile_line_ci"
)

FIGURE_CHECKLIST <- data.frame(
  figure_name = expected_figures,
  has_any = FALSE,
  stringsAsFactors = FALSE
)

reg_names <- names(FIGURE_REGISTRY)
if (length(reg_names) > 0) {
  for (i in seq_len(nrow(FIGURE_CHECKLIST))) {
    FIGURE_CHECKLIST$has_any[i] <- any(startsWith(reg_names, FIGURE_CHECKLIST$figure_name[i]))
  }
}

# ------------------------------------------------------------
# 21. add metadata
# ------------------------------------------------------------
if (is.data.frame(FIGURE_MANIFEST) && nrow(FIGURE_MANIFEST) > 0) {
  if ("figure_title" %in% names(FIGURE_MANIFEST)) {
    FIGURE_MANIFEST$figure_title <- normalize_latent_group_display(FIGURE_MANIFEST$figure_title)
  }
  FIGURE_MANIFEST$dataset_id <- DATASET_ID
  FIGURE_MANIFEST$analysis_id <- ANALYSIS_ID
  FIGURE_MANIFEST$mixture_mode <- mixture_mode
  FIGURE_MANIFEST$best_k <- best_k
  FIGURE_MANIFEST$best_tag <- best_tag
  FIGURE_MANIFEST$model_structure <- model_structure
  rownames(FIGURE_MANIFEST) <- NULL
}

if (is.data.frame(FIGURE_VALIDATION) && nrow(FIGURE_VALIDATION) > 0 && "issue" %in% names(FIGURE_VALIDATION)) {
  FIGURE_VALIDATION$issue <- normalize_latent_group_display(FIGURE_VALIDATION$issue)
}

FIGURE_SUMMARY <- list(
  mixture_mode    = mixture_mode,
  best_k          = best_k,
  best_tag        = best_tag,
  model_structure = model_structure,
  n_figures       = if (is.data.frame(FIGURE_MANIFEST)) nrow(FIGURE_MANIFEST) else 0L,
  n_registry      = length(FIGURE_REGISTRY),
  created_at      = Sys.time()
)

# ------------------------------------------------------------
# 22. save outputs
# ------------------------------------------------------------
log_info("Saving figure outputs ...")

write_csv_safe(
  if (is.data.frame(FIGURE_MANIFEST)) FIGURE_MANIFEST else data.frame(),
  PATH_FIGURE_MANIFEST_CSV
)

write_csv_safe(
  data.frame(
    metric = c("mixture_mode", "best_k", "best_tag", "model_structure", "n_figures", "n_registry", "created_at"),
    value = c(
      as.character(FIGURE_SUMMARY$mixture_mode %||% NA),
      as.character(FIGURE_SUMMARY$best_k %||% NA),
      as.character(FIGURE_SUMMARY$best_tag %||% NA),
      as.character(FIGURE_SUMMARY$model_structure %||% NA),
      as.character(FIGURE_SUMMARY$n_figures %||% 0L),
      as.character(FIGURE_SUMMARY$n_registry %||% 0L),
      as.character(FIGURE_SUMMARY$created_at %||% Sys.time())
    ),
    stringsAsFactors = FALSE
  ),
  PATH_FIGURE_SUMMARY_CSV
)

write_csv_safe(
  if (is.data.frame(FIGURE_VALIDATION)) FIGURE_VALIDATION else data.frame(),
  PATH_FIGURE_VALIDATION_CSV
)

write_csv_safe(
  if (is.data.frame(FIGURE_CHECKLIST)) FIGURE_CHECKLIST else data.frame(),
  PATH_FIGURE_CHECKLIST_CSV
)

save_rds_safe(FIGURE_MANIFEST, PATH_FIGURE_MANIFEST_RDS)
save_step_rds(FIGURE_SUMMARY, "FIGURE_SUMMARY", dir_rds = DIR_RDS)

save_named_rds_list(
  list(
    FIGURE_REGISTRY   = FIGURE_REGISTRY,
    FIGURE_MANIFEST   = FIGURE_MANIFEST,
    FIGURE_SUMMARY    = FIGURE_SUMMARY,
    FIGURE_VALIDATION = FIGURE_VALIDATION,
    FIGURE_CHECKLIST  = FIGURE_CHECKLIST
  ),
  dir_rds = DIR_RDS
)

# ------------------------------------------------------------
# 23. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_FIGURES, units = "secs")), 2)

log_info("07_figures.R completed.")
log_info("mixture_mode    = ", mixture_mode)
log_info("best_k          = ", best_k)
log_info("best_tag        = ", best_tag)
log_info("model_structure = ", model_structure)
log_info("n_figures       = ", if (is.data.frame(FIGURE_MANIFEST)) nrow(FIGURE_MANIFEST) else 0L)
log_info("n_registry      = ", length(FIGURE_REGISTRY))
log_info("elapsed         = ", elapsed_sec, " sec")

log_step_end("figures", elapsed_sec, ok = TRUE)
