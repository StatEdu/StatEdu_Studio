# ============================================================
# 05_dict.R
# Dictionary helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) data_dictionary.csv 표준화
# 2) 변수 role / subtype / label / order 정리
# 3) 범주형 변수 level / label / reference 정보 정리
# 4) 분석 단계에서 바로 쓸 수 있는 DICT 객체 생성
#
# DICT 구조
# - DICT$meta   : 변수 단위 메타정보
# - DICT$levels : 범주형 level 정보
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

as_chr_na2 <- function(x) {
  x <- as.character(x)
  x[x %in% c("", " ", "NA", "NaN", "NULL", "null")] <- NA_character_
  x
}

trim_ws2 <- function(x) {
  x <- as.character(x)
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

norm_role <- function(x) {
  x <- tolower(trim_ws2(as.character(x)))
  x[x %in% c("indicator", "ind")] <- "indicator"
  x[x %in% c("covariate", "cov", "predictor")] <- "covariate"
  x[x %in% c("outcome", "distal")] <- "outcome"
  x[x %in% c("weight", "sampling_weight")] <- "weight"
  x[x %in% c("strata", "stratum")] <- "strata"
  x[x %in% c("cluster", "psu")] <- "cluster"
  x[x %in% c("id", "caseid", "case_id")] <- "id"
  x[is.na(x) | !nzchar(x)] <- "none"
  x
}

norm_subtype <- function(x) {
  x <- tolower(trim_ws2(as.character(x)))
  x[x %in% c("cont", "continuous", "scale", "numeric")] <- "continuous"
  x[x %in% c("cat", "categorical", "binary", "ordinal", "nominal")] <- "categorical"
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

norm_type <- function(x) {
  x <- tolower(trim_ws2(as.character(x)))
  x[x %in% c("continuous", "cont", "scale")] <- "continuous"
  x[x %in% c("categorical", "cat", "binary", "ordinal", "nominal")] <- "categorical"
  x[x %in% c("num", "numeric", "double", "integer")] <- "numeric"
  x[x %in% c("char", "character", "string")] <- "character"
  x[x %in% c("factor")] <- "factor"
  x[x %in% c("logical", "bool", "boolean")] <- "logical"
  x[is.na(x) | !nzchar(x)] <- "auto"
  x
}

coalesce_chr2 <- function(...) {
  xs <- list(...)
  if (length(xs) == 0) return(character(0))
  out <- as.character(xs[[1]])
  for (i in seq_along(xs)) {
    y <- as.character(xs[[i]])
    if (length(out) == 0) {
      out <- y
    } else {
      if (length(y) == 1L && length(out) > 1L) y <- rep(y, length(out))
      idx <- is.na(out) | !nzchar(out)
      out[idx] <- y[idx]
    }
  }
  out
}

guess_var_type_from_data <- function(x) {
  if (is.factor(x)) return("factor")
  if (is.character(x)) return("character")
  if (is.logical(x)) return("logical")
  if (is.integer(x) || is.numeric(x)) return("numeric")
  "auto"
}

guess_subtype_from_data <- function(x, max_categorical_unique = 10) {
  if (is.factor(x) || is.character(x)) return("categorical")
  if (is.logical(x)) return("categorical")

  if (is.numeric(x) || is.integer(x)) {
    ux <- unique(stats::na.omit(x))
    if (length(ux) <= max_categorical_unique && all(abs(ux - round(ux)) < 1e-8)) {
      return("categorical")
    }
    return("continuous")
  }

  NA_character_
}

# ------------------------------------------------------------
# 1. standardize raw dictionary
# ------------------------------------------------------------
standardize_dictionary <- function(dict_raw) {
  if (is.null(dict_raw) || !is.data.frame(dict_raw) || nrow(dict_raw) == 0) {
    return(list(
      meta = data.frame(),
      levels = data.frame()
    ))
  }

  d <- as.data.frame(dict_raw, stringsAsFactors = FALSE)
  names(d) <- if (exists("make_clean_names")) make_clean_names(names(d)) else make.names(names(d), unique = TRUE)

  # var_name
  if (!"var_name" %in% names(d)) {
    cand <- intersect(c("variable", "var", "name"), names(d))
    if (length(cand) > 0) names(d)[match(cand[1], names(d))] <- "var_name"
  }
  if (!"var_name" %in% names(d)) {
    stop("Dictionary must contain var_name (or equivalent column).", call. = FALSE)
  }

  # label columns
  if (!"var_label" %in% names(d)) d$var_label <- NA_character_
  if (!"label_ko" %in% names(d)) d$label_ko <- NA_character_
  if (!"label_en" %in% names(d)) d$label_en <- NA_character_

  d$var_name  <- trim_ws2(as.character(d$var_name))
  d$var_label <- coalesce_chr2(d$var_label, d$label_ko, d$label_en, d$var_name)

  # roles / type / subtype
  if (!"analysis_role" %in% names(d)) {
    if ("role" %in% names(d)) {
      d$analysis_role <- d$role
    } else {
      d$analysis_role <- "none"
    }
  }
  d$analysis_role <- norm_role(d$analysis_role)

  if (!"subtype" %in% names(d)) {
    if ("indicator_subtype" %in% names(d)) {
      d$subtype <- d$indicator_subtype
    } else {
      d$subtype <- NA_character_
    }
  }
  d$subtype <- norm_subtype(d$subtype)

  if (!"type" %in% names(d)) d$type <- "auto"
  d$type <- norm_type(d$type)

  # Explicit dictionary type must dominate downstream heuristics.
  is_type_cont <- !is.na(d$type) & d$type %in% c("continuous", "numeric")
  is_type_cat  <- !is.na(d$type) & d$type %in% c("categorical", "factor")
  d$subtype[is_type_cont] <- "continuous"
  d$subtype[is_type_cat]  <- "categorical"

  # display/order columns
  if (!"display_order" %in% names(d)) d$display_order <- seq_len(nrow(d))
  if (!"display_group" %in% names(d)) d$display_group <- NA_character_

  # reference / level info
  if (!"reference" %in% names(d)) {
    if ("ref" %in% names(d)) {
      d$reference <- d$ref
    } else if ("reference_level" %in% names(d)) {
      d$reference <- d$reference_level
    } else {
      d$reference <- NA_character_
    }
  }

  if (!"value" %in% names(d)) d$value <- NA_character_
  if (!"value_label" %in% names(d)) {
    if ("category_label" %in% names(d)) {
      d$value_label <- d$category_label
    } else if ("level_label" %in% names(d)) {
      d$value_label <- d$level_label
    } else {
      d$value_label <- NA_character_
    }
  }

  # optional flags
  if (!"use_var" %in% names(d)) {
    d$use_var <- if ("use" %in% names(d)) d$use else TRUE
  }
  d$use_var <- ifelse(is.na(d$use_var), TRUE, as.logical(d$use_var))

  # split meta vs levels
  meta_cols <- c(
    "var_name", "var_label", "label_ko", "label_en",
    "analysis_role", "type", "subtype",
    "display_order", "display_group",
    "reference", "use_var", "description"
  )
  meta_cols <- intersect(meta_cols, names(d))

  meta <- unique(d[, meta_cols, drop = FALSE])

  # level info only when value exists
  level_cols <- c("var_name", "value", "value_label", "reference")
  level_cols <- intersect(level_cols, names(d))

  # 1) long format levels 먼저 처리
  levels_long <- if (length(level_cols) > 0) d[, level_cols, drop = FALSE] else data.frame()

  if (nrow(levels_long) > 0 && "value" %in% names(levels_long)) {
    levels_long$value <- as_chr_na2(levels_long$value)
    levels_long <- levels_long[!is.na(levels_long$value), , drop = FALSE]
  } else {
    levels_long <- data.frame()
  }

  # 2) wide format(value_1/label_1 ...)를 long으로 펼치기
  value_cols <- grep("^value_[0-9]+$", names(d), value = TRUE)
  label_cols <- grep("^label_[0-9]+$", names(d), value = TRUE)

  pair_ids <- intersect(
    sub("^value_", "", value_cols),
    sub("^label_", "", label_cols)
  )

  levels_wide_list <- list()
  idx_lv <- 1L

  if (length(pair_ids) > 0) {
    for (i in seq_len(nrow(d))) {
      vname <- as.character(d$var_name[i])
      ref_i <- if ("reference" %in% names(d)) as.character(d$reference[i]) else NA_character_

      for (pid in pair_ids) {
        vcol <- paste0("value_", pid)
        lcol <- paste0("label_", pid)

        vv <- if (vcol %in% names(d)) as_chr_na2(d[[vcol]][i]) else NA_character_
        ll <- if (lcol %in% names(d)) as_chr_na2(d[[lcol]][i]) else NA_character_

        if (is.na(vv) || !nzchar(vv)) next
        if (is.na(ll) || !nzchar(ll)) ll <- vv

        levels_wide_list[[idx_lv]] <- data.frame(
          var_name = vname,
          value = vv,
          value_label = ll,
          reference = ref_i,
          stringsAsFactors = FALSE
        )
        idx_lv <- idx_lv + 1L
      }
    }
  }

  levels_wide <- if (length(levels_wide_list) > 0) {
    do.call(rbind, levels_wide_list)
  } else {
    data.frame()
  }

  # 3) 합치기: wide/long 모두 지원
  levels <- safe_rbind(levels_long, levels_wide)

  if (nrow(levels) > 0) {
    if (!"value_label" %in% names(levels)) levels$value_label <- levels$value
    levels$value <- as_chr_na2(levels$value)
    levels$value_label <- coalesce_chr2(levels$value_label, levels$value)

    levels <- levels[!is.na(levels$value), , drop = FALSE]
    levels <- levels[!duplicated(levels[, intersect(c("var_name", "value"), names(levels)), drop = FALSE]), , drop = FALSE]
    rownames(levels) <- NULL
  }

  if (nrow(meta) > 0) {
    meta <- meta[!duplicated(meta$var_name), , drop = FALSE]
    rownames(meta) <- NULL
  }

  if (nrow(levels) > 0) {
    if (!"value_label" %in% names(levels)) levels$value_label <- levels$value
    levels$value_label <- coalesce_chr2(levels$value_label, levels$value)
    levels <- levels[!duplicated(levels[, intersect(c("var_name", "value"), names(levels)), drop = FALSE]), , drop = FALSE]
    rownames(levels) <- NULL
  }

  list(meta = meta, levels = levels)
}

# ------------------------------------------------------------
# 2. enrich dictionary using raw data
# ------------------------------------------------------------
enrich_dict_from_data <- function(dict, raw_data, max_categorical_unique = 10) {
  if (is.null(dict)) dict <- list(meta = data.frame(), levels = data.frame())
  meta <- dict$meta %||% data.frame()
  levels <- dict$levels %||% data.frame()

  if (!is.data.frame(raw_data) || ncol(raw_data) == 0) {
    return(list(meta = meta, levels = levels))
  }

  # add missing vars from data
  data_vars <- names(raw_data)
  missing_vars <- setdiff(data_vars, meta$var_name %||% character(0))

  if (length(missing_vars) > 0) {
    add_meta <- data.frame(
      var_name = missing_vars,
      var_label = missing_vars,
      label_ko = NA_character_,
      label_en = NA_character_,
      analysis_role = "none",
      type = vapply(raw_data[missing_vars], guess_var_type_from_data, character(1)),
      subtype = vapply(raw_data[missing_vars], guess_subtype_from_data, character(1), max_categorical_unique = max_categorical_unique),
      display_order = seq.int(from = nrow(meta) + 1L, length.out = length(missing_vars)),
      display_group = NA_character_,
      reference = NA_character_,
      use_var = TRUE,
      stringsAsFactors = FALSE
    )
    meta <- safe_rbind(meta, add_meta)
  }

  # fill blank fields using data
  if (nrow(meta) > 0) {
    for (i in seq_len(nrow(meta))) {
      v <- meta$var_name[i]
      if (!v %in% names(raw_data)) next

      if (is.na(meta$type[i]) || meta$type[i] == "auto" || !nzchar(meta$type[i])) {
        meta$type[i] <- guess_var_type_from_data(raw_data[[v]])
      }
      if (is.na(meta$subtype[i]) || !nzchar(meta$subtype[i])) {
        meta$subtype[i] <- guess_subtype_from_data(raw_data[[v]], max_categorical_unique = max_categorical_unique)
      }
      if (!is.na(meta$type[i]) && nzchar(meta$type[i])) {
        if (meta$type[i] %in% c("continuous", "numeric")) meta$subtype[i] <- "continuous"
        if (meta$type[i] %in% c("categorical", "factor")) meta$subtype[i] <- "categorical"
      }
      if (is.na(meta$var_label[i]) || !nzchar(meta$var_label[i])) {
        meta$var_label[i] <- v
      }
    }
  }

  # resolve categorical levels from data when not provided
  data_level_list <- list()
  idx <- 1L

  for (v in meta$var_name %||% character(0)) {
    if (!v %in% names(raw_data)) next

    is_cat <- identical(meta$subtype[match(v, meta$var_name)], "categorical") ||
      is.factor(raw_data[[v]]) || is.character(raw_data[[v]]) || is.logical(raw_data[[v]])

    if (!is_cat) next

    ux <- unique(stats::na.omit(raw_data[[v]]))
    if (length(ux) == 0) next

    ux_chr <- as.character(ux)

    tmp <- data.frame(
      var_name = v,
      value = ux_chr,
      value_label = vapply(
        ux_chr,
        function(val) {
          hit <- levels[
            as.character(levels$var_name) == as.character(v) &
              as.character(levels$value) == as.character(val),
            ,
            drop = FALSE
          ]

          if (nrow(hit) > 0 && "value_label" %in% names(hit)) {
            lab <- hit$value_label[1]
            if (!is.na(lab) && nzchar(as.character(lab))) {
              return(as.character(lab))
            }
          }

          as.character(val)
        },
        character(1)
      ),
      reference = NA_character_,
      stringsAsFactors = FALSE
    )

    data_level_list[[idx]] <- tmp
    idx <- idx + 1L
  }

  data_levels <- if (length(data_level_list) == 0) data.frame() else do.call(rbind, data_level_list)

  levels <- safe_rbind(levels, data_levels)
  if (nrow(levels) > 0) {
    levels <- levels[!duplicated(levels[, intersect(c("var_name", "value"), names(levels)), drop = FALSE]), , drop = FALSE]
    rownames(levels) <- NULL
  }

  list(meta = meta, levels = levels)
}

# ------------------------------------------------------------
# 3. build final DICT object
# ------------------------------------------------------------
build_dict <- function(dict_raw, raw_data = NULL, cfg = NULL, max_categorical_unique = 10) {
  out <- standardize_dictionary(dict_raw)
  out <- enrich_dict_from_data(out, raw_data = raw_data, max_categorical_unique = max_categorical_unique)

  meta <- out$meta %||% data.frame()
  levels <- out$levels %||% data.frame()

  # apply CFG-based role overrides if available
  if (!is.null(cfg) && is.list(cfg) && nrow(meta) > 0) {
    indicators <- cfg$analysis$indicators %||% cfg$indicators %||% character(0)
    covariates <- cfg$analysis$covariates %||% cfg$covariates %||% character(0)
    outcomes   <- cfg$analysis$outcomes %||% cfg$outcomes %||% character(0)

    weight_var <- cfg$survey_design$weight_var %||% cfg$weight_var %||% NULL
    strata_var <- cfg$survey_design$strata_var %||% cfg$strata_var %||% NULL
    cluster_var <- cfg$survey_design$cluster_var %||% cfg$cluster_var %||% NULL
    id_var <- cfg$survey_design$id_var %||% cfg$id_var %||% "id"

    meta$analysis_role[meta$var_name %in% indicators] <- "indicator"
    meta$analysis_role[meta$var_name %in% covariates] <- "covariate"
    meta$analysis_role[meta$var_name %in% outcomes]   <- "outcome"
    meta$analysis_role[meta$var_name %in% weight_var] <- "weight"
    meta$analysis_role[meta$var_name %in% strata_var] <- "strata"
    meta$analysis_role[meta$var_name %in% cluster_var] <- "cluster"
    meta$analysis_role[meta$var_name %in% id_var] <- "id"
  }

  # final cleanup
  meta$analysis_role <- norm_role(meta$analysis_role)
  meta$type <- norm_type(meta$type)
  meta$subtype <- norm_subtype(meta$subtype)
  is_meta_type_cont <- !is.na(meta$type) & meta$type %in% c("continuous", "numeric")
  is_meta_type_cat  <- !is.na(meta$type) & meta$type %in% c("categorical", "factor")
  meta$subtype[is_meta_type_cont] <- "continuous"
  meta$subtype[is_meta_type_cat]  <- "categorical"
  meta$var_label <- coalesce_chr2(meta$var_label, meta$label_ko, meta$label_en, meta$var_name)

  if (!"display_order" %in% names(meta)) {
    meta$display_order <- seq_len(nrow(meta))
  }
  meta$display_order <- suppressWarnings(as.numeric(meta$display_order))
  na_idx <- is.na(meta$display_order)
  if (any(na_idx)) {
    meta$display_order[na_idx] <- seq.int(from = max(meta$display_order, na.rm = TRUE) + 1, length.out = sum(na_idx))
  }

  meta <- meta[order(meta$display_order, meta$var_name), , drop = FALSE]
  rownames(meta) <- NULL

  if (nrow(levels) > 0) {
    if (!"value_label" %in% names(levels)) levels$value_label <- levels$value
    levels$value_label <- coalesce_chr2(levels$value_label, levels$value)
    levels <- levels[order(levels$var_name, levels$value), , drop = FALSE]
    rownames(levels) <- NULL
  }

  list(
    meta = meta,
    levels = levels
  )
}

# ------------------------------------------------------------
# 4. extract analysis variable groups
# ------------------------------------------------------------
extract_analysis_vars <- function(dict, use_only_flagged = TRUE) {
  meta <- dict$meta %||% data.frame()

  if (!is.data.frame(meta) || nrow(meta) == 0) {
    return(list(
      indicators = character(0),
      covariates = character(0),
      outcomes = character(0),
      weight_var = NULL,
      strata_var = NULL,
      cluster_var = NULL,
      id_var = NULL
    ))
  }

  if (isTRUE(use_only_flagged) && "use_var" %in% names(meta)) {
    meta <- meta[is.na(meta$use_var) | meta$use_var, , drop = FALSE]
  }

  list(
    indicators = meta$var_name[meta$analysis_role == "indicator"],
    covariates = meta$var_name[meta$analysis_role == "covariate"],
    outcomes = meta$var_name[meta$analysis_role == "outcome"],
    weight_var = first_of(meta$var_name[meta$analysis_role == "weight"], default = NULL),
    strata_var = first_of(meta$var_name[meta$analysis_role == "strata"], default = NULL),
    cluster_var = first_of(meta$var_name[meta$analysis_role == "cluster"], default = NULL),
    id_var = first_of(meta$var_name[meta$analysis_role == "id"], default = NULL)
  )
}

# ------------------------------------------------------------
# 5. split indicators by subtype
# ------------------------------------------------------------
split_indicator_subtypes <- function(dict, raw_data = NULL, max_categorical_unique = 10) {
  meta <- dict$meta %||% data.frame()
  if (!is.data.frame(meta) || nrow(meta) == 0) {
    return(list(
      indicators = character(0),
      indicators_continuous = character(0),
      indicators_categorical = character(0)
    ))
  }

  ind_meta <- meta[meta$analysis_role == "indicator", , drop = FALSE]
  if (nrow(ind_meta) == 0) {
    return(list(
      indicators = character(0),
      indicators_continuous = character(0),
      indicators_categorical = character(0)
    ))
  }

  # subtype 비어 있으면 raw_data로 보정
  if (!is.null(raw_data) && is.data.frame(raw_data)) {
    for (i in seq_len(nrow(ind_meta))) {
      v <- ind_meta$var_name[i]
      if ((is.na(ind_meta$subtype[i]) || !nzchar(ind_meta$subtype[i])) && v %in% names(raw_data)) {
        ind_meta$subtype[i] <- guess_subtype_from_data(raw_data[[v]], max_categorical_unique = max_categorical_unique)
      }
    }
  }

  list(
    indicators = ind_meta$var_name,
    indicators_continuous = ind_meta$var_name[ind_meta$subtype == "continuous"],
    indicators_categorical = ind_meta$var_name[ind_meta$subtype == "categorical"]
  )
}

# ------------------------------------------------------------
# 6. resolve levels / labels / references
# ------------------------------------------------------------
resolve_var_levels <- function(dict, var_name) {
  levels <- dict$levels %||% data.frame()
  if (!is.data.frame(levels) || nrow(levels) == 0) return(data.frame())
  out <- levels[levels$var_name == var_name, , drop = FALSE]
  rownames(out) <- NULL
  out
}

resolve_reference_levels <- function(dict, var_name) {
  lv <- resolve_var_levels(dict, var_name)
  if (nrow(lv) == 0) return(NULL)

  if ("reference" %in% names(lv)) {
    ref <- lv$reference[!is.na(lv$reference) & nzchar(as.character(lv$reference))]
    if (length(ref) > 0) return(as.character(ref[1]))
  }
  NULL
}

resolve_value_label <- function(dict, var_name, value) {
  lv <- resolve_var_levels(dict, var_name)
  if (nrow(lv) == 0) return(as.character(value))

  hit <- lv[as.character(lv$value) == as.character(value), , drop = FALSE]
  if (nrow(hit) == 0) return(as.character(value))

  out <- hit$value_label[1]
  if (is.na(out) || !nzchar(out)) return(as.character(value))
  as.character(out)
}

resolve_var_label <- function(dict, var_name) {
  meta <- dict$meta %||% data.frame()
  if (!is.data.frame(meta) || nrow(meta) == 0) return(var_name)

  hit <- meta[meta$var_name == var_name, , drop = FALSE]
  if (nrow(hit) == 0) return(var_name)

  out <- hit$var_label[1] %||% var_name
  if (is.na(out) || !nzchar(out)) out <- var_name
  as.character(out)
}


# ------------------------------------------------------------
# 6-1. validation helpers
# ------------------------------------------------------------
validate_dictionary_against_data <- function(dict_raw, raw_data) {
  d0 <- if (is.data.frame(dict_raw)) as.data.frame(dict_raw, stringsAsFactors = FALSE) else data.frame()
  dat <- if (is.data.frame(raw_data)) as.data.frame(raw_data, stringsAsFactors = FALSE) else data.frame()

  issues <- list()

  if (!is.data.frame(d0) || nrow(d0) == 0) {
    issues[[length(issues) + 1L]] <- make_issue_row(
      issue_type = "dictionary_empty",
      detail = "Dictionary is empty.",
      severity = "error"
    )

    problems <- bind_issue_rows(issues)
    return(list(
      summary = data.frame(
        metric = c("n_dict_vars", "n_data_vars", "n_errors", "n_warnings", "status"),
        value = c(0, ncol(dat), sum(problems$severity == "error"), sum(problems$severity == "warn"), "error"),
        stringsAsFactors = FALSE
      ),
      problems = problems,
      only_in_dict = character(0),
      only_in_data = names(dat) %||% character(0)
    ))
  }

  d1 <- d0
  names(d1) <- if (exists("make_clean_names")) make_clean_names(names(d1)) else make.names(names(d1), unique = TRUE)

  if (!"var_name" %in% names(d1)) {
    cand <- intersect(c("variable", "var", "name"), names(d1))
    if (length(cand) > 0) names(d1)[match(cand[1], names(d1))] <- "var_name"
  }

  raw_var_names <- trimws(as.character(d1$var_name %||% character(0)))
  dup_raw <- unique(raw_var_names[duplicated(raw_var_names) & !is.na(raw_var_names) & nzchar(raw_var_names)])

  std <- standardize_dictionary(d1)
  meta <- std$meta %||% data.frame()
  lev  <- std$levels %||% data.frame()

  dict_names <- as.character(meta$var_name %||% character(0))
  data_names <- names(dat) %||% character(0)

  cmp <- compare_name_sets(dict_names, data_names)

  if (length(dup_raw) > 0) {
    for (v in dup_raw) {
      issues[[length(issues) + 1L]] <- make_issue_row(
        issue_type = "duplicate_var_name",
        var_name = v,
        detail = "Duplicated var_name in dictionary.",
        severity = "error"
      )
    }
  }

  if (length(cmp$only_in_dict) > 0) {
    for (v in cmp$only_in_dict) {
      issues[[length(issues) + 1L]] <- make_issue_row(
        issue_type = "missing_in_data",
        var_name = v,
        detail = "Variable exists in dictionary but not in raw data.",
        severity = "warn"
      )
    }
  }

  if (length(cmp$only_in_data) > 0) {
    for (v in cmp$only_in_data) {
      issues[[length(issues) + 1L]] <- make_issue_row(
        issue_type = "missing_in_dictionary",
        var_name = v,
        detail = "Variable exists in raw data but not in dictionary.",
        severity = "info"
      )
    }
  }

  if (nrow(meta) > 0) {
    for (i in seq_len(nrow(meta))) {
      v <- as.character(meta$var_name[i])

      role_i <- tolower(trimws(as.character(meta$analysis_role[i])))
      type_i <- tolower(trimws(as.character(meta$type[i])))
      subtype_i <- tolower(trimws(as.character(meta$subtype[i])))
      order_i <- suppressWarnings(as.numeric(meta$display_order[i]))

      if (is.na(role_i) || !nzchar(role_i)) role_i <- "none"
      if (is.na(type_i) || !nzchar(type_i)) type_i <- "auto"
      if (is.na(subtype_i) || !nzchar(subtype_i)) subtype_i <- NA_character_

      if (role_i == "none") {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "missing_role",
          var_name = v,
          detail = "analysis_role is missing or none.",
          severity = "info"
        )
      }

      if (type_i == "auto") {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "missing_type",
          var_name = v,
          detail = "type is missing or auto.",
          severity = "info"
        )
      }

      if (identical(role_i, "indicator") && (is.na(subtype_i) || !nzchar(subtype_i))) {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "missing_subtype",
          var_name = v,
          detail = "indicator subtype is missing.",
          severity = "warn"
        )
      }

      if (is.na(order_i)) {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "missing_display_order",
          var_name = v,
          detail = "display_order is missing.",
          severity = "info"
        )
      }
    }

    dup_order <- meta$display_order[duplicated(meta$display_order) & !is.na(meta$display_order)]
    if (length(dup_order) > 0) {
      hit <- meta$var_name[meta$display_order %in% dup_order]
      for (v in unique(hit)) {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "duplicated_display_order",
          var_name = v,
          detail = "display_order is duplicated.",
          severity = "warn"
        )
      }
    }
  }

  if (nrow(meta) > 0) {
    for (i in seq_len(nrow(meta))) {
      v <- as.character(meta$var_name[i])

      subtype_i <- tolower(trimws(as.character(meta$subtype[i])))
      if (is.na(subtype_i) || !nzchar(subtype_i) || subtype_i != "categorical") next

      lv_i <- lev[as.character(lev$var_name) == v, , drop = FALSE]
      if (nrow(lv_i) == 0) {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "missing_levels",
          var_name = v,
          detail = "categorical variable has no level table.",
          severity = "warn"
        )
        next
      }

      if (!"value_label" %in% names(lv_i)) lv_i$value_label <- lv_i$value

      bad_lab <- is.na(lv_i$value_label) | !nzchar(trimws(as.character(lv_i$value_label)))
      if (any(bad_lab)) {
        issues[[length(issues) + 1L]] <- make_issue_row(
          issue_type = "missing_value_label",
          var_name = v,
          detail = "some categorical levels have no labels.",
          severity = "warn"
        )
      }

      ref_i <- trimws(as.character(meta$reference[i]))
      if (!is.na(ref_i) && nzchar(ref_i)) {
        if (!(ref_i %in% as.character(lv_i$value))) {
          issues[[length(issues) + 1L]] <- make_issue_row(
            issue_type = "reference_not_in_levels",
            var_name = v,
            detail = paste0("reference=", ref_i, " not found in level values."),
            severity = "error"
          )
        }
      }
    }
  }

  problems <- bind_issue_rows(issues)
  n_error <- sum(problems$severity == "error", na.rm = TRUE)
  n_warn  <- sum(problems$severity == "warn", na.rm = TRUE)
  status  <- flag_issue_level(n_error = n_error, n_warn = n_warn)

  summary <- data.frame(
    metric = c(
      "n_dict_vars",
      "n_data_vars",
      "n_only_in_dict",
      "n_only_in_data",
      "n_errors",
      "n_warnings",
      "status"
    ),
    value = c(
      length(dict_names),
      length(data_names),
      cmp$n_only_in_dict,
      cmp$n_only_in_data,
      n_error,
      n_warn,
      status
    ),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    problems = problems,
    only_in_dict = cmp$only_in_dict,
    only_in_data = cmp$only_in_data
  )
}

validate_cfg_against_dict_and_data <- function(cfg, dict_raw, raw_data) {
  cfg <- cfg %||% list()
  dat <- if (is.data.frame(raw_data)) raw_data else data.frame()

  std <- standardize_dictionary(dict_raw)
  meta <- std$meta %||% data.frame()
  lev  <- std$levels %||% data.frame()

  dict_names <- as.character(meta$var_name %||% character(0))
  data_names <- names(dat) %||% character(0)

  issues <- list()

  outcomes_cfg <- unique(trimws(as.character(cfg$outcomes %||% character(0))))
  outcomes_cfg <- outcomes_cfg[!is.na(outcomes_cfg) & nzchar(outcomes_cfg)]

  for (v in outcomes_cfg) {
    if (!(v %in% data_names)) {
      issues[[length(issues) + 1L]] <- make_issue_row(
        issue_type = "cfg_outcome_missing_in_data",
        var_name = v,
        detail = "CFG outcome not found in raw data.",
        severity = "error"
      )
    } else if (!(v %in% dict_names)) {
      issues[[length(issues) + 1L]] <- make_issue_row(
        issue_type = "cfg_outcome_missing_in_dict",
        var_name = v,
        detail = "CFG outcome not found in dictionary.",
        severity = "warn"
      )
    }
  }

  survey_vars <- c(
    cfg$survey_design$weight_var %||% NULL,
    cfg$survey_design$strata_var %||% NULL,
    cfg$survey_design$cluster_var %||% NULL
  )
  survey_vars <- unique(trimws(as.character(survey_vars)))
  survey_vars <- survey_vars[!is.na(survey_vars) & nzchar(survey_vars)]

  for (v in survey_vars) {
    if (!(v %in% data_names)) {
      issues[[length(issues) + 1L]] <- make_issue_row(
        issue_type = "cfg_survey_var_missing_in_data",
        var_name = v,
        detail = "survey_design variable not found in raw data.",
        severity = "error"
      )
    }
  }

  mixture_type_cfg <- tolower(as.character(cfg$analysis$mixture_type %||% cfg$mixture_type %||% NA_character_))
  valid_mixture_types <- c("lca", "lpa", "mixed", "mixed_model", "mixed-model", "auto")
  if (!is.na(mixture_type_cfg) && nzchar(mixture_type_cfg) && !mixture_type_cfg %in% valid_mixture_types) {
    issues[[length(issues) + 1L]] <- make_issue_row(
      issue_type = "cfg_invalid_mixture_type",
      detail = paste0("Invalid mixture_type: ", mixture_type_cfg),
      severity = "error"
    )
  }

  problems <- bind_issue_rows(issues)
  n_error <- sum(problems$severity == "error", na.rm = TRUE)
  n_warn  <- sum(problems$severity == "warn", na.rm = TRUE)
  status  <- flag_issue_level(n_error = n_error, n_warn = n_warn)

  summary <- data.frame(
    metric = c("n_errors", "n_warnings", "status"),
    value = c(n_error, n_warn, status),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    problems = problems
  )
}

validate_subsets_against_data <- function(subsets_spec, raw_data) {
  dat <- if (is.data.frame(raw_data)) raw_data else data.frame()

  if (!is.list(subsets_spec) || length(subsets_spec) == 0 || nrow(dat) == 0) {
    return(data.frame(
      subset_name = character(0),
      rule_type = character(0),
      valid = logical(0),
      n_selected = integer(0),
      n_total = integer(0),
      message = character(0),
      severity = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out_list <- list()
  idx <- 1L

  get_expr_vars <- function(expr_text) {
    expr_text <- as.character(expr_text)[1]
    if (is.na(expr_text) || !nzchar(expr_text)) return(character(0))

    toks <- unique(unlist(regmatches(
      expr_text,
      gregexpr("[A-Za-z.][A-Za-z0-9._]*", expr_text, perl = TRUE)
    )))
    toks <- toks[!toks %in% c("is.na", "TRUE", "FALSE", "NA")]
    toks
  }

  for (nm in names(subsets_spec)) {
    ss <- subsets_spec[[nm]]

    rule_type <- NA_character_
    valid_i <- TRUE
    n_selected <- NA_integer_
    msg_i <- "ok"
    sev_i <- "info"

    if (is.list(ss)) {
      if (!is.null(ss$expr) && nzchar(as.character(ss$expr))) {
        rule_type <- "expr"
        used_vars <- get_expr_vars(ss$expr)
        miss_vars <- setdiff(used_vars, names(dat))

        if (length(miss_vars) > 0) {
          valid_i <- FALSE
          msg_i <- paste0("expr contains missing vars: ", paste(miss_vars, collapse = ", "))
          sev_i <- "error"
        } else {
          idx_i <- tryCatch(
            safe_eval_in_data(as.character(ss$expr), dat),
            error = function(e) rep(FALSE, nrow(dat))
          )
          n_selected <- sum(idx_i, na.rm = TRUE)

          if (n_selected == 0) {
            valid_i <- FALSE
            msg_i <- "subset selected zero rows."
            sev_i <- "error"
          } else if (n_selected == nrow(dat)) {
            msg_i <- "subset selected all rows."
            sev_i <- "warn"
          }
        }
      } else {
        var_i <- as.character(ss$var %||% ss$subset_var %||% NA_character_)
        values_i <- ss$values %||% ss$value %||% ss$subset_value %||% NULL

        if (!is.na(var_i) && nzchar(var_i)) {
          rule_type <- if (length(values_i) > 1) "var_values" else "var_value"

          if (!(var_i %in% names(dat))) {
            valid_i <- FALSE
            msg_i <- paste0("subset var not found in data: ", var_i)
            sev_i <- "error"
          } else {
            if (is.null(values_i) || length(values_i) == 0) {
              idx_i <- !is.na(dat[[var_i]])
            } else if (is.numeric(dat[[var_i]]) || is.integer(dat[[var_i]])) {
              idx_i <- dat[[var_i]] %in% suppressWarnings(as.numeric(values_i))
            } else {
              idx_i <- as.character(dat[[var_i]]) %in% as.character(values_i)
            }

            n_selected <- sum(idx_i, na.rm = TRUE)

            if (n_selected == 0) {
              valid_i <- FALSE
              msg_i <- "subset selected zero rows."
              sev_i <- "error"
            } else if (n_selected == nrow(dat)) {
              msg_i <- "subset selected all rows."
              sev_i <- "warn"
            }
          }
        } else {
          valid_i <- FALSE
          rule_type <- "unknown"
          msg_i <- "subset definition has neither expr nor var."
          sev_i <- "error"
        }
      }
    } else {
      valid_i <- FALSE
      rule_type <- "invalid"
      msg_i <- "subset entry is not a list."
      sev_i <- "error"
    }

    out_list[[idx]] <- data.frame(
      subset_name = as.character(nm),
      rule_type = as.character(rule_type),
      valid = isTRUE(valid_i),
      n_selected = n_selected,
      n_total = nrow(dat),
      message = as.character(msg_i),
      severity = as.character(sev_i),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 7. helper summaries
# ------------------------------------------------------------
dict_summary <- function(dict) {
  meta       <- dict$meta %||% data.frame()
  levels     <- dict$levels %||% data.frame()

  data.frame(
    n_meta           = if (is.data.frame(meta)) nrow(meta) else 0L,
    n_levels         = if (is.data.frame(levels)) nrow(levels) else 0L,
    n_indicator      = if (is.data.frame(meta)) sum(meta$analysis_role == "indicator", na.rm = TRUE) else 0L,
    n_covariate      = if (is.data.frame(meta)) sum(meta$analysis_role == "covariate", na.rm = TRUE) else 0L,
    n_outcome        = if (is.data.frame(meta)) sum(meta$analysis_role == "outcome", na.rm = TRUE) else 0L,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 8. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("05_dict.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Dictionary helpers registered\n")
cat("============================================================\n\n")
