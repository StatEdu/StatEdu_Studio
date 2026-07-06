# ============================================================
# 06_survey.R
# Survey design helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) CFG 기반 survey 변수 해석
# 2) weight / strata / cluster / subset / id 정리
# 3) subset 필터 적용
# 4) preflight 점검
# 5) Mplus용 survey syntax 생성
# 6) downstream에서 쓰는 SURVEY_BUNDLE 생성
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.first_nonempty <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(default)
  x1 <- as.character(x[1])
  if (!nzchar(x1)) return(default)
  x1
}

.as_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  as.character(x)
}

.coalesce_nonnull <- function(...) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0) return(val)
  }
  NULL
}

.as_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

.as_flag2 <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(default)
  isTRUE(as.logical(x)[1])
}

.nonempty_chr <- function(x) {
  x <- as.character(x %||% character(0))
  x[!is.na(x) & nzchar(x)]
}

.safe_mean2 <- function(x) {
  x <- .as_num(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

.safe_sd2 <- function(x) {
  x <- .as_num(x)
  if (sum(!is.na(x)) <= 1) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

.safe_cv2 <- function(x) {
  m <- .safe_mean2(x)
  s <- .safe_sd2(x)
  if (is.na(m) || is.na(s) || m == 0) return(NA_real_)
  s / m
}

# ------------------------------------------------------------
# 1. resolve survey vars from CFG / DICT
# ------------------------------------------------------------
resolve_survey_vars <- function(cfg = NULL, dict = NULL, raw_data = NULL) {
  meta <- dict$meta %||% data.frame()

  from_dict <- function(role_name) {
    if (!is.data.frame(meta) || nrow(meta) == 0 || !"analysis_role" %in% names(meta)) return(NULL)
    hit <- meta$var_name[meta$analysis_role == role_name]
    .first_nonempty(hit, default = NULL)
  }

  s_cfg <- cfg$survey_design %||% list()

  weight_var <- .first_nonempty(c(
    s_cfg$weight_var,
    cfg$weight_var,
    from_dict("weight")
  ), default = NULL)

  strata_var <- .first_nonempty(c(
    s_cfg$strata_var,
    cfg$strata_var,
    from_dict("strata")
  ), default = NULL)

  cluster_var <- .first_nonempty(c(
    s_cfg$cluster_var,
    cfg$cluster_var,
    from_dict("cluster")
  ), default = NULL)

  id_var <- .first_nonempty(c(
    s_cfg$id_var,
    cfg$id_var,
    from_dict("id"),
    "id"
  ), default = "id")

  subset_var <- .first_nonempty(c(
    s_cfg$subset_var,
    cfg$analysis$subset_var,
    cfg$subset_var
  ), default = NULL)

  subset_name <- .first_nonempty(c(
    s_cfg$subset_name,
    cfg$analysis$subset_name,
    cfg$subset_name
  ), default = NULL)

  replicate_method <- .first_nonempty(c(
    s_cfg$replicate_method,
    cfg$replicate_method
  ), default = NULL)

  rep_weights_prefix <- .first_nonempty(c(
    s_cfg$rep_weights_prefix,
    cfg$rep_weights_prefix
  ), default = NULL)

  rep_weight_vars <- .as_chr(
    s_cfg$rep_weight_vars %||% cfg$rep_weight_vars %||% character(0)
  )
  rep_weight_vars <- .nonempty_chr(rep_weight_vars)

  replicates <- suppressWarnings(as.integer(
    s_cfg$replicates %||% cfg$replicates %||% NA_integer_
  ))

  rscales <- s_cfg$rscales %||% cfg$rscales %||% NULL
  scale <- suppressWarnings(as.numeric(s_cfg$scale %||% cfg$scale %||% NA_real_))
  mse <- .as_flag2(s_cfg$mse %||% cfg$mse, default = TRUE)
  degf <- suppressWarnings(as.numeric(s_cfg$degf %||% cfg$degf %||% NA_real_))

  # raw_data 기준 존재 여부 보정
  if (is.data.frame(raw_data) && ncol(raw_data) > 0) {
    nms <- names(raw_data)

    pick_existing <- function(x) {
      if (is.null(x) || !nzchar(as.character(x))) return(NULL)
      if (x %in% nms) x else NULL
    }

    weight_var  <- pick_existing(weight_var)
    strata_var  <- pick_existing(strata_var)
    cluster_var <- pick_existing(cluster_var)

    if (!id_var %in% nms) {
      cand <- intersect(c("id", "ID", "case_id", "CASE_ID"), nms)
      if (length(cand) > 0) id_var <- cand[1]
    }

    if (!is.null(subset_var) && !subset_var %in% nms) subset_var <- NULL

    if (length(rep_weight_vars) == 0 && !is.null(rep_weights_prefix) && nzchar(rep_weights_prefix)) {
      rep_weight_vars <- grep(paste0("^", rep_weights_prefix), nms, value = TRUE)
    }
    rep_weight_vars <- rep_weight_vars[rep_weight_vars %in% nms]
  }

  list(
    weight_var = weight_var,
    strata_var = strata_var,
    cluster_var = cluster_var,
    subset_var = subset_var,
    subset_name = subset_name,
    id_var = id_var,
    replicate_method = replicate_method,
    rep_weights_prefix = rep_weights_prefix,
    rep_weight_vars = rep_weight_vars,
    replicates = replicates,
    rscales = rscales,
    scale = scale,
    mse = mse,
    degf = degf
  )
}

# ------------------------------------------------------------
# 2. resolve survey case
# ------------------------------------------------------------
resolve_survey_case <- function(weight_var = NULL,
                                strata_var = NULL,
                                cluster_var = NULL,
                                replicate_method = NULL,
                                rep_weight_vars = character(0)) {
  has_w <- !is.null(weight_var) && nzchar(weight_var)
  has_s <- !is.null(strata_var) && nzchar(strata_var)
  has_c <- !is.null(cluster_var) && nzchar(cluster_var)
  has_r <- (!is.null(replicate_method) && nzchar(as.character(replicate_method))) ||
    length(rep_weight_vars) > 0

  if (has_r) return("replicate")
  if (has_w && has_s && has_c) return("weight_strata_cluster")
  if (has_w && has_s) return("weight_strata")
  if (has_w && has_c) return("weight_cluster")
  if (has_s && has_c) return("strata_cluster")
  if (has_w) return("weight_only")
  if (has_s) return("strata_only")
  if (has_c) return("cluster_only")
  "none"
}

# ------------------------------------------------------------
# 3. subset resolution
# ------------------------------------------------------------
resolve_subset_spec <- function(cfg = NULL, subsets_spec = NULL, survey_vars = NULL) {
  s_cfg <- cfg$survey_design %||% list()

  subset_name <- .first_nonempty(c(
    survey_vars$subset_name,
    s_cfg$subset_name,
    cfg$analysis$subset_name,
    cfg$subset_name
  ), default = NULL)

  subset_var <- .first_nonempty(c(
    survey_vars$subset_var,
    s_cfg$subset_var,
    cfg$analysis$subset_var,
    cfg$subset_var
  ), default = NULL)

  subset_value <- .coalesce_nonnull(
    s_cfg$subset_value,
    s_cfg$subset_values,
    cfg$analysis$subset_value,
    cfg$analysis$subset_values,
    cfg$subset_value,
    cfg$subset_values
  )
  subset_expr  <- .first_nonempty(c(
    s_cfg$subset_expr,
    cfg$analysis$subset_expr,
    cfg$subset_expr
  ), default = NULL)

  # subsets.yml named entry 우선
  if (!is.null(subset_name) && is.list(subsets_spec) && length(subsets_spec) > 0) {
    ss <- subsets_spec[[subset_name]]
    if (is.list(ss)) {
      subset_var <- .first_nonempty(c(ss$var, ss$subset_var, subset_var), default = subset_var)
      if (is.null(subset_value)) {
        subset_value <- .coalesce_nonnull(
          ss$values,
          ss$value,
          ss$subset_values,
          ss$subset_value
        )
      }
      subset_expr <- .first_nonempty(c(ss$expr, ss$subset_expr, subset_expr), default = subset_expr)
    } else if (is.character(ss) && length(ss) == 1) {
      subset_expr <- .first_nonempty(c(ss, subset_expr), default = subset_expr)
    }
  }

  list(
    subset_name = subset_name,
    subset_var = subset_var,
    subset_value = subset_value,
    subset_expr = subset_expr
  )
}

# ------------------------------------------------------------
# 4. apply subset filter
# ------------------------------------------------------------
apply_subset_filter <- function(data, subset_spec = NULL, drop_na = TRUE) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)
  if (is.null(subset_spec) || length(subset_spec) == 0) return(data)

  subset_var <- subset_spec$subset_var %||% NULL
  subset_value <- subset_spec$subset_value %||% NULL
  subset_expr <- subset_spec$subset_expr %||% NULL

  idx <- rep(TRUE, nrow(data))

  # expr 우선
  if (!is.null(subset_expr) && nzchar(as.character(subset_expr))) {
    if (exists("safe_eval_in_data")) {
      idx <- safe_eval_in_data(as.character(subset_expr), data)
    } else {
      idx <- tryCatch(
        eval(parse(text = as.character(subset_expr)), envir = data, enclos = parent.frame()),
        error = function(e) rep(TRUE, nrow(data))
      )
      if (!is.logical(idx) || length(idx) != nrow(data)) idx <- rep(TRUE, nrow(data))
    }
  } else if (!is.null(subset_var) && nzchar(as.character(subset_var)) && subset_var %in% names(data)) {
    v <- data[[subset_var]]

    if (is.null(subset_value) || length(subset_value) == 0) {
      if (is.logical(v)) {
        idx <- isTRUE(v) | (!is.na(v) & v)
      } else {
        idx <- !is.na(v)
      }
    } else {
      sv_chr <- as.character(subset_value)
      if (is.numeric(v) || is.integer(v)) {
        idx <- v %in% suppressWarnings(as.numeric(sv_chr))
      } else {
        idx <- as.character(v) %in% sv_chr
      }
    }
  }

  if (!is.logical(idx) || length(idx) != nrow(data)) idx <- rep(TRUE, nrow(data))
  if (isTRUE(drop_na)) idx[is.na(idx)] <- FALSE

  data[idx, , drop = FALSE]
}

# ------------------------------------------------------------
# 5. preflight summary
# ------------------------------------------------------------
survey_preflight_summary <- function(data,
                                     weight_var = NULL,
                                     strata_var = NULL,
                                     cluster_var = NULL,
                                     subset_var = NULL,
                                     id_var = NULL,
                                     preflight_tolerance_mean_ratio = 0.05,
                                     preflight_cv_warn = 0.50,
                                     preflight_cv_safe = 0.30) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data.frame(
      metric = character(0),
      value = character(0),
      stringsAsFactors = FALSE
    ))
  }

  n <- nrow(data)

  weight_mean <- NA_real_
  weight_sd   <- NA_real_
  weight_cv   <- NA_real_
  weight_min  <- NA_real_
  weight_max  <- NA_real_
  weight_nonmissing <- NA_integer_

  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- .as_num(data[[weight_var]])
    weight_mean <- .safe_mean2(w)
    weight_sd   <- .safe_sd2(w)
    weight_cv   <- .safe_cv2(w)
    weight_min  <- if (all(is.na(w))) NA_real_ else min(w, na.rm = TRUE)
    weight_max  <- if (all(is.na(w))) NA_real_ else max(w, na.rm = TRUE)
    weight_nonmissing <- sum(!is.na(w))
  }

  unweighted_n <- n
  weighted_n_approx <- if (!is.na(weight_mean)) sum(.as_num(data[[weight_var]]), na.rm = TRUE) else NA_real_
  mean_ratio <- if (!is.na(weight_mean)) abs(weight_mean - 1) else NA_real_

  strata_n <- if (!is.null(strata_var) && strata_var %in% names(data)) length(unique(stats::na.omit(data[[strata_var]]))) else NA_integer_
  cluster_n <- if (!is.null(cluster_var) && cluster_var %in% names(data)) length(unique(stats::na.omit(data[[cluster_var]]))) else NA_integer_
  subset_n <- if (!is.null(subset_var) && subset_var %in% names(data)) sum(!is.na(data[[subset_var]])) else NA_integer_
  id_n <- if (!is.null(id_var) && id_var %in% names(data)) length(unique(stats::na.omit(data[[id_var]]))) else NA_integer_

  weight_cv_flag <- if (is.na(weight_cv)) {
    NA_character_
  } else if (weight_cv < preflight_cv_safe) {
    "safe"
  } else if (weight_cv < preflight_cv_warn) {
    "warn"
  } else {
    "high"
  }

  mean_ratio_flag <- if (is.na(mean_ratio)) {
    NA_character_
  } else if (mean_ratio <= preflight_tolerance_mean_ratio) {
    "ok"
  } else {
    "check"
  }

  data.frame(
    metric = c(
      "n_rows",
      "unweighted_n",
      "weighted_n_approx",
      "weight_nonmissing",
      "weight_mean",
      "weight_sd",
      "weight_cv",
      "weight_cv_flag",
      "weight_mean_abs_diff_from_1",
      "weight_mean_flag",
      "n_strata",
      "n_clusters",
      "n_unique_id",
      "subset_nonmissing"
    ),
    value = as.character(c(
      n,
      unweighted_n,
      weighted_n_approx,
      weight_nonmissing,
      weight_mean,
      weight_sd,
      weight_cv,
      weight_cv_flag,
      mean_ratio,
      mean_ratio_flag,
      strata_n,
      cluster_n,
      id_n,
      subset_n
    )),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 6. preflight detail
# ------------------------------------------------------------
survey_preflight_detail <- function(data,
                                    weight_var = NULL,
                                    strata_var = NULL,
                                    cluster_var = NULL,
                                    id_var = NULL) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data.frame())

  out_list <- list()
  idx <- 1L

  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- .as_num(data[[weight_var]])
    out_list[[idx]] <- data.frame(
      component = "weight",
      metric = c("missing", "nonpositive", "min", "p1", "p5", "median", "p95", "p99", "max"),
      value = c(
        sum(is.na(w)),
        sum(!is.na(w) & w <= 0),
        if (all(is.na(w))) NA else min(w, na.rm = TRUE),
        if (all(is.na(w))) NA else stats::quantile(w, 0.01, na.rm = TRUE, names = FALSE),
        if (all(is.na(w))) NA else stats::quantile(w, 0.05, na.rm = TRUE, names = FALSE),
        if (all(is.na(w))) NA else stats::median(w, na.rm = TRUE),
        if (all(is.na(w))) NA else stats::quantile(w, 0.95, na.rm = TRUE, names = FALSE),
        if (all(is.na(w))) NA else stats::quantile(w, 0.99, na.rm = TRUE, names = FALSE),
        if (all(is.na(w))) NA else max(w, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (!is.null(strata_var) && strata_var %in% names(data)) {
    s <- data[[strata_var]]
    tab <- sort(table(s), decreasing = TRUE)
    out_list[[idx]] <- data.frame(
      component = "strata",
      metric = c("n_unique", "largest_stratum_n", "smallest_stratum_n"),
      value = c(
        length(unique(stats::na.omit(s))),
        if (length(tab) == 0) NA else unname(tab[1]),
        if (length(tab) == 0) NA else unname(tab[length(tab)])
      ),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (!is.null(cluster_var) && cluster_var %in% names(data)) {
    cvar <- data[[cluster_var]]
    tab <- sort(table(cvar), decreasing = TRUE)
    out_list[[idx]] <- data.frame(
      component = "cluster",
      metric = c("n_unique", "largest_cluster_n", "smallest_cluster_n"),
      value = c(
        length(unique(stats::na.omit(cvar))),
        if (length(tab) == 0) NA else unname(tab[1]),
        if (length(tab) == 0) NA else unname(tab[length(tab)])
      ),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1L
  }

  if (!is.null(id_var) && id_var %in% names(data)) {
    ids <- data[[id_var]]
    out_list[[idx]] <- data.frame(
      component = "id",
      metric = c("n_unique", "n_duplicated"),
      value = c(
        length(unique(stats::na.omit(ids))),
        sum(duplicated(ids) & !is.na(ids))
      ),
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
# 7. Mplus survey syntax
# ------------------------------------------------------------
make_mplus_survey_syntax <- function(weight_var = NULL,
                                     strata_var = NULL,
                                     cluster_var = NULL,
                                     use_variable_block_prefix = FALSE) {
  lines <- character(0)

  if (!is.null(weight_var) && nzchar(weight_var)) {
    lines <- c(lines, paste0("WEIGHT = ", weight_var, ";"))
  }
  if (!is.null(strata_var) && nzchar(strata_var)) {
    lines <- c(lines, paste0("STRATIFICATION = ", strata_var, ";"))
  }
  if (!is.null(cluster_var) && nzchar(cluster_var)) {
    lines <- c(lines, paste0("CLUSTER = ", cluster_var, ";"))
  }

  if (isTRUE(use_variable_block_prefix) && length(lines) > 0) {
    lines <- c("VARIABLE:", paste0("  ", lines))
  }

  lines
}

# ------------------------------------------------------------
# 8. build survey bundle
# ------------------------------------------------------------
build_survey_bundle <- function(cfg = NULL,
                                dict = NULL,
                                raw_data = NULL,
                                subsets_spec = NULL) {
  sv <- resolve_survey_vars(cfg = cfg, dict = dict, raw_data = raw_data)
  ss <- resolve_subset_spec(cfg = cfg, subsets_spec = subsets_spec, survey_vars = sv)

  survey_case <- resolve_survey_case(
    weight_var = sv$weight_var,
    strata_var = sv$strata_var,
    cluster_var = sv$cluster_var,
    replicate_method = sv$replicate_method,
    rep_weight_vars = sv$rep_weight_vars
  )

  preflight_check <- .as_flag2(
    cfg$survey_design$preflight_check %||% cfg$preflight_check,
    default = TRUE
  )
  preflight_tolerance_mean_ratio <- .as_num(
    cfg$survey_design$preflight_tolerance_mean_ratio %||% cfg$preflight_tolerance_mean_ratio %||% 0.05
  )
  preflight_cv_warn <- .as_num(
    cfg$survey_design$preflight_cv_warn %||% cfg$preflight_cv_warn %||% 0.50
  )
  preflight_cv_safe <- .as_num(
    cfg$survey_design$preflight_cv_safe %||% cfg$preflight_cv_safe %||% 0.30
  )

  subset_preview_n <- NA_integer_
  preflight_summary <- data.frame()
  preflight_detail <- data.frame()

  if (is.data.frame(raw_data) && nrow(raw_data) > 0) {
    data_sub <- apply_subset_filter(raw_data, ss)
    subset_preview_n <- nrow(data_sub)

    if (isTRUE(preflight_check)) {
      preflight_summary <- survey_preflight_summary(
        data = data_sub,
        weight_var = sv$weight_var,
        strata_var = sv$strata_var,
        cluster_var = sv$cluster_var,
        subset_var = ss$subset_var,
        id_var = sv$id_var,
        preflight_tolerance_mean_ratio = preflight_tolerance_mean_ratio,
        preflight_cv_warn = preflight_cv_warn,
        preflight_cv_safe = preflight_cv_safe
      )

      preflight_detail <- survey_preflight_detail(
        data = data_sub,
        weight_var = sv$weight_var,
        strata_var = sv$strata_var,
        cluster_var = sv$cluster_var,
        id_var = sv$id_var
      )
    }
  }

  mplus_syntax <- make_mplus_survey_syntax(
    weight_var = sv$weight_var,
    strata_var = sv$strata_var,
    cluster_var = sv$cluster_var,
    use_variable_block_prefix = FALSE
  )

  list(
    survey_case = survey_case,
    weight_var = sv$weight_var,
    strata_var = sv$strata_var,
    cluster_var = sv$cluster_var,
    subset_var = ss$subset_var,
    subset_name = ss$subset_name,
    subset_value = ss$subset_value,
    subset_expr = ss$subset_expr,
    id_var = sv$id_var,
    replicate_method = sv$replicate_method,
    rep_weights_prefix = sv$rep_weights_prefix,
    rep_weight_vars = sv$rep_weight_vars,
    replicates = sv$replicates,
    rscales = sv$rscales,
    scale = sv$scale,
    mse = sv$mse,
    degf = sv$degf,
    preflight_check = preflight_check,
    preflight_tolerance_mean_ratio = preflight_tolerance_mean_ratio,
    preflight_cv_warn = preflight_cv_warn,
    preflight_cv_safe = preflight_cv_safe,
    subset_preview_n = subset_preview_n,
    mplus_survey_syntax = mplus_syntax,
    preflight_summary = preflight_summary,
    preflight_detail = preflight_detail
  )
}

# ------------------------------------------------------------
# 9. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("06_survey.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Survey helpers registered\n")
cat(" - resolve_survey_vars()\n")
cat(" - resolve_survey_case()\n")
cat(" - resolve_subset_spec()\n")
cat(" - apply_subset_filter()\n")
cat(" - survey_preflight_summary()\n")
cat(" - survey_preflight_detail()\n")
cat(" - make_mplus_survey_syntax()\n")
cat(" - build_survey_bundle()\n")
cat("============================================================\n\n")
