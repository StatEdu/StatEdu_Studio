# ============================================================
# 08_table.R
# Table helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) 표 생성용 공통 요약 함수 제공
# 2) class별 continuous / categorical summary 생성
# 3) p-value / sig / formatted column 생성
# 4) dictionary label 결합 지원
# 5) T1~T6 작성 시 재사용 가능한 기반 제공
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.nz_chr <- function(x) {
  x <- as.character(x %||% character(0))
  x[!is.na(x) & nzchar(x)]
}

.first_or <- function(x, default = NA_character_) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(default)
  x[1]
}

.to_num_tbl <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

.trim_ws_tbl <- function(x) {
  x <- as.character(x)
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

.coalesce_chr_tbl <- function(...) {
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

# ------------------------------------------------------------
# 1. formatting helpers
# ------------------------------------------------------------
fmt_num_tbl <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

fmt_int_tbl <- function(x) {
  ifelse(is.na(x), NA_character_, formatC(as.integer(round(x)), format = "d"))
}

fmt_pct_tbl <- function(x, digits = 1, scale = 100) {
  ifelse(is.na(x), NA_character_,
         paste0(formatC(scale * x, format = "f", digits = digits), "%"))
}

fmt_p_tbl <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < .001, "<.001", formatC(p, format = "f", digits = 3)))
}

p_to_sig_tbl <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}

fmt_mean_sd_tbl <- function(mean, sd, digits = 2) {
  ifelse(is.na(mean), NA_character_,
         paste0(fmt_num_tbl(mean, digits), " (", fmt_num_tbl(sd, digits), ")"))
}

fmt_mean_se_tbl <- function(mean, se, digits = 2) {
  ifelse(is.na(mean), NA_character_,
         paste0(fmt_num_tbl(mean, digits), " (", fmt_num_tbl(se, digits), ")"))
}

fmt_n_pct_tbl <- function(n, prop, digits = 1) {
  ifelse(is.na(n), NA_character_,
         paste0(fmt_int_tbl(n), " (", fmt_pct_tbl(prop, digits = digits), ")"))
}

# ------------------------------------------------------------
# 2. type helpers
# ------------------------------------------------------------
is_binary_like_tbl <- function(x) {
  ux <- unique(stats::na.omit(x))
  length(ux) <= 2 && all(sort(ux) %in% c(0, 1))
}

is_categorical_like_tbl <- function(x, max_unique = 10) {
  if (is.factor(x) || is.character(x) || is.logical(x)) return(TRUE)
  ux <- unique(stats::na.omit(x))
  length(ux) <= max_unique
}

# ------------------------------------------------------------
# 3. simple stats
# ------------------------------------------------------------
safe_mean_tbl <- function(x) {
  x <- .to_num_tbl(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd_tbl <- function(x) {
  x <- .to_num_tbl(x)
  if (sum(!is.na(x)) <= 1) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

safe_se_tbl <- function(x) {
  x <- .to_num_tbl(x)
  n <- sum(!is.na(x))
  if (n <= 1) return(NA_real_)
  stats::sd(x, na.rm = TRUE) / sqrt(n)
}

safe_median_tbl <- function(x) {
  x <- .to_num_tbl(x)
  if (all(is.na(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

safe_min_tbl <- function(x) {
  x <- .to_num_tbl(x)
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max_tbl <- function(x) {
  x <- .to_num_tbl(x)
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_ttest_p_tbl <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tryCatch(stats::t.test(y ~ as.factor(g))$p.value, error = function(e) NA_real_)
}

safe_wilcox_p_tbl <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tryCatch(stats::wilcox.test(y ~ as.factor(g))$p.value, error = function(e) NA_real_)
}

safe_aov_p_tbl <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- g[ok]
  if (length(unique(g)) < 2) return(NA_real_)
  tryCatch(summary(stats::aov(y ~ as.factor(g)))[[1]][["Pr(>F)"]][1], error = function(e) NA_real_)
}

safe_chisq_p_tbl <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (length(unique(x)) < 2 || length(unique(g)) < 2) return(NA_real_)
  tb <- table(x, g)
  tryCatch(suppressWarnings(stats::chisq.test(tb)$p.value), error = function(e) NA_real_)
}

# ------------------------------------------------------------
# 4. dictionary label join
# ------------------------------------------------------------
get_dict_meta_tbl <- function(dict) {
  meta <- dict$meta %||% dict$META %||% dict
  if (!is.data.frame(meta) || nrow(meta) == 0) return(data.frame())

  out <- data.frame(
    var_name = as.character(meta$var_name %||% character(0)),
    stringsAsFactors = FALSE
  )

  out$var_label <- .coalesce_chr_tbl(
    meta$var_label %||% NULL,
    meta$label_ko %||% NULL,
    meta$label_en %||% NULL,
    out$var_name
  )

  if ("display_order" %in% names(meta)) {
    out$display_order <- suppressWarnings(as.numeric(meta$display_order))
  } else {
    out$display_order <- seq_len(nrow(out))
  }

  if ("display_group" %in% names(meta)) {
    out$display_group <- as.character(meta$display_group)
  } else {
    out$display_group <- NA_character_
  }

  out <- out[!duplicated(out$var_name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

get_dict_levels_tbl <- function(dict) {
  lev <- dict$levels %||% dict$LEVELS %||% data.frame()
  if (!is.data.frame(lev) || nrow(lev) == 0) return(data.frame())

  out <- lev
  if (!"var_name" %in% names(out)) return(data.frame())
  if (!"value" %in% names(out)) return(data.frame())
  if (!"value_label" %in% names(out)) out$value_label <- out$value
  out$value_label <- .coalesce_chr_tbl(out$value_label, out$value)
  rownames(out) <- NULL
  out
}

left_join_var_labels_tbl <- function(df, dict_meta, by = "var_name") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0) {
    if (!"var_label" %in% names(df) && by %in% names(df)) df$var_label <- df[[by]]
    return(df)
  }

  keep <- intersect(c(by, "var_label", "display_order", "display_group"), names(dict_meta))
  lab <- unique(dict_meta[, keep, drop = FALSE])

  out <- merge(df, lab, by = by, all.x = TRUE, sort = FALSE)
  if (!"var_label" %in% names(out)) out$var_label <- out[[by]]
  out$var_label <- .coalesce_chr_tbl(out$var_label, out[[by]])
  out
}

left_join_value_labels_tbl <- function(df, dict_levels, var_col = "var_name", value_col = "level") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (!is.data.frame(dict_levels) || nrow(dict_levels) == 0) {
    if (!"value_label" %in% names(df) && value_col %in% names(df)) df$value_label <- as.character(df[[value_col]])
    return(df)
  }

  map <- dict_levels[, intersect(c("var_name", "value", "value_label", "reference"), names(dict_levels)), drop = FALSE]
  names(map)[names(map) == "var_name"] <- var_col
  names(map)[names(map) == "value"] <- value_col

  df[[value_col]] <- as.character(df[[value_col]])
  map[[value_col]] <- as.character(map[[value_col]])

  out <- merge(df, unique(map), by = c(var_col, value_col), all.x = TRUE, sort = FALSE)
  if (!"value_label" %in% names(out)) out$value_label <- out[[value_col]]
  out$value_label <- .coalesce_chr_tbl(out$value_label, out[[value_col]])
  out
}

# ------------------------------------------------------------
# 5. class-level summary builders
# ------------------------------------------------------------
summarize_continuous_by_class <- function(df, var_name, class_col = "class_num") {
  if (!is.data.frame(df) || !all(c(var_name, class_col) %in% names(df))) return(data.frame())

  sub <- df[, c(class_col, var_name), drop = FALSE]
  names(sub) <- c("class_num", "value")
  sub <- sub[!is.na(sub$class_num), , drop = FALSE]
  if (nrow(sub) == 0) return(data.frame())

  agg_n <- aggregate(value ~ class_num, data = sub, function(x) sum(!is.na(x)))
  agg_mean <- aggregate(value ~ class_num, data = sub, function(x) mean(x, na.rm = TRUE))
  agg_sd <- aggregate(value ~ class_num, data = sub, function(x) stats::sd(x, na.rm = TRUE))
  agg_median <- aggregate(value ~ class_num, data = sub, function(x) stats::median(x, na.rm = TRUE))
  agg_min <- aggregate(value ~ class_num, data = sub, function(x) min(x, na.rm = TRUE))
  agg_max <- aggregate(value ~ class_num, data = sub, function(x) max(x, na.rm = TRUE))

  out <- merge(agg_n, agg_mean, by = "class_num", all = TRUE)
  out <- merge(out, agg_sd, by = "class_num", all = TRUE, suffixes = c("", "_sdtmp"))
  out <- merge(out, agg_median, by = "class_num", all = TRUE, suffixes = c("", "_medtmp"))
  out <- merge(out, agg_min, by = "class_num", all = TRUE, suffixes = c("", "_mintmp"))
  out <- merge(out, agg_max, by = "class_num", all = TRUE, suffixes = c("", "_maxtmp"))

  names(out)[2:6] <- c("n", "mean", "sd", "median", "min", "max")[seq_len(ncol(out) - 1)]
  out$se <- out$sd / sqrt(out$n)
  out$var_name <- var_name
  out$class <- paste("Class", out$class_num)

  p <- if (length(unique(stats::na.omit(sub$class_num))) == 2) {
    safe_ttest_p_tbl(sub$value, sub$class_num)
  } else {
    safe_aov_p_tbl(sub$value, sub$class_num)
  }

  out$p <- p
  out$p_fmt <- fmt_p_tbl(p)
  out$sig <- p_to_sig_tbl(p)

  out[, c("var_name", "class_num", "class", "n", "mean", "sd", "se", "median", "min", "max", "p", "p_fmt", "sig"), drop = FALSE]
}

summarize_categorical_by_class <- function(df, var_name, class_col = "class_num", include_all_levels = TRUE) {
  if (!is.data.frame(df) || !all(c(var_name, class_col) %in% names(df))) return(data.frame())

  sub <- df[, c(class_col, var_name), drop = FALSE]
  names(sub) <- c("class_num", "value")
  sub <- sub[!is.na(sub$class_num), , drop = FALSE]

  if (nrow(sub) == 0) return(data.frame())

  if (isTRUE(include_all_levels)) {
    levs <- sort(unique(stats::na.omit(as.character(sub$value))))
  } else {
    levs <- unique(stats::na.omit(as.character(sub$value)))
  }
  if (length(levs) == 0) return(data.frame())

  p <- safe_chisq_p_tbl(as.character(sub$value), sub$class_num)

  out_list <- vector("list", length(levs))
  for (i in seq_along(levs)) {
    lev <- levs[i]
    tmp <- data.frame(
      class_num = sub$class_num,
      y = ifelse(as.character(sub$value) == lev, 1, ifelse(is.na(sub$value), NA, 0))
    )

    agg_n <- aggregate(y ~ class_num, data = tmp, function(x) sum(!is.na(x)))
    agg_prop <- aggregate(y ~ class_num, data = tmp, function(x) mean(x, na.rm = TRUE))

    out_i <- merge(agg_n, agg_prop, by = "class_num", all = TRUE)
    names(out_i) <- c("class_num", "n", "prop")
    out_i$var_name <- var_name
    out_i$level <- lev
    out_i$class <- paste("Class", out_i$class_num)
    out_i$p <- p
    out_i$p_fmt <- fmt_p_tbl(p)
    out_i$sig <- p_to_sig_tbl(p)

    out_list[[i]] <- out_i[, c("var_name", "level", "class_num", "class", "n", "prop", "p", "p_fmt", "sig"), drop = FALSE]
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 6. multi-variable wrappers
# ------------------------------------------------------------
build_continuous_summary_table <- function(df, vars, class_col = "class_num", dict = NULL) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(data.frame())

  res <- do.call(rbind, lapply(vars, function(v) {
    summarize_continuous_by_class(df, v, class_col = class_col)
  }))

  if (!is.data.frame(res) || nrow(res) == 0) return(data.frame())

  dict_meta <- get_dict_meta_tbl(dict)
  res <- left_join_var_labels_tbl(res, dict_meta, by = "var_name")

  if ("display_order" %in% names(res)) {
    res <- res[order(res$display_order, res$var_name, res$class_num), , drop = FALSE]
  } else {
    res <- res[order(res$var_name, res$class_num), , drop = FALSE]
  }

  rownames(res) <- NULL
  res
}

build_categorical_summary_table <- function(df, vars, class_col = "class_num", dict = NULL) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(data.frame())

  res <- do.call(rbind, lapply(vars, function(v) {
    summarize_categorical_by_class(df, v, class_col = class_col)
  }))

  if (!is.data.frame(res) || nrow(res) == 0) return(data.frame())

  dict_meta <- get_dict_meta_tbl(dict)
  dict_levels <- get_dict_levels_tbl(dict)

  res <- left_join_var_labels_tbl(res, dict_meta, by = "var_name")
  res <- left_join_value_labels_tbl(res, dict_levels, var_col = "var_name", value_col = "level")

  if ("display_order" %in% names(res)) {
    res <- res[order(res$display_order, res$var_name, res$level, res$class_num), , drop = FALSE]
  } else {
    res <- res[order(res$var_name, res$level, res$class_num), , drop = FALSE]
  }

  rownames(res) <- NULL
  res
}

build_mixed_summary_table <- function(df, vars, class_col = "class_num", dict = NULL, continuous_rule_unique = 5) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(data.frame())

  cont_vars <- character(0)
  cat_vars <- character(0)

  for (v in vars) {
    x <- df[[v]]
    if (is.numeric(x) && !is_binary_like_tbl(x) && length(unique(stats::na.omit(x))) > continuous_rule_unique) {
      cont_vars <- c(cont_vars, v)
    } else {
      cat_vars <- c(cat_vars, v)
    }
  }

  cont_tab <- build_continuous_summary_table(df, cont_vars, class_col = class_col, dict = dict)
  cat_tab  <- build_categorical_summary_table(df, cat_vars, class_col = class_col, dict = dict)

  list(
    continuous = cont_tab,
    categorical = cat_tab
  )
}

# ------------------------------------------------------------
# 7. model fit / class summary helpers
# ------------------------------------------------------------
build_model_fit_table <- function(fit_summary, best_k = NA_integer_) {
  if (!is.data.frame(fit_summary) || nrow(fit_summary) == 0) return(data.frame())

  keep <- intersect(
    c("k", "ll", "aic", "bic", "sabic", "caic", "entropy", "tech11_p", "tech14_p", "status_ok"),
    names(fit_summary)
  )

  out <- fit_summary[, keep, drop = FALSE]

  if ("tech11_p" %in% names(out)) out$tech11_p_fmt <- fmt_p_tbl(out$tech11_p)
  if ("tech14_p" %in% names(out)) out$tech14_p_fmt <- fmt_p_tbl(out$tech14_p)
  if (!is.na(best_k) && "k" %in% names(out)) out$is_best <- out$k == best_k

  rownames(out) <- NULL
  out
}

build_class_summary_table <- function(class_summary_final) {
  if (!is.data.frame(class_summary_final) || nrow(class_summary_final) == 0) return(data.frame())

  out <- class_summary_final
  if (!"prop" %in% names(out) && "n" %in% names(out)) {
    out$prop <- out$n / sum(out$n, na.rm = TRUE)
  }
  if ("prop" %in% names(out)) out$prop_pct <- 100 * out$prop

  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 8. wide-format helpers for reporting
# ------------------------------------------------------------
pivot_class_wide_continuous <- function(df,
                                        value_cols = c("mean", "sd", "se", "p_fmt", "sig"),
                                        class_col = "class",
                                        id_cols = c("var_name", "var_label")) {
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  if (!class_col %in% names(df)) return(df)

  parts <- list()
  for (vc in value_cols) {
    if (!vc %in% names(df)) next

    tmp <- df[, c(intersect(id_cols, names(df)), class_col, vc), drop = FALSE]
    names(tmp)[names(tmp) == vc] <- "value"
    tmp$measure <- vc
    parts[[length(parts) + 1L]] <- tmp
  }

  if (length(parts) == 0) return(df)

  long <- do.call(rbind, parts)
  long$col_name <- paste0(long$measure, "_", make.names(as.character(long[[class_col]])))

  wide <- reshape(
    long[, c(intersect(id_cols, names(long)), "col_name", "value"), drop = FALSE],
    idvar = intersect(id_cols, names(long)),
    timevar = "col_name",
    direction = "wide"
  )

  names(wide) <- sub("^value\\.", "", names(wide))
  rownames(wide) <- NULL
  wide
}

pivot_class_wide_categorical <- function(df,
                                         value_cols = c("n", "prop", "p_fmt", "sig"),
                                         class_col = "class",
                                         id_cols = c("var_name", "var_label", "level", "value_label")) {
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  if (!class_col %in% names(df)) return(df)

  parts <- list()
  for (vc in value_cols) {
    if (!vc %in% names(df)) next

    tmp <- df[, c(intersect(id_cols, names(df)), class_col, vc), drop = FALSE]
    names(tmp)[names(tmp) == vc] <- "value"
    tmp$measure <- vc
    parts[[length(parts) + 1L]] <- tmp
  }

  if (length(parts) == 0) return(df)

  long <- do.call(rbind, parts)
  long$col_name <- paste0(long$measure, "_", make.names(as.character(long[[class_col]])))

  wide <- reshape(
    long[, c(intersect(id_cols, names(long)), "col_name", "value"), drop = FALSE],
    idvar = intersect(id_cols, names(long)),
    timevar = "col_name",
    direction = "wide"
  )

  names(wide) <- sub("^value\\.", "", names(wide))
  rownames(wide) <- NULL
  wide
}

# ------------------------------------------------------------
# 9. empty-table helpers
# ------------------------------------------------------------
empty_continuous_table <- function() {
  data.frame(
    var_name = character(0),
    var_label = character(0),
    class_num = integer(0),
    class = character(0),
    n = numeric(0),
    mean = numeric(0),
    sd = numeric(0),
    se = numeric(0),
    median = numeric(0),
    min = numeric(0),
    max = numeric(0),
    p = numeric(0),
    p_fmt = character(0),
    sig = character(0),
    stringsAsFactors = FALSE
  )
}

empty_categorical_table <- function() {
  data.frame(
    var_name = character(0),
    var_label = character(0),
    level = character(0),
    value_label = character(0),
    class_num = integer(0),
    class = character(0),
    n = numeric(0),
    prop = numeric(0),
    p = numeric(0),
    p_fmt = character(0),
    sig = character(0),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 10. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("08_table.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Table helpers registered\n")
cat("============================================================\n\n")