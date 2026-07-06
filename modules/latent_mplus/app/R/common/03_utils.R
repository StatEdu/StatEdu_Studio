# ============================================================
# 03_utils.R
# Common utility helpers for mixture analysis pipeline
# ------------------------------------------------------------
# 역할
# 1) 자주 쓰는 문자열/숫자/자료형 유틸 제공
# 2) 결측/병합/정렬/파일 관련 helper 제공
# 3) 파이프라인 전 단계에서 공통 재사용
# ============================================================

# ------------------------------------------------------------
# 0. core helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

`%notin%` <- function(x, table) !(x %in% table)

.now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

is_empty <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x))
}

nz <- function(x) {
  x <- as.character(x %||% character(0))
  x[!is.na(x) & nzchar(x)]
}

trim_ws2 <- function(x) {
  x <- as.character(x)
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

norm_space <- function(x) {
  x <- trim_ws2(x)
  gsub("[[:space:]]+", " ", x)
}

to_lower_safe <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), NA_character_, tolower(x))
}

to_upper_safe <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), NA_character_, toupper(x))
}

coalesce_chr <- function(...) {
  xs <- list(...)
  if (length(xs) == 0) return(character(0))
  out <- xs[[1]]
  if (is.null(out)) out <- character(0)
  out <- as.character(out)

  if (length(xs) >= 2) {
    for (i in 2:length(xs)) {
      y <- xs[[i]]
      y <- as.character(y)
      if (length(y) == 1L && length(out) > 1L) y <- rep(y, length(out))
      idx <- is.na(out) | !nzchar(out)
      out[idx] <- y[idx]
    }
  }
  out
}

# ------------------------------------------------------------
# 1. vector / name helpers
# ------------------------------------------------------------
unique_chr <- function(x) {
  unique(as.character(x))
}

unique_nz <- function(x) {
  unique(nz(x))
}

collapse_chr <- function(x, sep = ", ") {
  x <- nz(x)
  paste(x, collapse = sep)
}

any_of2 <- function(x, choices) {
  intersect(as.character(x), as.character(choices))
}

first_of <- function(x, default = NA_character_) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(default)
  x[1]
}

first_existing_name <- function(candidates, names_pool) {
  hit <- intersect(as.character(candidates), as.character(names_pool))
  if (length(hit) == 0) return(NULL)
  hit[1]
}

make_clean_names <- function(x) {
  x <- as.character(x)
  x <- trim_ws2(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x <- make.names(x, unique = TRUE)
  x
}

repair_names_if_needed <- function(df) {
  if (!is.data.frame(df)) return(df)
  names(df) <- make_clean_names(names(df))
  df
}

# ------------------------------------------------------------
# 2. type converters
# ------------------------------------------------------------
as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(default)
  isTRUE(as.logical(x)[1])
}

to_num <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "", x)
  x <- gsub("\\*", "", x)
  suppressWarnings(as.numeric(x))
}

to_int <- function(x) {
  suppressWarnings(as.integer(round(as.numeric(x))))
}

as_chr_na <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  x
}

as_num_safely <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.logical(x)) return(as.numeric(x))
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

as_factor_safely <- function(x) {
  if (is.factor(x)) return(x)
  factor(x)
}

# ------------------------------------------------------------
# 3. missing value helpers
# ------------------------------------------------------------
default_missing_strings <- function() {
  c("", " ", ".", "..", "...", "NA", "N/A", "na", "n/a", "NaN", "NULL", "null", "missing", "Missing")
}

clean_missing_strings <- function(x, extra_missing = NULL) {
  miss <- unique(c(default_missing_strings(), extra_missing))
  if (is.factor(x)) x <- as.character(x)

  if (is.character(x)) {
    x <- trim_ws2(x)
    x[x %in% miss] <- NA_character_
  }
  x
}

apply_missing_rule_vector <- function(x, missing_values = NULL) {
  if (is.null(missing_values) || length(missing_values) == 0) return(x)

  if (is.factor(x)) x <- as.character(x)

  if (is.character(x)) {
    x[trim_ws2(x) %in% as.character(missing_values)] <- NA_character_
    return(x)
  }

  if (is.numeric(x) || is.integer(x)) {
    x[x %in% suppressWarnings(as.numeric(missing_values))] <- NA
    return(x)
  }

  x
}

count_missing <- function(x) sum(is.na(x))
prop_missing <- function(x) mean(is.na(x))

missing_summary_df <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(data.frame(
      var_name = character(0),
      n = integer(0),
      n_missing = integer(0),
      prop_missing = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    var_name = names(df),
    n = nrow(df),
    n_missing = vapply(df, count_missing, integer(1)),
    prop_missing = vapply(df, prop_missing, numeric(1)),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 4. dataframe helpers
# ------------------------------------------------------------
add_missing_col <- function(df, col, default = NA) {
  if (!is.data.frame(df)) return(df)
  if (!col %in% names(df)) df[[col]] <- default
  df
}

add_missing_cols <- function(df, named_defaults) {
  if (!is.data.frame(df)) return(df)
  if (length(named_defaults) == 0) return(df)

  for (nm in names(named_defaults)) {
    if (!nm %in% names(df)) df[[nm]] <- named_defaults[[nm]]
  }
  df
}

move_cols_front <- function(df, cols) {
  if (!is.data.frame(df)) return(df)
  cols <- intersect(cols, names(df))
  df[, c(cols, setdiff(names(df), cols)), drop = FALSE]
}

safe_select <- function(df, cols) {
  if (!is.data.frame(df)) return(data.frame())
  cols <- intersect(cols, names(df))
  df[, cols, drop = FALSE]
}

safe_drop_dup <- function(df, by = names(df)) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  by <- intersect(by, names(df))
  if (length(by) == 0) return(df)
  df[!duplicated(df[, by, drop = FALSE]), , drop = FALSE]
}

safe_order_df <- function(df, cols, na.last = TRUE) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) return(df)
  ord <- do.call(order, c(df[, cols, drop = FALSE], list(na.last = na.last)))
  df[ord, , drop = FALSE]
}

safe_rbind <- function(...) {
  xs <- list(...)
  xs <- xs[vapply(xs, is.data.frame, logical(1))]
  if (length(xs) == 0) return(data.frame())

  all_names <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs2 <- lapply(xs, function(d) {
    miss <- setdiff(all_names, names(d))
    for (m in miss) d[[m]] <- rep(NA, nrow(d))
    d[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, xs2)
  rownames(out) <- NULL
  out
}

safe_merge <- function(x, y, by, all.x = TRUE, all.y = FALSE, sort = FALSE) {
  if (!is.data.frame(x)) x <- data.frame()
  if (!is.data.frame(y)) y <- data.frame()

  by <- intersect(by, intersect(names(x), names(y)))
  if (length(by) == 0) return(x)

  merge(x, y, by = by, all.x = all.x, all.y = all.y, sort = sort)
}

# ------------------------------------------------------------
# 5. file / path helpers
# ------------------------------------------------------------
.norm_path2 <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- gsub("/+", "/", x)
  x
}

file_exists2 <- function(path) {
  !is.na(path) && nzchar(path) && file.exists(path)
}

dir_exists2 <- function(path) {
  !is.na(path) && nzchar(path) && dir.exists(path)
}

ensure_dir2 <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

list_files2 <- function(path, pattern = NULL, recursive = FALSE, full.names = TRUE) {
  if (!dir.exists(path)) return(character(0))
  list.files(path, pattern = pattern, recursive = recursive, full.names = full.names)
}

first_existing_path <- function(paths) {
  paths <- as.character(paths)
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

safe_read_rds2 <- function(path, default = NULL) {
  if (!file.exists(path)) return(default)
  tryCatch(readRDS(path), error = function(e) default)
}

safe_write_csv <- function(df, path, row.names = FALSE, na = "") {
  ensure_dir2(dirname(path))
  utils::write.csv(df, file = path, row.names = row.names, na = na)
  invisible(path)
}

safe_write_lines <- function(lines, path) {
  ensure_dir2(dirname(path))
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(lines), con = con)
  invisible(path)
}

copy_if_exists2 <- function(from, to, overwrite = TRUE) {
  if (!file_exists2(from)) return(FALSE)
  ensure_dir2(dirname(to))
  tryCatch(file.copy(from, to, overwrite = overwrite), error = function(e) FALSE)
}

# ------------------------------------------------------------
# 6. formatting helpers
# ------------------------------------------------------------
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

fmt_int <- function(x) {
  ifelse(is.na(x), NA_character_, formatC(as.integer(round(x)), format = "d"))
}

fmt_pct <- function(x, digits = 1, scale = 100) {
  ifelse(is.na(x), NA_character_,
         paste0(formatC(scale * x, format = "f", digits = digits), "%"))
}

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < .001, "<.001", formatC(p, format = "f", digits = 3)))
}

p_to_sig <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ""))))
}

fmt_mean_sd <- function(mean, sd, digits = 2) {
  ifelse(is.na(mean), NA_character_,
         paste0(fmt_num(mean, digits), " (", fmt_num(sd, digits), ")"))
}

fmt_n_pct <- function(n, p, digits = 1) {
  ifelse(is.na(n), NA_character_,
         paste0(n, " (", fmt_pct(p, digits = digits), ")"))
}

# ------------------------------------------------------------
# 7. simple stats helpers
# ------------------------------------------------------------
is_binary_like <- function(x) {
  ux <- unique(stats::na.omit(x))
  length(ux) <= 2 && all(sort(ux) %in% c(0, 1))
}

is_categorical_like <- function(x, max_unique = 10) {
  if (is.factor(x) || is.character(x)) return(TRUE)
  ux <- unique(stats::na.omit(x))
  length(ux) <= max_unique
}

safe_mean <- function(x) {
  suppressWarnings(mean(as.numeric(x), na.rm = TRUE))
}

safe_sd <- function(x) {
  suppressWarnings(stats::sd(as.numeric(x), na.rm = TRUE))
}

safe_se <- function(x) {
  x <- as.numeric(x)
  n <- sum(!is.na(x))
  if (n <= 1) return(NA_real_)
  stats::sd(x, na.rm = TRUE) / sqrt(n)
}

safe_median <- function(x) {
  suppressWarnings(stats::median(as.numeric(x), na.rm = TRUE))
}

safe_min <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

mode_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_ttest_p <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tryCatch(stats::t.test(y ~ as.factor(g))$p.value, error = function(e) NA_real_)
}

safe_wilcox_p <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- g[ok]
  if (length(unique(g)) != 2) return(NA_real_)
  tryCatch(stats::wilcox.test(y ~ as.factor(g))$p.value, error = function(e) NA_real_)
}

safe_aov_p <- function(y, g) {
  ok <- !is.na(y) & !is.na(g)
  y <- y[ok]
  g <- g[ok]
  if (length(unique(g)) < 2) return(NA_real_)
  tryCatch(summary(stats::aov(y ~ as.factor(g)))[[1]][["Pr(>F)"]][1], error = function(e) NA_real_)
}

safe_chisq_p <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (length(unique(x)) < 2 || length(unique(g)) < 2) return(NA_real_)
  tb <- table(x, g)
  tryCatch(suppressWarnings(stats::chisq.test(tb)$p.value), error = function(e) NA_real_)
}

# ------------------------------------------------------------
# 8. label / dictionary helpers
# ------------------------------------------------------------
resolve_var_label <- function(var_name, dict_meta = NULL) {
  if (is.null(dict_meta) || !is.data.frame(dict_meta) || nrow(dict_meta) == 0) {
    return(var_name)
  }
  if (!"var_name" %in% names(dict_meta)) return(var_name)

  hit <- dict_meta[dict_meta$var_name == var_name, , drop = FALSE]
  if (nrow(hit) == 0) return(var_name)

  cand_cols <- c("var_label", "label_ko", "label_en")
  cand_cols <- intersect(cand_cols, names(hit))
  if (length(cand_cols) == 0) return(var_name)

  for (cc in cand_cols) {
    val <- as.character(hit[[cc]][1])
    if (!is.na(val) && nzchar(val)) return(val)
  }
  var_name
}

left_join_labels <- function(df, dict_meta, by = "var_name") {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (!is.data.frame(dict_meta) || nrow(dict_meta) == 0) {
    if (!"var_label" %in% names(df) && by %in% names(df)) df$var_label <- df[[by]]
    return(df)
  }

  keep <- intersect(c(by, "var_label", "label_ko", "label_en", "display_order", "display_group"), names(dict_meta))
  lab <- unique(dict_meta[, keep, drop = FALSE])
  out <- merge(df, lab, by = by, all.x = TRUE, sort = FALSE)

  if (!"var_label" %in% names(out)) out$var_label <- NA_character_
  if ("label_ko" %in% names(out)) out$var_label <- coalesce_chr(out$var_label, out$label_ko)
  if ("label_en" %in% names(out)) out$var_label <- coalesce_chr(out$var_label, out$label_en)
  out$var_label <- coalesce_chr(out$var_label, out[[by]])

  out
}

# ------------------------------------------------------------
# 9. subset / rule helpers
# ------------------------------------------------------------
safe_eval_in_data <- function(expr_text, data) {
  if (is.null(expr_text) || !nzchar(expr_text) || !is.data.frame(data)) {
    return(rep(TRUE, nrow(data)))
  }

  out <- tryCatch(
    eval(parse(text = expr_text), envir = data, enclos = parent.frame()),
    error = function(e) rep(TRUE, nrow(data))
  )

  if (!is.logical(out) || length(out) != nrow(data)) {
    out <- rep(TRUE, nrow(data))
  }
  out[is.na(out)] <- FALSE
  out
}

apply_subset_expr <- function(data, expr_text) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)
  idx <- safe_eval_in_data(expr_text, data)
  data[idx, , drop = FALSE]
}

# ------------------------------------------------------------
# 10. manifest helpers
# ------------------------------------------------------------
make_file_manifest <- function(files, section = NA_character_, role = NA_character_) {
  files <- unique(as.character(files))
  files <- files[file_exists2(files)]

  if (length(files) == 0) {
    return(data.frame(
      section = character(0),
      role = character(0),
      file_name = character(0),
      file_path = character(0),
      ext = character(0),
      size_bytes = numeric(0),
      modified_time = character(0),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(files)
  data.frame(
    section = section,
    role = role,
    file_name = basename(files),
    file_path = .norm_path2(files),
    ext = tools::file_ext(files),
    size_bytes = info$size,
    modified_time = as.character(info$mtime),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 10-1. validation / fingerprint helpers
# ------------------------------------------------------------
safe_digest_file <- function(path, algo = "md5") {
  path <- as.character(path)[1]

  if (is.na(path) || !nzchar(trimws(path)) || !file.exists(path)) {
    return(NA_character_)
  }

  if (requireNamespace("digest", quietly = TRUE)) {
    out <- tryCatch(
      digest::digest(file = path, algo = algo, serialize = FALSE),
      error = function(e) NA_character_
    )
    if (!is.na(out) && nzchar(out)) return(out)
  }

  finfo <- tryCatch(file.info(path), error = function(e) NULL)
  if (is.null(finfo) || nrow(finfo) == 0) return(NA_character_)

  paste0(
    "fallback:",
    as.character(finfo$size[1]),
    ":",
    as.character(finfo$mtime[1])
  )
}

compare_name_sets <- function(dict_names, data_names) {
  dict_names <- unique(trimws(as.character(dict_names)))
  data_names <- unique(trimws(as.character(data_names)))

  dict_names <- dict_names[!is.na(dict_names) & nzchar(dict_names)]
  data_names <- data_names[!is.na(data_names) & nzchar(data_names)]

  only_in_dict <- setdiff(dict_names, data_names)
  only_in_data <- setdiff(data_names, dict_names)
  in_both      <- intersect(dict_names, data_names)

  list(
    only_in_dict = sort(only_in_dict),
    only_in_data = sort(only_in_data),
    in_both = sort(in_both),
    n_only_in_dict = length(only_in_dict),
    n_only_in_data = length(only_in_data),
    n_in_both = length(in_both)
  )
}

flag_issue_level <- function(n_error = 0, n_warn = 0) {
  n_error <- suppressWarnings(as.integer(n_error)[1])
  n_warn  <- suppressWarnings(as.integer(n_warn)[1])

  if (is.na(n_error)) n_error <- 0L
  if (is.na(n_warn))  n_warn  <- 0L

  if (n_error > 0) return("error")
  if (n_warn > 0) return("warn")
  "ok"
}

make_issue_row <- function(issue_type,
                           var_name = NA_character_,
                           detail = NA_character_,
                           severity = c("info", "warn", "error")) {
  severity <- match.arg(severity)

  data.frame(
    issue_type = as.character(issue_type)[1],
    var_name   = as.character(var_name)[1],
    detail     = as.character(detail)[1],
    severity   = severity,
    stringsAsFactors = FALSE
  )
}

bind_issue_rows <- function(x) {
  if (length(x) == 0) {
    return(data.frame(
      issue_type = character(0),
      var_name   = character(0),
      detail     = character(0),
      severity   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  x <- x[vapply(x, is.data.frame, logical(1))]
  if (length(x) == 0) {
    return(data.frame(
      issue_type = character(0),
      var_name   = character(0),
      detail     = character(0),
      severity   = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- safe_rbind(x)
  rownames(out) <- NULL
  out
}


# ------------------------------------------------------------
# 11. console message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("03_utils.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Utility helpers registered\n")
cat("============================================================\n\n")
# ------------------------------------------------------------
# safe data.frame coercion
# ------------------------------------------------------------
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
