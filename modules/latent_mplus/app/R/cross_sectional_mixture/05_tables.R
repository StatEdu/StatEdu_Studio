# ============================================================
# 05_tables.R
# SCI-style publication tables for cross_sectional_mixture
# ------------------------------------------------------------
# Main:
#   T1  Model selection summary
#   T2  Model fit table
#   T3  Profile size
#   T4  Indicator profile means (wide; twoline)
#   T5  Naive covariate table (twoline)
#   T5b Primary three-step covariate table (twoline)
#   T5c Sensitivity covariate table (twoline)
#   T5d Comparison table
#   T6  BCH outcome table (twoline)
#   T6b Categorical BCH outcome multinomial model
#   T7  Registry summary
#
# Appendix:
#   A3  Raw profile means (wide; compact)
#   A4  Z-scale profile means (wide; compact)
#   A5  Posterior summary
#   A6  Classification bands
#
# Supplement:
#   S1  Overview
#   S2  Continuous auxiliary desc
#   S3  Categorical auxiliary desc
#   S4  Merged summary
#   S5  Primary RRR long table
#   S6  Multinomial detail
# ============================================================

T0_TABLES <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("TABLES", "05_tables.R")
log_info("Reloading previous outputs ...")

# ------------------------------------------------------------
# 1. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

safe_int <- function(x) {
  suppressWarnings(as.integer(as.numeric(x)))
}

safe_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  if (is.matrix(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  if (is.null(out)) out <- data.frame()
  out
}

is_nonempty_df <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}

safe_rbind <- function(x, y) {
  x      <- safe_df(x)
  y      <- safe_df(y)

  nx     <- names(x)
  ny     <- names(y)
  all_nm <- unique(c(nx, ny))

  if (length(all_nm) == 0) return(data.frame())

  for (nm in setdiff(all_nm, nx)) x[[nm]] <- rep(NA, nrow(x))
  for (nm in setdiff(all_nm, ny)) y[[nm]] <- rep(NA, nrow(y))

  x             <- x[, all_nm, drop = FALSE]
  y             <- y[, all_nm, drop = FALSE]

  out           <- rbind(x, y)
  rownames(out) <- NULL
  out
}

safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

normalize_prop_by_source <- function(values, source_name = NA_character_) {
  x <- safe_num(values)
  src <- tolower(as.character(source_name %||% ""))
  if (!nzchar(src)) return(x)
  if (src %in% c("percent", "weighted_percent", "%", "assigned %")) return(x / 100)
  if (src %in% c("prop", "proportion", "pct", "estimated_prop", "weighted_prop")) return(x)
  ifelse(!is.na(x) & x > 1, x / 100, x)
}

as_chr_flat <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x)) {
    x <- vapply(
      x,
      function(z) {
        if (length(z) == 0 || all(is.na(z))) return("")
        as.character(z[[1]])
      },
      character(1)
    )
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

fmt_num                                <- function(x, digits = 3) {
  x                                    <- safe_num(x)
  out                                  <- rep("", length(x))
  ok                                   <- !is.na(x)
  out[ok]                              <- formatC(x[ok], format = "f", digits = digits)
  out
}

fmt_p                                  <- function(x, digits = 3) {
  x                                    <- safe_num(x)
  out                                  <- rep("", length(x))
  ok                                   <- !is.na(x)
  out[ok]                              <- ifelse(x[ok] < .001, "<.001", sub("^0", "", formatC(x[ok], format = "f", digits = digits)))
  out
}

sig_mark                               <- function(p) {
  p                                    <- safe_num(p)
  out                                  <- rep("", length(p))
  out[!is.na(p) & p < .001]            <- "***"
  out[!is.na(p) & p >= .001 & p < .01] <- "**"
  out[!is.na(p) & p >= .01 & p < .05]  <- "*"
  out
}

first_existing_col                     <- function(df, candidates) {
  hit                                  <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

sort_profile_cols <- function(nm) {
  prof            <- grep("^Profile [0-9]+$", nm, value = TRUE)
  prof[order(suppressWarnings(as.integer(gsub("^Profile ", "", prof))))]
}

clean_profile_text <- function(x, prefix = "Profile") {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("^class\\s*:?\\s*", paste0(prefix, " "), x, ignore.case = TRUE)
  x <- gsub("^Class\\s*:?\\s*", paste0(prefix, " "), x, ignore.case = TRUE)
  x <- gsub(paste0("^", prefix, "\\s*([0-9]+)$"), paste0(prefix, " \\1"), x, ignore.case = TRUE)
  x <- gsub("^class([0-9]+)$", paste0(prefix, " \\1"), x, ignore.case = TRUE)
  x <- gsub("^Class([0-9]+)$", paste0(prefix, " \\1"), x, ignore.case = TRUE)
  x
}

extract_model_structure_from_tag <- function(x) {
  x        <- as.character(x)
  out      <- rep(NA_character_, length(x))
  hit      <- grepl("_model[0-9]+_k[0-9]+_", x, ignore.case = TRUE)
  out[hit] <- sub("^.*_(model[0-9]+)_k[0-9]+_.*$", "\\1", x[hit], ignore.case = TRUE)
  tolower(out)
}

extract_k_from_tag <- function(x) {
  suppressWarnings(as.integer(sub(".*_k([0-9]+)_.*", "\\1", x)))
}

extract_class_num_from_text <- function(x) {
  suppressWarnings(as.integer(gsub("^.*?([0-9]+).*$", "\\1", as.character(x))))
}

collapse_repeated_first_col <- function(df, col = "Variable") {
  if (!is.data.frame(df) || nrow(df) == 0 || !col %in% names(df)) return(df)
  x                <- as.character(df[[col]])
  x[duplicated(x)] <- ""
  df[[col]]        <- x
  df
}

make_empty_twoline_rrr <- function() {
  data.frame(
    Variable         = character(),
    Category         = character(),
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
}

make_empty_compare_table <- function() {
  data.frame(
    Comparison       = character(),
    Variable         = character(),
    Category         = character(),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 1-1. project table formatting policy helpers
# ------------------------------------------------------------
fmt_m2 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.2f", x))
}

fmt_sd2 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.2f", x))
}

fmt_n0 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.0f", x))
}

fmt_pct1_plain <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.1f", x))
}

fmt_rrr3 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

fmt_p3_strict <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x), "",
    ifelse(x < .001, "<.001", sub("^0", "", sprintf("%.3f", x)))
  )
}

format_p_value_columns <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0 || ncol(df) == 0) return(df)
  is_p_col <- function(nm) {
    low <- tolower(trimws(as.character(nm)))
    low == "p" ||
      grepl("(^|\\s)p($|\\s)", low) ||
      grepl("^p__", low) ||
      grepl("__p$", low) ||
      grepl("_p$", low)
  }
  p_cols <- names(df)[vapply(names(df), is_p_col, logical(1))]
  for (cc in p_cols) {
    x_chr <- trimws(as.character(df[[cc]]))
    x_chr[is.na(x_chr)] <- ""
    already <- grepl("^<\\s*\\.001$", x_chr)
    x_num <- suppressWarnings(as.numeric(x_chr))
    can_format <- !is.na(x_num)
    out <- x_chr
    out[already] <- "<.001"
    out[can_format] <- ifelse(x_num[can_format] < 0.001, "<.001", sub("^0", "", sprintf("%.3f", x_num[can_format])))
    df[[cc]] <- out
  }
  df
}

fmt_sig_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

has_mixed_indicators <- function() {
  length(SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% SETTINGS_SUMMARY$indicators_continuous %||% character(0)) > 0 &&
    length(SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% SETTINGS_SUMMARY$indicators_categorical %||% character(0)) > 0
}

latent_group_term <- function() {
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") "Class" else "Profile"
}

latent_group_term_lower <- function() tolower(latent_group_term())

latent_group_label <- function(x) {
  paste0(latent_group_term(), " ", suppressWarnings(as.integer(x)))
}

normalize_latent_group_text <- function(x) {
  clean_profile_text(x, prefix = latent_group_term())
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

normalize_latent_group_table_display <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0 && ncol(df) == 0) return(df)

  names(df) <- normalize_latent_group_display(names(df))
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
    names(df)[names(df) == "Profile"] <- "Class"
    names(df)[names(df) == "Profiles"] <- "Classes"
  } else {
    names(df)[names(df) == "Class"] <- "Profile"
    names(df)[names(df) == "Classes"] <- "Profiles"
  }

  for (cc in names(df)) {
    if (is.character(df[[cc]]) || is.factor(df[[cc]])) {
      df[[cc]] <- normalize_latent_group_display(as.character(df[[cc]]))
    }
  }
  df
}

latent_analysis_type <- function() {
  if (isTRUE(has_mixed_indicators())) return("Mixed model")
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") return("LCA")
  "LPA"
}

COMPACT_PM <- intToUtf8(177)

compact_mean_name <- function(weighted = FALSE) {
  paste("M", COMPACT_PM, if (isTRUE(weighted)) "SE" else "SD")
}

compact_mean_cols <- function() {
  c(compact_mean_name(FALSE), compact_mean_name(TRUE))
}

fmt_mean_sd_cell <- function(m, sd) {
  ifelse(
    is.na(suppressWarnings(as.numeric(m))),
    "",
    paste0(fmt_m2(m), " ", COMPACT_PM, " ", fmt_sd2(sd))
  )
}

fmt_n_pct_cell <- function(n, pct) {
  ifelse(
    is.na(suppressWarnings(as.numeric(n))),
    "",
    paste0(fmt_n0(n), " (", fmt_pct1_plain(pct), ")")
  )
}

fmt_rrr_ci_cell <- function(rrr, llci, ulci) {
  ifelse(
    is.na(suppressWarnings(as.numeric(rrr))),
    "",
    paste0(fmt_rrr3(rrr), " (", fmt_rrr3(llci), "~", fmt_rrr3(ulci), ")")
  )
}

ensure_compact_as_table <- function(df, table_name = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  nm <- names(df)
  mean_cols <- compact_mean_cols()

  # already compact
  if (any(nm %in% c(mean_cols, "n (%)", "RRR (LLCI~ULCI)"))) {
    if ("p" %in% names(df)) {
      df$p                  <- as.character(df$p)
      df$p[is.na(df$p)]     <- ""
    }
    if ("sig" %in% names(df)) {
      df$sig                <- as.character(df$sig)
      df$sig[is.na(df$sig)] <- ""
    }
    return(df)
  }

  # A3 / A4 : wide profile means -> create one compact anchor column
  prof_cols <- grep("^Profile [0-9]+$", nm, value = TRUE)
  if (!is.null(table_name) && table_name %in% c("A3", "A4") && length(prof_cols) > 0 &&
      !("Category" %in% names(df))) {
    df[[compact_mean_name(FALSE)]] <- as.character(df[[prof_cols[1]]])
  }

  # S1 : overview table -> use Value as compact anchor
  if (!is.null(table_name) && table_name %in% c("S1") && "Value" %in% names(df) && !compact_mean_name(FALSE) %in% names(df)) {
    df[[compact_mean_name(FALSE)]] <- as.character(df$Value)
  }

  # regular compact creation
  if (!any(names(df) %in% c(mean_cols, "n (%)", "RRR (LLCI~ULCI)"))) {
    if (all(c("M", "SD") %in% names(df))) {
      df[[compact_mean_name(FALSE)]] <- fmt_mean_sd_cell(df$M, df$SD)
    } else if (all(c("n", "pct") %in% names(df))) {
      df[["n (%)"]] <- fmt_n_pct_cell(df$n, df$pct)
    } else if (all(c("n", "%") %in% names(df))) {
      df[["n (%)"]] <- fmt_n_pct_cell(df$n, df[["%"]])
    } else if (all(c("RRR", "LLCI", "ULCI") %in% names(df))) {
      df[["RRR (LLCI~ULCI)"]] <- fmt_rrr_ci_cell(df$RRR, df$LLCI, df$ULCI)
    }
  }

  # S5 / S6 : p__*, sig__* wide outputs -> create explicit compact anchor
  if (!any(names(df) %in% c(mean_cols, "n (%)", "RRR (LLCI~ULCI)"))) {
    rrr_like <- setdiff(
      names(df)[grepl("^p__", names(df)) == FALSE & grepl("^sig__", names(df)) == FALSE],
      c("Variable", "Category")
    )
    if (!is.null(table_name) && table_name %in% c("S5", "S6") && length(rrr_like) > 0) {
      df[["RRR (LLCI~ULCI)"]] <- as.character(df[[rrr_like[1]]])
    }
  }

  if ("p" %in% names(df)) {
    df$p <- as.character(df$p)
    df$p[is.na(df$p)] <- ""
  }
  if ("sig" %in% names(df)) {
    df$sig <- as.character(df$sig)
    df$sig[is.na(df$sig)] <- ""
  }

  df
}

postprocess_table_output <- function(df, table_name = NULL) {
  df <- safe_df(df)
  rownames(df) <- NULL

  drop_fully_blank_rows_local <- function(x) {
    x <- safe_df(x)
    if (nrow(x) == 0 || ncol(x) == 0) return(x)
    keep <- apply(x, 1, function(r) any(nzchar(trimws(as.character(r)))))
    x[keep, , drop = FALSE]
  }

  if (!is.null(table_name) && grepl("^[AS][0-9]", table_name) && !table_name %in% c("S5")) {
    df <- ensure_compact_as_table(df, table_name = table_name)
  }

  df <- drop_fully_blank_rows_local(df)
  df <- normalize_latent_group_table_display(df)
  df <- format_p_value_columns(df)

  df
}

# ------------------------------------------------------------
# 1-2. generic twoline helpers
# ------------------------------------------------------------
twoline_key <- function(stat, group) {
  paste0(stat, "__", group)
}

extract_twoline_parts <- function(nm) {
  sp <- strsplit(as.character(nm), "__", fixed = TRUE)[[1]]
  list(
    stat = sp[1],
    group = paste(sp[-1], collapse = "__")
  )
}

order_twoline_groups <- function(groups) {
  groups <- unique(as.character(groups))

  prof_num <- suppressWarnings(as.integer(gsub("^Profile ([0-9]+)$", "\\1", groups)))
  if (all(!is.na(prof_num))) {
    return(groups[order(prof_num)])
  }

  cls_num <- suppressWarnings(as.integer(gsub("^Class ([0-9]+).*$", "\\1", groups)))
  if (all(!is.na(cls_num))) {
    return(groups[order(cls_num, groups)])
  }

  groups
}

shorten_comparison_label <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("^Class\\s+([0-9]+)\\s+vs\\s+Class\\s+([0-9]+)$", "C\\1 vs C\\2", x, ignore.case = TRUE)
  x <- gsub("^Profile\\s+([0-9]+)\\s+vs\\s+Profile\\s+([0-9]+)$", "P\\1 vs P\\2", x, ignore.case = TRUE)
  x
}

# ------------------------------------------------------------
# 1-3. A-series compact helpers
# ------------------------------------------------------------
normalize_a_table_names <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  nm <- names(df)

  if (!"Variable" %in% nm) {
    cand <- intersect(c("var_label", "variable", "Label", "label"), nm)
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- "Variable"
  }

  if (!"Category" %in% names(df)) {
    cand <- intersect(c("level", "value_label", "group", "Category"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- "Category"
  }

  if ("M_SD" %in% names(df)) names(df)[match("M_SD", names(df))] <- compact_mean_name(FALSE)
  if ("Mean_SD" %in% names(df)) names(df)[match("Mean_SD", names(df))] <- compact_mean_name(FALSE)
  if ("n_pct" %in% names(df)) names(df)[match("n_pct", names(df))] <- "n (%)"
  if ("N_PCT" %in% names(df)) names(df)[match("N_PCT", names(df))] <- "n (%)"
  if ("RRR_CI" %in% names(df)) names(df)[match("RRR_CI", names(df))] <- "RRR (LLCI~ULCI)"
  if ("RRR_LLCI_ULCI" %in% names(df)) names(df)[match("RRR_LLCI_ULCI", names(df))] <- "RRR (LLCI~ULCI)"

  if ("p_value" %in% names(df)) names(df)[match("p_value", names(df))] <- "p"
  if ("p_fmt" %in% names(df))   names(df)[match("p_fmt", names(df))]   <- "p"
  if ("stars" %in% names(df))   names(df)[match("stars", names(df))]   <- "sig"

  df
}

order_a_table_cols <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  preferred <- c(
    "Variable",
    "Category",
    compact_mean_name(FALSE),
    compact_mean_name(TRUE),
    "n (%)",
    "RRR (LLCI~ULCI)",
    "p",
    "sig"
  )

  keep <- c(preferred[preferred %in% names(df)],
            setdiff(names(df), preferred))

  df[, keep, drop = FALSE]
}

format_a_mean_table <- function(df,
                                group_col = "Profile",
                                value_col = "Mean",
                                spread_col = "SD",
                                first_col = "Variable") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if (!first_col %in% names(df)) {
    cand <- intersect(c("var_label", "variable", "Label"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- first_col
  }

  df$cell <- fmt_mean_sd_cell(df[[value_col]], df[[spread_col]])

  out <- reshape(
    df[, c(first_col, group_col, "cell"), drop = FALSE],
    idvar     = first_col,
    timevar   = group_col,
    direction = "wide"
  )

  names(out) <- sub("^cell\\.", "", names(out))
  out <- normalize_a_table_names(out)
  out <- order_a_table_cols(out)
  out
}

format_a_npct_table <- function(df,
                                group_col = "Profile",
                                n_col = "n",
                                pct_col = "pct",
                                first_col = "Variable") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if (!first_col %in% names(df)) {
    cand <- intersect(c("var_label", "variable", "Label"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- first_col
  }

  df$cell <- fmt_n_pct_cell(df[[n_col]], df[[pct_col]])

  out <- reshape(
    df[, c(first_col, group_col, "cell"), drop = FALSE],
    idvar     = first_col,
    timevar   = group_col,
    direction = "wide"
  )

  names(out) <- sub("^cell\\.", "", names(out))
  out <- normalize_a_table_names(out)
  out <- order_a_table_cols(out)
  out
}

format_a_rrr_table <- function(df,
                               group_col = "Comparison",
                               rrr_col = "RRR",
                               llci_col = "LLCI",
                               ulci_col = "ULCI",
                               p_col = "p",
                               sig_col = "sig",
                               first_col = "Variable") {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if (!first_col %in% names(df)) {
    cand <- intersect(c("var_label", "variable", "Label"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- first_col
  }

  if (!"Category" %in% names(df)) {
    df$Category <- ""
  }

  id_cols    <- c(first_col, "Category")

  df$cell    <- fmt_rrr_ci_cell(df[[rrr_col]], df[[llci_col]], df[[ulci_col]])
  df$p_out   <- fmt_p3_strict(df[[p_col]])
  df$sig_out <- fmt_sig_cell(df[[sig_col]])

  # --------------------------------------------------
  # FINAL FIX: reshape ??????????獄쏅챶留덌┼??????????????筌롈살젔??Comparison key ???????椰????????????
  # ???????椰??????????????????⑤벡???????????????獄쏅챶留덌┼??????????????筌롈살젔?? df$cell / df$p_out / df$sig_out ????????????????
  #              wide_main <- reshape(...) ??????濾?????????
  # --------------------------------------------------
  if (group_col %in% names(df)) {
    df[[group_col]] <- trimws(as.character(df[[group_col]]))

    # ????????????????????????
    df <- df[
      !grepl("^Class\\s*([0-9]+)\\s*vs\\s*Class\\s*\\1$", df[[group_col]]),
      ,
      drop = FALSE
    ]
  }

  # reshape key ???????????롪퍓梨????????????????????????????????
  dedup_key <- c(id_cols, group_col)
  dedup_key <- dedup_key[dedup_key %in% names(df)]

  if (length(dedup_key) > 0) {
    df <- df[!duplicated(df[, dedup_key, drop = FALSE]), , drop = FALSE]
  }

  wide_main  <- reshape(
    df[, c(id_cols, group_col, "cell"), drop = FALSE],
    idvar     = id_cols,
    timevar   = group_col,
    direction = "wide"
  )
  names(wide_main) <- sub("^cell\\.", "", names(wide_main))

  p_df   <- unique(df[, c(id_cols, group_col, "p_out", "sig_out"), drop = FALSE])

  wide_p <- reshape(
    p_df[, c(id_cols, group_col, "p_out"), drop = FALSE],
    idvar     = id_cols,
    timevar   = group_col,
    direction = "wide"
  )
  names(wide_p) <- sub("^p_out\\.", "p__", names(wide_p))

  wide_sig <- reshape(
    p_df[, c(id_cols, group_col, "sig_out"), drop = FALSE],
    idvar     = id_cols,
    timevar   = group_col,
    direction = "wide"
  )
  names(wide_sig) <- sub("^sig_out\\.", "sig__", names(wide_sig))

  out <- merge(wide_main, wide_p, by = id_cols, all = TRUE, sort = FALSE)
  out <- merge(out, wide_sig, by = id_cols, all = TRUE, sort = FALSE)

  out <- normalize_a_table_names(out)
  out <- order_a_table_cols(out)
  out
}

set_safe_colwidths <- function(wb, sheet_name, df) {
  ncol_df <- ncol(df)
  if (ncol_df == 0) return(invisible(NULL))

  widths <- rep(12, ncol_df)   # ?????????????????濾????????????

  nm <- names(df)

  for (j in seq_len(ncol_df)) {
    colname <- nm[j]

    # ????????????꿔꺂???⑸븶????????(Variable / Profile)
    if (j == 1) {
      widths[j] <- 24
    }

    # ???Category
    if (grepl("Category", colname, ignore.case = TRUE)) {
      widths[j] <- 18
    }

    # ???n, %, p
    if (colname %in% c("n", "%", "p")) {
      widths[j] <- 10
    }

    # ???sig
    if (grepl("sig", colname, ignore.case = TRUE)) {
      widths[j] <- 8
    }

    # ???RRR / CI
    if (grepl("RRR|LLCI|ULCI", colname)) {
      widths[j] <- 12
    }

    # ???twoline ?????
    if (grepl("__", colname)) {
      widths[j] <- 11
    }
  }

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol_df),
    widths = widths
  )
}

# ------------------------------------------------------------
# 2. reload current pipeline objects
# ------------------------------------------------------------
FIT_SUMMARY          <- load_step_rds("FIT_SUMMARY",          dir_rds = DIR_RDS, default = data.frame())
ESTIMATION_REGISTRY  <- load_step_rds("ESTIMATION_REGISTRY",  dir_rds = DIR_RDS, default = data.frame())
CLASS_SUMMARY_FINAL  <- load_step_rds("CLASS_SUMMARY_FINAL",  dir_rds = DIR_RDS, default = data.frame())
CLASS_SUMMARY        <- load_step_rds("CLASS_SUMMARY",        dir_rds = DIR_RDS, default = data.frame())
T3_indicator_profile <- load_step_rds("T3_indicator_profile", dir_rds = DIR_RDS, default = data.frame())
T3_indicator_profile_z <- load_step_rds("T3_indicator_profile_z", dir_rds = DIR_RDS, default = data.frame())
R3STEP_RESULTS_RAW   <- load_step_rds("R3STEP_RESULTS",       dir_rds = DIR_RDS, default = list())
T5_RRR_RAW           <- load_step_rds("T5_rrr",               dir_rds = DIR_RDS, default = data.frame())
BCH_RESULTS          <- load_step_rds("BCH_RESULTS",          dir_rds = DIR_RDS, default = data.frame())
BCH_RESULTS_FULL     <- load_step_rds("BCH_RESULTS_FULL",     dir_rds = DIR_RDS, default = data.frame())
BCH_POSTHOC          <- load_step_rds("BCH_POSTHOC",          dir_rds = DIR_RDS, default = data.frame())
BCH_OMNIBUS_BASIC    <- load_step_rds("BCH_OMNIBUS_BASIC",    dir_rds = DIR_RDS, default = data.frame())
BCH_MOD_RESULTS_FULL <- load_step_rds("BCH_MOD_RESULTS_FULL", dir_rds = DIR_RDS, default = data.frame())
BCH_MOD_POSTHOC      <- load_step_rds("BCH_MOD_POSTHOC",      dir_rds = DIR_RDS, default = data.frame())
BCH_MOD_OMNIBUS      <- load_step_rds("BCH_MOD_OMNIBUS",      dir_rds = DIR_RDS, default = data.frame())
BCH_STRATIFIED_RESULTS_FULL <- load_step_rds("BCH_STRATIFIED_RESULTS_FULL", dir_rds = DIR_RDS, default = data.frame())
BCH_STRATIFIED_POSTHOC      <- load_step_rds("BCH_STRATIFIED_POSTHOC",      dir_rds = DIR_RDS, default = data.frame())
BCH_STRATIFIED_OMNIBUS      <- load_step_rds("BCH_STRATIFIED_OMNIBUS",      dir_rds = DIR_RDS, default = data.frame())
BCH_INTERACTION      <- load_step_rds("BCH_INTERACTION",      dir_rds = DIR_RDS, default = data.frame())
BCH_OUTCOME_SPEC     <- load_step_rds("BCH_OUTCOME_SPEC",     dir_rds = DIR_RDS, default = data.frame())
CLASSIFIED_ANALYSIS  <- load_step_rds("CLASSIFIED_ANALYSIS",  dir_rds = DIR_RDS, default = data.frame())
ANALYSIS_DATA_CLASSIFIED <- load_step_rds("ANALYSIS_DATA_CLASSIFIED", dir_rds = DIR_RDS, default = data.frame())
if ((!is.data.frame(BCH_INTERACTION) || nrow(BCH_INTERACTION) == 0) &&
    exists("DIR_TABLES") && !is.null(DIR_TABLES)) {
  path_bch_inter_csv <- file.path(DIR_TABLES, "bch_interaction.csv")
  if (file.exists(path_bch_inter_csv)) {
    BCH_INTERACTION <- tryCatch(
      utils::read.csv(path_bch_inter_csv, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) data.frame()
    )
  }
}
BEST_K_SUMMARY       <- load_step_rds("BEST_K_SUMMARY",       dir_rds = DIR_RDS, default = list())
BEST_MODEL_ROW       <- load_step_rds("BEST_MODEL_ROW",       dir_rds = DIR_RDS, default = data.frame())
CLASSIFY_SUMMARY     <- load_step_rds("CLASSIFY_SUMMARY",     dir_rds = DIR_RDS, default = list())
CLASSIFICATION_QUALITY <- load_step_rds(
  "CLASSIFICATION_QUALITY",
  dir_rds = DIR_RDS,
  default = data.frame()
)

POSTERIOR_MAX_DF <- load_step_rds(
  "POSTERIOR_MAX_DF",
  dir_rds = DIR_RDS,
  default = data.frame()
)
RAW_DATA <- load_step_rds("RAW_DATA", dir_rds = DIR_RDS, default = data.frame())
ANALYSIS_DATA_SUB <- load_step_rds("ANALYSIS_DATA_SUB", dir_rds = DIR_RDS, default = data.frame())
SETTINGS_SUMMARY     <- load_step_rds("SETTINGS_SUMMARY",     dir_rds = DIR_RDS, default = list())
DICT                 <- load_step_rds("DICT",                 dir_rds = DIR_RDS, default = list())
if (exists("PATH_DICT") && file.exists(PATH_DICT)) {
  dict_file_current <- tryCatch(
    utils::read.csv(PATH_DICT, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
  if (is.data.frame(dict_file_current) && nrow(dict_file_current) > 0) {
    if (is.list(DICT)) {
      DICT$meta <- dict_file_current
    } else {
      DICT <- list(meta = dict_file_current)
    }
  }
}
R3STEP_PARSE_DEBUG   <- load_step_rds("R3STEP_PARSE_DEBUG",   dir_rds = DIR_RDS, default = data.frame())

DICT_META   <- if (is.list(DICT) && is.data.frame(DICT$meta)) DICT$meta else data.frame()
DICT_LEVELS <- if (is.list(DICT) && is.data.frame(DICT$levels)) DICT$levels else data.frame()

best_k            <- as.integer(BEST_K_SUMMARY$best_k %||% BEST_K_SUMMARY$BEST_K %||% NA_integer_)
best_tag          <- as.character(BEST_K_SUMMARY$best_tag %||% BEST_K_SUMMARY$BEST_TAG %||% NA_character_)
model_structure   <- tolower(as.character(
  BEST_K_SUMMARY$best_model_structure %||%
    BEST_K_SUMMARY$BEST_MODEL_STRUCTURE %||%
    if (is.data.frame(BEST_MODEL_ROW) && nrow(BEST_MODEL_ROW) > 0 && "model_structure" %in% names(BEST_MODEL_ROW)) BEST_MODEL_ROW$model_structure[1] else NA_character_
))
if (is.na(model_structure) || !nzchar(model_structure)) {
  model_structure <- extract_model_structure_from_tag(best_tag)
}

if (!exists("T3_indicator_profile")) T3_indicator_profile <- data.frame()
if (!exists("T3_indicator_profile_z")) T3_indicator_profile_z <- data.frame()
if (!exists("TABLE_A4_Z")) TABLE_A4_Z <- data.frame()
if (!exists("A4_indicator_profile_z")) A4_indicator_profile_z <- data.frame()
if (!exists("T5_RRR_RAW")) T5_RRR_RAW <- data.frame()
if (!exists("R3STEP_RESULTS_RAW")) R3STEP_RESULTS_RAW <- list()
if (!exists("BCH_RESULTS")) BCH_RESULTS <- data.frame()
if (!exists("BCH_RESULTS_FULL")) BCH_RESULTS_FULL <- data.frame()
if (!exists("BCH_POSTHOC")) BCH_POSTHOC <- data.frame()
if (!exists("BCH_OMNIBUS_BASIC")) BCH_OMNIBUS_BASIC <- data.frame()
if (!exists("BCH_MOD_RESULTS_FULL")) BCH_MOD_RESULTS_FULL <- data.frame()
if (!exists("BCH_MOD_POSTHOC")) BCH_MOD_POSTHOC <- data.frame()
if (!exists("BCH_MOD_OMNIBUS")) BCH_MOD_OMNIBUS <- data.frame()
if (!exists("BCH_STRATIFIED_RESULTS")) BCH_STRATIFIED_RESULTS <- data.frame()
if (!exists("BCH_STRATIFIED_RESULTS_FULL")) BCH_STRATIFIED_RESULTS_FULL <- data.frame()
if (!exists("BCH_STRATIFIED_POSTHOC")) BCH_STRATIFIED_POSTHOC <- data.frame()
if (!exists("BCH_STRATIFIED_OMNIBUS")) BCH_STRATIFIED_OMNIBUS <- data.frame()


filter_retained_profile <- function(df, best_k, best_tag = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)



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

  if ("outcome_class" %in% names(df)) {
    oc_ids <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(df$outcome_class))))
    keep_oc <- is.na(oc_ids) | oc_ids <= as.integer(best_k)
    df <- df[keep_oc, , drop = FALSE]
  }

  if ("reference_class" %in% names(df)) {
    rc_ids <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(df$reference_class))))
    keep_rc <- is.na(rc_ids) | rc_ids <= as.integer(best_k)
    df <- df[keep_rc, , drop = FALSE]
  }

  if ("comparison" %in% names(df)) {
    cmp_ids <- extract_class_ids_from_text(df$comparison)
    keep_cmp <- vapply(cmp_ids, function(ids) length(ids) == 0 || all(is.na(ids) | ids <= as.integer(best_k)), logical(1))
    df <- df[keep_cmp, , drop = FALSE]
  }

  if ("Comparison" %in% names(df)) {
    cmp_ids <- extract_class_ids_from_text(df$Comparison)
    keep_cmp <- vapply(cmp_ids, function(ids) length(ids) == 0 || all(is.na(ids) | ids <= as.integer(best_k)), logical(1))
    df <- df[keep_cmp, , drop = FALSE]
  }

  rownames(df) <- NULL
  df
}

filter_retained_solution <- function(x, best_k, best_tag = NULL) {
  if (is.data.frame(x)) return(filter_retained_solution_df(x, best_k = best_k, best_tag = best_tag))
  if (is.list(x)) return(lapply(x, filter_retained_solution, best_k = best_k, best_tag = best_tag))
  x
}

T3_indicator_profile <- filter_retained_profile(
  T3_indicator_profile,
  best_k   = best_k,
  best_tag = best_tag
)
T3_indicator_profile_z <- filter_retained_profile(T3_indicator_profile_z, best_k = best_k, best_tag = best_tag)
TABLE_A4_Z <- filter_retained_profile(TABLE_A4_Z, best_k = best_k, best_tag = best_tag)
A4_indicator_profile_z <- filter_retained_profile(A4_indicator_profile_z, best_k = best_k, best_tag = best_tag)
T5_RRR_RAW <- filter_retained_solution(T5_RRR_RAW, best_k = best_k, best_tag = best_tag)
R3STEP_RESULTS_RAW <- filter_retained_solution(R3STEP_RESULTS_RAW, best_k = best_k, best_tag = best_tag)
BCH_RESULTS <- filter_retained_solution(BCH_RESULTS, best_k = best_k, best_tag = best_tag)
BCH_RESULTS_FULL <- filter_retained_solution(BCH_RESULTS_FULL, best_k = best_k, best_tag = best_tag)
BCH_POSTHOC <- filter_retained_solution(BCH_POSTHOC, best_k = best_k, best_tag = best_tag)
BCH_OMNIBUS_BASIC <- filter_retained_solution(BCH_OMNIBUS_BASIC, best_k = best_k, best_tag = best_tag)
BCH_MOD_RESULTS_FULL <- filter_retained_solution(BCH_MOD_RESULTS_FULL, best_k = best_k, best_tag = best_tag)
BCH_MOD_POSTHOC <- filter_retained_solution(BCH_MOD_POSTHOC, best_k = best_k, best_tag = best_tag)
BCH_MOD_OMNIBUS <- filter_retained_solution(BCH_MOD_OMNIBUS, best_k = best_k, best_tag = best_tag)
BCH_STRATIFIED_RESULTS <- filter_retained_solution(BCH_STRATIFIED_RESULTS, best_k = best_k, best_tag = best_tag)
BCH_STRATIFIED_RESULTS_FULL <- filter_retained_solution(BCH_STRATIFIED_RESULTS_FULL, best_k = best_k, best_tag = best_tag)
BCH_STRATIFIED_POSTHOC <- filter_retained_solution(BCH_STRATIFIED_POSTHOC, best_k = best_k, best_tag = best_tag)
BCH_STRATIFIED_OMNIBUS <- filter_retained_solution(BCH_STRATIFIED_OMNIBUS, best_k = best_k, best_tag = best_tag)

mixture_mode <- tolower(
  CLASSIFY_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$mixture_type %||%
    SETTINGS_SUMMARY$MIXTURE_TYPE %||%
    "lpa"
)

cfg_survey <- if (exists("CFG") && is.list(CFG) && is.list(CFG$survey_design)) CFG$survey_design else list()
survey_case_chr <- tolower(as.character(
  SETTINGS_SUMMARY$survey_case %||%
    SETTINGS_SUMMARY$SURVEY_CASE %||%
    cfg_survey$survey_case %||%
    "none"
))
weight_var_chr <- as.character(
  SETTINGS_SUMMARY$weight_var %||%
    SETTINGS_SUMMARY$WEIGHT_VAR %||%
    cfg_survey$weight_var %||%
    ""
)
weight_var_chr[is.na(weight_var_chr)] <- ""
HAS_WEIGHT <- any(nzchar(trimws(weight_var_chr))) || grepl("weight", survey_case_chr, fixed = TRUE)

if (!exists("PATH_FINAL_EXCEL") || is.null(PATH_FINAL_EXCEL) || !nzchar(as.character(PATH_FINAL_EXCEL))) {
  PATH_FINAL_EXCEL <- file.path(DIR_OUTPUT, paste0(DATASET_ID, "_", ANALYSIS_ID, "_final_results.xlsx"))
}

# ------------------------------------------------------------
# 3. dictionary-driven helpers
# ------------------------------------------------------------
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

get_display_order <- function(v) {
  v <- as.character(v)

  if (!is.data.frame(DICT_META) || nrow(DICT_META) == 0 || !"var_name" %in% names(DICT_META)) {
    return(seq_along(v))
  }

  if ("display_order" %in% names(DICT_META)) {
    ord_map <- DICT_META[, c("var_name", "display_order"), drop = FALSE]
    ord_map <- ord_map[!duplicated(ord_map$var_name), , drop = FALSE]

    out <- suppressWarnings(as.numeric(ord_map$display_order[match(v, ord_map$var_name)]))
    miss <- is.na(out)
    if (any(miss)) out[miss] <- 999999 + seq_len(sum(miss))
    return(out)
  }

  out <- match(v, DICT_META$var_name)
  out[is.na(out)] <- 999999 + seq_len(sum(is.na(out)))
  out
}

get_category_order <- function(var, level = NULL, label = NULL) {
  var <- as.character(var)
  level <- as.character(level %||% "")
  label <- as.character(label %||% "")

  out <- rep(999999, length(var))

  if (is.data.frame(DICT_LEVELS) &&
      nrow(DICT_LEVELS) > 0 &&
      all(c("var_name", "value") %in% names(DICT_LEVELS))) {

    lvl <- DICT_LEVELS
    lvl$var_name_chr <- as.character(lvl$var_name)
    lvl$value_chr <- as.character(lvl$value)
    lvl$..ord.. <- ave(seq_len(nrow(lvl)), lvl$var_name_chr, FUN = seq_along)

    for (i in seq_along(var)) {
      hit <- lvl[lvl$var_name_chr == var[i], , drop = FALSE]
      if (nrow(hit) == 0) next

      if (!is.na(level[i]) && nzchar(trimws(level[i]))) {
        hit2 <- hit[as.character(hit$value_chr) == as.character(level[i]), , drop = FALSE]
        if (nrow(hit2) > 0) {
          out[i] <- hit2$..ord..[1]
          next
        }
      }

      lab_cols <- intersect(c("label", "value_label", "label_en", "label_ko"), names(hit))
      if (length(lab_cols) > 0 && !is.na(label[i]) && nzchar(trimws(label[i]))) {
        for (cc in lab_cols) {
          hit2 <- hit[as.character(hit[[cc]]) == as.character(label[i]), , drop = FALSE]
          if (nrow(hit2) > 0) {
            out[i] <- hit2$..ord..[1]
            break
          }
        }
      }
    }
  }

  miss <- is.na(out) | out >= 999999
  if (any(miss)) {
    lev_num <- suppressWarnings(as.numeric(level))
    out[miss & !is.na(lev_num)] <- lev_num[miss & !is.na(lev_num)]
  }

  out
}

get_value_label <- function(var, val) {
  var <- trimws(as.character(var))
  val_chr <- trimws(as.character(val))

  norm_key <- function(x) {
    x_chr <- trimws(as.character(x))
    x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
    x_num <- suppressWarnings(as.numeric(x_chr))
    out <- ifelse(is.na(x_num), x_chr, formatC(x_num, format = "fg", digits = 15))
    trimws(out)
  }

  val_key <- norm_key(val_chr)

  # 1) DICT_META??value_k / label_k ?????????
  if (is.data.frame(DICT_META) &&
      nrow(DICT_META) > 0 &&
      "var_name" %in% names(DICT_META)) {

    meta_hit <- DICT_META[trimws(as.character(DICT_META$var_name)) == var, , drop = FALSE]

    if (nrow(meta_hit) > 0) {
      value_cols <- grep("^value_[0-9]+$", names(meta_hit), value = TRUE)
      label_cols <- grep("^label_[0-9]+$", names(meta_hit), value = TRUE)

      idxs <- sort(unique(c(
        suppressWarnings(as.integer(gsub("^value_([0-9]+)$", "\\1", value_cols))),
        suppressWarnings(as.integer(gsub("^label_([0-9]+)$", "\\1", label_cols)))
      )))
      idxs <- idxs[!is.na(idxs)]

      for (k in idxs) {
        vcol <- paste0("value_", k)
        lcol <- paste0("label_", k)

        meta_val <- if (vcol %in% names(meta_hit)) norm_key(meta_hit[[vcol]][1]) else NA_character_
        meta_lab <- if (lcol %in% names(meta_hit)) as.character(meta_hit[[lcol]][1]) else NA_character_

        if (!is.na(meta_val) && identical(meta_val, val_key)) {
          if (!is.na(meta_lab) && nzchar(trimws(meta_lab))) return(trimws(meta_lab))
          if (vcol %in% names(meta_hit)) {
            vv <- as.character(meta_hit[[vcol]][1])
            if (!is.na(vv) && nzchar(trimws(vv))) return(trimws(vv))
          }
        }
      }
    }
  }

  # 2) DICT_LEVELS fallback
  if (is.data.frame(DICT_LEVELS) &&
      nrow(DICT_LEVELS) > 0 &&
      all(c("var_name", "value") %in% names(DICT_LEVELS))) {

    lvl <- DICT_LEVELS
    lvl$var_name_chr <- trimws(as.character(lvl$var_name))
    lvl$value_key <- norm_key(lvl$value)

    hit <- lvl[lvl$var_name_chr == var & lvl$value_key == val_key, , drop = FALSE]

    if (nrow(hit) > 0) {
      cand_cols <- intersect(c("label", "value_label", "label_en", "label_ko", "value"), names(hit))
      for (cc in cand_cols) {
        lab <- as.character(hit[[cc]][1])
        if (!is.na(lab) && nzchar(trimws(lab))) return(trimws(lab))
      }
    }
  }

  val_chr
}


format_variable_category_block <- function(
    df,
    var_col = "var_name",
    var_label_col = NULL,
    cat_col = "Category",
    level_col = NULL
) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  if (!var_col %in% names(df)) return(df)

  var_raw <- as.character(df[[var_col]])

  if (!is.null(var_label_col) && var_label_col %in% names(df)) {
    var_lab <- as.character(df[[var_label_col]])
    miss <- is.na(var_lab) | !nzchar(trimws(var_lab))
    var_lab[miss] <- get_var_label(var_raw[miss])
  } else {
    var_lab <- get_var_label(var_raw)
  }

  cat_txt <- if (!is.null(cat_col) && cat_col %in% names(df)) as.character(df[[cat_col]]) else rep("", nrow(df))
  cat_txt[is.na(cat_txt)] <- ""

  level_txt <- if (!is.null(level_col) && level_col %in% names(df)) as.character(df[[level_col]]) else rep("", nrow(df))

  ord_var <- get_display_order(var_raw)
  ord_cat <- get_category_order(var_raw, level = level_txt, label = cat_txt)
  ord_cat[is.na(ord_cat)] <- 999999

  df$..var_raw.. <- var_raw
  df$..Variable.. <- var_lab
  df$..Category.. <- cat_txt

  df <- df[order(ord_var, df$..var_raw.., ord_cat), , drop = FALSE]
  rownames(df) <- NULL

  dup_var <- duplicated(df$..var_raw..)
  df$..Variable..[dup_var] <- ""

  df$Variable <- df$..Variable..
  if (!is.null(cat_col) && cat_col %in% names(df)) df[[cat_col]] <- df$..Category..

  df$..var_raw.. <- NULL
  df$..Variable.. <- NULL
  df$..Category.. <- NULL
  df
}

# ------------------------------------------------------------
# 4. reference class selection
# ------------------------------------------------------------
resolve_reference_class <- function(class_summary_df = CLASS_SUMMARY_FINAL,
                                    classify_summary = CLASSIFY_SUMMARY) {
  df <- safe_df(class_summary_df)
  fallback_ref <- latent_group_label(1)

  if (nrow(df) == 0) return(fallback_ref)

  class_col <- first_existing_col(df, c("class", "Class", "profile", "Profile"))
  if (is.na(class_col)) return(fallback_ref)

  prop_col <- first_existing_col(df, c(
    "weighted_prop", "estimated_prop", "prop", "proportion", "pct", "percent", "Assigned %"
  ))

  avepp_col <- first_existing_col(df, c(
    "avePP", "AvePP", "avg_pp", "average_pp", "mean_pp", "posterior_mean", "posterior_prob_mean"
  ))

  out <- data.frame(
    class_num = extract_class_num_from_text(df[[class_col]]),
    prop = if (!is.na(prop_col)) safe_num(df[[prop_col]]) else NA_real_,
    avepp = if (!is.na(avepp_col)) safe_num(df[[avepp_col]]) else NA_real_,
    stringsAsFactors = FALSE
  )

  out$prop[is.na(out$prop)] <- -Inf
  out$avepp[is.na(out$avepp)] <- -Inf
  out$class_num[is.na(out$class_num)] <- 999999

  out <- out[order(-out$prop, -out$avepp, out$class_num), , drop = FALSE]
  if (nrow(out) == 0) return(fallback_ref)

  paste0("Class ", out$class_num[1])
}

REFERENCE_CLASS <- resolve_reference_class(CLASS_SUMMARY_FINAL, CLASSIFY_SUMMARY)

# ------------------------------------------------------------
# 4-1. caption / note helpers
# ------------------------------------------------------------
get_reference_class_label <- function(x = REFERENCE_CLASS) {
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) return(latent_group_label(1))
  normalize_latent_group_text(x)
}

make_table_note <- function(
    type = c("mean_sd", "m_se", "n_pct", "weighted_n_pct", "weighted_n_pct_app_occ", "pct_psig", "rrr", "rrr_compact", "mean_sd_compact", "mean_se_compact", "mixed_split", "generic"),
    reference_class = REFERENCE_CLASS
) {
  type <- match.arg(type)
  ref_lab <- get_reference_class_label(reference_class)

  switch(
    type,
    mean_sd         = "Values are M and SD.",
    m_se            = "Values are M and SE.",
    n_pct           = "Values are n and %.",
    weighted_n_pct  = "Values are n and weighted %.",
      weighted_n_pct_app_occ = "Values are weighted n, weighted %, APP, and OCC. OCC is omitted when APP is effectively 1.00 because the odds ratio diverges.",
    pct_psig        = "Values are %, p, sig, and post-hoc.",
    rrr             = paste0("Values are RRR, LLCI, ULCI, p, and sig. Reference class = ", ref_lab, "."),
    rrr_compact     = paste0("Values are RRR (LLCI~ULCI), p, and sig. Reference class = ", ref_lab, "."),
    mean_sd_compact = paste("Values are M", COMPACT_PM, "SD."),
    mean_se_compact = paste("Values are estimated M", COMPACT_PM, "SE."),
    mixed_split     = if (isTRUE(HAS_WEIGHT)) {
      paste0("For each ", latent_group_term_lower(), ", the first statistic column reports M for continuous indicators and n for categorical indicators; the second statistic column reports SE for continuous indicators and weighted % for categorical indicators.")
    } else {
      paste0("For each ", latent_group_term_lower(), ", the first statistic column reports M for continuous indicators and n for categorical indicators; the second statistic column reports SD for continuous indicators and % for categorical indicators.")
    },
    generic         = ""
  )
}

append_note_row   <- function(df, note_text) {
  df              <- safe_df(df)

  if (length(note_text) == 0 || all(is.na(note_text))) return(df)
  note_text       <- trimws(as.character(note_text)[1])
  if (!nzchar(note_text)) return(df)

  if (ncol(df) == 0) {
    return(data.frame(
      Note             = paste0("Note. ", note_text),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    ))
  }

  note_row        <- as.list(rep("", ncol(df)))
  names(note_row) <- names(df)
  note_row[[1]]   <- paste0("Note. ", note_text)

  safe_rbind(
    df,
    as.data.frame(note_row, stringsAsFactors = FALSE, check.names = FALSE)
  )
}

# ------------------------------------------------------------
# 5. builders: T1 / T2 / T3
# ------------------------------------------------------------
build_T1_model_selection <- function() {
  data.frame(
    Characteristic = c(
      "Analysis type",
      "Selected covariance model",
      "Selected number of profiles",
      "Primary selection criterion",
      "Best tag",
      "Reference class"
    ),
    Value = c(
      latent_analysis_type(),
      model_structure,
      best_k,
      BEST_K_SUMMARY$best_rule %||% BEST_K_SUMMARY$BEST_K_RULE %||% "hybrid",
      best_tag,
      normalize_latent_group_text(REFERENCE_CLASS)
    ),
    stringsAsFactors = FALSE
  )
}

parse_mplus_class_counts_most_likely <- function(out_file) {
  if (is.null(out_file) || length(out_file) == 0 || is.na(out_file[1]) || !file.exists(out_file[1])) {
    return(data.frame())
  }

  lines <- tryCatch(readLines(out_file[1], warn = FALSE), error = function(e) character(0))
  if (length(lines) == 0) return(data.frame())

  up <- toupper(lines)
  starts <- grep("FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES", up, fixed = TRUE)
  if (length(starts) == 0) return(data.frame())

  start_i <- NA_integer_
  for (ii in starts) {
    look <- up[ii:min(length(up), ii + 8L)]
    if (any(grepl("BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP", look, fixed = TRUE))) {
      start_i <- ii
      break
    }
  }
  if (is.na(start_i)) return(data.frame())

  end_i <- min(length(lines), start_i + 80L)
  block <- lines[start_i:end_i]
  rows <- list()
  started <- FALSE

  for (ln in block) {
    nums <- regmatches(
      ln,
      gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[Ee][-+]?[0-9]+)?", ln, perl = TRUE)
    )[[1]]

    if (length(nums) >= 3) {
      class_num <- suppressWarnings(as.integer(as.numeric(nums[1])))
      n_val <- suppressWarnings(as.numeric(nums[2]))
      prop_val <- suppressWarnings(as.numeric(nums[3]))
      if (!is.na(class_num) && class_num >= 1 && !is.na(n_val) && !is.na(prop_val)) {
        rows[[length(rows) + 1L]] <- data.frame(
          class_num = class_num,
          n = n_val,
          prop = prop_val,
          stringsAsFactors = FALSE
        )
        started <- TRUE
        next
      }
    }

    if (started && !nzchar(trimws(ln))) break
  }

  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  out <- out[order(out$class_num), , drop = FALSE]
  rownames(out) <- NULL
  out
}

resolve_t2_out_files <- function(df) {
  out <- rep(NA_character_, nrow(df))
  if ("out_file" %in% names(df)) out <- as.character(df$out_file)

  reg <- safe_df(ESTIMATION_REGISTRY)
  if (nrow(reg) == 0 || !"out_file" %in% names(reg)) return(out)

  reg_k <- if ("k" %in% names(reg)) safe_int(reg$k) else rep(NA_integer_, nrow(reg))
  reg_model <- if ("model_structure" %in% names(reg)) tolower(as.character(reg$model_structure)) else rep(NA_character_, nrow(reg))

  df_k <- if ("k" %in% names(df)) safe_int(df$k) else rep(NA_integer_, nrow(df))
  df_model <- if ("model_structure" %in% names(df)) tolower(as.character(df$model_structure)) else rep(NA_character_, nrow(df))

  for (i in seq_len(nrow(df))) {
    if (!is.na(out[i]) && file.exists(out[i])) next
    hit <- which(!is.na(reg_k) & reg_k == df_k[i] & !is.na(reg_model) & reg_model == df_model[i])
    if (length(hit) == 0) hit <- which(!is.na(reg_k) & reg_k == df_k[i])
    if (length(hit) > 0) out[i] <- as.character(reg$out_file[hit[1]])
  }

  out
}

build_T2_model_fit <- function() {
  df <- safe_df(FIT_SUMMARY)
  if (nrow(df) == 0) return(data.frame())

  n <- nrow(df)

  get_col_num <- function(candidates) {
    hit <- candidates[candidates %in% names(df)][1]
    if (is.na(hit)) return(rep(NA_real_, n))
    x <- safe_num(df[[hit]])
    if (length(x) == 0) return(rep(NA_real_, n))
    x
  }

  if (!"k" %in% names(df) && "model_tag" %in% names(df)) {
    df$k <- extract_k_from_tag(df$model_tag)
  }
  if (!"model_structure" %in% names(df)) {
    if ("model_tag" %in% names(df)) {
      df$model_structure <- extract_model_structure_from_tag(df$model_tag)
    } else {
      df$model_structure <- rep(NA_character_, n)
    }
  }

  k_num <- safe_num(df$k)
  model_chr <- tolower(as.character(df$model_structure))
  n_total <- NA_real_
  if (exists("RAW_DATA", inherits = TRUE)) {
    raw_df <- safe_df(get("RAW_DATA", inherits = TRUE))
    if (nrow(raw_df) > 0) n_total <- nrow(raw_df)
  }
  if (is.na(n_total) && exists("ANALYSIS_DATA_SUB", inherits = TRUE)) {
    an_df <- safe_df(get("ANALYSIS_DATA_SUB", inherits = TRUE))
    if (nrow(an_df) > 0) n_total <- nrow(an_df)
  }

  smallest_n_raw <- get_col_num(c("smallest_class_n", "min_class_n"))
  smallest_p_raw <- get_col_num(c("smallest_class_p", "min_class_prop"))
  swap_idx <- !is.na(smallest_n_raw) & smallest_n_raw > 0 & smallest_n_raw <= 1
  if (any(swap_idx) && !is.na(n_total) && n_total > 0) {
    smallest_n_raw[swap_idx] <- round(n_total * smallest_n_raw[swap_idx], 0)
  }

  out <- data.frame(
    Selected = ifelse(
      !is.na(k_num) & !is.na(model_chr) &
        k_num == best_k &
        model_chr == tolower(model_structure),
      "Yes", ""
    ),
    Model                   = as.character(df$model_structure),
    Profiles                = k_num,
    LogLik                  = get_col_num(c("loglik", "logLik", "ll")),
    Parameters              = get_col_num(c("npar", "parameters", "n_parameters")),
    AIC                     = get_col_num(c("aic", "AIC")),
    BIC                     = get_col_num(c("bic", "BIC")),
    DBIC                    = get_col_num(c("dbic", "DBIC")),
    SABIC                   = get_col_num(c("sabic", "SABIC")),
    Entropy                 = get_col_num(c("entropy", "Entropy")),
    VLMR                    = get_col_num(c("vlmr_stat", "VLMR", "vlmr")),
    `VLMR p`                = get_col_num(c("vlmr_p", "VLMR p", "vlmr_pvalue")),
    LMR                     = get_col_num(c("lmr_stat", "LMR", "lmr")),
    `LMR p`                 = get_col_num(c("lmr_p", "LMR p", "lmr_pvalue")),
    BLRT                    = get_col_num(c("blrt_stat", "BLRT", "blrt")),
    `BLRT p`                = get_col_num(c("blrt_p", "BLRT p", "blrt_pvalue")),
    `Smallest class, n`     = smallest_n_raw,
    `Smallest class, %`     = {
      x <- smallest_p_raw
      ifelse(!is.na(x) & x <= 1, 100 * x, x)
    },
    stringsAsFactors        = FALSE,
    check.names            = FALSE
  )

  out_files <- resolve_t2_out_files(df)
  class_counts <- lapply(out_files, parse_mplus_class_counts_most_likely)
  for (ii in seq_along(class_counts)) {
    cc_df <- safe_df(class_counts[[ii]])
    if (nrow(cc_df) == 0) next
    out[["Smallest class, n"]][ii] <- min(cc_df$n, na.rm = TRUE)
    out[["Smallest class, %"]][ii] <- 100 * min(cc_df$prop, na.rm = TRUE)
  }
  max_class <- suppressWarnings(max(c(k_num, unlist(lapply(class_counts, function(x) x$class_num))), na.rm = TRUE))
  if (is.finite(max_class) && max_class > 0) {
    for (jj in seq_len(as.integer(max_class))) {
      n_col <- paste0(latent_group_term(), " ", jj, " n")
      p_col <- paste0(latent_group_term(), " ", jj, " %")
      out[[n_col]] <- NA_real_
      out[[p_col]] <- NA_real_

      for (ii in seq_along(class_counts)) {
        cc_df <- safe_df(class_counts[[ii]])
        if (nrow(cc_df) == 0) next
        hit <- cc_df[cc_df$class_num == jj, , drop = FALSE]
        if (nrow(hit) == 0) next
        out[[n_col]][ii] <- hit$n[1]
        out[[p_col]][ii] <- 100 * hit$prop[1]
      }
    }
  }

  class_count_cols <- grep(paste0("^", latent_group_term(), " [0-9]+ (n|%)$"), names(out), value = TRUE)
  for (cc in c("LogLik", "Parameters", "AIC", "BIC", "DBIC", "SABIC", "Entropy", "VLMR", "VLMR p", "LMR", "LMR p", "BLRT", "BLRT p", "Smallest class, n", "Smallest class, %", class_count_cols)) {
    if (cc %in% names(out)) out[[cc]] <- safe_num(out[[cc]])
  }
  if ("Profiles" %in% names(out)) out$Profiles <- safe_int(out$Profiles)

  fmt_fixed <- function(x, digits) {
    x_num <- suppressWarnings(as.numeric(x))
    out_chr <- ifelse(
      is.na(x_num),
      "",
      formatC(x_num, format = "f", digits = digits)
    )
    out_chr
  }
  fmt_int <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x_num), "", as.character(as.integer(round(x_num, 0))))
  }
  if ("Profiles" %in% names(out)) out$Profiles <- fmt_int(out$Profiles)
  if ("LogLik" %in% names(out)) out$LogLik <- fmt_fixed(out$LogLik, 0)
  if ("Parameters" %in% names(out)) out$Parameters <- fmt_int(out$Parameters)
  for (cc in c("AIC", "BIC", "DBIC", "SABIC", "Entropy", "VLMR", "VLMR p", "LMR", "LMR p", "BLRT", "BLRT p")) {
    if (cc %in% names(out)) out[[cc]] <- fmt_fixed(out[[cc]], 3)
  }
  if ("Smallest class, n" %in% names(out)) out[["Smallest class, n"]] <- fmt_int(out[["Smallest class, n"]])
  if ("Smallest class, %" %in% names(out)) out[["Smallest class, %"]] <- fmt_fixed(out[["Smallest class, %"]], 1)
  for (cc in class_count_cols) {
    if (grepl(" n$", cc)) {
      out[[cc]] <- fmt_int(out[[cc]])
    } else if (grepl(" %$", cc)) {
      out[[cc]] <- fmt_fixed(out[[cc]], 1)
    }
  }

  rownames(out) <- NULL
  out
}

size_note_type <- if (isTRUE(HAS_WEIGHT)) "weighted_n_pct" else "n_pct"
spread_note_type <- if (isTRUE(HAS_WEIGHT)) "m_se" else "mean_sd"
compact_spread_note_type <- if (isTRUE(HAS_WEIGHT)) "mean_se_compact" else "mean_sd_compact"
t4_table_type <- if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
  "twoline_npct"
} else if (isTRUE(has_mixed_indicators())) {
  "normal"
} else if (isTRUE(HAS_WEIGHT)) {
  "twoline_mse"
} else {
  "twoline_mean"
}
t6d_table_type <- if (isTRUE(HAS_WEIGHT)) "twoline_mse" else "twoline_mean"
a5_note_type <- if (isTRUE(HAS_WEIGHT)) "weighted_n_pct_app_occ" else "generic"

format_t3_twoline <- function(df) {
  df              <- safe_df(df)
  if (nrow(df) == 0) return(df)

  df$Profile      <- clean_profile_text(df$Profile, prefix = "Profile")
  df$Variable     <- if ("Variable" %in% names(df)) df$Variable else "Class size"

  out             <- unique(df[, c("Variable"), drop = FALSE])

  prof_names      <- sort(unique(df$Profile))
  prof_names      <- prof_names[order(suppressWarnings(as.integer(gsub("^Profile ", "", prof_names))))]

  for (pp in prof_names) {
    sub <- df[df$Profile == pp, , drop = FALSE]
    out[[twoline_key("n", pp)]] <- fmt_n0(sub$n[match(out$Variable, sub$Variable)])
    out[[twoline_key("%", pp)]] <- fmt_pct1_plain(sub$pct[match(out$Variable, sub$Variable)])
  }

  out
}

build_T3_profile_size <- function() {
  df <- safe_df(CLASS_SUMMARY_FINAL)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  class_col <- first_existing_col(df, c("class", "Class", "profile", "Profile"))
  n_col     <- first_existing_col(df, c("n", "N"))
  prop_col  <- if (isTRUE(HAS_WEIGHT)) {
    first_existing_col(df, c("weighted_prop", "weighted_percent", "prop", "proportion", "pct", "percent",
                             "estimated_prop", "Assigned %"))
  } else {
    first_existing_col(df, c("prop", "proportion", "pct", "percent", "estimated_prop", "Assigned %",
                             "weighted_prop", "weighted_percent"))
  }

  if (is.na(class_col) || is.na(n_col)) return(data.frame())

  name_col <- latent_group_term()
  prefix_i <- latent_group_term()

  class_vec <- clean_profile_text(df[[class_col]], prefix = prefix_i)
  n_vec     <- suppressWarnings(as.numeric(df[[n_col]]))

  pct_vec <- rep(NA_real_, nrow(df))
  if (!is.na(prop_col) && prop_col %in% names(df)) {
    pct_vec <- 100 * normalize_prop_by_source(df[[prop_col]], prop_col)
  } else if (length(n_vec) > 0) {
    pct_vec <- 100 * n_vec / sum(n_vec, na.rm = TRUE)
  }

  out <- data.frame(
    group_lab = class_vec,
    n_val     = n_vec,
    pct_val   = pct_vec,
    stringsAsFactors = FALSE
  )

  grp_num <- suppressWarnings(as.integer(gsub(paste0("^", prefix_i, "\\s+([0-9]+)$"), "\\1", out$group_lab)))
  grp_num[is.na(grp_num)] <- 999999 + seq_len(sum(is.na(grp_num)))

  out <- out[order(grp_num), , drop = FALSE]
  rownames(out) <- NULL

  out$n_val   <- fmt_n0(out$n_val)
  out$pct_val <- fmt_pct1_plain(out$pct_val)

  names(out) <- c(name_col, "n", "%")
  out
}


build_lca_t3_from_classified <- function(df, indicators, dict = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())
  if (!"class_num" %in% names(df)) return(data.frame())

  indicators <- intersect(indicators, names(df))
  if (length(indicators) == 0) return(data.frame())

  out <- list()
  idx <- 1L

  classes <- sort(unique(safe_int(df$class_num)))
  classes <- classes[!is.na(classes)]

  for (v in indicators) {
    x_all <- df[[v]]
    cats  <- sort(unique(x_all[!is.na(x_all)]))
    if (length(cats) == 0) next

    var_lab <- v
    if (is.list(dict) && is.data.frame(dict$meta) && "var_name" %in% names(dict$meta)) {
      hit   <- dict$meta[dict$meta$var_name == v, , drop = FALSE]
      if (nrow(hit) > 0) {
        for (cc in c("label_en", "label_ko", "var_label", "var_name")) {
          if (cc %in% names(hit)) {
            vv <- as.character(hit[[cc]][1])
            if (!is.na(vv) && nzchar(vv)) {
              var_lab <- vv
              break
            }
          }
        }
      }
    }

    for (cl in classes) {
      sub <- df[df$class_num == cl & !is.na(df[[v]]), , drop = FALSE]
      if (nrow(sub) == 0) next

      tab <- prop.table(table(sub[[v]]))
      for (catv in names(tab)) {
        out[[idx]] <- data.frame(
          class_num = cl,
          class     = paste0("Class ", cl),
          var_name  = v,
          var_label = var_lab,
          category  = as.character(catv),
          mean      = as.numeric(tab[[catv]]),
          se        = NA_real_,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

if (nrow(T3_indicator_profile) == 0 &&
    exists("ANALYSIS_DATA_CLASSIFIED") &&
    is.data.frame(ANALYSIS_DATA_CLASSIFIED) &&
    isTRUE(mixture_mode == "lca")) {

  ind_all <- SETTINGS_SUMMARY$INDICATORS %||%
    SETTINGS_SUMMARY$indicators %||%
    character(0)

  T3_indicator_profile <- build_lca_t3_from_classified(
    df         = ANALYSIS_DATA_CLASSIFIED,
    indicators = ind_all,
    dict       = DICT
  )

  T3_indicator_profile$mean <- unlist(T3_indicator_profile$mean)
  T3_indicator_profile$se   <- unlist(T3_indicator_profile$se)

  T3_indicator_profile$mean <- suppressWarnings(as.numeric(T3_indicator_profile$mean))
  T3_indicator_profile$se   <- suppressWarnings(as.numeric(T3_indicator_profile$se))
}


# ------------------------------------------------------------
# 6. builders: T4 / A3 / A4
# ------------------------------------------------------------
build_lpa_profile_from_classified <- function(df, indicators, standardize = FALSE) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  if (!"class_num" %in% names(df)) {
    if ("Class" %in% names(df)) df$class_num <- extract_class_num_from_text(df$Class)
    if (!"class_num" %in% names(df) && "class" %in% names(df)) df$class_num <- extract_class_num_from_text(df$class)
  }
  if (!"class_num" %in% names(df)) return(data.frame())

  indicators <- intersect(as.character(indicators), names(df))
  if (length(indicators) == 0) return(data.frame())

  dat <- df[, c("class_num", indicators), drop = FALSE]
  dat$class_num <- safe_int(dat$class_num)
  dat <- dat[!is.na(dat$class_num), , drop = FALSE]
  if (nrow(dat) == 0) return(data.frame())

  for (nm in indicators) dat[[nm]] <- safe_num(dat[[nm]])

  if (isTRUE(standardize)) {
    for (nm in indicators) {
      x <- safe_num(dat[[nm]])
      mu <- mean(x, na.rm = TRUE)
      sdv <- stats::sd(x, na.rm = TRUE)
      if (is.na(sdv) || sdv == 0) {
        dat[[nm]] <- NA_real_
      } else {
        dat[[nm]] <- (x - mu) / sdv
      }
    }
  }

  cls <- sort(unique(dat$class_num))
  out <- list()
  idx <- 1L
  for (cl in cls) {
    sub <- dat[dat$class_num == cl, , drop = FALSE]
    for (nm in indicators) {
      x <- safe_num(sub[[nm]])
      x <- x[!is.na(x)]
      if (length(x) == 0) next
      out[[idx]] <- data.frame(
        model_tag = as.character(best_tag %||% ""),
        var_name = nm,
        Class = latent_group_label(cl),
        Mean = mean(x, na.rm = TRUE),
        SD = stats::sd(x, na.rm = TRUE),
        class = latent_group_label(cl),
        var_label = get_var_label(nm),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

build_categorical_profile_from_classified <- function(df, indicators, dict = NULL, weight_var = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())
  if (!"class_num" %in% names(df)) return(data.frame())

  indicators <- intersect(as.character(indicators), names(df))
  if (length(indicators) == 0) return(data.frame())

  df$class_num <- safe_int(df$class_num)
  df <- df[!is.na(df$class_num), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  weight_ok <- !is.null(weight_var) && nzchar(as.character(weight_var)) && weight_var %in% names(df)
  w_all <- if (isTRUE(weight_ok)) suppressWarnings(as.numeric(df[[weight_var]])) else NULL

  out <- list()
  idx <- 1L
  classes <- sort(unique(df$class_num))

  for (v in indicators) {
    x_all <- df[[v]]
    cats  <- sort(unique(x_all[!is.na(x_all)]))
    if (length(cats) == 0) next

    var_lab <- get_var_label(v)

    for (cl in classes) {
      keep_class <- df$class_num == cl & !is.na(df[[v]])
      sub <- df[keep_class, , drop = FALSE]
      if (nrow(sub) == 0) next

      if (isTRUE(weight_ok)) {
        w_sub <- suppressWarnings(as.numeric(sub[[weight_var]]))
        valid_w <- is.finite(w_sub) & !is.na(sub[[v]]) & w_sub > 0
        use_weight <- any(valid_w)
      } else {
        w_sub <- NULL
        use_weight <- FALSE
      }

      for (catv in cats) {
        prop_i <- NA_real_
        if (isTRUE(use_weight)) {
          denom <- sum(w_sub[valid_w], na.rm = TRUE)
          num <- sum(w_sub[valid_w & as.character(sub[[v]]) == as.character(catv)], na.rm = TRUE)
          if (is.finite(denom) && denom > 0) prop_i <- num / denom
        } else {
          x_sub <- sub[[v]]
          prop_i <- mean(as.character(x_sub) == as.character(catv), na.rm = TRUE)
        }
        if (!is.finite(prop_i) || is.na(prop_i)) next

        out[[idx]] <- data.frame(
          model_tag = as.character(best_tag %||% ""),
          class_num = as.integer(cl),
          class = latent_group_label(cl),
          Class = latent_group_label(cl),
          var_name = v,
          var_label = var_lab,
          category = as.character(catv),
          label = get_value_label(v, catv),
          mean = as.numeric(prop_i),
          se = NA_real_,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

build_mixed_profile_from_classified <- function(df, indicators_continuous, indicators_categorical, dict = NULL, weight_var = NULL, standardize = FALSE) {
  cont <- build_lpa_profile_from_classified(
    df = df,
    indicators = indicators_continuous,
    standardize = standardize
  )
  cat <- if (!isTRUE(standardize)) {
    build_categorical_profile_from_classified(
      df = df,
      indicators = indicators_categorical,
      dict = dict,
      weight_var = weight_var
    )
  } else {
    data.frame()
  }

  cont_out <- data.frame()
  if (nrow(cont) > 0) {
    cont_out <- data.frame(
      model_tag = as.character(cont$model_tag %||% best_tag %||% ""),
      class_num = extract_class_num_from_text(cont$Class),
      class = normalize_latent_group_text(as.character(cont$class %||% cont$Class)),
      Class = normalize_latent_group_text(as.character(cont$Class)),
      var_name = as.character(cont$var_name),
      var_label = as.character(cont$var_label),
      category = "",
      label = as.character(cont$var_label),
      mean = suppressWarnings(as.numeric(cont$Mean)),
      se = suppressWarnings(as.numeric(cont$SD)),
      stringsAsFactors = FALSE
    )
  }

  out <- safe_rbind(cont_out, cat)
  if (nrow(out) == 0) return(out)
  out$model_tag[is.na(out$model_tag) | !nzchar(trimws(out$model_tag))] <- as.character(best_tag %||% "")
  out
}

build_mixed_t4_twoline <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  if (!"category" %in% names(df)) df$category <- ""
  df$category <- as.character(df$category)

  is_categorical_row <- !is.na(df$category) & nzchar(trimws(df$category))

  cont_out <- if (any(!is_categorical_row)) {
    build_profile_wide_from_T3_twoline(df[!is_categorical_row, , drop = FALSE])
  } else {
    data.frame()
  }

  cat_out <- if (any(is_categorical_row)) {
    build_lca_t4_twoline(df[is_categorical_row, , drop = FALSE])
  } else {
    data.frame()
  }

  if (nrow(cont_out) == 0) return(cat_out)
  if (nrow(cat_out) == 0) return(cont_out)

  sep_row <- as.list(rep("", length(names(cont_out))))
  names(sep_row) <- names(cont_out)
  sep_row$Variable <- "Categorical indicators"
  sep_df <- as.data.frame(sep_row, stringsAsFactors = FALSE, check.names = FALSE)

  safe_rbind(safe_rbind(cont_out, sep_df), cat_out)
}

build_mixed_profile_compact <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  if ("model_tag" %in% names(df) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    hit_best <- df[as.character(df$model_tag) == as.character(best_tag), , drop = FALSE]
    if (nrow(hit_best) > 0) df <- hit_best
  }

  if (!"var_name" %in% names(df)) return(data.frame())
  if (!"class_num" %in% names(df)) {
    if ("Class" %in% names(df)) df$class_num <- extract_class_num_from_text(df$Class)
    if (!"class_num" %in% names(df) && "class" %in% names(df)) df$class_num <- extract_class_num_from_text(df$class)
  }
  if ("class_num" %in% names(df) && any(is.na(df$class_num))) {
    if ("Class" %in% names(df)) {
      miss_class_num <- is.na(df$class_num)
      df$class_num[miss_class_num] <- extract_class_num_from_text(df$Class[miss_class_num])
    }
    if ("class" %in% names(df) && any(is.na(df$class_num))) {
      miss_class_num <- is.na(df$class_num)
      df$class_num[miss_class_num] <- extract_class_num_from_text(df$class[miss_class_num])
    }
  }
  if (!"class_num" %in% names(df)) return(data.frame())

  if (!"Variable" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$Variable <- as.character(df$var_label)
    } else {
      df$Variable <- get_var_label(df$var_name)
    }
  }
  if (!"Category" %in% names(df)) {
    if ("label" %in% names(df) && "category" %in% names(df)) {
      cat_raw <- as.character(df$category)
      df$Category <- ifelse(
        !is.na(cat_raw) & nzchar(trimws(cat_raw)),
        as.character(df$label),
        ""
      )
    } else if ("category" %in% names(df)) {
      df$Category <- as.character(df$category)
    } else {
      df$Category <- ""
    }
  }
  df$Category <- as.character(df$Category)
  df$Category[is.na(df$Category)] <- ""

  df$class_num <- safe_int(df$class_num)
  df <- df[!is.na(df$class_num), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  if (!"mean" %in% names(df)) {
    mean_col <- first_existing_col(df, c("Mean", "estimate", "value"))
    if (is.na(mean_col)) return(data.frame())
    df$mean <- safe_num(df[[mean_col]])
  } else {
    df$mean <- safe_num(df$mean)
    mean_col <- first_existing_col(df, c("Mean", "estimate", "value"))
    if (!is.na(mean_col) && any(is.na(df$mean))) {
      miss_mean <- is.na(df$mean)
      df$mean[miss_mean] <- safe_num(df[[mean_col]][miss_mean])
    }
  }

  spread_col <- first_existing_col(df, c("se", "SE", "sd", "SD"))
  if (!is.na(spread_col) && !"spread_value" %in% names(df)) {
    df$spread_value <- safe_num(df[[spread_col]])
  }
  if (!"spread_value" %in% names(df)) df$spread_value <- NA_real_
  if (any(is.na(df$spread_value))) {
    spread_col_fallback <- first_existing_col(df, c("SE", "SD", "se", "sd"))
    if (!is.na(spread_col_fallback)) {
      miss_spread <- is.na(df$spread_value)
      df$spread_value[miss_spread] <- safe_num(df[[spread_col_fallback]][miss_spread])
    }
  }

  if (!"category" %in% names(df)) df$category <- ""
  df$category <- as.character(df$category)
  df$category[is.na(df$category)] <- ""
  is_categorical_row <- !is.na(df$category) & nzchar(trimws(df$category))

  if (any(is_categorical_row)) {
    class_n_map <- safe_df(CLASS_SUMMARY_FINAL)
    n_col_map <- first_existing_col(class_n_map, c("n", "weighted_n"))
    if (!"class_num" %in% names(class_n_map) || is.na(n_col_map)) {
      n_base <- rep(NA_real_, nrow(df))
    } else {
      class_n_map <- data.frame(
        class_num = safe_int(class_n_map$class_num),
        n_base = safe_num(class_n_map[[n_col_map]])
      )
      n_base <- class_n_map$n_base[match(df$class_num, class_n_map$class_num)]
    }
    df$n_cell <- ifelse(is_categorical_row, round(df$mean * safe_num(n_base), 0), NA_real_)
    df$pct_cell <- ifelse(is_categorical_row, 100 * df$mean, NA_real_)
  } else {
    df$n_cell <- NA_real_
    df$pct_cell <- NA_real_
  }

  df$cell <- ifelse(
    is_categorical_row,
    fmt_n_pct_cell(df$n_cell, df$pct_cell),
    fmt_mean_sd_cell(df$mean, df$spread_value)
  )

  ord_var <- get_display_order(df$var_name)
  ord_cat <- get_category_order(df$var_name, level = df$category, label = df$Category)
  ord_cat[is.na(ord_cat)] <- 999999
  df <- df[order(ord_var, df$var_name, ord_cat, df$class_num), , drop = FALSE]
  rownames(df) <- NULL

  id_df <- unique(df[, c("var_name", "Variable", "category", "Category"), drop = FALSE])
  id_df <- id_df[order(
    get_display_order(id_df$var_name),
    get_category_order(id_df$var_name, level = id_df$category, label = id_df$Category)
  ), , drop = FALSE]

  out <- id_df[, c("Variable", "Category"), drop = FALSE]
  key_out <- paste(id_df$var_name, id_df$category, sep = "||")

  prof_nums <- sort(unique(df$class_num))
  for (cl in prof_nums) {
    sub <- df[df$class_num == cl, , drop = FALSE]
    key_sub <- paste(sub$var_name, sub$category, sep = "||")
    out[[latent_group_label(cl)]] <- sub$cell[match(key_out, key_sub)]
  }

  dup_var <- duplicated(id_df$var_name)
  out$Variable[dup_var] <- ""

  out
}

build_mixed_profile_split <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  if ("model_tag" %in% names(df) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    hit_best <- df[as.character(df$model_tag) == as.character(best_tag), , drop = FALSE]
    if (nrow(hit_best) > 0) df <- hit_best
  }

  if (!"var_name" %in% names(df)) return(data.frame())

  if (!"class_num" %in% names(df)) {
    if ("Class" %in% names(df)) df$class_num <- extract_class_num_from_text(df$Class)
    if (!"class_num" %in% names(df) && "class" %in% names(df)) df$class_num <- extract_class_num_from_text(df$class)
  }
  if ("class_num" %in% names(df) && any(is.na(df$class_num))) {
    if ("Class" %in% names(df)) {
      miss_class_num <- is.na(df$class_num)
      df$class_num[miss_class_num] <- extract_class_num_from_text(df$Class[miss_class_num])
    }
    if ("class" %in% names(df) && any(is.na(df$class_num))) {
      miss_class_num <- is.na(df$class_num)
      df$class_num[miss_class_num] <- extract_class_num_from_text(df$class[miss_class_num])
    }
  }
  if (!"class_num" %in% names(df)) return(data.frame())

  if (!"Variable" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$Variable <- as.character(df$var_label)
    } else {
      df$Variable <- get_var_label(df$var_name)
    }
  }

  if (!"Category" %in% names(df)) {
    if ("label" %in% names(df) && "category" %in% names(df)) {
      cat_raw <- as.character(df$category)
      df$Category <- ifelse(
        !is.na(cat_raw) & nzchar(trimws(cat_raw)),
        as.character(df$label),
        ""
      )
    } else if ("category" %in% names(df)) {
      df$Category <- as.character(df$category)
    } else {
      df$Category <- ""
    }
  }

  df$Category <- as.character(df$Category)
  df$Category[is.na(df$Category)] <- ""

  if (!"mean" %in% names(df)) {
    mean_col <- first_existing_col(df, c("Mean", "estimate", "value"))
    if (is.na(mean_col)) return(data.frame())
    df$mean <- safe_num(df[[mean_col]])
  } else {
    df$mean <- safe_num(df$mean)
    mean_col <- first_existing_col(df, c("Mean", "estimate", "value"))
    if (!is.na(mean_col) && any(is.na(df$mean))) {
      miss_mean <- is.na(df$mean)
      df$mean[miss_mean] <- safe_num(df[[mean_col]][miss_mean])
    }
  }

  spread_col <- if (isTRUE(HAS_WEIGHT)) {
    first_existing_col(df, c("SE", "se", "SD", "sd"))
  } else {
    first_existing_col(df, c("SD", "sd", "SE", "se"))
  }
  df$spread_value <- if (!is.na(spread_col)) safe_num(df[[spread_col]]) else NA_real_

  if (!"category" %in% names(df)) df$category <- ""
  df$category <- as.character(df$category)
  df$category[is.na(df$category)] <- ""
  is_categorical_row <- nzchar(trimws(df$category))

  class_n_map <- safe_df(CLASS_SUMMARY_FINAL)
  n_col_map <- first_existing_col(class_n_map, c("n", "weighted_n"))
  if ("class_num" %in% names(class_n_map) && !is.na(n_col_map)) {
    n_base <- safe_num(class_n_map[[n_col_map]])[match(df$class_num, safe_int(class_n_map$class_num))]
  } else {
    n_base <- rep(NA_real_, nrow(df))
  }

  df$n_value <- ifelse(is_categorical_row, round(df$mean * safe_num(n_base), 0), NA_real_)
  df$pct_value <- ifelse(is_categorical_row, 100 * df$mean, NA_real_)

  ord_var <- get_display_order(df$var_name)
  ord_cat <- get_category_order(df$var_name, level = df$category, label = df$Category)
  ord_cat[is.na(ord_cat)] <- 999999
  df <- df[order(ord_var, df$var_name, ord_cat, df$class_num), , drop = FALSE]
  rownames(df) <- NULL

  id_df <- unique(df[, c("var_name", "Variable", "category", "Category"), drop = FALSE])
  id_df <- id_df[order(
    get_display_order(id_df$var_name),
    get_category_order(id_df$var_name, level = id_df$category, label = id_df$Category)
  ), , drop = FALSE]

  out <- id_df[, c("Variable", "Category"), drop = FALSE]
  key_out <- paste(id_df$var_name, id_df$category, sep = "||")
  prof_nums <- sort(unique(safe_int(df$class_num)))
  spread_key <- if (isTRUE(HAS_WEIGHT)) "SE/%" else "SD/%"

  for (cl in prof_nums) {
    prof_lab <- latent_group_label(cl)
    sub <- df[df$class_num == cl, , drop = FALSE]
    key_sub <- paste(sub$var_name, sub$category, sep = "||")
    idx <- match(key_out, key_sub)
    sub_is_categorical <- nzchar(trimws(as.character(sub$category)))

    out[[twoline_key("M/n", prof_lab)]] <- ifelse(
      is.na(idx),
      "",
      ifelse(sub_is_categorical[idx], fmt_n0(sub$n_value[idx]), fmt_m2(sub$mean[idx]))
    )
    out[[twoline_key(spread_key, prof_lab)]] <- ifelse(
      is.na(idx),
      "",
      ifelse(sub_is_categorical[idx], fmt_pct1_plain(sub$pct_value[idx]), fmt_sd2(sub$spread_value[idx]))
    )
  }

  dup_var <- duplicated(id_df$var_name)
  out$Variable[dup_var] <- ""
  out
}

resolve_lpa_profile_source <- function(prefer_z = FALSE) {
  src <- if (isTRUE(prefer_z)) safe_df(T3_indicator_profile_z) else safe_df(T3_indicator_profile)

  indicators_continuous <- SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||%
    SETTINGS_SUMMARY$indicators_continuous %||%
    character(0)
  indicators_categorical <- SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||%
    SETTINGS_SUMMARY$indicators_categorical %||%
    character(0)
  has_mixed_indicators <- length(indicators_continuous) > 0 && length(indicators_categorical) > 0
  weight_var <- SETTINGS_SUMMARY$WEIGHT_VAR %||% SETTINGS_SUMMARY$weight_var %||% NULL
  classified_src <- safe_df(CLASSIFIED_ANALYSIS)
  if (nrow(classified_src) == 0) classified_src <- safe_df(ANALYSIS_DATA_CLASSIFIED)

  append_categorical_part <- function(base_df) {
    base_df <- safe_df(base_df)
    if (!isTRUE(has_mixed_indicators) || isTRUE(prefer_z) || nrow(classified_src) == 0) {
      return(base_df)
    }
    if (!"category" %in% names(base_df)) base_df$category <- ""
    if (!"label" %in% names(base_df)) base_df$label <- ""
    cat_part <- build_categorical_profile_from_classified(
      df = classified_src,
      indicators = indicators_categorical,
      dict = DICT,
      weight_var = weight_var
    )
    if (nrow(cat_part) == 0) return(base_df)
    safe_rbind(base_df, cat_part)
  }

  if (!isTRUE(HAS_WEIGHT)) {
    fallback <- if (isTRUE(has_mixed_indicators)) {
      build_mixed_profile_from_classified(
        df = classified_src,
        indicators_continuous = indicators_continuous,
        indicators_categorical = indicators_categorical,
        dict = DICT,
        weight_var = weight_var,
        standardize = isTRUE(prefer_z)
      )
    } else {
      indicators <- SETTINGS_SUMMARY$INDICATORS %||% SETTINGS_SUMMARY$indicators %||% character(0)
      build_lpa_profile_from_classified(
        df = classified_src,
        indicators = indicators,
        standardize = isTRUE(prefer_z)
      )
    }
    if (nrow(fallback) > 0) return(fallback)
  }

  if (nrow(src) > 0 && "model_tag" %in% names(src) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    hit_best <- src[as.character(src$model_tag) == as.character(best_tag), , drop = FALSE]
    if (nrow(hit_best) > 0) {
      return(append_categorical_part(hit_best))
    }
  }

  if (!isTRUE(prefer_z) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    out_file <- file.path(DIR_MPLUS_INP, paste0(best_tag, ".out"))
    if (file.exists(out_file)) {
      indicators <- SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||%
        SETTINGS_SUMMARY$indicators_continuous %||%
        SETTINGS_SUMMARY$INDICATORS %||%
        SETTINGS_SUMMARY$indicators %||%
        character(0)
      indicators <- toupper(unique_nz(indicators))

      if (length(indicators) > 0) {
        lines <- tryCatch(readLines(out_file, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
        lines <- trimws(lines)
        if (length(lines) > 0) {
          in_model_results <- FALSE
          in_means_block <- FALSE
          current_class <- NA_integer_
          out_list <- list()
          idx <- 1L

          for (ln in lines) {
            if (!nzchar(ln)) next
            up_ln <- toupper(ln)
            if (grepl("MODEL RESULTS", up_ln, fixed = TRUE)) {
              in_model_results <- TRUE
              in_means_block <- FALSE
              current_class <- NA_integer_
              next
            }
            if (!in_model_results) next
            if (grepl("LATENT CLASS\\s+[0-9]+", up_ln)) {
              current_class <- suppressWarnings(as.integer(gsub(".*LATENT CLASS\\s+([0-9]+).*", "\\1", up_ln)))
              in_means_block <- FALSE
              next
            }
            if (grepl("^MEANS$", up_ln)) {
              in_means_block <- TRUE
              next
            }
            if (grepl("^(VARIANCES|INTERCEPTS|THRESHOLDS|CATEGORICAL LATENT VARIABLES|LATENT CLASS|QUALITY OF NUMERICAL RESULTS)", up_ln)) {
              in_means_block <- FALSE
            }
            if (!in_means_block || is.na(current_class)) next

            parts <- unlist(strsplit(ln, "\\s+"))
            if (length(parts) < 3) next
            var_i <- toupper(parts[1])
            if (!(var_i %in% indicators)) next

            est_i <- suppressWarnings(as.numeric(parts[2]))
            se_i <- suppressWarnings(as.numeric(parts[3]))
            if (is.na(est_i)) next

            out_list[[idx]] <- data.frame(
              model_tag = as.character(best_tag),
              var_name = tolower(var_i),
              Class = paste0("Class ", current_class),
              Mean = est_i,
              SE = se_i,
              class = paste0("Class ", current_class),
              var_label = get_var_label(tolower(var_i)),
              stringsAsFactors = FALSE
            )
            idx <- idx + 1L
          }

          if (length(out_list) > 0) {
            parsed_best <- do.call(rbind, out_list)
            rownames(parsed_best) <- NULL
            return(append_categorical_part(parsed_best))
          }
        }
      }
    }
  }

  fallback <- if (isTRUE(has_mixed_indicators)) {
    build_mixed_profile_from_classified(
      df = classified_src,
      indicators_continuous = indicators_continuous,
      indicators_categorical = indicators_categorical,
      dict = DICT,
      weight_var = weight_var,
      standardize = isTRUE(prefer_z)
    )
  } else {
    indicators <- SETTINGS_SUMMARY$INDICATORS %||% SETTINGS_SUMMARY$indicators %||% character(0)
    build_lpa_profile_from_classified(
      df = classified_src,
      indicators = indicators,
      standardize = isTRUE(prefer_z)
    )
  }
  if (nrow(fallback) > 0) return(fallback)

  append_categorical_part(src)
}

build_profile_wide_from_T3 <- function(df_in) {
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
    return(data.frame())
  }

  df <- safe_df(df_in)
  if (nrow(df) == 0) return(data.frame())

  if ("model_tag" %in% names(df) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    hit_best <- df[as.character(df$model_tag) == as.character(best_tag), , drop = FALSE]
    if (nrow(hit_best) > 0) df <- hit_best
  }

  # ???FIX: var_name fallback ?????????????곕섯???
  if (!"Variable" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$Variable <- as.character(df$var_label)
    } else if ("label" %in% names(df)) {
      df$Variable <- as.character(df$label)
    } else if ("var_name" %in% names(df)) {
      df$Variable <- get_var_label(df$var_name)
    }
  }

  # Class fallback
  if (!"Class" %in% names(df)) {
    if ("class" %in% names(df)) {
      df$Class <- as.character(df$class)
    } else if ("class_num" %in% names(df)) {
      df$Class <- paste0("Profile ", df$class_num)
    }
  }

  if (!"Class" %in% names(df) && "class" %in% names(df)) df$Class <- as.character(df$class)
  if (!"Class" %in% names(df) && "profile" %in% names(df)) df$Class <- as.character(df$profile)
  if (!"Class" %in% names(df) && "Profile" %in% names(df)) df$Class <- as.character(df$Profile)

  mean_col <- first_existing_col(df, c("Mean", "mean", "mu", "estimate", "value", "class_mean"))
  spread_col <- if (isTRUE(HAS_WEIGHT)) {
    first_existing_col(df, c("SE", "se", "stderr", "std_error", "SD", "sd"))
  } else {
    first_existing_col(df, c("SD", "sd", "SE", "se", "stderr", "std_error"))
  }
  spread_key <- if (isTRUE(HAS_WEIGHT)) "SE" else "SD"

  if (all(c("Variable", "Class") %in% names(df)) && !is.na(mean_col)) {
    df$Class <- clean_profile_text(df$Class, prefix = "Profile")

    if (!is.na(spread_col)) {
      df$Mean_SE <- paste0(fmt_m2(df[[mean_col]]), " ", COMPACT_PM, " ", fmt_sd2(df[[spread_col]]))
    } else {
      df$Mean_SE <- fmt_m2(df[[mean_col]])
    }

    wide <- reshape(
      df[, c("Variable", "Class", "Mean_SE"), drop = FALSE],
      idvar                                        = "Variable",
      timevar                                      = "Class",
      direction                                    = "wide"
    )

    names(wide) <- gsub("^Mean_SE\\.", "", names(wide))

    if ("var_name" %in% names(df)) {
      map_df <- unique(df[, c("Variable", "var_name"), drop = FALSE])
      wide$var_name_internal <- map_df$var_name[match(wide$Variable, map_df$Variable)]
    } else {
      wide$var_name_internal <- as.character(wide$Variable)
    }

    prof_names <- sort(setdiff(names(wide), c("Variable", "var_name_internal")))
    prof_names <- prof_names[order(suppressWarnings(as.integer(gsub("^Profile ", "", prof_names))))]

    keep <- c("Variable", prof_names, "var_name_internal")
    keep <- keep[keep %in% names(wide)]

    wide <- wide[, keep, drop = FALSE]

    wide <- format_variable_category_block(
      df = wide,
      var_col = "var_name_internal",
      var_label_col = "Variable",
      cat_col = NULL
    )
    wide$var_name_internal <- NULL
    return(wide)
  }

  data.frame(Note = "T4 source object exists but could not be reshaped.", stringsAsFactors = FALSE)
}

build_profile_wide_from_T3_twoline <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if ("model_tag" %in% names(df) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    hit_best <- df[as.character(df$model_tag) == as.character(best_tag), , drop = FALSE]
    if (nrow(hit_best) > 0) df <- hit_best
  }

  # ???FIX: Variable fallback ?????????????곕섯???
  if (!"Variable" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$Variable <- as.character(df$var_label)
    } else if ("label" %in% names(df)) {
      df$Variable <- as.character(df$label)
    } else if ("var_name" %in% names(df)) {
      df$Variable <- get_var_label(df$var_name)
    }
  }

  # ???FIX: Class fallback ?????????????곕섯???
  if (!"Class" %in% names(df)) {
    if ("class" %in% names(df)) {
      df$Class <- as.character(df$class)
    } else if ("profile" %in% names(df)) {
      df$Class <- as.character(df$profile)
    } else if ("Profile" %in% names(df)) {
      df$Class <- as.character(df$Profile)
    } else if ("class_num" %in% names(df)) {
      df$Class <- paste0("Profile ", df$class_num)
    }
  }

  if (!"var_name" %in% names(df) && "variable" %in% names(df)) df$var_name <- as.character(df$variable)
  if (!"var_name" %in% names(df) && "Variable" %in% names(df)) df$var_name <- as.character(df$Variable)

  mean_col <- first_existing_col(df, c("Mean", "mean", "mu", "estimate", "value", "class_mean"))
  spread_col <- if (isTRUE(HAS_WEIGHT)) {
    first_existing_col(df, c("SE", "se", "stderr", "std_error", "SD", "sd"))
  } else {
    first_existing_col(df, c("SD", "sd", "SE", "se", "stderr", "std_error"))
  }
  spread_key <- if (isTRUE(HAS_WEIGHT)) "SE" else "SD"
  if (!all(c("Variable", "Class") %in% names(df)) || is.na(mean_col)) return(data.frame())

  df$Class  <- clean_profile_text(df$Class, prefix = "Profile")
  df$M_fmt  <- fmt_m2(df[[mean_col]])
  df$SPREAD_fmt <- if (!is.na(spread_col)) fmt_sd2(df[[spread_col]]) else ""

  vars <- unique(as.character(df$Variable))
  out  <- data.frame(Variable = vars, stringsAsFactors = FALSE, check.names = FALSE)

  prof_names <- unique(as.character(df$Class))
  prof_names <- prof_names[grepl("^Profile [0-9]+$", prof_names)]
  prof_names <- prof_names[order(suppressWarnings(as.integer(gsub("^Profile ", "", prof_names))))]

  for (pp in prof_names) {
    sub <- df[df$Class == pp, , drop = FALSE]
    out[[twoline_key("M", pp)]]  <- sub$M_fmt[match(out$Variable, sub$Variable)]
    out[[twoline_key(spread_key, pp)]] <- sub$SPREAD_fmt[match(out$Variable, sub$Variable)]
  }

  out <- out[, c("Variable", unlist(lapply(prof_names, function(pp) c(twoline_key("M", pp), twoline_key(spread_key, pp))))), drop = FALSE]
  out
}

build_lca_t4_twoline <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  if ("model_tag" %in% names(df) && !is.na(best_tag) && nzchar(as.character(best_tag))) {
    hit_best <- df[as.character(df$model_tag) == as.character(best_tag), , drop = FALSE]
    if (nrow(hit_best) > 0) df <- hit_best
  }

  if (!"var_name" %in% names(df)) return(data.frame())
  if (!"class_num" %in% names(df)) return(data.frame())

  if (!"Variable" %in% names(df)) {
    if ("var_label" %in% names(df)) {
      df$Variable <- as.character(df$var_label)
    } else {
      df$Variable <- get_var_label(df$var_name)
    }
  }

  if (!"Category" %in% names(df)) {
    if ("label" %in% names(df)) {
      df$Category <- as.character(df$label)
    } else if ("category" %in% names(df)) {
      df$Category <- as.character(df$category)
    } else {
      df$Category <- ""
    }
  }

  # mean = category probability
  if (!"mean" %in% names(df)) return(data.frame())
  df$mean <- safe_num(df$mean)

  # n ????????????? class summary?? ??????꿔꺂???⑸븶??????????????????????롮쾸?椰????????????????????????????count ?????????ш끽維뽳쭩?뱀땡???얩맪???????????
  class_n_map <- safe_df(CLASS_SUMMARY_FINAL)
  if (!all(c("class_num", "n") %in% names(class_n_map))) {
    class_n_map <- data.frame(class_num = sort(unique(df$class_num)), n = NA_real_)
  }

  df <- merge(
    df,
    class_n_map[, c("class_num", "n"), drop = FALSE],
    by = "class_num",
    all.x = TRUE,
    sort = FALSE
  )

  df$n_cat  <- round(df$mean * safe_num(df$n), 0)
  df$pct_cat <- 100 * df$mean

  df$Class <- paste0("Class ", df$class_num)

  # ???????椰??????????????
  ord_var <- get_display_order(df$var_name)
  ord_cat <- get_category_order(df$var_name, level = df$category, label = df$Category)
  ord_cat[is.na(ord_cat)] <- 999999
  ord_cls <- safe_int(df$class_num)

  df <- df[order(ord_var, df$var_name, ord_cat, ord_cls), , drop = FALSE]
  rownames(df) <- NULL

  id_df <- unique(df[, c("var_name", "Variable", "category", "Category"), drop = FALSE])
  id_df <- id_df[order(
    get_display_order(id_df$var_name),
    get_category_order(id_df$var_name, level = id_df$category, label = id_df$Category)
  ), , drop = FALSE]

  out <- id_df[, c("Variable", "Category"), drop = FALSE]

  class_names <- sort(unique(df$Class))
  class_names <- class_names[order(safe_int(gsub("^Class ", "", class_names)))]

  key_out <- paste(id_df$var_name, id_df$category, sep = "||")

  for (cc in class_names) {
    sub <- df[df$Class == cc, , drop = FALSE]
    key_sub <- paste(sub$var_name, sub$category, sep = "||")
    idx <- match(key_out, key_sub)

    out[[twoline_key("n", cc)]] <- ifelse(is.na(idx), "", fmt_n0(sub$n_cat[idx]))
    out[[twoline_key("%", cc)]] <- ifelse(is.na(idx), "", fmt_pct1_plain(sub$pct_cat[idx]))
  }

  # variable ??????????밸븶筌믩끃??獄???????멥렑?????????????????????μ떜媛?걫???????????濾????????????????븐뼐?????????룸챷援??????
  dup_var <- duplicated(id_df$var_name)
  out$Variable[dup_var] <- ""

  out
}

build_T4_profile_raw <- function() {
  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
    build_lca_t4_twoline(T3_indicator_profile)
  } else {
    df <- resolve_lpa_profile_source(prefer_z = FALSE)
    if ("category" %in% names(df) && any(nzchar(trimws(as.character(df$category))), na.rm = TRUE)) {
      build_mixed_profile_split(df)
    } else {
      build_profile_wide_from_T3_twoline(df)
    }
  }
}

build_A3_profile_raw_wide <- function() {
  df <- resolve_lpa_profile_source(prefer_z = FALSE)
  if (nrow(df) == 0) return(data.frame())

  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
    return(build_lca_t4_twoline(df))
  }
  if ("category" %in% names(df) && any(nzchar(trimws(as.character(df$category))), na.rm = TRUE)) {
    return(build_mixed_profile_split(df))
  }

  out <- build_profile_wide_from_T3(df)
  if (isTRUE(HAS_WEIGHT)) {
    if (compact_mean_name(FALSE) %in% names(out)) names(out)[names(out) == compact_mean_name(FALSE)] <- compact_mean_name(TRUE)
  }
  out
}

build_A4_profile_z_wide <- function() {
  df <- resolve_lpa_profile_source(prefer_z = TRUE)
  if (nrow(df) == 0) return(data.frame())

  if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {
    return(build_lca_t4_twoline(df))
  }

  out <- build_profile_wide_from_T3(df)
  if (isTRUE(HAS_WEIGHT)) {
    if (compact_mean_name(FALSE) %in% names(out)) names(out)[names(out) == compact_mean_name(FALSE)] <- compact_mean_name(TRUE)
  }
  out
}

# ------------------------------------------------------------
# 7. R3STEP helpers
# ------------------------------------------------------------
standardize_rrr_table <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  if (!"var_name" %in% names(df) && "predictor" %in% names(df)) df$var_name <- as.character(df$predictor)
  if (!"var_label" %in% names(df) && "var_name" %in% names(df)) df$var_label <- get_var_label(df$var_name)
  if (!"outcome_class" %in% names(df) && "comparison" %in% names(df)) {
    cmp_chr <- as.character(df$comparison)
    lhs_num <- suppressWarnings(as.integer(gsub("^.*Class\\s*([0-9]+).*$", "\\1", cmp_chr)))
    df$outcome_class <- ifelse(is.na(lhs_num), NA_character_, paste0("Class ", lhs_num))
  }
  if (!"reference_class" %in% names(df)) df$reference_class <- REFERENCE_CLASS
  if (!"level" %in% names(df)) df$level <- ""
  if (!"value_label" %in% names(df)) {
    df$value_label <- mapply(
      function(v, lv) {
        if (is.na(lv) || !nzchar(trimws(as.character(lv)))) return("")
        get_value_label(v, lv)
      },
      df$var_name, df$level
    )
  }
  if (!"predictor_type" %in% names(df)) df$predictor_type <- ""

  normalize_blank_chr <- function(x) {
    x <- as.character(x)
    x[is.na(x) | x %in% c("NA", "NaN", "NULL")] <- ""
    trimws(x)
  }

  df$var_name        <- normalize_blank_chr(df$var_name)
  df$var_label       <- normalize_blank_chr(df$var_label)
  df$level           <- normalize_blank_chr(df$level)
  df$value_label     <- normalize_blank_chr(df$value_label)
  df$outcome_class   <- normalize_blank_chr(df$outcome_class)
  df$reference_class <- normalize_blank_chr(df$reference_class)
  df$predictor_type  <- tolower(normalize_blank_chr(df$predictor_type))

  df$rrr             <- safe_num(df$rrr)
  df$llci            <- safe_num(df$llci)
  df$ulci            <- safe_num(df$ulci)
  df$p               <- safe_num(df$p)

  df$Category <- ifelse(
    nzchar(df$value_label),
    df$value_label,
    ifelse(nzchar(df$level), df$level, "")
  )

  df$Category[df$predictor_type %in% c("continuous", "numeric", "scale")] <- ""
  df$level[df$predictor_type %in% c("continuous", "numeric", "scale")]    <- ""
  df
}
build_covariate_long_common <- function(df) {
  df <- standardize_rrr_table(df)
  if (nrow(df) == 0) return(data.frame())

  # --------------------------------------------------
  # column aliases / safety
  # --------------------------------------------------
  if (!"var_name" %in% names(df) && "predictor" %in% names(df)) df$var_name <- df$predictor
  if (!"level" %in% names(df)) df$level <- ""
  if (!"Category" %in% names(df)) df$Category <- ""
  if (!"predictor_type" %in% names(df)) df$predictor_type <- ""
  if (!"var_label" %in% names(df) && "Variable" %in% names(df)) df$var_label <- df$Variable

  if (!"rrr"  %in% names(df) && "RRR"  %in% names(df)) df$rrr  <- df$RRR
  if (!"llci" %in% names(df) && "LLCI" %in% names(df)) df$llci <- df$LLCI
  if (!"ulci" %in% names(df) && "ULCI" %in% names(df)) df$ulci <- df$ULCI
  if (!"p"    %in% names(df) && "p_value" %in% names(df)) df$p <- df$p_value

  ref_class <- normalize_latent_group_text(REFERENCE_CLASS %||% latent_group_label(1))

  normalize_class_label_local <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("\\s+", " ", x)
    x
  }

  if (!"outcome_class" %in% names(df)) {
    stop("build_covariate_long_common(): outcome_class column is missing", call. = FALSE)
  }

  df$reference_class <- ref_class
  df$outcome_class <- as.character(df$outcome_class)

  ref_norm <- normalize_class_label_local(ref_class)
  out_norm <- normalize_class_label_local(df$outcome_class)

  # self-comparison ???????????????
  df <- df[nzchar(out_norm) & out_norm != ref_norm, , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  df$outcome_class <- normalize_latent_group_text(df$outcome_class)
  df$Comparison <- paste0(df$outcome_class, " vs ", ref_class)

  # --------------------------------------------------
  # dictionary ???????????????base_rows ?????????ш끽維뽳쭩?뱀땡???얩맪???????????
  # --------------------------------------------------

  build_base_rows_from_dict <- function() {

    if (!is.data.frame(DICT_META) || nrow(DICT_META) == 0) {
      return(unique(df[, c("var_name", "var_label", "level", "Category", "predictor_type"), drop = FALSE]))
    }

    meta <- DICT_META[
      tolower(as.character(DICT_META$role)) == "covariate" &
        tolower(as.character(DICT_META$use)) %in% c("true", "t", "1"),
      ,
      drop = FALSE
    ]

    if (nrow(meta) == 0) {
      return(unique(df[, c("var_name", "var_label", "level", "Category", "predictor_type"), drop = FALSE]))
    }

    res_list <- list()

    for (i in seq_len(nrow(meta))) {
      vn   <- as.character(meta$var_name[i])
      vlab <- get_var_label(vn)
      typ  <- tolower(as.character(meta$type[i] %||% ""))

      if (typ == "continuous") {
        res_list[[length(res_list) + 1]] <- data.frame(
          var_name         = vn,
          var_label        = vlab,
          level            = "",
          Category         = "",
          predictor_type   = typ,
          stringsAsFactors = FALSE
        )
        next
      }

      lvl_hit <- data.frame()
      if (is.data.frame(DICT_LEVELS) && nrow(DICT_LEVELS) > 0) {
        lvl_hit <- DICT_LEVELS[as.character(DICT_LEVELS$var_name) == vn, , drop = FALSE]
      }

      if (nrow(lvl_hit) > 0) {
        rows_i <- data.frame(
          var_name         = vn,
          var_label        = vlab,
          level            = as.character(lvl_hit$value),
          Category         = as.character(
            if ("value_label" %in% names(lvl_hit)) lvl_hit$value_label else lvl_hit$value
          ),
          predictor_type   = typ,
          stringsAsFactors = FALSE
        )
        rows_i$level[is.na(rows_i$level)] <- ""
        rows_i$Category[is.na(rows_i$Category)] <- ""
        res_list[[length(res_list) + 1]] <- rows_i
      } else {
        res_list[[length(res_list) + 1]] <- data.frame(
          var_name         = vn,
          var_label        = vlab,
          level            = "",
          Category         = "",
          predictor_type   = typ,
          stringsAsFactors = FALSE
        )
      }
    }

    out <- do.call(rbind, res_list)
    rownames(out) <- NULL
    out
  }

  base_rows <- build_base_rows_from_dict()

  # --------------------------------------------------
  # continuous ???????????ㅻ깹??????dummy row ???????????????
  # --------------------------------------------------
  is_cont_var <- function(vn) {
    hit <- DICT_META[as.character(DICT_META$var_name) == vn, , drop = FALSE]
    if (nrow(hit) == 0) return(FALSE)

    typ <- ""
    if ("type" %in% names(hit)) typ <- tolower(as.character(hit$type[1]))

    typ %in% c("continuous", "numeric", "scale")
  }

  cont_vars <- unique(base_rows$var_name[vapply(base_rows$var_name, is_cont_var, logical(1))])

  if (length(cont_vars) > 0) {
    for (vn in cont_vars) {
      idx <- which(base_rows$var_name == vn)
      if (length(idx) > 1) {
        keep <- idx[1]
        drop <- setdiff(idx, keep)
        base_rows <- base_rows[-drop, , drop = FALSE]

        base_rows$level[keep]    <- ""
        base_rows$Category[keep] <- ""
      }
    }
  }

  # --------------------------------------------------
  # reference level ???????????ㅻ깹??????
  # --------------------------------------------------
  if ("reference_level" %in% names(df)) {
    ref_rows <- unique(df[, c("var_name", "var_label", "reference_level", "predictor_type"), drop = FALSE])
    names(ref_rows)[names(ref_rows) == "reference_level"] <- "level"

    ref_rows$Category <- mapply(
      function(v, lv, typ) {
        if (tolower(as.character(typ)) == "continuous") return("")
        if (is.na(lv) || !nzchar(trimws(as.character(lv)))) return("")
        get_value_label(v, lv)
      },
      ref_rows$var_name, ref_rows$level, ref_rows$predictor_type
    )

    ref_rows <- ref_rows[, c("var_name", "var_label", "level", "Category", "predictor_type"), drop = FALSE]

    ref_key_new <- paste(as.character(ref_rows$var_name), as.character(ref_rows$level), sep = "||")
    ref_key_old <- paste(as.character(base_rows$var_name), as.character(base_rows$level), sep = "||")
    ref_rows <- ref_rows[!ref_key_new %in% ref_key_old, , drop = FALSE]

    if (nrow(ref_rows) > 0) {
      base_rows <- rbind(base_rows, ref_rows)
    }
  }

  # --------------------------------------------------
  # univariable parse???????extra row ???????????ㅻ깹??????
  # --------------------------------------------------
  uni_df <- safe_df(R3STEP_RESULTS_RAW$univariable %||% data.frame())
  if (nrow(uni_df) > 0) {
    if (!"var_name" %in% names(uni_df) && "predictor" %in% names(uni_df)) {
      uni_df$var_name <- as.character(uni_df$predictor)
    }
    if (!"level" %in% names(uni_df)) uni_df$level <- ""
    if (!"predictor_type" %in% names(uni_df)) uni_df$predictor_type <- ""
    if (!"value_label" %in% names(uni_df)) {
      uni_df$value_label <- mapply(
        function(v, lv) {
          if (is.na(lv) || !nzchar(trimws(as.character(lv)))) return("")
          get_value_label(v, lv)
        },
        uni_df$var_name, uni_df$level
      )
    }

    extra_rows <- unique(data.frame(
      var_name         = as.character(uni_df$var_name),
      var_label        = get_var_label(uni_df$var_name),
      level            = as.character(uni_df$level),
      Category         = as.character(uni_df$value_label),
      predictor_type   = as.character(uni_df$predictor_type),
      stringsAsFactors = FALSE
    ))

    extra_key   <- paste(as.character(extra_rows$var_name), as.character(extra_rows$level), sep = "||")
    base_key    <- paste(as.character(base_rows$var_name),  as.character(base_rows$level),  sep = "||")
    extra_rows  <- extra_rows[!extra_key %in% base_key, , drop = FALSE]

    if (nrow(extra_rows) > 0) {
      base_rows <- rbind(base_rows, extra_rows)
    }
  }

  # --------------------------------------------------
  # type / category ???????椰????????????
  # --------------------------------------------------
  base_rows$var_name       <- as_chr_flat(base_rows$var_name)
  base_rows$var_label      <- as_chr_flat(base_rows$var_label)
  base_rows$level          <- as_chr_flat(base_rows$level)
  base_rows$Category       <- as_chr_flat(base_rows$Category)
  base_rows$predictor_type <- tolower(as_chr_flat(base_rows$predictor_type))

  base_rows$Category[is.na(base_rows$Category)] <- ""
  base_rows$level[is.na(base_rows$level)]       <- ""

  fill_idx <- base_rows$Category == "" & base_rows$level != ""
  if (any(fill_idx)) {
    base_rows$Category[fill_idx] <- mapply(
      get_value_label,
      base_rows$var_name[fill_idx],
      base_rows$level[fill_idx]
    )
  }

  is_cont <- !is.na(base_rows$predictor_type) &
    nzchar(trimws(base_rows$predictor_type)) &
    tolower(trimws(base_rows$predictor_type)) == "continuous"

  base_rows$Category[is_cont] <- ""

  need_fill <- (is.na(base_rows$Category) | base_rows$Category == "") &
    !is.na(base_rows$level) & base_rows$level != ""

  if (any(need_fill)) {
    base_rows$Category[need_fill] <- mapply(
      get_value_label,
      base_rows$var_name[need_fill],
      base_rows$level[need_fill]
    )
  }

  # --------------------------------------------------
  # ???????椰??????????????+ ???????????롪퍓梨????????????????????????????????
  # --------------------------------------------------
  var_name_chr <- as_chr_flat(base_rows$var_name)
  level_chr    <- as_chr_flat(base_rows$level)
  cat_chr      <- as_chr_flat(base_rows$Category)

  ord0 <- order(
    get_display_order(var_name_chr),
    get_category_order(var_name_chr, level = level_chr, label = cat_chr),
    cat_chr
  )
  base_rows <- base_rows[ord0, , drop = FALSE]

  dup_key <- paste(as_chr_flat(base_rows$var_name), as_chr_flat(base_rows$level), sep = "||")
  base_rows <- base_rows[!duplicated(dup_key), , drop = FALSE]
  rownames(base_rows) <- NULL

  base_rows$Variable <- get_var_label(base_rows$var_name)

  # --------------------------------------------------
  # reference ??????
  # --------------------------------------------------
  base_rows$is_ref <- FALSE

  if ("reference_level" %in% names(df)) {

    ref_key <- unique(df[, c("var_name", "reference_level", "predictor_type"), drop = FALSE])
    names(ref_key)[2] <- "level"

    ref_key$var_name <- as_chr_flat(ref_key$var_name)
    ref_key$level <- as_chr_flat(ref_key$level)
    ref_key$predictor_type <- tolower(as_chr_flat(ref_key$predictor_type))

    ref_key <- ref_key[ref_key$predictor_type != "continuous", , drop = FALSE]

    if (nrow(ref_key) > 0) {
      ref_key$is_ref_ref <- TRUE

      base_rows <- merge(
        base_rows,
        ref_key[, c("var_name", "level", "is_ref_ref"), drop = FALSE],
        by = c("var_name", "level"),
        all.x = TRUE,
        sort = FALSE
      )

      base_rows$is_ref <- ifelse(is.na(base_rows$is_ref_ref), FALSE, TRUE)
      base_rows$is_ref_ref <- NULL
    }
  }

  # --------------------------------------------------
  # comparison block????????꿔꺂???⑸븶?????????merge
  # --------------------------------------------------
  block_levels <- unique(df$Comparison)
  block_num    <- extract_class_num_from_text(block_levels)
  block_levels <- block_levels[order(block_num)]

  out_list <- vector("list", length(block_levels))

  for (i in seq_along(block_levels)) {
    bl     <- block_levels[i]
    sub_df <- df[df$Comparison == bl, , drop = FALSE]

    need_cols <- c("var_name", "level", "rrr", "llci", "ulci", "p")
    miss_cols <- setdiff(need_cols, names(sub_df))
    if (length(miss_cols) > 0) {
      stop(
        paste0("build_covariate_long_common(): missing columns in sub_df -> ",
               paste(miss_cols, collapse = ", ")),
        call. = FALSE
      )
    }

    tmp <- merge(
      base_rows,
      sub_df[, need_cols, drop = FALSE],
      by = c("var_name", "level"),
      all.x = TRUE,
      sort = FALSE
    )

    if (!"is_ref" %in% names(tmp)) tmp$is_ref <- FALSE

    tmp$Comparison <- bl
    tmp$RRR        <- ifelse(tmp$is_ref, "1.000", ifelse(is.na(tmp$rrr), "", fmt_rrr3(tmp$rrr)))
    tmp$LLCI       <- ifelse(tmp$is_ref, "", ifelse(is.na(tmp$llci), "", fmt_rrr3(tmp$llci)))
    tmp$ULCI       <- ifelse(tmp$is_ref, "", ifelse(is.na(tmp$ulci), "", fmt_rrr3(tmp$ulci)))
    tmp$p_out      <- ifelse(tmp$is_ref, "", ifelse(is.na(tmp$p), "", fmt_p3_strict(tmp$p)))
    tmp$sig        <- ifelse(tmp$is_ref, "", ifelse(is.na(tmp$p), "", sig_mark(tmp$p)))

    out_list[[i]] <- tmp[, c(
      "Comparison", "var_name", "Variable", "level", "Category",
      "RRR", "LLCI", "ULCI", "p_out", "sig"
    ), drop = FALSE]
  }

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  names(out)[names(out) == "p_out"] <- "p"

  ord_var <- get_display_order(out$var_name)
  ord_cat <- get_category_order(out$var_name, level = out$level, label = out$Category)
  ord_cat[is.na(ord_cat)] <- 999999
  cmp_num <- extract_class_num_from_text(out$Comparison)

  out <- out[order(cmp_num, ord_var, out$var_name, ord_cat), , drop = FALSE]
  rownames(out) <- NULL

  out
}

format_t5_twoline <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) {
    return(data.frame(
      Variable = character(),
      Category = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (!"Variable" %in% names(df)) {
    cand <- intersect(c("var_label", "variable", "Label"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- "Variable"
  }
  if (!"Comparison" %in% names(df)) {
    cand <- intersect(c("contrast", "comparison"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- "Comparison"
  }
  if (!"Category" %in% names(df)) df$Category <- ""
  if (!"var_name" %in% names(df)) df$var_name <- df$Variable
  if (!"level" %in% names(df)) df$level <- ""
  if (!"predictor_type" %in% names(df)) df$predictor_type <- ""

  if (!all(c("Variable", "Comparison", "Category", "var_name", "level") %in% names(df))) {
    return(data.frame())
  }

  df$Comparison <- normalize_latent_group_text(as.character(df$Comparison))
  df$Comparison <- gsub(
    paste0("^", latent_group_term(), "\\s*([0-9]+)\\s*vs\\s*", latent_group_term(), "\\s*([0-9]+)$"),
    paste0(latent_group_term(), " \\1 vs ", latent_group_term(), " \\2"),
    df$Comparison,
    ignore.case = TRUE
  )

  df$var_name   <- as.character(df$var_name)
  df$Variable   <- as.character(df$Variable)
  df$level      <- as.character(df$level)
  df$Category   <- as.character(df$Category)

  need_fill <- is.na(df$Category) | !nzchar(trimws(df$Category))
  if (any(need_fill)) {
    df$Category[need_fill] <- mapply(
      get_value_label,
      df$var_name[need_fill],
      df$level[need_fill]
    )
  }

  # dictionary ?????????????????????????獄쏅챶留덌┼??????????????筌롈살젔??covariate ???????????ㅻ깹??????
  if (is.data.frame(DICT_META) && nrow(DICT_META) > 0) {
    vars_in_df <- DICT_META$var_name[
      tolower(as.character(DICT_META$role)) == "covariate" &
        tolower(as.character(DICT_META$use)) %in% c("true", "t", "1")
    ]
  } else {
    vars_in_df <- unique(df$var_name)
  }

  vars_in_df <- unique(as.character(vars_in_df))
  vars_in_df <- vars_in_df[!is.na(vars_in_df) & nzchar(vars_in_df)]

  build_dict_rows_one <- function(vn) {
    vlab <- get_var_label(vn)

    meta_hit <- data.frame()
    if (is.data.frame(DICT_META) && nrow(DICT_META) > 0 && "var_name" %in% names(DICT_META)) {
      meta_hit <- DICT_META[as.character(DICT_META$var_name) == vn, , drop = FALSE]
    }

    if (nrow(meta_hit) == 0) {
      sub <- unique(df[df$var_name == vn, c("var_name", "Variable", "level", "Category"), drop = FALSE])
      if (nrow(sub)                == 0) {
        return(data.frame(
          var_name                 = vn,
          Variable                 = vlab,
          level                    = "",
          Category                 = "",
          stringsAsFactors         = FALSE
        ))
      }
      sub$Variable <- vlab
      return(sub[, c("var_name", "Variable", "level", "Category"), drop = FALSE])
    }

    typ   <- ""
    if ("measure_type" %in% names(meta_hit)) {
      typ <- tolower(as.character(meta_hit$measure_type[1]))
    } else if ("type" %in% names(meta_hit)) {
      typ <- tolower(as.character(meta_hit$type[1]))
    }
    if (is.na(typ) || !nzchar(typ)) typ <- ""

    # --------------------------------------------------
    # continuous ???????????ㅻ깹???????????轅붽틓????몃마??????????룸챷援???????category row????????濾????????????? ???????????濡?씀?濾????
    # --------------------------------------------------
    if (typ %in% c("continuous", "numeric", "scale")) {
      return(data.frame(
        var_name = vn,
        Variable = vlab,
        level = "",
        Category = "",
        stringsAsFactors = FALSE
      ))
    }

    if (is.data.frame(DICT_LEVELS) &&
        nrow(DICT_LEVELS) > 0 &&
        all(c("var_name", "value") %in% names(DICT_LEVELS))) {
      lvl_hit <- DICT_LEVELS[as.character(DICT_LEVELS$var_name) == vn, , drop = FALSE]
      if (nrow(lvl_hit) > 0) {
        lab_col <- first_existing_col(lvl_hit, c("value_label", "label", "label_en", "label_ko", "value"))
        rows_i <- data.frame(
          var_name = vn,
          Variable = vlab,
          level = as.character(lvl_hit$value),
          Category = if (!is.na(lab_col)) as.character(lvl_hit[[lab_col]]) else as.character(lvl_hit$value),
          stringsAsFactors = FALSE
        )
        rows_i$level[is.na(rows_i$level)] <- ""
        rows_i$Category[is.na(rows_i$Category)] <- ""
        rows_i <- rows_i[nzchar(rows_i$level) | nzchar(rows_i$Category), , drop = FALSE]
        if (nrow(rows_i) > 0) return(rows_i)
      }
    }

    # --------------------------------------------------
    # categorical / binary / ordinal??dictionary level ???????椰??????????
    # --------------------------------------------------
    value_cols <- grep("^value_[0-9]+$", names(meta_hit), value = TRUE)
    idxs       <- suppressWarnings(as.integer(gsub("^value_([0-9]+)$", "\\1", value_cols)))
    idxs       <- sort(idxs[!is.na(idxs)])

    rows       <- lapply(idxs, function(k) {
      vcol     <- paste0("value_", k)
      lcol     <- paste0("label_", k)

      vv <- if (vcol %in% names(meta_hit)) as.character(meta_hit[[vcol]][1]) else NA_character_
      ll <- if (lcol %in% names(meta_hit)) as.character(meta_hit[[lcol]][1]) else NA_character_

      if (is.na(vv) || !nzchar(trimws(vv))) return(NULL)

      cat_lab <- if (!is.na(ll) && nzchar(trimws(ll))) trimws(ll) else trimws(vv)

      data.frame(
        var_name         = vn,
        Variable         = vlab,
        level            = trimws(vv),
        Category         = cat_lab,
        stringsAsFactors = FALSE
      )
    })

    rows <- Filter(Negate(is.null), rows)

    if (length(rows)     == 0) {
      return(data.frame(
        var_name         = vn,
        Variable         = vlab,
        level            = "",
        Category         = "",
        stringsAsFactors = FALSE
      ))
    }

    out <- do.call(rbind, rows)
    out$Variable <- vlab
    out
  }

  id_df <- do.call(rbind, lapply(vars_in_df, build_dict_rows_one))

  extra_df <- unique(df[, c("var_name", "Variable", "level", "Category"), drop = FALSE])
  id_df <- safe_rbind(id_df, extra_df)

  id_df$var_name <- as.character(id_df$var_name)
  id_df$Variable <- as.character(id_df$Variable)
  id_df$level    <- as.character(id_df$level)
  id_df$Category <- as.character(id_df$Category)

  id_df <- id_df[!duplicated(paste(id_df$var_name, id_df$level, sep = "||")), , drop = FALSE]

  has_detail <- ave(nzchar(id_df$level) | nzchar(id_df$Category), id_df$var_name, FUN = function(z) any(z, na.rm = TRUE))
  dummy_row <- !nzchar(id_df$level) & !nzchar(id_df$Category)
  id_df <- id_df[!(has_detail & dummy_row), , drop = FALSE]

  ord_var <- get_display_order(id_df$var_name)
  ord_cat <- get_category_order(id_df$var_name, level = id_df$level, label = id_df$Category)
  ord_cat[is.na(ord_cat)] <- 999999

  id_df           <- id_df[order(ord_var, ord_cat, id_df$Category), , drop = FALSE]
  rownames(id_df) <- NULL

  out             <- id_df[, c("Variable", "Category"), drop = FALSE]

  comp_names      <- unique(df$Comparison)
  comp_num        <- extract_class_num_from_text(comp_names)
  comp_names      <- comp_names[order(comp_num)]

  key_out         <- paste(id_df$var_name, id_df$level, sep = "||")

  for (gg in comp_names) {
    sub <- df[df$Comparison == gg, , drop = FALSE]
    key_sub <- paste(sub$var_name, sub$level, sep = "||")
    idx <- match(key_out, key_sub)

    out[[twoline_key("RRR",  gg)]] <- ifelse(is.na(idx), "", as.character(sub$RRR[idx]))
    out[[twoline_key("LLCI", gg)]] <- ifelse(is.na(idx), "", as.character(sub$LLCI[idx]))
    out[[twoline_key("ULCI", gg)]] <- ifelse(is.na(idx), "", as.character(sub$ULCI[idx]))
    out[[twoline_key("p",    gg)]] <- ifelse(is.na(idx), "", as.character(sub$p[idx]))
    out[[twoline_key("sig",  gg)]] <- ifelse(is.na(idx), "", as.character(sub$sig[idx]))
  }

  out$Variable[duplicated(id_df$var_name)] <- ""

  keep_row <- apply(out, 1, function(r) any(nzchar(trimws(as.character(r)))))
  out <- out[keep_row, , drop = FALSE]

  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 8. T5 / T5b / T5c / T5d
# ------------------------------------------------------------
build_T5_naive <- function() {
  df <- build_covariate_long_common(T5_RRR_RAW)
  if (nrow(df) == 0) return(data.frame())
  format_t5_twoline(df)
}

build_T5b_postwgt <- function() {
  df <- safe_df(T5_RRR_RAW)
  if (nrow(df) == 0 && is.list(R3STEP_RESULTS_RAW)) {
    df <- safe_df(R3STEP_RESULTS_RAW$T5_rrr %||% R3STEP_RESULTS_RAW$univariable)
  }
  df <- build_covariate_long_common(df)
  if (nrow(df) == 0) return(data.frame())
  format_t5_twoline(df)
}

build_T5c_LTRBLT <- function() {
  df <- build_covariate_long_common(
    safe_df(R3STEP_RESULTS_RAW$modal %||% data.frame())
  )

  if (nrow(df) == 0) {
    out <- make_empty_twoline_rrr()
    return(out)
  }

  out <- format_t5_twoline(df)
  out <- safe_df(out)

  if (nrow(out) == 0 && ncol(out) == 0) {
    out <- make_empty_twoline_rrr()
  }

  out
}

build_T5d_compare <- function() {
  naive_long   <- build_covariate_long_common(safe_df(R3STEP_RESULTS_RAW$univariable %||% data.frame()))
  primary_long <- build_covariate_long_common(T5_RRR_RAW)
  modal_long   <- build_covariate_long_common(safe_df(R3STEP_RESULTS_RAW$modal %||% data.frame()))

  prep <- function(df, suffix) {
    df <- safe_df(df)
    if (nrow(df) == 0) return(data.frame())

    out <- df[, c("Comparison", "var_name", "Variable", "level", "Category",
                  "RRR", "LLCI", "ULCI", "p", "sig"), drop = FALSE]
    names(out)[6:10] <- paste0(c("RRR_", "LLCI_", "ULCI_", "p_", "sig_"), suffix)
    out
  }

  b1 <- prep(naive_long, "naive")
  b2 <- prep(primary_long, "primary")
  b3 <- prep(modal_long, "modal")

  base_long <- if (nrow(primary_long) > 0) primary_long else if (nrow(naive_long) > 0) naive_long else modal_long
  base_long <- safe_df(base_long)
  if (nrow(base_long) == 0) {
    return(make_empty_compare_table())
  }

  base_rows <- unique(base_long[, c("var_name", "Variable", "level", "Category"), drop = FALSE])
  ord_var <- get_display_order(base_rows$var_name)
  ord_cat <- get_category_order(base_rows$var_name, level = base_rows$level, label = base_rows$Category)
  ord_cat[is.na(ord_cat)] <- 999999
  base_rows <- base_rows[order(ord_var, ord_cat, base_rows$Category), , drop = FALSE]
  rownames(base_rows) <- NULL

  has_detail <- ave(nzchar(base_rows$level) | nzchar(base_rows$Category), base_rows$var_name, FUN = function(z) any(z, na.rm = TRUE))
  dummy_row <- !nzchar(base_rows$level) & !nzchar(base_rows$Category)
  base_rows <- base_rows[!(has_detail & dummy_row), , drop = FALSE]

  comp_names <- unique(c(
    as.character(b1$Comparison %||% character(0)),
    as.character(b2$Comparison %||% character(0)),
    as.character(b3$Comparison %||% character(0))
  ))
  comp_names <- comp_names[nzchar(comp_names)]
  cmp_num <- suppressWarnings(as.integer(gsub("^Class\\s*([0-9]+).*", "\\1", comp_names)))
  comp_names <- comp_names[order(cmp_num)]

  block_list <- vector("list", length(comp_names))
  key_base <- paste(base_rows$var_name, base_rows$level, sep = "||")

  for (ii in seq_along(comp_names)) {
    cmp <- comp_names[ii]
    tmp <- base_rows
    tmp$Comparison <- cmp

    add_block <- function(src, suffix) {
      src <- safe_df(src)
      if (nrow(src) == 0) return(tmp)
      sub <- src[src$Comparison == cmp, , drop = FALSE]
      sub_key <- paste(sub$var_name, sub$level, sep = "||")
      idx <- match(key_base, sub_key)
      tmp[[paste0("RRR_", suffix)]]  <<- ifelse(is.na(idx), "", as.character(sub$RRR[idx]))
      tmp[[paste0("LLCI_", suffix)]] <<- ifelse(is.na(idx), "", as.character(sub$LLCI[idx]))
      tmp[[paste0("ULCI_", suffix)]] <<- ifelse(is.na(idx), "", as.character(sub$ULCI[idx]))
      tmp[[paste0("p_", suffix)]]    <<- ifelse(is.na(idx), "", as.character(sub$p[idx]))
      tmp[[paste0("sig_", suffix)]]  <<- ifelse(is.na(idx), "", as.character(sub$sig[idx]))
      tmp
    }

    tmp <- add_block(b1, "naive")
    tmp <- add_block(b2, "primary")
    if (nrow(b3) > 0) tmp <- add_block(b3, "modal")

    tmp <- format_variable_category_block(
      df = tmp,
      var_col = "var_name",
      var_label_col = "Variable",
      cat_col = "Category",
      level_col = "level"
    )

    if (nrow(tmp) > 1) tmp$Comparison[2:nrow(tmp)] <- ""
    block_list[[ii]] <- tmp
  }

  out_disp <- do.call(rbind, block_list)
  rownames(out_disp) <- NULL

  if ("var_name" %in% names(out_disp)) out_disp$var_name <- NULL
  if ("level" %in% names(out_disp)) out_disp$level <- NULL

  # --------------------------------------------------
  # T5d??twoline writer??????????? ??????????"__" ???????????????⑤벡瑜???????????????????ㅻ깹??????
  # --------------------------------------------------
  names(out_disp) <- gsub("^RRR_naive$",    "RRR__Naive",    names(out_disp))
  names(out_disp) <- gsub("^LLCI_naive$",   "LLCI__Naive",   names(out_disp))
  names(out_disp) <- gsub("^ULCI_naive$",   "ULCI__Naive",   names(out_disp))
  names(out_disp) <- gsub("^p_naive$",      "p__Naive",      names(out_disp))
  names(out_disp) <- gsub("^sig_naive$",    "sig__Naive",    names(out_disp))

  names(out_disp) <- gsub("^RRR_primary$",  "RRR__Primary",  names(out_disp))
  names(out_disp) <- gsub("^LLCI_primary$", "LLCI__Primary", names(out_disp))
  names(out_disp) <- gsub("^ULCI_primary$", "ULCI__Primary", names(out_disp))
  names(out_disp) <- gsub("^p_primary$",    "p__Primary",    names(out_disp))
  names(out_disp) <- gsub("^sig_primary$",  "sig__Primary",  names(out_disp))

  names(out_disp) <- gsub("^RRR_modal$",    "RRR__Modal",    names(out_disp))
  names(out_disp) <- gsub("^LLCI_modal$",   "LLCI__Modal",   names(out_disp))
  names(out_disp) <- gsub("^ULCI_modal$",   "ULCI__Modal",   names(out_disp))
  names(out_disp) <- gsub("^p_modal$",      "p__Modal",      names(out_disp))
  names(out_disp) <- gsub("^sig_modal$",    "sig__Modal",    names(out_disp))

  lead_cols <- c("Comparison", "Variable", "Category")
  other_cols <- setdiff(names(out_disp), lead_cols)
  out_disp <- out_disp[, c(lead_cols[lead_cols %in% names(out_disp)], other_cols), drop = FALSE]

  out_disp
}

# ------------------------------------------------------------
# 9. BCH / T6 / T7
# ------------------------------------------------------------
parse_bch_posthoc_pairs <- function(pair_text = "", pairs_df = NULL, alpha = .05) {
  out <- data.frame(class1 = integer(0), class2 = integer(0), stringsAsFactors = FALSE)
  if (is.data.frame(pairs_df) && nrow(pairs_df) > 0 && all(c("class1", "class2") %in% names(pairs_df))) {
    use <- pairs_df
    if ("p" %in% names(use)) {
      p_num <- suppressWarnings(as.numeric(use$p))
      use <- use[!is.na(p_num) & p_num < alpha, , drop = FALSE]
    }
    if (nrow(use) > 0) {
      out <- data.frame(
        class1 = suppressWarnings(as.integer(use$class1)),
        class2 = suppressWarnings(as.integer(use$class2)),
        stringsAsFactors = FALSE
      )
    }
  }
  pair_text <- paste(as.character(pair_text %||% ""), collapse = ", ")
  hits <- gregexpr("C\\s*([0-9]+)\\s*-\\s*C\\s*([0-9]+)", pair_text, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(pair_text, hits)[[1]]
  if (length(parts) > 0 && !identical(parts, character(0))) {
    parsed <- do.call(rbind, lapply(parts, function(x) {
      tok <- regmatches(x, regexec("C\\s*([0-9]+)\\s*-\\s*C\\s*([0-9]+)", x, perl = TRUE, ignore.case = TRUE))[[1]]
      if (length(tok) < 3) return(NULL)
      data.frame(class1 = as.integer(tok[2]), class2 = as.integer(tok[3]), stringsAsFactors = FALSE)
    }))
    if (is.data.frame(parsed) && nrow(parsed) > 0) out <- rbind(out, parsed)
  }
  out <- out[!is.na(out$class1) & !is.na(out$class2) & out$class1 != out$class2, , drop = FALSE]
  if (nrow(out) == 0) return(out)
  out$key <- ifelse(out$class1 < out$class2, paste(out$class1, out$class2, sep = "-"), paste(out$class2, out$class1, sep = "-"))
  out <- out[!duplicated(out$key), c("class1", "class2"), drop = FALSE]
  rownames(out) <- NULL
  out
}

bch_ordered_posthoc_notation <- function(means, pair_text = "", pairs_df = NULL, alpha = .05, label_prefix = "Profile ") {
  means <- suppressWarnings(as.numeric(means))
  class_ids <- suppressWarnings(as.integer(gsub("[^0-9]+", "", names(means))))
  if (length(class_ids) != length(means) || all(is.na(class_ids))) class_ids <- seq_along(means)
  keep <- !is.na(class_ids) & !is.na(means)
  class_ids <- class_ids[keep]
  means <- means[keep]
  if (length(class_ids) < 2L) return("")
  names(means) <- as.character(class_ids)
  pairs <- parse_bch_posthoc_pairs(pair_text = pair_text, pairs_df = pairs_df, alpha = alpha)
  if (!is.data.frame(pairs) || nrow(pairs) == 0) return("")
  pair_keys <- unique(ifelse(pairs$class1 < pairs$class2, paste(pairs$class1, pairs$class2, sep = "-"), paste(pairs$class2, pairs$class1, sep = "-")))
  ordered <- names(sort(means, decreasing = TRUE))
  display <- function(x) paste0(label_prefix, as.integer(x))
  statements <- character(0)
  for (i in seq_along(ordered)) {
    higher <- ordered[[i]]
    if (i >= length(ordered)) next
    lower <- character(0)
    for (j in seq.int(i + 1L, length(ordered))) {
      candidate <- ordered[[j]]
      key <- if (as.integer(higher) < as.integer(candidate)) paste(higher, candidate, sep = "-") else paste(candidate, higher, sep = "-")
      if (key %in% pair_keys && means[[higher]] > means[[candidate]]) {
        lower <- c(lower, display(candidate))
      }
    }
    if (length(lower) > 0) {
      statements <- c(statements, sprintf("%s>%s", display(higher), paste(lower, collapse = ", ")))
    }
  }
  paste(statements, collapse = "\n")
}

extract_bch_means_for_variable <- function(full_df, var_name = NULL) {
  full_df <- safe_df(full_df)
  if (nrow(full_df) == 0) return(numeric(0))
  var_col <- first_existing_col(full_df, c("var_name", "Variable", "outcome", "distal", "y"))
  if (!is.na(var_col) && nzchar(as.character(var_name %||% ""))) {
    full_df <- full_df[as.character(full_df[[var_col]]) == as.character(var_name), , drop = FALSE]
  }
  if ("result_type" %in% names(full_df)) {
    class_rows <- full_df[as.character(full_df$result_type) == "class_estimate", , drop = FALSE]
    if (nrow(class_rows) > 0) full_df <- class_rows
  }
  class_col <- first_existing_col(full_df, c("class_num", "class", "Profile"))
  mean_col <- first_existing_col(full_df, c("Mean", "mean", "M", "estimate"))
  if (is.na(class_col) || is.na(mean_col)) return(numeric(0))
  class_ids <- suppressWarnings(as.integer(gsub("[^0-9]+", "", as.character(full_df[[class_col]]))))
  means <- suppressWarnings(as.numeric(full_df[[mean_col]]))
  keep <- !is.na(class_ids) & !is.na(means)
  means <- means[keep]
  names(means) <- as.character(class_ids[keep])
  means[!duplicated(names(means))]
}

extract_bch_posthoc_text <- function(var_name, full_df = BCH_RESULTS_FULL) {

  post_df <- load_step_rds("BCH_POSTHOC", dir_rds = DIR_RDS, default = data.frame())

  if (!is.data.frame(post_df) || nrow(post_df) == 0) return("")

  hit <- post_df[post_df$outcome == var_name & post_df$p < .05, , drop = FALSE]

  if (nrow(hit) == 0) return("")

  parts <- paste0("C", hit$class1, "-C", hit$class2)
  pair_text <- paste(parts, collapse = ", ")
  ordered <- bch_ordered_posthoc_notation(
    extract_bch_means_for_variable(full_df, var_name),
    pair_text = pair_text
  )
  if (nzchar(ordered)) ordered else pair_text
}

format_t6_bch_twoline <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  if (!"Variable" %in% names(df)) {
    cand <- intersect(c("var_label", "variable", "Label"), names(df))
    if (length(cand) > 0) names(df)[match(cand[1], names(df))] <- "Variable"
  }

  if (!"Profile" %in% names(df)) {
    if ("class" %in% names(df)) {
      df$Profile <- gsub("^class", "Profile ", as.character(df$class), ignore.case = TRUE)
    } else if ("class_num" %in% names(df)) {
      df$Profile <- paste0("Profile ", df$class_num)
    }
  }

  if (!"Mean" %in% names(df) && "estimate" %in% names(df)) df$Mean <- df$estimate
  if (!"SE" %in% names(df) && "se" %in% names(df)) df$SE <- df$se

  out <- unique(df[, c("Variable"), drop = FALSE])

  prof_names <- unique(df$Profile)
  prof_names <- prof_names[order(suppressWarnings(as.integer(gsub("^Profile ", "", prof_names))))]

  for (pp in prof_names) {
    sub <- df[df$Profile == pp, , drop = FALSE]
    out[[twoline_key("M",  pp)]] <- fmt_m2(sub$Mean[match(out$Variable, sub$Variable)])
    out[[twoline_key("SE", pp)]] <- fmt_sd2(sub$SE[match(out$Variable, sub$Variable)])
  }

  p_sig <- unique(df[, c("Variable", "p", "sig"), drop = FALSE])
  out[[twoline_key("p", "Overall")]]   <- fmt_p3_strict(p_sig$p[match(out$Variable, p_sig$Variable)])
  out[[twoline_key("sig", "Overall")]] <- fmt_sig_cell(p_sig$sig[match(out$Variable, p_sig$Variable)])

  if ("posthoc" %in% names(df)) {
    ph_map <- unique(df[, c("Variable", "posthoc"), drop = FALSE])
    ph_map$posthoc <- as.character(ph_map$posthoc)
    ph_map$posthoc[is.na(ph_map$posthoc)] <- ""
    out[[twoline_key("post-hoc", "Overall")]] <- ph_map$posthoc[match(out$Variable, ph_map$Variable)]
    out[[twoline_key("post-hoc", "Overall")]][is.na(out[[twoline_key("post-hoc", "Overall")]])] <- ""
  }

  out
}

build_classified_outcome_summary <- function(outcome_vars, moderator_var = NULL) {
  src <- safe_df(CLASSIFIED_ANALYSIS)
  if (nrow(src) == 0) src <- safe_df(ANALYSIS_DATA_CLASSIFIED)
  if (nrow(src) == 0) return(data.frame())
  if (!"class_num" %in% names(src)) return(data.frame())

  outcome_vars <- intersect(as.character(outcome_vars), names(src))
  if (length(outcome_vars) == 0) return(data.frame())

  src$class_num <- safe_int(src$class_num)
  src <- src[!is.na(src$class_num), , drop = FALSE]
  if (nrow(src) == 0) return(data.frame())

  if (!is.null(moderator_var) && nzchar(as.character(moderator_var)) && moderator_var %in% names(src)) {
    src$moderator_level <- as.character(src[[moderator_var]])
    keep_mod <- !is.na(src$moderator_level) & nzchar(src$moderator_level)
    src <- src[keep_mod, , drop = FALSE]
  } else {
    src$moderator_level <- ""
  }

  out <- list()
  idx <- 1L
  for (vn in outcome_vars) {
    x_all <- safe_num(src[[vn]])
    if (all(is.na(x_all))) next
    for (cl in sort(unique(src$class_num))) {
      sub_cl <- src[src$class_num == cl, , drop = FALSE]
      if (nrow(sub_cl) == 0) next
      lvls <- unique(as.character(sub_cl$moderator_level))
      for (lv in lvls) {
        sub <- sub_cl[as.character(sub_cl$moderator_level) == lv, , drop = FALSE]
        x <- safe_num(sub[[vn]])
        x <- x[!is.na(x)]
        if (length(x) == 0) next
        out[[idx]] <- data.frame(
          var_name = as.character(vn),
          class_num = as.integer(cl),
          moderator_level = as.character(lv),
          M = mean(x, na.rm = TRUE),
          SD = stats::sd(x, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }
  if (length(out) == 0) return(data.frame())
  do.call(rbind, out)
}

replace_t6_spread_with_sd <- function(out_df, outcome_vars, moderator_var = NULL) {
  out_df <- safe_df(out_df)
  if (isTRUE(HAS_WEIGHT) || nrow(out_df) == 0) return(out_df)

  reorder_t6_columns <- function(df) {
    preferred <- c("Variable", "Profile", "M", "SD", "Statistic", "p", "sig", "Post-hoc")
    preferred <- preferred[preferred %in% names(df)]
    remain <- setdiff(names(df), preferred)
    df[, c(preferred, remain), drop = FALSE]
  }

  summ <- build_classified_outcome_summary(outcome_vars = outcome_vars, moderator_var = moderator_var)
  if (nrow(summ) == 0) {
    if ("SE" %in% names(out_df) && !"SD" %in% names(out_df)) {
      names(out_df)[names(out_df) == "SE"] <- "SD"
    }
    return(reorder_t6_columns(out_df))
  }

  if ("Profile" %in% names(out_df)) {
    prof_num <- suppressWarnings(as.integer(gsub("^Profile\\s+", "", as.character(out_df$Profile))))
    if (!"Variable" %in% names(out_df) || all(!nzchar(trimws(as.character(out_df$Variable))))) {
      sd_vals <- summ$SD[match(prof_num, summ$class_num)]
    } else {
      var_lab_map <- setNames(get_var_label(unique(summ$var_name)), unique(summ$var_name))
      summ$Variable <- unname(var_lab_map[summ$var_name])
      var_fill <- as.character(out_df$Variable)
      for (i in seq_along(var_fill)) if ((!nzchar(trimws(var_fill[i])) || is.na(var_fill[i])) && i > 1) var_fill[i] <- var_fill[i - 1]
      key_out <- paste(var_fill, prof_num, sep = "||")
      key_sum <- paste(summ$Variable, summ$class_num, sep = "||")
      sd_vals <- summ$SD[match(key_out, key_sum)]
      if (all(is.na(sd_vals)) && length(unique(summ$var_name)) == 1L) {
        sd_vals <- summ$SD[match(prof_num, summ$class_num)]
      }
    }
    out_df$SD <- fmt_sd2(sd_vals)
    if ("SE" %in% names(out_df)) out_df$SE <- NULL
  }
  reorder_t6_columns(out_df)
}

# ------------------------------------------------------------
# 9.        T6
# ------------------------------------------------------------
build_T6_bch <- function() {
  df_full  <- safe_df(BCH_RESULTS_FULL)
  df_one   <- safe_df(BCH_RESULTS)
  df_ph    <- safe_df(BCH_POSTHOC)
  om_basic <- safe_df(BCH_OMNIBUS_BASIC)

  parse_bch_overall_from_out <- function(out_path) {
    out <- list(stat = NA_real_, p = NA_real_, posthoc = "")
    if (is.null(out_path) || !nzchar(as.character(out_path)) || !file.exists(out_path)) return(out)

    lines <- tryCatch(readLines(out_path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    if (length(lines) == 0) return(out)

    x <- trimws(gsub("[[:space:]]+", " ", lines))
    x <- x[nzchar(x)]

    idx <- grep("EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE", x, ignore.case = TRUE)
    if (length(idx) == 0) return(out)

    blk <- x[idx[1]:length(x)]
    blk <- blk[!grepl("^TECHNICAL\\s+[0-9]+\\s+OUTPUT", blk, ignore.case = TRUE)]

    pair_txt <- character(0)
    for (ln in blk) {
      hit_overall <- regexec("Overall test\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)", ln, ignore.case = TRUE)
      tok_overall <- regmatches(ln, hit_overall)[[1]]
      if (length(tok_overall) >= 3) {
        out$stat <- suppressWarnings(as.numeric(tok_overall[2]))
        out$p <- suppressWarnings(as.numeric(tok_overall[3]))
      }

      hit_pairs <- gregexpr("Class\\s+([0-9]+)\\s+vs\\.\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)", ln, perl = TRUE, ignore.case = TRUE)
      pair_str <- regmatches(ln, hit_pairs)[[1]]
      if (length(pair_str) > 0) {
        for (one in pair_str) {
          tok <- regmatches(one, regexec("Class\\s+([0-9]+)\\s+vs\\.\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)", one, perl = TRUE, ignore.case = TRUE))[[1]]
          if (length(tok) >= 5) {
            p_i <- suppressWarnings(as.numeric(tok[5]))
            if (!is.na(p_i) && p_i < .05) {
              pair_txt <- c(pair_txt, paste0("C", tok[2], "-C", tok[3]))
            }
          }
        }
      }
    }

    if (length(pair_txt) > 0) {
      out$posthoc <- paste(unique(pair_txt), collapse = ", ")
    }
    out
  }

  if (nrow(df_full) == 0) {
    return(data.frame(
      Profile          = character(),
      M                = character(),
      SD               = character(),
      Statistic        = character(),
      p                = character(),
      `Post-hoc`       = character(),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    ))
  }

  if (nrow(df_one) == 1L &&
      "var_name" %in% names(df_one) &&
      any(grepl("^class[0-9]+$", names(df_one)))) {

    out_file_path <- NA_character_
    if ("out_file" %in% names(df_full)) {
      out_file_candidates <- unique(as.character(df_full$out_file))
      out_file_candidates <- out_file_candidates[!is.na(out_file_candidates) & nzchar(out_file_candidates)]
      if (length(out_file_candidates) > 0) out_file_path <- out_file_candidates[1]
    }

    parsed_out <- parse_bch_overall_from_out(out_file_path)

    class_cols <- grep("^class[0-9]+$", names(df_one), value = TRUE)
    class_cols <- class_cols[order(suppressWarnings(as.integer(gsub("^class", "", class_cols))))]
    se_cols <- paste0("se_", class_cols)
    se_cols[!se_cols %in% names(df_one)] <- paste0("se_class", gsub("^class", "", se_cols[!se_cols %in% names(df_one)]))

    p_val <- suppressWarnings(as.numeric(parsed_out$p))
    stat_val <- suppressWarnings(as.numeric(parsed_out$stat))

    out <- data.frame(
      Variable = rep(if ("label" %in% names(df_one)) as.character(df_one$label[1]) else get_var_label(df_one$var_name[1]), length(class_cols)),
      Profile = paste0("Profile ", suppressWarnings(as.integer(gsub("^class", "", class_cols)))),
      M = vapply(class_cols, function(cc) fmt_m2(df_one[[cc]][1]), character(1)),
      SE = vapply(seq_along(class_cols), function(i) {
        se_col_i <- se_cols[i]
        if (se_col_i %in% names(df_one)) fmt_sd2(df_one[[se_col_i]][1]) else ""
      }, character(1)),
      Statistic = rep("", length(class_cols)),
      p = rep("", length(class_cols)),
      sig = rep("", length(class_cols)),
      `Post-hoc` = rep("", length(class_cols)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (nrow(out) > 0) {
      out$Statistic[1] <- if (!is.na(stat_val)) sprintf("%.2f", stat_val) else ""
      out$p[1] <- fmt_p3_strict(p_val)
      out$sig[1] <- fmt_sig_cell(sig_mark(p_val))
      class_nums <- suppressWarnings(as.integer(gsub("^class", "", class_cols)))
      class_means <- vapply(class_cols, function(cc) safe_num(df_one[[cc]][1]), numeric(1))
      names(class_means) <- as.character(class_nums)
      ordered_posthoc <- bch_ordered_posthoc_notation(class_means, pair_text = parsed_out$posthoc)
      out$`Post-hoc`[1] <- if (nzchar(ordered_posthoc)) ordered_posthoc else as.character(parsed_out$posthoc %||% "")
      if (nrow(out) > 1) {
        out$Variable[2:nrow(out)] <- ""
      }
    }

    if (!isTRUE(HAS_WEIGHT)) {
      classified_src <- safe_df(CLASSIFIED_ANALYSIS)
      if (nrow(classified_src) == 0) classified_src <- safe_df(ANALYSIS_DATA_CLASSIFIED)
      outcome_var <- as.character(df_one$var_name[1] %||% "")
      if (nzchar(outcome_var) && all(c("class_num", outcome_var) %in% names(classified_src))) {
        prof_num <- suppressWarnings(as.integer(gsub("^Profile\\s+", "", as.character(out$Profile))))
        sd_map <- tapply(
          safe_num(classified_src[[outcome_var]]),
          safe_int(classified_src$class_num),
          stats::sd,
          na.rm = TRUE
        )
        out$SD <- fmt_sd2(as.numeric(sd_map[as.character(prof_num)]))
        out$SE <- NULL
      }
    }

    return(replace_t6_spread_with_sd(out, outcome_vars = unique(df_one$var_name)))
  }

  var_col_full0 <- first_existing_col(df_full, c("var_name", "Variable", "outcome", "distal", "y"))

  # ???robust outcome detection
  if (!is.na(var_col_full0) && var_col_full0 %in% names(df_full)) {
    vals   <- unique(as.character(df_full[[var_col_full0]]))
    vals   <- vals[!is.na(vals) & nzchar(trimws(vals))]
    n_vars <- length(vals)
  } else {
    n_vars <- 1L   # fallback: single outcome
  }

  if (n_vars > 1) {
    tmp <- df_full

    if (!"Variable" %in% names(tmp)) {
      cand <- intersect(c("var_label", "variable", "Label"), names(tmp))
      if (length(cand) > 0) {
        names(tmp)[match(cand[1], names(tmp))] <- "Variable"
      } else if (!is.na(var_col_full0)) {
        tmp$Variable <- as.character(tmp[[var_col_full0]])
      }
    }

    if (!"p" %in% names(tmp)) tmp$p <- NA_real_
    if (!"sig" %in% names(tmp)) tmp$sig <- ""

    # --------------------------------------------------
    # omnibus p / sig robust merge
    # --------------------------------------------------
    if (is.data.frame(om_basic) && nrow(om_basic) > 0) {

      om_use <- safe_df(om_basic)

      # 1) var_name ???????????ㅻ깹??????
      if (!"var_name" %in% names(om_use)) {
        cand_om <- intersect(c("outcome", "variable", "Variable", "y"), names(om_use))
        if (length(cand_om) > 0) {
          om_use$var_name <- as.character(om_use[[cand_om[1]]])
        }
      }

      if (!"var_name" %in% names(tmp) && !is.na(var_col_full0)) {
        tmp$var_name <- as.character(tmp[[var_col_full0]])
      }

      if ("var_name" %in% names(om_use)) om_use$var_name <- trimws(as.character(om_use$var_name))
      if ("var_name" %in% names(tmp))    tmp$var_name    <- trimws(as.character(tmp$var_name))

      # 2) p ???????꿔꺂???⑸븶?????????????????????????ㅻ깹??????
      if (!"p" %in% names(om_use)) {
        cand_p <- intersect(c("omnibus_p", "p_value", "pval", "p_fmt"), names(om_use))
        if (length(cand_p) > 0) {
          if (cand_p[1] == "p_fmt") {
            p_chr <- as.character(om_use[[cand_p[1]]])
            p_chr[p_chr %in% c("", NA)] <- NA
            p_chr <- gsub("^\\.", "0.", p_chr)
            p_chr[p_chr == "<.001"] <- "0.0009"
            om_use$p <- suppressWarnings(as.numeric(p_chr))
          } else {
            om_use$p <- suppressWarnings(as.numeric(om_use[[cand_p[1]]]))
          }
        }
      }

      # 3) sig ??????????????????됰Ŧ?????????????붺몭??잞쭜?????p???????????ш끽維뽳쭩?뱀땡???얩맪???????????
      if (!"sig" %in% names(om_use) && "p" %in% names(om_use)) {
        om_use$sig <- sig_mark(om_use$p)
      }

      # 4) var_name ??????? merge
      if (all(c("var_name", "p") %in% names(om_use)) && "var_name" %in% names(tmp)) {

        om_keep <- intersect(c("var_name", "p", "sig"), names(om_use))
        om_use  <- unique(om_use[, om_keep, drop = FALSE])

        tmp <- merge(
          tmp,
          om_use,
          by       = "var_name",
          all.x    = TRUE,
          sort     = FALSE,
          suffixes = c("", "_om")
        )

        if ("p_om" %in% names(tmp)) {
          fill_p        <- is.na(tmp$p) & !is.na(tmp$p_om)
          tmp$p[fill_p] <- tmp$p_om[fill_p]
          tmp$p_om      <- NULL
        }

        if ("sig_om" %in% names(tmp)) {
          tmp$sig                 <- as.character(tmp$sig)
          tmp$sig[is.na(tmp$sig)] <- ""
          fill_sig                <- !nzchar(trimws(tmp$sig)) & !is.na(tmp$sig_om)
          tmp$sig[fill_sig]       <- as.character(tmp$sig_om[fill_sig])
          tmp$sig_om              <- NULL
        }
      }

      # 5) ????????????????p??????????? ???????轅붽틓????몃마????????븐뼐?????????????????????????????獄쏅챶留덌┼??????????????筌롈살젔?????????????ㅻ깹????????????嫄?????????????????????밸븶筌믩끃??獄???????멥렑???????????
      if ("p" %in% names(om_use)) {
        p_nonmiss <- unique(om_use$p[!is.na(om_use$p)])
        if (length(p_nonmiss) == 1) {
          if (!"p" %in% names(tmp)) tmp$p <- NA_real_
          tmp$p[is.na(tmp$p)] <- p_nonmiss[1]
        }
      }

      if ("sig" %in% names(om_use)) {
        sig_nonmiss <- unique(as.character(om_use$sig[!is.na(om_use$sig) & nzchar(trimws(as.character(om_use$sig)))]))
        if (length(sig_nonmiss) == 1) {
          if (!"sig" %in% names(tmp)) tmp$sig <- ""
          tmp$sig <- as.character(tmp$sig)
          tmp$sig[is.na(tmp$sig)] <- ""
          tmp$sig[!nzchar(trimws(tmp$sig))] <- sig_nonmiss[1]
        }
      }

      # 6) ??????濾????????????????????밸븶筌믩끃??獄???????멥렑??????癰귙룗????????????????????? p?????????????sig ???????????????????됰뼸????????????ш끽維뽳쭩?뱀땡???얩맪???????????
      if ("p" %in% names(tmp)) {
        if (!"sig" %in% names(tmp)) tmp$sig <- ""
        tmp$sig <- as.character(tmp$sig)
        tmp$sig[is.na(tmp$sig)] <- ""
        need_sig <- !nzchar(trimws(tmp$sig)) & !is.na(tmp$p)
        tmp$sig[need_sig] <- sig_mark(tmp$p[need_sig])
      }
    }

    if (tolower(as.character(mixture_mode %||% "lpa")) == "lca") {

      if (!"class_num" %in% names(tmp)) return(data.frame())
      if (!"estimate" %in% names(tmp)) return(data.frame())
      if (!"var_name" %in% names(tmp) && !is.na(var_col_full0)) {
        tmp$var_name <- as.character(tmp[[var_col_full0]])
      }

      tmp$class_num <- safe_int(tmp$class_num)
      class_tmp <- tmp[!is.na(tmp$class_num), , drop = FALSE]
      if ("result_type" %in% names(class_tmp)) {
        hit_class <- as.character(class_tmp$result_type) == "class_estimate"
        if (any(hit_class)) class_tmp <- class_tmp[hit_class, , drop = FALSE]
      }
      if (nrow(class_tmp) == 0) return(data.frame())

      class_tmp$Profile <- paste0("Profile ", class_tmp$class_num)

      classified_src <- safe_df(CLASSIFIED_ANALYSIS)
      if (nrow(classified_src) == 0) classified_src <- safe_df(ANALYSIS_DATA_CLASSIFIED)
      if ("class_num" %in% names(classified_src)) classified_src$class_num <- safe_int(classified_src$class_num)
      spec_src_for_data <- safe_df(BCH_OUTCOME_SPEC)
      spec_var_col_for_data <- first_existing_col(spec_src_for_data, c("var_name", "outcome", "variable", "Variable"))
      outcome_vars_for_data <- if (!is.na(spec_var_col_for_data)) {
        unique(as.character(spec_src_for_data[[spec_var_col_for_data]]))
      } else {
        character(0)
      }
      missing_classified_outcomes <- setdiff(
        outcome_vars_for_data[!is.na(outcome_vars_for_data) & nzchar(trimws(outcome_vars_for_data))],
        names(classified_src)
      )
      if (length(missing_classified_outcomes) > 0 &&
          exists("PATH_DATA") &&
          file.exists(PATH_DATA) &&
          exists("read_data_file")) {
        data_file_current <- tryCatch(
          as.data.frame(read_data_file(PATH_DATA, required = FALSE), stringsAsFactors = FALSE),
          error = function(e) data.frame()
        )
        if (is.data.frame(data_file_current) && nrow(data_file_current) == nrow(classified_src)) {
          add_outcomes <- intersect(missing_classified_outcomes, names(data_file_current))
          for (vv in add_outcomes) classified_src[[vv]] <- data_file_current[[vv]]
        }
      }

      is_multicat_outcome <- function(vv) {
        vv <- as.character(vv)[1]
        if (!nzchar(vv) || !vv %in% names(classified_src)) return(FALSE)
        ux <- sort(unique(as.character(stats::na.omit(classified_src[[vv]]))))
        length(ux) > 2L
      }

      multicat_vars <- unique(as.character(class_tmp$var_name[
        vapply(class_tmp$var_name, is_multicat_outcome, logical(1))
      ]))
      spec_src <- safe_df(BCH_OUTCOME_SPEC)
      if (nrow(spec_src) > 0) {
        spec_var_col <- first_existing_col(spec_src, c("var_name", "outcome", "variable", "Variable"))
        if (!is.na(spec_var_col)) {
          spec_vars <- unique(as.character(spec_src[[spec_var_col]]))
          spec_vars <- spec_vars[!is.na(spec_vars) & nzchar(trimws(spec_vars))]
          spec_multicat <- spec_vars[vapply(spec_vars, is_multicat_outcome, logical(1))]
          multicat_vars <- unique(c(multicat_vars, spec_multicat))
        }
      }
      binary_tmp <- class_tmp[!as.character(class_tmp$var_name) %in% multicat_vars, , drop = FALSE]

      out_parts <- list()
      if (nrow(binary_tmp) > 0) {
        out_bin <- unique(binary_tmp[, c("Variable"), drop = FALSE])
        out_bin$Category <- ""
        prof_names_bin <- unique(binary_tmp$Profile)
        prof_names_bin <- prof_names_bin[order(suppressWarnings(as.integer(gsub("^Profile ", "", prof_names_bin))))]

        for (pp in prof_names_bin) {
          sub <- binary_tmp[binary_tmp$Profile == pp, , drop = FALSE]
          out_bin[[twoline_key("%", pp)]] <- ifelse(
            is.na(sub$estimate[match(out_bin$Variable, sub$Variable)]),
            "",
            sprintf("%.1f", 100 * sub$estimate[match(out_bin$Variable, sub$Variable)])
          )
        }
        out_parts[[length(out_parts) + 1L]] <- out_bin
      }

      if (length(multicat_vars) > 0 && is.data.frame(classified_src) && nrow(classified_src) > 0) {
        class_levels <- sort(unique(safe_int(class_tmp$class_num)))
        if (length(class_levels) == 0 || all(is.na(class_levels))) {
          class_levels <- sort(unique(safe_int(classified_src$class_num)))
        }
        class_levels <- class_levels[!is.na(class_levels)]
        multi_rows <- list()
        ii <- 1L
        for (vv in multicat_vars) {
          if (!vv %in% names(classified_src)) next
          cats <- sort(unique(as.character(stats::na.omit(classified_src[[vv]]))))
          for (cat_i in cats) {
            row_i <- data.frame(
              Variable = get_var_label(vv),
              Category = as.character(cat_i),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
            for (cl in class_levels) {
              sub <- classified_src[classified_src$class_num == cl, , drop = FALSE]
              x <- as.character(sub[[vv]])
              pct_i <- if (length(x) > 0) mean(x == cat_i, na.rm = TRUE) * 100 else NA_real_
              row_i[[twoline_key("%", paste0("Profile ", cl))]] <- ifelse(is.na(pct_i), "", sprintf("%.1f", pct_i))
            }
            multi_rows[[ii]] <- row_i
            ii <- ii + 1L
          }
        }
        if (length(multi_rows) > 0) out_parts[[length(out_parts) + 1L]] <- do.call(rbind, multi_rows)
      }

      out <- if (length(out_parts) > 0) {
        all_cols_out <- unique(unlist(lapply(out_parts, names), use.names = FALSE))
        out_parts <- lapply(out_parts, function(z) {
          for (cc in setdiff(all_cols_out, names(z))) z[[cc]] <- ""
          z[, all_cols_out, drop = FALSE]
        })
        do.call(rbind, out_parts)
      } else {
        data.frame()
      }
      if (nrow(out) == 0) return(data.frame())

      p_src <- tmp
      p_src$Variable <- trimws(as.character(p_src$Variable))
      if (!"p" %in% names(p_src)) p_src$p <- NA_real_
      if (!"sig" %in% names(p_src)) p_src$sig <- ""
      p_src$p <- suppressWarnings(as.numeric(p_src$p))
      p_src$sig <- as.character(p_src$sig)
      p_src$sig[is.na(p_src$sig)] <- ""
      if ("result_type" %in% names(p_src)) {
        p_overall <- p_src[as.character(p_src$result_type) == "overall", , drop = FALSE]
        if (nrow(p_overall) > 0) p_src <- rbind(p_overall, p_src)
      }
      p_map <- do.call(rbind, lapply(split(p_src, p_src$Variable), function(z) {
        p_hit <- z$p[!is.na(z$p)]
        sig_hit <- z$sig[nzchar(trimws(z$sig))]
        data.frame(
          Variable = z$Variable[1],
          p = if (length(p_hit) > 0) p_hit[1] else NA_real_,
          sig = if (length(sig_hit) > 0) sig_hit[1] else "",
          stringsAsFactors = FALSE
        )
      }))
      rownames(p_map) <- NULL

      # p??????????? ???????????sig??????????? ?????????????????????????뀀맩鍮???癲???????Β?щ엠???????????????????됰뼸????????????ш끽維뽳쭩?뱀땡???얩맪???????????
      need_sig <- !nzchar(trimws(p_map$sig)) & !is.na(p_map$p)
      p_map$sig[need_sig] <- sig_mark(p_map$p[need_sig])

      # omnibus p??????????? ???????????ㅻ깹?????????????轅붽틓?????????merge???????????????怨뺤떪??????????????????????산뭐????fallback
      if (all(is.na(p_map$p))) {

        p_fallback <- NA_real_

        if (is.data.frame(om_basic) && nrow(om_basic) > 0) {
          cand_p <- intersect(c("p", "omnibus_p", "p_value", "pval"), names(om_basic))
          if (length(cand_p) > 0) {
            p_fallback <- suppressWarnings(as.numeric(om_basic[[cand_p[1]]][1]))
          }
        }

        if (is.na(p_fallback) && is.data.frame(df_one) && nrow(df_one) > 0) {
          cand_p2 <- intersect(c("p", "omnibus_p", "p_value", "pval"), names(df_one))
          if (length(cand_p2) > 0) {
            p_fallback <- suppressWarnings(as.numeric(df_one[[cand_p2[1]]][1]))
          }
        }

        if (!is.na(p_fallback)) {
          p_map$p <- rep(p_fallback, nrow(p_map))
          need_sig2 <- !nzchar(trimws(p_map$sig)) & !is.na(p_map$p)
          p_map$sig[need_sig2] <- sig_mark(p_map$p[need_sig2])
        }
      }

      out[[twoline_key("p", "Overall")]] <- fmt_p3_strict(
        p_map$p[match(trimws(as.character(out$Variable)), p_map$Variable)]
      )

      out[[twoline_key("sig", "Overall")]] <- fmt_sig_cell(
        p_map$sig[match(trimws(as.character(out$Variable)), p_map$Variable)]
      )

      # post-hoc
      posthoc_map <- data.frame(
        Variable = unique(tmp$Variable),
        posthoc = "",
        stringsAsFactors = FALSE
      )

      if (is.data.frame(df_ph) && nrow(df_ph) > 0) {
        ph_use <- df_ph

        outcome_col_ph <- first_existing_col(ph_use, c("outcome", "var_name"))
        if (!is.na(outcome_col_ph) && all(c("class1", "class2") %in% names(ph_use))) {
          ph_use <- ph_use[!is.na(safe_num(ph_use$p)) & safe_num(ph_use$p) < .05, , drop = FALSE]

          if (nrow(ph_use) > 0) {
            ph_use$pair_txt <- paste0("C", ph_use$class1, "-C", ph_use$class2)

            ph_sum <- aggregate(
              pair_txt ~ .,
              data = ph_use[, c(outcome_col_ph, "pair_txt"), drop = FALSE],
              FUN = function(z) paste(unique(z), collapse = ", ")
            )

            names(ph_sum)[1] <- "var_name"

            var_map <- unique(tmp[, c("var_name", "Variable"), drop = FALSE])
            ph_sum <- merge(ph_sum, var_map, by = "var_name", all.x = TRUE, sort = FALSE)

            posthoc_map <- merge(
              posthoc_map,
              ph_sum[, c("Variable", "pair_txt"), drop = FALSE],
              by = "Variable",
              all.x = TRUE,
              sort = FALSE
            )

            posthoc_map$posthoc <- ifelse(
              !is.na(posthoc_map$pair_txt) & nzchar(posthoc_map$pair_txt),
              posthoc_map$pair_txt,
              posthoc_map$posthoc
            )
            posthoc_map$pair_txt <- NULL
          }
        }
      }

      if ("out_file" %in% names(tmp)) {
        posthoc_from_out <- lapply(unique(tmp$var_name), function(vv) {
          out_files <- unique(as.character(tmp$out_file[as.character(tmp$var_name) == as.character(vv)]))
          out_files <- out_files[!is.na(out_files) & nzchar(out_files)]
          parsed_v <- if (length(out_files) > 0) parse_bch_overall_from_out(out_files[1]) else list(posthoc = "")
          data.frame(
            var_name = vv,
            posthoc_from_out = as.character(parsed_v$posthoc %||% ""),
            stringsAsFactors = FALSE
          )
        })
        posthoc_from_out <- if (length(posthoc_from_out) > 0) do.call(rbind, posthoc_from_out) else data.frame()
        if (is.data.frame(posthoc_from_out) && nrow(posthoc_from_out) > 0) {
          var_map_out <- unique(tmp[, c("var_name", "Variable"), drop = FALSE])
          posthoc_from_out <- merge(posthoc_from_out, var_map_out, by = "var_name", all.x = TRUE, sort = FALSE)
          posthoc_map <- merge(
            posthoc_map,
            posthoc_from_out[, c("Variable", "posthoc_from_out"), drop = FALSE],
            by = "Variable",
            all.x = TRUE,
            sort = FALSE
          )
          fill_out <- !nzchar(trimws(posthoc_map$posthoc)) &
            !is.na(posthoc_map$posthoc_from_out) &
            nzchar(trimws(posthoc_map$posthoc_from_out))
          posthoc_map$posthoc[fill_out] <- posthoc_map$posthoc_from_out[fill_out]
          posthoc_map$posthoc_from_out <- NULL
        }
      }

      if (nrow(posthoc_map) > 0 && "var_name" %in% names(tmp)) {
        var_map_for_posthoc <- unique(tmp[, c("var_name", "Variable"), drop = FALSE])
        for (ii in seq_len(nrow(posthoc_map))) {
          current <- as.character(posthoc_map$posthoc[ii] %||% "")
          if (!nzchar(trimws(current))) next
          vv <- var_map_for_posthoc$var_name[match(posthoc_map$Variable[ii], var_map_for_posthoc$Variable)]
          means_i <- extract_bch_means_for_variable(tmp, vv)
          ordered_i <- bch_ordered_posthoc_notation(means_i, pair_text = current)
          if (nzchar(ordered_i)) posthoc_map$posthoc[ii] <- ordered_i
        }
      }

      out[[twoline_key("post-hoc", "Overall")]] <- posthoc_map$posthoc[match(out$Variable, posthoc_map$Variable)]
      out[[twoline_key("post-hoc", "Overall")]][is.na(out[[twoline_key("post-hoc", "Overall")]])] <- ""
      is_category_row <- "Category" %in% names(out) & nzchar(trimws(as.character(out$Category)))
      if (any(is_category_row)) {
        cat_idx <- which(is_category_row)
        keep_cat <- rep(FALSE, nrow(out))
        keep_cat[cat_idx[!duplicated(out$Variable[cat_idx])]] <- TRUE
        blank_cat <- is_category_row & !keep_cat
        out[[twoline_key("p", "Overall")]][blank_cat] <- ""
        out[[twoline_key("sig", "Overall")]][blank_cat] <- ""
        out[[twoline_key("post-hoc", "Overall")]][is_category_row] <- ""
      }

      return(out)

    } else {
      return(replace_t6_spread_with_sd(format_t6_bch_twoline(tmp), outcome_vars = unique(tmp$var_name)))
    }
  }

  var_col      <- first_existing_col(df_full, c("var_name", "Variable", "outcome", "distal", "y"))
  classnum_col <- first_existing_col(df_full, c("class_num"))
  class_col    <- first_existing_col(df_full, c("class", "Class"))
  mean_col     <- first_existing_col(df_full, c("Mean", "mean", "estimate"))
  sd_col       <- first_existing_col(df_full, c("SD", "sd", "SE", "se"))

  if ("result_type" %in% names(df_full)) {
    class_rows <- df_full[df_full$result_type == "class_estimate", , drop = FALSE]
    overall_rows <- df_full[df_full$result_type == "overall", , drop = FALSE]

    if (nrow(class_rows) > 0) {
      tmp <- class_rows

      if (!"Variable" %in% names(tmp)) {
        cand <- intersect(c("var_label", "variable", "Label"), names(tmp))
        if (length(cand) > 0) {
          names(tmp)[match(cand[1], names(tmp))] <- "Variable"
        } else if ("var_label" %in% names(tmp)) {
          tmp$Variable <- as.character(tmp$var_label)
        } else if ("var_name" %in% names(tmp)) {
          tmp$Variable <- as.character(tmp$var_name)
        }
      }

      if (!"Profile" %in% names(tmp)) {
        if ("class_num" %in% names(tmp)) {
          tmp$Profile <- paste0("Profile ", suppressWarnings(as.integer(tmp$class_num)))
        } else if ("class" %in% names(tmp)) {
          tmp$Profile <- clean_profile_text(tmp$class, "Profile")
        }
      }

      if (!"Mean" %in% names(tmp) && "estimate" %in% names(tmp)) tmp$Mean <- tmp$estimate
      if (!"SE" %in% names(tmp) && "se" %in% names(tmp)) tmp$SE <- tmp$se

      p_val <- NA_real_
      stat_val <- NA_real_
      if (nrow(overall_rows) > 0) {
        stat_hit <- suppressWarnings(as.numeric(overall_rows$stat))
        p_hit <- suppressWarnings(as.numeric(overall_rows$p))
        stat_hit <- stat_hit[!is.na(stat_hit)]
        p_hit <- p_hit[!is.na(p_hit)]
        if (length(stat_hit) > 0) stat_val <- stat_hit[1]
        if (length(p_hit) > 0) p_val <- p_hit[1]
      }
      if (is.na(p_val) && is.data.frame(om_basic) && nrow(om_basic) > 0 && "p" %in% names(om_basic)) {
        p_hit <- suppressWarnings(as.numeric(om_basic$p))
        p_hit <- p_hit[!is.na(p_hit)]
        if (length(p_hit) > 0) p_val <- p_hit[1]
      }

      posthoc_txt <- ""
      if (is.data.frame(df_ph) && nrow(df_ph) > 0) {
        ph_use <- df_ph
        outcome_target <- unique(as.character(tmp$var_name))
        outcome_target <- outcome_target[!is.na(outcome_target) & nzchar(trimws(outcome_target))]
        if ("outcome" %in% names(ph_use) && length(outcome_target) > 0) {
          ph_use <- ph_use[as.character(ph_use$outcome) %in% outcome_target, , drop = FALSE]
        }
        if (all(c("class1", "class2", "p") %in% names(ph_use))) {
          ph_use$p_num <- suppressWarnings(as.numeric(ph_use$p))
          ph_use <- ph_use[!is.na(ph_use$p_num) & ph_use$p_num < .05, , drop = FALSE]
          if (nrow(ph_use) > 0) {
            posthoc_txt <- paste0("C", ph_use$class1, "-C", ph_use$class2)
            posthoc_txt <- paste(unique(posthoc_txt), collapse = ", ")
            ordered_posthoc <- bch_ordered_posthoc_notation(extract_bch_means_for_variable(tmp, outcome_target[1]), pair_text = posthoc_txt)
            if (nzchar(ordered_posthoc)) posthoc_txt <- ordered_posthoc
          }
        }
      }

      tmp$p <- p_val
      tmp$sig <- sig_mark(tmp$p)
      tmp$posthoc <- posthoc_txt

      return(replace_t6_spread_with_sd(format_t6_bch_twoline(tmp), outcome_vars = unique(tmp$var_name)))
    }
  }

  if (is.na(mean_col)) {
    return(data.frame(
      Profile          = character(),
      M                = character(),
      SD               = character(),
      Statistic        = character(),
      p                = character(),
      `Post-hoc`       = character(),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    ))
  }

  if (!is.na(classnum_col)) {
    prof_num <- suppressWarnings(as.integer(df_full[[classnum_col]]))
    prof     <- paste0("Profile ", prof_num)
  } else if (!is.na(class_col)) {
    prof     <- clean_profile_text(df_full[[class_col]], "Profile")
    prof_num <- suppressWarnings(as.integer(gsub("^Profile\\s+([0-9]+)$", "\\1", prof)))
  } else {
    prof_num <- seq_len(nrow(df_full))
    prof     <- paste0("Profile ", prof_num)
  }

  prof_num[is.na(prof_num)] <- 999999 + seq_len(sum(is.na(prof_num)))

  # overall p
  p_val   <- NA_real_

  if ("p" %in% names(df_full)) {
    p_val <- df_full$p[1]
  } else if ("omnibus_p" %in% names(df_full)) {
    p_val <- df_full$omnibus_p[1]
  } else if (is.data.frame(om_basic) && "p" %in% names(om_basic)) {
    p_val <- om_basic$p[1]
  }

  p_val <- fmt_p3_strict(p_val)


  if (is.data.frame(om_basic) && nrow(om_basic) > 0 && "p" %in% names(om_basic)) {
    p_val <- fmt_p3_strict(om_basic$p[1])
  }

  if (!nzchar(p_val)) {
    p_col_one <- first_existing_col(df_one, c("p", "p_value"))
    if (!is.na(p_col_one) && nrow(df_one) > 0) {
      p_val <- fmt_p3_strict(df_one[[p_col_one]][1])
    }
  }

  if (!nzchar(p_val) && "omnibus_p" %in% names(df_full) && nrow(df_full) > 0) {
    p_val <- fmt_p3_strict(df_full$omnibus_p[1])
  }

  # statistic
  # ???FIX: stat ???????????????????????椰????????????釉먮폁?????????癲꾧퀗???볤괌?됰굝??????꿔꺂???⑸븶????????
  stat_val   <- NA_real_

  if ("stat" %in% names(df_full)) {
    stat_val <- df_full$stat[1]
  } else if ("chi_sq" %in% names(df_full)) {
    stat_val <- df_full$chi_sq[1]
  }

  stat_val   <- ifelse(is.na(stat_val), "", sprintf("%.2f", stat_val))

  if (is.data.frame(om_basic) && nrow(om_basic) > 0 && "chi_sq" %in% names(om_basic)) {
    x_stat   <- suppressWarnings(as.numeric(om_basic$chi_sq[1]))
    stat_val <- ifelse(is.na(x_stat), "", sprintf("%.2f", x_stat))
  }

  if (!nzchar(stat_val) && "stat" %in% names(df_full) && nrow(df_full) > 0) {
    x_stat   <- suppressWarnings(as.numeric(df_full$stat[1]))
    stat_val <- ifelse(is.na(x_stat), "", sprintf("%.2f", x_stat))
  }

  stat_vec   <- rep(stat_val, nrow(df_full))
  p_vec      <- rep(p_val,   nrow(df_full))

  # post-hoc
  posthoc_txt <- ""
  if (is.data.frame(df_ph) && nrow(df_ph) > 0) {
    main_var <- if (!is.na(var_col)) as.character(df_full[[var_col]][1]) else ""

    hit <- df_ph
    if ("outcome" %in% names(hit) && nzchar(main_var)) {
      hit <- hit[as.character(hit$outcome) == main_var, , drop = FALSE]
    } else if ("var_name" %in% names(hit) && nzchar(main_var)) {
      hit <- hit[as.character(hit$var_name) == main_var, , drop = FALSE]
    }

    if ("p" %in% names(hit)) {
      hit <- hit[!is.na(safe_num(hit$p)) & safe_num(hit$p) < .05, , drop = FALSE]
    }

    if (nrow(hit) > 0 && all(c("class1", "class2") %in% names(hit))) {
      parts <- paste0("C", hit$class1, "-C", hit$class2)
      parts <- unique(parts)
      posthoc_txt <- paste(parts, collapse = ", ")
      ordered_posthoc <- bch_ordered_posthoc_notation(extract_bch_means_for_variable(df_full, main_var), pair_text = posthoc_txt)
      if (nzchar(ordered_posthoc)) posthoc_txt <- ordered_posthoc
    }
  }

  out <- data.frame(
    Profile          = prof,
    M                = fmt_m2(df_full[[mean_col]]),
    SE               = if (!is.na(sd_col)) fmt_sd2(df_full[[sd_col]]) else "",
    Statistic        = stat_vec,
    p                = p_vec,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  out$`Post-hoc` <- rep("", nrow(out))
  if (nrow(out) > 0 && nzchar(trimws(posthoc_txt))) {
    out$`Post-hoc`[1] <- posthoc_txt
  }

  out$..ord.. <- prof_num
  out <- out[order(out$..ord..), , drop = FALSE]
  out$..ord.. <- NULL
  rownames(out) <- NULL

  replace_t6_spread_with_sd(out, outcome_vars = if (!is.na(var_col)) unique(as.character(df_full[[var_col]])) else character(0))
}


# ------------------------------------------------------------
#                     T6b
# ------------------------------------------------------------
build_T6b_bch_categorical_multinom <- function() {
  spec <- safe_df(BCH_OUTCOME_SPEC)
  if (nrow(spec) == 0) return(data.frame())
  if (!requireNamespace("nnet", quietly = TRUE)) {
    log_warn("nnet package is not available; skipping T6b categorical BCH multinomial table.")
    return(data.frame())
  }

  spec_var_col <- first_existing_col(spec, c("var_name", "outcome", "variable", "Variable"))
  if (is.na(spec_var_col)) return(data.frame())

  spec_type_col <- first_existing_col(spec, c("outcome_type", "type", "measurement", "subtype"))
  outcome_vars <- unique(as.character(spec[[spec_var_col]]))
  outcome_vars <- outcome_vars[!is.na(outcome_vars) & nzchar(trimws(outcome_vars))]

  if (!is.na(spec_type_col)) {
    type_chr <- tolower(trimws(as.character(spec[[spec_type_col]])))
    outcome_vars <- unique(as.character(spec[[spec_var_col]][type_chr %in% c("categorical", "category", "factor", "binary", "multinomial")]))
    outcome_vars <- outcome_vars[!is.na(outcome_vars) & nzchar(trimws(outcome_vars))]
  }
  if (length(outcome_vars) == 0) return(data.frame())

  dat <- safe_df(CLASSIFIED_ANALYSIS)
  if (nrow(dat) == 0) dat <- safe_df(ANALYSIS_DATA_CLASSIFIED)
  if (nrow(dat) == 0 || !"class_num" %in% names(dat)) return(data.frame())

  missing_outcomes <- setdiff(outcome_vars, names(dat))
  if (length(missing_outcomes) > 0 && is.data.frame(ANALYSIS_DATA_SUB) && nrow(ANALYSIS_DATA_SUB) == nrow(dat)) {
    add_vars <- intersect(missing_outcomes, names(ANALYSIS_DATA_SUB))
    for (vv in add_vars) dat[[vv]] <- ANALYSIS_DATA_SUB[[vv]]
  }
  missing_outcomes <- setdiff(outcome_vars, names(dat))
  if (length(missing_outcomes) > 0 && is.data.frame(RAW_DATA) && nrow(RAW_DATA) == nrow(dat)) {
    add_vars <- intersect(missing_outcomes, names(RAW_DATA))
    for (vv in add_vars) dat[[vv]] <- RAW_DATA[[vv]]
  }

  outcome_vars <- intersect(outcome_vars, names(dat))
  if (length(outcome_vars) == 0) return(data.frame())

  ref_class_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(REFERENCE_CLASS)[1])))
  if (is.na(ref_class_num)) ref_class_num <- 1L

  class_nums <- sort(unique(safe_int(dat$class_num)))
  class_nums <- class_nums[!is.na(class_nums)]
  if (length(class_nums) < 2L) return(data.frame())
  if (!ref_class_num %in% class_nums) ref_class_num <- class_nums[1]
  class_levels <- c(ref_class_num, setdiff(class_nums, ref_class_num))

  clean_outcome_values <- function(x) {
    x_chr <- trimws(as.character(x))
    x_chr[x_chr %in% c("", "NA", "NaN", "-9999", "-9999.0")] <- NA_character_
    x_chr
  }

  outcome_is_categorical <- function(vv) {
    x <- clean_outcome_values(dat[[vv]])
    ux <- unique(stats::na.omit(x))
    if (length(ux) < 2L) return(FALSE)
    if (!is.na(spec_type_col)) return(TRUE)

    if (is.factor(dat[[vv]]) || is.character(dat[[vv]]) || is.logical(dat[[vv]])) return(TRUE)
    x_num <- suppressWarnings(as.numeric(ux))
    all(!is.na(x_num)) && length(ux) <= 10L && all(abs(x_num - round(x_num)) < 1e-8)
  }

  outcome_vars <- outcome_vars[vapply(outcome_vars, outcome_is_categorical, logical(1))]
  if (length(outcome_vars) == 0) return(data.frame())

  out_rows <- list()
  idx <- 1L

  for (vv in outcome_vars) {
    d <- data.frame(
      y = clean_outcome_values(dat[[vv]]),
      class_num = safe_int(dat$class_num),
      stringsAsFactors = FALSE
    )
    d <- d[!is.na(d$y) & !is.na(d$class_num) & d$class_num %in% class_levels, , drop = FALSE]
    if (nrow(d) == 0 || length(unique(d$y)) < 2L || length(unique(d$class_num)) < 2L) next

    y_num <- suppressWarnings(as.numeric(unique(d$y)))
    y_levels <- unique(d$y)
    if (all(!is.na(y_num))) {
      y_levels <- y_levels[order(suppressWarnings(as.numeric(y_levels)))]
    } else {
      y_levels <- sort(y_levels)
    }
    d$y_factor <- stats::relevel(factor(d$y, levels = y_levels), ref = y_levels[1])
    d$class_factor <- factor(paste0("C", d$class_num), levels = paste0("C", class_levels))

    fit <- tryCatch(
      nnet::multinom(y_factor ~ class_factor, data = d, trace = FALSE),
      error = function(e) NULL
    )
    if (is.null(fit)) next

    fit_sum <- tryCatch(summary(fit), error = function(e) NULL)
    if (is.null(fit_sum)) next

    coef_mat <- fit_sum$coefficients
    se_mat <- fit_sum$standard.errors
    if (is.null(coef_mat) || is.null(se_mat)) next
    if (is.null(dim(coef_mat))) {
      coef_mat <- matrix(coef_mat, nrow = 1L, dimnames = list(levels(d$y_factor)[2], names(coef_mat)))
      se_mat <- matrix(se_mat, nrow = 1L, dimnames = dimnames(coef_mat))
    }

    coef_cols <- colnames(coef_mat)
    class_cols <- coef_cols[grepl("^class_factorC[0-9]+$", coef_cols)]
    if (length(class_cols) == 0) next

    for (outcome_level in rownames(coef_mat)) {
      for (cc in class_cols) {
        cls <- suppressWarnings(as.integer(gsub("^class_factorC", "", cc)))
        est <- suppressWarnings(as.numeric(coef_mat[outcome_level, cc]))
        se <- suppressWarnings(as.numeric(se_mat[outcome_level, cc]))
        if (is.na(est) || is.na(se) || se <= 0) next
        z <- est / se
        p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)

        out_rows[[idx]] <- data.frame(
          Variable = get_var_label(vv),
          var_name = vv,
          level = as.character(outcome_level),
          Category = paste0(get_value_label(vv, outcome_level), " vs ", get_value_label(vv, y_levels[1])),
          Outcome = get_value_label(vv, outcome_level),
          Reference_outcome = get_value_label(vv, y_levels[1]),
          Comparison = paste0(latent_group_label(cls), " vs ", latent_group_label(ref_class_num)),
          RRR = fmt_rrr3(exp(est)),
          LLCI = fmt_rrr3(exp(est - 1.96 * se)),
          ULCI = fmt_rrr3(exp(est + 1.96 * se)),
          p = fmt_p3_strict(p),
          sig = fmt_sig_cell(sig_mark(p)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  if (length(out_rows) == 0) return(data.frame())
  long <- do.call(rbind, out_rows)
  long <- safe_df(long)

  base_rows <- unique(long[, c("var_name", "Variable", "level", "Category"), drop = FALSE])
  ord_var <- get_display_order(base_rows$var_name)
  ord_cat <- get_category_order(base_rows$var_name, level = base_rows$level, label = base_rows$Category)
  ord_cat[is.na(ord_cat)] <- 999999
  base_rows <- base_rows[order(ord_var, ord_cat, base_rows$Category), , drop = FALSE]
  rownames(base_rows) <- NULL

  out <- base_rows[, c("Variable", "Category"), drop = FALSE]
  comp_names <- unique(as.character(long$Comparison))
  comp_names <- comp_names[nzchar(comp_names)]
  comp_num <- extract_class_num_from_text(comp_names)
  comp_names <- comp_names[order(comp_num)]

  key_out <- paste(base_rows$var_name, base_rows$level, sep = "||")
  for (gg in comp_names) {
    sub <- long[as.character(long$Comparison) == gg, , drop = FALSE]
    key_sub <- paste(sub$var_name, sub$level, sep = "||")
    idx <- match(key_out, key_sub)

    out[[twoline_key("RRR",  gg)]] <- ifelse(is.na(idx), "", as.character(sub$RRR[idx]))
    out[[twoline_key("LLCI", gg)]] <- ifelse(is.na(idx), "", as.character(sub$LLCI[idx]))
    out[[twoline_key("ULCI", gg)]] <- ifelse(is.na(idx), "", as.character(sub$ULCI[idx]))
    out[[twoline_key("p",    gg)]] <- ifelse(is.na(idx), "", as.character(sub$p[idx]))
    out[[twoline_key("sig",  gg)]] <- ifelse(is.na(idx), "", as.character(sub$sig[idx]))
  }

  out$Variable[duplicated(base_rows$var_name)] <- ""
  rownames(out) <- NULL
  out
}


# ------------------------------------------------------------
#                     T6B
# ------------------------------------------------------------
build_T6B_bch_stratified <- function() {

  df <- safe_df(BCH_STRATIFIED_RESULTS_FULL)
  if (!is.data.frame(df) || nrow(df) == 0) df <- safe_df(BCH_MOD_RESULTS_FULL)
  ph <- safe_df(BCH_STRATIFIED_POSTHOC)
  if (!is.data.frame(ph) || nrow(ph) == 0) ph <- safe_df(BCH_MOD_POSTHOC)

  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  if (!"moderator" %in% names(df) && "moderator_var" %in% names(df)) df$moderator <- df$moderator_var
  if (!"var_name" %in% names(df) && "outcome" %in% names(df)) df$var_name <- df$outcome
  need_df <- c("moderator", "moderator_level", "class_num", "estimate", "se", "var_name")
  if (!all(need_df %in% names(df))) return(data.frame())

  parse_bch_overall_from_out_local <- function(out_path) {
    out <- list(stat = "", p = "", sig = "", posthoc = "")
    if (is.null(out_path) || !nzchar(as.character(out_path)) || !file.exists(out_path)) return(out)

    lines <- tryCatch(readLines(out_path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    if (length(lines) == 0) return(out)

    x <- trimws(gsub("[[:space:]]+", " ", lines))
    x <- x[nzchar(x)]
    idx <- grep("EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE", x, ignore.case = TRUE)
    if (length(idx) == 0) return(out)

    blk <- x[idx[1]:length(x)]
    pair_txt <- character(0)

    for (ln in blk) {
      hit_overall <- regexec("Overall test\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)", ln, ignore.case = TRUE)
      tok_overall <- regmatches(ln, hit_overall)[[1]]
      if (length(tok_overall) >= 3) {
        stat_i <- suppressWarnings(as.numeric(tok_overall[2]))
        p_i <- suppressWarnings(as.numeric(tok_overall[3]))
        out$stat <- if (!is.na(stat_i)) sprintf("%.2f", stat_i) else ""
        out$p <- fmt_p3_strict(p_i)
        out$sig <- fmt_sig_cell(sig_mark(p_i))
      }

      hit_pairs <- gregexpr("Class\\s+([0-9]+)\\s+vs\\.\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)", ln, perl = TRUE, ignore.case = TRUE)
      pair_str <- regmatches(ln, hit_pairs)[[1]]
      if (length(pair_str) > 0) {
        for (one in pair_str) {
          tok <- regmatches(one, regexec("Class\\s+([0-9]+)\\s+vs\\.\\s+([0-9]+)\\s+([-]?[0-9]*\\.?[0-9]+)\\s+([.0-9]+)", one, perl = TRUE, ignore.case = TRUE))[[1]]
          if (length(tok) >= 5) {
            p_i <- suppressWarnings(as.numeric(tok[5]))
            if (!is.na(p_i) && p_i < .05) pair_txt <- c(pair_txt, paste0("C", tok[2], "-C", tok[3]))
          }
        }
      }
    }

    if (length(pair_txt) > 0) out$posthoc <- paste(unique(pair_txt), collapse = ", ")
    out
  }

  num_class <- suppressWarnings(as.integer(df$class_num))
  df_class <- df[!is.na(num_class), , drop = FALSE]
  df_overall <- df[
    is.na(num_class) &
      (!is.na(safe_num(df$stat)) | !is.na(safe_num(df$p))),
    ,
    drop = FALSE
  ]

  if (!is.data.frame(df_class) || nrow(df_class) == 0) return(data.frame())

  df_class$Moderator <- as.character(df_class$moderator)
  df_class$Level <- as.character(df_class$moderator_level)
  df_class$Profile <- paste0("Profile ", suppressWarnings(as.integer(df_class$class_num)))
  df_class$M <- fmt_m2(df_class$estimate)
  if (isTRUE(HAS_WEIGHT)) {
    df_class$SPREAD <- fmt_sd2(df_class$se)
    spread_key <- "SE"
  } else {
    raw_sum <- build_classified_outcome_summary(unique(df_class$var_name), moderator_var = unique(df_class$moderator)[1])
    raw_key <- paste(raw_sum$var_name, raw_sum$class_num, raw_sum$moderator_level, sep = "||")
    cls_key <- paste(df_class$var_name, df_class$class_num, df_class$moderator_level, sep = "||")
    df_class$SPREAD <- fmt_sd2(raw_sum$SD[match(cls_key, raw_key)])
    spread_key <- "SD"
  }
  if ("var_label" %in% names(df_class)) {
    df_class$Variable <- as.character(df_class$var_label)
  } else {
    df_class$Variable <- get_var_label(df_class$var_name)
  }

  keep_class <- c("Moderator", "Variable", "Profile", "Level", "var_name", "M", "SPREAD")
  df_class <- unique(df_class[, keep_class, drop = FALSE])

  levels_ord <- unique(df_class$Level)
  levels_ord <- levels_ord[order(suppressWarnings(as.numeric(levels_ord)), levels_ord)]
  moderator_vals <- unique(df_class$Moderator)
  if (length(moderator_vals) == 1L && nzchar(moderator_vals[1])) {
    level_labels <- setNames(paste0(moderator_vals[1], " ", levels_ord), levels_ord)
  } else {
    level_labels <- setNames(levels_ord, levels_ord)
  }

  base_rows <- unique(df_class[, c("Moderator", "Variable", "var_name", "Profile"), drop = FALSE])
  base_rows$..ord.. <- suppressWarnings(as.integer(gsub("^Profile\\s+", "", base_rows$Profile)))
  base_rows <- base_rows[
    order(base_rows$Moderator, base_rows$Variable, base_rows$..ord..),
    c("Moderator", "Variable", "var_name", "Profile"),
    drop = FALSE
  ]
  rownames(base_rows) <- NULL

  var_key <- paste(base_rows$Moderator, base_rows$var_name, sep = "||")
  row_key <- paste(base_rows$Moderator, base_rows$var_name, base_rows$Profile, sep = "||")

  out <- base_rows[, c("Moderator", "Variable", "Profile"), drop = FALSE]

  build_first_nonempty <- function(x, formatter = identity) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- ""
    nz <- x_chr[nzchar(x_chr)]
    if (length(nz) == 0) return("")
    formatter(nz[1])
  }

  stat_map <- data.frame(
    moderator = character(0),
    moderator_level = character(0),
    var_name = character(0),
    Statistic = character(0),
    p = character(0),
    sig = character(0),
    `post-hoc` = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (is.data.frame(df_overall) && nrow(df_overall) > 0) {
    grp_key <- paste(df_overall$moderator, df_overall$moderator_level, df_overall$var_name, sep = "||")
    grp_ids <- unique(grp_key)
    stat_list <- lapply(grp_ids, function(k) {
      ii <- grp_key == k
      one <- df_overall[ii, , drop = FALSE]
      data.frame(
        moderator = as.character(one$moderator[1]),
        moderator_level = as.character(one$moderator_level[1]),
        var_name = as.character(one$var_name[1]),
        Statistic = build_first_nonempty(one$stat, function(z) {
          zz <- safe_num(z)
          if (is.na(zz)) "" else sprintf("%.2f", zz)
        }),
        p = build_first_nonempty(one$p, function(z) fmt_p3_strict(safe_num(z))),
        sig = build_first_nonempty(one$p, function(z) fmt_sig_cell(sig_mark(safe_num(z)))),
        `post-hoc` = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    if (length(stat_list) > 0) stat_map <- do.call(rbind, stat_list)
  }

  if ("out_file" %in% names(df)) {
    file_rows <- unique(df[, intersect(c("moderator", "moderator_level", "var_name", "out_file"), names(df)), drop = FALSE])
    file_rows <- file_rows[!is.na(file_rows$out_file) & nzchar(as.character(file_rows$out_file)), , drop = FALSE]
    if (nrow(file_rows) > 0) {
      parsed_list <- lapply(seq_len(nrow(file_rows)), function(i) {
        pp <- parse_bch_overall_from_out_local(file_rows$out_file[i])
        data.frame(
          moderator = as.character(file_rows$moderator[i]),
          moderator_level = as.character(file_rows$moderator_level[i]),
          var_name = as.character(file_rows$var_name[i]),
          Statistic = as.character(pp$stat %||% ""),
          p = as.character(pp$p %||% ""),
          sig = as.character(pp$sig %||% ""),
          `post-hoc` = as.character(pp$posthoc %||% ""),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      })
      parsed_df <- do.call(rbind, parsed_list)
      parsed_key <- paste(parsed_df$moderator, parsed_df$moderator_level, parsed_df$var_name, sep = "||")
      if (nrow(stat_map) == 0) {
        stat_map <- parsed_df
      } else {
        stat_key <- paste(stat_map$moderator, stat_map$moderator_level, stat_map$var_name, sep = "||")
        for (cc in c("Statistic", "p", "sig", "post-hoc")) {
          miss <- is.na(stat_map[[cc]]) | !nzchar(as.character(stat_map[[cc]]))
          stat_map[[cc]][miss] <- parsed_df[[cc]][match(stat_key[miss], parsed_key)]
          stat_map[[cc]][is.na(stat_map[[cc]])] <- ""
        }
      }
    }
  }

  if (is.data.frame(ph) && nrow(ph) > 0 && all(c("moderator", "moderator_level", "p") %in% names(ph))) {
    ph_use <- ph[!is.na(safe_num(ph$p)) & safe_num(ph$p) < .05, , drop = FALSE]
    if (nrow(ph_use) > 0) {
      if ("var_name" %in% names(ph_use)) {
        ph_use$var_name <- as.character(ph_use$var_name)
      } else if ("outcome" %in% names(ph_use)) {
        ph_use$var_name <- as.character(ph_use$outcome)
      } else {
        ph_use$var_name <- ""
      }

      if (all(c("class1", "class2") %in% names(ph_use))) {
        ph_use$pair_txt <- paste0("C", ph_use$class1, "-C", ph_use$class2)
      } else {
        ph_use$pair_txt <- ""
      }

      ph_use <- ph_use[nzchar(ph_use$pair_txt), , drop = FALSE]
      if (nrow(ph_use) > 0) {
        grp_key <- paste(ph_use$moderator, ph_use$moderator_level, ph_use$var_name, sep = "||")
        grp_ids <- unique(grp_key)
        ph_list <- lapply(grp_ids, function(k) {
          one <- ph_use[grp_key == k, , drop = FALSE]
          data.frame(
            moderator = as.character(one$moderator[1]),
            moderator_level = as.character(one$moderator_level[1]),
            var_name = as.character(one$var_name[1]),
            `post-hoc` = paste(unique(one$pair_txt), collapse = ", "),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        })
        ph_map <- do.call(rbind, ph_list)

        if (nrow(stat_map) == 0) {
          stat_map <- ph_map
          stat_map$Statistic <- ""
          stat_map$p <- ""
          stat_map$sig <- ""
          stat_map <- stat_map[, c("moderator", "moderator_level", "var_name", "Statistic", "p", "sig", "post-hoc"), drop = FALSE]
        } else {
          stat_key <- paste(stat_map$moderator, stat_map$moderator_level, stat_map$var_name, sep = "||")
          ph_key <- paste(ph_map$moderator, ph_map$moderator_level, ph_map$var_name, sep = "||")
          stat_map$`post-hoc` <- ifelse(
            nzchar(stat_map$`post-hoc`),
            stat_map$`post-hoc`,
            ph_map$`post-hoc`[match(stat_key, ph_key)]
          )
          stat_map$`post-hoc`[is.na(stat_map$`post-hoc`)] <- ""

          miss_key <- setdiff(ph_key, stat_key)
          if (length(miss_key) > 0) {
            add_rows <- ph_map[ph_key %in% miss_key, , drop = FALSE]
            add_rows$Statistic <- ""
            add_rows$p <- ""
            add_rows$sig <- ""
            add_rows <- add_rows[, c("moderator", "moderator_level", "var_name", "Statistic", "p", "sig", "post-hoc"), drop = FALSE]
            stat_map <- rbind(stat_map, add_rows)
          }
        }
      }
    }
  }

  for (lv in levels_ord) {
    lv_label <- level_labels[[lv]]
    sub <- df_class[df_class$Level == lv, c("Moderator", "var_name", "Profile", "M", "SPREAD"), drop = FALSE]
    sub <- unique(sub)
    sub_key <- paste(sub$Moderator, sub$var_name, sub$Profile, sep = "||")

    out[[twoline_key("M", lv_label)]] <- sub$M[match(row_key, sub_key)]
    out[[twoline_key(spread_key, lv_label)]] <- sub$SPREAD[match(row_key, sub_key)]
    out[[twoline_key("M", lv_label)]][is.na(out[[twoline_key("M", lv_label)]])] <- ""
    out[[twoline_key(spread_key, lv_label)]][is.na(out[[twoline_key(spread_key, lv_label)]])] <- ""

    stat_col <- twoline_key("Statistic", lv_label)
    p_col <- twoline_key("p", lv_label)
    sig_col <- twoline_key("sig", lv_label)
    ph_col <- twoline_key("post-hoc", lv_label)
    out[[stat_col]] <- ""
    out[[p_col]] <- ""
    out[[sig_col]] <- ""
    out[[ph_col]] <- ""

    if (nrow(stat_map) > 0) {
      st <- stat_map[stat_map$moderator_level == lv, , drop = FALSE]
      if (nrow(st) > 0) {
        st_key <- paste(st$moderator, st$var_name, sep = "||")
        idx <- match(var_key, st_key)
        first_row <- !duplicated(var_key)

        stat_vals <- st$Statistic[idx]
        p_vals <- st$p[idx]
        sig_vals <- st$sig[idx]
        ph_vals <- st$`post-hoc`[idx]

        stat_vals[is.na(stat_vals)] <- ""
        p_vals[is.na(p_vals)] <- ""
        sig_vals[is.na(sig_vals)] <- ""
        ph_vals[is.na(ph_vals)] <- ""

        out[[stat_col]][first_row] <- stat_vals[first_row]
        out[[p_col]][first_row] <- p_vals[first_row]
        out[[sig_col]][first_row] <- sig_vals[first_row]
        out[[ph_col]][first_row] <- ph_vals[first_row]
      }
    }
  }

  if (length(unique(out$Moderator)) == 1L) out$Moderator <- NULL
  if (length(unique(out$Variable)) == 1L) out$Variable <- NULL
  rownames(out) <- NULL
  out
}

build_T6D_moderator_descriptives <- function() {
  df <- safe_df(BCH_MOD_RESULTS_FULL)
  if (!is_nonempty_df(df)) df <- safe_df(BCH_STRATIFIED_RESULTS_FULL)
  if (!is_nonempty_df(df)) return(data.frame())

  if (!"moderator" %in% names(df) && "moderator_var" %in% names(df)) df$moderator <- df$moderator_var
  if (!"var_name" %in% names(df) && "outcome" %in% names(df)) df$var_name <- df$outcome
  need_df <- c("moderator", "moderator_level", "class_num", "estimate", "se", "var_name")
  if (!all(need_df %in% names(df))) return(data.frame())

  df$class_num <- safe_int(df$class_num)
  df$estimate <- safe_num(df$estimate)
  df$se <- safe_num(df$se)
  df$moderator <- as.character(df$moderator)
  df$moderator_level <- as.character(df$moderator_level)
  df$var_name <- as.character(df$var_name)

  df <- df[
    !is.na(df$class_num) &
      !is.na(df$estimate) &
      nzchar(df$moderator) &
      nzchar(df$moderator_level) &
      nzchar(df$var_name),
    ,
    drop = FALSE
  ]
  if (!is_nonempty_df(df)) return(data.frame())

  if ("var_label" %in% names(df)) {
    df$Variable <- as.character(df$var_label)
    miss_var <- is.na(df$Variable) | !nzchar(trimws(df$Variable))
    df$Variable[miss_var] <- get_var_label(df$var_name[miss_var])
  } else {
    df$Variable <- get_var_label(df$var_name)
  }

  df$Moderator <- get_var_label(df$moderator)
  df$Profile <- paste0("Profile ", df$class_num)
  df$Level <- vapply(
    seq_len(nrow(df)),
    function(i) {
      lab <- get_value_label(df$moderator[i], df$moderator_level[i])
      if (is.na(lab) || !nzchar(trimws(lab))) as.character(df$moderator_level[i]) else lab
    },
    character(1)
  )

  keep_class <- c("Moderator", "Variable", "Profile", "Level", "var_name", "moderator", "moderator_level", "class_num", "estimate", "se")
  df <- unique(df[, keep_class, drop = FALSE])
  df <- df[order(df$Moderator, df$Variable, df$class_num), , drop = FALSE]

  base_rows <- unique(df[, c("Moderator", "Variable", "var_name", "Profile", "class_num"), drop = FALSE])
  base_rows <- base_rows[order(base_rows$Moderator, base_rows$Variable, base_rows$class_num), , drop = FALSE]
  row_key <- paste(base_rows$Moderator, base_rows$var_name, base_rows$Profile, sep = "||")

  out <- base_rows[, c("Moderator", "Variable", "Profile"), drop = FALSE]

  level_map <- unique(df[, c("moderator", "moderator_level", "Level"), drop = FALSE])
  level_map$..ord.. <- suppressWarnings(as.numeric(level_map$moderator_level))
  level_map$..ord..[is.na(level_map$..ord..)] <- seq_len(sum(is.na(level_map$..ord..))) + 9999
  level_map <- level_map[order(level_map$moderator, level_map$..ord.., level_map$moderator_level), , drop = FALSE]
  level_map$..ord.. <- NULL
  level_keys <- unique(paste(level_map$moderator, level_map$moderator_level, sep = "||"))

  for (lv_key in level_keys) {
    lv_row <- level_map[paste(level_map$moderator, level_map$moderator_level, sep = "||") == lv_key, , drop = FALSE][1, , drop = FALSE]
    lv_label <- paste0(get_var_label(lv_row$moderator), " ", lv_row$Level)
    sub <- df[
      df$moderator == lv_row$moderator & df$moderator_level == lv_row$moderator_level,
      c("Moderator", "var_name", "Profile", "estimate", "se"),
      drop = FALSE
    ]
    sub_key <- paste(sub$Moderator, sub$var_name, sub$Profile, sep = "||")

    out[[twoline_key("M", lv_label)]] <- fmt_m2(sub$estimate[match(row_key, sub_key)])
    if (isTRUE(HAS_WEIGHT)) {
      out[[twoline_key("SE", lv_label)]] <- fmt_sd2(sub$se[match(row_key, sub_key)])
      out[[twoline_key("SE", lv_label)]][is.na(out[[twoline_key("SE", lv_label)]])] <- ""
    } else {
      raw_sum <- build_classified_outcome_summary(unique(df$var_name), moderator_var = unique(df$moderator)[1])
      raw_key <- paste(raw_sum$var_name, raw_sum$class_num, raw_sum$moderator_level, sep = "||")
      sd_vals <- raw_sum$SD[match(paste(base_rows$var_name, base_rows$class_num, lv_row$moderator_level, sep = "||"), raw_key)]
      out[[twoline_key("SD", lv_label)]] <- fmt_sd2(sd_vals)
      out[[twoline_key("SD", lv_label)]][is.na(out[[twoline_key("SD", lv_label)]])] <- ""
    }
    out[[twoline_key("M", lv_label)]][is.na(out[[twoline_key("M", lv_label)]])] <- ""
  }

  if (length(unique(out$Moderator)) == 1L) out$Moderator <- NULL
  if (length(unique(out$Variable)) == 1L) out$Variable <- NULL
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
#                     T6C
# ------------------------------------------------------------
build_T6C_bch_interaction <- function() {

  df <- safe_df(BCH_INTERACTION)
  if (!is_nonempty_df(df)) return(data.frame())

  if (!"moderator" %in% names(df)) df$moderator <- ""
  if (!"outcome"   %in% names(df)) df$outcome   <- ""
  if (!"effect"    %in% names(df)) df$effect    <- "class*group"
  if (!"stat"      %in% names(df)) df$stat      <- NA_real_
  if (!"p"         %in% names(df)) df$p         <- NA_real_
  if (!"p_fmt"     %in% names(df)) df$p_fmt     <- fmt_p3_strict(df$p)
  if (!"sig"       %in% names(df)) df$sig       <- sig_mark(df$p)

  df$Moderator <- get_var_label(df$moderator)
  df$Variable  <- get_var_label(df$outcome)

  # effect ???????椰????????????
  df$effect <- as.character(df$effect)
  df$effect[df$effect %in% c("class:group", "class*group")] <- "class*group"

  # ????????????濾?????????
  df$effect <- factor(df$effect, levels = c("class", "group", "class*group"))
  df <- df[order(df$moderator, df$outcome, df$effect), , drop = FALSE]

  # ?????????????????꿔꺂???⑸븶????????
  df$Effect    <- as.character(df$effect)
  df$Statistic <- ifelse(is.na(df$stat), "", sprintf("%.2f", df$stat))
  df$p_out     <- df$p_fmt
  df$sig_out   <- df$sig

  out <- df[, c("Moderator", "Variable", "Effect", "Statistic", "p_out", "sig_out"), drop = FALSE]
  names(out) <- c("Moderator", "Variable", "Effect", "Statistic", "p", "sig")

  # ???????? ??????????밸븶筌믩끃??獄???????멥렑??????????????????????????(SCI ?????
  out$Moderator[duplicated(paste(out$Moderator, out$Variable))] <- ""
  out$Variable[duplicated(out$Variable)] <- ""

  rownames(out) <- NULL
  out
}

build_T6C_bch_interaction <- function() {

  df <- safe_df(BCH_INTERACTION)
  if (!is_nonempty_df(df)) return(data.frame())

  if (!"row_type"   %in% names(df)) df$row_type <- "omnibus"
  if (!"moderator"  %in% names(df)) df$moderator <- ""
  if (!"outcome"    %in% names(df)) df$outcome <- ""
  if (!"effect"     %in% names(df)) df$effect <- "class*moderator"
  if (!"term"       %in% names(df)) df$term <- as.character(df$effect)
  if (!"estimate"   %in% names(df)) df$estimate <- NA_real_
  if (!"se"         %in% names(df)) df$se <- NA_real_
  if (!"t_value"    %in% names(df)) df$t_value <- NA_real_
  if (!"llci"       %in% names(df)) df$llci <- NA_real_
  if (!"ulci"       %in% names(df)) df$ulci <- NA_real_
  if (!"stat"       %in% names(df)) df$stat <- NA_real_
  if (!"p"          %in% names(df)) df$p <- NA_real_
  if (!"p_fmt"      %in% names(df)) df$p_fmt <- fmt_p3_strict(df$p)
  if (!"sig"        %in% names(df)) df$sig <- sig_mark(df$p)
  if (!"moderator_type" %in% names(df)) df$moderator_type <- ""

  df$Moderator <- get_var_label(df$moderator)
  df$Variable  <- get_var_label(df$outcome)

  coef_df <- df[
    as.character(df$row_type) == "coefficient" &
      !is.na(df$estimate),
    ,
    drop = FALSE
  ]

  if (nrow(coef_df) > 0) {
    coef_df$effect <- as.character(coef_df$effect)
    coef_df$effect[coef_df$effect == "class*moderator"] <- "class x moderator"
    coef_df$effect[coef_df$effect == "moderator"] <- "moderator"
    coef_df$effect[coef_df$effect == "class"] <- "class"
    coef_df$Effect <- coef_df$effect
    coef_df$Term <- as.character(coef_df$term)
    coef_df$B    <- ifelse(is.na(coef_df$estimate), "", sprintf("%.3f", coef_df$estimate))
    coef_df$SE   <- ifelse(is.na(coef_df$se), "", sprintf("%.3f", coef_df$se))
    coef_df$t    <- ifelse(is.na(coef_df$t_value), "", sprintf("%.2f", coef_df$t_value))
    coef_df$LLCI <- ifelse(is.na(coef_df$llci), "", sprintf("%.3f", coef_df$llci))
    coef_df$ULCI <- ifelse(is.na(coef_df$ulci), "", sprintf("%.3f", coef_df$ulci))
    coef_df$p_out <- coef_df$p_fmt
    coef_df$sig_out <- coef_df$sig

    eff_ord <- c("class", "moderator", "class x moderator")
    coef_df$..eff.. <- match(coef_df$Effect, eff_ord)
    coef_df$..eff..[is.na(coef_df$..eff..)] <- 999L
    coef_df <- coef_df[order(coef_df$moderator, coef_df$outcome, coef_df$..eff.., coef_df$Term), , drop = FALSE]

    out <- coef_df[, c("Moderator", "Variable", "Effect", "Term", "B", "SE", "t", "LLCI", "ULCI", "p_out", "sig_out"), drop = FALSE]
    names(out) <- c("Moderator", "Variable", "Effect", "Term", "B", "SE", "t", "LLCI", "ULCI", "p", "sig")

    grp_key <- paste(coef_df$Moderator, coef_df$Variable)
    out$Moderator[duplicated(grp_key)] <- ""
    out$Variable[duplicated(grp_key)] <- ""
    out$Effect[duplicated(paste(grp_key, coef_df$Effect))] <- ""

    rownames(out) <- NULL
    return(out)
  }

  df$effect <- as.character(df$effect)
  df$effect[df$effect %in% c("class:group", "class*group")] <- "class*moderator"
  df$effect[df$effect == "class*moderator"] <- "class x moderator"
  df$effect[df$effect == "moderator"] <- "moderator"
  df$effect[df$effect == "class"] <- "class"
  df$Effect <- df$effect
  df$Statistic <- ifelse(is.na(df$stat), "", sprintf("%.2f", df$stat))
  df$p_out <- df$p_fmt
  df$sig_out <- df$sig

  out <- df[, c("Moderator", "Variable", "Effect", "Statistic", "p_out", "sig_out"), drop = FALSE]
  names(out) <- c("Moderator", "Variable", "Effect", "Statistic", "p", "sig")

  out$Moderator[duplicated(paste(df$Moderator, df$Variable))] <- ""
  out$Variable[duplicated(paste(df$Moderator, df$Variable))] <- ""

  rownames(out) <- NULL
  out
}


# ------------------------------------------------------------
# 9.        T7
# ------------------------------------------------------------
build_T7_registry <- function() {
  data.frame(
    item = c(
      "mixture_mode",
      "best_k",
      "best_tag",
      "model_structure",
      "reference_class",
      "n_fit_rows",
      "n_class_rows",
      "n_r3step_rows",
      "n_bch_rows"
    ),
    value = c(
      mixture_mode,
      best_k,
      best_tag,
      model_structure,
      REFERENCE_CLASS,
      nrow(safe_df(FIT_SUMMARY)),
      nrow(safe_df(CLASS_SUMMARY_FINAL)),
      nrow(safe_df(T5_RRR_RAW)),
      nrow(safe_df(BCH_RESULTS))
    ),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 10. appendix / supplement
# ------------------------------------------------------------
build_A5_posterior_max <- function() {
  df <- safe_df(CLASSIFY_SUMMARY$posterior_max_summary %||% data.frame())
  if (nrow(df) == 0) df <- safe_df(POSTERIOR_MAX_DF)
  if (nrow(df) == 0) {
    src <- safe_df(CLASS_SUMMARY_FINAL)
    if (nrow(src) == 0) src <- safe_df(CLASS_SUMMARY)
    if (nrow(src) > 0) {
      cls_col <- first_existing_col(src, c("class_label", "Class", "class", "profile"))
      n_col <- first_existing_col(src, c("weighted_n", "n"))
      p_col <- first_existing_col(src, c("weighted_percent", "percent"))
      mp_col <- first_existing_col(src, c("mean_posterior"))
      prop_use <- normalize_prop_by_source(src[[p_col]], p_col)
      app_use <- safe_num(src[[mp_col]])
      occ_use <- rep(NA_real_, length(app_use))
      occ_ok <- is.finite(app_use) & is.finite(prop_use) &
        app_use > 0 & app_use < 0.9995 &
        prop_use > 0 & prop_use < 1
      occ_use[occ_ok] <- (
        (app_use[occ_ok] / (1 - app_use[occ_ok])) /
          (prop_use[occ_ok] / (1 - prop_use[occ_ok]))
      )
      n_name <- "n"
      p_name <- "%"
      return(data.frame(
        Profile = normalize_latent_group_text(as.character(src[[cls_col]])),
        n = fmt_n0(src[[n_col]]),
        `%` = fmt_pct1_plain(100 * prop_use),
        APP = fmt_m2(app_use),
        OCC = fmt_rrr3(occ_use),
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    return(data.frame(Note = "Posterior summary object not available.", stringsAsFactors = FALSE))
  }
  df
}

build_A6_classification_bands <- function() {
  df <- safe_df(CLASSIFY_SUMMARY$classification_bands %||% data.frame())
  if (nrow(df) == 0) df <- safe_df(CLASSIFICATION_QUALITY)
  if (nrow(df) == 0) {
    src <- safe_df(CLASS_SUMMARY_FINAL)
    if (nrow(src) == 0) src <- safe_df(CLASS_SUMMARY)
    if (nrow(src) > 0 && "mean_posterior" %in% names(src)) {
      mp <- safe_num(src$mean_posterior)
      band <- ifelse(
        is.na(mp), "",
        ifelse(mp >= 0.90, "Excellent",
          ifelse(mp >= 0.80, "Good",
            ifelse(mp >= 0.70, "Acceptable", "Low")
          )
        )
      )
      cls_col <- first_existing_col(src, c("class_label", "Class", "class", "profile"))
      return(data.frame(
        Profile = normalize_latent_group_text(as.character(src[[cls_col]])),
        MeanPosterior = fmt_m2(mp),
        Band = band,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }
    return(data.frame(Note = "Classification band object not available.", stringsAsFactors = FALSE))
  }
  df
}

# ------------------------------------------------------------
#    S1, S2, S3, S4, S5, S6
# ------------------------------------------------------------

build_S1_overview <- function() {
  data.frame(
    Metric = c("mixture_mode", "best_k", "best_tag", "model_structure", "reference_class"),
    Value = c(latent_analysis_type(), best_k, best_tag, model_structure, normalize_latent_group_text(REFERENCE_CLASS)),
    setNames(list(c(
      as.character(latent_analysis_type()),
      as.character(best_k),
      as.character(best_tag),
      as.character(model_structure),
      as.character(REFERENCE_CLASS)
    )), compact_mean_name(FALSE)),
    stringsAsFactors = FALSE
  )
}

build_S2_desc_cont <- function() {
  data.frame(Note = "Reserved for continuous auxiliary descriptions", stringsAsFactors = FALSE)
}

build_T0_sample_flow <- function() {
  raw_n <- if (exists("RAW_DATA", inherits = TRUE)) nrow(safe_df(get("RAW_DATA", inherits = TRUE))) else NA_integer_
  ana_n <- if (exists("ANALYSIS_DATA_SUB", inherits = TRUE)) nrow(safe_df(get("ANALYSIS_DATA_SUB", inherits = TRUE))) else NA_integer_
  if (is.na(raw_n)) raw_n <- ana_n
  if (is.na(ana_n)) ana_n <- raw_n
  excl_n <- ifelse(is.na(raw_n) || is.na(ana_n), NA_integer_, raw_n - ana_n)

  data.frame(
    Step = c("Initial sample", "Final analytic sample", "Excluded / removed"),
    N = c(fmt_n0(raw_n), fmt_n0(ana_n), fmt_n0(excl_n)),
    stringsAsFactors = FALSE
  )
}

build_A8_misclassification_matrix <- function() {
  df <- safe_df(CLASSIFIED_ANALYSIS)
  if (nrow(df) == 0) df <- safe_df(ANALYSIS_DATA_CLASSIFIED)
  if (nrow(df) == 0 || !"class_num" %in% names(df)) {
    return(data.frame(Note = "Misclassification matrix object not available.", stringsAsFactors = FALSE))
  }

  post_cols <- grep("^post_class[0-9]+$", names(df), value = TRUE)
  if (length(post_cols) == 0) {
    return(data.frame(Note = "Misclassification matrix object not available.", stringsAsFactors = FALSE))
  }

  cls_num <- safe_int(df$class_num)
  keep <- !is.na(cls_num)
  df <- df[keep, , drop = FALSE]
  cls_num <- cls_num[keep]
  if (nrow(df) == 0) {
    return(data.frame(Note = "Misclassification matrix object not available.", stringsAsFactors = FALSE))
  }

  class_ids <- suppressWarnings(as.integer(gsub("^post_class", "", post_cols)))
  ord <- order(class_ids)
  post_cols <- post_cols[ord]
  class_ids <- class_ids[ord]
  row_ids <- sort(unique(cls_num))

  out <- data.frame(
    setNames(list(latent_group_label(row_ids)), paste0("Assigned ", tolower(latent_group_term()))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (j in seq_along(post_cols)) {
    vals <- tapply(safe_num(df[[post_cols[j]]]), cls_num, mean, na.rm = TRUE)
    out[[latent_group_label(class_ids[j])]] <- fmt_m2(vals[match(row_ids, as.integer(names(vals)))])
  }
  out
}

build_S5_primary_rrr_twoline <- function() {
  df <- safe_df(T5d)
  if (nrow(df) == 0) return(data.frame())
  if (!all(c("Comparison", "Variable", "Category") %in% names(df))) return(data.frame())
  fmt3 <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x_num), "", formatC(x_num, format = "f", digits = 3))
  }

  cmp_fill <- as.character(df$Comparison)
  for (i in seq_along(cmp_fill)) {
    if (!nzchar(trimws(cmp_fill[i])) && i > 1) cmp_fill[i] <- cmp_fill[i - 1]
  }
  df$Comparison_fill <- cmp_fill

  out <- unique(df[, c("Variable", "Category"), drop = FALSE])
  row_key <- paste(df$Variable, df$Category, sep = "||")
  base_key <- paste(out$Variable, out$Category, sep = "||")

  cmp_names <- unique(df$Comparison_fill[nzchar(df$Comparison_fill)])
  cmp_num <- extract_class_num_from_text(cmp_names)
  cmp_names <- cmp_names[order(cmp_num)]

  for (cmp in cmp_names) {
    sub <- df[df$Comparison_fill == cmp, , drop = FALSE]
    sub_key <- paste(sub$Variable, sub$Category, sep = "||")
    idx <- match(base_key, sub_key)
    out[[twoline_key("RRR", cmp)]] <- ifelse(
      is.na(idx), "",
      fmt3(sub$RRR__Primary[idx])
    )
    out[[twoline_key("(LLCI~ULCI)", cmp)]] <- ifelse(
      is.na(idx), "",
      ifelse(
        is.na(safe_num(sub$LLCI__Primary[idx])) | is.na(safe_num(sub$ULCI__Primary[idx])),
        "",
        paste0("(", fmt3(sub$LLCI__Primary[idx]), "~", fmt3(sub$ULCI__Primary[idx]), ")")
      )
    )
    out[[twoline_key("p", cmp)]] <- ifelse(is.na(idx), "", as.character(sub$p__Primary[idx]))
  }

  out
}

build_S3_desc_cat <- function() {
  data.frame(Note = "Reserved for categorical auxiliary descriptions", stringsAsFactors = FALSE)
}

build_S4_summary_merged <- function() {
  data.frame(Note = "Reserved for merged auxiliary summary", stringsAsFactors = FALSE)
}

build_S5_primary_rrr <- function() {
  build_S5_primary_rrr_twoline()
}

build_rrr_detail_twoline <- function(df) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(data.frame())
  if (!all(c("Comparison", "Variable", "Category") %in% names(df))) return(data.frame())

  fmt3 <- function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x_num), "", formatC(x_num, format = "f", digits = 3))
  }

  cmp_fill <- as.character(df$Comparison)
  for (i in seq_along(cmp_fill)) {
    if (!nzchar(trimws(cmp_fill[i])) && i > 1) cmp_fill[i] <- cmp_fill[i - 1]
  }
  df$Comparison_fill <- cmp_fill

  out <- unique(df[, c("Variable", "Category"), drop = FALSE])
  row_key <- paste(df$Variable, df$Category, sep = "||")
  base_key <- paste(out$Variable, out$Category, sep = "||")

  cmp_names <- unique(df$Comparison_fill[nzchar(df$Comparison_fill)])
  cmp_num <- extract_class_num_from_text(cmp_names)
  cmp_names <- cmp_names[order(cmp_num)]

  for (cmp in cmp_names) {
    sub <- df[df$Comparison_fill == cmp, , drop = FALSE]
    sub_key <- paste(sub$Variable, sub$Category, sep = "||")
    idx <- match(base_key, sub_key)
    out[[twoline_key("RRR", cmp)]] <- ifelse(is.na(idx), "", fmt3(sub$RRR[idx]))
    out[[twoline_key("(LLCI~ULCI)", cmp)]] <- ifelse(
      is.na(idx),
      "",
      ifelse(
        is.na(safe_num(sub$LLCI[idx])) | is.na(safe_num(sub$ULCI[idx])),
        "",
        paste0("(", fmt3(sub$LLCI[idx]), "~", fmt3(sub$ULCI[idx]), ")")
      )
    )
    out[[twoline_key("p", cmp)]] <- ifelse(is.na(idx), "", as.character(sub$p[idx]))
  }

  out
}

build_S6_multinom_detail <- function() {
  df <- build_covariate_long_common(safe_df(R3STEP_RESULTS_RAW$univariable %||% data.frame()))
  if (nrow(df) == 0) return(data.frame())
  out <- build_rrr_detail_twoline(df)
  keep <- names(out) %in% c("Variable", "Category") |
    grepl("^RRR__", names(out)) |
    grepl("^\\(LLCI~ULCI\\)__", names(out)) |
    grepl("^p__", names(out))
  out <- out[, keep, drop = FALSE]
  out <- out[, names(out) != "RRR (LLCI~ULCI)", drop = FALSE]
  out
}

# ------------------------------------------------------------
#                     T6B
# ------------------------------------------------------------
build_T6B_bch_stratified_legacy <- function() {

  df <- safe_df(BCH_MOD_RESULTS_FULL)
  ph <- safe_df(BCH_MOD_POSTHOC)

  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  need_df <- c("moderator", "moderator_level", "class_num", "estimate", "se", "var_name")
  if (!all(need_df %in% names(df))) return(data.frame())

  df$Moderator <- as.character(df$moderator)
  df$Level     <- as.character(df$moderator_level)
  df$Profile   <- paste0("Profile ", suppressWarnings(as.integer(df$class_num)))
  df$M         <- fmt_m2(df$estimate)
  df$SD        <- fmt_sd2(df$se)

  if ("var_label" %in% names(df)) {
    df$Variable <- as.character(df$var_label)
  } else {
    df$Variable <- get_var_label(df$var_name)
  }

  # p/sig from BCH_MOD_RESULTS_FULL omnibus_p
  p_map <- unique(
    df[, c("moderator", "moderator_level", "var_name", "omnibus_p"), drop = FALSE]
  )

  p_map$p_fmt <- fmt_p3_strict(p_map$omnibus_p)
  p_map$sig   <- sig_mark(p_map$omnibus_p)

  df <- merge(
    df,
    p_map[, c("moderator", "moderator_level", "var_name", "p_fmt", "sig")],
    by = c("moderator", "moderator_level", "var_name"),
    all.x = TRUE,
    sort = FALSE
  )

  if (!"p_fmt" %in% names(df)) df$p_fmt <- ""
  if (!"sig" %in% names(df))   df$sig   <- ""

  df$p_fmt[is.na(df$p_fmt)] <- ""
  df$sig[is.na(df$sig)]     <- ""

  # moderator-level post-hoc
  df$Post_hoc <- ""

  if (is.data.frame(ph) && nrow(ph) > 0) {
    ph_use <- ph

    # moderator info??????????? ??????????????????됰Ŧ?????????????붺몭??잞쭜???????posthoc ???
    need_ph <- c("moderator", "moderator_level", "outcome", "class1", "class2", "p")
    if (all(need_ph %in% names(ph_use))) {
      ph_use <- ph_use[!is.na(safe_num(ph_use$p)) & safe_num(ph_use$p) < .05, , drop = FALSE]

      if (nrow(ph_use) > 0) {
        ph_use$pair_txt <- paste0("C", ph_use$class1, "-C", ph_use$class2)

        ph_sum <- aggregate(
          pair_txt ~ moderator + moderator_level + outcome,
          data = ph_use,
          FUN = function(z) paste(unique(z), collapse = ", ")
        )

        names(ph_sum)[names(ph_sum) == "outcome"] <- "var_name"

        df <- merge(
          df,
          ph_sum,
          by = c("moderator", "moderator_level", "var_name"),
          all.x = TRUE,
          sort = FALSE
        )

        if ("pair_txt" %in% names(df)) {
          df$Post_hoc <- ifelse(is.na(df$pair_txt), "", as.character(df$pair_txt))
          df$pair_txt <- NULL
        }
      }
    }
  }

  out <- df[, c("Moderator", "Level", "Variable", "Profile", "M", "SD", "p_fmt", "sig", "Post_hoc"), drop = FALSE]
  names(out)[names(out) == "p_fmt"]   <- "p"
  names(out)[names(out) == "Post_hoc"] <- "Post-hoc"

  out <- out[order(out$Moderator, out$Level, out$Variable, out$Profile), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 11. table registry
# ------------------------------------------------------------
TABLE_BUILDERS <- list(
  T0  = build_T0_sample_flow,
  T1  = build_T1_model_selection,
  T2  = build_T2_model_fit,
  T3  = build_T3_profile_size,
  T4  = build_T4_profile_raw,
  T5  = build_T5_naive,
  T5b = build_T5b_postwgt,
  T5c = build_T5c_LTRBLT,
  T5d = build_T5d_compare,
  T6  = build_T6_bch,
  T6b = build_T6b_bch_categorical_multinom,
  T6C = build_T6C_bch_interaction,
  T6D = build_T6D_moderator_descriptives,
  T6E = build_T6B_bch_stratified,
  T7  = build_T7_registry,
  A3  = build_A3_profile_raw_wide,
  A4  = build_A4_profile_z_wide,
  A5  = build_A5_posterior_max,
  A6  = build_A6_classification_bands,
  A8  = build_A8_misclassification_matrix,
  S1  = build_S1_overview,
  S2  = build_S2_desc_cont,
  S3  = build_S3_desc_cat,
  S4  = build_S4_summary_merged,
  S5  = build_S5_primary_rrr,
  S6  = build_S6_multinom_detail
)

TABLE_META <- list(
  T0  = list(caption = "Table 0. Overview of analysis inputs and settings", type = "normal", note_type = "generic"),
  T1  = list(caption = paste0("Table 1. Summary of the retained latent ", latent_group_term_lower(), " solution"), type = "normal", note_type = "generic"),
  T2  = list(caption = paste0("Table 2. Fit indices for candidate ", latent_group_term_lower(), " solutions"), type = "normal", note_type = "generic"),
  T3  = list(caption = paste0("Table 3. ", latent_group_term(), " sizes in the retained solution"), type = "normal", note_type = size_note_type),
    T4  = list(
      caption = if (tolower(as.character(mixture_mode %||% "lpa")) == "lca")
        "Table 4. Indicator category distribution by class"
      else if (length(SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% SETTINGS_SUMMARY$indicators_continuous %||% character(0)) > 0 &&
               length(SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% SETTINGS_SUMMARY$indicators_categorical %||% character(0)) > 0)
        paste0("Table 4. Indicator means and category distributions by ", tolower(latent_group_term()))
      else
        paste0("Table 4. Indicator means by ", tolower(latent_group_term())),
      type = if (isTRUE(has_mixed_indicators())) "twoline_mixed" else t4_table_type,
      note_type = if (tolower(as.character(mixture_mode %||% "lpa")) == "lca")
        "n_pct"
      else if (isTRUE(has_mixed_indicators()))
        "mixed_split"
      else
        spread_note_type
    ),
  T5  = list(caption = paste0("Table 5. Covariates predicting ", tolower(latent_group_term()), " membership"), type = "twoline_rrr", note_type = "rrr"),
  T5b = list(caption = paste0("Table 5b. Covariates predicting ", tolower(latent_group_term()), " membership: primary analysis"), type = "twoline_rrr", note_type = "rrr"),
  T5c = list(caption = paste0("Table 5c. Covariates predicting ", tolower(latent_group_term()), " membership: sensitivity analysis"), type = "twoline_rrr", note_type = "rrr"),
  T5d = list(
    caption    = "Table 5d. Comparison of covariate results across analytic approaches",
    type       = "twoline_rrr",
    note_type  = "rrr_compact"
  ),
  T6  = list(
    caption   = paste0("Table 6. Distal outcomes by ", tolower(latent_group_term())),
    type      = "normal",
    note_type = spread_note_type
  ),
  T6b = list(caption = paste0("Table 6b. Multinomial logistic regression for categorical distal outcomes by ", tolower(latent_group_term())), type = "twoline_rrr", note_type = "rrr"),
  T6C = list(caption = paste0("Table 6C. PROCESS Model 1-style regression coefficients for ", latent_group_term_lower(), " x moderator effects on distal outcomes"),type = "normal", note_type = "generic"),
  T6D = list(caption = paste0("Table 6D. Distal outcome means by ", tolower(latent_group_term()), " across moderator levels"), type = t6d_table_type, note_type = spread_note_type),
  T6E = list(caption = paste0("Table 6E. Distal outcomes by ", tolower(latent_group_term()), " within moderator levels"), type = "twoline_t6b", note_type = spread_note_type),
  T7  = list(caption = "Table 7. Analysis summary", type = "normal", note_type = "generic"),

  A3  = list(
    caption = if (length(SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||% SETTINGS_SUMMARY$indicators_continuous %||% character(0)) > 0 &&
                  length(SETTINGS_SUMMARY$INDICATORS_CATEGORICAL %||% SETTINGS_SUMMARY$indicators_categorical %||% character(0)) > 0)
      paste0("Appendix Table A3. Indicator means and category distributions by ", tolower(latent_group_term()), " (raw scale)")
    else
      paste0("Appendix Table A3. Indicator means by ", tolower(latent_group_term()), " (raw scale)"),
    type = if (isTRUE(has_mixed_indicators())) "twoline_mixed" else "normal",
    note_type = if (isTRUE(has_mixed_indicators())) "mixed_split" else compact_spread_note_type
  ),
  A4  = list(caption = paste0("Appendix Table A4. Indicator means by ", tolower(latent_group_term()), " (standardized scale)"), type = "normal", note_type = compact_spread_note_type),
    A5  = list(caption = "Appendix Table A5. Classification summary", type = "normal", note_type = a5_note_type),
  A6  = list(caption = paste0("Appendix Table A6. Classification quality by ", tolower(latent_group_term())), type = "normal", note_type = "generic"),
  A8  = list(caption = "Appendix Table A8. Misclassification matrix", type = "normal", note_type = "generic"),

  S1  = list(caption = "Supplement Table S1. Overview of auxiliary analyses", type = "normal", note_type = "generic"),
  S2  = list(caption = paste0("Supplement Table S2. Continuous auxiliary variables by ", tolower(latent_group_term())), type = "normal", note_type = "mean_sd_compact"),
  S3  = list(caption = paste0("Supplement Table S3. Categorical auxiliary variables by ", tolower(latent_group_term())), type = "normal", note_type = "n_pct"),
  S4  = list(caption = paste0("Supplement Table S4. Auxiliary variable summary across ", tolower(latent_group_term()), "s"), type = "normal", note_type = "generic"),
  S5  = list(caption = "Supplement Table S5. Primary multinomial results", type = "twoline_s5", note_type = "rrr_compact"),
  S6  = list(caption = "Supplement Table S6. Multinomial model details", type = "twoline_s5", note_type = "rrr_compact")
)

# ------------------------------------------------------------
# 12. workbook writers
# ------------------------------------------------------------
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required for 05_tables.R", call. = FALSE)
}

base_title_style <- function() {
  openxlsx::createStyle(
    textDecoration = "bold",
    fontSize       = 12,
    halign         = "left",
    valign         = "center",
    wrapText       = TRUE
  )
}

base_header_style <- function() {
  openxlsx::createStyle(
    textDecoration = "bold",
    halign         = "center",
    valign         = "center",
    border         = c("top", "bottom", "left", "right"),
    borderStyle    = c("thick", "thin", "thin", "thin"),
    wrapText       = TRUE
  )
}

base_body_style <- function() {
  openxlsx::createStyle(
    valign         = "center",
    wrapText       = FALSE
  )
}

write_review_sheet <- function(wb, sheet_name, dat, title_text = NULL) {
  if (is.null(title_text) || !nzchar(title_text)) title_text <- sheet_name
  dat <- safe_df(dat)

  openxlsx::addWorksheet(wb, sheet_name)

  merge_to <- max(2, min(max(1, ncol(dat)), 8))
  openxlsx::writeData(wb, sheet_name, data.frame(title_text), startRow = 1, startCol = 1, colNames = FALSE)
  if (merge_to > 1) {
    openxlsx::mergeCells(wb, sheet_name, cols = 1:merge_to, rows = 1)
  }

  title_style  <- base_title_style()
  header_style <- base_header_style()
  body_style   <- base_body_style()

  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 24)

  if (ncol(dat) == 0) {
    dat          <- data.frame(Note = character(), stringsAsFactors = FALSE)
  }

  has_note       <- nrow(dat) > 0 && grepl("^Note\\.", as.character(dat[[1]][nrow(dat)]))
  note_row_excel <- if (has_note) nrow(dat) + 3 else NA_integer_

  openxlsx::writeData(wb, sheet_name, dat, startRow = 3, startCol = 1, withFilter = FALSE)

  if (ncol(dat) > 0) {
    openxlsx::addStyle(wb, sheet_name, header_style, rows = 3, cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, sheet_name, rows = 3, heights = 22)
  }

  if (nrow(dat) > 0 && ncol(dat) > 0) {
    body_rows <- 4:(nrow(dat) + 3)
    openxlsx::addStyle(wb, sheet_name, body_style, rows = body_rows, cols = 1:ncol(dat), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(
      wb, sheet_name,
      openxlsx::createStyle(halign = "left", valign = "center"),
      rows       = body_rows, cols = 1,
      gridExpand = TRUE, stack = TRUE
    )

    if (!has_note) {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(border = "bottom", borderStyle = "thick"),
        rows                         = nrow(dat) + 3, cols = 1:ncol(dat),
        gridExpand                   = TRUE, stack = TRUE
      )
    } else {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(border = "top", borderStyle = "thick"),
        rows                         = note_row_excel, cols = 1:ncol(dat),
        gridExpand                   = TRUE, stack = TRUE
      )
    }
  }

  if (ncol(dat) > 0) {
    set_safe_colwidths(wb, sheet_name, dat)
  }
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 4, firstActiveCol = 2)
}

write_twoline_stat_sheet <- function(wb,
                                     sheet_name,
                                     dat,
                                     title_text = NULL,
                                     stat_order = c("M", "SD"),
                                     fixed_cols = NULL,
                                     group_stat_map = NULL) {
  if (is.null(title_text) || !nzchar(title_text)) title_text <- sheet_name
  dat <- safe_df(dat)

  key_cols <- names(dat)[grepl("__", names(dat), fixed = TRUE)]

  if (is.null(fixed_cols)) {
    fixed_cols <- setdiff(names(dat), key_cols)
  }
  fixed_cols <- fixed_cols[fixed_cols %in% names(dat)]

  # fallback first
  if (length(key_cols) == 0) {
    write_review_sheet(wb, sheet_name, dat, title_text)
    return(invisible(NULL))
  }

  openxlsx::addWorksheet(wb, sheet_name)

  merge_to <- max(2, min(max(1, ncol(dat)), 14))
  openxlsx::writeData(
    wb, sheet_name,
    data.frame(title_text),
    startRow = 1, startCol = 1,
    colNames = FALSE
  )
  if (merge_to > 1) {
    openxlsx::mergeCells(wb, sheet_name, cols = 1:merge_to, rows = 1)
  }

  title_style  <- base_title_style()
  header_style <- base_header_style()
  body_style   <- base_body_style()

  openxlsx::addStyle(
    wb, sheet_name,
    title_style,
    rows = 1, cols = 1,
    gridExpand = TRUE, stack = TRUE
  )

  parts <- lapply(key_cols, extract_twoline_parts)
  groups <- vapply(parts, `[[`, "", "group")
  groups_ord <- order_twoline_groups(groups)

  get_stats_for_group <- function(g) {
    if (is.null(group_stat_map)) return(stat_order)
    stats_g <- group_stat_map[[g]]
    if (is.null(stats_g) || length(stats_g) == 0) return(stat_order)
    as.character(stats_g)
  }

  for (g in groups_ord) {
    for (s in get_stats_for_group(g)) {
      kk <- twoline_key(s, g)
      if (!kk %in% names(dat)) dat[[kk]] <- ""
    }
  }

  ordered_cols <- fixed_cols
  for (g in groups_ord) {
    for (s in get_stats_for_group(g)) {
      ordered_cols <- c(ordered_cols, twoline_key(s, g))
    }
  }
  ordered_cols <- ordered_cols[ordered_cols %in% names(dat)]
  dat <- dat[, ordered_cols, drop = FALSE]

  header_row1 <- 3
  header_row2 <- 4
  data_row    <- 5

  col_ptr <- 1

  for (fc in fixed_cols) {
    openxlsx::writeData(wb, sheet_name, fc, startRow = header_row1, startCol = col_ptr, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = col_ptr, rows = header_row1:header_row2)
    col_ptr <- col_ptr + 1
  }

  for (g in groups_ord) {
    stats_g <- get_stats_for_group(g)
    n_stat <- length(stats_g)

    openxlsx::writeData(wb, sheet_name, g, startRow = header_row1, startCol = col_ptr, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = col_ptr:(col_ptr + n_stat - 1), rows = header_row1)

    for (j in seq_along(stats_g)) {
      openxlsx::writeData(
        wb, sheet_name,
        stats_g[j],
        startRow                     = header_row2,
        startCol                     = col_ptr + j - 1,
        colNames                     = FALSE
      )
    }

    col_ptr <- col_ptr + n_stat
  }

  openxlsx::writeData(
    wb, sheet_name,
    dat,
    startRow                         = data_row,
    startCol                         = 1,
    colNames                         = FALSE,
    withFilter                       = FALSE
  )

  end_col <- ncol(dat)

  openxlsx::addStyle(
    wb, sheet_name,
    header_style,
    rows                             = header_row1:header_row2,
    cols                             = 1:end_col,
    gridExpand                       = TRUE,
    stack                            = TRUE
  )

  if (nrow(dat) > 0) {
    has_note <- grepl("^Note\\.", as.character(dat[[1]][nrow(dat)]))
    note_row_excel <- if (has_note) data_row + nrow(dat) - 1 else NA_integer_

    openxlsx::addStyle(
      wb, sheet_name,
      body_style,
      rows                           = data_row:(data_row + nrow(dat) - 1),
      cols                           = 1:end_col,
      gridExpand                     = TRUE,
      stack                          = TRUE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      openxlsx::createStyle(halign   = "left", valign = "center"),
      rows                           = data_row:(data_row + nrow(dat) - 1),
      cols                           = 1,
      gridExpand                     = TRUE,
      stack                          = TRUE
    )

    if (length(fixed_cols) >= 2) {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(halign = "left", valign = "center"),
        rows                         = data_row:(data_row + nrow(dat) - 1),
        cols                         = 2,
        gridExpand                   = TRUE,
        stack                        = TRUE
      )
    }

    if (end_col > length(fixed_cols)) {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(halign = "center", valign = "center"),
        rows                         = data_row:(data_row + nrow(dat) - 1),
        cols                         = (length(fixed_cols) + 1):end_col,
        gridExpand                   = TRUE,
        stack                        = TRUE
      )
    }

    if (!has_note) {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(border = "bottom", borderStyle = "thick"),
        rows                         = data_row + nrow(dat) - 1,
        cols                         = 1:end_col,
        gridExpand                   = TRUE,
        stack                        = TRUE
      )
    } else {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(border = "top", borderStyle = "thick"),
        rows                         = note_row_excel,
        cols                         = 1:end_col,
        gridExpand                   = TRUE,
        stack                        = TRUE
      )
    }
  }

  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 24)
  openxlsx::setRowHeights(wb, sheet_name, rows = header_row1, heights = 22)
  openxlsx::setRowHeights(wb, sheet_name, rows = header_row2, heights = 20)

  if (end_col > 0) {
    set_safe_colwidths(wb, sheet_name, dat)
  }

  openxlsx::freezePane(
    wb, sheet_name,
    firstActiveRow = data_row,
    firstActiveCol = length(fixed_cols) + 1
  )
}

write_t6_lca_sheet <- function(wb, sheet_name, dat, title_text = NULL) {
  if (is.null(title_text) || !nzchar(title_text)) title_text <- sheet_name
  dat <- safe_df(dat)

  openxlsx::addWorksheet(wb, sheet_name)

  title_style  <- base_title_style()
  header_style <- base_header_style()
  body_style   <- base_body_style()

  # --------------------------------------------------
  # title
  # --------------------------------------------------
  merge_to <- max(2, min(max(1, ncol(dat)), 14))
  openxlsx::writeData(
    wb, sheet_name,
    data.frame(title_text),
    startRow = 1, startCol = 1,
    colNames = FALSE
  )
  if (merge_to > 1) {
    openxlsx::mergeCells(wb, sheet_name, cols = 1:merge_to, rows = 1)
  }
  openxlsx::addStyle(wb, sheet_name, title_style, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)

  # --------------------------------------------------
  # note row ??????????????
  # --------------------------------------------------
  has_note <- nrow(dat) > 0 && grepl("^Note\\.", as.character(dat[[1]][nrow(dat)]))
  note_df  <- if (has_note) dat[nrow(dat), , drop = FALSE] else data.frame()
  body_df  <- if (has_note) dat[-nrow(dat), , drop = FALSE] else dat

  # --------------------------------------------------
  # fixed/data columns
  # --------------------------------------------------
  fixed_cols <- setdiff(names(body_df), names(body_df)[grepl("__", names(body_df), fixed = TRUE)])
  if (!"Variable" %in% fixed_cols && ncol(body_df) > 0) fixed_cols <- names(body_df)[1]

  prof_cols <- grep("^%__(Profile|Class) [0-9]+$", names(body_df), value = TRUE)
  prof_num  <- suppressWarnings(as.integer(gsub("^%__(Profile|Class) ([0-9]+)$", "\\2", prof_cols)))
  prof_cols <- prof_cols[order(prof_num)]
  prof_labs <- gsub("^%__", "", prof_cols)

  overall_cols <- c("p__Overall", "sig__Overall", "post-hoc__Overall")
  overall_cols <- overall_cols[overall_cols %in% names(body_df)]

  ordered_cols <- c(fixed_cols, prof_cols, overall_cols)
  ordered_cols <- ordered_cols[ordered_cols %in% names(body_df)]
  body_df <- body_df[, ordered_cols, drop = FALSE]

  header_row1 <- 3
  header_row2 <- 4
  data_row    <- 5

  col_ptr <- 1

  # --------------------------------------------------
  # fixed columns
  # --------------------------------------------------
  for (fc in fixed_cols) {
    openxlsx::writeData(wb, sheet_name, fc, startRow = header_row1, startCol = col_ptr, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = col_ptr, rows = header_row1:header_row2)
    col_ptr <- col_ptr + 1
  }

  # --------------------------------------------------
  # profile blocks: each only "%"
  # --------------------------------------------------
  for (pp in seq_along(prof_cols)) {
    openxlsx::writeData(wb, sheet_name, prof_labs[pp], startRow = header_row1, startCol = col_ptr, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = col_ptr, rows = header_row1)
    openxlsx::writeData(wb, sheet_name, "%", startRow = header_row2, startCol = col_ptr, colNames = FALSE)
    col_ptr <- col_ptr + 1
  }

  # --------------------------------------------------
  # overall block: p, sig, post-hoc
  # --------------------------------------------------
  if (length(overall_cols) > 0) {
    openxlsx::writeData(wb, sheet_name, "Overall", startRow = header_row1, startCol = col_ptr, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet_name, cols = col_ptr:(col_ptr + length(overall_cols) - 1), rows = header_row1)

    overall_labels <- c(
      "p__Overall"        = "p",
      "sig__Overall"      = "sig",
      "post-hoc__Overall" = "post-hoc"
    )

    for (j in seq_along(overall_cols)) {
      openxlsx::writeData(
        wb, sheet_name,
        overall_labels[[overall_cols[j]]],
        startRow = header_row2,
        startCol = col_ptr + j - 1,
        colNames = FALSE
      )
    }

    col_ptr <- col_ptr + length(overall_cols)
  }

  # --------------------------------------------------
  # body write
  # --------------------------------------------------
  openxlsx::writeData(
    wb, sheet_name,
    body_df,
    startRow = data_row,
    startCol = 1,
    colNames = FALSE,
    withFilter = FALSE
  )

  end_col <- ncol(body_df)

  openxlsx::addStyle(
    wb, sheet_name,
    header_style,
    rows = header_row1:header_row2,
    cols = 1:end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  if (nrow(body_df) > 0) {
    openxlsx::addStyle(
      wb, sheet_name,
      body_style,
      rows = data_row:(data_row + nrow(body_df) - 1),
      cols = 1:end_col,
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      openxlsx::createStyle(halign = "left", valign = "center"),
      rows = data_row:(data_row + nrow(body_df) - 1),
      cols = 1,
      gridExpand = TRUE,
      stack = TRUE
    )

    if (end_col >= 2) {
      openxlsx::addStyle(
        wb, sheet_name,
        openxlsx::createStyle(halign = "center", valign = "center"),
        rows = data_row:(data_row + nrow(body_df) - 1),
        cols = 2:end_col,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  # --------------------------------------------------
  # note
  # --------------------------------------------------
  if (has_note) {
    note_row_excel <- data_row + nrow(body_df)
    openxlsx::writeData(
      wb, sheet_name,
      note_df,
      startRow = note_row_excel,
      startCol = 1,
      colNames = FALSE,
      withFilter = FALSE
    )

    openxlsx::addStyle(
      wb, sheet_name,
      openxlsx::createStyle(border = "top", borderStyle = "thick"),
      rows = note_row_excel,
      cols = 1:end_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  } else if (nrow(body_df) > 0) {
    openxlsx::addStyle(
      wb, sheet_name,
      openxlsx::createStyle(border = "bottom", borderStyle = "thick"),
      rows = data_row + nrow(body_df) - 1,
      cols = 1:end_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 24)
  openxlsx::setRowHeights(wb, sheet_name, rows = header_row1, heights = 22)
  openxlsx::setRowHeights(wb, sheet_name, rows = header_row2, heights = 20)

  if (end_col > 0) {
    set_safe_colwidths(wb, sheet_name, body_df)
  }

  openxlsx::freezePane(wb, sheet_name, firstActiveRow = data_row, firstActiveCol = 2)
}


# ------------------------------------------------------------
# 13. validation helpers
# ------------------------------------------------------------
has_any_prefix <- function(nm, prefix) {
  any(startsWith(as.character(nm), prefix))
}

validate_table_structure <- function(table_name, df, meta_i = NULL) {
  df <- safe_df(df)

  out <- data.frame(
    table_name       = table_name,
    valid            = TRUE,
    issue            = "",
    stringsAsFactors = FALSE
  )

  type_i <- meta_i$type %||% "normal"

  if (nrow(df) == 0 && ncol(df) == 0) {
    if (table_name %in% c("T5c", "T6C")) {
      return(out)
    }
    out$valid <- FALSE
    out$issue <- "empty table"
    return(out)
  }

  nm <- names(df)

  if (grepl("^[AS][0-9]", table_name) && "Note" %in% nm) {
    return(out)
  }

  if (type_i == "twoline_mean") {
    if (!has_any_prefix(nm, "M__") || !has_any_prefix(nm, "SD__")) {
      out$valid <- FALSE
      out$issue <- "twoline_mean requires M__ and SD__ columns"
      return(out)
    }
  }

  if (type_i == "twoline_mse") {
    if (!has_any_prefix(nm, "M__") || !has_any_prefix(nm, "SE__")) {
      out$valid <- FALSE
      out$issue <- "twoline_mse requires M__ and SE__ columns"
      return(out)
    }
  }

  if (type_i == "twoline_mse_psig") {
    need        <- c("M__", "SE__", "p__", "sig__")
    miss        <- need[!vapply(need, function(pf) has_any_prefix(nm, pf), logical(1))]
    if (length(miss) > 0) {
      out$valid <- FALSE
      out$issue <- paste0("twoline_mse_psig missing prefixes: ", paste(miss, collapse = ", "))
      return(out)
    }
  }

  if (type_i == "twoline_t6b") {
    need <- c("M__", if (isTRUE(HAS_WEIGHT)) "SE__" else "SD__", "Statistic__", "p__", "sig__", "post-hoc__")
    miss <- need[!vapply(need, function(pf) has_any_prefix(nm, pf), logical(1))]
    if (length(miss) > 0) {
      out$valid <- FALSE
      out$issue <- paste0("twoline_t6b missing prefixes: ", paste(miss, collapse = ", "))
      return(out)
    }
  }

  if (type_i == "twoline_npct") {
    if (!has_any_prefix(nm, "n__") || !has_any_prefix(nm, "%__")) {
      out$valid <- FALSE
      out$issue <- "twoline_npct requires n__ and %__ columns"
      return(out)
    }
  }

  if (type_i == "twoline_rrr") {
    if (table_name %in% c("T5c") && nrow(df) == 0) {
      return(out)
    }

    need        <- c("RRR__", "LLCI__", "ULCI__", "p__", "sig__")
    miss        <- need[!vapply(need, function(pf) has_any_prefix(nm, pf), logical(1))]
    if (length(miss) > 0) {
      out$valid <- FALSE
      out$issue <- paste0("twoline_rrr missing prefixes: ", paste(miss, collapse = ", "))
      return(out)
    }
  }

  if (type_i == "twoline_mixed") {
    need <- c("M/n__", if (isTRUE(HAS_WEIGHT)) "SE/%__" else "SD/%__")
    miss <- need[!vapply(need, function(pf) has_any_prefix(nm, pf), logical(1))]
    if (length(miss) > 0) {
      out$valid <- FALSE
      out$issue <- paste0("twoline_mixed missing prefixes: ", paste(miss, collapse = ", "))
      return(out)
    }
  }

  if (type_i == "twoline_s5") {
    need <- c("RRR__", "(LLCI~ULCI)__", "p__")
    miss <- need[!vapply(need, function(pf) has_any_prefix(nm, pf), logical(1))]
    if (length(miss) > 0) {
      out$valid <- FALSE
      out$issue <- paste0("twoline_s5 missing prefixes: ", paste(miss, collapse = ", "))
      return(out)
    }
  }

  if (type_i == "normal") {
    # exclude overview/wide tables from strict compact warning
    if (table_name %in% c("A3", "A4", "A5", "A6", "A8", "S1")) {
      return(out)
    }

    df2         <- ensure_compact_as_table(df, table_name = table_name)
    nm2         <- names(df2)

    compact_ok  <- any(nm2 %in% c(compact_mean_cols(), "n (%)", "RRR (LLCI~ULCI)"))

    if (!compact_ok && table_name %in% c("S5", "S6", "A5", "A6", "S2", "S3", "S4")) {
      if (table_name == "S6") {
        return(out)
      }
      out$valid <- FALSE
      out$issue <- paste0(
        "A/S table expected compact columns such as '",
        compact_mean_name(FALSE),
        "', '",
        compact_mean_name(TRUE),
        "', 'n (%)', or 'RRR (LLCI~ULCI)'"
      )
      return(out)
    }
  }

  out
}


validate_table_registry <- function(table_registry, table_meta) {
  if (!is.list(table_registry) || length(table_registry) == 0) return(data.frame())

  res           <- lapply(names(table_registry), function(nm) {
    meta_i      <- table_meta[[nm]] %||% list(type = "normal")
    validate_table_structure(nm, table_registry[[nm]], meta_i = meta_i)
  })

  out           <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

# Override compact-column handling late so A3/A4 can keep estimated M-SE
# labels without depending on earlier mojibake-prone name matching.
# Final compact-column override used in workbook export.
ensure_compact_as_table <- function(df, table_name = NULL) {
  df <- safe_df(df)
  if (nrow(df) == 0) return(df)

  nm <- names(df)
  mean_cols <- compact_mean_cols()

  if (any(nm %in% c(mean_cols, "n (%)", "RRR (LLCI~ULCI)"))) {
    if ("p" %in% names(df)) {
      df$p <- as.character(df$p)
      df$p[is.na(df$p)] <- ""
    }
    if ("sig" %in% names(df)) {
      df$sig <- as.character(df$sig)
      df$sig[is.na(df$sig)] <- ""
    }
    return(df)
  }

  compact_mean_label <- compact_mean_name(isTRUE(HAS_WEIGHT))

  prof_cols <- grep("^Profile [0-9]+$", nm, value = TRUE)
  if (!is.null(table_name) && table_name %in% c("A3", "A4") && length(prof_cols) > 0 &&
      !("Category" %in% names(df))) {
    df[[compact_mean_label]] <- as.character(df[[prof_cols[1]]])
  }

  if (!is.null(table_name) && table_name %in% c("S1") && "Value" %in% names(df) &&
      !any(names(df) %in% mean_cols)) {
    df[[compact_mean_label]] <- as.character(df$Value)
  }

  if (!any(names(df) %in% c(mean_cols, "n (%)", "RRR (LLCI~ULCI)"))) {
    if (all(c("M", "SE") %in% names(df))) {
      df[[compact_mean_name(TRUE)]] <- fmt_mean_sd_cell(df$M, df$SE)
    } else if (all(c("M", "SD") %in% names(df))) {
      df[[compact_mean_name(FALSE)]] <- fmt_mean_sd_cell(df$M, df$SD)
    } else if (all(c("n", "pct") %in% names(df))) {
      df[["n (%)"]] <- fmt_n_pct_cell(df$n, df$pct)
    } else if (all(c("n", "%") %in% names(df))) {
      df[["n (%)"]] <- fmt_n_pct_cell(df$n, df[["%"]])
    } else if (all(c("RRR", "LLCI", "ULCI") %in% names(df))) {
      df[["RRR (LLCI~ULCI)"]] <- fmt_rrr_ci_cell(df$RRR, df$LLCI, df$ULCI)
    }
  }

  if (!any(names(df) %in% c(mean_cols, "n (%)", "RRR (LLCI~ULCI)"))) {
    rrr_like <- setdiff(
      names(df)[grepl("^p__", names(df)) == FALSE & grepl("^sig__", names(df)) == FALSE],
      c("Variable", "Category")
    )
    if (!is.null(table_name) && table_name %in% c("S5", "S6") && length(rrr_like) > 0) {
      df[["RRR (LLCI~ULCI)"]] <- as.character(df[[rrr_like[1]]])
    }
  }

  if ("p" %in% names(df)) {
    df$p <- as.character(df$p)
    df$p[is.na(df$p)] <- ""
  }
  if ("sig" %in% names(df)) {
    df$sig <- as.character(df$sig)
    df$sig[is.na(df$sig)] <- ""
  }

  df
}

# ------------------------------------------------------------
# 14. build all tables
# ------------------------------------------------------------
log_info("REFERENCE_CLASS ... ", REFERENCE_CLASS)

log_info("Building T0  ...");  T0  <- postprocess_table_output(TABLE_BUILDERS$T0(),  "T0")
log_info("Building T1  ...");  T1  <- postprocess_table_output(TABLE_BUILDERS$T1(),  "T1")
log_info("Building T2  ...");  T2  <- postprocess_table_output(TABLE_BUILDERS$T2(),  "T2")
log_info("Building T3  ...");  T3  <- postprocess_table_output(TABLE_BUILDERS$T3(),  "T3")
log_info("Building T4  ...");  T4  <- postprocess_table_output(TABLE_BUILDERS$T4(),  "T4")
log_info("Building T5  ...");  T5  <- postprocess_table_output(TABLE_BUILDERS$T5(),  "T5")
log_info("Building T5b ..."); T5b  <- postprocess_table_output(TABLE_BUILDERS$T5b(), "T5b")
log_info("Building T5c ..."); T5c  <- postprocess_table_output(TABLE_BUILDERS$T5c(), "T5c")
log_info("Building T5d ..."); T5d  <- postprocess_table_output(TABLE_BUILDERS$T5d(), "T5d")
log_info("Building T6  ...");  T6  <- postprocess_table_output(TABLE_BUILDERS$T6(),  "T6")
log_info("Building T6b ..."); T6b  <- postprocess_table_output(TABLE_BUILDERS$T6b(), "T6b")
log_info("Building T6C ..."); T6C  <- postprocess_table_output(TABLE_BUILDERS$T6C(), "T6C")
log_info("Building T6D ..."); T6D  <- postprocess_table_output(TABLE_BUILDERS$T6D(), "T6D")
log_info("Building T6E ..."); T6E  <- postprocess_table_output(TABLE_BUILDERS$T6E(), "T6E")
log_info("Building T7  ...");  T7  <- postprocess_table_output(TABLE_BUILDERS$T7(),  "T7")
log_info("Building A3/A4/A5/A6/A8 ...")
A3 <- postprocess_table_output(TABLE_BUILDERS$A3(), "A3")
A4 <- postprocess_table_output(TABLE_BUILDERS$A4(), "A4")
A5 <- postprocess_table_output(TABLE_BUILDERS$A5(), "A5")
A6 <- postprocess_table_output(TABLE_BUILDERS$A6(), "A6")
A8 <- postprocess_table_output(TABLE_BUILDERS$A8(), "A8")
if (is.data.frame(A3) && nrow(A3) > 0) {
  names(A3) <- gsub("M.+(SE|SD)$", compact_mean_name(isTRUE(HAS_WEIGHT)), names(A3))
}
if (is.data.frame(A4) && nrow(A4) > 0) {
  names(A4) <- gsub("M.+(SE|SD)$", compact_mean_name(isTRUE(HAS_WEIGHT)), names(A4))
}
log_info("Building S1/S2/S3/S4/S5/S6 ...")
S1 <- postprocess_table_output(TABLE_BUILDERS$S1(), "S1")
S2 <- postprocess_table_output(TABLE_BUILDERS$S2(), "S2")
S3 <- postprocess_table_output(TABLE_BUILDERS$S3(), "S3")
S4 <- postprocess_table_output(TABLE_BUILDERS$S4(), "S4")
S5 <- postprocess_table_output(TABLE_BUILDERS$S5(), "S5")
S6 <- postprocess_table_output(TABLE_BUILDERS$S6(), "S6")
if (is.data.frame(S6) && "RRR (LLCI~ULCI)" %in% names(S6)) {
  S6 <- S6[, names(S6) != "RRR (LLCI~ULCI)", drop = FALSE]
}
if (is.data.frame(S1) && all(c("Metric", "Value") %in% names(S1))) {
  S1 <- S1[, c("Metric", "Value"), drop = FALSE]
}

TABLE_REGISTRY <- list(
  T0 = T0, T1 = T1, T2 = T2, T3 = T3, T4 = T4,
  T5 = T5, T5b = T5b, T5c = T5c, T5d = T5d,
  T6 = T6, T6b = T6b, T6C = T6C, T6D = T6D, T6E = T6E, T7 = T7,
  A3 = A3, A4 = A4, A5 = A5, A6 = A6, A8 = A8,
  S1 = S1, S2 = S2, S3 = S3, S4 = S4, S5 = S5, S6 = S6
)

# ------------------------------------------------------------
# validate table structures
# ------------------------------------------------------------
TABLE_VALIDATION <- validate_table_registry(TABLE_REGISTRY, TABLE_META)

if (is.data.frame(TABLE_VALIDATION) && nrow(TABLE_VALIDATION) > 0) {
  bad_tbl <- TABLE_VALIDATION[!TABLE_VALIDATION$valid, , drop = FALSE]

  if (nrow(bad_tbl) > 0) {
    for (i in seq_len(nrow(bad_tbl))) {
      log_warn("[TABLE_VALIDATION] ", bad_tbl$table_name[i], " / ", bad_tbl$issue[i])
    }
  } else {
    log_info("TABLE_VALIDATION: all tables passed structure checks.")
  }
}

TABLE_SUMMARY <- list(
  mixture_mode     = mixture_mode,
  best_k           = best_k,
  model_structure  = model_structure,
  best_tag         = best_tag,
  reference_class  = REFERENCE_CLASS,
  n_tables         = length(TABLE_REGISTRY),
  excel_file       = PATH_FINAL_EXCEL,
  created_at       = Sys.time()
)

TABLE_INDEX <- data.frame(
  table_name       = names(TABLE_REGISTRY),
  nrow             = vapply(TABLE_REGISTRY, nrow, integer(1)),
  ncol             = vapply(TABLE_REGISTRY, ncol, integer(1)),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 15. save csv files
# ------------------------------------------------------------
dir.create(DIR_TABLES, recursive = TRUE, showWarnings = FALSE)
if (!"T6B" %in% names(TABLE_REGISTRY)) {
  unlink(file.path(DIR_TABLES, "T6B.csv"))
  unlink(file.path(DIR_RDS, c("T6B.rds", "TABLE_T6B.rds")))
}

for (nm in names(TABLE_REGISTRY)) {
  write_csv_safe(TABLE_REGISTRY[[nm]], file.path(DIR_TABLES, paste0(nm, ".csv")))
}

write_csv_safe(TABLE_INDEX, file.path(DIR_TABLES, "TABLE_INDEX.csv"))
write_csv_safe(TABLE_VALIDATION, file.path(DIR_TABLES, "TABLE_VALIDATION.csv"))

TABLE_MANIFEST <- data.frame(
  table_name = names(TABLE_REGISTRY),
  file_path = file.path(DIR_TABLES, paste0(names(TABLE_REGISTRY), ".csv")),
  stringsAsFactors = FALSE
)

write_csv_safe(TABLE_MANIFEST, file.path(DIR_TABLES, "TABLE_MANIFEST.csv"))

# ------------------------------------------------------------
# 16. export registry with notes
# ------------------------------------------------------------
TABLE_REGISTRY_FOR_EXPORT <- TABLE_REGISTRY

for (nm in names(TABLE_REGISTRY_FOR_EXPORT)) {
  dat_i  <- safe_df(TABLE_REGISTRY_FOR_EXPORT[[nm]])
  meta_i <- TABLE_META[[nm]]

  note_type_i <- meta_i$note_type %||% "generic"
  note_text_i <- make_table_note(
    type = note_type_i,
    reference_class = REFERENCE_CLASS
  )

  TABLE_REGISTRY_FOR_EXPORT[[nm]] <- append_note_row(dat_i, note_text_i)
}

for (nm in c("A3", "A4")) {
  dat_i <- safe_df(TABLE_REGISTRY_FOR_EXPORT[[nm]])
  if (!is.data.frame(dat_i) || nrow(dat_i) == 0) next
  names(dat_i) <- gsub("M.+(SE|SD)$", compact_mean_name(isTRUE(HAS_WEIGHT)), names(dat_i))
  TABLE_REGISTRY_FOR_EXPORT[[nm]] <- dat_i
}

# ------------------------------------------------------------
# 17. save workbook
# ------------------------------------------------------------
wb <- openxlsx::createWorkbook()

for (nm in names(TABLE_REGISTRY_FOR_EXPORT)) {
  dat_i   <- safe_df(TABLE_REGISTRY_FOR_EXPORT[[nm]])
  meta_i  <- TABLE_META[[nm]]
  type_i  <- meta_i$type %||% "normal"
  title_i <- meta_i$caption %||% meta_i$title %||% nm

  if (identical(type_i, "twoline_t6_lca")) {

    write_t6_lca_sheet(
      wb         = wb,
      sheet_name = substr(nm, 1, 31),
      dat        = dat_i,
      title_text = title_i
    )

    } else if (type_i %in% c("twoline_mean", "twoline_mse", "twoline_mse_psig", "twoline_npct", "twoline_rrr", "twoline_t6b", "twoline_s5", "twoline_mixed")) {

    stat_order_i <- switch(
      type_i,
      twoline_mean     = c("M", "SD"),
        twoline_mse      = c("M", "SE"),
        twoline_mse_psig = c("M", "SE", "p", "sig"),
        twoline_npct     = c("n", "%"),
        twoline_rrr      = c("RRR", "LLCI", "ULCI", "p", "sig"),
        twoline_t6b      = c("M", if (isTRUE(HAS_WEIGHT)) "SE" else "SD", "Statistic", "p", "sig", "post-hoc"),
        twoline_s5       = c("RRR", "(LLCI~ULCI)", "p"),
        twoline_mixed    = c("M/n", if (isTRUE(HAS_WEIGHT)) "SE/%" else "SD/%")
      )

    group_stat_map_i <- NULL
    fixed_cols_i <- NULL
    if (identical(nm, "T6")) {
      t6_key_cols <- names(dat_i)[grepl("__", names(dat_i), fixed = TRUE)]
      t6_parts <- lapply(t6_key_cols, extract_twoline_parts)
      t6_groups <- unique(vapply(t6_parts, `[[`, "", "group"))
      t6_profile_groups <- t6_groups[grepl("^(Profile|Class)\\s+[0-9]+$", t6_groups)]
      group_stat_map_i <- setNames(vector("list", length(t6_groups)), t6_groups)
      for (gg in t6_profile_groups) group_stat_map_i[[gg]] <- c("M", if (isTRUE(HAS_WEIGHT)) "SE" else "SD")
      if ("Overall" %in% t6_groups) group_stat_map_i[["Overall"]] <- c("Statistic", "p", "sig", "post-hoc")
    } else if (identical(nm, "T6E")) {
      t6b_key_cols <- names(dat_i)[grepl("__", names(dat_i), fixed = TRUE)]
      t6b_parts <- lapply(t6b_key_cols, extract_twoline_parts)
      t6b_groups <- unique(vapply(t6b_parts, `[[`, "", "group"))
      group_stat_map_i <- setNames(vector("list", length(t6b_groups)), t6b_groups)
      for (gg in t6b_groups) group_stat_map_i[[gg]] <- c("M", if (isTRUE(HAS_WEIGHT)) "SE" else "SD", "Statistic", "p", "sig", "post-hoc")
      fixed_cols_i <- latent_group_term()
    } else if (identical(nm, "T6D")) {
      t6d_key_cols <- names(dat_i)[grepl("__", names(dat_i), fixed = TRUE)]
      t6d_parts <- lapply(t6d_key_cols, extract_twoline_parts)
      t6d_groups <- unique(vapply(t6d_parts, `[[`, "", "group"))
      group_stat_map_i <- setNames(vector("list", length(t6d_groups)), t6d_groups)
      for (gg in t6d_groups) group_stat_map_i[[gg]] <- c("M", if (isTRUE(HAS_WEIGHT)) "SE" else "SD")
      fixed_cols_i <- latent_group_term()
    } else if (identical(nm, "S5")) {
      s5_key_cols <- names(dat_i)[grepl("__", names(dat_i), fixed = TRUE)]
      s5_parts <- lapply(s5_key_cols, extract_twoline_parts)
      s5_groups <- unique(vapply(s5_parts, `[[`, "", "group"))
      group_stat_map_i <- setNames(vector("list", length(s5_groups)), s5_groups)
      for (gg in s5_groups) group_stat_map_i[[gg]] <- c("RRR", "(LLCI~ULCI)", "p")
      fixed_cols_i <- c("Variable", "Category")
    }

    write_twoline_stat_sheet(
      wb         = wb,
      sheet_name = substr(nm, 1, 31),
      dat        = dat_i,
      title_text = title_i,
      stat_order = stat_order_i,
      group_stat_map = group_stat_map_i,
      fixed_cols = fixed_cols_i
    )

    if (identical(nm, "T5d") && is.data.frame(dat_i) && "Comparison" %in% names(dat_i)) {
      cmp_rows <- which(nzchar(trimws(as.character(dat_i$Comparison))))
      cmp_rows <- cmp_rows[cmp_rows > 1]
      if (length(cmp_rows) > 0) {
        sep_style <- openxlsx::createStyle(border = "top", borderColour = "black", borderStyle = "medium")
        openxlsx::addStyle(
          wb,
          substr(nm, 1, 31),
          sep_style,
          rows = 4 + cmp_rows,
          cols = seq_len(ncol(dat_i)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

  } else {

    write_review_sheet(
      wb         = wb,
      sheet_name = substr(nm, 1, 31),
      dat        = dat_i,
      title_text = title_i
    )
  }
}

dir.create(dirname(PATH_FINAL_EXCEL), recursive = TRUE, showWarnings = FALSE)
openxlsx::saveWorkbook(wb, PATH_FINAL_EXCEL, overwrite = TRUE)

save_step_rds(T3_indicator_profile, "T3_indicator_profile", dir_rds = DIR_RDS)
save_step_rds(CLASSIFICATION_QUALITY, "CLASSIFICATION_QUALITY", dir_rds = DIR_RDS)
save_step_rds(POSTERIOR_MAX_DF, "POSTERIOR_MAX_DF", dir_rds = DIR_RDS)

# ------------------------------------------------------------
# 18. save rds
# ------------------------------------------------------------
save_step_rds(TABLE_SUMMARY, "TABLE_SUMMARY", dir_rds = DIR_RDS)

save_named_rds_list(
  list(
    T0 = T0, T1 = T1, T2  = T2,   T3  = T3,   T4  = T4,
    T5 = T5, T5b = T5b,  T5c = T5c,  T5d = T5d,
    T6 = T6, T6b = T6b,  T6C = T6C,  T6D = T6D, T6E = T6E, T7  = T7,
    A3 = A3, A4  = A4,   A5  = A5,   A6  = A6, A8 = A8,
    S1 = S1, S2  = S2,   S3  = S3,   S4  = S4,
    S5 = S5, S6  = S6,
    TABLE_REGISTRY            = TABLE_REGISTRY,
    TABLE_REGISTRY_FOR_EXPORT = TABLE_REGISTRY_FOR_EXPORT,
    TABLE_INDEX               = TABLE_INDEX,
    TABLE_MANIFEST            = TABLE_MANIFEST,
    TABLE_SUMMARY             = TABLE_SUMMARY,
    TABLE_VALIDATION          = TABLE_VALIDATION
  ),
  dir_rds = DIR_RDS
)

# ------------------------------------------------------------
# 19. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_TABLES, units = "secs")), 2)

log_info("05_tables.R completed.")
log_info("mixture_mode    = ", mixture_mode)
log_info("best_k          = ", best_k)
log_info("model_structure = ", model_structure)
log_info("best_tag        = ", best_tag)
log_info("reference_class = ", REFERENCE_CLASS)
log_info("n(T1)           = ", nrow(T1))
log_info("n(T2)           = ", nrow(T2))
log_info("n(T3)           = ", nrow(T3))
log_info("n(T4)           = ", nrow(T4))
log_info("n(T5)           = ", nrow(T5))
log_info("n(T5b)          = ", nrow(T5b))
log_info("n(T5c)          = ", nrow(T5c))
log_info("n(T5d)          = ", nrow(T5d))
log_info("n(T6)           = ", nrow(T6))
log_info("n(T7)           = ", nrow(T7))
log_info("Excel file      = ", PATH_FINAL_EXCEL)
log_info("elapsed         = ", elapsed_sec, " sec")

log_step_end("tables", elapsed_sec, ok = TRUE)
