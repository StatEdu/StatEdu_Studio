# ============================================================
# 04b_classify.R
# - Read best-solution savedata / cprob
# - Recover class assignment robustly for LCA/LPA
# - Prefer ID merge when valid
# - Fallback to row-order merge when SAVEDATA ID is duplicated/invalid
# - Save classified analysis/display data and class summaries
# ============================================================

T0_CLASSIFY <- Sys.time()

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

.safe_msg <- function(level = "INFO", ...) {
  msg <- paste0(...)
  if (exists("log_info", mode = "function") && identical(level, "INFO")) {
    log_info(msg)
  } else if (exists("log_warn", mode = "function") && identical(level, "WARN")) {
    log_warn(msg)
  } else if (exists("log_error", mode = "function") && identical(level, "ERROR")) {
    log_error(msg)
  } else {
    cat(sprintf("[%s] [%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg))
  }
}

.safe_stop <- function(...) stop(paste0(...), call. = FALSE)

.safe_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
}

.safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
.safe_int <- function(x) suppressWarnings(as.integer(round(.safe_num(x))))

.is_blank_scalar <- function(x) {
  length(x) == 0 || is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)[1]))
}

.first_nonempty <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(as.character(x)))]
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

.norm_chr <- function(x) trimws(as.character(x))

.save_rds_local <- function(object, name, dir_rds = NULL) {
  if (exists("save_step_rds", mode = "function")) {
    save_step_rds(object, name, dir_rds = dir_rds %||% get0("DIR_RDS", ifnotfound = NULL))
  } else {
    out_dir <- dir_rds %||% get0("DIR_RDS", ifnotfound = getwd())
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(object, file.path(out_dir, paste0(name, ".rds")))
  }
}

.load_rds_local <- function(name, default = NULL, dir_rds = NULL) {
  if (exists("load_step_rds", mode = "function")) {
    return(load_step_rds(name, dir_rds = dir_rds %||% get0("DIR_RDS", ifnotfound = NULL), default = default))
  }
  in_dir <- dir_rds %||% get0("DIR_RDS", ifnotfound = getwd())
  f <- file.path(in_dir, paste0(name, ".rds"))
  if (!file.exists(f)) return(default)
  readRDS(f)
}

.pick_col <- function(nms, patterns = character(0), exclude = character(0)) {
  if (length(nms) == 0) return(NA_character_)
  keep <- setdiff(nms, exclude)
  if (length(keep) == 0) return(NA_character_)
  for (pt in patterns) {
    hit <- keep[grepl(pt, keep, ignore.case = TRUE, perl = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

.is_unique_id <- function(x, min_unique_prop = 0.98) {
  if (length(x) == 0) return(FALSE)
  x <- .norm_chr(x)
  bad <- is.na(x) | !nzchar(x)
  x2 <- x[!bad]
  if (length(x2) == 0) return(FALSE)
  prop_unique <- length(unique(x2)) / length(x2)
  no_dup <- !anyDuplicated(x2)
  isTRUE(no_dup) || isTRUE(prop_unique >= min_unique_prop)
}

.find_id_candidates <- function(df, id_var = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) return(character(0))
  nms <- names(df)

  cand <- c(
    id_var,
    "id", "ID", "Id", "pid", "PID", "caseid", "CASEID",
    "subject_id", "subjectid", "respondent_id", "RESPID"
  )
  cand <- unique(cand[!is.na(cand) & nzchar(cand)])
  cand <- cand[cand %in% nms]
  cand
}

.detect_savedata_structure <- function(savedata, best_k) {
  nms <- names(savedata)

  # 1) ID 후보
  id_col <- NA_character_
  id_cands <- .find_id_candidates(savedata, id_var = NULL)
  if (length(id_cands) > 0) {
    for (cc in id_cands) {
      if (.is_unique_id(savedata[[cc]])) {
        id_col <- cc
        break
      }
    }
  }

  # Mplus SAVE = CPROBABILITIES without headers is usually:
  # original USEVARIABLES, posterior probabilities for C#1..C#K,
  # most-likely class, and sometimes an ID column. Generic V* names
  # make the early 0/1 indicator columns look like posterior columns,
  # so resolve the rightmost valid class column first.
  generic_names <- length(nms) > 0 && all(grepl("^V[0-9]+$", nms))
  cprob_posterior_cols <- character(0)
  if (isTRUE(generic_names) && ncol(savedata) >= best_k + 1L) {
    numeric_ok <- vapply(savedata, function(z) {
      x <- suppressWarnings(as.numeric(as.character(z)))
      mean(!is.na(x)) > 0.8
    }, logical(1))

    class_score <- rep(NA_real_, length(nms))
    for (i in seq_along(nms)) {
      if (!isTRUE(numeric_ok[i])) next
      x <- .safe_num(savedata[[nms[i]]])
      fin <- is.finite(x)
      if (sum(fin) == 0) next
      is_int <- abs(x[fin] - round(x[fin])) < 1e-8
      in_class <- x[fin] %in% seq_len(best_k)
      class_score[i] <- mean(is_int & in_class)
    }

    class_idx <- which(!is.na(class_score) & class_score > 0.8)
    if (length(class_idx) > 0) {
      class_idx <- max(class_idx)
      cand_idx <- seq.int(class_idx - best_k, class_idx - 1L)
      if (min(cand_idx) >= 1L && all(numeric_ok[cand_idx])) {
        post_ok <- vapply(cand_idx, function(i) {
          x <- .safe_num(savedata[[nms[i]]])
          fin <- is.finite(x)
          sum(fin) > 0 && mean(x[fin] >= -1e-8 & x[fin] <= 1 + 1e-8) > 0.8
        }, logical(1))
        if (all(post_ok)) {
          class_col <- nms[class_idx]
          cprob_posterior_cols <- nms[cand_idx]
        }
      }
    }
  }

  # 2) class 열 후보
  if (!exists("class_col", inherits = FALSE)) class_col <- NA_character_
  class_exact <- c("class", "Class", "CLASS", "c", "C", "most_likely_class", "MostLikelyClass")
  hit <- intersect(class_exact, nms)
  if (length(hit) > 0) {
    for (cc in hit) {
      x <- .safe_int(savedata[[cc]])
      ok <- mean(!is.na(x) & x %in% seq_len(best_k)) > 0.8
      if (isTRUE(ok)) {
        class_col <- cc
        break
      }
    }
  }

  # 3) posterior 열 후보
  posterior_cols <- character(0)
  if (length(cprob_posterior_cols) == best_k) {
    posterior_cols <- cprob_posterior_cols
  }

  # 이름 기반
  nm_hits <- nms[grepl("CPROB|POST|PROB|^P[#_]?C?\\d+$|^CPROB\\d+$", nms, ignore.case = TRUE, perl = TRUE)]
  if (length(posterior_cols) < best_k && length(nm_hits) >= best_k) {
    posterior_cols <- nm_hits[seq_len(best_k)]
  }

  # 이름 기반이 없으면 numeric 열에서 0~1 범위로 best_k개 찾기
  if (length(posterior_cols) < best_k) {
    score <- rep(NA_real_, length(nms))
    for (i in seq_along(nms)) {
      x <- .safe_num(savedata[[nms[i]]])
      fin <- is.finite(x)
      if (sum(fin) == 0) next
      in01 <- mean(x[fin] >= -1e-8 & x[fin] <= 1 + 1e-8)
      score[i] <- in01
    }

    ord <- order(score, decreasing = TRUE, na.last = NA)
    cand <- nms[ord]
    cand <- setdiff(cand, c(id_col, class_col))
    cand <- cand[seq_len(min(length(cand), best_k))]
    if (length(cand) == best_k) posterior_cols <- cand
  }

  list(
    id_col = id_col,
    class_col = class_col,
    posterior_cols = posterior_cols
  )
}

.reconstruct_class_from_posterior <- function(savedata, posterior_cols, best_k) {
  if (length(posterior_cols) < best_k) {
    .safe_stop("Not enough posterior columns to reconstruct class.")
  }

  post <- savedata[, posterior_cols[seq_len(best_k)], drop = FALSE]
  post[] <- lapply(post, .safe_num)
  pm <- as.matrix(post)

  if (ncol(pm) != best_k) .safe_stop("Posterior matrix column count != BEST_K.")
  if (nrow(pm) == 0) .safe_stop("Posterior matrix has 0 rows.")

  class_num <- max.col(pm, ties.method = "first")
  max_prob  <- apply(pm, 1, function(z) {
    z <- suppressWarnings(as.numeric(z))
    if (all(is.na(z))) return(NA_real_)
    max(z, na.rm = TRUE)
  })

  out <- data.frame(
    class_num = as.integer(class_num),
    max_posterior = as.numeric(max_prob),
    stringsAsFactors = FALSE
  )

  for (j in seq_len(best_k)) {
    out[[paste0("post_class", j)]] <- pm[, j]
  }

  out
}

.find_analysis_id <- function(analysis_df, cfg = NULL, survey_bundle = NULL) {
  nms <- names(analysis_df)
  cand <- c(
    survey_bundle$id_var %||% NULL,
    cfg$id_var %||% NULL,
    "PID", "pid", "ID", "id"
  )
  cand <- unique(cand[!is.na(cand) & nzchar(cand)])
  cand <- cand[cand %in% nms]
  if (length(cand) == 0) return(NA_character_)

  for (cc in cand) {
    if (.is_unique_id(analysis_df[[cc]])) return(cc)
  }
  cand[1]
}

.read_savedata_with_retry <- function(path, max_tries = 8L, sleep_sec = 1) {
  last_err <- NULL
  for (i in seq_len(max_tries)) {
    finfo <- tryCatch(file.info(path), error = function(e) NULL)
    if (!is.null(finfo) && nrow(finfo) == 1 && is.finite(finfo$size) && finfo$size > 0) {
      obj <- tryCatch({
        if (exists("read_mplus_savedata_free", mode = "function")) {
          read_mplus_savedata_free(path, header = FALSE)
        } else {
          tmp <- utils::read.table(
            file = path,
            header = FALSE,
            sep = "",
            fill = TRUE,
            stringsAsFactors = FALSE,
            na.strings = c(".", "*", "NA", "")
          )
          names(tmp) <- paste0("V", seq_len(ncol(tmp)))
          tmp
        }
      }, error = function(e) {
        last_err <<- conditionMessage(e)
        NULL
      })
      if (!is.null(obj)) {
        return(obj)
      }
    }
    Sys.sleep(sleep_sec)
  }
  if (!is.null(last_err)) {
    .safe_stop("Failed to read Mplus savedata after retry: ", path, " / ", last_err)
  }
  .safe_stop("Failed to read Mplus savedata: ", path)
}

.attach_class_by_id_or_row <- function(savedata, analysis_df, display_df, best_k, cfg = NULL, survey_bundle = NULL) {
  det <- .detect_savedata_structure(savedata, best_k = best_k)

  .safe_msg("INFO", "Detected id_col         = ", det$id_col %||% "NULL")
  .safe_msg("INFO", "Detected class_col      = ", det$class_col %||% "NULL")
  .safe_msg("INFO", "Detected posterior_cols = ", paste(det$posterior_cols, collapse = ", "))

  cls_df <- NULL

  if (!is.na(det$class_col) && nzchar(det$class_col)) {
    cls_df <- data.frame(
      class_num = .safe_int(savedata[[det$class_col]]),
      stringsAsFactors = FALSE
    )
    if (length(det$posterior_cols) >= best_k) {
      post <- savedata[, det$posterior_cols[seq_len(best_k)], drop = FALSE]
      post[] <- lapply(post, .safe_num)
      pm <- as.matrix(post)
      cls_df$max_posterior <- apply(pm, 1, function(z) {
        z <- suppressWarnings(as.numeric(z))
        if (all(is.na(z))) return(NA_real_)
        max(z, na.rm = TRUE)
      })
      for (j in seq_len(best_k)) {
        cls_df[[paste0("post_class", j)]] <- pm[, j]
      }
    }
  } else {
    .safe_msg("WARN", "class_col not found -> reconstructing class from posterior")
    cls_df <- .reconstruct_class_from_posterior(
      savedata = savedata,
      posterior_cols = det$posterior_cols,
      best_k = best_k
    )
    .safe_msg("INFO", "Reconstructed class_col = class_num")
  }

  cls_df$class <- paste0("class", cls_df$class_num)
  cls_df$class_label <- paste0("Class ", cls_df$class_num)

  savedata_id_ok <- FALSE
  if (!is.na(det$id_col) && nzchar(det$id_col)) {
    idv <- savedata[[det$id_col]]
    savedata_id_ok <- .is_unique_id(idv)
  }

  analysis_id_col <- .find_analysis_id(analysis_df, cfg = cfg, survey_bundle = survey_bundle)
  analysis_id_ok  <- !is.na(analysis_id_col) && nzchar(analysis_id_col) && .is_unique_id(analysis_df[[analysis_id_col]])

  # A. ID merge
  if (isTRUE(savedata_id_ok) && isTRUE(analysis_id_ok)) {
    .safe_msg("INFO", "Using ID-based merge: savedata(", det$id_col, ") <-> analysis(", analysis_id_col, ")")

    cls_df$.merge_id <- .norm_chr(savedata[[det$id_col]])
    analysis_df$.merge_id <- .norm_chr(analysis_df[[analysis_id_col]])
    display_df$.merge_id  <- .norm_chr(display_df[[analysis_id_col %||% names(display_df)[1]]])

    # display_df의 id 열이 analysis와 다를 수 있어 보정
    if (!(analysis_id_col %in% names(display_df))) {
      disp_id_col <- .find_analysis_id(display_df, cfg = cfg, survey_bundle = survey_bundle)
      if (!is.na(disp_id_col) && nzchar(disp_id_col)) {
        display_df$.merge_id <- .norm_chr(display_df[[disp_id_col]])
      }
    }

    cls_keep <- unique(c(".merge_id", "class_num", "class", "class_label", "max_posterior",
                         paste0("post_class", seq_len(best_k))))
    cls_keep <- intersect(cls_keep, names(cls_df))

    analysis_out <- merge(
      analysis_df,
      cls_df[, cls_keep, drop = FALSE],
      by = ".merge_id",
      all.x = TRUE,
      sort = FALSE
    )
    display_out <- merge(
      display_df,
      cls_df[, cls_keep, drop = FALSE],
      by = ".merge_id",
      all.x = TRUE,
      sort = FALSE
    )

    if (nrow(analysis_out) == nrow(analysis_df) && sum(is.na(analysis_out$class_num)) == 0) {
      analysis_out$.row_order <- match(analysis_out$.merge_id, .norm_chr(analysis_df[[analysis_id_col]]))
      analysis_out <- analysis_out[order(analysis_out$.row_order), , drop = FALSE]
      analysis_out$.row_order <- NULL

      # display도 자신의 id 순서 복원
      disp_id_col <- if (analysis_id_col %in% names(display_df)) analysis_id_col else .find_analysis_id(display_df, cfg = cfg, survey_bundle = survey_bundle)
      if (!is.na(disp_id_col) && nzchar(disp_id_col)) {
        display_out$.row_order <- match(display_out$.merge_id, .norm_chr(display_df[[disp_id_col]]))
        display_out <- display_out[order(display_out$.row_order), , drop = FALSE]
        display_out$.row_order <- NULL
      }

      return(list(
        analysis = subset(analysis_out, select = -".merge_id"),
        display  = subset(display_out,  select = -".merge_id"),
        class_df = cls_df,
        merge_method = paste0("id:", det$id_col, "->", analysis_id_col)
      ))
    }

    .safe_msg("WARN", "ID-based merge did not fully resolve classes -> fallback to row-order merge")
  } else {
    if (!isTRUE(savedata_id_ok) && !is.na(det$id_col) && nzchar(det$id_col)) {
      .safe_msg("WARN", "Detected id_col appears invalid (duplicates or low uniqueness) -> fallback to row-order merge")
    } else {
      .safe_msg("WARN", "Valid ID merge path unavailable -> fallback to row-order merge")
    }
  }

  # B. row-order merge
  if (nrow(savedata) != nrow(analysis_df)) {
    .safe_stop(
      "Row-order merge failed: nrow(savedata)=", nrow(savedata),
      " != nrow(analysis_df)=", nrow(analysis_df)
    )
  }

  .safe_msg("INFO", "Using row-order merge")
  analysis_out <- analysis_df
  display_out  <- display_df

  add_cols <- intersect(c("class_num", "class", "class_label", "max_posterior",
                          paste0("post_class", seq_len(best_k))),
                        names(cls_df))

  for (cc in add_cols) {
    analysis_out[[cc]] <- cls_df[[cc]]
    if (nrow(display_out) == nrow(cls_df)) {
      display_out[[cc]] <- cls_df[[cc]]
    }
  }

  list(
    analysis = analysis_out,
    display  = display_out,
    class_df = cls_df,
    merge_method = "row_order"
  )
}

.make_class_summary <- function(class_num, weight = NULL, class_prefix = "Class ") {
  cls <- .safe_int(class_num)
  keep <- !is.na(cls)

  if (sum(keep) == 0) return(data.frame())

  if (is.null(weight)) {
    tb <- as.data.frame(table(cls[keep]), stringsAsFactors = FALSE)
    names(tb) <- c("class_num", "n")
    tb$class_num <- .safe_int(tb$class_num)
    tb$weighted_n <- tb$n
  } else {
    w <- .safe_num(weight)
    w[!is.finite(w)] <- 0
    d <- data.frame(class_num = cls, w = w, stringsAsFactors = FALSE)
    d <- d[!is.na(d$class_num), , drop = FALSE]

    wn <- stats::aggregate(w ~ class_num, data = d, FUN = sum)
    nn <- as.data.frame(table(d$class_num), stringsAsFactors = FALSE)
    names(nn) <- c("class_num", "n")
    nn$class_num <- .safe_int(nn$class_num)

    tb <- merge(nn, wn, by = "class_num", all = TRUE, sort = TRUE)
    names(tb)[names(tb) == "w"] <- "weighted_n"
  }

  tb <- tb[order(tb$class_num), , drop = FALSE]
  tb$class <- paste0("class", tb$class_num)
  tb$class_label <- paste0(class_prefix, tb$class_num)
  tb$percent <- 100 * tb$n / sum(tb$n, na.rm = TRUE)
  tb$weighted_percent <- 100 * tb$weighted_n / sum(tb$weighted_n, na.rm = TRUE)

  tb[, c("class_num", "class", "class_label", "n", "percent", "weighted_n", "weighted_percent"), drop = FALSE]
}

.resolve_reference_class <- function(class_summary, cfg = NULL) {
  # user fixed reference_class 우선
  ref_cfg <- cfg$three_step$reference_class %||% NULL
  if (!is.null(ref_cfg) && !is.na(ref_cfg)) {
    ref_num <- .safe_int(ref_cfg)[1]
    hit <- class_summary[class_summary$class_num == ref_num, , drop = FALSE]
    if (nrow(hit) > 0) {
      return(list(
        reference_class = ref_num,
        reference_class_label = hit$class_label[1],
        rule = "cfg_fixed"
      ))
    }
  }

  # 기본: largest n
  hit <- class_summary[order(-class_summary$n, class_summary$class_num), , drop = FALSE]
  list(
    reference_class = hit$class_num[1],
    reference_class_label = hit$class_label[1],
    rule = "largest_n"
  )
}

# ------------------------------------------------------------
# 1. start
# ------------------------------------------------------------
if (exists("log_step_start", mode = "function")) {
  log_step_start("CLASSIFY", "04b_classify.R")
} else {
  .safe_msg("INFO", "--------------------------------------------------------")
  .safe_msg("INFO", "[STEP] CLASSIFY")
  .safe_msg("INFO", "[FILE] 04b_classify.R")
  .safe_msg("INFO", "--------------------------------------------------------")
}

tryCatch({

  .safe_msg("INFO", "Reloading best-solution outputs ...")

  CFG <- get0("CFG", ifnotfound = .load_rds_local("CFG", default = list()))
  SURVEY_BUNDLE <- get0("SURVEY_BUNDLE", ifnotfound = .load_rds_local("SURVEY_BUNDLE", default = list()))

  ANALYSIS_DATA <- get0("ANALYSIS_DATA_SUB", ifnotfound = .load_rds_local("ANALYSIS_DATA_SUB", default = data.frame()))
  if (!is.data.frame(ANALYSIS_DATA) || nrow(ANALYSIS_DATA) == 0) {
    ANALYSIS_DATA <- get0("ANALYSIS_DATA", ifnotfound = .load_rds_local("ANALYSIS_DATA", default = data.frame()))
  }

  DISPLAY_DATA <- get0("DISPLAY_DATA_SUB", ifnotfound = .load_rds_local("DISPLAY_DATA_SUB", default = data.frame()))
  if (!is.data.frame(DISPLAY_DATA) || nrow(DISPLAY_DATA) == 0) {
    DISPLAY_DATA <- get0("DISPLAY_DATA", ifnotfound = .load_rds_local("DISPLAY_DATA", default = data.frame()))
  }

  BEST_K <- get0("BEST_K", ifnotfound = .load_rds_local("BEST_K", default = NA_integer_))
  BEST_TAG <- get0("BEST_TAG", ifnotfound = .load_rds_local("BEST_TAG", default = NA_character_))
  BEST_MODEL_STRUCTURE <- get0("BEST_MODEL_STRUCTURE", ifnotfound = .load_rds_local("BEST_MODEL_STRUCTURE", default = NA_character_))
  MIXTURE_TYPE <- get0("MIXTURE_TYPE", ifnotfound = .load_rds_local("MIXTURE_TYPE", default = NA_character_))
  ESTIMATION_REGISTRY <- get0("ESTIMATION_REGISTRY", ifnotfound = .load_rds_local("ESTIMATION_REGISTRY", default = data.frame()))
  BEST_K_SUMMARY <- get0("BEST_K_SUMMARY", ifnotfound = .load_rds_local("BEST_K_SUMMARY", default = list()))
  if (!is.finite(BEST_K) && (is.list(BEST_K_SUMMARY) || is.data.frame(BEST_K_SUMMARY))) {
    BEST_K <- .safe_int(BEST_K_SUMMARY$best_k %||% BEST_K_SUMMARY$BEST_K %||% NA_integer_)[1]
  }
  if (.is_blank_scalar(BEST_TAG) && (is.list(BEST_K_SUMMARY) || is.data.frame(BEST_K_SUMMARY))) {
    BEST_TAG <- as.character(BEST_K_SUMMARY$best_tag %||% BEST_K_SUMMARY$BEST_TAG %||% NA_character_)[1]
  }
  if (.is_blank_scalar(BEST_MODEL_STRUCTURE) && (is.list(BEST_K_SUMMARY) || is.data.frame(BEST_K_SUMMARY))) {
    BEST_MODEL_STRUCTURE <- as.character(
      BEST_K_SUMMARY$model_structure %||%
        BEST_K_SUMMARY$MODEL_STRUCTURE %||%
        BEST_K_SUMMARY$best_model_structure %||%
        NA_character_
    )[1]
  }

  if (!is.finite(BEST_K)) .safe_stop("BEST_K not found.")
  if (.is_blank_scalar(BEST_TAG)) .safe_stop("BEST_TAG not found.")
  if (!is.data.frame(ANALYSIS_DATA) || nrow(ANALYSIS_DATA) == 0) .safe_stop("ANALYSIS_DATA not found or empty.")
  if (!is.data.frame(DISPLAY_DATA) || nrow(DISPLAY_DATA) == 0) DISPLAY_DATA <- ANALYSIS_DATA

  .safe_msg("INFO", "BEST_K               = ", BEST_K)
  .safe_msg("INFO", "BEST_TAG             = ", BEST_TAG)
  .safe_msg("INFO", "BEST_MODEL_STRUCTURE = ", BEST_MODEL_STRUCTURE %||% "NULL")

  # ----------------------------------------------------------
  # 2. locate savedata / cprob
  # ----------------------------------------------------------
  cprob_path <- NA_character_

  if (is.data.frame(ESTIMATION_REGISTRY) && nrow(ESTIMATION_REGISTRY) > 0) {
    tag_col <- .pick_col(names(ESTIMATION_REGISTRY), c("^model_tag$", "^tag$", "best_tag"))
    path_col <- .pick_col(names(ESTIMATION_REGISTRY), c("savedata_file$", "cprob", "savedata"))
    if (!is.na(tag_col) && !is.na(path_col)) {
      hit <- ESTIMATION_REGISTRY[ESTIMATION_REGISTRY[[tag_col]] == BEST_TAG, , drop = FALSE]
      if (nrow(hit) > 0) {
        cprob_path <- as.character(hit[[path_col]][1])
      }
    }
  }

  if (.is_blank_scalar(cprob_path) || !file.exists(cprob_path)) {
    # fallback: model_structure를 이용한 전형적 경로
    maybe <- file.path(get0("DIR_MPLUS_SAVEDATA", ifnotfound = file.path(get0("DIR_MPLUS", ifnotfound = "."), "savedata")),
                       paste0(BEST_MODEL_STRUCTURE, "_cprob_k", BEST_K, ".dat"))
    if (file.exists(maybe)) cprob_path <- maybe
  }

  if (.is_blank_scalar(cprob_path) || !file.exists(cprob_path)) {
    .safe_stop("CPROB PATH not found for BEST_TAG: ", BEST_TAG)
  }

  .safe_msg("INFO", "CPROB PATH           = ", cprob_path)
  .safe_msg("INFO", "MODEL STRUCTURE USED = ", BEST_MODEL_STRUCTURE %||% "NULL")

  # ----------------------------------------------------------
  # 3. read savedata
  # ----------------------------------------------------------
  savedata <- .read_savedata_with_retry(cprob_path, max_tries = 8L, sleep_sec = 1)

  savedata <- .safe_df(savedata)

  .safe_msg("INFO", "Savedata loaded: nrow=", nrow(savedata), ", ncol=", ncol(savedata))
  .safe_msg("INFO", "Savedata columns: ", paste(names(savedata), collapse = ", "))

  # ----------------------------------------------------------
  # 4. attach classes
  # ----------------------------------------------------------
  attach_res <- .attach_class_by_id_or_row(
    savedata      = savedata,
    analysis_df   = ANALYSIS_DATA,
    display_df    = DISPLAY_DATA,
    best_k        = BEST_K,
    cfg           = CFG,
    survey_bundle = SURVEY_BUNDLE
  )

  ANALYSIS_DATA_CLASSIFIED  <- attach_res$analysis
  DISPLAY_DATA_CLASSIFIED   <- attach_res$display
  CLASS_ASSIGNMENT          <- attach_res$class_df
  CLASS_ASSIGNMENT$best_k   <- BEST_K
  CLASS_ASSIGNMENT$best_tag <- BEST_TAG

  CLASS_MEMBERSHIP          <- CLASS_ASSIGNMENT[, intersect(
    c("class_num", "class", "class_label", "max_posterior", paste0("post_class", seq_len(BEST_K))),
    names(CLASS_ASSIGNMENT)
  ), drop = FALSE]
  # ----------------------------------------------------------
  # 5. class summary / reference class
  # ----------------------------------------------------------
  weight_var <- SURVEY_BUNDLE$weight_var %||% CFG$survey_design$weight_var %||% NULL
  weight_vec <- NULL
  if (!.is_blank_scalar(weight_var) && weight_var %in% names(ANALYSIS_DATA_CLASSIFIED)) {
    weight_vec <- ANALYSIS_DATA_CLASSIFIED[[weight_var]]
  }

  CLASS_SUMMARY <- .make_class_summary(
    class_num    = ANALYSIS_DATA_CLASSIFIED$class_num,
    weight       = weight_vec,
    class_prefix = "Class "
  )

  # ------------------------------------------------------------
  # posterior summary (always save for downstream tables)
  # ------------------------------------------------------------
  POSTERIOR_MAX_DF <- data.frame()

  if (is.data.frame(CLASS_ASSIGNMENT) &&
      nrow(CLASS_ASSIGNMENT) > 0 &&
      all(c("class_num", "max_posterior") %in% names(CLASS_ASSIGNMENT))) {

    tmp_post <- stats::aggregate(
      max_posterior ~ class_num,
      data = CLASS_ASSIGNMENT,
      FUN = function(z) mean(as.numeric(z), na.rm = TRUE)
    )

    names(tmp_post) <- c("class_num", "mean_posterior")

    if (is.data.frame(CLASS_SUMMARY) &&
        nrow(CLASS_SUMMARY) > 0 &&
        "class_num" %in% names(CLASS_SUMMARY)) {
      CLASS_SUMMARY <- merge(
        CLASS_SUMMARY,
        tmp_post,
        by = "class_num",
        all.x = TRUE,
        sort = TRUE
      )
    }

    POSTERIOR_MAX_DF <- data.frame(
      Class_num    = tmp_post$class_num,
      PosteriorMax = tmp_post$mean_posterior,
      stringsAsFactors = FALSE
    )
  }

  CLASS_SUMMARY_FINAL <- CLASS_SUMMARY

  ref_info <- .resolve_reference_class(CLASS_SUMMARY, cfg = CFG)
  REFERENCE_CLASS       <- ref_info$reference_class
  REFERENCE_CLASS_LABEL <- ref_info$reference_class_label

  .safe_msg("INFO", "REFERENCE_CLASS       = ", REFERENCE_CLASS)
  .safe_msg("INFO", "REFERENCE_CLASS_LABEL = ", REFERENCE_CLASS_LABEL)
  .safe_msg("INFO", "REFERENCE_RULE        = ", ref_info$rule)

  # ----------------------------------------------------------
  # 6. save outputs
  # ----------------------------------------------------------
  .safe_msg("INFO", "Saving classify outputs ...")

  .save_rds_local(ANALYSIS_DATA_CLASSIFIED, "ANALYSIS_DATA_CLASSIFIED")
  .save_rds_local(DISPLAY_DATA_CLASSIFIED,  "DISPLAY_DATA_CLASSIFIED")
  .save_rds_local(CLASS_ASSIGNMENT,         "CLASS_ASSIGNMENT")
  .save_rds_local(CLASS_MEMBERSHIP,         "CLASS_MEMBERSHIP")
  .save_rds_local(CLASS_SUMMARY,            "CLASS_SUMMARY")
  .save_rds_local(CLASS_SUMMARY_FINAL,      "CLASS_SUMMARY_FINAL")
  .save_rds_local(REFERENCE_CLASS,          "REFERENCE_CLASS")
  .save_rds_local(REFERENCE_CLASS_LABEL,    "REFERENCE_CLASS_LABEL")
  .save_rds_local(attach_res$merge_method,  "CLASSIFY_MERGE_METHOD")
  .save_rds_local(ANALYSIS_DATA_CLASSIFIED, "CLASSIFIED_ANALYSIS")
  .save_rds_local(DISPLAY_DATA_CLASSIFIED,  "CLASSIFIED_DISPLAY")

  # csv도 같이 저장
  dir_csv <- get0("DIR_CSV", ifnotfound = file.path(get0("DIR_OUTPUT", ifnotfound = getwd()), "csv"))
  dir.create(dir_csv, recursive = TRUE, showWarnings = FALSE)

  utils::write.csv(CLASS_SUMMARY, file.path(dir_csv, "CLASS_SUMMARY.csv"), row.names = FALSE, na = "")
  utils::write.csv(CLASS_ASSIGNMENT, file.path(dir_csv, "CLASS_ASSIGNMENT.csv"), row.names = FALSE, na = "")

  # ----------------------------------------------------------
  # 7. finish
  # ----------------------------------------------------------
  elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_CLASSIFY, units = "secs")), 2)

  .safe_msg("INFO", "04b_classify.R completed.")
  .safe_msg("INFO", "n(classified analysis) = ", nrow(ANALYSIS_DATA_CLASSIFIED))
  .safe_msg("INFO", "n(classified display)  = ", nrow(DISPLAY_DATA_CLASSIFIED))
  .safe_msg("INFO", "n(class rows)          = ", nrow(CLASS_ASSIGNMENT))
  .safe_msg("INFO", "BEST_K                 = ", BEST_K)
  .safe_msg("INFO", "elapsed                = ", elapsed_sec, " sec")

  if (exists("log_step_ok", mode = "function")) {
    log_step_ok("classify", elapsed_sec)
  }

}, error = function(e) {
  elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_CLASSIFY, units = "secs")), 2)

  .safe_msg("ERROR", "classify failed (", elapsed_sec, " sec)")
  .safe_msg("ERROR", "MESSAGE: ", conditionMessage(e))

  if (exists("log_step_error", mode = "function")) {
    log_step_error("classify", conditionMessage(e), elapsed_sec)
  }

  stop(e)
})
