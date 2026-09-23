# ============================================================
# 03c_estimation_collect.R
# Collect Mplus output summaries robustly
# ------------------------------------------------------------
# 역할
# 1) 03b run_results / registry 재로딩
# 2) Mplus output(.out)에서 fit summary 파싱
# 3) FIT_SUMMARY / MODEL_CANDIDATES 생성
# 4) best_bic 기준의 임시 best solution 계산
#
# 핵심 수정
# - model_structure 유지
# - 선택 단위를 (model_structure + k) 조합으로 확장
# - FIT_SUMMARY / MODEL_CANDIDATES / CLASS_SUMMARY에 model_structure 연결
# ============================================================

T0_03C <- Sys.time()

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
.now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

log_txt <- function(level = "INFO", ...) {
  cat(sprintf("[%s] [%s] %s\n", .now_txt(), level, paste0(...)))
}
log_info <- function(...) log_txt("INFO", ...)
log_warn <- function(...) log_txt("WARN", ...)
log_err  <- function(...) log_txt("ERROR", ...)

`%||%` <- function(x, y) if (is.null(x)) y else x

if (!exists("mixture_parse_run_quality")) {
  stop("mixture_parse_run_quality() not found. Make sure 15_mixture_selection_core.R is sourced before 03c.", call. = FALSE)
}

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

find_existing_first <- function(paths) {
  paths <- as.character(paths)
  paths <- unique(paths[!is.na(paths) & nzchar(trimws(paths))])
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

extract_k_from_tag <- function(x) {
  suppressWarnings(as.integer(sub(".*_k([0-9]+)_.*", "\\1", x)))
}

extract_model_structure_from_tag <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  hit <- grepl("_model[0-9]+_k[0-9]+_", x, ignore.case = TRUE)
  out[hit] <- sub("^.*_(model[0-9]+)_k[0-9]+_.*$", "\\1", x[hit], ignore.case = TRUE)
  tolower(out)
}

extract_numeric_values <- function(line) {
  nums <- regmatches(
    line,
    gregexpr("-?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[DEde][+-]?[0-9]+)?", line, perl = TRUE)
  )[[1]]
  vals <- suppressWarnings(as.numeric(gsub("[Dd]", "E", nums)))
  vals[!is.na(vals)]
}

extract_value_with_fallback <- function(pattern, txt, up, max_lookahead = 5L) {
  idx <- grep(pattern, up, perl = TRUE)
  if (length(idx) == 0) return(NA_real_)

  line <- trimws(txt[idx[1]])
  vals <- extract_numeric_values(line)

  if (length(vals) > 0) return(vals[length(vals)])

  start <- idx[1] + 1L
  end   <- min(length(txt), idx[1] + max_lookahead)

  for (j in seq.int(start, end)) {
    line_j <- trimws(txt[j])
    if (!nzchar(line_j)) next

    vals <- extract_numeric_values(line_j)

    if (length(vals) > 0) return(vals[length(vals)])
  }

  NA_real_
}

extract_value_after_anchor <- function(anchor_pattern, value_pattern, txt, up, max_lookahead = 20L) {
  idx <- grep(anchor_pattern, up, perl = TRUE)
  if (length(idx) == 0) return(NA_real_)
  for (ii in idx) {
    rng <- seq.int(ii, min(length(txt), ii + max_lookahead))
    hit <- rng[grepl(value_pattern, up[rng], perl = TRUE)]
    if (length(hit) == 0) next
    vals <- extract_numeric_values(trimws(txt[hit[1]]))
    if (length(vals) > 0) return(vals[length(vals)])
  }
  NA_real_
}

# ------------------------------------------------------------
# 1. parser
# ------------------------------------------------------------
parse_mplus_fit <- function(out_file, model_tag = NA_character_, expected_k = NA_integer_) {
  txt <- safe_read_lines(out_file)
  up  <- toupper(txt)
  quality <- mixture_parse_run_quality(txt, k = expected_k)

  if (length(txt) == 0) {
    return(data.frame(
      model_tag        = model_tag,
      out_file         = out_file,
      parse_ok         = FALSE,
      ll               = NA_real_,
      aic              = NA_real_,
      bic              = NA_real_,
      sabic            = NA_real_,
      entropy          = NA_real_,
      vlmr_stat        = NA_real_,
      vlmr_p           = NA_real_,
      lmr_stat         = NA_real_,
      lmr_p            = NA_real_,
      blrt_stat        = NA_real_,
      blrt_p           = NA_real_,
      npar             = NA_real_,
      smallest_class_n = NA_real_,
      smallest_class_p = NA_real_,
      class_count_complete = FALSE,
      fit_metrics_complete = FALSE,
      status = "failed",
      output_present = FALSE,
      terminated_normally = FALSE,
      converged = FALSE,
      best_ll_replicated = NA,
      loglik_replicated = NA,
      replication_ok = FALSE,
      local_maxima_warning = FALSE,
      admissible = FALSE,
      run_quality_ok = FALSE,
      failure_reasons = paste(quality$failure_reasons, collapse = ";"),
      warning_text = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  extract_npar_mplus <- function(txt, up) {
    val <- extract_value_with_fallback("NUMBER OF FREE PARAMETERS", txt, up, max_lookahead = 8L)
    if (!is.na(val)) return(val)

    idx <- grep("FREE PARAMETERS", up, perl = TRUE)
    if (length(idx) == 0) return(NA_real_)

    for (ii in idx) {
      rng <- seq.int(ii, min(length(txt), ii + 8L))
      block <- txt[rng]

      for (ln in block) {
        line <- trimws(ln)
        if (!nzchar(line)) next

        vals <- extract_numeric_values(line)

        if (length(vals) > 0) return(vals[length(vals)])
      }
    }

    NA_real_
  }

  extract_smallest_class_prop_mplus <- function(txt, up, expected_k = NA_integer_) {
    parsed <- mixture_parse_class_count_block(txt, expected_k = expected_k)
    list(
      smallest_class_n = parsed$smallest_class_n,
      smallest_class_p = parsed$smallest_class_p,
      class_count_complete = isTRUE(parsed$complete)
    )
  }

  ll_val <- extract_value_after_anchor("LOGLIKELIHOOD", "H0\\s+VALUE", txt, up, max_lookahead = 8L)
  if (is.na(ll_val)) {
    ll_val <- extract_value_with_fallback("FINAL STAGE LOGLIKELIHOOD|BEST LOGLIKELIHOOD VALUE", txt, up)
  }

  aic_val <- extract_value_with_fallback(
    "AIC",
    txt, up
  )

  bic_val <- extract_value_with_fallback(
    "BIC",
    txt, up
  )

  sabic_val <- extract_value_with_fallback(
    "SAMPLE-SIZE ADJUSTED BIC|SABIC|ADJUSTED BIC",
    txt, up
  )

  ent_val <- extract_value_with_fallback(
    "ENTROPY",
    txt, up
  )
  vlmr_stat <- extract_value_after_anchor(
    "VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST",
    "2\\s+TIMES\\s+THE\\s+LOGLIKELIHOOD\\s+DIFFERENCE",
    txt, up, max_lookahead = 20L
  )
  vlmr_p <- extract_value_after_anchor(
    "VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST",
    "^\\s*P-VALUE",
    txt, up, max_lookahead = 25L
  )
  lmr_stat <- extract_value_after_anchor(
    "LO-MENDELL-RUBIN ADJUSTED LRT TEST",
    "^\\s*VALUE",
    txt, up, max_lookahead = 12L
  )
  lmr_p <- extract_value_after_anchor(
    "LO-MENDELL-RUBIN ADJUSTED LRT TEST",
    "^\\s*P-VALUE",
    txt, up, max_lookahead = 12L
  )
  blrt_stat <- extract_value_after_anchor(
    "PARAMETRIC BOOTSTRAPPED LIKELIHOOD RATIO TEST",
    "2\\s+TIMES\\s+THE\\s+LOGLIKELIHOOD\\s+DIFFERENCE",
    txt, up, max_lookahead = 35L
  )
  blrt_p <- extract_value_after_anchor(
    "PARAMETRIC BOOTSTRAPPED LIKELIHOOD RATIO TEST",
    "P-VALUE",
    txt, up, max_lookahead = 45L
  )
  npar_val         <- extract_npar_mplus(txt, up)
  class_info       <- extract_smallest_class_prop_mplus(txt, up, expected_k = expected_k)
  smallest_class_n <- class_info$smallest_class_n
  smallest_class_p <- class_info$smallest_class_p

  out <- data.frame(
    model_tag        = model_tag,
    out_file         = out_file,
    parse_ok         = TRUE,
    ll               = ll_val,
    aic              = aic_val,
    bic              = bic_val,
    sabic            = sabic_val,
    entropy          = ent_val,
    vlmr_stat        = vlmr_stat,
    vlmr_p           = vlmr_p,
    lmr_stat         = lmr_stat,
    lmr_p            = lmr_p,
    blrt_stat        = blrt_stat,
    blrt_p           = blrt_p,
    npar             = npar_val,
    smallest_class_n = smallest_class_n,
    smallest_class_p = smallest_class_p,
    class_count_complete = isTRUE(class_info$class_count_complete),
    fit_metrics_complete = FALSE,
    status = as.character(quality$status),
    output_present = isTRUE(quality$output_present),
    terminated_normally = isTRUE(quality$terminated_normally),
    converged = isTRUE(quality$converged),
    best_ll_replicated = quality$best_ll_replicated,
    loglik_replicated = quality$loglik_replicated,
    replication_ok = isTRUE(quality$replication_ok),
    local_maxima_warning = isTRUE(quality$local_maxima_warning),
    admissible = isTRUE(quality$admissible),
    run_quality_ok = isTRUE(quality$run_quality_ok),
    failure_reasons = paste(quality$failure_reasons, collapse = ";"),
    warning_text = if (length(quality$warning_lines) > 0L) paste(quality$warning_lines, collapse = "\n") else NA_character_,
    stringsAsFactors = FALSE
  )

  required_fit <- c("ll", "aic", "bic", "sabic", "npar")
  out$fit_metrics_complete <- all(vapply(out[required_fit], function(x) is.finite(suppressWarnings(as.numeric(x[1]))), logical(1)))
  out$parse_ok <- isTRUE(out$fit_metrics_complete)
  out
}

parse_lpa_indicator_profile <- function(out_file, indicators, model_tag = NA_character_) {
  parsed <- profile_parse_mplus_means(
    path = out_file,
    indicators = indicators,
    model_tag = model_tag,
    require_normal_termination = TRUE
  )
  if (!isTRUE(parsed$available)) return(data.frame())
  parsed$data
}

build_best_lpa_indicator_profile <- function(best_row, dict = NULL) {
  if (!is.data.frame(best_row) || nrow(best_row) == 0) return(data.frame())

  out_file_i  <- as.character(best_row$out_file[1] %||% NA_character_)
  model_tag_i <- as.character(best_row$model_tag[1] %||% NA_character_)

  # ------------------------------------------------------------
  # resolve indicators (robust)
  # ------------------------------------------------------------
  get_first_cell_chr <- function(df, col) {
    if (!is.data.frame(df) || !col %in% names(df) || nrow(df) == 0) return(character(0))

    x <- df[[col]]

    # list-column
    if (is.list(x)) {
      cell <- x[[1]]
      if (is.null(cell)) return(character(0))
      return(as.character(unlist(cell, use.names = FALSE)))
    }

    # plain vector/data.frame column
    cell <- x[1]
    if (length(cell) == 0 || is.na(cell)) return(character(0))
    as.character(cell)
  }

  indicators_i <- character(0)

  # 1차: registry의 continuous
  indicators_i <- get_first_cell_chr(best_row, "continuous")

  # 2차: registry의 usevariables
  if (length(indicators_i) == 0) {
    indicators_i <- get_first_cell_chr(best_row, "usevariables")
  }

  # 3차: settings summary
  if (length(indicators_i) == 0) {
    indicators_i <- as.character(
      SETTINGS_SUMMARY$INDICATORS_CONTINUOUS %||%
        SETTINGS_SUMMARY$indicators_continuous %||%
        SETTINGS_SUMMARY$INDICATORS %||%
        SETTINGS_SUMMARY$indicators %||%
        character(0)
    )
  }

  # 4차: 전역 객체 fallback
  if (length(indicators_i) == 0 && exists("INDICATORS_CONTINUOUS")) {
    indicators_i <- as.character(get("INDICATORS_CONTINUOUS"))
  }
  if (length(indicators_i) == 0 && exists("INDICATORS")) {
    indicators_i <- as.character(get("INDICATORS"))
  }

  # 정리
  indicators_i <- as.character(indicators_i)
  indicators_i <- trimws(indicators_i)
  indicators_i <- indicators_i[!is.na(indicators_i) & nzchar(indicators_i)]
  indicators_i <- unique(toupper(indicators_i))

  log_info("Indicator candidates for profile = ", paste(indicators_i, collapse = ", "))

  prof <- parse_lpa_indicator_profile(
    out_file   = out_file_i,
    indicators = indicators_i,
    model_tag  = model_tag_i
  )

  log_info("Parsed indicator profile rows = ", nrow(prof))

  if (nrow(prof) == 0) return(prof)

  prof$class <- prof$Class

  if (is.list(dict) && is.data.frame(dict$meta) && nrow(dict$meta) > 0) {
    meta <- dict$meta

    if ("var_name" %in% names(meta)) {
      prof$var_label <- vapply(
        prof$var_name,
        function(v) {
          hit <- meta[meta$var_name == v, , drop = FALSE]
          if (nrow(hit) == 0) return(v)

          cand <- character(0)
          if ("label_en" %in% names(hit)) cand <- c(cand, as.character(hit$label_en[1]))
          if ("label_ko" %in% names(hit)) cand <- c(cand, as.character(hit$label_ko[1]))
          if ("var_label" %in% names(hit)) cand <- c(cand, as.character(hit$var_label[1]))

          cand <- trimws(cand)
          cand <- cand[!is.na(cand) & nzchar(cand)]

          if (length(cand) == 0) v else cand[1]
        },
        character(1)
      )
    }
  }

  prof
}

# ------------------------------------------------------------
# 2. reload run outputs
# ------------------------------------------------------------
log_info("Reloading estimation run outputs ...")

RUN_RDS <- file.path(DIR_RDS, "estimation_run.rds")
if (!file.exists(RUN_RDS)) {
  stop("Missing estimation_run.rds: ", RUN_RDS)
}

run_obj     <- readRDS(RUN_RDS)
run_results <- run_obj$run_results %||% data.frame()

# ------------------------------------------------------------
# 2-1. reload supporting objects
# ------------------------------------------------------------
SETTINGS_SUMMARY <- load_step_rds(
  "SETTINGS_SUMMARY",
  dir_rds = DIR_RDS,
  default = list()
)

DICT <- load_step_rds(
  "DICT",
  dir_rds = DIR_RDS,
  default = list()
)

# ------------------------------------------------------------
# 2-2. reload / normalize ESTIMATION_REGISTRY
# ------------------------------------------------------------
ESTIMATION_REGISTRY <- load_step_rds(
  "ESTIMATION_REGISTRY",
  dir_rds = DIR_RDS,
  required = TRUE
)

registry_df <- tryCatch(
  mixture_registry_to_row_df(ESTIMATION_REGISTRY),
  error = function(e) NULL
)

if (is.null(registry_df) || !is.data.frame(registry_df) || nrow(registry_df) == 0) {
  stop("Failed to normalize ESTIMATION_REGISTRY.", call. = FALSE)
}

if (!"model_tag" %in% names(registry_df)) {
  stop("ESTIMATION_REGISTRY does not contain 'model_tag'.", call. = FALSE)
}
if (!"k" %in% names(registry_df)) {
  registry_df$k <- extract_k_from_tag(registry_df$model_tag)
}
if (!"model_structure" %in% names(registry_df)) {
  registry_df$model_structure <- extract_model_structure_from_tag(registry_df$model_tag)
}
registry_df$model_structure <- tolower(as.character(registry_df$model_structure))

if (!is.data.frame(run_results) || nrow(run_results) == 0) {
  stop("No run_results found in estimation_run.rds", call. = FALSE)
}

if (!"model_tag" %in% names(run_results)) {
  stop("run_results must contain 'model_tag'.", call. = FALSE)
}
if (!"k" %in% names(run_results)) {
  run_results$k <- extract_k_from_tag(run_results$model_tag)
}
if (!"model_structure" %in% names(run_results)) {
  run_results$model_structure <- extract_model_structure_from_tag(run_results$model_tag)
}
run_results$model_structure <- tolower(as.character(run_results$model_structure))

# ------------------------------------------------------------
# 3. collect
# ------------------------------------------------------------
fit_list <- vector("list", nrow(run_results))

for (i in seq_len(nrow(run_results))) {
  rr <- run_results[i, , drop = FALSE]

  model_tag <- as.character(rr$model_tag[1])
  k_i <- suppressWarnings(as.integer(rr$k[1] %||% extract_k_from_tag(model_tag)))
  model_structure_i <- as.character(rr$model_structure[1] %||% extract_model_structure_from_tag(model_tag))

  # Only parse the exact output path certified by the execution registry.
  # A conventional-name fallback can silently pick up a stale file.
  out_candidates <- as.character(rr$out_file %||% NA_character_)
  out_file <- if (
    length(out_candidates) == 1L && !is.na(out_candidates) &&
      nzchar(trimws(out_candidates)) && file.exists(out_candidates)
  ) out_candidates else NA_character_

  if (is.na(out_file)) {
    log_warn("Output file not found: ", paste(out_candidates, collapse = " | "))

    fit_list[[i]] <- data.frame(
      k                = k_i,
      model_structure  = model_structure_i,
      model_tag        = model_tag,
      out_file         = NA_character_,
      status           = "failed",
      parse_ok         = FALSE,
      ll               = NA_real_,
      aic              = NA_real_,
      bic              = NA_real_,
      sabic            = NA_real_,
      entropy          = NA_real_,
      npar             = NA_real_,
      smallest_class_n = NA_real_,
      smallest_class_p = NA_real_,
      class_count_complete = FALSE,
      fit_metrics_complete = FALSE,
      exec_ok          = isTRUE(rr$exec_ok %||% FALSE),
      out_fresh        = isTRUE(rr$out_fresh %||% FALSE),
      out_signature_ok = FALSE,
      cprob_fresh      = isTRUE(rr$cprob_fresh %||% FALSE),
      output_present   = FALSE,
      terminated_normally = FALSE,
      converged        = FALSE,
      best_ll_replicated = NA,
      loglik_replicated = NA,
      replication_ok   = FALSE,
      local_maxima_warning = FALSE,
      admissible       = FALSE,
      run_quality_ok   = FALSE,
      failure_reasons  = "output_missing",
      warning_text     = as.character(rr$warning_text %||% NA_character_),
      stringsAsFactors = FALSE
    )
    next
  }

  out_signature_ok <- mixture_file_signature_matches(
    out_file,
    expected_size = rr$out_size,
    expected_mtime = rr$out_mtime,
    expected_md5 = rr$out_md5
  )
  fit_i <- parse_mplus_fit(out_file, model_tag = model_tag, expected_k = k_i)
  fit_i$exec_ok <- isTRUE(rr$exec_ok %||% FALSE)
  fit_i$out_signature_ok <- out_signature_ok
  fit_i$out_fresh <- isTRUE(rr$out_fresh %||% FALSE) && out_signature_ok
  fit_i$cprob_fresh <- isTRUE(rr$cprob_fresh %||% FALSE)
  fit_i$run_quality_ok <- isTRUE(fit_i$run_quality_ok[1]) && fit_i$exec_ok[1] && fit_i$out_fresh[1]
  fit_i$status <- if (isTRUE(fit_i$run_quality_ok[1])) "ok" else "failed"
  combined_reasons <- unique(c(
    if (!fit_i$exec_ok[1]) "execution_failed" else character(0),
    if (!out_signature_ok) "output_signature_mismatch" else character(0),
    if (!fit_i$out_fresh[1]) "output_not_fresh" else character(0),
    unlist(strsplit(as.character(rr$failure_reasons %||% ""), ";", fixed = TRUE)),
    unlist(strsplit(as.character(fit_i$failure_reasons[1] %||% ""), ";", fixed = TRUE))
  ))
  combined_reasons <- trimws(combined_reasons)
  combined_reasons <- combined_reasons[!is.na(combined_reasons) & nzchar(combined_reasons)]
  fit_i$failure_reasons <- paste(combined_reasons, collapse = ";")
  fit_i$k <- k_i
  fit_i$model_structure <- model_structure_i

  fit_list[[i]] <- fit_i
}

fit_tbl <- dplyr::bind_rows(fit_list)

if (!"k" %in% names(fit_tbl)) {
  fit_tbl$k <- extract_k_from_tag(fit_tbl$model_tag)
}
if (!"model_structure" %in% names(fit_tbl)) {
  fit_tbl$model_structure <- extract_model_structure_from_tag(fit_tbl$model_tag)
}
fit_tbl$model_structure <- tolower(as.character(fit_tbl$model_structure))

# ------------------------------------------------------------
# 4. simple best bic by full solution
# ------------------------------------------------------------
best_bic_k               <- NA_real_
best_bic_tag             <- NA_character_
best_bic_model_structure <- NA_character_
best_row                 <- data.frame()

cand <- fit_tbl[
  !is.na(fit_tbl$parse_ok) &
    fit_tbl$parse_ok &
    !is.na(fit_tbl$status) & fit_tbl$status == "ok" &
    !is.na(fit_tbl$run_quality_ok) & fit_tbl$run_quality_ok &
    !is.na(fit_tbl$class_count_complete) & fit_tbl$class_count_complete &
    !is.na(fit_tbl$bic),
  ,
  drop = FALSE
]

if (nrow(cand) > 0) {
  best_row <- cand[which.min(cand$bic), , drop = FALSE]

  # ------------------------------------------------------------
  # 🔥 FIX: enrich best_row from ESTIMATION_REGISTRY
  # ------------------------------------------------------------
  registry_row <- registry_df[
    registry_df$model_tag == best_row$model_tag[1],
    ,
    drop = FALSE
  ]

  if (nrow(registry_row) > 0) {
    best_row <- registry_row[1, , drop = FALSE]
  }

  best_bic_tag <- best_row$model_tag[1]
  best_bic_k   <- best_row$k[1]
  best_bic_model_structure <- best_row$model_structure[1]
}

# ------------------------------------------------------------
# 4-1. build indicator profile for best LPA solution
# ------------------------------------------------------------
T3_indicator_profile <- data.frame()

if (is.data.frame(best_row) && nrow(best_row) > 0) {
  T3_indicator_profile <- tryCatch(
    build_best_lpa_indicator_profile(best_row = best_row, dict = DICT),
    error = function(e) {
      log_warn("Failed to build T3_indicator_profile: ", conditionMessage(e))
      data.frame()
    }
  )
}

# ------------------------------------------------------------
# 5. save
# ------------------------------------------------------------
log_info("FIT_SUMMARY prepared: n = ", nrow(fit_tbl))
log_info("parse_ok models     = ", sum(fit_tbl$parse_ok, na.rm = TRUE))

FIT_SUMMARY <- fit_tbl

CLASS_SUMMARY_FINAL <- load_step_rds(
  "CLASS_SUMMARY_FINAL",
  dir_rds = DIR_RDS,
  default = data.frame()
)

if (is.data.frame(CLASS_SUMMARY_FINAL) && nrow(CLASS_SUMMARY_FINAL) > 0) {
  CLASS_SUMMARY <- CLASS_SUMMARY_FINAL

  if (!"k" %in% names(CLASS_SUMMARY)) {
    CLASS_SUMMARY$k <- best_bic_k
  }
  if (!"model_tag" %in% names(CLASS_SUMMARY)) {
    CLASS_SUMMARY$model_tag <- best_bic_tag
  }
  if (!"model_structure" %in% names(CLASS_SUMMARY)) {
    CLASS_SUMMARY$model_structure <- best_bic_model_structure
  }
} else {
  CLASS_SUMMARY <- data.frame()
}

MODEL_CANDIDATES <- fit_tbl

ESTIMATION_COLLECT_SUMMARY <- list(
  fit_tbl = fit_tbl,
  best_bic_k = best_bic_k,
  best_bic_tag = best_bic_tag,
  best_bic_model_structure = best_bic_model_structure,
  elapsed_sec = as.numeric(difftime(Sys.time(), T0_03C, units = "secs"))
)

saveRDS(
  ESTIMATION_COLLECT_SUMMARY,
  file = file.path(DIR_RDS, "estimation_collect.rds")
)

save_named_rds_list(
  list(
    FIT_SUMMARY = FIT_SUMMARY,
    CLASS_SUMMARY = CLASS_SUMMARY,
    MODEL_CANDIDATES = MODEL_CANDIDATES,
    T3_indicator_profile = T3_indicator_profile,
    ESTIMATION_COLLECT_SUMMARY = ESTIMATION_COLLECT_SUMMARY
  ),
  dir_rds = DIR_RDS
)

utils::write.csv(
  fit_tbl,
  file = file.path(DIR_TABLES, "estimation_fit_summary.csv"),
  row.names = FALSE,
  na = ""
)

log_info("03c_estimation_collect.R completed.")
log_info("n(models ok)            = ", sum(fit_tbl$status == "ok", na.rm = TRUE))
log_info("n(fit rows)             = ", nrow(fit_tbl))
log_info("n(parse ok)             = ", sum(fit_tbl$parse_ok, na.rm = TRUE))
log_info("best bic k              = ", ifelse(is.na(best_bic_k), "NA", best_bic_k))
log_info("best bic model_struct   = ", ifelse(is.na(best_bic_model_structure), "NA", best_bic_model_structure))
log_info("best bic tag            = ", ifelse(is.na(best_bic_tag), "NA", best_bic_tag))
log_info(
  "elapsed                 = ",
  round(as.numeric(difftime(Sys.time(), T0_03C, units = "secs")), 2),
  " sec"
)
log_info("n(T3_indicator_profile) = ", nrow(T3_indicator_profile))
