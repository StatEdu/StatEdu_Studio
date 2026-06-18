# ============================================================
# 04a_select_best_k.R
# Select best mixture solution for cross-sectional mixture analysis
# ------------------------------------------------------------
# 역할
# 1) 03c 산출물(FIT_SUMMARY 등) 재로딩
# 2) best solution 선택 규칙 적용
# 3) fixed / auto 모드 지원
# 4) 최종 BEST_MODEL_ROW / BEST_K / BEST_TAG / BEST_MODEL_STRUCTURE 저장
#
# 핵심 수정
# - 선택 단위를 k 단독이 아니라 (model_structure + k) 조합으로 확장
# - hybrid rule 지원
# - filter -> shortlist -> tie-break 구조 적용
# - 선택 이유(best_reason_detail) 저장
# ============================================================

T0_SELECT_BEST_K <- Sys.time()

# ------------------------------------------------------------
# 0. start log
# ------------------------------------------------------------
log_step_start("SELECT_BEST_K", "04a_select_best_k.R")
log_info("Reloading estimation collect outputs ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------
# 1. reload objects
# ------------------------------------------------------------
FIT_SUMMARY <- load_step_rds(
  "FIT_SUMMARY",
  dir_rds  = DIR_RDS,
  required = TRUE
)

CLASS_SUMMARY <- load_step_rds(
  "CLASS_SUMMARY",
  dir_rds  = DIR_RDS,
  default  = data.frame()
)

MODEL_CANDIDATES <- load_step_rds(
  "MODEL_CANDIDATES",
  dir_rds  = DIR_RDS,
  default  = FIT_SUMMARY
)

ESTIMATION_COLLECT_SUMMARY <- load_step_rds(
  "ESTIMATION_COLLECT_SUMMARY",
  dir_rds  = DIR_RDS,
  default  = list()
)

CFG <- load_step_rds(
  "CFG",
  dir_rds  = DIR_RDS,
  required = TRUE
)

SETTINGS_SUMMARY <- load_step_rds(
  "SETTINGS_SUMMARY",
  dir_rds  = DIR_RDS,
  required = TRUE
)

SURVEY_BUNDLE <- load_step_rds(
  "SURVEY_BUNDLE",
  dir_rds  = DIR_RDS,
  default  = list()
)

ANALYSIS_DATA_SUB <- load_step_rds(
  "ANALYSIS_DATA_SUB",
  dir_rds  = DIR_RDS,
  default  = data.frame()
)

if (!is.data.frame(FIT_SUMMARY) || nrow(FIT_SUMMARY) == 0) {
  stop("FIT_SUMMARY is missing or empty.", call. = FALSE)
}

# ------------------------------------------------------------
# 1b. ensure required columns
# ------------------------------------------------------------
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

if (!"model_tag" %in% names(FIT_SUMMARY)) {
  stop("FIT_SUMMARY must contain 'model_tag'.", call. = FALSE)
}
if (!"k" %in% names(FIT_SUMMARY)) {
  FIT_SUMMARY$k <- extract_k_from_tag(FIT_SUMMARY$model_tag)
}
if (!"model_structure" %in% names(FIT_SUMMARY)) {
  FIT_SUMMARY$model_structure <- extract_model_structure_from_tag(FIT_SUMMARY$model_tag)
}
FIT_SUMMARY$model_structure <- tolower(as.character(FIT_SUMMARY$model_structure))

if (is.data.frame(MODEL_CANDIDATES) && nrow(MODEL_CANDIDATES) > 0) {
  if (!"model_tag" %in% names(MODEL_CANDIDATES) && "model_tag" %in% names(FIT_SUMMARY)) {
    MODEL_CANDIDATES$model_tag <- FIT_SUMMARY$model_tag
  }
  if (!"k" %in% names(MODEL_CANDIDATES) && "model_tag" %in% names(MODEL_CANDIDATES)) {
    MODEL_CANDIDATES$k <- extract_k_from_tag(MODEL_CANDIDATES$model_tag)
  }
  if (!"model_structure" %in% names(MODEL_CANDIDATES) && "model_tag" %in% names(MODEL_CANDIDATES)) {
    MODEL_CANDIDATES$model_structure <- extract_model_structure_from_tag(MODEL_CANDIDATES$model_tag)
  }
  MODEL_CANDIDATES$model_structure <- tolower(as.character(MODEL_CANDIDATES$model_structure))
}

if (is.data.frame(CLASS_SUMMARY) && nrow(CLASS_SUMMARY) > 0) {
  if (!"k" %in% names(CLASS_SUMMARY) && "model_tag" %in% names(CLASS_SUMMARY)) {
    CLASS_SUMMARY$k <- extract_k_from_tag(CLASS_SUMMARY$model_tag)
  }
  if (!"model_structure" %in% names(CLASS_SUMMARY) && "model_tag" %in% names(CLASS_SUMMARY)) {
    CLASS_SUMMARY$model_structure <- extract_model_structure_from_tag(CLASS_SUMMARY$model_tag)
  }
  CLASS_SUMMARY$model_structure <- tolower(as.character(CLASS_SUMMARY$model_structure))
}

# ------------------------------------------------------------
# 2. resolve selection options
# ------------------------------------------------------------
BEST_K_MODE <- tolower(
  CFG$analysis$best_k_mode %||%
    CFG$best_k_mode %||%
    if (!is.null(CFG$estimation$best_k) && !is.na(CFG$estimation$best_k)) "fixed" else "auto"
)

BEST_K_RULE <- tolower(
  CFG$analysis$best_k_rule %||%
    CFG$best_k_rule %||%
    CFG$estimation$best_k_rule %||%
    "hybrid"
)

BEST_K_FIXED <- suppressWarnings(as.integer(
  CFG$analysis$best_k_fixed %||%
    CFG$best_k_fixed %||%
    CFG$estimation$best_k %||%
    NA_integer_
))

BEST_MODEL_STRUCTURE_FIXED <- tolower(as.character(
  CFG$analysis$best_model_structure_fixed %||%
    CFG$best_model_structure_fixed %||%
    CFG$estimation$best_model_structure %||%
    CFG$estimation$model_structure %||%
    NA_character_
))

MIN_CLASS_PROP <- suppressWarnings(as.numeric(
  CFG$analysis$min_class_prop %||%
    CFG$min_class_prop %||%
    0
))

MIN_CLASS_N <- suppressWarnings(as.numeric(
  CFG$analysis$min_class_n %||%
    CFG$min_class_n %||%
    0
))

MIN_ENTROPY_SOFT <- suppressWarnings(as.numeric(
  CFG$analysis$min_entropy_soft %||%
    CFG$min_entropy_soft %||%
    0.70
))

MIN_ENTROPY_HARD <- suppressWarnings(as.numeric(
  CFG$analysis$min_entropy_hard %||%
    CFG$min_entropy_hard %||%
    0.60
))

SHORTLIST_DELTA_DBIC <- suppressWarnings(as.numeric(
  CFG$analysis$shortlist_delta_dbic %||%
    CFG$shortlist_delta_dbic %||%
    10
))

SHORTLIST_DELTA_BIC <- suppressWarnings(as.numeric(
  CFG$analysis$shortlist_delta_bic %||%
    CFG$shortlist_delta_bic %||%
    10
))

SHORTLIST_DELTA_SABIC <- suppressWarnings(as.numeric(
  CFG$analysis$shortlist_delta_sabic %||%
    CFG$shortlist_delta_sabic %||%
    10
))

PREFER_SMALLER_K_ON_TIE <- as_flag(
  CFG$analysis$prefer_smaller_k_on_tie %||%
    CFG$prefer_smaller_k_on_tie,
  default = TRUE
)

PARSE_OK_REQUIRED <- as_flag(
  CFG$analysis$parse_ok_required %||%
    CFG$parse_ok_required,
  default = TRUE
)

if (!BEST_K_MODE %in% c("auto", "fixed")) {
  stop("BEST_K_MODE must be 'auto' or 'fixed'. Got: ", BEST_K_MODE, call. = FALSE)
}

valid_rules <- c("bic", "aic", "sabic", "caic", "entropy", "dbic", "hybrid")
if (!BEST_K_RULE %in% valid_rules) {
  stop(
    "BEST_K_RULE must be one of: ", paste(valid_rules, collapse = ", "),
    ". Got: ", BEST_K_RULE,
    call. = FALSE
  )
}

log_info("BEST_K_MODE              = ", BEST_K_MODE)
log_info("BEST_K_RULE              = ", BEST_K_RULE)
log_info("BEST_K_FIXED             = ", ifelse(is.na(BEST_K_FIXED), "NA", BEST_K_FIXED))
log_info("BEST_MODEL_STRUCTURE_FIX = ", ifelse(is.na(BEST_MODEL_STRUCTURE_FIXED), "NA", BEST_MODEL_STRUCTURE_FIXED))
log_info("MIN_CLASS_PROP           = ", MIN_CLASS_PROP)
log_info("MIN_CLASS_N              = ", MIN_CLASS_N)
log_info("MIN_ENTROPY_SOFT         = ", MIN_ENTROPY_SOFT)
log_info("MIN_ENTROPY_HARD         = ", MIN_ENTROPY_HARD)
log_info("PARSE_OK_REQUIRED        = ", PARSE_OK_REQUIRED)

# ------------------------------------------------------------
# 3. helper functions
# ------------------------------------------------------------
calc_effective_n <- function(weights) {
  w <- suppressWarnings(as.numeric(weights))
  w <- w[is.finite(w) & !is.na(w) & w > 0]
  if (length(w) == 0) return(NA_real_)
  (sum(w)^2) / sum(w^2)
}

calc_dbic <- function(loglik, n_par, n_eff) {
  if (any(is.na(c(loglik, n_par, n_eff))) || n_eff <= 1) return(NA_real_)
  (-2 * loglik) + log(n_eff) * n_par
}

rank_min <- function(x) rank(x, ties.method = "min", na.last = "keep")
rank_max <- function(x) rank(-x, ties.method = "min", na.last = "keep")

safe_min1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

choose_best_row <- function(df, rule) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  if (!"k" %in% names(df)) {
    if ("model_tag" %in% names(df)) {
      df$k <- extract_k_from_tag(df$model_tag)
    } else {
      df$k <- seq_len(nrow(df))
    }
  }

  if (rule == "entropy") {
    if (!"entropy" %in% names(df)) return(NULL)
    cand <- df[!is.na(df$entropy), , drop = FALSE]
    if (nrow(cand) == 0) return(NULL)
    cand <- cand[order(-cand$entropy, cand$k), , drop = FALSE]
    return(cand[1, , drop = FALSE])
  }

  if (!rule %in% names(df)) return(NULL)
  cand <- df[!is.na(df[[rule]]), , drop = FALSE]
  if (nrow(cand) == 0) return(NULL)

  cand <- cand[order(cand[[rule]], cand$k), , drop = FALSE]
  cand[1, , drop = FALSE]
}

choose_best_row_hybrid <- function(df,
                                   shortlist_delta_dbic = 10,
                                   shortlist_delta_bic = 10,
                                   shortlist_delta_sabic = 10,
                                   min_entropy_soft = 0.70,
                                   prefer_smaller_k_on_tie = TRUE) {
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

  cand <- df

  min_dbic  <- safe_min1(cand$dbic)
  min_bic   <- safe_min1(cand$bic)
  min_sabic <- safe_min1(cand$sabic)

  cand$short_dbic  <- if (is.na(min_dbic))  TRUE else cand$dbic  <= (min_dbic  + shortlist_delta_dbic)
  cand$short_bic   <- if (is.na(min_bic))   TRUE else cand$bic   <= (min_bic   + shortlist_delta_bic)
  cand$short_sabic <- if (is.na(min_sabic)) TRUE else cand$sabic <= (min_sabic + shortlist_delta_sabic)

  shortlist <- cand[cand$short_dbic & cand$short_bic & cand$short_sabic, , drop = FALSE]
  if (nrow(shortlist) == 0) shortlist <- cand

  shortlist$entropy_soft_ok <- ifelse(is.na(shortlist$entropy), FALSE, shortlist$entropy >= min_entropy_soft)

  if (any(shortlist$entropy_soft_ok, na.rm = TRUE)) {
    shortlist2 <- shortlist[shortlist$entropy_soft_ok %in% TRUE, , drop = FALSE]
  } else {
    shortlist2 <- shortlist
  }

  shortlist2$rank_dbic <- rank_min(shortlist2$dbic)
  shortlist2$rank_bic <- rank_min(shortlist2$bic)
  shortlist2$rank_sabic <- rank_min(shortlist2$sabic)
  shortlist2$rank_entropy <- rank_max(shortlist2$entropy)

  shortlist2$hybrid_score <- shortlist2$rank_dbic +
    shortlist2$rank_bic +
    shortlist2$rank_sabic +
    shortlist2$rank_entropy

  if (isTRUE(prefer_smaller_k_on_tie)) {
    shortlist2 <- shortlist2[order(shortlist2$hybrid_score, shortlist2$k, shortlist2$dbic), , drop = FALSE]
  } else {
    shortlist2 <- shortlist2[order(shortlist2$hybrid_score, shortlist2$dbic, shortlist2$k), , drop = FALSE]
  }

  shortlist2[1, , drop = FALSE]
}

# ------------------------------------------------------------
# 4. compute DBIC if needed
# ------------------------------------------------------------
wvar <- SURVEY_BUNDLE$weight_var %||% NULL

if (!is.null(wvar) && is.data.frame(ANALYSIS_DATA_SUB) && wvar %in% names(ANALYSIS_DATA_SUB)) {
  weights <- ANALYSIS_DATA_SUB[[wvar]]
} else {
  weights <- rep(1, if (is.data.frame(ANALYSIS_DATA_SUB)) nrow(ANALYSIS_DATA_SUB) else 1)
}

n_eff <- calc_effective_n(weights)
FIT_SUMMARY$n_eff <- n_eff

if (!"npar" %in% names(FIT_SUMMARY)) {
  FIT_SUMMARY$npar <- NA_real_
}

if (!"dbic" %in% names(FIT_SUMMARY) || all(is.na(FIT_SUMMARY$dbic))) {
  FIT_SUMMARY$dbic <- mapply(
    calc_dbic,
    FIT_SUMMARY$ll,
    FIT_SUMMARY$npar,
    FIT_SUMMARY$n_eff
  )
}

# ------------------------------------------------------------
# 5. build candidate table with filters
# ------------------------------------------------------------
CANDIDATES_RAW <- FIT_SUMMARY

if (!"status" %in% names(CANDIDATES_RAW)) CANDIDATES_RAW$status <- NA_character_
if (!"parse_ok" %in% names(CANDIDATES_RAW)) CANDIDATES_RAW$parse_ok <- TRUE
if (!"smallest_class_p" %in% names(CANDIDATES_RAW)) CANDIDATES_RAW$smallest_class_p <- NA_real_
if (!"smallest_class_n" %in% names(CANDIDATES_RAW)) CANDIDATES_RAW$smallest_class_n <- NA_real_
if (!"model_structure" %in% names(CANDIDATES_RAW)) {
  CANDIDATES_RAW$model_structure <- extract_model_structure_from_tag(CANDIDATES_RAW$model_tag)
}
CANDIDATES_RAW$model_structure <- tolower(as.character(CANDIDATES_RAW$model_structure))

CANDIDATES_RAW$min_class_prop <- CANDIDATES_RAW$smallest_class_p
CANDIDATES_RAW$min_class_n <- CANDIDATES_RAW$smallest_class_n


# ------------------------------------------------------------
# normalize min_class_prop scale
# - accept either proportion (0~1) or percent (0~100)
# ------------------------------------------------------------
CANDIDATES_RAW$min_class_prop <- suppressWarnings(as.numeric(CANDIDATES_RAW$min_class_prop))

idx_pct <- !is.na(CANDIDATES_RAW$min_class_prop) & CANDIDATES_RAW$min_class_prop > 1
if (any(idx_pct)) {
  CANDIDATES_RAW$min_class_prop[idx_pct] <- CANDIDATES_RAW$min_class_prop[idx_pct] / 100
}

if (all(is.na(CANDIDATES_RAW$min_class_prop))) {
  log_warn("All min_class_prop values are NA. Class proportion filter may be ineffective.")
} else {
  log_info(
    "Normalized min_class_prop = ",
    paste(signif(CANDIDATES_RAW$min_class_prop, 4), collapse = ", ")
  )
}

log_info("MIN_CLASS_PROP cutoff = ", MIN_CLASS_PROP)


CANDIDATES_RAW$pass_parse <- if (isTRUE(PARSE_OK_REQUIRED)) {
  !is.na(CANDIDATES_RAW$parse_ok) & (CANDIDATES_RAW$parse_ok %in% TRUE)
} else {
  TRUE
}

CANDIDATES_RAW$pass_status <- if ("status" %in% names(CANDIDATES_RAW)) {
  is.na(CANDIDATES_RAW$status) | CANDIDATES_RAW$status == "ok"
} else {
  TRUE
}

CANDIDATES_RAW$pass_class_prop <- ifelse(
  is.na(CANDIDATES_RAW$min_class_prop),
  FALSE,
  CANDIDATES_RAW$min_class_prop >= MIN_CLASS_PROP
)

CANDIDATES_RAW$pass_class_n <- ifelse(
  is.na(CANDIDATES_RAW$min_class_n),
  TRUE,
  CANDIDATES_RAW$min_class_n >= MIN_CLASS_N
)

CANDIDATES_RAW$pass_entropy_hard <- ifelse(
  is.na(CANDIDATES_RAW$entropy),
  TRUE,
  CANDIDATES_RAW$entropy >= MIN_ENTROPY_HARD
)

CANDIDATES_FINAL <- CANDIDATES_RAW[
  CANDIDATES_RAW$pass_parse &
    CANDIDATES_RAW$pass_status &
    CANDIDATES_RAW$pass_class_prop &
    CANDIDATES_RAW$pass_class_n &
    CANDIDATES_RAW$pass_entropy_hard,
  ,
  drop = FALSE
]

if (nrow(CANDIDATES_FINAL) == 0) {
  log_warn("No candidates survived strict filters. Falling back to parse/status/class_prop filters only.")
  CANDIDATES_FINAL <- CANDIDATES_RAW[
    CANDIDATES_RAW$pass_parse &
      CANDIDATES_RAW$pass_status &
      CANDIDATES_RAW$pass_class_prop,
    ,
    drop = FALSE
  ]
}

if (nrow(CANDIDATES_FINAL) == 0) {
  log_warn("No candidates survived fallback filters. Falling back to raw candidates.")
  CANDIDATES_FINAL <- CANDIDATES_RAW
}

log_info("Candidate table prepared: raw = ", nrow(CANDIDATES_RAW), ", final = ", nrow(CANDIDATES_FINAL))
log_info("k(raw)                   = ", paste(CANDIDATES_RAW$k, collapse = ", "))
log_info("model_structure(raw)     = ", paste(CANDIDATES_RAW$model_structure, collapse = ", "))
log_info("min_class_prop(raw)      = ", paste(signif(CANDIDATES_RAW$min_class_prop, 6), collapse = ", "))
log_info("pass_class_prop(raw)     = ", paste(CANDIDATES_RAW$pass_class_prop, collapse = ", "))

# ------------------------------------------------------------
# 6. choose best solution
# ------------------------------------------------------------
BEST_MODEL_ROW <- NULL
BEST_K <- NA_integer_
BEST_TAG <- NA_character_
BEST_MODEL_STRUCTURE <- NA_character_
BEST_REASON <- NA_character_
BEST_REASON_DETAIL <- NA_character_

if (BEST_K_MODE == "fixed") {
  if (is.na(BEST_K_FIXED)) {
    stop("BEST_K_MODE = 'fixed' but BEST_K_FIXED is NA.", call. = FALSE)
  }

  hit <- CANDIDATES_RAW[CANDIDATES_RAW$k == BEST_K_FIXED, , drop = FALSE]

  if (!is.na(BEST_MODEL_STRUCTURE_FIXED) && nzchar(BEST_MODEL_STRUCTURE_FIXED)) {
    hit <- hit[tolower(hit$model_structure) == BEST_MODEL_STRUCTURE_FIXED, , drop = FALSE]
  }

  if (nrow(hit) == 0) {
    stop("Fixed best solution not found among model candidates.", call. = FALSE)
  }

  if (nrow(hit) > 1) {
    if (BEST_K_RULE == "hybrid") {
      BEST_MODEL_ROW <- choose_best_row_hybrid(
        hit,
        shortlist_delta_dbic = SHORTLIST_DELTA_DBIC,
        shortlist_delta_bic = SHORTLIST_DELTA_BIC,
        shortlist_delta_sabic = SHORTLIST_DELTA_SABIC,
        min_entropy_soft = MIN_ENTROPY_SOFT,
        prefer_smaller_k_on_tie = PREFER_SMALLER_K_ON_TIE
      )
    } else {
      BEST_MODEL_ROW <- choose_best_row(hit, BEST_K_RULE)
    }
  }

  if (is.null(BEST_MODEL_ROW)) BEST_MODEL_ROW <- hit[1, , drop = FALSE]

  BEST_K <- as.integer(BEST_MODEL_ROW$k[1])
  BEST_TAG <- as.character(BEST_MODEL_ROW$model_tag[1])
  BEST_MODEL_STRUCTURE <- as.character(BEST_MODEL_ROW$model_structure[1])
  BEST_REASON <- "fixed:user_override"
  BEST_REASON_DETAIL <- paste0(
    "User fixed k = ", BEST_K_FIXED,
    if (!is.na(BEST_MODEL_STRUCTURE_FIXED) && nzchar(BEST_MODEL_STRUCTURE_FIXED)) {
      paste0(", model_structure = ", BEST_MODEL_STRUCTURE_FIXED)
    } else {
      ""
    }
  )

} else {
  if (BEST_K_RULE == "hybrid") {
    BEST_MODEL_ROW <- choose_best_row_hybrid(
      CANDIDATES_FINAL,
      shortlist_delta_dbic = SHORTLIST_DELTA_DBIC,
      shortlist_delta_bic = SHORTLIST_DELTA_BIC,
      shortlist_delta_sabic = SHORTLIST_DELTA_SABIC,
      min_entropy_soft = MIN_ENTROPY_SOFT,
      prefer_smaller_k_on_tie = PREFER_SMALLER_K_ON_TIE
    )

    if (is.null(BEST_MODEL_ROW)) {
      stop("Hybrid selection failed.", call. = FALSE)
    }

    BEST_K <- as.integer(BEST_MODEL_ROW$k[1])
    BEST_TAG <- as.character(BEST_MODEL_ROW$model_tag[1])
    BEST_MODEL_STRUCTURE <- as.character(BEST_MODEL_ROW$model_structure[1])
    BEST_REASON <- "auto:hybrid"
    BEST_REASON_DETAIL <- paste0(
      "Filtered by parse/status/class size; shortlisted by DBIC/BIC/SABIC deltas; ",
      "final choice favored higher entropy",
      if (isTRUE(PREFER_SMALLER_K_ON_TIE)) " and smaller k on ties." else "."
    )

  } else {
    BEST_MODEL_ROW <- choose_best_row(CANDIDATES_FINAL, BEST_K_RULE)

    if (is.null(BEST_MODEL_ROW)) {
      log_warn("Rule-based selection on filtered candidates failed. Falling back to raw candidates.")
      BEST_MODEL_ROW <- choose_best_row(CANDIDATES_RAW, BEST_K_RULE)
    }

    if (is.null(BEST_MODEL_ROW)) {
      stop("Failed to choose best model row using rule: ", BEST_K_RULE, call. = FALSE)
    }

    BEST_K <- as.integer(BEST_MODEL_ROW$k[1])
    BEST_TAG <- as.character(BEST_MODEL_ROW$model_tag[1])
    BEST_MODEL_STRUCTURE <- as.character(BEST_MODEL_ROW$model_structure[1])
    BEST_REASON <- paste0("auto:", BEST_K_RULE)
    BEST_REASON_DETAIL <- paste0("Selected by minimum ", BEST_K_RULE, ".")
  }
}

# ------------------------------------------------------------
# 7. annotate candidate table
# ------------------------------------------------------------
if (is.data.frame(CANDIDATES_RAW) && nrow(CANDIDATES_RAW) > 0) {
  CANDIDATES_RAW$is_selected <- CANDIDATES_RAW$model_tag == BEST_TAG
}
if (is.data.frame(CANDIDATES_FINAL) && nrow(CANDIDATES_FINAL) > 0) {
  CANDIDATES_FINAL$is_selected <- CANDIDATES_FINAL$model_tag == BEST_TAG
}

# ------------------------------------------------------------
# 8. build summary
# ------------------------------------------------------------
BEST_K_SUMMARY <- list(
  best_k                = BEST_K,
  best_tag              = BEST_TAG,
  best_model_structure  = BEST_MODEL_STRUCTURE,
  best_rule             = BEST_K_RULE,
  best_mode             = BEST_K_MODE,
  best_k_fixed          = BEST_K_FIXED,
  best_model_structure_fixed = BEST_MODEL_STRUCTURE_FIXED,
  best_reason           = BEST_REASON,
  best_reason_detail    = BEST_REASON_DETAIL,
  min_class_prop        = MIN_CLASS_PROP,
  min_class_n           = MIN_CLASS_N,
  min_entropy_soft      = MIN_ENTROPY_SOFT,
  min_entropy_hard      = MIN_ENTROPY_HARD,
  parse_ok_required     = PARSE_OK_REQUIRED,
  n_candidates_raw      = if (is.data.frame(CANDIDATES_RAW)) nrow(CANDIDATES_RAW) else 0L,
  n_candidates_final    = if (is.data.frame(CANDIDATES_FINAL)) nrow(CANDIDATES_FINAL) else 0L,
  mixture_type          = SETTINGS_SUMMARY$mixture_type %||% SETTINGS_SUMMARY$MIXTURE_TYPE %||% NA_character_,
  created_at            = Sys.time()
)

BEST_K_SUMMARY$BEST_K <- BEST_K
BEST_K_SUMMARY$BEST_TAG <- BEST_TAG
BEST_K_SUMMARY$BEST_MODEL_STRUCTURE <- BEST_MODEL_STRUCTURE
BEST_K_SUMMARY$BEST_K_RULE <- BEST_K_RULE
BEST_K_SUMMARY$BEST_K_MODE <- BEST_K_MODE

# ------------------------------------------------------------
# 9. save outputs
# ------------------------------------------------------------
log_info("Saving best-solution outputs ...")

save_named_rds_list(
  list(
    BEST_MODEL_ROW         = BEST_MODEL_ROW,
    BEST_K_SUMMARY         = BEST_K_SUMMARY,
    SELECT_BEST_K_SUMMARY  = BEST_K_SUMMARY,
    MODEL_CANDIDATES_RAW   = CANDIDATES_RAW,
    MODEL_CANDIDATES_FINAL = CANDIDATES_FINAL,
    MODEL_CANDIDATES       = CANDIDATES_FINAL
  ),
  dir_rds = DIR_RDS
)

# ------------------------------------------------------------
# 10. finish
# ------------------------------------------------------------
elapsed_sec <- round(as.numeric(difftime(Sys.time(), T0_SELECT_BEST_K, units = "secs")), 2)

log_info("04a_select_best_k.R completed.")
log_info("BEST_K               = ", BEST_K)
log_info("BEST_MODEL_STRUCTURE = ", BEST_MODEL_STRUCTURE)
log_info("BEST_K_RULE          = ", BEST_K_RULE)
log_info("BEST_TAG             = ", BEST_TAG)

best_dbic_val <- if (is.data.frame(BEST_MODEL_ROW) && "dbic" %in% names(BEST_MODEL_ROW)) {
  BEST_MODEL_ROW$dbic[1]
} else {
  NA_real_
}

best_bic_val <- if (is.data.frame(BEST_MODEL_ROW) && "bic" %in% names(BEST_MODEL_ROW)) {
  BEST_MODEL_ROW$bic[1]
} else {
  NA_real_
}

log_info("BEST_DBIC            = ", ifelse(is.na(best_dbic_val), "NA", formatC(best_dbic_val, format = "f", digits = 3)))
log_info("BEST_BIC             = ", ifelse(is.na(best_bic_val), "NA", formatC(best_bic_val, format = "f", digits = 3)))
log_info("n(candidates raw)    = ", if (is.data.frame(CANDIDATES_RAW)) nrow(CANDIDATES_RAW) else 0L)
log_info("n(candidates final)  = ", if (is.data.frame(CANDIDATES_FINAL)) nrow(CANDIDATES_FINAL) else 0L)
log_info("elapsed              = ", elapsed_sec, " sec")

log_step_end("select_best_k", elapsed_sec, ok = TRUE)