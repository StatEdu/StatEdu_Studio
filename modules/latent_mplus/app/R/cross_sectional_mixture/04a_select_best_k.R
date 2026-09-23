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

if (!exists("mixture_annotate_eligibility") || !exists("mixture_select_candidate")) {
  stop("Mixture selection core is unavailable. Make sure 15_mixture_selection_core.R is sourced before 04a.", call. = FALSE)
}

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
  mixture_extract_k(x)
}

extract_model_structure_from_tag <- function(x) {
  mixture_extract_structure(x)
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
    if (!is.null(CFG$estimation[["best_k"]]) && !is.na(CFG$estimation[["best_k"]])) "fixed" else "auto"
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
    CFG$estimation[["best_k"]] %||%
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
    0.03
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

# Parsing, convergence, optimum replication, admissibility, and class-size
# checks are non-overridable scientific-computation gates.
PARSE_OK_REQUIRED <- TRUE

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
  FIT_SUMMARY$dbic <- mixture_dbic(FIT_SUMMARY$ll, FIT_SUMMARY$npar, FIT_SUMMARY$n_eff)
}
if (!"caic" %in% names(FIT_SUMMARY) || all(is.na(FIT_SUMMARY$caic))) {
  FIT_SUMMARY$caic <- mixture_caic(FIT_SUMMARY$ll, FIT_SUMMARY$npar, FIT_SUMMARY$n_eff)
}

# ------------------------------------------------------------
# 5. build candidate table with filters
# ------------------------------------------------------------
CANDIDATES_RAW <- mixture_annotate_eligibility(
  FIT_SUMMARY,
  rule = BEST_K_RULE,
  min_class_prop = MIN_CLASS_PROP,
  min_class_n = MIN_CLASS_N,
  min_entropy_hard = MIN_ENTROPY_HARD,
  parse_ok_required = TRUE,
  status_ok_required = TRUE,
  convergence_required = TRUE,
  replication_required = TRUE,
  admissibility_required = TRUE,
  class_prop_required = TRUE,
  class_n_required = TRUE
)
CANDIDATES_FINAL <- CANDIDATES_RAW[
  !is.na(CANDIDATES_RAW$eligible) & CANDIDATES_RAW$eligible,
  ,
  drop = FALSE
]

log_info("Candidate table prepared: raw = ", nrow(CANDIDATES_RAW), ", final = ", nrow(CANDIDATES_FINAL))
log_info("k(raw)                   = ", paste(CANDIDATES_RAW$k, collapse = ", "))
log_info("model_structure(raw)     = ", paste(CANDIDATES_RAW$model_structure, collapse = ", "))
log_info("min_class_prop(raw)      = ", paste(signif(CANDIDATES_RAW$min_class_prop, 6), collapse = ", "))
log_info("pass_class_prop(raw)     = ", paste(CANDIDATES_RAW$pass_class_prop, collapse = ", "))
if (any(!CANDIDATES_RAW$eligible)) {
  rejected <- CANDIDATES_RAW[!CANDIDATES_RAW$eligible, c("model_tag", "failure_reasons"), drop = FALSE]
  for (i in seq_len(nrow(rejected))) {
    log_warn("Candidate excluded: ", rejected$model_tag[i], " / ", rejected$failure_reasons[i])
  }
}

# ------------------------------------------------------------
# 6. choose best solution
# ------------------------------------------------------------
BEST_MODEL_ROW <- NULL
BEST_K <- NA_integer_
BEST_TAG <- NA_character_
BEST_MODEL_STRUCTURE <- NA_character_
BEST_REASON <- NA_character_
BEST_REASON_DETAIL <- NA_character_

SELECTION_RESULT <- tryCatch(
  mixture_select_candidate(
    CANDIDATES_RAW,
    mode = BEST_K_MODE,
    rule = BEST_K_RULE,
    fixed_k = BEST_K_FIXED,
    fixed_structure = BEST_MODEL_STRUCTURE_FIXED,
    shortlist_delta_dbic = SHORTLIST_DELTA_DBIC,
    shortlist_delta_bic = SHORTLIST_DELTA_BIC,
    shortlist_delta_sabic = SHORTLIST_DELTA_SABIC,
    min_entropy_soft = MIN_ENTROPY_SOFT,
    prefer_smaller_k_on_tie = PREFER_SMALLER_K_ON_TIE
  ),
  mixture_selection_error = function(e) {
    CANDIDATES_RAW$is_selected <- FALSE
    CANDIDATES_FINAL <- CANDIDATES_RAW[
      !is.na(CANDIDATES_RAW$eligible) & CANDIDATES_RAW$eligible,
      , drop = FALSE
    ]
    failure_summary <- list(
      status = "failed",
      error_class = class(e)[1],
      message = conditionMessage(e),
      rule = BEST_K_RULE,
      mode = BEST_K_MODE,
      n_candidates_raw = nrow(CANDIDATES_RAW),
      n_candidates_eligible = nrow(CANDIDATES_FINAL),
      created_at = Sys.time()
    )
    save_named_rds_list(
      list(
        MODEL_CANDIDATES_RAW = CANDIDATES_RAW,
        MODEL_CANDIDATES_FINAL = CANDIDATES_FINAL,
        SELECTION_FAILURE_SUMMARY = failure_summary
      ),
      dir_rds = DIR_RDS
    )
    log_error("Mixture selection stopped: ", conditionMessage(e))
    stop(e)
  }
)

BEST_MODEL_ROW <- SELECTION_RESULT$selected
CANDIDATES_RAW <- SELECTION_RESULT$candidates_raw
CANDIDATES_FINAL <- SELECTION_RESULT$candidates_eligible
BEST_K <- as.integer(BEST_MODEL_ROW$k[1])
BEST_TAG <- as.character(BEST_MODEL_ROW$model_tag[1])
BEST_MODEL_STRUCTURE <- as.character(BEST_MODEL_ROW$model_structure[1])
BEST_REASON <- as.character(SELECTION_RESULT$reason)
BEST_REASON_DETAIL <- if (BEST_K_MODE == "fixed") {
  paste0(
    "User-specified candidate passed all computation, optimum-replication, admissibility, class-size, and metric gates: k = ",
    BEST_K,
    if (!is.na(BEST_MODEL_STRUCTURE) && nzchar(BEST_MODEL_STRUCTURE)) paste0(", model_structure = ", BEST_MODEL_STRUCTURE) else "",
    "."
  )
} else if (BEST_K_RULE == "hybrid") {
  paste0(
    "Only eligible candidates were considered; shortlisted by DBIC/BIC/SABIC deltas and ranked with entropy",
    if (isTRUE(PREFER_SMALLER_K_ON_TIE)) ", with smaller k preferred on exact ties." else "."
  )
} else {
  paste0("Only eligible candidates were considered; selected by ", BEST_K_RULE, ".")
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
  n_candidates_excluded = if (is.data.frame(CANDIDATES_RAW)) sum(!CANDIDATES_RAW$eligible, na.rm = TRUE) else 0L,
  selected_eligible     = isTRUE(BEST_MODEL_ROW$eligible[1]),
  selected_terminated_normally = isTRUE(BEST_MODEL_ROW$terminated_normally[1]),
  selected_loglik_replicated = BEST_MODEL_ROW$loglik_replicated[1],
  selected_admissible   = isTRUE(BEST_MODEL_ROW$admissible[1]),
  quality_gate          = "fail_closed",
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
