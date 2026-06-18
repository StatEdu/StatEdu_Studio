T0_COLLECT <- Sys.time()

log_step_start("ESTIMATION_COLLECT", "04c_estimation_collect.R")
log_info("Collecting Mplus latent transition outputs ...")

`%||%` <- function(x, y) if (is.null(x)) y else x

safe_read_lines <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !file.exists(path)) return(character(0))
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
}

extract_value_with_fallback <- function(pattern, txt, up, max_lookahead = 5L) {
  idx <- grep(pattern, up, perl = TRUE)
  if (length(idx) == 0) return(NA_real_)
  for (j in seq.int(idx[1], min(length(txt), idx[1] + max_lookahead))) {
    nums <- regmatches(txt[j], gregexpr("-?[0-9]+\\.?[0-9]*", txt[j], perl = TRUE))[[1]]
    vals <- suppressWarnings(as.numeric(nums))
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) return(vals[length(vals)])
  }
  NA_real_
}

parse_mplus_fit <- function(out_file, model_tag = NA_character_) {
  txt <- safe_read_lines(out_file)
  up <- toupper(txt)
  data.frame(
    model_tag = model_tag,
    out_file = out_file,
    parse_ok = length(txt) > 0,
    ll = extract_value_with_fallback("LOGLIKELIHOOD", txt, up),
    aic = extract_value_with_fallback("AIC", txt, up),
    bic = extract_value_with_fallback("BIC", txt, up),
    sabic = extract_value_with_fallback("SAMPLE-SIZE ADJUSTED BIC|SABIC|ADJUSTED BIC", txt, up),
    entropy = extract_value_with_fallback("ENTROPY", txt, up),
    stringsAsFactors = FALSE
  )
}

LTA_SPEC <- load_step_rds("LTA_SPEC", dir_rds = DIR_RDS, required = TRUE)
ESTIMATION_REGISTRY <- load_step_rds("ESTIMATION_REGISTRY", dir_rds = DIR_RDS, required = TRUE)
ESTIMATION_RUN_RESULTS <- load_step_rds("ESTIMATION_RUN_RESULTS", dir_rds = DIR_RDS, required = TRUE)

FIT_SUMMARY <- parse_mplus_fit(ESTIMATION_RUN_RESULTS$out_file[1], ESTIMATION_REGISTRY$model_tag[1])
MEASUREMENT_MANIFEST <- do.call(
  rbind,
  lapply(seq_along(LTA_SPEC$wave_order), function(i) {
    wave_i <- LTA_SPEC$wave_order[i]
    vars_i <- LTA_SPEC$indicators_by_wave[[wave_i]] %||% character(0)
    data.frame(
      wave = wave_i,
      indicator_order = seq_along(vars_i),
      indicator = vars_i,
      stringsAsFactors = FALSE
    )
  })
)

ESTIMATION_COLLECT_SUMMARY <- list(
  parse_ok = FIT_SUMMARY$parse_ok[1] %||% FALSE,
  measurement_mode = LTA_SPEC$measurement_mode,
  n_classes = LTA_SPEC$n_classes,
  n_indicators = nrow(MEASUREMENT_MANIFEST)
)

save_named_rds_list(
  list(
    FIT_SUMMARY = FIT_SUMMARY,
    MEASUREMENT_MANIFEST = MEASUREMENT_MANIFEST,
    ESTIMATION_COLLECT_SUMMARY = ESTIMATION_COLLECT_SUMMARY
  ),
  dir_rds = DIR_RDS
)

log_step_end("estimation_collect", round(as.numeric(difftime(Sys.time(), T0_COLLECT, units = "secs")), 2), ok = TRUE)
