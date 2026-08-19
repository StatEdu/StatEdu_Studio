source(file.path("scripts", "validate_cfa_common.R"), encoding = "UTF-8")

stopifnot(requireNamespace("lavaan", quietly = TRUE))

set.seed(20260814)
n <- 180L
factor_score <- stats::rnorm(n)
continuous <- data.frame(
  x1 = .8 * factor_score + stats::rnorm(n, sd = .6),
  x2 = .7 * factor_score + stats::rnorm(n, sd = .7),
  x3 = .9 * factor_score + stats::rnorm(n, sd = .5)
)
second_factor_score <- .35 * factor_score + sqrt(1 - .35^2) * stats::rnorm(n)
continuous$y1 <- .75 * second_factor_score + stats::rnorm(n, sd = .65)
continuous$y2 <- .70 * second_factor_score + stats::rnorm(n, sd = .70)
continuous$y3 <- .85 * second_factor_score + stats::rnorm(n, sd = .55)

single_fit <- lavaan::cfa("eta1 =~ x1 + x2 + x3", data = continuous, estimator = "ML", auto.cov.lv.x = FALSE)
reliability_progress_events <- list()
reliability_bootstrap <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", continuous, reps = 12L, seed = 20260814L,
  estimator = "ML", missing = "listwise", original_fit = single_fit,
  progress = function(done, total, valid) {
    reliability_progress_events[[length(reliability_progress_events) + 1L]] <<- c(done = done, total = total, valid = valid)
  }
)
reliability_cancel_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 2L, seed = 20260814L,
    estimator = "ML", missing = "listwise", original_fit = single_fit,
    cancel = function() TRUE
  )
  ""
}, error = conditionMessage)
stopifnot(
  nrow(reliability_bootstrap) == 4L,
  all(reliability_bootstrap[["CI method"]] == "Bias-corrected (BC)"),
  all(reliability_bootstrap[["Requested replicates"]] == 12L),
  all(reliability_bootstrap[["Valid replicates"]] > 0L),
  reliability_progress_events[[1L]][["done"]] == 0L,
  tail(reliability_progress_events, 1L)[[1L]][["done"]] == 12L,
  grepl("canceled", reliability_cancel_error, fixed = TRUE)
)

small_fit <- lavaan::cfa("eta1 =~ x1 + x2 + x3", data = continuous[seq_len(90L), , drop = FALSE], auto.cov.lv.x = FALSE)
reliability_bca <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", continuous[seq_len(90L), , drop = FALSE],
  reps = 12L, seed = 20260815L, estimator = "ML", missing = "listwise",
  original_fit = small_fit, ci_method = "bca"
)
stopifnot(
  nrow(reliability_bca) == 4L,
  all(reliability_bca[["CI method"]] %in% c("BCa", "BCa unavailable"))
)

cfa_job_progress <- tempfile(fileext = ".rds")
cfa_job_value <- structural_canvas_cfa_bootstrap_job_value(list(
  fit = single_fit, syntax = "eta1 =~ x1 + x2 + x3", data = continuous,
  estimator = "ML", missing = "listwise", std_lv = FALSE, ordered = character(0),
  validity_formula = "standardized", reliability_bootstrap = 4L,
  reliability_seed = 20260814L, reliability_ci_method = "bias_corrected",
  bollen_stine_bootstrap = 0L, bollen_stine_seed = 20260814L,
  htmt_bootstrap = 0L, htmt_seed = 20260814L, htmt_threshold = .85,
  htmt_ci_method = "bias_corrected"
), cfa_job_progress)
cfa_job_status <- readRDS(cfa_job_progress)
unlink(cfa_job_progress)
stopifnot(
  nrow(cfa_job_value$reliability_bootstrap_result) == 4L,
  identical(cfa_job_status$phase, "complete"),
  identical(cfa_job_status$completed, 4L),
  identical(cfa_job_status$total, 4L)
)
if (requireNamespace("callr", quietly = TRUE)) {
  cancellable_job <- structural_canvas_start_cfa_bootstrap_job(list(
    fit = single_fit, syntax = "eta1 =~ x1 + x2 + x3", analysis_data = continuous,
    estimator = "ML", missing = "listwise", std_lv = FALSE, ordered = character(0),
    validity_formula = "standardized", reliability_bootstrap = 50L,
    reliability_seed = 20260814L, reliability_ci_method = "bias_corrected",
    bollen_stine_bootstrap = 0L, bollen_stine_seed = 20260814L,
    htmt_bootstrap = 0L, htmt_seed = 20260814L, htmt_threshold = .85,
    htmt_ci_method = "bias_corrected"
  ))
  cancellable_directory <- cancellable_job$directory
  stopifnot(cancellable_job$process$is_alive(), file.exists(cancellable_job$progress_file))
  cancellable_job$process$kill()
  structural_canvas_cleanup_cfa_bootstrap_job(cancellable_job)
  stopifnot(!dir.exists(cancellable_directory))
}

ordinal <- as.data.frame(lapply(continuous[c("x1", "x2", "x3")], function(value) {
  ordered(cut(value, breaks = stats::quantile(value, probs = seq(0, 1, .2)), include.lowest = TRUE))
}))
ordered_names <- names(ordinal)
theta_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3", data = ordinal, estimator = "WLSMV",
  missing = "pairwise", ordered = ordered_names, parameterization = "theta",
  auto.cov.lv.x = FALSE
)
theta_reliability_bootstrap <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", ordinal, reps = 4L, estimator = "WLSMV",
  missing = "pairwise", ordered = ordered_names, original_fit = theta_fit
)
stopifnot(
  identical(lavaan::lavInspect(theta_fit, "options")$parameterization, "theta"),
  nrow(theta_reliability_bootstrap) == 4L,
  all(theta_reliability_bootstrap[["Requested replicates"]] == 4L)
)

htmt_progress_events <- list()
htmt_bootstrap <- structural_canvas_htmt_bootstrap(
  continuous, list(eta1 = c("x1", "x2", "x3"), eta2 = c("y1", "y2", "y3")),
  reps = 20L, confidence = .95, seed = 20260816L,
  progress = function(done, total, valid) {
    htmt_progress_events[[length(htmt_progress_events) + 1L]] <<- c(done = done, total = total, valid = valid)
  }
)
htmt_cancel_error <- tryCatch({
  structural_canvas_htmt_bootstrap(
    continuous, list(eta1 = c("x1", "x2", "x3"), eta2 = c("y1", "y2", "y3")),
    reps = 2L, seed = 20260816L, cancel = function() TRUE
  )
  ""
}, error = conditionMessage)
stopifnot(
  nrow(htmt_bootstrap) == 1L,
  htmt_bootstrap[["Requested replicates"]][[1L]] == 20L,
  htmt_progress_events[[1L]][["done"]] == 0L,
  tail(htmt_progress_events, 1L)[[1L]][["done"]] == 20L,
  grepl("canceled", htmt_cancel_error, fixed = TRUE)
)

two_factor_fit <- lavaan::cfa(
  "eta1 =~ x1 + x2 + x3\neta2 =~ y1 + y2 + y3\neta1 ~~ eta2",
  data = continuous, estimator = "ML", auto.cov.lv.x = FALSE
)
bollen_progress_events <- list()
bollen_stine <- structural_canvas_bollen_stine(
  two_factor_fit, reps = 8L, seed = 20260817L,
  progress = function(done, total, valid) {
    bollen_progress_events[[length(bollen_progress_events) + 1L]] <<- c(done = done, total = total, valid = valid)
  }
)
bollen_cancel_error <- tryCatch({
  structural_canvas_bollen_stine(two_factor_fit, reps = 2L, seed = 20260817L, cancel = function() TRUE)
  ""
}, error = conditionMessage)
stopifnot(
  nrow(bollen_stine) == 1L,
  bollen_stine[["Requested replicates"]][[1L]] == 8L,
  bollen_progress_events[[1L]][["done"]] == 0L,
  tail(bollen_progress_events, 1L)[[1L]][["done"]] == 8L,
  tail(bollen_progress_events, 1L)[[1L]][["valid"]] == bollen_stine[["Valid replicates"]][[1L]],
  grepl("canceled", bollen_cancel_error, fixed = TRUE)
)

cat("CFA bootstrap validations passed.\n")
