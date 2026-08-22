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
public_reliability <- structural_canvas_reliability_estimates(single_fit, "standardized")
old_isolated_worker <- getOption("statedu.isolated_lavaan_bootstrap_worker")
options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
fast_reliability <- structural_canvas_reliability_estimates_bootstrap_fast(single_fit, "standardized")
options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker)
stopifnot(isTRUE(all.equal(
  public_reliability, fast_reliability,
  tolerance = 1e-12, check.attributes = TRUE
)))

# Reusable-worker callback results have three states. Only a trusted strict-gate
# rejection is final; NULL/short/malformed/transport failures are unknown and
# must retry the identical master-sampled frame in original replicate order.
callback_fixture <- data.frame(Factor = "eta1", AVE = .5, CR = .7, Alpha = .6, Omega = .7)
callback_valid <- structural_canvas_reliability_bootstrap_callback_result(
  list(valid = TRUE, screening_path = "fused_0_7_2"),
  function() callback_fixture
)
callback_invalid <- structural_canvas_reliability_bootstrap_callback_result(
  list(valid = FALSE, screening_path = "public_fallback"),
  function() stop("must not run")
)
callback_unknown <- structural_canvas_reliability_bootstrap_callback_result(
  list(valid = FALSE, screening_path = "error"),
  function() stop("must not run")
)
whole_call_error <- structural_canvas_reliability_bootstrap_fast_call(
  function() stop("injected CFA lavaanList transport failure"), 5L
)
wrong_call_class <- structural_canvas_reliability_bootstrap_fast_call(
  function() structure(list(funList = list()), class = "lavaanList"), 5L
)
retry_frames <- lapply(seq_len(5L), function(index) data.frame(master_index = index))
retry_seen <- integer(0)
resolved_items <- structural_canvas_reliability_bootstrap_resolve_fast_items(
  list(
    callback_valid,
    callback_invalid,
    NULL,
    simpleError("injected per-item transport failure")
    # The fifth entry is intentionally short/missing.
  ),
  retry_frames,
  function(frame, index) {
    retry_seen <<- c(retry_seen, frame$master_index[[1L]])
    data.frame(master_index = frame$master_index[[1L]], retry_position = index)
  }
)
stopifnot(
  identical(callback_valid$status, "valid"),
  identical(callback_valid$value, callback_fixture),
  identical(callback_invalid$status, "known_invalid"),
  identical(callback_unknown$status, "unknown"),
  is.null(whole_call_error),
  is.null(wrong_call_class),
  identical(resolved_items$retry_indices, 3:5),
  identical(retry_seen, 3:5),
  identical(resolved_items$values[[1L]], callback_fixture),
  is.null(resolved_items$values[[2L]]),
  identical(
    vapply(resolved_items$values[3:5], function(value) value$master_index[[1L]], integer(1)),
    3:5
  )
)

# The isolated worker's fused admissibility screen and reliability extractor
# must match the public scientific path on the same deterministic resamples.
set.seed(20260814L)
reliability_equivalence_frames <- replicate(
  8L,
  continuous[sample.int(nrow(continuous), nrow(continuous), replace = TRUE), , drop = FALSE],
  simplify = FALSE
)
reliability_equivalence <- lapply(reliability_equivalence_frames, function(frame) {
  fit <- lavaan::cfa("eta1 =~ x1 + x2 + x3", data = frame, estimator = "ML", auto.cov.lv.x = FALSE)
  public_valid <- isTRUE(structural_canvas_fit_admissibility(fit)$admissible)
  options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
  on.exit(options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker), add = TRUE)
  fast_valid <- isTRUE(structural_canvas_effect_bootstrap_extract_fit(
    fit, character(0), list(), as.numeric(lavaan::fitMeasures(single_fit, "df")[[1L]])
  )$valid)
  fast_value <- if (fast_valid) structural_canvas_reliability_estimates_bootstrap_fast(fit) else NULL
  options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker)
  public_value <- if (public_valid) structural_canvas_reliability_estimates(fit) else NULL
  list(public_valid = public_valid, fast_valid = fast_valid, public = public_value, fast = fast_value)
})
stopifnot(
  all(vapply(reliability_equivalence, function(item) identical(item$public_valid, item$fast_valid), logical(1))),
  all(vapply(reliability_equivalence, function(item) {
    !item$public_valid || isTRUE(all.equal(item$public, item$fast, tolerance = 1e-12, check.attributes = TRUE))
  }, logical(1)))
)

# The fast pool must retain the full-SE vcov gate.  This deterministic edge
# object passes the early theta/cov.lv screen but fails both the public strict
# gate and the full fused gate after its parameter covariance is made indefinite.
vcov_edge_fit <- single_fit
vcov_edge_fit@vcov$vcov[1L, 1L] <- -1
vcov_edge_df <- as.numeric(lavaan::fitMeasures(single_fit, "df")[[1L]])
stopifnot(
  !isTRUE(structural_canvas_fit_admissibility(vcov_edge_fit)$admissible),
  isTRUE(structural_canvas_effect_bootstrap_extract_fit(
    vcov_edge_fit, character(0), list(), vcov_edge_df, screen_only = TRUE
  )$valid),
  !isTRUE(structural_canvas_effect_bootstrap_extract_fit(
    vcov_edge_fit, character(0), list(), vcov_edge_df
  )$valid)
)

# Generate indices once in the master process, then prove that the legacy
# public path, one-worker fused path, and reusable PSOCK path return the same
# admissibility mask and AVE/CR/Alpha/Omega draws in replicate order.
reliability_draw_mask <- function(value) vapply(value$estimates, function(item) {
  is.data.frame(item) && nrow(item) > 0L
}, logical(1))
reliability_draws_equal <- function(left, right) all(vapply(seq_along(left$estimates), function(index) {
  lhs <- left$estimates[[index]]
  rhs <- right$estimates[[index]]
  if (is.null(lhs) || is.null(rhs)) return(identical(is.null(lhs), is.null(rhs)))
  isTRUE(all.equal(lhs, rhs, tolerance = 1e-12, check.attributes = TRUE))
}, logical(1)))
options(statedu.isolated_lavaan_bootstrap_worker = FALSE)
legacy_draws <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", continuous, reps = 6L, seed = 20260814L,
  estimator = "ML", missing = "listwise", original_fit = single_fit,
  workers = 1L, return_draws = TRUE
)
options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
reliability_metadata_state <- structural_canvas_lavaan_worker_metadata_fast_path_install()
fast_serial_draws <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", continuous, reps = 6L, seed = 20260814L,
  estimator = "ML", missing = "listwise", original_fit = single_fit,
  workers = 1L, return_draws = TRUE
)
parallel_progress <- list()
fast_parallel_draws <- structural_canvas_reliability_bootstrap(
  "eta1 =~ x1 + x2 + x3", continuous, reps = 6L, seed = 20260814L,
  estimator = "ML", missing = "listwise", original_fit = single_fit,
  workers = 2L, chunk_size = 4L, return_draws = TRUE,
  progress = function(done, total, valid) {
    parallel_progress[[length(parallel_progress) + 1L]] <<- c(done = done, total = total, valid = valid)
  }
)
reliability_metadata_state$restore()
options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker)
parallel_progress_matrix <- do.call(rbind, parallel_progress)
parallel_draw_mask <- reliability_draw_mask(fast_parallel_draws)
first_completed_progress <- which(parallel_progress_matrix[, "done"] > 0L)[[1L]]
invalid_worker_override <- tryCatch({
  structural_canvas_reliability_bootstrap_workers("0", 1000L)
  ""
}, error = conditionMessage)
stopifnot(
  identical(legacy_draws$sample_indices, fast_serial_draws$sample_indices),
  identical(fast_serial_draws$sample_indices, fast_parallel_draws$sample_indices),
  identical(reliability_draw_mask(legacy_draws), reliability_draw_mask(fast_serial_draws)),
  identical(reliability_draw_mask(fast_serial_draws), reliability_draw_mask(fast_parallel_draws)),
  reliability_draws_equal(legacy_draws, fast_serial_draws),
  reliability_draws_equal(fast_serial_draws, fast_parallel_draws),
  isTRUE(all.equal(fast_serial_draws$summary, fast_parallel_draws$summary, tolerance = 1e-12, check.attributes = FALSE)),
  identical(fast_serial_draws$timings$workers, 1L),
  identical(fast_parallel_draws$timings$workers, 2L),
  identical(fast_parallel_draws$timings$fast_chunk_fallbacks, 0L),
  identical(fast_parallel_draws$timings$fast_item_retries, 0L),
  all(diff(parallel_progress_matrix[, "done"]) >= 0L),
  all(diff(parallel_progress_matrix[, "valid"]) >= 0L),
  length(unique(parallel_progress_matrix[, "total"])) == 1L,
  parallel_progress_matrix[first_completed_progress, "done"] == 1L,
  parallel_progress_matrix[first_completed_progress, "valid"] == as.integer(parallel_draw_mask[[1L]]),
  tail(parallel_progress_matrix[, "done"], 1L) == 6L,
  tail(parallel_progress_matrix[, "valid"], 1L) == sum(parallel_draw_mask),
  grepl("positive integer", invalid_worker_override, fixed = TRUE)
)

# A low-core/one-worker job retains per-draw cancellation rather than waiting
# for an entire chunk to finish.
options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
cancel_checks <- 0L
chunk_cancel_error <- tryCatch({
  structural_canvas_reliability_bootstrap(
    "eta1 =~ x1 + x2 + x3", continuous, reps = 6L, seed = 20260814L,
    estimator = "ML", missing = "listwise", original_fit = single_fit,
    workers = 1L, chunk_size = 4L,
    cancel = function() {
      cancel_checks <<- cancel_checks + 1L
      cancel_checks > 2L
    }
  )
  ""
}, error = conditionMessage)
options(statedu.isolated_lavaan_bootstrap_worker = old_isolated_worker)
stopifnot(grepl("canceled", chunk_cancel_error, fixed = TRUE))

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
  reliability_progress_events[[2L]][["done"]] == 1L,
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
progress_phase_one <- structural_canvas_cfa_bootstrap_progress_offsets(0L, 0L, 4L, 3L)
progress_phase_two <- structural_canvas_cfa_bootstrap_progress_offsets(
  progress_phase_one$completed, progress_phase_one$valid, 5L, 2L
)
stopifnot(
  nrow(cfa_job_value$reliability_bootstrap_result) == 4L,
  identical(cfa_job_status$phase, "complete"),
  identical(cfa_job_status$completed, 4L),
  identical(cfa_job_status$total, 4L),
  identical(cfa_job_status$valid, 4L),
  identical(progress_phase_two$completed, 9L),
  identical(progress_phase_two$valid, 5L)
)

# The progress file is polled while the background worker replaces it. Repeated
# writes must always leave a readable, monotonic snapshot so the UI cannot jump
# between two progress cards or regress to an earlier phase.
cfa_atomic_progress <- tempfile(fileext = ".rds")
cfa_cached_progress <- NULL
for (done in 0:20) {
  stopifnot(isTRUE(structural_canvas_write_cfa_bootstrap_progress(
    cfa_atomic_progress, "reliability", done, 20L, done
  )))
  candidate <- structural_canvas_read_bootstrap_progress_snapshot(cfa_atomic_progress)
  merged <- structural_canvas_cfa_bootstrap_progress_merge(cfa_cached_progress, candidate)
  stopifnot(is.list(merged), identical(merged$completed, as.integer(done)))
  cfa_cached_progress <- merged
}
stable_progress <- cfa_cached_progress
stopifnot(
  identical(
    structural_canvas_cfa_bootstrap_progress_merge(stable_progress, list(phase = "reliability")),
    stable_progress
  ),
  identical(
    structural_canvas_cfa_bootstrap_progress_merge(
      stable_progress,
      list(phase = "starting", completed = 0L, total = 20L, valid = 0L)
    ),
    stable_progress
  ),
  identical(
    structural_canvas_cfa_bootstrap_progress_merge(
      stable_progress,
      list(phase = "reliability", completed = 19L, total = 20L, valid = 19L)
    ),
    stable_progress
  )
)
unlink(cfa_atomic_progress)
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
bollen_progress_matrix <- do.call(rbind, bollen_progress_events)
stopifnot(
  nrow(bollen_stine) == 1L,
  bollen_stine[["Requested replicates"]][[1L]] == 8L,
  bollen_progress_events[[1L]][["done"]] == 0L,
  tail(bollen_progress_events, 1L)[[1L]][["done"]] == 8L,
  tail(bollen_progress_events, 1L)[[1L]][["valid"]] == bollen_stine[["Valid replicates"]][[1L]],
  all(diff(bollen_progress_matrix[, "done"]) >= 0L),
  all(diff(bollen_progress_matrix[, "valid"]) >= 0L),
  all(bollen_progress_matrix[, "done"] <= bollen_progress_matrix[, "total"]),
  grepl("canceled", bollen_cancel_error, fixed = TRUE)
)

cat("CFA bootstrap validations passed.\n")
