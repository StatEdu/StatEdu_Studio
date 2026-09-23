Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
statedu_apply_preferences(statedu_default_preferences())

set.seed(904)
d <- data.frame(x = rnorm(120), w = factor(c(rep("A", 59), rep("B", 60), "C")))
d$y <- .3 * d$x + rnorm(120)
x <- model.matrix(~ x * w, d)
cases <- list(x, cbind(x, duplicate = x[, "x"]), x[, FALSE, drop = FALSE], unname(x))
for (design in cases) {
  for (i in seq_len(100)) {
    rows <- sample.int(nrow(design), nrow(design), replace = TRUE)
    reference <- stats::lm.fit(design[rows, , drop = FALSE], d$y[rows])
    actual <- regression_bootstrap_lm_fit(design[rows, , drop = FALSE], d$y[rows])
    stopifnot(identical(actual$coefficients, reference$coefficients),
              identical(actual$residuals, reference$residuals))
  }
}

# The isolated background process must use the same draws, aliases and R-squared.
job <- list(model_matrix = x, outcome = d$y, terms = colnames(x), r = 200L, seed = 902L,
            chunk = 37L, progress_file = tempfile(), result_file = tempfile())
process <- start_bootstrap_process(job)
process$wait(timeout = 20000)
if (process$is_alive()) { process$kill(); stop("Bootstrap worker timed out.") }
stopifnot(identical(process$get_exit_status(), 0L))
actual <- readRDS(job$result_file)
set.seed(job$seed)
expected <- matrix(NA_real_, job$r, ncol(x), dimnames = list(NULL, colnames(x)))
r_squared <- numeric(job$r)
for (i in seq_len(job$r)) {
  rows <- sample.int(nrow(x), nrow(x), replace = TRUE)
  fit <- stats::lm.fit(x[rows, , drop = FALSE], d$y[rows])
  expected[i, ] <- fit$coefficients
  total_ss <- sum((d$y[rows] - mean(d$y[rows], na.rm = TRUE))^2, na.rm = TRUE)
  r_squared[i] <- 1 - sum(fit$residuals^2, na.rm = TRUE) / total_ss
}
stopifnot(identical(actual$samples, expected), identical(actual$r_squared, r_squared))
unlink(c(job$progress_file, job$result_file))

# Cache only fixed reference contrasts, never fitted coefficients or resamples.
spec <- mediation_moderation_fast_lm_spec(d, "y", c("x", "w", "x:w"))
uncached <- spec
uncached$slope_weights_cache <- NULL
for (i in seq_len(40)) {
  rows <- sample.int(nrow(d), nrow(d), replace = TRUE)
  fit <- mediation_moderation_fast_lm_fit(spec, rows)
  for (level in levels(d$w)) {
    cached <- mediation_moderation_fast_conditional_slope(fit$coefficients, spec, "x", list(w = level))
    reference <- mediation_moderation_fast_conditional_slope(fit$coefficients, uncached, "x", list(w = level))
    stopifnot(identical(cached, reference))
  }
}
stopifnot(length(ls(spec$slope_weights_cache)) == nlevels(d$w))
other <- mediation_moderation_fast_lm_spec(d, "y", c("x", "w", "x:w"))
stopifnot(length(ls(other$slope_weights_cache)) == 0L)
cat("Fast regression bootstrap: exact QR parity, singular draws, background RNG and slope-cache isolation passed.\n")
