# Run with the bundled R library, from the repository root.
source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()

# Full HTMT output remains the oracle for the matrix-only bootstrap path.
set.seed(701)
data <- as.data.frame(matrix(rnorm(600), 100, 6))
names(data) <- paste0('x', 1:6)
data[1:4, 1] <- NA_real_
correlations <- cor(data, use = 'pairwise.complete.obs')
for (indicators in list(
  list(A = c('x1', 'x2', 'x3'), B = c('x4', 'x5', 'x6')),
  list(A = 'x1', B = c('x2', 'x3')),
  list(A = c('x1', 'x2'), B = c('x2', 'x3')),
  list(A = c('x1', 'missing'), B = c('x4', 'x5'))
)) {
  public <- structural_canvas_htmt(correlations, indicators)
  fast <- structural_canvas_htmt(correlations, indicators, include_pairs = FALSE)
  stopifnot(identical(public$matrix, fast$matrix, num.eq = FALSE),
            nrow(public$pairs) == 1L, nrow(fast$pairs) == 0L)
}

# Compare complete bootstrap results with the validated public ICC function,
# retaining the original resample sequence and exact quantile calculation.
matrix <- as.matrix(data)
complete <- matrix[complete.cases(matrix), , drop = FALSE]
for (model in c('icc1', 'icc2', 'icc3')) for (agreement in c(TRUE, FALSE)) for (average in c(TRUE, FALSE)) {
  set.seed(19)
  draws <- replicate(100L, {
    rows <- sample.int(nrow(complete), nrow(complete), replace = TRUE)
    interrater_icc_value(complete[rows, , drop = FALSE], model, agreement, average)
  })
  expected_seed <- .Random.seed
  alpha <- (1 - .95) / 2
  expected <- quantile(draws[is.finite(draws)], c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE)
  actual <- interrater_icc_bootstrap_ci(matrix, model, agreement, average, resamples = 100, seed = 19)
  stopifnot(identical(actual, expected, num.eq = FALSE), identical(.Random.seed, expected_seed))
}

# Initialization stays lazy and does not alter the global compiler setting.
label_env <- new.env(parent = .GlobalEnv)
source('R/labels.R', local = label_env, encoding = 'UTF-8')
stopifnot(is.null(environment(label_env$statedu_translation_table)$cache))
old_jit <- compiler::enableJIT(3)
translated <- label_env$statedu_translation_table()
stopifnot(length(translated) > 1000L,
          identical(translated, label_env$statedu_translation_table()),
          identical(translated, statedu_translation_table()))
stopifnot(compiler::enableJIT(old_jit) == 3L)

# Shiny's directory loader must leave module loading to app.R.
support_env <- new.env(parent = .GlobalEnv)
shiny:::loadSupport('.', renv = support_env, globalrenv = NULL)
stopifnot(length(ls(support_env, all.names = TRUE)) == 0L)
message('PASS: HTMT public/matrix paths, ICC bootstrap/RNG, lazy translation cache/JIT and single Shiny loader.')
