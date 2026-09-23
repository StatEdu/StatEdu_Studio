#!/usr/bin/env Rscript

# Deterministic, self-contained manifest-variable fixture for PLS/PLSc
# regression validation. The bundled runtime intentionally omits package
# example datasets, so validators must not depend on seminr::mobi being
# installed. Caller RNG state and RNG kind are restored exactly.
statedu_pls_validation_fixture <- function(n = 240L, seed = 20260826L) {
  n <- as.integer(n)
  seed <- as.integer(seed)
  if (!is.finite(n) || n < 120L || !is.finite(seed)) {
    stop("PLS validation fixture requires n >= 120 and a finite integer seed.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  previous_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  previous_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  standardize <- function(x) as.numeric(scale(x))
  image <- standardize(stats::rnorm(n))
  expect <- standardize(.55 * image + sqrt(1 - .55^2) * stats::rnorm(n))
  quality <- standardize(.28 * image + .52 * expect + .66 * stats::rnorm(n))

  reflective_block <- function(score, loadings, prefix) {
    values <- vapply(loadings, function(loading) {
      loading * score + sqrt(1 - loading^2) * stats::rnorm(n)
    }, numeric(n))
    values <- as.data.frame(values, check.names = FALSE)
    names(values) <- paste0(prefix, seq_along(loadings))
    values
  }

  data.frame(
    reflective_block(image, c(.72, .78, .82, .75, .80), "IMAG"),
    reflective_block(expect, c(.70, .76, .83), "CUEX"),
    reflective_block(quality, c(.68, .74, .81, .77, .72, .79, .84), "PERQ"),
    check.names = FALSE
  )
}
