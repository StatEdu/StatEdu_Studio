# Deterministic, self-contained fixture for the structural bootstrap release
# gates. The historical validators referenced an untracked local CSV whose
# source data produces an inadmissible original fit under the current
# four-factor all-pairs DMC model, so it is not a valid fail-closed bootstrap
# fixture.
#
# This generator deliberately fixes every RNG component and restores the
# caller's RNG kind and seed exactly. Its canonical numeric payload is pinned
# by SHA-256 so a changed R RNG, coefficient, column order, or draw order fails
# before any timing/equivalence claim is accepted.

structural_bootstrap_dmc_fixture_expected_hash <- function() {
  "8a0788d0a806be4bfb5c556f326d9d1e61a71cc16305f55a454f16602417f10f"
}

structural_bootstrap_dmc_fixture_runtime_contract <- function() {
  list(
    R_version = "4.5.3",
    RNGkind = c("Mersenne-Twister", "Inversion", "Rejection"),
    seed = 8L,
    generator_version = 1L
  )
}

structural_bootstrap_dmc_fixture_hash <- function(data) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest is required to verify the structural bootstrap fixture.", call. = FALSE)
  }
  expected_names <- c(paste0("y", 1:8), paste0("x", 1:3))
  if (!is.data.frame(data) || !identical(dim(data), c(75L, 11L)) ||
      !identical(names(data), expected_names) || anyNA(data) ||
      !all(vapply(data, is.numeric, logical(1)))) {
    stop(
      "The deterministic structural bootstrap fixture must be a complete numeric 75-by-11 data frame in canonical column order.",
      call. = FALSE
    )
  }
  canonical <- paste(
    c(
      paste(names(data), collapse = ","),
      sprintf("%.17g", unlist(data, use.names = FALSE))
    ),
    collapse = "\n"
  )
  digest::digest(canonical, algo = "sha256", serialize = FALSE)
}

structural_bootstrap_dmc_fixture <- function() {
  contract <- structural_bootstrap_dmc_fixture_runtime_contract()
  observed_r_version <- as.character(getRversion())
  if (!identical(observed_r_version, contract$R_version)) {
    stop(
      sprintf(
        "The structural bootstrap fixture generator is pinned to R %s; observed R %s.",
        contract$R_version, observed_r_version
      ),
      call. = FALSE
    )
  }
  caller_kind <- RNGkind()
  caller_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (caller_seed_exists) {
    caller_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(RNGkind, as.list(caller_kind))
    if (caller_seed_exists) {
      assign(".Random.seed", caller_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  do.call(RNGkind, as.list(contract$RNGkind))
  set.seed(contract$seed)
  rows <- 75L
  structural_scale <- 0.90
  residual_scale <- 0.90
  predictor <- stats::rnorm(rows)
  moderator <- stats::rnorm(rows)
  outcome <- 0.40 * predictor + 0.20 * moderator +
    0.12 * predictor * moderator +
    stats::rnorm(rows, sd = 0.70 * structural_scale)
  downstream <- 0.32 * outcome + 0.18 * predictor +
    stats::rnorm(rows, sd = 0.72 * structural_scale)
  indicator <- function(latent, loading, residual_sd) {
    loading * latent + stats::rnorm(rows, sd = residual_sd)
  }
  data <- data.frame(
    y1 = indicator(outcome, 0.86, 0.42 * residual_scale),
    y2 = indicator(outcome, 0.80, 0.48 * residual_scale),
    y3 = indicator(outcome, 0.76, 0.52 * residual_scale),
    y4 = indicator(moderator, 0.88, 0.40 * residual_scale),
    y5 = indicator(moderator, 0.70, 0.44 * residual_scale),
    y6 = indicator(downstream, 0.87, 0.41 * residual_scale),
    y7 = indicator(downstream, 0.81, 0.47 * residual_scale),
    y8 = indicator(downstream, 0.75, 0.53 * residual_scale),
    x1 = indicator(predictor, 0.88, 0.40 * residual_scale),
    x2 = indicator(predictor, 0.82, 0.46 * residual_scale),
    x3 = indicator(predictor, 0.76, 0.52 * residual_scale),
    check.names = FALSE
  )
  actual_hash <- structural_bootstrap_dmc_fixture_hash(data)
  expected_hash <- structural_bootstrap_dmc_fixture_expected_hash()
  if (!identical(actual_hash, expected_hash)) {
    stop(
      sprintf(
        "The deterministic structural bootstrap fixture hash changed: expected %s; observed %s.",
        expected_hash, actual_hash
      ),
      call. = FALSE
    )
  }
  list(
    id = "statedu_dmc_75_v1",
    data = data,
    R_version = contract$R_version,
    RNGkind = contract$RNGkind,
    seed = contract$seed,
    generator_version = contract$generator_version,
    sha256 = actual_hash,
    provenance = "StatEdu deterministic four-factor all-pairs DMC validation fixture"
  )
}
