# Structural equation canvas Bollen-Stine bootstrap helpers.

structural_canvas_bollen_stine <- function(fit, reps = 500L, seed = default_seed(), progress = NULL, cancel = NULL) {
  reps <- as.integer(reps)
  if (!is.finite(reps) || reps < 1L) stop("Bollen-Stine bootstrap requires at least one resample.")
  eligibility <- structural_canvas_bollen_stine_eligibility(fit)
  if (!isTRUE(eligibility$available)) stop(eligibility$reason)
  observed <- unname(lavaan::fitMeasures(fit, "chisq"))
  evaluations <- 0L
  completed <- 0L
  valid <- 0L
  progress_step <- max(1L, floor(reps / 100L))
  if (is.function(progress)) progress(0L, reps, 0L)
  draws <- suppressWarnings(lavaan::bootstrapLavaan(
    fit, R = reps, type = "bollen.stine", iseed = as.integer(seed),
    FUN = function(candidate) {
      if (is.function(cancel) && isTRUE(cancel())) stop("Bollen-Stine bootstrap canceled.")
      evaluations <<- evaluations + 1L
      value <- if (isTRUE(structural_canvas_fit_admissibility(candidate)$admissible)) {
        unname(lavaan::fitMeasures(candidate, "chisq"))
      } else {
        NA_real_
      }
      # lavaan evaluates FUN once on the original fit to establish the result
      # shape (t0) before evaluating the requested bootstrap samples.  Do not
      # count that sizing call as a resample or progress briefly exceeds 100%.
      if (evaluations > 1L) {
        completed <<- completed + 1L
        if (is.finite(value)) valid <<- valid + 1L
        if (is.function(progress) && (completed == 1L || completed == reps || completed %% progress_step == 0L)) {
          progress(completed, reps, valid)
        }
      }
      value
    }
  ))
  draws <- as.numeric(draws)
  valid_draws <- draws[is.finite(draws)]
  valid <- length(valid_draws)
  if (is.function(progress)) progress(reps, reps, valid)
  exceedances <- if (valid) sum(valid_draws >= observed) else 0L
  trials <- valid + 1L
  successes <- exceedances + 1L
  p_bootstrap <- if (valid) successes / trials else NA_real_
  mcse <- if (valid) sqrt(p_bootstrap * (1 - p_bootstrap) / trials) else NA_real_
  z_95 <- stats::qnorm(.975)
  wilson_denominator <- 1 + z_95^2 / trials
  wilson_center <- if (valid) (p_bootstrap + z_95^2 / (2 * trials)) / wilson_denominator else NA_real_
  wilson_margin <- if (valid) z_95 * sqrt(p_bootstrap * (1 - p_bootstrap) / trials + z_95^2 / (4 * trials^2)) / wilson_denominator else NA_real_
  data.frame(
    `Observed chi-square` = observed,
    `Bootstrap p` = p_bootstrap,
    `Monte Carlo SE` = mcse,
    `Monte Carlo 95% lower` = if (valid) max(0, wilson_center - wilson_margin) else NA_real_,
    `Monte Carlo 95% upper` = if (valid) min(1, wilson_center + wilson_margin) else NA_real_,
    `Valid replicates` = valid,
    `Requested replicates` = reps,
    `Valid %` = 100 * valid / reps,
    Status = structural_canvas_bootstrap_status(valid, reps),
    Seed = as.integer(seed),
    check.names = FALSE
  )
}

structural_canvas_bollen_stine_eligibility <- function(fit) {
  estimator <- toupper(as.character(lavaan::lavInspect(fit, "options")$estimator %||% ""))
  if (!identical(estimator, "ML")) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is available only for ML estimation."))
  if (length(lavaan::lavNames(fit, "ov.ord"))) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is not available for ordered indicators."))
  if (as.integer(lavaan::lavInspect(fit, "ngroups")) != 1L) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is currently available only for single-group CFA."))
  admissibility <- structural_canvas_fit_admissibility(fit)
  if (!isTRUE(admissibility$admissible)) return(list(
    available = FALSE,
    reason = paste0("Bollen-Stine bootstrap requires an admissible fitted model: ", paste(admissibility$reasons, collapse = "; "), ".")
  ))
  degrees_of_freedom <- unname(lavaan::fitMeasures(fit, "df"))
  if (!is.finite(degrees_of_freedom) || degrees_of_freedom <= 0) return(list(available = FALSE, reason = "Bollen-Stine bootstrap is not informative for a saturated model with df = 0."))
  analyzed_data <- as.matrix(lavaan::lavInspect(fit, "data"))
  if (anyNA(analyzed_data)) return(list(available = FALSE, reason = "Bollen-Stine bootstrap requires complete analyzed data in this implementation."))
  list(available = TRUE, reason = "Eligible complete continuous single-group ML model with positive degrees of freedom.")
}
