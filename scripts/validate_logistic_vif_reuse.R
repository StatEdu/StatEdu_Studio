source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE); source_app_modules()
capture <- function(f) {
  set.seed(401)
  warnings <- character()
  value <- withCallingHandlers(tryCatch(f(), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning')})
  list(value = value, warnings = warnings, rng = .Random.seed)
}
set.seed(719)
d <- data.frame(y = sample(0:1, 120, TRUE), x = rnorm(120), z = rnorm(120), group = factor(rep(letters[1:3], 40)))
singular <- d; singular$z <- 2 * singular$x
constant <- d; constant$z <- 1
missing <- d; missing$x[1:8] <- NA_real_
count <- 0L
for (data in list(d, singular, constant, missing, d[FALSE, ])) {
  for (predictors in list(c('x', 'z', 'group'), 'x', character(), 'absent')) {
    formula <- if (length(predictors)) reformulate(predictors, 'y') else y ~ 1
    old <- capture(function() list(maximum = logistic_vif_summary(formula, data),
                                  by_predictor = logistic_vif_by_predictor(formula, data, predictors)))
    new <- capture(function() logistic_vif_diagnostics(formula, data, predictors))
    stopifnot(identical(old, new, num.eq = FALSE))
    count <- count + 1L
  }
}
# A normal model computes collinearity once, without a session/global cache.
calls <- 0L
env <- new.env(parent = .GlobalEnv)
env$coefficient_collinearity <- function(mm) {calls <<- calls + 1L; coefficient_collinearity(mm)}
diagnose <- logistic_vif_diagnostics; environment(diagnose) <- env
invisible(diagnose(y ~ x + z, d, c('x', 'z')))
stopifnot(calls == 1L)
d$z <- d$z + d$x
invisible(diagnose(y ~ x + z, d, c('x', 'z')))
stopifnot(calls == 2L)
message(sprintf('PASS: %d VIF result/warning/error/RNG comparisons and one calculation per model.', count))
