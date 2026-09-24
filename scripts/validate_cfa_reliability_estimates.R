source('R/utils.R', encoding = 'UTF-8')
source('R/setup_custom_model_canvas_structural_reliability.R', encoding = 'UTF-8')
stopifnot(requireNamespace('lavaan', quietly = TRUE))

# Reconstruct the prior accessor call while retaining the unchanged arithmetic.
reference <- structural_canvas_reliability_estimates
original_body <- as.list(body(reference))
stopifnot(identical(original_body[[2L]][[2L]], as.name('estimates_only')),
          identical(original_body[[3L]][[2L]], as.name('standardized')))
body(reference) <- as.call(c(list(as.name('{'),
  quote(standardized <- lavaan::standardizedSolution(fit))), original_body[-(1:3)]))
capture <- function(fun, fit, mode) {
  warnings <- messages <- character()
  set.seed(919)
  value <- withCallingHandlers(
    tryCatch(fun(fit, mode), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(value = value, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(715)
latent <- matrix(rnorm(900), 300, 3)
data <- as.data.frame(sapply(1:9, function(j) latent[, ceiling(j/3)] + rnorm(300)))
names(data) <- paste0('x', 1:9)
data$school <- rep(c('A', 'B'), 150)
ordered_data <- data
ordered_data[paste0('x', 1:9)] <- lapply(data[paste0('x', 1:9)], function(x)
  ordered(cut(x, breaks = quantile(x, c(0, .25, .5, .75, 1)), include.lowest = TRUE)))
missing_data <- data
missing_data$x1[seq(1, nrow(data), 7)] <- NA
model <- 'visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'
cases <- list(ML = list(), MLR = list(estimator = 'MLR'),
  ordered = list(data = ordered_data, ordered = paste0('x', 1:9)),
  no_se = list(se = 'none'), nonconverged = list(control = list(iter.max = 1)),
  fiml = list(data = missing_data, missing = 'fiml'),
  negative_residual = list(model = paste(model, 'x1 ~~ -0.01*x1', sep = '\n')),
  zero_residual = list(model = paste(model, 'x1 ~~ 0*x1', sep = '\n')),
  bootstrap = list(se = 'bootstrap', bootstrap = 10L),
  multigroup = list(group = 'school'),
  defined = list(model = paste(model, 'visual ~~ a*textual\n twice := 2*a', sep = '\n')),
  equality = list(model = sub('x2 + x3', 'a*x2 + a*x3', model, fixed = TRUE)))
fits <- lapply(cases, function(args) {
  set.seed(713)
  suppressWarnings(do.call(lavaan::cfa, modifyList(list(model = model, data = data), args)))
})
fits$missing_vcov <- fits$ML
fits$missing_vcov@vcov$vcov <- NULL
fits$other_version <- fits$ML
fits$other_version@version <- '0.6-21'
fits$invalid <- list()
for (name in names(fits)) {
  for (mode in c('standardized', 'model_implied')) {
    before <- capture(reference, fits[[name]], mode)
    after <- capture(structural_canvas_reliability_estimates, fits[[name]], mode)
    stopifnot(identical(before, after, num.eq = FALSE))
    if (name == 'bootstrap') stopifnot(length(after$warnings) > 0L)
  }
}
# Verify the optimization really runs and exceptional cases keep inference.
local({
  trace('standardizedSolution', where = asNamespace('lavaan'), print = FALSE,
    tracer = quote(assign('.cfa_test_inference', se, envir = .GlobalEnv)))
  on.exit({untrace('standardizedSolution', where = asNamespace('lavaan'))
    if (exists('.cfa_test_inference', envir = .GlobalEnv)) rm('.cfa_test_inference', envir = .GlobalEnv)})
  for (name in c('ML', 'MLR', 'ordered', 'fiml', 'bootstrap', 'nonconverged',
                 'negative_residual', 'zero_residual', 'defined', 'equality', 'missing_vcov')) {
    capture(structural_canvas_reliability_estimates, fits[[name]], 'standardized')
    expected <- !(name %in% c('ML', 'MLR', 'ordered', 'fiml'))
    stopifnot(identical(get('.cfa_test_inference', envir = .GlobalEnv), expected))
  }
})
cat('PASS: 30 exact value/warning/message/error/RNG comparisons and inference guards.\n')
