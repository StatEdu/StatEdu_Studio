source('R/utils.R', encoding = 'UTF-8')
source('R/analysis_frequencies.R', encoding = 'UTF-8')
reference <- new.env(parent = .GlobalEnv)
sys.source('R/analysis_frequencies.R', reference)
b <- body(reference$descriptive_table_for_variable)
for (i in seq_along(b)) {
  expr <- b[[i]]
  if (!is.call(expr) || !identical(expr[[1]], as.name('<-'))) next
  if (identical(expr[[2]], as.name('quartiles'))) b[[i]] <- quote(quartiles <- c(
    stats::quantile(values, 0.25, names = FALSE, type = 7),
    stats::quantile(values, 0.75, names = FALSE, type = 7)))
  if (identical(expr[[2]], as.name('iqr_value'))) b[[i]] <- quote(iqr_value <- stats::IQR(values))
}
body(reference$descriptive_table_for_variable) <- b
args <- commandArgs(TRUE)
if (length(args)) sys.source(args[[1]], reference)
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(value = value, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(52)
cases <- list(numeric(), NA_real_, 1, c(-Inf, Inf), c(1, Inf), c(-Inf, 1),
              c(NA, NaN, 0, -0, 1), rep(3, 100), c('1', 'bad', NA, '2'), factor(c('a','b','a')))
for (n in c(2, 3, 4, 5, 10, 101, 10000)) for (scale in c(1, 1e-100, 1e100)) {
  cases[[length(cases)+1L]] <- rnorm(n) * scale
  cases[[length(cases)+1L]] <- sample(c(-2:2, NA), n, TRUE)
}
for (values in cases) {
  data <- data.frame(x = values)
  before <- capture(reference$descriptive_table_for_variable(data, 'x'))
  after <- capture(descriptive_table_for_variable(data, 'x'))
  stopifnot(identical(before, after, num.eq = FALSE))
}
cat('PASS:', length(cases), 'exact descriptive table/condition/RNG comparisons.\n')
