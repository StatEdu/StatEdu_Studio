source('R/utils.R', encoding = 'UTF-8')
source('R/data_io.R', encoding = 'UTF-8')
Sys.setenv(STATEDU_TIMING = '0')
reference <- new.env(parent = .GlobalEnv)
sys.source('R/data_io.R', reference)
reference$variable_min <- function(x, prepared_values = NULL) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return('')
  as.character(min(values))
}
reference$variable_max <- function(x, prepared_values = NULL) {
  values <- stats::na.omit(as.vector(x))
  if (length(values) == 0 || !(is.numeric(values) || is.integer(values))) return('')
  as.character(max(values))
}
args <- commandArgs(TRUE)
if (length(args)) sys.source(args[[1]], reference)
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(value = value, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(831)
cases <- list(numeric(), c(1,2,NA), c(-Inf,Inf,NaN,NA), c(-0,0,1e-100,1e100),
  rep(NA_real_,5), c(TRUE,FALSE,NA), c('a','한글',NA), factor(c('a','b',NA)),
  ordered(c('a','b',NA)), as.Date(c('2026-01-01','2026-02-01',NA)),
  as.POSIXct(c('2026-01-01','2026-02-01',NA),tz='UTC'))
checks <- 0L
for (x in cases) {
  for (name in c('variable_min','variable_max')) {
    stopifnot(identical(capture(reference[[name]](x)), capture(get(name)(x)), num.eq=FALSE))
    checks <- checks + 1L
  }
  data <- data.frame(x=x)
  stopifnot(identical(capture(reference$variable_summary_table(data,list())),
                      capture(variable_summary_table(data,list())),num.eq=FALSE))
  checks <- checks + 1L
}
as.vector.range_probe <- function(x, mode='any') {
  warning('range conversion retained',call.=FALSE)
  NextMethod()
}
data <- data.frame(x=c(1,2,NA))
class(data$x) <- 'range_probe'
stopifnot(identical(capture(reference$variable_summary_table(data,list())),
                    capture(variable_summary_table(data,list())),num.eq=FALSE))
cat('PASS:',checks+1L,'exact helper/table/condition/RNG comparisons.\n')
