source('R/utils.R', encoding = 'UTF-8')
source('R/data_io.R', encoding = 'UTF-8')
reference <- new.env(parent = .GlobalEnv)
sys.source('R/data_io.R', reference)
b <- as.list(body(reference$infer_measurement))
first <- which(vapply(b, function(x) is.call(x) && identical(x[[1]], as.name('<-')) &&
  identical(x[[2]], as.name('values')), logical(1)))
stopifnot(length(first) == 1L)
body(reference$infer_measurement) <- as.call(c(list(as.name('{')), b[first:length(b)]))
args <- commandArgs(TRUE)
if (length(args)) sys.source(args[[1]], reference)
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(value = value, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(825)
cases <- list(logical(), c(TRUE,FALSE,NA), rep(NA,100), matrix(c(TRUE,FALSE),2),
  factor(character()), factor(c('a','a',NA),levels=c('a','unused','third')),
  factor(c('a','b',NA)), ordered(c('a','b',NA)), ordered(character()),
  1:20, c(1,2,NA), c(1,2,3), c(1.2,3.4,5.6), c(Inf,NaN,NA),
  c('한글','a',NA), as.Date(c('2026-01-01',NA)), list(1,2,NA))
as.vector.measurement_probe <- function(x, mode = 'any') {
  warning('conversion retained', call. = FALSE)
  NextMethod()
}
cases <- c(cases, list(structure(c(TRUE,FALSE),class='measurement_probe'),
  structure(factor(c('a','b')),class=c('measurement_probe','factor'))))
for (x in cases) stopifnot(identical(capture(reference$infer_measurement(x)),
                                    capture(infer_measurement(x)), num.eq = FALSE))
data <- data.frame(binary=rep(c(TRUE,FALSE,NA),100),
  category=factor(rep(c('a','b','c'),100)), ordered=ordered(rep(c('a','b','c'),100)),
  continuous=rnorm(300), text=rep(c('한글','a',NA),100))
for (n in c(1,10,300)) {
  x <- data[seq_len(n),,drop=FALSE]
  stopifnot(identical(reference$variable_summary_table(x,list()),
                      variable_summary_table(x,list()), num.eq=FALSE))
}
cat('PASS:',length(cases),'type/result/condition/RNG cases and 3 full variable tables.\n')
