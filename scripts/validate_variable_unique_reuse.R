source('R/utils.R', encoding='UTF-8')
source('R/data_io.R', encoding='UTF-8')
Sys.setenv(STATEDU_TIMING='0')
reference <- new.env(parent=.GlobalEnv)
sys.source('R/data_io.R',reference)
restore <- function(expr) {
  if (identical(expr, quote(prepared_unique_values %||% values))) return(quote(values))
  if (identical(expr, quote(prepared_values %||% stats::na.omit(as.vector(x)))))
    return(quote(stats::na.omit(as.vector(x))))
  if (identical(expr, quote(prepared_unique_n %||% length(unique(values)))))
    return(quote(length(unique(values))))
  if (identical(expr, quote(unique_n %||% length(unique(present)))))
    return(quote(length(unique(present))))
  if (is.call(expr)) for(i in seq_along(expr)) expr[[i]] <- restore(expr[[i]])
  expr
}
body(reference$infer_measurement) <- restore(body(reference$infer_measurement))
body(reference$variable_summary_table) <- restore(body(reference$variable_summary_table))
args <- commandArgs(TRUE)
if(length(args))sys.source(args[[1]],reference)
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(634)
cases <- list(numeric(), rep(NA_real_,20), c(Inf,-Inf,NaN), c(-0,0,1e-100,1e100),
              c(TRUE,FALSE,NA), factor(c('a','b','c')), ordered(c('a','b','c')),
              as.Date(c('2026-01-01',NA)))
for(k in c(1,2,3,12,13,100))for(offset in c(0,1e-9,0.5))
  cases[[length(cases)+1L]] <- c(rep(seq_len(k)+offset,3),NA)
for(k in c(1,2,3,12,13))cases[[length(cases)+1L]] <- c(paste0('값',seq_len(k)),NA)
for(multiplier in c(1-1e-6,1,1+1e-6))for(sign in c(-1,1))
  cases[[length(cases)+1L]] <- c(rep(sign*(0:2+sqrt(.Machine$double.eps)*multiplier),100),NA)
cases <- c(cases,list(c(1,2,Inf),c(1,2,-Inf),c(1,2,3,NaN)))
for(x in cases) {
  d<-data.frame(x=x)
  stopifnot(identical(capture(reference$infer_measurement(x)),capture(infer_measurement(x)),num.eq=FALSE),
    identical(capture(reference$variable_summary_table(d,list())),capture(variable_summary_table(d,list())),num.eq=FALSE))
}
cat('PASS:',length(cases),'measurement and full-table scenarios, conditions/RNG identical.\n')
