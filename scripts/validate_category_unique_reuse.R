source('R/utils.R',encoding='UTF-8')
source('R/data_io.R',encoding='UTF-8')
Sys.setenv(STATEDU_TIMING='0')
reference <- new.env(parent=.GlobalEnv)
sys.source('R/data_io.R',reference)
restore <- function(expr) {
  if(identical(expr,quote(prepared_unique_values %||% unique(stats::na.omit(as.vector(prepared_x))))))
    return(quote(unique(stats::na.omit(as.vector(prepared_x)))))
  if(is.call(expr))for(i in seq_along(expr))expr[[i]]<-restore(expr[[i]])
  expr
}
body(reference$value_label_pairs)<-restore(body(reference$value_label_pairs))
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1]],reference)
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(324)
cases<-list(character(),c('z','a','한글','',NA,'a'),c(3,1,2,NA,1),c(TRUE,FALSE,NA),
  rep(NA_real_,3),c(Inf,-Inf,NaN,0),factor(c('z','a',NA)),ordered(c('z','a',NA)))
checks<-0L
for(x in cases)for(labelled in c(FALSE,TRUE))for(limit in c(1L,2L,5L)) {
  raw<-x
  if(labelled)attr(raw,'labels')<-c('third'=3,'first'=1,'second'=2)
  prepared<-if(!is.object(x))unique(stats::na.omit(as.vector(x)))else NULL
  before<-capture(reference$value_label_pairs(raw,x,max_pairs=limit,measurement='category'))
  after<-capture(value_label_pairs(raw,x,max_pairs=limit,measurement='category',prepared_unique_values=prepared))
  stopifnot(identical(before,after,num.eq=FALSE));checks<-checks+1L
}
for(x in cases) {
  data<-data.frame(x=x)
  raw<-data
  attr(raw$x,'labels')<-c('last'=3,'first'=1)
  stopifnot(identical(capture(reference$variable_summary_table(data,list(),raw)),
    capture(variable_summary_table(data,list(),raw)),num.eq=FALSE))
  checks<-checks+1L
}
cat('PASS:',checks,'exact category labels/table/condition/RNG comparisons.\n')
