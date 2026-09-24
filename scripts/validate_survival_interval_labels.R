source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
before_env<-new.env(parent=.GlobalEnv);sys.source('R/analysis_survival.R',before_env)
after_env<-new.env(parent=.GlobalEnv);sys.source('R/analysis_survival.R',after_env)
restore<-function(expr) {
  if(identical(expr,quote(interval_label <- interval_labels[[index]])))return(quote(interval_label <- NULL))
  if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(before_env$survival_life_table)<-restore(body(before_env$survival_life_table))
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1]],before_env)
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(642)
d<-data.frame(time=rep(c(0,.001,.5,1,5,10),4),event=rep(c(TRUE,FALSE),12),group=rep(letters[1:4],each=6))
checks<-0L
for(digits in c(2,3,4,6))for(group in c('','group')) {
  options(statedu.output_decimal_digits=digits)
  stopifnot(identical(capture(before_env$survival_life_table(d,'time','event',group,c(.001,.5,1,5))),
    capture(after_env$survival_life_table(d,'time','event',group,c(.001,.5,1,5))),num.eq=FALSE))
  checks<-checks+1L
}
for(kind in c('warning','message')) {
  for(env in list(before_env,after_env)) {
    env$survival_format_number<-local({mode<-kind;function(x) {
      if(mode=='warning')warning('format warning',call.=FALSE)else message('format message')
      survival_format_number(x)
    }})
  }
  a<-capture(before_env$survival_life_table(d,'time','event','group',c(.5,5)))
  b<-capture(after_env$survival_life_table(d,'time','event','group',c(.5,5)))
  stopifnot(identical(a,b,num.eq=FALSE),length(c(a$warnings,a$messages))>0L)
  checks<-checks+1L
}
cat('PASS:',checks,'interval-label precision/diagnostic/RNG scenarios.\n')
