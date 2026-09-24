source('R/app_bootstrap.R',encoding='UTF-8')
load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_reliability.R',reference)
original<-reference$reliability_item_diagnostics
body(original)<-as.call(c(list(as.name('{'),quote(include_deleted_reliability<-TRUE)),as.list(body(original))[-1L]))
reference$reliability_item_diagnostics<-original
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1L]],reference)
capture_kr20<-function(expr) {
  warnings<-messages<-character()
  set.seed(78)
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(88)
d<-as.data.frame(matrix(sample(0:1,240*6,TRUE),240,6));names(d)<-paste0('x',1:6)
datasets<-list(d,d[,1:2],as.data.frame(lapply(d,function(x)ifelse(x==1,'yes','no'))),as.data.frame(lapply(d,factor)))
x<-d;x[1:15,1]<-NA;datasets[[5]]<-x
x<-d;x$x1<-1;datasets[[6]]<-x
x<-d;x[1:20,1]<-2;datasets[[7]]<-x
datasets[[8]]<-d[FALSE,]
count<-0L
for(data in datasets)for(deleted in c(FALSE,TRUE))for(total in c(FALSE,TRUE)) {
  info<-data.frame(name=names(data),measurement='binary')
  opts<-list(reliability_if_deleted=deleted,item_total_correlation=total,omega=FALSE)
  before<-capture_kr20(reference$prepare_reliability_results(data,names(data),info,options=opts))
  after<-capture_kr20(prepare_reliability_results(data,names(data),info,options=opts))
  stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
}
for(measurement in c('continuous','ordered')) {
  info<-data.frame(name=names(d),measurement=measurement)
  opts<-list(reliability_if_deleted=FALSE,item_total_correlation=TRUE,omega=TRUE)
  before<-capture_kr20(reference$prepare_reliability_results(d,names(d),info,options=opts))
  after<-capture_kr20(prepare_reliability_results(d,names(d),info,options=opts))
  stopifnot(is.null(before$value$error),identical(before,after,num.eq=FALSE));count<-count+1L
}
local({
  calls<-0L
  env<-new.env(parent=.GlobalEnv);sys.source('R/analysis_reliability.R',env)
  env$reliability_compute_value<-function(...) {calls<<-calls+1L;reliability_compute_value(...)}
  info<-data.frame(name=names(d),measurement='binary')
  env$prepare_reliability_results(d,names(d),info,options=list(item_total_correlation=TRUE,omega=FALSE))
  stopifnot(calls==1L)
  calls<-0L
  env$prepare_reliability_results(d,names(d),info,options=list(item_total_correlation=TRUE,reliability_if_deleted=TRUE,omega=FALSE))
  stopifnot(calls==7L)
})
cat('PASS:',count,'exact result/condition/RNG comparisons; requested deletion calculations retained.\n')
