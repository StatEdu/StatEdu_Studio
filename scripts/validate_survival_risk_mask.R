source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_life_table
restore<-function(expr) {
  if(is.call(expr)&&identical(expr[[1]],as.name('if'))&&identical(expr[[2]],quote(!is.object(group_data[[time]]))))
    return(expr[[4]])
  if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$survival_life_table}
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(824)
Ops.survival_time_probe<-function(e1,e2) {warning('time comparison retained',call.=FALSE);NextMethod()}
d<-data.frame(time=c(-1,0,0,5,5,10,10,15),event=rep(c(TRUE,FALSE),4),group=rep(c('a','b'),4))
cases<-list(d,d[FALSE,])
x<-d;x$time[1]<-NA;cases[[3]]<-x
x<-d;x$event[2]<-NA;cases[[4]]<-x
x<-d;x$time[8]<-Inf;cases[[5]]<-x
x<-d;class(x$time)<-'survival_time_probe';cases[[6]]<-x
checks<-0L
for(data in cases)for(group in c('','group'))for(breaks in list(numeric(),c(5,10),c(0,5,5,10,15))) {
  stopifnot(identical(capture(reference(data,'time','event',group,breaks)),
                      capture(survival_life_table(data,'time','event',group,breaks)),num.eq=FALSE))
  checks<-checks+1L
}
cat('PASS:',checks,'exact risk-mask boundary/condition/RNG comparisons.\n')
