source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_weighted_rank_test
restore<-function(expr) {
  if(is.call(expr)&&identical(expr[[1]],as.name('if'))&&identical(expr[[2]],quote(k >= 20L)))return(expr[[4]])
  if(is.call(expr))for(i in seq_along(expr))if(!identical(expr[[i]],quote(expr=)))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$survival_weighted_rank_test}
state_function<-function(fn) {
  change<-function(expr) {
    if(identical(expr,quote(keep <- seq_len(k - 1L))))return(quote(return(list(variance=variance,score=observed_minus_expected))))
    if(is.call(expr))for(i in seq_along(expr))if(!identical(expr[[i]],quote(expr=)))expr[i]<-list(change(expr[[i]]))
    expr
  }
  body(fn)<-change(body(fn));fn
}
capture<-function(expr) {
  diagnostics<-character();set.seed(74)
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){diagnostics<<-c(diagnostics,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
    message=function(m){diagnostics<<-c(diagnostics,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')})
  list(value=value,diagnostics=diagnostics,rng=.Random.seed)
}
before_state<-state_function(reference);after_state<-state_function(survival_weighted_rank_test)
checks<-0L
for(k in c(2,3,4,10,19,20,21,30))for(seed in 1:8) {
 set.seed(seed);n<-k*20
 d<-data.frame(time=sample(1:30,n,TRUE),event=sample(c(TRUE,FALSE),n,TRUE),group=factor(rep(seq_len(k),20)))
 d$entry<-pmax(0,d$time-sample(1:5,n,TRUE))
 if(seed==2)d$event[]<-FALSE
 if(seed==3)d$event[]<-TRUE
 if(seed==4)d$time[]<-30
 for(method in c('logrank','breslow','tarone_ware'))for(entry in c('','entry')) {
  run<-function(fn)capture(fn(d,'time','event','group',method,entry))
  stopifnot(identical(run(reference),run(survival_weighted_rank_test),num.eq=FALSE),
            identical(run(before_state),run(after_state),num.eq=FALSE))
  checks<-checks+1L
 }
}
cat('PASS:',checks,'exact rank result AND covariance/score/diagnostic/RNG comparisons.\n')
