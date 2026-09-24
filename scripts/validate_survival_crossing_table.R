source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_km_crossing_diagnostics
restore<-function(expr) {
 if(is.call(expr)&&identical(expr[[1]],as.name('list'))&&identical(names(expr)[2],'Comparison')) {
  expr[[1]]<-as.name('data.frame')
  return(as.call(c(as.list(expr),list(check.names=FALSE,stringsAsFactors=FALSE))))
 }
 if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('columns')))
  return(quote(return(do.call(rbind,rows))))
 if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
 expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$survival_km_crossing_diagnostics}
capture<-function(expr) {
 diagnostics<-character();set.seed(741)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
  warning=function(w){diagnostics<<-c(diagnostics,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){diagnostics<<-c(diagnostics,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')})
 list(value=value,diagnostics=diagnostics,rng=.Random.seed)
}
checks<-0L
for(k in c(1,2,3,10,20))for(seed in 1:6) {
 set.seed(seed);n<-k*30
 d<-data.frame(time=sample(1:100,n,TRUE),event=sample(c(TRUE,FALSE),n,TRUE),group=factor(rep(seq_len(k),30)))
 if(seed==2)d$event[]<-FALSE
 if(seed==3)d$event[]<-TRUE
 if(seed==4)d$time[]<-30
 fit<-survival::survfit(survival::Surv(time,event)~group,data=d)
 for(tolerance in c(0,1e-10,.1)) {
  stopifnot(identical(capture(reference(fit,tolerance)),capture(survival_km_crossing_diagnostics(fit,tolerance)),num.eq=FALSE))
  checks<-checks+1L
 }
}
summary.crossing_probe<-function(object,...)object$summary
for(s in list(list(time=numeric(),surv=numeric(),strata=character()),
 list(time=c(1,2,3,4),surv=c(1,.8,.9,.7),strata=c('a','a','b','b')),
 list(time=c(NA,Inf,1,2),surv=c(1,.8,NaN,.7),strata=c('a','a','b','b')),
 list(time=c(1,2,3,4),surv=c(1,.8,.9,.7),strata=c('a',NA,'b','b')))) {
 fit<-structure(list(summary=s),class='crossing_probe')
 stopifnot(identical(capture(reference(fit)),capture(survival_km_crossing_diagnostics(fit)),num.eq=FALSE));checks<-checks+1L
}
fit<-structure(list(summary=list(time=rep(1:3,3),surv=rep(c(1,.8,.5),3),strata=rep(c('a','b','c'),each=3))),class='crossing_probe')
for(kind in c('warning','message')) {
 env<-new.env(parent=.GlobalEnv)
 env$max<-local({mode<-kind;function(...) {
  if(mode=='warning')warning('follow-up diagnostic',call.=FALSE)else message('follow-up diagnostic')
  base::max(...)
 }})
 before<-reference;after<-survival_km_crossing_diagnostics
 environment(before)<-env;environment(after)<-env
 a<-capture(before(fit));b<-capture(after(fit))
 stopifnot(length(a$diagnostics)>0L,identical(a,b,num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact crossing-table/condition/RNG comparisons.\n')

