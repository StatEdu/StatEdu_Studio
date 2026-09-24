source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-survival_km_posthoc_table
restore<-function(expr) {
 if(is.call(expr)&&identical(expr[[1]],as.name('list'))&&identical(names(expr)[2],'Comparison')) {
  expr[[1]]<-as.name('data.frame');return(as.call(c(as.list(expr),list(stringsAsFactors=FALSE))))
 }
 if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('columns')))
  return(quote(columns <- do.call(rbind,rows)))
 if(identical(expr,quote(names(columns) <- names(rows[[1L]]))))return(quote(invisible(NULL)))
 if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
 expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$survival_km_posthoc_table}
capture<-function(expr) {
 diagnostics<-character();set.seed(741)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
  warning=function(w){diagnostics<<-c(diagnostics,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){diagnostics<<-c(diagnostics,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')})
 list(value=value,diagnostics=diagnostics,rng=.Random.seed)
}
checks<-0L
for(k in c(1,2,3,10))for(seed in 1:6) {
 set.seed(seed);n<-k*12
 d<-data.frame(time=sample(1:20,n,TRUE),event=sample(c(TRUE,FALSE),n,TRUE),group=factor(rep(seq_len(k),12)))
 d$entry<-pmax(0,d$time-2)
 if(seed==2)d$event[]<-FALSE
 if(seed==3)d$event[]<-TRUE
 if(seed==4)d$event[d$group==1]<-FALSE
 if(seed==5)d$time[]<-20
 if(seed==6)d$event[1]<-NA
 for(method in c('logrank','breslow','tarone_ware'))for(entry in c('','entry')) {
  run<-function(fn)capture(fn(d,'time','event','group',NULL,method,entry))
  stopifnot(identical(run(reference),run(survival_km_posthoc_table),num.eq=FALSE));checks<-checks+1L
 }
}
cat('PASS:',checks,'exact posthoc table/condition/RNG comparisons.\n')
