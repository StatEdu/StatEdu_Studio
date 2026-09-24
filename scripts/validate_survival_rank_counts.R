.libPaths(R.home('library'));source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(e in list(old,new))sys.source('R/analysis_survival.R',e)
old$survival_rank_counts<-function(...)NULL
capture<-function(f) {
 set.seed(718);conditions<-list()
 output<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,output=output,rng=.Random.seed)
}
Ops.rank_time<-function(e1,e2) {warning('custom time comparison');invisible(runif(1));NextMethod()}
count<-0L;fast<-0L;fallback<-0L
for(k in c(2L,4L,20L))for(kind in c('normal','tied','near','none','all','missing_time','missing_group','infinite','numeric_event','invalid_entry','custom'))for(entry in c('','entry')) {
 set.seed(939);n<-80L;d<-data.frame(time=runif(n,1,20),event=runif(n)<.65,g=factor(rep(seq_len(k),length.out=n)))
 d$entry<-d$time*.2
 if(kind=='tied'){d$time<-floor(d$time);d$entry<-pmax(0,d$time-1)}
 if(kind=='near'){d$time<-1+seq_len(n)*.Machine$double.eps;d$entry<-d$time}
 if(kind=='none')d$event[]<-FALSE
 if(kind=='all')d$event[]<-TRUE
 if(kind=='missing_time')d$time[1]<-NA_real_
 if(kind=='missing_group')d$g[1]<-NA
 if(kind=='infinite')d$time[1]<-Inf
 if(kind=='numeric_event')d$event<-as.integer(d$event)
 if(kind=='invalid_entry')d$entry[1]<-d$time[1]+1
 if(kind=='custom')d$time<-structure(d$time,class='rank_time')
 for(method in c('logrank','breslow','tarone_ware')) {
  invoke<-function(e)capture(function()e$survival_weighted_rank_test(d,'time','event','g',method,entry))
  stopifnot(identical(invoke(old),invoke(new),num.eq=FALSE));count<-count+1L
 }
 if(kind=='custom')next
 times<-sort(unique(d$time[d$event]));codes<-as.integer(droplevels(d$g));kk<-nlevels(droplevels(d$g))
 counts<-new$survival_rank_counts(d,'time','event',codes,times,kk,entry)
 if(is.null(counts)){fallback<-fallback+1L;next}
 fast<-fast+1L
 for(i in seq_along(times)) {
  risk<-d$time>=times[i];if(nzchar(entry))risk<-risk & d$entry<times[i]
  events<-d$time==times[i] & d$event
  stopifnot(identical(counts$risk[,i],tabulate(codes[risk],nbins=kk),num.eq=FALSE),identical(counts$events[,i],tabulate(codes[events],nbins=kk),num.eq=FALSE))
  stopifnot(identical(sum(counts$risk[,i]),sum(risk),num.eq=FALSE),identical(sum(counts$events[,i]),sum(events),num.eq=FALSE))
 }
}
stopifnot(fast>0L,fallback>0L,is.null(new$survival_rank_counts(data.frame(time=1,event=TRUE),'time','event',1L,c(1,2),1000001L,'')))
cat('PASS raw tests/diagnostics/stdout/RNG:',count,'; count fixtures:',fast,'fast,',fallback,'fallback; allocation guard\n')
