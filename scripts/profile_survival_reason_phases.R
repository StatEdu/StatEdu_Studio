.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/survival-reason-phases-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1]);if(is.na(run_id))run_id<-1L
original<-new.env(parent=.GlobalEnv);instrumented<-new.env(parent=.GlobalEnv)
for(env in list(original,instrumented))sys.source('R/analysis_survival.R',env)
phase_records<-list()
replacement<-quote(function(mask,code){
 t0<-Sys.time();force(mask);t1<-Sys.time()
 indexes<-which(!is.na(mask)&mask);t2<-Sys.time()
 for(index in indexes)reasons[[index]]<<-unique(c(reasons[[index]],code))
 t3<-Sys.time()
 phase_records[[length(phase_records)+1L]]<<-data.frame(code=code,rows=length(indexes),
  mask_seconds=as.numeric(difftime(t1,t0,units='secs')),
  index_seconds=as.numeric(difftime(t2,t1,units='secs')),
  write_seconds=as.numeric(difftime(t3,t2,units='secs')))
})
modified<-body(instrumented$survival_preflight);replaced<-0L
for(i in seq_along(modified)){
 node<-modified[[i]]
 if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],as.name('add_reason'))){modified[[i]][[3]]<-replacement;replaced<-replaced+1L}
}
stopifnot(replaced==1L);body(instrumented$survival_preflight)<-modified
for(env in list(original,instrumented))env$survival_preflight<-compiler::cmpfun(env$survival_preflight)
capture<-function(env,data,settings){
 set.seed(919);ds<-list()
 stdout<-capture.output(value<-withCallingHandlers(env$survival_preflight(data,settings),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
cases<-expand.grid(n=c(20000L,100000L),kind=c('complete','sparse','overlap'),stringsAsFactors=FALSE)
if(run_id%%2L==0L)cases<-cases[nrow(cases):1,]
all_records<-list();times<-list();checks<-0L
for(k in seq_len(nrow(cases))){
 n<-cases$n[k];kind<-cases$kind[k];set.seed(778)
 data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)))
 for(i in 1:6)data[[paste0('x',i)]]<-rnorm(n)
 if(kind=='sparse'){data$event[seq.int(1L,n,20L)]<-NA;data$group[seq.int(2L,n,20L)]<-NA}
 if(kind=='overlap'){
  rows<-seq.int(1L,n,2L);data$event[rows]<-NA;data$group[rows]<-NA
  for(i in 1:6)data[[paste0('x',i)]][rows]<-NA
 }
 settings<-survival_legacy_settings('time','event','1','group',paste0('x',1:6))
 expected<-capture(original,data,settings)
 stopifnot(isTRUE(expected$value$ok),expected$value$counts$analysis_rows==n*switch(kind,complete=1,sparse=.9,overlap=.5))
 for(iteration in 1:5){
  gc();t0<-Sys.time();actual<-capture(original,data,settings);elapsed<-as.numeric(difftime(Sys.time(),t0,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE));checks<-checks+1L
  times[[length(times)+1L]]<-data.frame(run=run_id,n,kind,iteration,seconds=elapsed)
 }
 phase_records<-list();warm<-capture(instrumented,data,settings)
 stopifnot(identical(expected,warm,num.eq=FALSE));checks<-checks+1L
 for(iteration in 1:3){
  phase_records<-list();actual<-capture(instrumented,data,settings)
  stopifnot(identical(expected,actual,num.eq=FALSE));checks<-checks+1L
  rows<-do.call(rbind,phase_records);rows$run<-run_id;rows$n<-n;rows$kind<-kind;rows$iteration<-iteration
  all_records[[length(all_records)+1L]]<-rows
 }
 saveRDS(expected,file.path(root,paste(kind,n,run_id,'rds',sep='.')))
}
records<-do.call(rbind,all_records);timings<-do.call(rbind,times)
write.csv(records,file.path(root,paste0('phases-',run_id,'.csv')),row.names=FALSE)
write.csv(timings,file.path(root,paste0('times-',run_id,'.csv')),row.names=FALSE)
totals<-aggregate(cbind(mask_seconds,index_seconds,write_seconds)~n+kind+iteration,records,sum)
print(aggregate(seconds~n+kind,timings,median))
print(aggregate(cbind(mask_seconds,index_seconds,write_seconds)~n+kind,totals,median))
cat('PASS:',checks,'exact original/instrumented full result comparisons, diagnostics/stdout/RNG\n')
