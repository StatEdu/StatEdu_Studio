.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/survival-event-group-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1]);if(is.na(run_id))run_id<-1L
capture<-function(data,settings){
 ds<-list();set.seed(919)
 stdout<-capture.output(value<-withCallingHandlers(survival_preflight(data,settings),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
timings<-list();samples<-list()
cases<-expand.grid(n=c(20000L,100000L),kind=c('numeric-auto','numeric-explicit','factor-auto','character-auto','missing-auto'),stringsAsFactors=FALSE)
if(run_id%%2L==0L)cases<-cases[nrow(cases):1L,]
for(k in seq_len(nrow(cases))){
 n<-cases$n[k];kind<-cases$kind[k];set.seed(778)
 data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)))
 for(i in 1:6)data[[paste0('x',i)]]<-rnorm(n)
 settings<-survival_legacy_settings('time','event','1','group',paste0('x',1:6))
 if(kind=='numeric-explicit')settings$event_map<-data.frame(raw_value=c('0','1'),role=c('censored','event_of_interest'))
 if(kind=='factor-auto')data$event<-factor(data$event)
 if(kind=='character-auto')data$event<-as.character(data$event)
 if(kind=='missing-auto'){data$event[seq.int(1L,n,20L)]<-NA;data$group[seq.int(2L,n,20L)]<-NA}
 expected<-capture(data,settings);stopifnot(isTRUE(expected$value$ok),expected$value$counts$analysis_rows==if(kind=='missing-auto')n*.9 else n)
 for(iteration in 1:5){
  gc();start<-Sys.time();actual<-capture(data,settings);elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  timings[[length(timings)+1L]]<-data.frame(run=run_id,n,kind,iteration,seconds=elapsed)
 }
 prefix<-file.path(root,paste(kind,n,run_id,sep='-'))
 gc();Rprof(paste0(prefix,'.out'),interval=.005)
 profiled<-lapply(1:10,function(i)capture(data,settings))
 Rprof(NULL)
 stopifnot(all(vapply(profiled,function(x)identical(expected,x,num.eq=FALSE),logical(1))))
 p<-summaryRprof(paste0(prefix,'.out'))
 write.csv(p$by.self,paste0(prefix,'-self.csv'));write.csv(p$by.total,paste0(prefix,'-total.csv'))
 samples[[length(samples)+1L]]<-data.frame(run=run_id,n,kind,sampled_seconds=p$sampling.time)
 saveRDS(expected,paste0(prefix,'.rds'))
}
out<-do.call(rbind,timings);write.csv(out,file.path(root,paste0('times-',run_id,'.csv')),row.names=FALSE)
write.csv(do.call(rbind,samples),file.path(root,paste0('samples-',run_id,'.csv')),row.names=FALSE)
print(aggregate(seconds~n+kind,out,median))
cat('PASS: 150 exact repeat comparisons; full preflight, diagnostics/stdout/RNG; profiling excludes comparison\n')
