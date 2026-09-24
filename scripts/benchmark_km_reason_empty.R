.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/km-reason-empty-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1]);if(is.na(run_id))run_id<-1L
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_survival.R',env)
sys.source('scripts/fixtures/survival_reason_empty_reference.R',old)
sys.source('scripts/fixtures/survival_reason_empty_candidate.R',new)
for(env in list(old,new))for(name in ls(env))if(is.function(env[[name]]))env[[name]]<-compiler::cmpfun(env[[name]])
# Only formula .Environment attributes are excluded, including formulas inside calls.
normalize<-function(x){
 a<-attributes(x)
 if(is.call(x))x<-as.call(lapply(as.list(x),normalize))
 else if(is.pairlist(x))x<-as.pairlist(lapply(as.list(x),normalize))
 else if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize(x[[i]]))
 if(!is.null(a)){a$.Environment<-NULL;for(i in seq_along(a))a[i]<-list(normalize(a[[i]]));attributes(x)<-a}
 x
}
capture<-function(env,fun,args){
 set.seed(991);ds<-list()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(env[[fun]],args),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),
 error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
compare<-function(a,b){
 a$value<-normalize(a$value);b$value<-normalize(b$value)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(a=a,b=b),file.path(root,'integration-mismatch.rds'));stop('Result mismatch')}
}
cases<-expand.grid(n=c(1000L,100000L),pattern=c('complete','overlap','disjoint'),kind=c('standard','delayed'),stringsAsFactors=FALSE)
focused<-length(commandArgs(trailingOnly=TRUE))>1L
if(focused)cases<-subset(cases,n==100000L&pattern == 'complete')
if(run_id%%2L==0L)cases<-cases[nrow(cases):1L,]
timings<-list();checks<-0L
for(k in seq_len(nrow(cases))){
 n<-cases$n[k];pattern<-cases$pattern[k];kind<-cases$kind[k];set.seed(778)
 # Discrete elapsed days model common tied event times; no claim about continuous-time speed.
 data<-data.frame(time=1+floor(rexp(n,.01)),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)))
 data$entry<-floor(data$time*runif(n,0,.5))
 if(pattern=='overlap'){
  rows<-seq.int(1L,n,2L);for(name in c('time','event','group','entry'))data[[name]][rows]<-NA
 }
 if(pattern=='disjoint'){
  data$time[which(seq_len(n)%%6L==1L)]<-NA
  data$event[which(seq_len(n)%%6L==2L)]<-NA
  data$group[which(seq_len(n)%%6L==3L)]<-NA
 }
 expected_n<-sum(complete.cases(data))
 args<-list(data=data,time='time',event='event',group='group',rate_times=c(0,30,90,180),rmst_tau=180)
 if(kind=='delayed')args$entry<-'entry'
 expected<-capture(old,'prepare_km_analysis_result',args)
 stopifnot(inherits(expected$value$fit,'survfit'),expected$value$n==expected_n,
  all(is.finite(expected$value$fit$surv)),all(expected$value$fit$surv>=0&expected$value$fit$surv<=1),
  is.list(expected$value$logrank),is.list(expected$value$rmst),nrow(expected$value$rmst$estimates)==3L)
 compare(expected,capture(new,'prepare_km_analysis_result',args));checks<-checks+1L
 for(iteration in seq_len(if(focused)15L else 3L))for(version in if((iteration+run_id)%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-capture(get(version),'prepare_km_analysis_result',args)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  compare(expected,actual);checks<-checks+1L
  timings[[length(timings)+1L]]<-data.frame(run=run_id,n,pattern,kind,iteration,version,seconds=elapsed,analysis_rows=actual$value$n,events=actual$value$events,diagnostics=length(actual$diagnostics))
 }
 out<-do.call(rbind,timings);write.csv(out,file.path(root,paste0(if(focused)'focused-'else '', 'times-',run_id,'.csv')),row.names=FALSE)
 cat('PASS:',n,pattern,kind,'rows',expected_n,'\n')
 print(aggregate(seconds~version,subset(out,n==cases$n[k]&pattern==cases$pattern[k]&kind==cases$kind[k]),median))
}
cat('PASS:',checks,'full KM comparisons; environment attributes excluded; diagnostics/stdout/RNG exact\n')
