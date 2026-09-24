.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/survival-event-observed-20260915/final'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1]);if(is.na(run_id))run_id<-1L
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_survival.R',env)
sys.source('scripts/fixtures/survival_event_observed_reference.R',old)
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
set.seed(778);n<-20000L
data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)),cluster_id=rep(seq_len(n/10L),each=10L))
for(i in 1:6)data[[paste0('x',i)]]<-rnorm(n)
timings<-list();checks<-0L
for(kind in c('basic','strata','cluster','spline')){
 args<-list(data=data,time='time',event='event',covariates=paste0('x',1:6),adjusted_bootstrap_reps=0L,event_map=data.frame(raw_value=c('0','1'),role=c('censored','event_of_interest')))
 if(kind=='strata')args$strata<-'group'
 if(kind=='cluster')args$cluster<-'cluster_id'
 if(kind=='spline'){args$spline_covariate<-'x1';args$spline_df<-4L}
 expected<-capture(old,'prepare_cox_analysis_result',args)
 stopifnot(inherits(expected$value$fit,'coxph'),expected$value$n==n,all(is.finite(coef(expected$value$fit))))
 compare(expected,capture(new,'prepare_cox_analysis_result',args));checks<-checks+1L
 for(iteration in 1:5){
  for(version in if((iteration+run_id)%%2L)c('old','new')else c('new','old')){
   gc();start<-Sys.time();actual<-capture(get(version),'prepare_cox_analysis_result',args)
   elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
   compare(expected,actual);checks<-checks+1L
   timings[[length(timings)+1L]]<-data.frame(run=run_id,kind,iteration,version,seconds=elapsed)
  }
 }
}
settings<-list(roles=list(time='time',event='event'),time_origin='baseline',time_unit='days')
for(times in list(1:8,c(0,-1,NA,NaN,Inf,-Inf,1e-300,1e300),as.character(1:8),factor(1:8),as.Date('2020-01-01')+0:7)){
 args<-list(data=data.frame(time=times,event=rep(0:1,4)),settings=settings)
 compare(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args));checks<-checks+1L
}
out<-do.call(rbind,timings)
write.csv(out,file.path(root,paste0('times-',run_id,'.csv')),row.names=FALSE)
print(aggregate(seconds~kind+version,out,median))
cat('PASS:',checks,'full result comparisons; formula environments excluded; diagnostics/stdout/RNG exact\n')
