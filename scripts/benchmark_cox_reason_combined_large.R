.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/cox-reason-combined-large-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1]);if(is.na(run_id))run_id<-1L
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_survival.R',env)
sys.source('scripts/fixtures/survival_reason_dedup_reference.R',old)
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
set.seed(778);n<-100000L
base_data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)),cluster_id=rep(seq_len(n/10L),each=10L))
for(i in 1:6)base_data[[paste0('x',i)]]<-rnorm(n)
cases<-expand.grid(pattern=c('complete','overlap','disjoint'),kind=c('basic','strata','cluster','spline'),stringsAsFactors=FALSE)
if(run_id%%2L==0L)cases<-cases[nrow(cases):1L,]
timings<-list();checks<-0L
for(k in seq_len(nrow(cases))){
 pattern<-cases$pattern[k];kind<-cases$kind[k];data<-base_data
 if(pattern=='overlap'){
  rows<-seq.int(1L,n,2L);data$event[rows]<-NA;data$group[rows]<-NA
  for(i in 1:6)data[[paste0('x',i)]][rows]<-NA
 }
 if(pattern=='disjoint')for(i in 1:6)data[[paste0('x',i)]][which(seq_len(n)%%12L==i)]<-NA
 expected_n<-sum(complete.cases(data))
 args<-list(data=data,time='time',event='event',covariates=paste0('x',1:6),adjusted_bootstrap_reps=0L)
 if(kind=='strata')args$strata<-'group'
 if(kind=='cluster')args$cluster<-'cluster_id'
 if(kind=='spline'){args$spline_covariate<-'x1';args$spline_df<-4L}
 expected<-capture(old,'prepare_cox_analysis_result',args)
 stopifnot(inherits(expected$value$fit,'coxph'),expected$value$n==expected_n,all(is.finite(coef(expected$value$fit))))
 compare(expected,capture(new,'prepare_cox_analysis_result',args));checks<-checks+1L
 for(iteration in 1:3)for(version in if((iteration+run_id)%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-capture(get(version),'prepare_cox_analysis_result',args)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  compare(expected,actual);checks<-checks+1L
  timings[[length(timings)+1L]]<-data.frame(run=run_id,pattern,kind,iteration,version,seconds=elapsed,analysis_rows=actual$value$n,coefficients=length(coef(actual$value$fit)),diagnostics=length(actual$diagnostics))
 }
 out<-do.call(rbind,timings);write.csv(out,file.path(root,paste0('times-',run_id,'.csv')),row.names=FALSE)
 cat('PASS:',pattern,kind,'rows',expected_n,'\n')
 print(aggregate(seconds~version,subset(out,pattern==cases$pattern[k]&kind==cases$kind[k]),median))
}
cat('PASS:',checks,'full Cox comparisons; formula environments excluded; diagnostics/stdout/RNG exact\n')
