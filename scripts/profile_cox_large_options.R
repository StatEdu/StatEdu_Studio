.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root<-'output/cox-large-options-20260915/analysis-only';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run_id))run_id<-1L
# Formula environments are not numeric results and differ across fresh fits.
# Retain every other field/attribute and record this normalization explicitly.
normalize<-function(x){
 a<-attributes(x)
 if(is.call(x))x<-as.call(lapply(as.list(x),normalize))
 else if(is.pairlist(x))x<-as.pairlist(lapply(as.list(x),normalize))
 else if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize(x[[i]]))
 if(!is.null(a)){
  a$.Environment<-NULL
  for(i in seq_along(a))a[i]<-list(normalize(a[[i]]))
  attributes(x)<-a
 }
 x
}
set.seed(778);n<-20000L
data<-data.frame(time=rexp(n,.01),event=rbinom(n,1,.7),group=factor(rep(letters[1:3],length.out=n)),cluster_id=rep(seq_len(n/10L),each=10L))
for(i in 1:6)data[[paste0('x',i)]]<-rnorm(n)
run_case<-function(kind,normalize_result=TRUE){
 args<-list(data=data,time='time',event='event',covariates=paste0('x',1:6),adjusted_bootstrap_reps=0L)
 if(kind=='strata')args$strata<-'group'
 if(kind=='cluster')args$cluster<-'cluster_id'
 if(kind=='spline'){args$spline_covariate<-'x1';args$spline_df<-4L}
 diagnostics<-list()
 stdout<-capture.output(value<-withCallingHandlers(do.call(prepare_cox_analysis_result,args),
  warning=function(e){diagnostics[[length(diagnostics)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
  message=function(e){diagnostics[[length(diagnostics)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 stopifnot(inherits(value$fit,'coxph'),value$n==n,all(is.finite(stats::coef(value$fit))))
 list(value=if(normalize_result)normalize(value)else value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
timings<-list()
kinds<-if(run_id%%2L)c('basic','strata','cluster','spline')else c('spline','cluster','strata','basic')
if(length(commandArgs(trailingOnly=TRUE))>1L)kinds<-commandArgs(trailingOnly=TRUE)[2L]
for(kind in kinds){
 expected<-run_case(kind)
 for(iteration in 1:3){
  gc();start<-Sys.time();actual<-run_case(kind,normalize_result=FALSE)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  actual$value<-normalize(actual$value)
  if(!identical(expected,actual,num.eq=FALSE)){
   saveRDS(list(expected=expected,actual=actual),file.path(root,paste0('mismatch-',kind,'.rds')))
   print(all.equal(expected,actual));stop('Repeatability mismatch')
  }
  timings[[length(timings)+1L]]<-data.frame(run=run_id,kind,iteration,seconds=elapsed,n=actual$value$n,coefficients=length(coef(actual$value$fit)),diagnostics=length(actual$diagnostics))
 }
 gc();Rprof(file.path(root,paste0(kind,'-',run_id,'.out')),interval=.01)
 profiled<-lapply(1:3,function(i)run_case(kind,normalize_result=FALSE))
 Rprof(NULL)
 profiled<-lapply(profiled,function(item){item$value<-normalize(item$value);item})
 stopifnot(all(vapply(profiled,function(value)identical(expected,value,num.eq=FALSE),logical(1))))
 p<-summaryRprof(file.path(root,paste0(kind,'-',run_id,'.out')))
 write.csv(p$by.self,file.path(root,paste0(kind,'-',run_id,'-self.csv')))
 write.csv(p$by.total,file.path(root,paste0(kind,'-',run_id,'-total.csv')))
 saveRDS(expected,file.path(root,paste0(kind,'-',run_id,'.rds')))
 cat(kind,'sampled seconds',p$sampling.time,'\n');print(head(p$by.self,5L))
}
write.csv(do.call(rbind,timings),file.path(root,paste0('times-',run_id,'.csv')),row.names=FALSE)
print(aggregate(seconds~kind,do.call(rbind,timings),median))
cat('PASS: successful fits, repeatability of normalized results/diagnostics/stdout/RNG\n')
