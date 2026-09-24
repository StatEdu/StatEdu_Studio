.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
source('scripts/fixtures/longitudinal_mermod_normalize.R')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_longitudinal.R',env)
reference_path<-commandArgs(TRUE)
if(length(reference_path))sys.source(reference_path[[1]],old) else {
 original_sensitivity<-old$longitudinal_sensitivity_analysis_results
 old$longitudinal_sensitivity_analysis_results<-function(...,selected_fit=NULL)
  original_sensitivity(...,selected_fit=if(inherits(selected_fit$model,'lmerMod'))NULL else selected_fit)
}
run<-function(env,d,args,signal){
 original_fit<-env$longitudinal_fit_model;calls<-0L;primary_quiet<-TRUE;metrics<-list()
 env$longitudinal_fit_model<-function(...){
  calls<<-calls+1L
  if(signal=='error')stop('reuse test error')
  if(signal=='warning')warning('reuse test warning')
  if(signal=='message')message('reuse test message')
  if(signal=='rng')runif(1)
  value<-withCallingHandlers(original_fit(...),warning=function(w){if(calls==1L)primary_quiet<<-FALSE},message=function(m){if(calls==1L)primary_quiet<<-FALSE})
  metrics[[length(metrics)+1L]]<<-list(aic=value$aic,bic=value$bic,singular=lme4::isSingular(value$model,tol=1e-4))
  value
 }
 on.exit(env$longitudinal_fit_model<-original_fit)
 set.seed(718);conditions<-list()
 output<-capture.output(value<-withCallingHandlers(do.call(env$prepare_longitudinal_analysis_result,c(list(data=d,outcome='y',id='id',time='time',predictors=c('x1','x2'),variable_info=data.frame(name=names(d),measurement='continuous')),args)),
 warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=normalize(value),conditions=conditions,output=output,rng=.Random.seed),calls=calls,quiet=primary_quiet,metrics=metrics)
}
passed<-0L;reused<-0L;fallback<-0L;reused_slope<-0L
check<-function(d,args,signal='quiet'){
 a<-run(old,d,args,signal);b<-run(new,d,args,signal)
 stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE))
 eligible<-identical(args$model_type,'lmm')&&identical(args$family,'gaussian')&&signal=='quiet'&&a$quiet
 if(signal=='error')stopifnot(a$calls==1L,b$calls==1L)else{
  stopifnot(length(a$snapshot$value)==1L,!is.null(a$snapshot$value[[1]]$model$fixed),a$calls==3L,b$calls==if(eligible)2L else 3L)
  # Check unformatted AIC/BIC and singularity of both sensitivity fits.
  expected<-if(eligible)a$metrics[c(1L,3L)]else a$metrics
  stopifnot(identical(expected,b$metrics,num.eq=FALSE))
  if(eligible){stopifnot(identical(a$metrics[[1]],a$metrics[[2]],num.eq=FALSE));reused<<-reused+1L;if(isTRUE(args$random_slope))reused_slope<<-reused_slope+1L}else fallback<<-fallback+1L
 }
 passed<<-passed+1L;cat('PASS',passed,args$model_type,args$family,args$random_slope,signal,'calls',a$calls,b$calls,'\n')
}
set.seed(940);subjects<-200L;n<-subjects*6L
d<-data.frame(id=rep(1:subjects,each=6),time=rep(seq(-1,1,length.out=6),subjects),x1=rnorm(n),x2=rnorm(n),w=runif(n,.7,1.3))
eta<-.2+.3*d$time+.3*d$x1+rep(rnorm(subjects,sd=.8),each=6)+rep(rnorm(subjects,sd=1.5),each=6)*d$time
d$y<-eta+rnorm(n)
for(slope in c(FALSE,TRUE)){
 args<-list(model_type='lmm',family='gaussian',random_slope=slope)
 for(signal in c('quiet','warning','message','rng','error'))check(d,args,signal)
 check(d,c(args,list(exponentiate=FALSE)))
 check(d,c(args,list(weight='w',weight_type='sampling')))
 missing<-d;missing$y[c(3,27)]<-NA_real_;check(missing[rev(seq_len(n)),],args)
 named_rows<-d;rownames(named_rows)<-paste0('row-',seq_len(n));check(named_rows,args)
}
clustered<-d;clustered$site<-rep(1:20,each=60)
for(slope in c(FALSE,TRUE))check(clustered,list(model_type='lmm',family='gaussian',random_slope=slope,cluster='site'))
stopifnot(reused>0L,reused_slope>0L,fallback>0L)
cat('PASS:',passed,'LMM cases;',reused,'reused including',reused_slope,'random slopes;',fallback,'fallback cases\n')


