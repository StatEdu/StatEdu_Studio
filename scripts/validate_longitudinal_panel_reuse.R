.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
source('scripts/fixtures/longitudinal_mermod_normalize.R')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_longitudinal.R',env)
reference_path<-commandArgs(TRUE)
if(length(reference_path))sys.source(reference_path[[1]],old)else{
 original_sensitivity<-old$longitudinal_sensitivity_analysis_results
 old$longitudinal_sensitivity_analysis_results<-function(...,selected_fit=NULL)
  original_sensitivity(...,selected_fit=if(inherits(selected_fit$model,'plm'))NULL else selected_fit)
}
run<-function(env,d,args,signal){
 original_fit<-env$longitudinal_fit_model;calls<-0L;quiet<-TRUE;fits<-list()
 env$longitudinal_fit_model<-function(...){
  calls<<-calls+1L
  if(signal=='error')stop('reuse test error')
  if(signal=='warning')warning('reuse test warning')
  if(signal=='message')message('reuse test message')
  if(signal=='rng')runif(1)
  value<-withCallingHandlers(original_fit(...),warning=function(w){if(calls==1L)quiet<<-FALSE},message=function(m){if(calls==1L)quiet<<-FALSE})
  fits[[length(fits)+1L]]<<-normalize(value);value
 }
 on.exit(env$longitudinal_fit_model<-original_fit)
 set.seed(718);conditions<-list()
 output<-capture.output(value<-withCallingHandlers(do.call(env$prepare_longitudinal_analysis_result,c(list(data=d,outcome='y',id='id',time='time',predictors=c('x1','x2'),family='gaussian',variable_info=data.frame(name=names(d),measurement='continuous')),args)),
 warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=normalize(value),conditions=conditions,output=output,rng=.Random.seed),calls=calls,quiet=quiet,fits=fits)
}
passed<-0L;reused<-0L
check<-function(d,args,signal='quiet',skip=FALSE){
 a<-run(old,d,args,signal);b<-run(new,d,args,signal)
 stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE))
 if(signal=='error'||skip)stopifnot(length(a$snapshot$value)==0L,a$calls==1L,b$calls==1L)else{
  eligible<-signal=='quiet'&&a$quiet&&!isTRUE(args$random_slope)
  stopifnot(length(a$snapshot$value)==1L,!is.null(a$snapshot$value[[1]]$model$coefficients),a$calls==3L,b$calls==if(eligible)2L else 3L)
  selected_index<-if(args$model_type=='panel_fe')2L else 3L
  if(eligible){stopifnot(identical(a$fits[[1]],a$fits[[selected_index]],num.eq=FALSE));reused<<-reused+1L}
  expected<-if(eligible)a$fits[-selected_index]else a$fits
  stopifnot(identical(expected,b$fits,num.eq=FALSE))
 }
 passed<<-passed+1L;cat('PASS',passed,args$model_type,signal,'calls',a$calls,b$calls,'\n')
}
set.seed(941);subjects<-100L;n<-subjects*6L
d<-data.frame(id=rep(1:subjects,each=6),time=rep(0:5,subjects),x1=rnorm(n),x2=rnorm(n),w=runif(n,.7,1.3))
d$y<-.3*d$time+.3*d$x1+rep(rnorm(subjects),each=6)+rnorm(n)
for(model in c('panel_fe','panel_re')){
 args<-list(model_type=model)
 for(signal in c('quiet','warning','message','rng','error'))check(d,args,signal)
 check(d,c(args,list(exponentiate=FALSE)))
 # The RE fit on this unbalanced fixture is singular in the pre-change code.
 missing<-d;missing$y[c(3,27)]<-NA_real_;check(missing[rev(seq_len(n)),],args,skip=model=='panel_re')
 missing_subject<-d;missing_subject$y[1:6]<-NA_real_;check(missing_subject,args)
 named<-d;rownames(named)<-paste0('row-',seq_len(n));check(named,args)
 check(d,c(args,list(random_slope=TRUE)))
 check(d,c(args,list(weight='w',weight_type='sampling')),skip=TRUE)
}
stopifnot(reused==9L)
cat('PASS:',passed,'panel cases;',reused,'reused; weighted and singular skipped results preserved\n')
