.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
source('scripts/fixtures/longitudinal_mermod_normalize.R')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_longitudinal.R',env)
# Independent literal of the original unweighted test computation.
old$longitudinal_hausman_test<-function(data,formula,id,time){
 fixed<-plm::plm(formula,data=data,index=c(id,time),model='within',effect='individual')
 random<-plm::plm(formula,data=data,index=c(id,time),model='random',effect='individual')
 plm::phtest(fixed,random)
}
old$longitudinal_hausman_provider<-function(data,formula,id,time){
 force(data);force(formula);force(id);force(time)
 function()old$longitudinal_hausman_test(data,formula,id,time)
}
run<-function(env,d,args,signal){
 original<-env$longitudinal_hausman_test;calls<-0L;tests<-list()
 env$longitudinal_hausman_test<-function(...){
  calls<<-calls+1L
  if(signal=='error')stop('Hausman test error')
  if(signal=='warning')warning('Hausman test warning')
  if(signal=='message')message('Hausman test message')
  if(signal=='rng')runif(1)
  value<-original(...);tests[[length(tests)+1L]]<<-value;value
 }
 on.exit(env$longitudinal_hausman_test<-original)
 set.seed(718);conditions<-list()
 output<-capture.output(value<-withCallingHandlers(do.call(env$prepare_longitudinal_analysis_result,c(list(data=d,outcome='y',id='id',time='time',predictors=c('x1','x2'),family='gaussian',variable_info=data.frame(name=names(d),measurement='continuous')),args)),
 warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=normalize(value),conditions=conditions,output=output,rng=.Random.seed),calls=calls,tests=tests)
}
passed<-0L
check<-function(d,args,signal='quiet',both=TRUE){
 a<-run(old,d,args,signal);b<-run(new,d,args,signal)
 stopifnot(length(a$snapshot$value)==1L,identical(a$snapshot,b$snapshot,num.eq=FALSE),
  a$calls==if(both)2L else 1L,b$calls==if(both&&signal!='quiet')2L else 1L)
 if(signal!='error'){
  stopifnot(length(a$tests)==a$calls,length(b$tests)==b$calls)
  for(test in c(a$tests,b$tests))stopifnot(identical(test,a$tests[[1]],num.eq=FALSE))
 }
 passed<<-passed+1L;cat('PASS',passed,args$model_type,signal,'Hausman calls',a$calls,b$calls,'\n')
}
set.seed(942);subjects<-100L;n<-subjects*6L
d<-data.frame(id=rep(1:subjects,each=6),time=rep(0:5,subjects),x1=rnorm(n),x2=rnorm(n))
d$y<-.3*d$time+.3*d$x1+rep(rnorm(subjects),each=6)+rnorm(n)
for(model in c('panel_fe','panel_re')){
 for(mode in c('all','hausman_off','checks_off'))for(signal in c('quiet','warning','message','rng','error')){
  args<-list(model_type=model)
  if(mode=='hausman_off')args$check_options<-list(hausman=FALSE)
  if(mode=='checks_off')args$assumption_checks<-FALSE
  check(d,args,signal,both=mode=='all')
 }
 missing<-d;missing$y[1:6]<-NA_real_;check(missing[rev(seq_len(n)),],list(model_type=model))
}
# The helper's default (no provider) path also preserves its original result.
formula<-longitudinal_formula('y',c('time','x1','x2'))
stopifnot(identical(old$longitudinal_hausman_test(d,formula,'id','time'),new$longitudinal_hausman_test(d,formula,'id','time'),num.eq=FALSE))
cat('PASS:',passed,'Hausman reuse cases and independent raw-test comparison\n')
