.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
source('scripts/fixtures/longitudinal_mermod_normalize.R')
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_longitudinal.R',env)
for(env in list(old,new)){
 f<-env$longitudinal_panel_driscoll_kraay_summary
 b<-body(f);last<-length(b)
 b[[last]]<-substitute(list(text=TEXT,hc1_se=hc1_se,dk_se=dk_se,ratios=ratios),list(TEXT=b[[last]]));body(f)<-b
 env$.raw_summary<-f
 env$longitudinal_panel_driscoll_kraay_summary<-local({target<-env;reference<-identical(env,old)
  function(model,hc1_table=NULL){v<-target$.raw_summary(model,if(reference)NULL else hc1_table);target$.ratios[[length(target$.ratios)+1L]]<-v;v$text}
 })
}
.hc1_signal<-'quiet';.hc1_calls<-0L
trace('vcovHC.plm',where=asNamespace('plm'),print=FALSE,tracer=quote({
 .GlobalEnv$.hc1_calls<-.GlobalEnv$.hc1_calls+1L
 if(.GlobalEnv$.hc1_signal=='warning')warning('HC1 test warning')
 if(.GlobalEnv$.hc1_signal=='message')message('HC1 test message')
 if(.GlobalEnv$.hc1_signal=='rng')runif(1)
 if(.GlobalEnv$.hc1_signal=='error')stop('HC1 test error')
}))
run<-function(env,d,model,signal){
 .GlobalEnv$.hc1_signal<-signal;.GlobalEnv$.hc1_calls<-0L;env$.ratios<-list()
 set.seed(718);conditions<-list()
 output<-capture.output(value<-withCallingHandlers(env$prepare_longitudinal_analysis_result(d,'y','id','time',predictors=c('x1','x2'),model_type=model,family='gaussian',variable_info=data.frame(name=names(d),measurement='continuous')),
 warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=normalize(value),conditions=conditions,output=output,rng=.Random.seed,raw=env$.ratios),calls=.GlobalEnv$.hc1_calls)
}
set.seed(943);n<-600L;d<-data.frame(id=rep(1:100,each=6),time=rep(0:5,100),x1=rnorm(n),x2=rnorm(n))
d$y<-.3*d$time+.3*d$x1+rep(rnorm(100),each=6)+rnorm(n)
passed<-0L
for(model in c('panel_fe','panel_re'))for(signal in c('quiet','warning','message','rng','error')){
 a<-run(old,d,model,signal);b<-run(new,d,model,signal)
 stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE),a$calls-b$calls==if(signal=='quiet')2L else 0L)
 if(signal!='error')stopifnot(length(a$snapshot$value)==1L,length(a$snapshot$raw)==2L)
 passed<-passed+1L;cat('PASS',model,signal,'HC1 calls',a$calls,b$calls,'\n')
}
untrace('vcovHC.plm',where=asNamespace('plm'))
cat('PASS:',passed,'HC1 cases including raw standard errors and ratios\n')
