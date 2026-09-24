.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_survival.R',env)
sys.source('scripts/fixtures/survival_duration_reference.R',old)
capture<-function(env,data,settings){
 set.seed(119);ds<-list()
 stdout<-capture.output(value<-withCallingHandlers(env$survival_preflight(data,settings),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
 stopifnot(is.list(value),is.list(value$counts))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
checks<-0L
for(shape in c('single_record','entry_exit','start_stop')){
 for(times in list(1:8,c(0,-1,NA,NaN,Inf,-Inf,1e-300,1e300),as.character(1:8),factor(1:8),as.Date('2020-01-01')+0:7)){
  data<-data.frame(time=times,event=rep(0:1,4),entry=rep(0,8),start=rep(0,8),id=1:8)
  settings<-survival_legacy_settings('time','event','1');settings$data_shape<-shape
  if(shape=='entry_exit')settings$roles$entry<-'entry'
  if(shape=='start_stop'){settings$roles$start<-'start';settings$roles$stop<-'time';settings$roles$subject_id<-'id'}
  a<-capture(old,data,settings);b<-capture(new,data,settings)
  stopifnot(identical(a,b,num.eq=FALSE));checks<-checks+1L
 }
}
cat('PASS:',checks,'exact full preflight comparisons across three duration layouts, diagnostics/stdout/RNG\n')
