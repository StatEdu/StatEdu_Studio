.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_survival.R',env)
sys.source('scripts/fixtures/survival_explicit_map_reference.R',old)
capture<-function(env,fun,args){
 set.seed(919);ds<-list()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(env[[fun]],args),
 warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
 message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
as.character.event_probe<-function(x,...){warning('event warning');message('event message');runif(1);as.character(unclass(x))}
`[.event_probe`<-function(x,...){structure(NextMethod('['),class='event_probe')}
valid<-data.frame(raw_value=c('0','1'),role=c('censored','event_of_interest'))
maps<-list(NULL,valid,data.frame(raw_value=c(' 0 ','1','1'),role=c(' CENSORED ','event_of_interest','exclude'),label=c('a',NA,'b')),data.frame(raw_value='0'),list(raw_value='0',role='censored'),transform(valid,role=factor(role)),structure(valid,class=c('event_table','data.frame')))
values<-list(integer(),numeric(),c(0L,1L,NA_integer_),c(0,-0,1,NA,NaN,Inf,-Inf,1e-300,1e300),c('0',' 1 ',NA,'bad'),factor(c('0','1',NA)),structure(0:1,names=c('a','b')),matrix(0:1,1),as.Date('2020-01-01')+0:1,structure(0:1,class='event_probe'),list(0,1),c(FALSE,TRUE,NA),as.raw(0:1),c(1+2i,NA_complex_))
checks<-0L
for(v in values)for(m in maps)for(interest in list('1',character(),structure(1,class='event_probe'))){
 args<-list(values=v,event_map=m,event_of_interest=interest)
 stopifnot(identical(capture(old,'survival_normalize_event_map',args),capture(new,'survival_normalize_event_map',args),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact helper comparisons (values, attributes, diagnostics/stdout/RNG)\n')
checks<-0L
for(shape in c('single_record','entry_exit','start_stop'))for(m in maps[1:6])for(missing in c(FALSE,TRUE)){
 data<-data.frame(time=1:20,event=rep(0:1,10),entry=0,start=0,id=1:20,group=rep(letters[1:2],10))
 if(missing){data$event[c(1,5)]<-NA;data$group[7]<-NA;data$time[9]<--1}
 s<-survival_legacy_settings('time','event','1','group');s['event_map']<-list(m);s$data_shape<-shape
 if(shape=='entry_exit')s$roles$entry<-'entry'
 if(shape=='start_stop'){s$roles$start<-'start';s$roles$stop<-'time';s$roles$subject_id<-'id'}
 args<-list(data=data,settings=s)
 stopifnot(identical(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact full preflight comparisons\n')
