source('scripts/validate_survival_explicit_map.R')
# Compare only the new preflight change; both sides use the current map helper.
old$survival_normalize_event_map<-new$survival_normalize_event_map
sys.source('scripts/fixtures/survival_event_observed_reference.R',old)
checks<-0L
events<-list(rep(0:1,10),rep(c(0,1,NA,NaN,Inf,-Inf,1e-300,1e300,0,-0),2),
 rep(c('0',' 1 ',NA,'bad'),5),factor(rep(c('0','1',NA,'2'),5)),structure(rep(0:1,10),class='event_probe'),
 setNames(rep(0:1,10),letters[1:20]),rep(c(TRUE,FALSE),10),as.Date('2020-01-01')+1:20)
for(shape in c('single_record','entry_exit','start_stop'))for(m in maps)for(ev in events){
 data<-data.frame(time=1:20,entry=0,start=0,id=1:20,group=rep(letters[1:2],10));data$event<-ev
 s<-survival_legacy_settings('time','event','1','group');s['event_map']<-list(m);s$data_shape<-shape
 if(shape=='entry_exit')s$roles$entry<-'entry'
 if(shape=='start_stop'){s$roles$start<-'start';s$roles$stop<-'time';s$roles$subject_id<-'id'}
 args<-list(data=data,settings=s)
 stopifnot(identical(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'new observed-text full preflight comparisons including diagnostics/stdout/RNG\n')
as.character.event_options_probe<-function(x,...){options(scipen=999);as.character(unclass(x))}
for(location in c('interest','map')){
 data<-data.frame(time=1:20,event=rep(c(0,1e20),10))
 s<-survival_legacy_settings('time','event','1');s$event_map<-valid
 if(location=='interest')s$event_of_interest<-structure('1',class='event_options_probe')
 else s$event_map$raw_value<-structure(c('0','1'),class='event_options_probe')
 saved_options<-options();options(scipen=0)
 a<-capture(old,'survival_preflight',list(data=data,settings=s));after_old<-getOption('scipen')
 options(scipen=0)
 b<-capture(new,'survival_preflight',list(data=data,settings=s));after_new<-getOption('scipen')
 options(saved_options)
 stopifnot(identical(a,b,num.eq=FALSE),identical(after_old,after_new))
}
cat('PASS: 2 custom conversion option side-effect comparisons\n')
