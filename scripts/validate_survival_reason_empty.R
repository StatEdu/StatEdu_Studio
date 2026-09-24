source('scripts/validate_survival_explicit_map.R')
old$survival_normalize_event_map<-new$survival_normalize_event_map
sys.source('scripts/fixtures/survival_reason_empty_reference.R',old)
# Preserve the rejected candidate for reproducible comparisons after product restoration.
sys.source('scripts/fixtures/survival_reason_empty_candidate.R',new)
checks<-0L
for(n in c(0L,1L,30L,120L))for(shape in c('single_record','entry_exit','start_stop'))for(kind in c('complete','overlap','disjoint','all')){
 set.seed(778);data<-data.frame(time=seq_len(n)+1,event=rep(0:1,length.out=n),group=rep(letters[1:2],length.out=n),id=seq_len(n),start=rep(0,n),entry=rep(0,n))
 for(i in 1:6){
  x<-rnorm(n)
  rows<-switch(kind,complete=integer(),overlap=which(seq_len(n)%%2==0),disjoint=which(seq_len(n)%%6==i-1),all=seq_len(n))
  x[rows]<-NA;data[[paste0('x',i)]]<-x
 }
 if(n>4&&kind!='complete'){data$time[2]<--1;data$event[3]<-NA;data$group[4]<-NA}
 s<-survival_legacy_settings('time','event','1','group',paste0('x',1:6));s$data_shape<-shape
 if(shape=='entry_exit')s$roles$entry<-'entry'
 if(shape=='start_stop'){s$roles$start<-'start';s$roles$stop<-'time';s$roles$subject_id<-'id'}
 args<-list(data=data,settings=s)
 stopifnot(identical(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args),num.eq=FALSE));checks<-checks+1L
 # Repeated roles preserve checks and the first occurrence of each reason.
 s$roles$covariates<-c('group','x3','x1','x3','x6','x2')
 args$settings<-s
 stopifnot(identical(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'reason label full result/diagnostics/stdout/RNG comparisons\n')
is.na.cov_probe<-function(x){warning('covariate missing check');message('covariate message');runif(1);is.na(unclass(x))}
for(duplicate in c(FALSE,TRUE)){
 data<-data.frame(time=1:20,event=rep(0:1,10),x=rep(c(1,NA),10),y=rep(c(NA,1),10))
 data$x<-structure(data$x,class='cov_probe');data$y<-structure(data$y,class='cov_probe')
 s<-survival_legacy_settings('time','event','1',covariates=if(duplicate)c('x','x','y')else c('x','y'))
 args<-list(data=data,settings=s)
 stopifnot(identical(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args),num.eq=FALSE))
}
cat('PASS: 2 custom missing-check diagnostic/RNG comparisons\n')
for(shape in c('entry_exit','start_stop'))for(invalid in c('text','date')){
 data<-data.frame(time=c('bad','bad','3','4'),entry=c('bad','bad','0','0'),start=c('bad','bad','0','0'),event=c(0,1,0,1),id=1:4,group=c(NA,NA,'a','b'),x=c(NA,NA,1,2))
 if(invalid=='date')for(name in c('time','entry','start'))data[[name]]<-as.Date('2020-01-01')+1:4
 s<-survival_legacy_settings('time','event','1','group',c('group','x'));s$data_shape<-shape
 if(shape=='entry_exit')s$roles$entry<-'entry'
 else {s$roles$start<-'start';s$roles$stop<-'time';s$roles$subject_id<-'id'}
 a<-capture(old,'survival_preflight',list(data=data,settings=s));b<-capture(new,'survival_preflight',list(data=data,settings=s))
 stopifnot(identical(a,b,num.eq=FALSE),!isTRUE(a$value$ok),any(grepl('invalid_time_encoding',a$value$row_audit$exclusion_reasons,fixed=TRUE)))
}
cat('PASS: 4 repeated time-encoding/group-reason comparisons\n')
