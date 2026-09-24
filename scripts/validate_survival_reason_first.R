source('scripts/validate_survival_explicit_map.R')
old$survival_normalize_event_map<-new$survival_normalize_event_map
sys.source('scripts/fixtures/survival_reason_first_reference.R',old)
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
cat('PASS:',checks,'reason first full result/diagnostics/stdout/RNG comparisons\n')
is.na.cov_probe<-function(x){warning('covariate missing check');message('covariate message');runif(1);is.na(unclass(x))}
for(duplicate in c(FALSE,TRUE)){
 data<-data.frame(time=1:20,event=rep(0:1,10),x=rep(c(1,NA),10),y=rep(c(NA,1),10))
 data$x<-structure(data$x,class='cov_probe');data$y<-structure(data$y,class='cov_probe')
 s<-survival_legacy_settings('time','event','1',covariates=if(duplicate)c('x','x','y')else c('x','y'))
 args<-list(data=data,settings=s)
 stopifnot(identical(capture(old,'survival_preflight',args),capture(new,'survival_preflight',args),num.eq=FALSE))
}
cat('PASS: 2 custom missing-check diagnostic/RNG comparisons\n')
