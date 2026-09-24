.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
set.seed(42);y<-matrix(rnorm(480),ncol=4);g<-factor(rep(LETTERS[1:4],each=30));covs<-data.frame(x=rnorm(120))
original<-mixed_rm_adjusted_model;calls<-0L
mixed_rm_adjusted_model<-function(...){calls<<-calls+1L;original(...)}
run<-function()mixed_rm_descriptives(y,g,paste0('t',1:4),covs,include_within=FALSE,include_posthoc=FALSE)
first<-run();stopifnot(calls==4L)
second<-run();stopifnot(calls==8L,identical(first,second))
# A new summary must not retain models from the previous data.
y[1,1]<-y[1,1]+4;third<-run();stopifnot(calls==12L,!identical(first,third))
for(kind in c('warning','message','rng','error')){
 calls<-0L;seen<-0L
 mixed_rm_adjusted_model<-function(...){calls<<-calls+1L;switch(kind,warning=warning('expected'),message=message('expected'),rng=runif(1),error=stop('expected'));original(...)}
 value<-withCallingHandlers(run(),warning=function(w){seen<<-seen+1L;invokeRestart('muffleWarning')},message=function(m){seen<<-seen+1L;invokeRestart('muffleMessage')})
 stopifnot(calls==16L)
 if(kind %in% c('warning','message'))stopifnot(seen==16L)
}
cat('PASS: one fit per time point; per-summary lifetime; warning/message/RNG/error paths retain per-cell behavior\n')
