source('R/analysis_survival.R')
original<-function(x)apply(x,2,stats::quantile,probs=c(.025,.975),na.rm=TRUE,names=FALSE)
capture<-function(fn){
 conditions<-character()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(),
  warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
set.seed(471);checks<-0L
for(n in c(0L,1L,60L,1023L,1024L,1025L,2000L))for(k in c(0L,1L,3L))for(kind in c('random','missing','all_missing','constant','named','dim_names','infinite','integer')){
 x<-matrix(runif(n*k),n,k)
 if(kind=='missing'&&length(x))x[seq_len(min(5,length(x)))]<-NA_real_
 if(kind=='all_missing')x[]<-NA_real_
 if(kind=='constant')x[]<-1
 if(kind=='named')dimnames(x)<-list(if(n)paste0('r',seq_len(n))else NULL,if(k)paste0('c',seq_len(k))else NULL)
 if(kind=='dim_names')dimnames(x)<-list(rows=if(n)paste0('r',seq_len(n))else NULL,columns=if(k)paste0('c',seq_len(k))else NULL)
 if(kind=='infinite'&&length(x))x[seq_len(min(3,length(x)))]<-c(Inf,-Inf,NaN)[seq_len(min(3,length(x)))]
 if(kind=='integer')storage.mode(x)<-'integer'
 stopifnot(identical(capture(function()original(x)),capture(function()survival_bootstrap_intervals(x)),num.eq=FALSE))
 checks<-checks+1L
}
cat('PASS:',checks,'threshold/boundary matrices, exact values/attributes/conditions/stdout/RNG\n')
