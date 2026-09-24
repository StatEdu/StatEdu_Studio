source('R/analysis_correlation.R')
old<-new.env();new<-new.env()
old$correlation_rank_test<-correlation_rank_test;new$correlation_rank_test<-correlation_rank_test
capture<-function(fn,x,y,method,exact){
 set.seed(91);conditions<-character()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(x,y,method,exact),
  warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
set.seed(73)
inputs<-list(double=rnorm(40),integer=sample(1:8,40,TRUE),constant=rep(2,40),
 missing=rep(c(NA_real_,NaN,1,2,3),8),infinite=rep(c(-Inf,1,2,Inf),10),
 zeros=rep(c(0,-0,1,-1),10),empty=numeric(),one=1,two=c(1,2),
 named=setNames(rnorm(40),paste0('v',1:40)),date=as.Date('2020-01-01')+1:40)
count<-0L
for(limit in c(1L,2L,20L))for(method in c('spearman','pearson','kendall'))for(exact in list(FALSE,TRUE,NULL)){
 a<-old$correlation_rank_test(limit);b<-new$correlation_rank_test(limit, cache_unique=TRUE)
 for(x in inputs)for(repeat_id in 1:2){
  y<-rev(x)
  stopifnot(identical(capture(a,x,y,method,exact),capture(b,x,y,method,exact),num.eq=FALSE))
  count<-count+1L
 }
 stopifnot(length(environment(b)$entries)<=limit)
}
cat('PASS:',count,'boundary/reuse cases; full values, conditions, stdout and RNG identical.\n')
