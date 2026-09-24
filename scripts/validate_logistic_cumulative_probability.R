expressions <- parse('R/analysis_logistic.R', encoding='UTF-8')
helper <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) && identical(x[[2]], as.name('logistic_cumulative_probability')), as.list(expressions))
stopifnot(length(helper)==1L)
eval(helper[[1L]])
reference<-function(p)t(apply(p,1L,cumsum))
capture<-function(fn,p){
 warnings<-character();set.seed(87)
 value<-withCallingHandlers(tryCatch(fn(p),error=conditionMessage),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
 list(value=value,warnings=warnings,rng=.Random.seed)
}
count<-0L
for(n in c(0L,1L,5L,49999L,50000L,50001L))for(k in c(1L,3L,19L,20L,21L)){
 p<-matrix(rep(c(0,.1,.2,.7),length.out=n*k),n,k)
 for(named in c(FALSE,TRUE)){
  if(named)dimnames(p)<-list(row=if(n)paste0('r',seq_len(n))else NULL,column=paste0('c',seq_len(k)))
  stopifnot(identical(capture(reference,p),capture(logistic_cumulative_probability,p),num.eq=FALSE));count<-count+1L
 }
}
for(value in c(NA_real_,NaN,Inf,-Inf)){
 p<-matrix(.05,50000,20);p[2,2]<-value
 stopifnot(identical(capture(reference,p),capture(logistic_cumulative_probability,p),num.eq=FALSE));count<-count+1L
}
cat('PASS:',count,'matrix boundary cases; values/attributes/conditions/RNG identical.\n')
