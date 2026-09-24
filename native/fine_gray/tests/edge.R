source('native/fine_gray/tests/common.R')
# Evidence is written beside the explicitly selected build.
run<-function(f,d,init,maxiter) {
 calls<-0L;e<-new.env(parent=environment(f));delegate<-get('.Fortran',environment(f),inherits=TRUE)
 e$.Fortran<-function(...) {args<-list(...);if(identical(args[[1]],'crrvv'))calls<<-calls+1L;delegate(...)}
 environment(f)<-e;conditions<-character();set.seed(88)
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(
  f(ftime=d$time,fstatus=d$status,cov1=d$x,cengroup=d$group,init=rep(init,ncol(d$x)),maxiter=maxiter),
  error=function(e)list(error=conditionMessage(e))),
  warning=function(w){conditions<<-c(conditions,paste0('warning: ',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,paste0('message: ',conditionMessage(m)));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed,variance_calls=calls)
}
rows<-list()
for(kind in c('plain','ties','all_censored','all_target','no_target','zero_time','large_time','tiny_x','large_x','constant')) {
 d<-make(80,3)
 if(kind=='ties')d$time<-round(d$time,1)
 if(kind=='all_censored')d$status[]<-0L
 if(kind=='all_target')d$status[]<-1L
 if(kind=='no_target')d$status[d$status==1L]<-2L
 if(kind=='zero_time')d$time[]<-0
 if(kind=='large_time')d$time<-d$time*1e200
 if(kind=='tiny_x')d$x<-d$x*1e-150
 if(kind=='large_x')d$x<-d$x*1e150
 if(kind=='constant')d$x[,1]<-1
 for(init in c(0,1,-1,1000,-1000))for(maxiter in c(0L,10L)) {
  a<-run(versions$installed,d,init,maxiter);b<-run(versions$original,d,init,maxiter);c<-run(versions$candidate,d,init,maxiter)
  equal<-identical(a,b,num.eq=FALSE)&&identical(a,c,num.eq=FALSE)
  rows[[length(rows)+1L]]<-data.frame(kind=kind,init=init,maxiter=maxiter,equal=equal,variance_calls=a$variance_calls,error=if(is.null(a$value$error))'' else a$value$error,converged=isTRUE(a$value$converged))
  if(!equal)saveRDS(list(installed=a,original=b,candidate=c),file.path(root,paste0('mismatch-',length(rows),'.rds')))
 }
 cat('Checked',kind,'\n');flush.console()
}
result<-do.call(rbind,rows);write.csv(result,file.path(root,'coverage.csv'),row.names=FALSE)
print(aggregate(cbind(conditions=rep(1,nrow(result)),variance_calls=result$variance_calls)~equal,data=result,sum))
stopifnot(all(result$equal))

