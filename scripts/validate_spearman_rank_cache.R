.libPaths(R.home('library'));scope<-new.env(parent=.GlobalEnv);sys.source('R/analysis_correlation.R',scope)
engine<-scope$correlation_rank_test(4L);count<-0L
capture<-function(f,x,y){
 set.seed(99);conditions<-character()
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(f(x,y,method='spearman',exact=FALSE),error=function(e)list(error=conditionMessage(e))),warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
for(n in c(3L,10L,100L))for(kind in c('plain','ties','missing','infinite','constant','named','increasing','decreasing','empty','allmissing','class')) {
 set.seed(33);x<-rnorm(n);y<-rnorm(n)
 if(kind=='ties'){x<-round(x);y<-round(y)}
 if(kind=='missing'){x[1]<-NA;y[n]<-NA}
 if(kind=='infinite'){x[1]<-Inf;y[n]<--Inf}
 if(kind=='constant')x[]<-1
 if(kind=='named')names(x)<-as.character(seq_len(n))
 if(kind=='increasing')y<-x
 if(kind=='decreasing')y<--x
 if(kind=='empty'){x<-numeric();y<-numeric()}
 if(kind=='allmissing')x[]<-NA_real_
 if(kind=='class')class(x)<-'AsIs'
 for(repeat_call in 1:2)stopifnot(identical(capture(stats::cor.test,x,y),capture(engine,x,y),num.eq=FALSE),length(environment(engine)$entries)<=4L)
 count<-count+1L
}
cat('PASS',count,'direct htest/diagnostic/stdout/RNG conditions with repeated calls and bounded eviction\n')

