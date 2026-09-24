source('R/analysis_regression.R')
capture<-function(fn,x){
 conditions<-character();stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(x),
 warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout)
}
set.seed(281);count<-0L
for(n in c(20L,200L))for(p in c(2L,5L))for(kind in c('ordinary','constant','singular','missing','named','factor')){
 x<-matrix(rnorm(n*p),n,p);colnames(x)<-paste0('x',1:p);x<-cbind('(Intercept)'=1,x)
 if(kind=='constant')x[,2]<-2
 if(kind=='singular')x[,3]<-x[,2]
 if(kind=='missing')x[1,2]<-NA
 if(kind=='named')rownames(x)<-paste0('row',seq_len(n))
 if(kind=='factor')x<-model.matrix(~a+b,data.frame(a=factor(rep(1:3,length.out=n)),b=rnorm(n)))
 fn<-regression_collinearity_cache()
 for(i in 1:2){seed<-.Random.seed;stopifnot(identical(capture(coefficient_collinearity,x),capture(fn,x),num.eq=FALSE),identical(seed,.Random.seed));count<-count+1L}
}
local({
 opts<-options()[c('contrasts','na.action')];on.exit(options(opts))
 x<-matrix(1:6,3);calls<-0L
 compute<-function(x){calls<<-calls+1L;sum(x)}
 fn<-regression_collinearity_cache(compute)
 fn(x);fn(x);stopifnot(calls==1L)
 y<-x;y[1]<-99;fn(y);stopifnot(calls==2L)
 rownames(y)<-letters[1:3];fn(y);stopifnot(calls==3L)
 options(na.action='na.exclude');fn(y);stopifnot(calls==4L)
 options(contrasts=c('contr.sum','contr.poly'));fn(y);stopifnot(calls==5L)
 fresh<-regression_collinearity_cache(compute);fresh(y);stopifnot(calls==6L)
 large<-matrix(0,1001,1000);fn(large);fn(large);stopifnot(calls==8L)
 for(kind in c('warning','message','error','rng')){
  calls<-0L
  compute<-function(x){calls<<-calls+1L;switch(kind,warning=warning('diagnostic'),message=message('diagnostic'),error=stop('diagnostic'),rng=runif(1));sum(x)}
  fn<-regression_collinearity_cache(compute)
  for(i in 1:2){set.seed(71);a<-capture(compute,x);after<-.Random.seed;set.seed(71);b<-capture(fn,x);stopifnot(identical(a,b,num.eq=FALSE),identical(after,.Random.seed))}
  stopifnot(calls==4L)
 }
 saved<-.Random.seed;rm('.Random.seed',envir=.GlobalEnv);fn<-regression_collinearity_cache(function(x)sum(x));fn(x);fn(x)
 stopifnot(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE));assign('.Random.seed',saved,envir=.GlobalEnv)
})
cat('PASS:',count,'numeric/condition cases; input/context/fresh-analysis invalidation, size cap, diagnostic/error/RNG bypass and absent seed\n')
