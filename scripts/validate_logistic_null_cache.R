.libPaths(R.home('library'));source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
scopes<-lapply(c('baseline','current'),function(mode){e<-new.env(parent=.GlobalEnv);sys.source('R/analysis_logistic.R',e);e});names(scopes)<-c('baseline','current')
scopes$baseline$logistic_cached_null<-function(cache,family,data,dependent,fit)fit()
normalize<-function(x){
 if(inherits(x,'formula')){environment(x)<-.GlobalEnv;return(x)}
 if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize(x[[i]]))
 x
}
fixture<-function(kind,n=50000L){
 set.seed(719);d<-as.data.frame(matrix(rnorm(n*6),n,6));names(d)<-paste0('x',1:6)
 eta<-.3*d$x1-.2*d$x2+.1*d$x3
 if(kind=='binary')d$y<-factor(rbinom(n,1,plogis(eta)))
 else d$y<-factor(cut(eta+rlogis(n),c(-Inf,-.6,.6,Inf),labels=c('a','b','c')),ordered=kind=='ordered')
 list(d=d,info=data.frame(name=names(d),measurement=c(rep('continuous',6),kind)))
}
capture<-function(f){
 set.seed(99);conditions<-character()
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(f(),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=normalize(value),conditions=conditions,stdout=stdout,rng=.Random.seed)
}
run<-function(mode,input)capture(function()scopes[[mode]]$prepare_logistic_analysis_results(input$d,'y',c('x1','x2'),c('x3','x4'),c('x5','x6'),variable_info=input$info))

count<-0L
for(kind in c('binary','ordered','category'))for(case in c('plain','missing','factor','constant','duplicate','weights')){
 input<-fixture(kind,360L)
 if(case=='missing'){input$d$x1[1:10]<-NA_real_;input$d$x6[11:20]<-NA_real_}
 if(case=='factor'){input$d$x1<-factor(rep(c('a','b','c'),120));input$info$measurement[1]<-'category'}
 if(case=='constant')input$d$x1<-1
 if(case=='duplicate')input$d$x2<-input$d$x1
 if(case=='weights')input$d$x3<-round(input$d$x3)
 a<-run('baseline',input);b<-run('current',input)
 stopifnot(identical(a,b,num.eq=FALSE),is.null(a$value$error));count<-count+1L
}
cat('PASS',count,'full results/diagnostics/stdout/RNG conditions\n')

calls<-0L
cache<-new.env(parent=emptyenv())
d<-data.frame(y=factor(c('a','b','a')))
fit<-function(){calls<<-calls+1L;list(value=2)}
helper<-scopes$current$logistic_cached_null
helper(cache,'binary',d,'y',fit);helper(cache,'binary',d,'y',fit);stopifnot(calls==1L)
changed<-d;changed$y<-rev(changed$y);changed$y[1]<-'b'
helper(cache,'binary',changed,'y',fit);stopifnot(calls==2L)
row_changed<-changed;row.names(row_changed)<-c('u','v','w')
helper(cache,'binary',row_changed,'y',fit);stopifnot(calls==3L)
helper(cache,'multinomial',row_changed,'y',fit);stopifnot(calls==4L)
for(type in c('warning','message','error')){
 e<-new.env(parent=emptyenv());before<-calls
 f<-function(){calls<<-calls+1L;switch(type,warning=warning('test'),message=message('test'),error=stop('test'));list(value=2)}
 a<-capture(function()helper(e,'binary',d,'y',f));b<-capture(function()helper(e,'binary',d,'y',f))
 stopifnot(calls==before+2L,identical(a,b,num.eq=FALSE),!length(ls(e)))
}
counts<-list()
actual<-scopes$current$logistic_cached_null
for(kind in c('binary','ordered','category')){
 misses<-hits<-0L
 scopes$current$logistic_cached_null<-function(cache,family,data,dependent,fit){
  before<-misses
  value<-actual(cache,family,data,dependent,function(){misses<<-misses+1L;fit()})
  if(misses==before)hits<<-hits+1L
  value
 }
 result<-run('current',fixture(kind,5000L))
 stopifnot(is.null(result$value$error),length(result$value)==3L,misses==1L,hits==2L)
 counts[[kind]]<-data.frame(kind=kind,misses=misses,hits=hits)
}
scopes$current$logistic_cached_null<-actual
cat('PASS: y/rows/family invalidation; warning/message/error preserved; 1 fit + 2 hits for each of 3 model families\n')
