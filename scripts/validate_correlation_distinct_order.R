source_path<-Sys.getenv('STATEDU_CORRELATION_TEST_SOURCE','R/analysis_correlation.R')
scope<-new.env(parent=.GlobalEnv);sys.source(source_path,scope)
values<-list(rep(c(1,2,3),100),rep(c(1L,2L,3L),100),rep(c(0,-0,1),100),
 rep(c(1,1+1e-14,2),100),rep(c(1e-100,1e100,2),100),rep(c(1,NA_real_,NaN,Inf,-Inf),60),
 rep(1:64,5),rep(1:65,5),numeric(),rep(1:3,10),setNames(rep(1:3,100),paste0('n',1:300)))
levels_list<-list(c(1,2,3),c('3','1','2','unused'),c(1,1,2),c('1','1','2'),
 c(NA_character_,'NaN','Inf','-Inf','0','1','2','3'),character(),c(1,1+1e-14,2),c(1e-100,1e100,2),as.character(64:1))
record<-function(f) {
 set.seed(19);conditions<-character()
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(f(),error=function(e)list(error=conditionMessage(e))),
  warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
main<-function(){
 previous<-options();on.exit(options(previous),add=TRUE);count<-0L
 for(scipen in c(-999,0,999))for(x in values)for(levels in levels_list) {
  options(scipen=scipen)
  expected<-record(function()ordered(x,levels=levels));cache<-new.env(parent=emptyenv())
  for(repeat_call in 1:2) {
   actual<-record(function()scope$correlation_ordered_vector(x,levels,cache,'variable'))
   if(!identical(expected,actual,num.eq=FALSE)){print(all.equal(expected,actual));stop('Ordered conversion differs')}
  }
  # Changed missing pattern / value order must not reuse stale results.
  y<-rev(x);expected_y<-record(function()ordered(y,levels=levels))
  stopifnot(identical(expected_y,record(function()scope$correlation_ordered_vector(y,levels,cache,'variable')),num.eq=FALSE))
  count<-count+1L
 }
 cat('PASS:',count,'input/level/format conditions; first, repeated and reordered calls compared to base ordered()\n')
}
main()
