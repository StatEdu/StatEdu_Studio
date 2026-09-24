.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
calls<-0L;ctx<-1L
render<-function(plot_function,result,width,height,res){calls<<-calls+1L;paste(result,width,height,res)}
cache<-correlation_export_image_cache(render=render,context=function()ctx)
f<-function(x)x
run<-function(value=1,width=10,height=20,res=96,fun=f)cache$render(fun,value,width,height,res)
run();run();stopifnot(calls==1L)
run(2);run(2);stopifnot(calls==2L)
run(3);run(1);stopifnot(calls==4L,length(environment(cache$render)$entries)==2L)
for(args in list(list(width=11),list(height=21),list(res=120),list(fun=function(x)x+1))){
 before<-calls;do.call(run,args);do.call(run,args);stopifnot(calls==before+1L)
}
ctx<-2L;before<-calls;run();run();stopifnot(calls==before+1L)
cache$clear();stopifnot(length(environment(cache$render)$entries)==0L)
before<-calls;run();stopifnot(calls==before+1L)
other<-correlation_export_image_cache(render=render,context=function()ctx)
before<-calls;other$render(f,1,10,20,96);stopifnot(calls==before+1L)
tiny<-correlation_export_image_cache(render=render,context=function()0,max_bytes=1)
before<-calls;tiny$render(f,1);tiny$render(f,1);stopifnot(calls==before+2L,length(environment(tiny$render)$entries)==0L)
for(kind in c('warning','message','rng','error')){
 count<-0L;conditions<-character()
 mock<-function(...){count<<-count+1L;switch(kind,warning=warning('expected'),message=message('expected'),rng=runif(1),error=stop('expected'));'value'}
 tested<-correlation_export_image_cache(render=mock,context=function()0)
 for(i in 1:2)tryCatch(withCallingHandlers(tested$render(f,1),
  warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e){conditions<<-c(conditions,conditionMessage(e))})
 stopifnot(count==2L,length(environment(tested$render)$entries)==0L)
 if(kind!='rng')stopifnot(length(conditions)==2L)
}
cat('PASS: exact key hits, FIFO capacity, arguments/function/context invalidation, clearing, isolation, byte-limit bypass, warning/message/RNG/error bypass without retries.\n')
