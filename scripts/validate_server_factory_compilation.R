source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
reference<-create_app_server
args<-commandArgs(TRUE)
if(length(args)) {
 e<-new.env(parent=.GlobalEnv);sys.source(args[[1]],e);reference<-e$create_app_server;environment(reference)<-.GlobalEnv
} else {
 b<-body(reference);last<-length(b)
 stopifnot(identical(b[[last]][[1]],as.name('eval')))
 b[[last]]<-b[[last]][[2]][[2]];body(reference)<-b
}
capture<-function(fn,kind) {
 set.seed(91);conditions<-character()
 value<-withCallingHandlers(tryCatch({
  server<-fn({
   if(kind=='error')stop('version failure')
   if(kind=='warning')warning('version warning')
   if(kind=='message')message('version message')
   if(kind=='rng')runif(1)else if(kind=='null')NULL else 'test-version'
  })
  list(formals=formals(server),body=body(server),version=environment(server)$app_version,parent=parent.env(environment(server)))
 },error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,conditions=conditions,rng=.Random.seed)
}
for(kind in c('normal','null','error','warning','message','rng'))stopifnot(identical(capture(reference,kind),capture(create_app_server,kind),num.eq=FALSE))
cat('PASS: six server closure/body/forced-version/condition/RNG comparisons.\n')
