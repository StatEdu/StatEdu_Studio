.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER=normalizePath(R.home('library'),winslash='/'),STATEDU_NO_PACKAGE_INSTALL='true')
id<-as.integer(commandArgs(TRUE)[1]);stopifnot(!is.na(id),id %in% 1:3)
root<-paste0('output/sem-gradient-preparation-20260915/run-',id)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
stopifnot(!length(list.files(root,pattern='times[.]csv$')))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
wanted<-c('node','edge','stable_data','stable_snapshot','stable_fixture');found<-0L
for(x in parse('scripts/benchmark_sem_product_bootstrap_5000.R')) {
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&as.character(x[[2]])[1] %in% wanted) {
  eval(x);found<-found+1L
 }
}
stopifnot(found==length(wanted))
fixture<-stable_fixture('all_pairs_dmc',9L,20260833L)
original<-suppressWarnings(run_structural_canvas_analysis(fixture$snapshot,fixture$data,'sem',
 estimator='ML',missing='fiml',std_lv=FALSE,ordered=character(0),nominal=character(0),residual_variance_fixes=numeric(0)))
stopifnot(original$converged,original$admissible,
 original$moderation_definitions[[1L]]$product_indicator_count==9L)
prepared<-structural_canvas_prepare_effect_bootstrap(fixture$snapshot,fixture$data,'sem','ML','fiml',FALSE,
 character(0),character(0),numeric(0),original_result=original)
ns<-asNamespace('lavaan');reference<-get('lav_model_grad',ns)
model<-original$fit@Model
nmat<-model@nmat
cached_mm<-lapply(seq_along(nmat),function(g)1:nmat[g]+cumsum(c(0,nmat))[g])
stopifnot(model@estimator=='ML',model@representation=='LISREL',!model@ceq.simple.only)
target<-quote(1:nmat[g]+cumsum(c(0,nmat))[g]);replaced<-0L
rewrite<-function(x){
 if(identical(x,target)){replaced<<-replaced+1L;return(quote(.cached_mm[[g]]))}
 if(is.call(x))for(i in seq_along(x)) {
  if(identical(x[[i]],quote(expr=)))next
  x[i]<-list(rewrite(x[[i]]))
 }
 x
}
candidate<-reference;body(candidate)<-rewrite(body(reference))
stopifnot(replaced==3L)
candidate_env<-new.env(parent=ns);candidate_env$.cached_mm<-cached_mm
environment(candidate)<-candidate_env
reference<-compiler::cmpfun(reference);candidate<-compiler::cmpfun(candidate)
x<-get('lav_model_get_parameters',ns)(model)
arguments<-list()
for(j in seq_along(x))for(offset in c(-1,-2,1,2)) {
 xp<-x;xp[j]<-xp[j]+offset*1e-6
 arguments[[length(arguments)+1L]]<-list(lavmodel=model,
  glist=get('lav_model_x2glist',ns)(lavmodel=model,type='free',x=xp),
  lavsamplestats=original$fit@SampleStats,lavdata=original$fit@Data,lavcache=original$fit@Cache)
}
capture<-function(fun,repetitions=1L){
 set.seed(99);conditions<-list()
 stdout<-capture.output(value<-withCallingHandlers({
  out<-vector('list',repetitions)
  for(k in seq_len(repetitions))out[[k]]<-lapply(arguments,function(a)do.call(fun,a))
  out
 },warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
expected<-capture(reference,10L)
stopifnot(identical(expected,capture(candidate,10L),num.eq=FALSE),
 all(vapply(expected$value[[1]],function(v)length(v)==length(x)&&all(is.finite(v)),logical(1))))
times<-list()
for(iteration in 1:5)for(version in if((iteration+id)%%2L)c('reference','candidate')else c('candidate','reference')) {
 gc();started<-Sys.time();actual<-capture(get(version),10L)
 seconds<-as.numeric(difftime(Sys.time(),started,units='secs'))
 stopifnot(identical(expected,actual,num.eq=FALSE))
 times[[length(times)+1L]]<-data.frame(run=id,iteration,version,seconds,sweeps=10L,gradients_per_sweep=length(arguments))
}
write.csv(do.call(rbind,times),file.path(root,'times.csv'),row.names=FALSE)
saveRDS(expected,file.path(root,'reference.rds'))
writeLines(deparse(get('lav_model_grad',ns)),file.path(root,'bundled-lav_model_grad.R'))
print(aggregate(seconds~version,do.call(rbind,times),median))
cat('PASS:',length(arguments),'perturbations; 11 blocks exactly equal including diagnostics/stdout/RNG; index sites',replaced,'\n')
