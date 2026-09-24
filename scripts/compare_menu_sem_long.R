.libPaths(R.home('library'))
args<-commandArgs(trailingOnly=TRUE);version<-args[[1]];iteration<-as.integer(args[[2]])
host<-normalizePath(getwd(),winslash='/');out<-file.path(host,'output/menu-before-after-20260915')
Sys.setenv(STATEDU_BENCHMARK_REPO=file.path(out,version),STATEDU_BENCHMARK_REPS='5000',STATEDU_BENCHMARK_WORKERS='12',STATEDU_BENCHMARK_CHUNK_SIZE='250',STATEDU_BENCHMARK_MISSING_RATE='0',STATEDU_BENCHMARK_OUTPUT='',STATEDU_NO_PACKAGE_INSTALL='true',STATEDU_MODULE_CACHE_DIR=file.path(out,paste0('cache-',version)),R_LIBS_USER=R.home('library'))
job<-NULL;conditions<-list()
execute<-function(){
 on.exit({if(!is.null(job)){if(job$process$is_alive())job$process$kill_tree();structural_canvas_cleanup_effect_bootstrap_job(job)}},add=TRUE)
 withCallingHandlers({
  for(e in parse(file.path(host,'scripts/benchmark_sem_complete_bootstrap_5000.R'))){
   if(is.call(e)&&identical(e[[1]],as.name('on.exit')))next
   eval(e,envir=.GlobalEnv)
  }
  saveRDS(list(value=result,summary=summary,conditions=conditions,rng=.Random.seed),file.path(out,paste0('sem-long-',version,'-',iteration,'.rds')))
 },warning=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){conditions[[length(conditions)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')})
}
execute()
