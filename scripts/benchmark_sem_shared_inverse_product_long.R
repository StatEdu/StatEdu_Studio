.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER=normalizePath(R.home('library'),winslash='/'),STATEDU_NO_PACKAGE_INSTALL='true')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
for(x in parse('scripts/benchmark_sem_product_bootstrap_5000.R'))
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&as.character(x[[2]])[1] %in%
    c('node','edge','stable_data','stable_snapshot','stable_fixture'))eval(x)
root <- 'output/sem-shared-inverse-product-long-20260915'
execute <- function(fixture,original,reps,cancel=FALSE) {
 set.seed(99);before<-.Random.seed
 started<-Sys.time()
 job<-structural_canvas_start_effect_bootstrap_job(fixture$snapshot,fixture$data,'sem','ML','fiml',FALSE,
  character(),character(),numeric(),reps=reps,seed=fixture$bootstrap_seed,
  ci_method='bias_corrected',original_result=original,workers=4L,chunk_size=100L)
 on.exit({
  if(job$process$is_alive())structural_canvas_stop_effect_bootstrap_job(job)
  target<-normalizePath(job$directory,winslash='/',mustWork=FALSE)
  allowed<-paste0(normalizePath(tempdir(),winslash='/'),'/')
  stopifnot(startsWith(tolower(target),tolower(allowed)))
  structural_canvas_cleanup_effect_bootstrap_job(job)
 },add=TRUE)
 repeat {
  p<-structural_canvas_read_bootstrap_progress_snapshot(job$progress_file)
  if(cancel&&is.list(p)&&isTRUE(p$completed>0)&&job$process$is_alive()) {
   children<-ps::ps_children(ps::ps_handle(job$process$get_pid()),recursive=TRUE)
   stopifnot(length(children)>=4L)
   structural_canvas_stop_effect_bootstrap_job(job)
   job$process$wait(5000)
   for(i in seq_len(50L)) {
    alive<-vapply(children,function(h)isTRUE(tryCatch(ps::ps_is_running(h),error=function(e)FALSE)),logical(1))
    if(!any(alive))break
    Sys.sleep(.1)
   }
   stopifnot(!job$process$is_alive(),!any(alive))
   return(list(cancelled=TRUE,children_stopped=length(children),rng=.Random.seed))
  }
  if(!job$process$is_alive())break
  if(as.numeric(difftime(Sys.time(),started,units='secs'))>900)stop('job timeout')
  Sys.sleep(.1)
 }
 stopifnot(!cancel,job$process$get_exit_status()==0L,file.exists(job$result_file))
 value<-readRDS(job$result_file);timings<-attr(value,'timings');attr(value,'timings')<-NULL
 list(value=value,before=before,after=.Random.seed,timings=timings,
 elapsed=as.numeric(difftime(Sys.time(),started,units='secs')))
}
id <- as.integer(commandArgs(TRUE)[1]);stopifnot(id %in% 1:2)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_dir<-file.path(root,paste0('run-',id))
dir.create(run_dir,showWarnings=FALSE)
stopifnot(!length(list.files(run_dir,pattern='[.]rds$')))
fixture<-stable_fixture('all_pairs_dmc',9L,20260833L)
original<-suppressWarnings(run_structural_canvas_analysis(fixture$snapshot,fixture$data,'sem',
 estimator='ML',missing='fiml',std_lv=FALSE,ordered=character(),nominal=character(),residual_variance_fixes=numeric()))
stopifnot(original$converged,original$admissible)
pair<-list()
for(version in if(id==1L)c('disabled','enabled')else c('enabled','disabled')) {
 options(statedu.internal.disable_sem_shared_inverse=version=='disabled')
 cat('START',id,version,format(Sys.time()),'\n')
 conditions<-list()
 stdout<-capture.output(result<-withCallingHandlers(execute(fixture,original,5000L),
  warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 pair[[version]]<-c(result,list(conditions=conditions,stdout=stdout))
 saveRDS(pair[[version]],file.path(run_dir,paste0(version,'.rds')))
 cat('DONE',id,version,format(Sys.time()),'wall',result$elapsed,'resampling',result$timings$resampling,'\n')
}
a<-pair$disabled;b<-pair$enabled
a$timings<-b$timings<-NULL;a$elapsed<-b$elapsed<-NULL
stopifnot(identical(a,b,num.eq=FALSE),all(a$value$valid==5000L),
 !length(pair$disabled$timings$fixed_index$shared_inverse_workers),
 length(pair$enabled$timings$fixed_index$shared_inverse_workers)==4L,
 all(vapply(pair$enabled$timings$fixed_index$shared_inverse_workers,function(x)x$applied,logical(1))))
for(x in pair)stopifnot(x$timings$workers==4L,x$timings$chunk_size==100L,
 x$timings$fixed_index$active,x$timings$fixed_index$fallbacks==0L)
saveRDS(pair,file.path(run_dir,'comparison.rds'))
cat('PASS: actual 5000 callr job full returned result/parent diagnostics/stdout/RNG exact\n')
