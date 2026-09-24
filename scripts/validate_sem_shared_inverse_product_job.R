.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER=normalizePath(R.home('library'),winslash='/'),STATEDU_NO_PACKAGE_INSTALL='true')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
for(x in parse('scripts/benchmark_sem_product_bootstrap_5000.R'))
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&as.character(x[[2]])[1] %in%
    c('node','edge','stable_data','stable_snapshot','stable_fixture'))eval(x)
root <- 'output/sem-shared-inverse-product-20260915'
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
  if(as.numeric(difftime(Sys.time(),started,units='secs'))>180)stop('job timeout')
  Sys.sleep(.1)
 }
 stopifnot(!cancel,job$process$get_exit_status()==0L,file.exists(job$result_file))
 value<-readRDS(job$result_file);timings<-attr(value,'timings');attr(value,'timings')<-NULL
 list(value=value,before=before,after=.Random.seed,timings=timings,
  elapsed=as.numeric(difftime(Sys.time(),started,units='secs')))
}
out<-list()
for(mode in c('all_pairs_dmc','matched_pair_dmc')) {
 fixture<-stable_fixture(mode,if(mode=='all_pairs_dmc')9L else 3L,if(mode=='all_pairs_dmc')20260833L else 20260832L)
 original<-suppressWarnings(run_structural_canvas_analysis(fixture$snapshot,fixture$data,'sem',
  estimator='ML',missing='fiml',std_lv=FALSE,ordered=character(),nominal=character(),residual_variance_fixes=numeric()))
 pair<-list()
 for(version in c('disabled','enabled')) {
  options(statedu.internal.disable_sem_shared_inverse=version=='disabled')
  pair[[version]]<-execute(fixture,original,200L)
 }
 a<-pair$disabled;b<-pair$enabled
 a$timings<-b$timings<-NULL;a$elapsed<-b$elapsed<-NULL
 stopifnot(identical(a,b,num.eq=FALSE),!length(pair$disabled$timings$fixed_index$shared_inverse_workers),
  all(vapply(pair$enabled$timings$fixed_index$shared_inverse_workers,function(x)x$applied,logical(1))),
  length(pair$enabled$timings$fixed_index$shared_inverse_workers)==4L)
 out[[mode]]<-pair
 cat('PASS: actual callr job exact disabled/enabled',mode,'\n')
}
out$cancel<-execute(fixture,original,5000L,cancel=TRUE)
out$after_cancel<-execute(fixture,original,200L)
a<-out$after_cancel;b<-out$matched_pair_dmc$enabled
a$timings<-b$timings<-NULL;a$elapsed<-b$elapsed<-NULL
stopifnot(identical(a,b,num.eq=FALSE))
saveRDS(out,file.path(root,'jobs.rds'))
cat('PASS: actual job cancellation stops parent and',out$cancel$children_stopped,'children; next job exact\n')
