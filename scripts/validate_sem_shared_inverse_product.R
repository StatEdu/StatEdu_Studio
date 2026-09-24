.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER=normalizePath(R.home('library'),winslash='/'),STATEDU_NO_PACKAGE_INSTALL='true')
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root <- 'output/sem-shared-inverse-product-20260915'
for(x in parse('scripts/benchmark_sem_product_bootstrap_5000.R'))
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&as.character(x[[2]])[1] %in%
    c('node','edge','stable_data','stable_snapshot','stable_fixture'))eval(x)
fixture <- stable_fixture('all_pairs_dmc',9L,20260833L)
original <- suppressWarnings(run_structural_canvas_analysis(fixture$snapshot,fixture$data,'sem',
 estimator='ML',missing='fiml',std_lv=FALSE,ordered=character(),nominal=character(),residual_variance_fixes=numeric()))
prepared <- structural_canvas_prepare_effect_bootstrap(fixture$snapshot,fixture$data,'sem','ML','fiml',FALSE,
 character(),character(),numeric(),original_result=original)
# Reuse compatibility tests against product factories, not research factories.
make_guarded_shared_inverse_gradient <- structural_canvas_sem_shared_inverse_gradient
for(x in parse('scripts/validate_sem_shared_inverse_compatibility.R')) {
 if(is.call(x)&&identical(x[[1]],as.name('source')))next
 eval(x)
}
sem_shared_inverse_eligible <- structural_canvas_sem_shared_inverse_eligible
for(x in parse('scripts/validate_sem_shared_inverse_eligibility.R')) {
 if(is.call(x)&&identical(x[[1]],as.name('source')))next
 eval(x)
}
ns <- asNamespace('lavaan'); saved <- get('lav_model_grad',ns); locked <- bindingIsLocked('lav_model_grad',ns)
for(kind in c('normal','error','repeat')) {
 outcome <- tryCatch(local({
  on.exit(structural_canvas_effect_bootstrap_worker_cleanup())
  state <- structural_canvas_sem_shared_inverse_install(structural_canvas_sem_shared_inverse_gradient)
  stopifnot(state$applied,!identical(get('lav_model_grad',ns),saved))
  stopifnot(!structural_canvas_sem_shared_inverse_install(structural_canvas_sem_shared_inverse_gradient)$applied)
  if(kind=='error')stop('injected lifecycle error')
  TRUE
 }),error=function(e)conditionMessage(e))
 stopifnot(identical(get('lav_model_grad',ns),saved),identical(bindingIsLocked('lav_model_grad',ns),locked),
  !exists('.statedu_sem_shared_inverse_state',.GlobalEnv,inherits=FALSE))
 if(kind=='error')stopifnot(identical(outcome,'injected lifecycle error'))else stopifnot(isTRUE(outcome))
}
cat('PASS: product factory/eligibility and normal/error/repeated lifecycle\n')
# Audit actual PSOCK cleanup on successful, failed and cancelled engine calls.
cleanup_body <- body(structural_canvas_effect_bootstrap_worker_cleanup)
audit_dir <- normalizePath(root,winslash='/')
body(structural_canvas_effect_bootstrap_worker_cleanup) <- substitute({
 had_shared <- exists('.statedu_sem_shared_inverse_state',.GlobalEnv,inherits=FALSE)
 ORIGINAL
 ns <- asNamespace('lavaan')
 stopifnot(bindingIsLocked('lav_model_grad',ns),
  identical(digest::digest(list(formals(get('lav_model_grad',ns)),body(get('lav_model_grad',ns))),algo='sha256'),
   '329157f0aeaf6c966820d33df0d014a57aef4a560b73b591717a19867a298d27'),
  !exists('.statedu_sem_shared_inverse_state',.GlobalEnv,inherits=FALSE))
 saveRDS(list(shared=had_shared,restored=TRUE),file.path(DIR,paste0('cleanup-',Sys.getpid(),'.rds')))
 TRUE
},list(ORIGINAL=cleanup_body,DIR=audit_dir))
baseline_body <- NULL
for(x in parse(file.path(root,'baseline.R')))
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('structural_canvas_effect_bootstrap_prepared')))
  baseline_body <- body(eval(x[[3]]))
stopifnot(!is.null(baseline_body))
product_body <- body(structural_canvas_effect_bootstrap_prepared)
options(statedu.isolated_lavaan_bootstrap_worker=TRUE)
capture_run <- function(kind,reps) {
 set.seed(99);conditions<-list()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(structural_canvas_effect_bootstrap_prepared(
  prepared,reps=reps,seed=fixture$bootstrap_seed,workers=4L,chunk_size=100L,return_draws=TRUE,
  cancel=if(kind=='cancel')function()TRUE else NULL,
  progress=if(kind=='error')function(done,...)if(done>0)stop('injected progress error')else NULL else NULL),
  warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}),
  error=function(e)list(error=conditionMessage(e),class=class(e))))
 timings<-attr(value,'timings');attr(value,'timings')<-NULL
 list(capture=list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed),timings=timings)
}
results<-list()
for(kind in c('normal','repeat','block','item','cancel','error','short')) {
 options(statedu.internal.sem_bootstrap_fixed_index_test_failure=if(kind %in% c('block','item'))kind else '')
 pair<-list();reps<-if(kind=='short')24L else 200L
 for(version in c('baseline','product')) {
  body(structural_canvas_effect_bootstrap_prepared)<-if(version=='baseline')baseline_body else product_body
  pair[[version]]<-capture_run(kind,reps)
 }
 stopifnot(identical(pair$baseline$capture,pair$product$capture,num.eq=FALSE))
 if(!kind %in% c('cancel','error')) {
  installs<-pair$product$timings$fixed_index$shared_inverse_workers
  if(kind=='short')stopifnot(!length(installs))else stopifnot(length(installs)==4L,all(vapply(installs,function(x)x$applied,logical(1))))
 } else stopifnot(nzchar(pair$product$capture$value$error))
 results[[kind]]<-pair;cat('PASS: exact product A/B',kind,'\n')
}
body(structural_canvas_effect_bootstrap_prepared)<-product_body
body(structural_canvas_effect_bootstrap_worker_cleanup)<-cleanup_body
files<-list.files(root,pattern='^cleanup-.*[.]rds$',full.names=TRUE)
audits<-lapply(files,readRDS)
stopifnot(length(audits)==56L,all(vapply(audits,function(x)x$restored,logical(1))),
 sum(vapply(audits,function(x)x$shared,logical(1)))==24L)
saveRDS(results,file.path(root,'integration.rds'))
cat('PASS: 56 actual worker cleanups, including 24 installed-state restorations\n')
