# Equivalent MLR robust-vcov bootstrap scheduling on supported lavaan runtimes.
options(statedu.isolated_lavaan_bootstrap_worker = TRUE)
source('R/utils.R', encoding='UTF-8')
source('R/setup_custom_model_canvas_structural_bootstrap.R', encoding='UTF-8')
suppressPackageStartupMessages(library(lavaan))
run_pair <- function(prepared, reps=40L, failure='') {
  options(statedu.internal.sem_mlr_fixed_index=FALSE)
  old <- structural_canvas_effect_bootstrap_prepared(prepared,reps,20260920L,workers=2L,chunk_size=20L,return_draws=TRUE)
  options(statedu.internal.sem_mlr_fixed_index=NULL,
          statedu.internal.sem_bootstrap_fixed_index_test_failure=failure)
  set.seed(92); before <- .Random.seed
  new <- structural_canvas_effect_bootstrap_prepared(prepared,reps,20260920L,workers=2L,chunk_size=20L,return_draws=TRUE)
  stopifnot(identical(before,.Random.seed))
  stopifnot(isTRUE(all.equal(attr(old,'bootstrap_draws'),attr(new,'bootstrap_draws'),tolerance=0)))
  stopifnot(isTRUE(all.equal(as.data.frame(old),as.data.frame(new),check.attributes=FALSE,tolerance=0)))
  timing <- attr(new,'timings')$fixed_index
  stopifnot(timing$mlr,timing$active,!timing$information$expected_active)
  if(nzchar(failure))stopifnot(timing$fallbacks>0) else stopifnot(timing$fallbacks==0)
  options(statedu.internal.sem_bootstrap_fixed_index_test_failure='')
  cat('PASS',prepared$fit_template@Options$missing,'failure=',failure,'\n')
}
set.seed(175)
f<-rnorm(150);g<-.5*f+rnorm(150)
d<-as.data.frame(setNames(lapply(1:6,function(i)if(i<=3)f+rnorm(150,sd=.6) else g+rnorm(150,sd=.6)),paste0('x',1:6)))
d$unused_text<-rep('ignored',150);d$unused_na<-NA_real_
for(missing in c(FALSE,TRUE)) {
  frame<-d
  if(missing)frame$x2[seq(5,150,10)]<-NA
  fit<-sem('F =~ x1+x2+x3\nG =~ x4+x5+x6\nG ~ a*F\nind := a',data=frame,estimator='MLR',missing='fiml')
  p<-structural_canvas_prepare_effect_bootstrap(NULL,frame,'sem','MLR','fiml',FALSE,character(),character(),NULL,
    original_result=list(fit=fit,df=unname(fitMeasures(fit,'df')),converged=TRUE,admissible=TRUE))
  run_pair(p)
  if(!missing) {
    run_pair(p,20L,'block');run_pair(p,20L,'item')
    options(statedu.internal.sem_mlr_fixed_index=TRUE)
    canceled<-tryCatch({structural_canvas_effect_bootstrap_prepared(p,20L,workers=2L,cancel=function()TRUE);FALSE},error=function(e)grepl('cancel',conditionMessage(e),ignore.case=TRUE))
    stopifnot(canceled)
  }
}
cat('PASS MLR coefficients, bootstrap SE, CI, p, valid masks, RNG, missing data, cancellation and fallback; lavaan',as.character(packageVersion('lavaan')),'\n')
