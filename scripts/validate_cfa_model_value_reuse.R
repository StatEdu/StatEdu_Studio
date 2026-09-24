source('scripts/validate_cfa_reliability_estimates.R', encoding='UTF-8')
# Restore the prior per-factor accessor calls without copying the arithmetic.
uncache <- function(expr) {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], as.name('reuse_model_value'))) return(expr[[3L]])
  as.call(lapply(as.list(expr), uncache))
}
uncached <- structural_canvas_reliability_estimates
body(uncached) <- uncache(body(uncached))
args <- commandArgs(TRUE)
if(length(args)) {
  previous <- new.env(parent=.GlobalEnv)
  sys.source(args[[1L]],previous)
  uncached <- previous$structural_canvas_reliability_estimates
}
for(name in names(fits)) for(mode in c('standardized','model_implied')) {
  stopifnot(identical(capture(uncached,fits[[name]],mode),
    capture(structural_canvas_reliability_estimates,fits[[name]],mode),num.eq=FALSE))
}
local({
  assign('.cfa_accessor_count',0L,.GlobalEnv)
  assign('.cfa_accessor_noisy',FALSE,.GlobalEnv)
  trace('parameterEstimates',where=asNamespace('lavaan'),print=FALSE,tracer=quote({
    assign('.cfa_accessor_count',get('.cfa_accessor_count',.GlobalEnv)+1L,.GlobalEnv)
    if(get('.cfa_accessor_noisy',.GlobalEnv)) {
      warning('accessor warning',call.=FALSE); message('accessor message')
    }
  }))
  on.exit({untrace('parameterEstimates',where=asNamespace('lavaan'))
    rm(list=c('.cfa_accessor_count','.cfa_accessor_noisy'),envir=.GlobalEnv)})
  for(noisy in c(FALSE,TRUE)) {
    assign('.cfa_accessor_noisy',noisy,.GlobalEnv)
    assign('.cfa_accessor_count',0L,.GlobalEnv)
    before <- capture(uncached,fits$ML,'model_implied')
    stopifnot(get('.cfa_accessor_count',.GlobalEnv)==3L)
    assign('.cfa_accessor_count',0L,.GlobalEnv)
    after <- capture(structural_canvas_reliability_estimates,fits$ML,'model_implied')
    stopifnot(identical(before,after,num.eq=FALSE),
      get('.cfa_accessor_count',.GlobalEnv)==if(noisy) 3L else 1L)
    if(noisy) stopifnot(length(after$warnings)==3L,length(after$messages)==3L)
  }
})
local({
  assign('.cfa_cov_count',0L,.GlobalEnv)
  assign('.cfa_cov_noisy',FALSE,.GlobalEnv)
  trace('lavInspect',where=asNamespace('lavaan'),print=FALSE,tracer=quote({
    if(identical(what,'cov.lv')) {
      assign('.cfa_cov_count',get('.cfa_cov_count',.GlobalEnv)+1L,.GlobalEnv)
      if(get('.cfa_cov_noisy',.GlobalEnv)) {
        warning('covariance warning',call.=FALSE); message('covariance message')
      }
    }
  }))
  on.exit({untrace('lavInspect',where=asNamespace('lavaan'))
    rm(list=c('.cfa_cov_count','.cfa_cov_noisy'),envir=.GlobalEnv)})
  for(noisy in c(FALSE,TRUE)) {
    assign('.cfa_cov_noisy',noisy,.GlobalEnv)
    assign('.cfa_cov_count',0L,.GlobalEnv)
    before <- capture(uncached,fits$ML,'model_implied')
    stopifnot(get('.cfa_cov_count',.GlobalEnv)==3L)
    assign('.cfa_cov_count',0L,.GlobalEnv)
    after <- capture(structural_canvas_reliability_estimates,fits$ML,'model_implied')
    stopifnot(identical(before,after,num.eq=FALSE),
      get('.cfa_cov_count',.GlobalEnv)==if(noisy) 3L else 1L)
  }
})
cat('PASS: 30 exact per-factor cache comparisons; both accessor counts and diagnostic fallback.\n')
