.libPaths(R.home('library'))
source('R/app_bootstrap.R'); load_app_packages(check=FALSE); source_app_modules()
source('scripts/fixtures/crosstab_gamma_reference.R')
capture_gamma <- function(fun,tab) {
  diagnostics <- list()
  stdout <- capture.output(value <- tryCatch(withCallingHandlers(fun(tab),
    warning=function(e) {diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e)); invokeRestart('muffleWarning')},
    message=function(e) {diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e)); invokeRestart('muffleMessage')}),
    error=function(e) list(class=class(e),text=conditionMessage(e))))
  list(value=value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915)
cases <- list()
for(nr in c(0L,1L,2L,7L,8L,9L,20L)) for(nc in c(0L,1L,2L,7L,8L,9L,20L)) {
  for(kind in c('integer','double','table')) {
    tab <- matrix(sample(0:10,nr*nc,replace=TRUE),nr,nc)
    if(kind=='double') storage.mode(tab) <- 'double'
    if(kind=='table') class(tab) <- 'table'
    cases[[length(cases)+1L]] <- tab
  }
}
edge <- matrix(1L,8,8)
for(value in c(NA_real_,NaN,Inf,-Inf,-1,.5,94906265,1e16)) {
  tab <- edge; tab[1,1] <- value; cases[[length(cases)+1L]] <- tab
}
cases <- c(cases,list(matrix(0L,8,8),matrix(50000L,8,8),
  matrix(.Machine$integer.max,8,8),matrix(1482910,8,8),
  structure(edge,class=c('custom_matrix','matrix','array'))))
for(tab in cases) stopifnot(identical(capture_gamma(crosstab_gamma_reference,tab),
  capture_gamma(crosstab_gamma,tab),num.eq=FALSE))
cat(length(cases),'Gamma exact comparisons passed.\n')
