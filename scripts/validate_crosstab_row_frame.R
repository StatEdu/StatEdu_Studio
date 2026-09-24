.libPaths(R.home('library'))
source('R/app_bootstrap.R'); load_app_packages(check=FALSE); source_app_modules()
source('scripts/fixtures/crosstab_row_frame_reference.R')
capture_display <- function(fun,tab,opts) {
  conditions <- list()
  stdout <- capture.output(value <- tryCatch(withCallingHandlers(fun(tab,'x','y',options=opts),
    warning=function(e) {conditions[[length(conditions)+1L]] <<- list(class(e),conditionMessage(e)); invokeRestart('muffleWarning')},
    message=function(e) {conditions[[length(conditions)+1L]] <<- list(class(e),conditionMessage(e)); invokeRestart('muffleMessage')}),
    error=function(e) list(class=class(e),text=conditionMessage(e))))
  list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915)
cases <- list()
for(k in c(1L,2L,3L,8L,20L)) {
  tab <- matrix(sample(0:50,k*k,replace=TRUE),k,k,dimnames=list(paste0('r',1:k),paste0('c',1:k)))
  cases[[length(cases)+1L]] <- tab
  tab[1,] <- 0; cases[[length(cases)+1L]] <- tab
  tab[,1] <- 0; cases[[length(cases)+1L]] <- tab
}
tab <- matrix(0,3,3,dimnames=list(letters[1:3],LETTERS[1:3]))
for(value in c(0,NA,NaN,Inf,-Inf,-1,.5,1e-100,1e100)) {
  edge <- tab; edge[1,1] <- value; cases[[length(cases)+1L]] <- edge
}
flags <- expand.grid(row_percent=c(FALSE,TRUE),column_percent=c(FALSE,TRUE),total_percent=c(FALSE,TRUE),total_n=c(FALSE,TRUE))
for(labels in list(c('same','same','last'),c('Row','Total','other'),c('','space name','a-b'),
                  c(NA_character_,'b','c'),c('한글','空白','β'))) {
  edge <- matrix(1:9,3,3,dimnames=list(c('duplicate','duplicate','last'),labels))
  cases[[length(cases)+1L]] <- edge
}
saved_options <- options()
checks <- 0L
for(decimal in c('.',',')) {
  options(OutDec=decimal)
  for(tab in cases) for(i in seq_len(nrow(flags))) {
    opts <- as.list(flags[i,])
    stopifnot(identical(capture_display(crosstab_row_frame_reference,tab,opts),
      capture_display(crosstab_display_table,tab,opts),num.eq=FALSE))
    checks <- checks+1L
  }
}
options(OutDec=saved_options$OutDec)
cat(checks,'exact display/diagnostic/RNG comparisons passed.\n')
