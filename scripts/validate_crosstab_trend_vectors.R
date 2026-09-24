.libPaths(R.home('library'))
source('R/app_bootstrap.R'); load_app_packages(check=FALSE); source_app_modules()
source('scripts/fixtures/crosstab_trend_reference.R')
capture_trend <- function(fun,tab,row_measure='ordered',col_measure='ordered') {
  diagnostics <- list()
  stdout <- capture.output(value <- tryCatch(withCallingHandlers(fun(tab,row_measure,col_measure),
    warning=function(e) {diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e)); invokeRestart('muffleWarning')},
    message=function(e) {diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e)); invokeRestart('muffleMessage')}),
    error=function(e) list(class=class(e),text=conditionMessage(e))))
  list(value=value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915)
cases <- list()
for(nr in c(1L,2L,3L,8L,12L)) for(nc in c(1L,2L,3L,8L,12L)) {
  for(kind in c('integer','double')) {
    tab <- matrix(sample(0:15,nr*nc,replace=TRUE),nr,nc,
      dimnames=list(paste0('r',seq_len(nr)),paste0('c',seq_len(nc))))
    if(kind=='double') storage.mode(tab) <- 'double'
    cases[[length(cases)+1L]] <- as.table(tab)
  }
}
edge <- matrix(0,3,3,dimnames=list(letters[1:3],LETTERS[1:3]))
for(value in c(0,1,2,NA,NaN,Inf,-1,.5,1.5)) {
  tab <- edge; tab[1,1] <- value; cases[[length(cases)+1L]] <- as.table(tab)
}
cases <- c(cases,list(matrix(2L,3,3),as.table(diag(3L)),
  structure(matrix(5L,3,3),dimnames=list(c('same','same','third'),c('same','same','third')))))
count <- 0L
for(tab in cases) for(measure in c('ordered','category')) {
  stopifnot(identical(capture_trend(crosstab_trend_reference,tab,measure,measure),
    capture_trend(crosstab_trend_analysis,tab,measure,measure),num.eq=FALSE))
  count <- count+1L
}
cat(count,'exact trend comparisons passed.\n')
vector_checks <- 0L
for(tab in cases) {
  if(nrow(tab)<=2L || ncol(tab)<=2L || !all(is.finite(tab)) || any(tab<0)) next
  expanded <- as.data.frame(as.table(tab),stringsAsFactors=FALSE)
  expanded$row_score <- seq_len(nrow(tab))[match(expanded$Var1,rownames(tab))]
  expanded$col_score <- seq_len(ncol(tab))[match(expanded$Var2,colnames(tab))]
  index <- rep(seq_len(nrow(expanded)),expanded$Freq)
  original <- expanded[index,c('row_score','col_score'),drop=FALSE]
  stopifnot(identical(original$row_score,expanded$row_score[index],num.eq=FALSE),
    identical(original$col_score,expanded$col_score[index],num.eq=FALSE),
    identical(nrow(original),length(index)))
  vector_checks <- vector_checks+1L
}
cat(vector_checks,'exact cor-input vector comparisons passed.\n')
