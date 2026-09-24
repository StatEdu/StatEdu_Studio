# Exact reference comparison: preserve the original per-evaluation membership matrix.
invisible(try(Sys.setlocale('LC_CTYPE', 'English_United States.utf8'), silent=TRUE))
current <- new.env(parent=.GlobalEnv)
sys.source('R/analysis_meta.R', current)
reference <- new.env(parent=.GlobalEnv)
sys.source('R/analysis_meta.R', reference)
original_components <- reference$meta_three_level_components
reference$meta_three_level_components <- function(yi, vi, study_id, sigma2_within, sigma2_between, same_study=NULL) {
  original_components(yi, vi, study_id, sigma2_within, sigma2_between)
}
capture <- function(env, input) {
  set.seed(817)
  conditions <- list()
  record <- function(x) list(class=class(x), message=conditionMessage(x))
  value <- tryCatch(withCallingHandlers(env$meta_fit_three_level(input),
    warning=function(w) {conditions[[length(conditions)+1L]] <<- record(w); invokeRestart('muffleWarning')},
    message=function(m) {conditions[[length(conditions)+1L]] <<- record(m); invokeRestart('muffleMessage')}),
    error=function(e) record(e))
  list(value=value, conditions=conditions, rng=.Random.seed)
}
checked <- 0L
for (family in c('g', 'r', 'or')) for (shape in c('plain', 'factor', 'named', 'numeric', 'missing', 'unicode', 'independent', 'two_studies')) {
  set.seed(971)
  ids <- rep(paste0('study', 1:4), each=3)
  ids <- switch(shape, factor=factor(ids), named=setNames(ids, seq_along(ids)),
    numeric=rep(1:4, each=3), missing=replace(ids,1,NA_character_),
    unicode=rep(c('연구 가','연구 나','α','β'),each=3), independent=paste0('study',1:12),
    two_studies=rep(c('a','b'),each=6), ids)
  input <- structure(list(rows=data.frame(yi=rnorm(12,.2,.3),vi=runif(12,.04,.15),study_id=ids),tau2=.1,conf_level=.95,family=family),class='statedu_meta_model')
  if(shape=='named') names(input$rows$study_id) <- seq_len(12)
  stopifnot(identical(capture(current,input),capture(reference,input),num.eq=FALSE))
  checked <- checked+1L
}
# Repeated calls must not reuse another dataset's study grouping.
input$rows$study_id <- rep(c('a','b','c','d'),each=3)
first <- capture(current,input)
input$rows$study_id <- rep(c('a','b','c','d'),3)
stopifnot(identical(capture(current,input),capture(reference,input),num.eq=FALSE))
input$rows$study_id <- rep(c('a','b','c','d'),each=3)
stopifnot(identical(first,capture(current,input),num.eq=FALSE))
cat('PASS:',checked,'family/input cases and study-group lifecycle checks\n')
