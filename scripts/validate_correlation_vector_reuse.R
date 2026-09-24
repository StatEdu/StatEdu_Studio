source('R/app_bootstrap.R', encoding='UTF-8')
load_app_packages(check=FALSE)
source_app_modules()
reference <- new.env(parent=.GlobalEnv)
sys.source('R/analysis_correlation.R', reference)
# Force the previous per-pair conversion path, retaining preflight and output.
for(name in c('correlation_pair_result','correlation_latent_pair_result')) {
  fun <- reference[[name]]
  body(fun) <- as.call(c(list(as.name('{'),quote(prepared_vectors <- NULL)),as.list(body(fun))[-1L]))
  reference[[name]] <- fun
}
args <- commandArgs(TRUE)
if(length(args)) sys.source(args[[1L]],reference)
capture_correlation <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr),error=function(e) list(error=conditionMessage(e))),
    warning=function(w) {warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')},
    message=function(m) {messages <<- c(messages,conditionMessage(m)); invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(983)
n <- 90L
data <- data.frame(x=rnorm(n), y=rnorm(n), ordinal=sample(c('low','middle','high'),n,TRUE),
  binary=sample(c('no','yes'),n,TRUE), nominal=sample(c('A','B','C'),n,TRUE),
  constant=1, missing=NA_real_)
info <- data.frame(name=names(data),measurement=c('continuous','continuous','ordered','binary','category','continuous','continuous'))
missing_data <- data; missing_data[seq(1,n,5),1:5] <- NA
blank_data <- data; blank_data$ordinal[1:10] <- ''; blank_data$binary[11:20] <- ' '
factor_data <- data; factor_data$ordinal <- ordered(factor_data$ordinal,levels=c('high','middle','low'))
factor_data$nominal <- factor(factor_data$nominal)
numeric_text <- data; numeric_text$x <- as.character(numeric_text$x); numeric_text$x[1:3] <- 'bad'
categories <- data.frame(name='ordinal',value_1='middle',value_2='low',value_3='high')
stopifnot(identical(correlation_ordered_score(c('middle','low','high'), 'ordinal', categories), c(1,2,3)))
comparisons <- 0L
for(d in list(data,missing_data,blank_data,factor_data,numeric_text)) {
  for(method in c('auto','pearson','spearman','kendall')) {
    for(latent in c(FALSE,TRUE)) {
      options <- list(continuous_method=method,latent_correlations=latent,normality=FALSE)
      before <- capture_correlation(reference$prepare_correlation_results(d,names(d),info,category_table=categories,options=options))
      after <- capture_correlation(prepare_correlation_results(d,names(d),info,category_table=categories,options=options))
      stopifnot(is.null(before$value$error), identical(before,after,num.eq=FALSE))
      comparisons <- comparisons+1L
    }
  }
}
for(vars in list('x',c('absent','x'),c('constant','missing'),c('binary','ordinal','x','x'))) {
  before <- capture_correlation(reference$prepare_correlation_results(data,vars,info))
  after <- capture_correlation(prepare_correlation_results(data,vars,info))
  stopifnot(identical(before,after,num.eq=FALSE))
  comparisons <- comparisons+1L
}
# Instrument a three-variable run: three conversions, not nine. A warning and
# message on one column must still recur for every pair that uses that column.
for(noisy in c(FALSE,TRUE)) {
  run <- function(is_reference) {
    env <- new.env(parent=.GlobalEnv)
    sys.source('R/analysis_correlation.R',env)
    if(is_reference) {
      pair <- env$correlation_pair_result
      body(pair) <- as.call(c(list(as.name('{'),quote(prepared_vectors <- NULL)),as.list(body(pair))[-1L]))
      env$correlation_pair_result <- pair
    }
    calls <- 0L
    env$correlation_analysis_vector <- function(values,measurement,name=NULL,category_table=NULL) {
      calls <<- calls+1L
      if(noisy && identical(name,'x')) {warning('conversion warning',call.=FALSE); message('conversion message')}
      correlation_analysis_vector(values,measurement,name,category_table)
    }
    value <- capture_correlation(env$prepare_correlation_results(data,c('x','y','binary'),info,options=list(continuous_method='pearson')))
    list(value=value,calls=calls)
  }
  before <- run(TRUE); after <- run(FALSE)
  stopifnot(identical(before$value,after$value,num.eq=FALSE),before$calls==9L,after$calls==if(noisy) 5L else 3L)
  if(noisy) stopifnot(length(after$value$warnings)==3L,length(after$value$messages)==3L)
}
cat('PASS:',comparisons,'exact full-result/condition/RNG comparisons; conversion counts and warning/message fallback.\n')
