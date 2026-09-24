.libPaths(R.home('library'))
source('R/analysis_mixed_rm_anova.R')
set.seed(917)
original_options <- options(na.action='na.omit')
cases <- 0L
capture <- function(fn) {
  conditions <- character()
  stdout <- capture.output(value <- tryCatch(withCallingHandlers(fn(),
    warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},
    message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}),
    error=function(e)list(error=conditionMessage(e))))
  list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
for(kind in c('numeric','integer','factor','collinear','ill_conditioned','constant',
             'missing_response','missing_covariate','infinite','overflow','perfect',
             'perfect_first','perfect_later','tiny','large','empty_covariates')) {
  n<-80L;y<-matrix(rnorm(n*6),n,6);d<-data.frame(x=rnorm(n))
  if(kind=='integer')d$x<-seq_len(n)
  if(kind=='factor')d$f<-factor(rep(c('A','B'),n/2))
  if(kind=='collinear')d$x2<-d$x
  if(kind=='ill_conditioned')d$x2<-d$x+rnorm(n,sd=1e-6)
  if(kind=='constant')d$x<-rep(1,n)
  if(kind=='missing_response'){y[1,1]<-NA;y[2,2]<-NA}
  if(kind=='missing_covariate')d$x[1]<-NA
  if(kind=='infinite')y[1,1]<-Inf
  if(kind=='overflow'){y[,1]<-1e308;y[,2]<- -1e308}
  if(kind=='perfect')y[,]<-0
  if(kind=='perfect_first'){y[,1]<-0;y[,2]<-2*d$x}
  if(kind=='perfect_later'){y[,5]<-0;y[,6]<-2*d$x}
  if(kind=='tiny')y<-y*1e-150
  if(kind=='large')y<-y*1e100
  if(kind=='empty_covariates')d<-data.frame()
  pairs<-combn(ncol(y),2,simplify=FALSE)
  for(policy in c('na.omit','na.exclude','na.fail','na.pass')) {
    options(na.action=policy)
    a<-capture(function()lapply(pairs,function(p)mixed_rm_adjusted_pair_test(y[,p[2]]-y[,p[1]],d)))
    b<-capture(function()mixed_rm_adjusted_pair_tests(y,d,pairs))
    stopifnot(identical(a,b));cases<-cases+1L
  }
}
options(na.action='na.omit')
# Instrument the shared statistic helper to prove one shared QR per eligible batch.
original <- mixed_rm_adjusted_pair_fit_test
fits <- list()
mixed_rm_adjusted_pair_fit_test <- function(fit,safe_covariates,design=NULL,cov_beta=NULL) {
  fits[[length(fits)+1L]] <<- dim(fit$model$.diff)
  if(!is.null(cov_beta))stopifnot(identical(cov_beta,stats::vcov(fit)))
  original(fit,safe_covariates,design,cov_beta)
}
y<-matrix(rnorm(80*6),80,6);d<-data.frame(x=rnorm(80));pairs<-combn(6,2,simplify=FALSE)
invisible(mixed_rm_adjusted_pair_tests(y,d,pairs))
stopifnot(length(fits)==15L,all(vapply(fits,function(x)identical(x,c(80L,15L)),logical(1))))
mixed_rm_adjusted_pair_fit_test<-original
# A batch constructs its reference design once; a new batch uses current inputs.
reference_grid <- mixed_rm_covariate_reference_grid
reference_calls <- 0L
mixed_rm_covariate_reference_grid <- function(covariate_data) {
  reference_calls <<- reference_calls + 1L
  reference_grid(covariate_data)
}
first <- mixed_rm_adjusted_pair_tests(y,d,pairs)
stopifnot(reference_calls==1L)
d$x <- d$x + seq_len(nrow(d))/10
second <- mixed_rm_adjusted_pair_tests(y,d,pairs)
stopifnot(reference_calls==2L)
individual <- lapply(pairs,function(p)mixed_rm_adjusted_pair_test(y[,p[2]]-y[,p[1]],d))
stopifnot(identical(second,individual),!identical(first,second),reference_calls==17L)
mixed_rm_covariate_reference_grid <- reference_grid
options(original_options)
cat('PASS:',cases,'exact pair-list/conditions/stdout/RNG comparisons; shared fit/reference design; fresh design after data changes\n')
