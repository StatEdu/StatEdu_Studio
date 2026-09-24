source('R/utils.R',encoding='UTF-8')
source('R/analysis_survival.R',encoding='UTF-8')
stopifnot(requireNamespace('survival',quietly=TRUE))
capture_ci <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr),error=function(e) list(error=conditionMessage(e))),
    warning=function(w) {warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')},
    message=function(m) {messages <<- c(messages,conditionMessage(m)); invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,
    rng=if(exists('.Random.seed',.GlobalEnv)) .Random.seed else NULL)
}
old_interval <- function(values) list(
  lower=apply(values,2,stats::quantile,probs=.025,na.rm=TRUE,names=FALSE),
  upper=apply(values,2,stats::quantile,probs=.975,na.rm=TRUE,names=FALSE))
new_interval <- function(values) {
  intervals <- apply(values,2,stats::quantile,probs=c(.025,.975),na.rm=TRUE,names=FALSE)
  list(lower=intervals[1L,],upper=intervals[2L,])
}
set.seed(789)
comparisons <- 0L
for(n in c(1,2,20,39,40,41,1999,2000)) for(p in c(1,3,10)) {
  for(kind in c('random','ties','constant','missing','all_missing','infinite')) {
    values <- matrix(runif(n*p),n,p,dimnames=list(NULL,paste0('t',seq_len(p))))
    if(kind=='ties') values <- round(values,1)
    if(kind=='constant') values[] <- 0
    if(kind=='missing') values[seq(1,length(values),3)] <- NA_real_
    if(kind=='all_missing') values[] <- NA_real_
    if(kind=='infinite') values[seq(1,length(values),3)] <- Inf
    stopifnot(identical(capture_ci(old_interval(values)),capture_ci(new_interval(values)),num.eq=FALSE))
    comparisons <- comparisons+1L
  }
}
# Restore just the old endpoint computation; the fit and bootstrap stay shared.
reference <- survival_adjusted_curve
text <- paste(deparse(body(reference),width.cutoff=500L),collapse='\n')
text <- sub('estimate$Lower[target] <- intervals[1L, ]',
  'estimate$Lower[target] <- apply(values, 2, stats::quantile, probs=.025, na.rm=TRUE, names=FALSE)',text,fixed=TRUE)
text <- sub('estimate$Upper[target] <- intervals[2L, ]',
  'estimate$Upper[target] <- apply(values, 2, stats::quantile, probs=.975, na.rm=TRUE, names=FALSE)',text,fixed=TRUE)
text <- sub('intervals <- apply(values, 2, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)', 'NULL',text,fixed=TRUE)
stopifnot(!grepl('intervals <- apply',text,fixed=TRUE))
body(reference) <- parse(text=text)[[1L]]
args <- commandArgs(TRUE)
if(length(args)) {
  previous <- new.env(parent=.GlobalEnv); sys.source(args[[1L]],previous)
  reference <- previous$survival_adjusted_curve
}
set.seed(87)
data <- data.frame(time=round(rexp(90),2),event=sample(0:1,90,TRUE),x=rnorm(90),group=factor(rep(c('A','B','C'),30)))
fit <- survival::coxph(survival::Surv(time,event) ~ group+x,data=data,x=TRUE)
for(reps in c(0L,5L,25L)) for(seed_exists in c(FALSE,TRUE)) {
  if(seed_exists) set.seed(89) else if(exists('.Random.seed',.GlobalEnv)) rm('.Random.seed',envir=.GlobalEnv)
  before <- capture_ci(reference(fit,data,'group',bootstrap_reps=reps,seed=192,selected_times=c(0,.5,1,3)))
  after <- capture_ci(survival_adjusted_curve(fit,data,'group',bootstrap_reps=reps,seed=192,selected_times=c(0,.5,1,3)))
  stopifnot(is.null(before$value$error),identical(before,after,num.eq=FALSE))
  if(reps==25L) stopifnot(any(is.finite(after$value$curve$Lower)))
  comparisons <- comparisons+1L
}
cat('PASS:',comparisons,'exact interval/full adjusted-survival comparisons including conditions and RNG.\n')
