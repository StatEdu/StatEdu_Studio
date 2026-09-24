source('R/utils.R',encoding='UTF-8')
source('R/analysis_interrater_agreement.R',encoding='UTF-8')
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_interrater_agreement.R',reference)
kernel<-reference$interrater_icc_complete_value
b<-body(kernel)
restore_row_sweep<-function(x) {
  if(identical(x,quote(matrix - row_means)))return(quote(sweep(matrix,1L,row_means,"-")))
  if(is.call(x))for(i in seq_along(x))if(!identical(x[[i]],quote(expr=)))x[i]<-list(restore_row_sweep(x[[i]]))
  x
}
b<-restore_row_sweep(b)
position<-which(vapply(as.list(b),function(x) is.call(x) && identical(x[[1L]],as.name('if')) && grepl('identical(model, "icc',paste(deparse(x[[2L]]),collapse=''),fixed=TRUE),logical(1)))
stopifnot(length(position)==2L)
for(index in position) b[[index]]<-b[[index]][[3L]]
body(kernel)<-b;reference$interrater_icc_complete_value<-kernel
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1L]],reference)
capture_icc<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(98)
x<-matrix(rnorm(400),100,4)
matrices<-list(x,round(x),matrix(1,20,3),x[1:2,1:2],x[1,,drop=FALSE],x[,1,drop=FALSE],x[FALSE,],x*1e150,x*1e-150)
z<-x;z[1:3,1]<-NA;matrices[[10]]<-z
z<-x;z[1,1]<-Inf;matrices[[11]]<-z
count<-0L
for(mat in matrices)for(model in c('icc1','icc2','icc3'))for(agreement in c(FALSE,TRUE))for(average in c(FALSE,TRUE)) {
  before<-capture_icc(reference$interrater_icc_value(mat,model,agreement,average))
  after<-capture_icc(interrater_icc_value(mat,model,agreement,average))
  stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
}
for(mat in matrices[c(1,3,10,11)])for(model in c('icc1','icc2','icc3'))for(average in c(FALSE,TRUE)) {
  set.seed(48);before<-capture_icc(reference$interrater_icc_bootstrap_ci(mat,model,TRUE,average,resamples=100L,seed=88))
  set.seed(48);after<-capture_icc(interrater_icc_bootstrap_ci(mat,model,TRUE,average,resamples=100L,seed=88))
  stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
}
cat('PASS:',count,'exact ICC/CI/condition/RNG comparisons across all three models.\n')
