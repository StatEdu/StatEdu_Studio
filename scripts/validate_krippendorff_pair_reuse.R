source('R/utils.R',encoding='UTF-8')
source('R/analysis_interrater_agreement.R',encoding='UTF-8')
reference<-interrater_krippendorff_alpha
b<-body(reference)
loop<-which(vapply(as.list(b),function(x)is.call(x)&&identical(x[[1L]],as.name('for')),logical(1)))
stopifnot(length(loop)==1L)
block<-b[[loop]][[4L]]
for(i in seq_along(block)) {
  expr<-block[[i]]
  if(is.call(expr)&&identical(expr[[1L]],as.name('if'))&&grepl('pair_size',paste(deparse(expr[[2L]]),collapse=''),fixed=TRUE))block[[i]]<-quote(invisible(NULL))
  if(is.call(expr)&&identical(expr[[1L]],as.name('<-'))&&identical(expr[[2L]],as.name('pairs')))block[[i]]<-quote(pairs<-utils::combn(row,2L))
}
b[[loop]][[4L]]<-block;body(reference)<-b
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1L]],old);reference<-old$interrater_krippendorff_alpha}
capture_kripp<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(879);count<-0L
for(k in c(1,2,3,10,20,32,33,40)) {
  frame<-as.data.frame(matrix(sample(1:5,20*k,TRUE),20,k))
  missing<-frame;missing[seq(1,20,3),1]<-NA
  sparse<-frame;sparse[seq(1,20,2),]<-NA
  constant<-frame;constant[]<-1
  for(data in list(frame,missing,sparse,constant,frame[FALSE,],as.data.frame(lapply(frame,factor)))) {
    for(level in c('nominal','ordinal','continuous')) {
      before<-capture_kripp(reference(data,as.character(1:5),level))
      after<-capture_kripp(interrater_krippendorff_alpha(data,as.character(1:5),level))
      stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
    }
  }
}
for(scale in c(1,1e100,1e-100)) {
  data<-matrix(rnorm(90)*scale,30,3);data[1,1]<-NA
  stopifnot(identical(capture_kripp(reference(data,level='continuous')),
    capture_kripp(interrater_krippendorff_alpha(data,level='continuous')),num.eq=FALSE));count<-count+1L
}
cat('PASS:',count,'exact Krippendorff coefficient/condition/RNG comparisons.\n')
