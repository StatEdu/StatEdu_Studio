source('R/utils.R',encoding='UTF-8')
source('R/analysis_interrater_agreement.R',encoding='UTF-8')
reference<-interrater_percent_agreement
b<-body(reference)
loop<-which(vapply(as.list(b),function(x)is.call(x)&&identical(x[[1L]],as.name('for')),logical(1)))
stopifnot(length(loop)==1L)
block<-b[[loop]][[4L]]
for(i in seq_along(block)) {
  expr<-block[[i]]
  if(is.call(expr)&&identical(expr[[1L]],as.name('if'))&&grepl('pair_size',paste(deparse(expr[[2L]]),collapse=''),fixed=TRUE))block[[i]]<-quote(pair_count<-utils::combn(seq_along(values),2L))
}
b[[loop]][[4L]]<-block;body(reference)<-b
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1L]],old);reference<-old$interrater_percent_agreement}
capture_percent<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(872);count<-0L
for(k in c(1,2,3,10,30,32,33,40)) {
  frame<-as.data.frame(matrix(sample(c('A','B','C'),100*k,TRUE),100,k))
  missing<-frame;missing[seq(1,100,3),1]<-NA
  sparse<-frame;sparse[seq(1,100,2),]<-NA
  constant<-frame;constant[]<-'A'
  for(data in list(frame,missing,sparse,constant,frame[FALSE,],as.data.frame(lapply(frame,factor)),as.matrix(frame))) {
    before<-capture_percent(reference(data));after<-capture_percent(interrater_percent_agreement(data))
    stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
  }
}
cat('PASS:',count,'exact percent-agreement/pair-count/condition/RNG comparisons.\n')
