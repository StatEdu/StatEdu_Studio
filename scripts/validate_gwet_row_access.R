source('R/utils.R',encoding='UTF-8');source('R/analysis_interrater_agreement.R',encoding='UTF-8')
reference<-interrater_gwet_ac
b<-body(reference)
for(i in seq_along(b))if(identical(b[[i]],quote(row_frame <- interrater_row_frame(frame))))
  b[[i]]<-quote(row_frame <- frame)
body(reference)<-b
reference_percent<-interrater_percent_agreement
b<-body(reference_percent)
for(i in seq_along(b))if(identical(b[[i]],quote(row_frame <- interrater_row_frame(frame))))
  b[[i]]<-quote(row_frame <- frame)
body(reference_percent)<-b
args<-commandArgs(TRUE)
if(length(args)){old<-new.env(parent=.GlobalEnv);sys.source(args[[1]],old);reference<-old$interrater_gwet_ac;reference_percent<-old$interrater_percent_agreement}
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(823)
x<-data.frame(a=sample(c(1:3,NA),40,TRUE),b=sample(c(1:3,NA),40,TRUE),c=sample(c(1:3,NA),40,TRUE))
mixed<-x;mixed$b<-as.character(mixed$b)
cases<-list(x,as.data.frame(lapply(x,as.double)),as.data.frame(lapply(x,as.character)),
  as.data.frame(lapply(x,function(v)v>1)),as.data.frame(lapply(x,factor)),mixed,
  x[FALSE,],x[1,,drop=FALSE],as.matrix(x),x*1e-100,x*1e100,
  as.data.frame(lapply(x,function(v)as.Date('2026-01-01')+v)))
checks<-0L
for(frame in cases)for(ordinal in c(FALSE,TRUE))for(weight in c('linear','quadratic')) {
  levels<-unique(c(as.character(unlist(frame,use.names=FALSE)),'unused'))
  levels<-levels[!is.na(levels)]
  stopifnot(identical(capture(reference(frame,levels,ordinal,weight)),
                      capture(interrater_gwet_ac(frame,levels,ordinal,weight)),num.eq=FALSE))
  checks<-checks+1L
}
for(frame in cases) {
  stopifnot(identical(capture(reference_percent(frame)),capture(interrater_percent_agreement(frame)),num.eq=FALSE))
  checks<-checks+1L
}
cat('PASS:',checks,'exact Gwet/percent row-access/condition/RNG comparisons.\n')
