source('R/utils.R',encoding='UTF-8')
source('R/data_io.R',encoding='UTF-8')
reference<-new.env(parent=.GlobalEnv)
sys.source('R/data_io.R',reference)
b<-body(reference$repair_text_encoding)
removed<-0L
for(i in seq_along(b))if(identical(b[[i]],quote(broken[is.na(values)] <- FALSE))) {
  b[[i]]<-quote(invisible(NULL));removed<-removed+1L
}
stopifnot(removed==1L)
body(reference$repair_text_encoding)<-b
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1]],reference)
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(822)
bad<-rawToChar(as.raw(c(255,254,65)))
cp949<-rawToChar(iconv('한글',from='UTF-8',to='CP949',toRaw=TRUE)[[1]])
samples<-list(character(),NA_character_,rep(NA_character_,1000),c('hello','한글','é',''),
  c('hello',NA,'한글'),c('\uFFFD',NA,'hello'),c(bad,NA,'hello'),c(cp949,NA,'hello'),
  c(NA,bad,cp949,'\uFFFD','',NA),factor(c('a','b',NA)),c(1,NA,Inf),
  as.Date(c('2026-01-01',NA)))
for(mark in c('unknown','UTF-8','bytes')) {
  value<-c(bad,cp949,'hello',NA)
  Encoding(value)<-mark
  samples[[length(samples)+1L]]<-value
}
checks<-0L
for(x in samples) {
  stopifnot(identical(capture(reference$repair_text_encoding(x)),capture(repair_text_encoding(x)),num.eq=FALSE))
  d<-data.frame(x=x)
  stopifnot(identical(capture(reference$normalize_text_encoding(d)),capture(normalize_text_encoding(d)),num.eq=FALSE))
  checks<-checks+2L
}
cat('PASS:',checks,'exact repaired text/data/condition/RNG comparisons.\n')
