source('R/utils.R',encoding='UTF-8');source('R/data_io.R',encoding='UTF-8')
reference<-new.env(parent=.GlobalEnv);sys.source('R/data_io.R',reference)
b<-body(reference$repair_text_encoding)
for(i in seq_along(b)) {
  if(identical(b[[i]],quote(pending <- which(broken))))b[[i]]<-quote(invisible(NULL))
  if(is.call(b[[i]])&&identical(b[[i]][[1]],as.name('for')))b[[i]]<-quote(
    for(encoding in c('CP949','EUC-KR','UTF-8','latin1')) {
      converted<-suppressWarnings(iconv(values[broken],from=encoding,to='UTF-8'))
      usable<-!is.na(converted)
      if(any(usable)) {
        repaired[which(broken)[usable]]<-converted[usable]
        broken[which(broken)[usable]]<-FALSE
      }
      if(!any(broken,na.rm=TRUE))break
    })
}
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
set.seed(351)
bytes<-vapply(1:255,function(i)rawToChar(as.raw(i)),character(1))
cp949<-rawToChar(iconv('한글',from='UTF-8',to='CP949',toRaw=TRUE)[[1]])
pool<-c(bytes,cp949,'한글','hello','\uFFFD',NA_character_,'')
cases<-list(character(),rep(NA_character_,10),pool)
for(i in 1:50)cases[[length(cases)+1L]]<-sample(pool,100,TRUE)
for(x in cases) {
  stopifnot(identical(capture(reference$repair_text_encoding(x)),capture(repair_text_encoding(x)),num.eq=FALSE))
  d<-data.frame(x=x)
  stopifnot(identical(capture(reference$normalize_text_encoding(d)),capture(normalize_text_encoding(d)),num.eq=FALSE))
}
cat('PASS:',length(cases),'repair/normalized-data scenarios, conditions and RNG identical.\n')
