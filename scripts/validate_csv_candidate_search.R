source('R/utils.R',encoding='UTF-8')
source('R/data_io.R',encoding='UTF-8')
reference<-new.env(parent=.GlobalEnv);sys.source('R/data_io.R',reference)
restore<-function(expr) {
  if(identical(expr,quote(if (identical(converted, previous_text)) return(previous_score))))
    return(quote(invisible(NULL)))
  if(identical(expr,quote(codepoints <- utf8ToInt(converted))))return(quote(invisible(NULL)))
  if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('latin1_supplement_count')))
    return(quote(latin1_supplement_count <- sum(suppressWarnings(gregexpr('[\u00A0-\u00FF]',converted)[[1]]) > 0)))
  if(is.call(expr)&&identical(expr[[1]],as.name('<-'))&&identical(expr[[2]],as.name('cjk_count')))
    return(quote(cjk_count <- sum(suppressWarnings(gregexpr('[\u3130-\u318F\uAC00-\uD7AF]',converted)[[1]]) > 0)))
  if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(reference$csv_encoding_candidates_from_bytes)<-restore(body(reference$csv_encoding_candidates_from_bytes))
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1]],reference)
scores_function<-function(fn) {
  b<-body(fn)
  at<-which(vapply(as.list(b),function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('ranked')),logical(1)))[[1]]
  b[[at]]<-quote(return(scores));body(fn)<-b;fn
}
before_scores<-scores_function(reference$csv_encoding_candidates_from_bytes)
after_scores<-scores_function(csv_encoding_candidates_from_bytes)
capture<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(963)
texts<-c('', 'plain ASCII', '한글과 ㄱㄴㄷ', 'é à ÿ', '\uFFFD',
  intToUtf8(c(0x9f,0xa0,0xff,0x100,0x312f,0x3130,0x318f,0x3190,0xabff,0xac00,0xd7af,0xd7b0)),
  intToUtf8(c(0x1f600,0x4e00,0x3042,10,13,9)), paste(rep('한글,é,ASCII',1000),collapse='\n'))
cases<-list(raw(),as.raw(c(0,65,0)),as.raw(c(0xef,0xbb,0xbf,65)))
cases[[length(cases)+1L]]<-charToRaw(intToUtf8(c(1:0xD7FF,0xE000:0xFFFF)))
for(text in texts)for(encoding in c('UTF-8','CP949','EUC-KR','latin1')) {
  bytes<-iconv(text,from='UTF-8',to=encoding,toRaw=TRUE)[[1]]
  if(!is.null(bytes))cases[[length(cases)+1L]]<-bytes
}
for(i in 1:100)cases[[length(cases)+1L]]<-as.raw(sample(0:255,sample(1:300,1),TRUE))
for(bytes in cases) {
  stopifnot(identical(capture(reference$csv_encoding_candidates_from_bytes(bytes)),capture(csv_encoding_candidates_from_bytes(bytes)),num.eq=FALSE),
            identical(capture(before_scores(bytes)),capture(after_scores(bytes)),num.eq=FALSE))
}
cat('PASS:',length(cases),'byte scenarios: exact scores/ranks/conditions/RNG.\n')
