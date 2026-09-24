.libPaths(R.home('library'))
source('R/utils.R',encoding='UTF-8')
new <- new.env(parent=.GlobalEnv);sys.source('R/data_io.R',new)
old <- new.env(parent=.GlobalEnv);sys.source('R/data_io.R',old)
uncached_candidates <- old$csv_encoding_candidates_from_bytes
old$csv_encoding_candidates_from_bytes <- function(bytes,decoded_cache=NULL) uncached_candidates(bytes)
capture <- function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  if(is.data.frame(value)&&!is.null(attr(value,'problems'))) {
    problems<-readr::problems(value);attr(value,'problems')<-NULL
    value<-list(value=value,problems=problems)
  }
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
root<-tempfile('csv-decoded-');dir.create(root)
set.seed(492)
cases<-list(raw(),as.raw(c(0,65,0)),as.raw(c(239,187,191,65)),as.raw(c(255,254,65)))
for(text in c('a,b\n1,한글\n2,값','a,b\n1,é\n2,ÿ','a,b\n1,"two\nlines"','a,b\n1,"unclosed','a,b\n1,2,3','a,b\nNA,NA','a,b\n1,\uFFFD','ascii,only\n1,2')) {
 for(encoding in c('UTF-8','CP949','EUC-KR','latin1')) {
  bytes<-iconv(text,from='UTF-8',to=encoding,toRaw=TRUE)[[1L]]
  if(!is.null(bytes))cases[[length(cases)+1L]]<-bytes
 }
}
for(i in 1:50)cases[[length(cases)+1L]]<-as.raw(sample(1:255,100,TRUE))
for(i in seq_along(cases)) {
 bytes<-cases[[i]];cache<-new.env(parent=emptyenv())
 ranked<-new$csv_encoding_candidates_from_bytes(bytes,cache)
 stopifnot(identical(ranked,old$csv_encoding_candidates_from_bytes(bytes)))
 if(!is.null(cache$encoding))stopifnot(identical(cache$encoding,ranked[[1L]]),identical(cache$text,suppressWarnings(iconv(rawToChar(bytes),from=cache$encoding,to='UTF-8'))))
 path<-file.path(root,paste0(i,'.csv'));writeBin(bytes,path)
 for(header in c(TRUE,FALSE))stopifnot(identical(capture(old$read_csv_robust(path,header)),capture(new$read_csv_robust(path,header)),num.eq=FALSE))
}
unlink(root,recursive=TRUE)
cat('PASS:',length(cases),'rank/cache cases and',2*length(cases),'full import/error/condition/RNG comparisons.\n')
