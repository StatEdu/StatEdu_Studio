.libPaths(R.home('library'));source('R/utils.R',encoding='UTF-8')
new<-new.env(parent=.GlobalEnv);sys.source('R/data_io.R',new)
old<-new.env(parent=.GlobalEnv);sys.source('R/data_io.R',old)
restore<-function(x) {
 if(is.call(x)&&identical(x[[1]],as.name('if'))&&identical(x[[2]],quote(startsWith(converted,"\ufeff"))))return(quote(converted<-sub('^\ufeff','',converted)))
 if(is.call(x))for(i in seq_along(x))x[i]<-list(restore(x[[i]]))
 x
}
body(old$read_csv_robust)<-restore(body(old$read_csv_robust))
args<-commandArgs(TRUE)
if(length(args)>0L)sys.source(args[[1]],new)
if(length(args)>1L)sys.source(args[[2]],old)
capture<-function(expr) {
 warnings<-messages<-character();set.seed(618)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
 if(is.data.frame(value)&&!is.null(attr(value,'problems'))){problems<-readr::problems(value);attr(value,'problems')<-NULL;value<-list(value=value,problems=problems)}
 list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
root<-tempfile('csv-bom-');dir.create(root)
texts<-c('', 'a,b\n1,한글', 'a,b\r\n1,hello', 'a,b\n1,"hello\nworld"',
 '\ufeffa,b\n1,2','\ufeff\ufeffa,b\n1,2','a,b\n1,\ufefftext',' \ufeffa,b\n1,2',
 '\ufeff','\ufeffa,b\n1,"unterminated','a,b\n1,\uFFFD')
count<-0L
for(text in texts)for(encoding in c('UTF-8','CP949','EUC-KR','latin1')) {
 bytes<-iconv(text,from='UTF-8',to=encoding,toRaw=TRUE)[[1L]]
 if(is.null(bytes))next
 path<-file.path(root,'input.csv');writeBin(bytes,path)
 for(header in c(TRUE,FALSE)) {
  stopifnot(identical(capture(old$read_csv_robust(path,header)),capture(new$read_csv_robust(path,header)),num.eq=FALSE))
  count<-count+1L
 }
}
cat('PASS:',count,'BOM/no-BOM complete import/error/condition/RNG comparisons.\n')
