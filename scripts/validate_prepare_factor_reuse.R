.libPaths(R.home('library'));source('R/utils.R',encoding='UTF-8')
old<-new.env(parent=.GlobalEnv)
old$prepare_data<-function(data) {
 data<-as.data.frame(data,stringsAsFactors=FALSE,check.names=TRUE)
 data[]<-lapply(data,function(x) {
  if(inherits(x,'haven_labelled_spss'))x<-haven::zap_missing(x)
  if(inherits(x,'haven_labelled'))x<-haven::zap_labels(x)
  if(is.character(x))return(factor(x))
  x
 });data
}
environment(old$prepare_data)<-old
new<-new.env(parent=.GlobalEnv);sys.source('R/data_io.R',new)
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1]],new)
capture<-function(expr) {
 warnings<-messages<-character();set.seed(826)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
count<-0L
for(n in c(0,1,1023,1024,10000))for(kind in c('plain','missing','named','labelled','factor','ordered','date','spss','invalid','encoding')) {
 x<-rep(c('한글','a','é',NA,''),length.out=n)
 if(kind=='missing')x[]<-NA_character_
 if(kind=='named')names(x)<-seq_along(x)
 if(kind=='labelled')x<-haven::labelled(x,label='label')
 if(kind=='factor')x<-factor(x)
 if(kind=='ordered')x<-ordered(x)
 if(kind=='date')x<-as.Date(rep(c('2026-01-01',NA),length.out=n))
 if(kind=='spss')x<-haven::labelled_spss(rep(c(1,2,99),length.out=n),na_values=99)
 if(kind=='invalid')x<-rep(rawToChar(as.raw(c(255,254))),length.out=n)
 if(kind=='encoding')Encoding(x)<-'bytes'
 for(layout in 1:3) {
  y<-x
  if(layout==2 && n>0 && is.character(y))y[[n]]<-'different'
  columns<-if(layout==3)list(a=x,b=seq_len(n),c=y)else list(a=x,b=y)
  d<-as.data.frame(columns,stringsAsFactors=FALSE)
  stopifnot(identical(capture(old$prepare_data(d)),capture(new$prepare_data(d)),num.eq=FALSE));count<-count+1L
 }
}
cat('PASS:',count,'full preparation/attribute/condition/RNG comparisons.\n')
for(n in c(1023L,1024L,10000L))for(k in c(16L,17L,64L,128L))for(layout in 1:4) {
 x<-rep(paste0('값',seq_len(k)),length.out=n);y<-x
 if(layout==2)y[[n]]<-'other'
 if(layout==3)attr(y,'label')<-'other'
 if(layout==4)Encoding(y)<-'bytes'
 d<-data.frame(a=x,b=y,c=x,stringsAsFactors=FALSE)
 stopifnot(identical(capture(old$prepare_data(d)),capture(new$prepare_data(d)),num.eq=FALSE))
}
d<-data.frame(a=paste0('v',1:2048),b=paste0('v',1:2048))
for(condition in c('quiet','warning','message','error')) {
 calls<-0L
 new$factor<-function(x) {
  calls<<-calls+1L
  if(condition=='warning')warning('test warning')
  if(condition=='message')message('test message')
  if(condition=='error')stop('test error')
  base::factor(x)
 }
 old$factor<-new$factor
 a<-capture(old$prepare_data(d));calls<-0L;b<-capture(new$prepare_data(d))
 stopifnot(identical(a,b,num.eq=FALSE),calls==if(condition %in% c('quiet','error'))1L else 2L)
}
cat('PASS: 48 high-cardinality eligibility cases and four quiet/condition/cache-call guards.\n')
rm('factor',envir=new)
result<-new$prepare_data(d);other<-result$b;input<-d
result$a[[1L]]<-levels(result$a)[[2L]]
stopifnot(identical(result$b,other),identical(d,input))
cat('PASS: editing one reused factor leaves the other column and input unchanged.\n')
