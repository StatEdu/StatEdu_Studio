.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
source('scripts/fixtures/generalized_missing_pattern_reference.R')


capture<-function(fn,d,complete=NULL) {
 conditions<-list();set.seed(71)
 value<-tryCatch(withCallingHandlers(fn(d,complete,names(d)[1],names(d),tail(names(d),1)),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class=class(w),message=conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class=class(m),message=conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e)))
 list(value=value,conditions=conditions,rng=.Random.seed)
}
cases<-list(empty=data.frame(),zero_cols=data.frame(row.names=letters[1:3]),one=data.frame(x=1),one_na=data.frame(x=NA_real_),all_na=data.frame(x=rep(NA_real_,20),y=rep(NA_character_,20)),mixed=data.frame(x=c(1,NA,NaN,Inf),y=c('a',NA,'b','c')),factor=data.frame(x=factor(c('a',NA,'b')),y=as.Date(c('2020-01-01',NA,'2020-01-03'))))
set.seed(91)
for(n in c(2L,20L,1000L)) for(p in c(1L,6L,20L)) for(rate in c(0,.1,.7,1)) {
 d<-as.data.frame(matrix(rnorm(n*p),n,p));d[matrix(runif(n*p)<rate,n,p)]<-NA_real_
 names(d)<-rep(c('Complete','x, y','한글 변수',''),length.out=p)
 cases[[paste(n,p,rate,sep='-')]]<-d
}
fail<-character();count<-0L
for(name in names(cases)) for(explicit in c(FALSE,TRUE)) {
 d<-cases[[name]];complete<-if(explicit)rep(c(TRUE,NA,FALSE),length.out=nrow(d))else NULL
 a<-capture(generalized_missing_pattern_reference,d,complete);b<-capture(generalized_missing_pattern_summary,d,complete)
 count<-count+1L
 if(!identical(a,b,num.eq=FALSE)){fail<-c(fail,paste(name,explicit));print(list(case=name,old=a,new=b))}
}
stopifnot(length(fail)==0L)
cat('PASS:',count,'boundary comparisons\n')


