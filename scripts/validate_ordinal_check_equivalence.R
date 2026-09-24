.libPaths(R.home('library'));source('R/analysis_correlation.R',encoding='UTF-8');make_checked_polychor<-correlation_build_polychor_engine
capture<-function(expr) {
 conditions<-character();set.seed(821)
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),warning=function(w){conditions<<-c(conditions,paste('warning',conditionMessage(w)));invokeRestart('muffleWarning')},message=function(m){conditions<<-c(conditions,paste('message',conditionMessage(m)));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
engine<-make_checked_polychor();original<-get('chkcorr',asNamespace('mvtnorm'))
matrices<-list(NULL,1,1:4,matrix(numeric(),0,0),matrix(1,2,3),matrix(letters[1:4],2),diag(2),structure(diag(2),dimnames=list(c('a','b'),c('x','y'))))
for(rho in c(-Inf,-1.1,-1,-.9,0,.9,1,1.1,Inf,NA,NaN))matrices[[length(matrices)+1L]]<-matrix(c(1,rho,rho,1),2)
for(diagonal in c(0,.999,1+1e-10,1.001,NA))matrices[[length(matrices)+1L]]<-matrix(c(diagonal,.5,.4,1),2)
for(x in matrices)for(i in 1:2)stopifnot(identical(capture(original(x)),capture(engine$check(x)),num.eq=FALSE))
checks<-0L
for(k in c(2L,3L,5L))for(kind in c('independent','associated'))for(ml in c(FALSE,TRUE)) {
 set.seed(338+k);n<-2000L;x<-rnorm(n);y<-if(kind=='associated').6*x+rnorm(n)else rnorm(n)
 cuts<-c(-Inf,qnorm(seq_len(k-1L)/k),Inf);x<-cut(x,cuts,ordered_result=TRUE);y<-cut(y,cuts,ordered_result=TRUE)
 stopifnot(identical(capture(polycor::polychor(x,y,ML=ml,std.err=TRUE)),capture(engine$fit(x,y,ML=ml,std.err=TRUE)),num.eq=FALSE));checks<-checks+1L
}
for(tab in list(matrix(c(5,0,0,5),2),matrix(c(0,0,1,2,0,0,3,4,0),3),matrix(1,1,3))) {
 stopifnot(identical(capture(polycor::polychor(tab,std.err=TRUE)),capture(engine$fit(tab,std.err=TRUE)),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',length(matrices)*2,'matrix checks and',checks,'polychor result/condition/stdout/RNG comparisons.\n')


