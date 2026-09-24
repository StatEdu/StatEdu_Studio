.libPaths(R.home('library'));source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
capture<-function(f) {
 set.seed(99);conditions<-list()
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(f(),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}

frequency_tau<-correlation_kendall_tau
guarded_cor<-correlation_kendall_cor
current<-list(kendall_test=correlation_kendall_engine())
stopifnot(!identical(current$kendall_test,stats::cor.test))
stopifnot(identical(correlation_build_kendall_engine(version='0'),stats::cor.test),identical(correlation_build_kendall_engine(platform='other'),stats::cor.test),identical(correlation_build_kendall_engine(reference=function()NULL),stats::cor.test),identical(correlation_kendall_engine(),correlation_kendall_engine()))
xtfrm.audit_numbers<-function(x){warning('custom rank warning');message('custom rank message');runif(1);unclass(x)}
fixtures<-list(
 list(name='ordinary',x=rep(1:3,200),y=rep(3:1,200),fast=TRUE),
 list(name='near',x=1+rep(1:3,200)*1e-15,y=rep(1:5,120),fast=TRUE),
 list(name='minimum_n',x=rep(1:4,128),y=rep(1:5,length.out=512),fast=TRUE),
 list(name='below_minimum_n',x=rep(1:4,length.out=511),y=rep(1:5,length.out=511),fast=FALSE),
 list(name='small_many_levels',x=rep(1:64,2),y=rep(64:1,2),fast=FALSE),
 list(name='maximum_n',x=rep(1:4,2500),y=rep(1:5,2000),fast=TRUE),
 list(name='maximum_levels',x=rep(1:64,8),y=rep(64:1,8),fast=TRUE),
 list(name='one_extra_level',x=rep(1:65,2),y=rep(65:1,2),fast=FALSE),
 list(name='too_long',x=rep(1:3,length.out=10001),y=rep(1:5,length.out=10001),fast=FALSE),
 list(name='missing',x=c(1,2,NA,3),y=1:4,fast=FALSE),
 list(name='infinite',x=c(1,2,Inf,3),y=1:4,fast=FALSE),
 list(name='constant',x=rep(1,4),y=1:4,fast=FALSE),
 list(name='short',x=1:2,y=2:1,fast=FALSE),
 list(name='empty',x=numeric(),y=numeric(),fast=FALSE),
 list(name='names',x=setNames(1:4,letters[1:4]),y=4:1,fast=FALSE),
 list(name='matrix',x=matrix(1:4,ncol=1),y=4:1,fast=FALSE),
 list(name='logical',x=c(TRUE,FALSE,TRUE,FALSE),y=1:4,fast=FALSE),
 list(name='unequal',x=1:3,y=1:4,fast=FALSE),
 list(name='custom',x=structure(c(1,3,2,4),class='audit_numbers'),y=4:1,fast=FALSE))
rows<-list()
for(z in fixtures) {
 a<-capture(function()stats::cor(z$x,z$y,method='kendall'));b<-capture(function()guarded_cor(z$x,z$y,method='kendall'))
 stopifnot(identical(a,b,num.eq=FALSE))
 fast<-!is.null(frequency_tau(z$x,z$y));stopifnot(identical(fast,z$fast))
 rows[[length(rows)+1L]]<-data.frame(name=z$name,fast=fast)
}
original_test<-getS3method('cor.test','default',envir=asNamespace('stats'))
count<-0L
for(n in c(10L,100L))for(method in c('kendall','pearson','spearman'))for(exact in list(FALSE,TRUE,NULL)) {
 x<-rep(1:3,length.out=n);y<-rep(1:5,length.out=n)
 invoke<-function(f)capture(function()f(x,y,method=method,exact=exact))
 stopifnot(identical(invoke(original_test),invoke(current$kendall_test),num.eq=FALSE));count<-count+1L
}
rows<-do.call(rbind,rows);print(rows)
cat('PASS',nrow(rows),'cor guard captures and',count,'raw htest captures\n')

reference<-getS3method('cor.test','default',envir=asNamespace('stats'));candidate<-correlation_kendall_engine()
capture<-function(test,x,y) {
 set.seed(99);conditions<-list()
 stdout<-capture.output(value<-withCallingHandlers(test(x,y,method='kendall',exact=FALSE),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
rows<-list();failures<-list()
for(n in c(5L,20L,100L,1000L))for(k in c(2L,5L,9L))for(seed in 1:10)for(kind in c('integer','negative','near','subnormal','large','signed_zero')) {
 set.seed(seed);x<-sample.int(k,n,replace=TRUE);y<-sample.int(k,n,replace=TRUE)
 transform<-switch(kind,integer=function(z)z,negative=function(z)-z,near=function(z)1+z*1e-15,subnormal=function(z)z*.Machine$double.xmin/16,large=function(z)z*1e300,signed_zero=function(z){z<-as.double(z-1L);z[z==0]<--0;z})
 x<-transform(x);y<-transform(y)
 if(length(unique(x))<2L||length(unique(y))<2L)next
 a<-capture(reference,x,y);b<-capture(candidate,x,y);same<-identical(a,b,num.eq=FALSE)
 rows[[length(rows)+1L]]<-data.frame(n=n,k=k,seed=seed,kind=kind,identical=same)
 if(!same)failures[[length(failures)+1L]]<-list(n=n,k=k,seed=seed,kind=kind,x=x,y=y,reference=a,candidate=b)
}
rows<-do.call(rbind,rows);stopifnot(all(rows$identical))
cat('Raw htest/diagnostics/stdout/RNG:',nrow(rows),'same:',sum(rows$identical),'different:',sum(!rows$identical),'\n')

# Broad finite-input boundary coverage for the active kernel.
cases<-expand.grid(n=c(128L,1000L),k=c(2L,16L,64L),kind=c('balanced','skew','perfect','reverse','almost'),seed=1:3,stringsAsFactors=FALSE)
cases<-rbind(cases,expand.grid(n=10000L,k=c(2L,16L,64L),kind=c('balanced','skew'),seed=1L,stringsAsFactors=FALSE))
rows<-list();failures<-list()
for(i in seq_len(nrow(cases))) {
 z<-cases[i,];set.seed(z$seed)
 prob<-if(z$kind=='skew')c(.99,rep(.01/(z$k-1),z$k-1))else rep(1,z$k)
 x<-sample.int(z$k,z$n,replace=TRUE,prob=prob);y<-sample.int(z$k,z$n,replace=TRUE,prob=prob)
 x[seq_len(z$k)]<-seq_len(z$k);y[seq_len(z$k)]<-seq_len(z$k)
 if(z$kind=='perfect')y<-x
 if(z$kind=='reverse')y<-z$k+1L-x
 if(z$kind=='almost'){y<-x;y[z$n]<-y[z$n]%%z$k+1L}
 a<-capture(reference,x,y);b<-capture(candidate,x,y);same<-identical(a,b,num.eq=FALSE)
 fast<-!is.null(correlation_kendall_tau(x,y));stopifnot(identical(fast,z$n>=512L),is.null(a$value$error))
 rows[[i]]<-cbind(z,identical=same,fast=fast)
 if(!same)failures[[length(failures)+1L]]<-list(case=z,x=x,y=y,reference=a,candidate=b)
 if(i%%30L==0L)cat('checked',i,'of',nrow(cases),'\n')
}
rows<-do.call(rbind,rows)
cat('PASS status:',all(rows$identical),'cases:',nrow(rows),'mismatches:',length(failures),'\n')
stopifnot(all(rows$identical))
