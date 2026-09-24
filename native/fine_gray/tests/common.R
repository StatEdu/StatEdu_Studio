.libPaths(R.home('library'));loadNamespace('cmprsk')
root<-Sys.getenv('STATEDU_FINE_GRAY_BUILD_DIR');stopifnot(nzchar(root),dir.exists(root))
original_dll<-dyn.load(file.path(root,'original.dll'));candidate_dll<-dyn.load(file.path(root,'candidate.dll'))
wrap<-function(dll,symbol){f<-cmprsk::crr;e<-new.env(parent=environment(f));address<-getNativeSymbolInfo(symbol,dll)$address
 e$.Fortran<-function(...){args<-list(...);if(identical(args[[1]],'crrvv')){args[[1]]<-address;args$PACKAGE<-NULL};do.call(base::.Fortran,args)}
 environment(f)<-e;f
}
versions<-list(installed=cmprsk::crr,original=wrap(original_dll,'crrvv'),candidate=wrap(candidate_dll,'crrvvc'))
make<-function(n,p,kind='plain'){
 set.seed(44);x<-matrix(rnorm(n*p),n,p);time<-rexp(n);status<-sample(0:2,n,TRUE);group<-rep(1L,n)
 if(kind=='ties')time<-round(time,1)
 if(kind=='groups')group<-rep(1:3,length.out=n)
 if(kind=='binary')x[,1]<-as.numeric(x[,1]>0)
 if(kind=='duplicate'&&p>1)x[,2]<-x[,1]
 if(kind=='near'&&p>1)x[,2]<-x[,1]+1e-6*x[,2]
 list(x=x,time=time,status=status,group=group,kind=kind)
}
capture<-function(f,d){conditions<-character();set.seed(88)
 value<-withCallingHandlers(tryCatch(if(d$kind=='timevarying')f(ftime=d$time,fstatus=d$status,cov1=d$x,cov2=d$x[,1,drop=FALSE],tf=function(t)cbind(t),cengroup=d$group,gtol=1e-6,maxiter=10L)else f(ftime=d$time,fstatus=d$status,cov1=d$x,cengroup=d$group,gtol=1e-6,maxiter=10L),error=function(e)list(error=conditionMessage(e))),warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,conditions=conditions,rng=.Random.seed)
}
capture_all<-function(f,d){stdout<-capture.output(value<-capture(f,d));list(value=value,stdout=stdout)}

