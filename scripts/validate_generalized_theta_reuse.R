.libPaths(R.home('library'))
source('R/analysis_generalized.R',encoding='UTF-8')
original<-MASS::theta.ml;candidate<-environment(generalized_nb_implementation())$theta.ml
capture<-function(fn,args){
 set.seed(172);conditions<-list();record<-function(x)list(class=class(x),message=conditionMessage(x))
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(fn,args),warning=function(w){conditions[[length(conditions)+1L]]<<-record(w);invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-record(m);invokeRestart('muffleMessage')}),error=record))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
results<-list()
for(n in c(20L,999L,1000L,2000L))for(shape in c('poisson','nb','unique','named','negative','fractional','missing','infinite','zero')){
 set.seed(671);mu<-runif(n,.5,2);y<-rpois(n,mu)
 if(shape=='nb')y<-rnbinom(n,mu=mu,size=.5)
 if(shape=='unique')y<-seq_len(n)
 if(shape=='named')names(y)<-seq_len(n)
 if(shape=='negative')y[1]<- -1
 if(shape=='fractional')y[1]<-.25
 if(shape=='missing')y[1]<-NA_real_
 if(shape=='infinite')y[1]<-Inf
 if(shape=='zero')y[]<-0
 for(weighted in c(FALSE,TRUE)){
  args<-list(y=y,mu=mu,limit=25,trace=TRUE)
  if(weighted)args$weights<-rep(c(0,.5,2),length.out=n)
  a<-capture(original,args);b<-capture(candidate,args)
  stopifnot(identical(a,b,num.eq=FALSE))
  results[[length(results)+1L]]<-data.frame(n=n,shape=shape,weighted=weighted,exact=TRUE)
 }
}
for(limit in c(1L,2L,10L)){
 set.seed(391);args<-list(y=rpois(1000,1),mu=rep(1,1000),limit=limit,trace=TRUE)
 stopifnot(identical(capture(original,args),capture(candidate,args),num.eq=FALSE))
}
cat('PASS: 72 count/weight cases and 3 iteration-limit cases; theta, attributes, conditions, trace output and RNG exact\n')
stopifnot(identical(generalized_nb_implementation(version='unsupported'),MASS::glm.nb))
changed<-MASS::theta.ml;body(changed)<-quote(NULL)
stopifnot(identical(generalized_nb_implementation(theta=changed),MASS::glm.nb))
changed_nb<-MASS::glm.nb;body(changed_nb)<-quote(NULL)
stopifnot(identical(generalized_nb_implementation(nb=changed_nb),changed_nb))
normalize_model<-function(x){
 if(is.function(x))return(list(formals=formals(x),body=body(x)))
 if(!is.null(attr(x,'.Environment')))attr(x,'.Environment')<-.GlobalEnv
 if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize_model(x[[i]]))
 x
}
for(shape in c('ordinary','weights','zero_weight','missing','offset','alias')){
 set.seed(819);d<-data.frame(x=rnorm(2000),z=rnorm(2000),w=runif(2000,.5,2),exposure=runif(2000,.5,2))
 d$y<-rnbinom(2000,mu=exp(.2*d$x),size=.5)
 if(shape=='zero_weight')d$w[1]<-0
 if(shape=='missing')d$x[1]<-NA_real_
 if(shape=='alias')d$z<-d$x
 if(!shape%in%c('weights','zero_weight'))d$w<-1
 formula<-if(shape=='offset')y~x+z+offset(log(exposure))else y~x+z
 a<-MASS::glm.nb(formula,data=d,weights=w,na.action=na.exclude,link='log')
 b<-generalized_glm_nb(formula,data=d,weights=w,na.action=na.exclude,link='log')
 stopifnot(identical(a$call,b$call),identical(normalize_model(a),normalize_model(b),num.eq=FALSE))
}
stopifnot(!identical(environment(generalized_nb_implementation()),environment(MASS::glm.nb)))
stopifnot(identical(original,MASS::theta.ml))
cat('PASS: six complete model comparisons, preserved calls, fallback guards and warm activation\n')

