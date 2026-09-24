.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
normalize <- function(x) {
 if(inherits(x,'formula')) {environment(x)<-.GlobalEnv;return(x)}
 if(is.call(x)) {if(is.function(x[[1L]]))x[[1L]]<-as.name('.normalized_call_function');for(i in seq_along(x))if(!identical(x[[i]],quote(expr=)))x[i]<-list(normalize(x[[i]]));return(x)}
 if(is.function(x))return(list(formals=formals(x),body=body(x)))
 if(!is.null(attr(x,'terms')))attr(x,'terms')<-normalize(attr(x,'terms'))
 if(!is.null(attr(x,'.Environment')))attr(x,'.Environment')<-.GlobalEnv
 if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize(x[[i]]))
 x
}
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new))sys.source('R/analysis_longitudinal.R',env)
# Restore the independent pre-change correlation validator in the reference.
sys.source('scripts/fixtures/longitudinal_gee_correlation_reference.R',old)

# Capture the unrounded QIC vector as well as the public formatted tables.
gee_reuse_qic<-list()
trace('QIC.geeglm',where=asNamespace('geepack'),tracer=quote(NULL),
 exit=quote(.GlobalEnv$gee_reuse_qic[[length(.GlobalEnv$gee_reuse_qic)+1L]]<-returnValue()),print=FALSE)
run <- function(env,d,args,signal) {
 original_fit<-env$longitudinal_fit_model;calls<-0L
 env$longitudinal_fit_model<-function(...) {
  calls<<-calls+1L
  if(signal=='error')stop('reuse test error')
  if(signal=='warning')warning('reuse test warning')
  if(signal=='message')message('reuse test message')
  if(signal=='rng')runif(1)
  original_fit(...)
 }
 on.exit(env$longitudinal_fit_model<-original_fit)
 set.seed(718);conditions<-list();.GlobalEnv$gee_reuse_qic<-list()
 output<-capture.output(value<-withCallingHandlers(do.call(env$prepare_longitudinal_analysis_result,c(list(data=d,outcome='y',id='id',time='time',predictors=c('x1','x2'),model_type='gee',variable_info=data.frame(name=names(d),measurement='continuous')),args)),
 warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=normalize(value),conditions=conditions,output=output,rng=.Random.seed,qic=.GlobalEnv$gee_reuse_qic),calls=calls)
}
passed<-0L
check <- function(d,args,signal='quiet',reuse=TRUE) {
 cat('Checking',args$family,args$corstr,args$weight_type,signal,'reuse',reuse,'\n')
 a<-run(old,d,args,signal);b<-run(new,d,args,signal)
 stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE))
 if(signal!='error')stopifnot(length(a$snapshot$value)==1L,!is.null(a$snapshot$value[[1]]$model),
   length(a$snapshot$qic)==3L,all(vapply(a$snapshot$qic,function(x)is.numeric(x)&&length(x)==6L,logical(1))))
 stopifnot(a$calls==b$calls)
 passed<<-passed+1L
}
for(family in c('gaussian','binomial','poisson','gamma')) {
 set.seed(937);subjects<-100L;n<-subjects*4L
 d<-data.frame(id=rep(seq_len(subjects),each=4),time=rep(0:3,subjects),x1=rnorm(n),x2=rnorm(n),w=runif(n,.7,1.3),exposure=runif(n,.8,1.2))
 eta<-.2*d$time+.3*d$x1
 d$y<-switch(family,gaussian=eta+rnorm(n),binomial=rbinom(n,1,plogis(eta)),poisson=rpois(n,exp(eta)),gamma=rgamma(n,shape=3,rate=3/exp(eta)))
 for(corstr in c('independence','exchangeable','ar1'))for(weighted in c(FALSE,TRUE)) {
  args<-list(family=family,corstr=corstr,exponentiate=weighted)
  if(weighted){args$weight<-'w';args$weight_type<-'sampling'}
  if(family=='poisson')args$exposure<-'exposure'
  # Noninteger binomial sampling weights emit package warnings: retain refits.
  check(d,args,reuse=!(weighted && family=='binomial'))
 }
 for(signal in c('warning','message','rng','error'))check(d,list(family=family,corstr='exchangeable'),signal)
 check(d,list(family=family,corstr='exchangeable',random_slope=TRUE),reuse=FALSE)
 # Missing rows and shuffled input retain the same fitted row order and output.
 d$y[c(3,27)]<-NA_real_;d<-d[rev(seq_len(n)),]
 check(d,list(family=family,corstr='exchangeable'))
}
cat('PASS:',passed,'GEE matrix validation whole-analysis cases\n')
untrace('QIC.geeglm',where=asNamespace('geepack'))

# Direct validator boundaries include invalid alpha, invalid waves, singularity,
# empty groups, repeated and changing matrices. Capture eigen inputs and results.
capture_check<-function(env,corstr,alpha,id,waves) {
 calculations<-list();conditions<-list();set.seed(719)
 env$eigen<-function(x,...) {
  value<-base::eigen(x,...)
  calculations[[length(calculations)+1L]]<<-list(matrix=x,value=value)
  value
 }
 on.exit(rm('eigen',envir=env))
 output<-capture.output(value<-withCallingHandlers(tryCatch(
  withVisible(env$longitudinal_gee_check_simple_correlation(corstr,alpha,id,waves)),
  error=function(e)list(error=class(e),message=conditionMessage(e))),
  warning=function(w){conditions[[length(conditions)+1L]]<<-list(class(w),conditionMessage(w));invokeRestart('muffleWarning')},
  message=function(m){conditions[[length(conditions)+1L]]<<-list(class(m),conditionMessage(m));invokeRestart('muffleMessage')}))
 list(snapshot=list(value=value,conditions=conditions,output=output,rng=.Random.seed),calculations=calculations)
}
boundary_passed<-0L
patterns<-list(
 list(id=integer(),waves=numeric()),
 list(id=1:5,waves=rep(0,5)),
 list(id=rep(1:20,each=4),waves=rep(0:3,20)),
 list(id=rep(1:3,c(2,4,2)),waves=c(0:1,0:3,0:1)),
 list(id=rep(1:3,each=3),waves=c(0,1,3,0,2,3,0,1,3)),
 list(id=c(1,1,2,2),waves=c(0,0,0,1)),
 list(id=c(1,1,2,2),waves=c(0,1,NA,2)),
 list(id=c(1,1,2,2),waves=c(0,1,Inf,2)),
 list(id=c(1,1,NA,NA),waves=c(0,1,NA,Inf)),
 list(id=rep(1:2,each=3),waves=factor(rep(0:2,2))),
 list(id=rep(1:2,each=3),waves=setNames(rep(0:2,2),letters[1:6]))
)
for(corstr in c('exchangeable','ar1','independence'))for(alpha in list(.3,0,-0,-.Machine$double.xmin,1,1+.Machine$double.eps,-1,-.5,1-1e-9,NA_real_,NaN,Inf,numeric(),c(.1,.2)))for(p in patterns) {
 a<-capture_check(old,corstr,alpha,p$id,p$waves);b<-capture_check(new,corstr,alpha,p$id,p$waves)
 stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE))
 for(calc in b$calculations)stopifnot(any(vapply(a$calculations,function(ref)identical(ref,calc,num.eq=FALSE),logical(1))))
 boundary_passed<-boundary_passed+1L
}
for(corstr in c('exchangeable','ar1')) {
 a<-capture_check(old,corstr,.3,rep(1:1000,each=4),rep(0:3,1000))
 b<-capture_check(new,corstr,.3,rep(1:1000,each=4),rep(0:3,1000))
 stopifnot(length(a$calculations)==1000L,length(b$calculations)==1L,
  identical(a$snapshot,b$snapshot,num.eq=FALSE),identical(a$calculations[[1]],b$calculations[[1]],num.eq=FALSE))
}
cat('PASS:',boundary_passed,'validator boundaries; repeated-matrix eigen calls 1000 -> 1 for both structures\n')

