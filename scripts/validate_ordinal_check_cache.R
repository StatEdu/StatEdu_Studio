.libPaths(R.home('library'));source('R/analysis_correlation.R',encoding='UTF-8');make_checked_polychor<-correlation_build_polychor_engine
baseline<-polycor::polychor
expect_fallback<-function(fn) {
 conditions<-character()
 result<-withCallingHandlers(fn(),warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 stopifnot(identical(result$cached,FALSE),identical(result$fit,baseline),length(conditions)==0L)
}
engine<-make_checked_polychor();stopifnot(isTRUE(engine$cached))
for(name in c('polychor','binBvn','pmvnorm','checkmvArgs','chkcorr')) {
 e<-new.env(parent=.GlobalEnv)
 e$get<-local({key<-name;function(x,...) {
  f<-base::get(x,...)
  if(identical(x,key)){body(f)<-quote(stop('changed function'));f}else f
 }})
 fn<-make_checked_polychor;environment(fn)<-e;expect_fallback(fn)
}
for(kind in c('error','warning')) {
 e<-new.env(parent=.GlobalEnv)
 e$asNamespace<-local({condition<-kind;function(...)if(condition=='error')stop('setup error')else{warning('setup warning');base::asNamespace(...)}})
 fn<-make_checked_polychor;environment(fn)<-e;expect_fallback(fn)
}
for(package in c('mvtnorm','polycor')) {
 replace<-function(x) {
  if(identical(x,quote(utils::packageVersion)))return(as.name('test_version'))
  if(is.call(x))for(i in seq_along(x))x[i]<-list(replace(x[[i]]))
  x
 }
 e<-new.env(parent=.GlobalEnv);e$test_version<-local({target<-package;function(pkg)if(pkg==target)numeric_version('0.0.0')else utils::packageVersion(pkg)})
 fn<-make_checked_polychor;body(fn)<-replace(body(fn));environment(fn)<-e;expect_fallback(fn)
}
# A numerical failure in the selected engine must not cause a second fit/RNG path.
calls<-0L;environment(engine$fit)$fit<-function(...){calls<<-calls+1L;stop('fit error')}
error<-tryCatch(engine$fit(),error=conditionMessage)
stopifnot(identical(error,'fit error'),calls==1L,is.null(environment(engine$fit)$last))
cat('PASS: enabled guard, nine setup/version/body fallbacks and one non-retried fit failure.\n')
stopifnot(is.null(environment(correlation_cached_polychor)$engine))
tab<-matrix(c(20,5,8,17),2)
set.seed(77);a<-baseline(tab,std.err=TRUE);rng<- .Random.seed
set.seed(77);b<-correlation_cached_polychor(tab,std.err=TRUE)
stopifnot(identical(a,b,num.eq=FALSE),identical(rng,.Random.seed),isTRUE(environment(correlation_cached_polychor)$engine$cached))
entry<-environment(correlation_cached_polychor)$engine
set.seed(77);c<-correlation_cached_polychor(tab,std.err=TRUE)
stopifnot(identical(a,c,num.eq=FALSE),identical(entry,environment(correlation_cached_polychor)$engine))
e<-new.env(parent=.GlobalEnv);e$engine<-NULL;setup_calls<-0L
e$correlation_build_polychor_engine<-function(){setup_calls<<-setup_calls+1L;list(fit=baseline,cached=FALSE)}
fn<-correlation_cached_polychor;environment(fn)<-e
set.seed(77);d<-fn(tab,std.err=TRUE);set.seed(77);f<-fn(tab,std.err=TRUE)
stopifnot(setup_calls==1L,identical(a,d,num.eq=FALSE),identical(a,f,num.eq=FALSE))
cat('PASS: lazy initialization, retained engine, and one-time fallback setup.\n')
