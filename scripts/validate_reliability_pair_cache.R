.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
factory<-reliability_polychoric_pair_engine
stopifnot(factory()$cached)
fallbacks<-0L
for(name in c('polychoric','polyc','polyF','polyBinBvn','tableFast')){
 f<-factory;e<-new.env(parent=environment(f));target<-name
 e$get<-local({target<-name;function(x,envir,...){v<-base::get(x,envir,...);if(x==target)body(v)<-quote(NULL);v}})
 environment(f)<-e;r<-f();stopifnot(!r$cached,identical(r$fit,psych::polychoric));fallbacks<-fallbacks+1L
}
replace_constant<-function(x,from){
 if(identical(x,from))return('unsupported')
 if(is.call(x))for(i in seq_along(x))x[i]<-list(replace_constant(x[[i]],from))
 x
}
for(version in c('2.6.5','2.1.2')){
 f<-factory;body(f)<-replace_constant(body(f),version)
 stopifnot(!f()$cached,identical(f()$fit,psych::polychoric));fallbacks<-fallbacks+1L
}
for(kind in c('warning','error')){
 f<-factory;e<-new.env(parent=environment(f))
 e$asNamespace<-if(kind=='warning')function(...)warning('setup')else function(...)stop('setup')
 environment(f)<-e;stopifnot(!f()$cached);fallbacks<-fallbacks+1L
}
for(kind in c('quiet','warning','message','rng','error')){
 engine<-factory();cached<-environment(engine$fit)$polyc;env<-environment(cached);calls<-0L
 env$original<-function(...){calls<<-calls+1L;switch(kind,warning=warning('fit warning'),message=message('fit message'),rng=runif(1),error=stop('fit error'));1}
 set.seed(21)
 for(i in 1:2)suppressWarnings(suppressMessages(tryCatch(cached(x=1),error=function(e)NULL)))
 stopifnot(calls==if(kind=='quiet')1L else 2L,length(env$order)==if(kind=='quiet')1L else 0L)
}
a<-factory();b<-factory();stopifnot(!identical(environment(a$fit)$polyc,environment(b$fit)$polyc))

# Calling a fresh engine must retain per-matrix smoothing and all diagnostics/RNG.
original_helper<-reliability_polychoric_correlation
install_variant<-function(mode){
 if(mode=='baseline'){assign('reliability_polychoric_correlation',original_helper,.GlobalEnv);return(NULL)}
 engine<-factory()
 helper<-function(matrix)original_helper(matrix,polychoric_fit=engine$fit)
 assign('reliability_polychoric_correlation',helper,.GlobalEnv);engine
}
capture<-function(f){
 set.seed(941);conditions<-character()
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(f(),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
checks<-smoothed<-0L
for(n in c(12L,80L))for(k in c(2L,3L,5L))for(missing in c(FALSE,TRUE))for(seed in 1:2){
 set.seed(seed);d<-as.data.frame(replicate(6,sample.int(k,n,replace=TRUE)))
 if(seed==2L)d[[1]]<-pmin(d[[1]],2L)
 if(missing)for(j in 1:6)d[sample.int(n,max(1L,n%/%10L)),j]<-NA
 frames<-c(list(d),lapply(1:6,function(j)d[,-j,drop=FALSE]))
 install_variant('baseline');before<-lapply(frames,function(x)capture(function()reliability_polychoric_correlation(x)))
 engine<-install_variant('current');original_smooth<-get('cor.smooth',asNamespace('psych'))
 environment(engine$fit)$cor.smooth<-function(x,...){
  ev<-tryCatch(eigen(x,only.values=TRUE)$values,error=function(e)NA_real_)
  if(all(is.finite(ev))&&min(ev)<.Machine$double.eps)smoothed<<-smoothed+1L
  original_smooth(x,...)
 }
 after<-lapply(frames,function(x)capture(function()reliability_polychoric_correlation(x)))
 stopifnot(identical(before,after,num.eq=FALSE));checks<-checks+length(frames)
}
install_variant('baseline');stopifnot(smoothed>0L)
cat('PASS:',fallbacks,'fallbacks; cache conditions/RNG/error/isolation;',checks,'exact matrix comparisons;',smoothed,'smoothing calls.\n')
