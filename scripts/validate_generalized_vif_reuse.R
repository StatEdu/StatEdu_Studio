.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()


normalize <- function(x) {
 if(inherits(x,'formula')) { environment(x)<-.GlobalEnv;return(x) }
 if(is.function(x)) return(list(formals=formals(x),body=body(x)))
 if(!is.null(attr(x,'terms'))) attr(x,'terms')<-normalize(attr(x,'terms'))
 if(!is.null(attr(x,'.Environment'))) attr(x,'.Environment')<-.GlobalEnv
 if(is.list(x)) for(i in seq_along(x)) x[i]<-list(normalize(x[[i]]))
 x
}
# Compare the product cache with the same computation forced to run each time.
# The archived benchmark also compares against the pre-change source.
replaced <- 0L
uncached <- function(node) {
 if (identical(node, quote(if (vif_ready) return(vif_value)))) {
  replaced <<- replaced + 1L
  return(quote(NULL))
 }
 if (is.call(node)) for (i in seq_along(node)) node[i] <- list(uncached(node[[i]]))
 node
}
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new)) for(name in c('prepare_generalized_analysis_result','generalized_assumption_checks')) {
 fn<-get(name,envir=.GlobalEnv);environment(fn)<-env;assign(name,fn,envir=env)
}
body(old$prepare_generalized_analysis_result)<-uncached(body(old$prepare_generalized_analysis_result))
stopifnot(replaced==1L)
base_vif<-generalized_vif_table
run<-function(env,d,family,show,checks,signal,missing='complete') {
 calls<-0L;conditions<-list();set.seed(713)
 env$generalized_vif_table<-function(formula,data) {
  calls<<-calls+1L
  if(signal=='warning') warning('VIF test warning',call.=FALSE)
  if(signal=='message') message('VIF test message')
  if(signal=='error') stop('VIF test error',call.=FALSE)
  base_vif(formula,data)
 }
 output<-capture.output(value<-tryCatch(withCallingHandlers(env$prepare_generalized_analysis_result(d,'y',c('x1','x2','group'),family=family,show_vif=show,assumption_checks=checks,robust=TRUE,missing_strategy=missing,missing_imputations=2L,missing_iterations=1L),warning=function(w){conditions[[length(conditions)+1L]]<<-list(class=class(w),message=conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-list(class=class(m),message=conditionMessage(m));invokeRestart('muffleMessage')}),error=function(e)list(error=class(e),message=conditionMessage(e))))
 list(snapshot=list(value=normalize(value),conditions=conditions,output=output,rng=.Random.seed),calls=calls)
}
count<-0L
for(family in c('gaussian','binomial','gamma','count')) {
 set.seed(918);n<-160L;d<-data.frame(x1=rnorm(n),x2=rnorm(n),group=factor(rep(letters[1:3],length.out=n)))
 d$y<-switch(family,gaussian=rnorm(n),binomial=rbinom(n,1,.4),gamma=rgamma(n,2),count=rnbinom(n,mu=2,size=.5))
 for(show in c(FALSE,TRUE)) for(checks in c(FALSE,TRUE)) for(signal in c('quiet','warning','message','error')) {
  a<-run(old,d,family,show,checks,signal);b<-run(new,d,family,show,checks,signal)
  stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE))
  expected<-if(!show)0L else if(signal=='error')1L else if(checks)2L else 1L
  stopifnot(a$calls==expected,b$calls==if(show&&checks&&signal=='quiet')1L else expected)
  count<-count+1L
 }
 d$x1[1:20]<-NA_real_
 for(missing in c('mi','ipw')) {
  a<-run(old,d,family,TRUE,TRUE,'quiet',missing);b<-run(new,d,family,TRUE,TRUE,'quiet',missing)
  stopifnot(identical(a$snapshot,b$snapshot,num.eq=FALSE),a$calls==2L,b$calls==1L)
  count<-count+1L
 }
}
cat('PASS:',count,'result and call-count comparisons\n')

