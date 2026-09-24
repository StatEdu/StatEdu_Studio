source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
args<-commandArgs(TRUE);old<-new.env(parent=.GlobalEnv)
if(length(args))sys.source(args[[1]],old)else {
 for(name in c('format_decimal3','format_decimal2')) {
  fn<-get(name);change<-function(expr) {
   if(identical(expr,quote(statedu_strip_decimal_zero(text))))return(quote(sub('^0\\.','.',sub('^-0\\.','-.',text))))
   if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(change(expr[[i]]));expr
  };body(fn)<-change(body(fn));old[[name]]<-fn
 }
}
capture<-function(expr) {
 conditions<-character();set.seed(81)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,conditions=conditions,rng=.Random.seed)
}
set.seed(91)
values<-c(as.list(runif(200,-2,2)),list(-0,0,-.000001,.000001,-.99995,.99995,NA_real_,NaN,Inf,-Inf,1e100,1e-100,NULL,numeric(),c(.1,.2),'.1',TRUE,factor('a'),structure(.1,names='x')))
is.na.decimal_probe<-function(x)FALSE
values<-c(values,list(structure(c(.1,-.2),class='decimal_probe'),structure(numeric(),class='decimal_probe'),structure(NA_real_,class='decimal_probe')))
checks<-0L
for(digits in 0:6)for(name in c('format_decimal3','format_decimal2'))for(x in values) {
 options(statedu.output_decimal_digits=digits)
 stopifnot(identical(capture(old[[name]](x)),capture(get(name)(x)),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact decimal output/condition/RNG comparisons.\n')
