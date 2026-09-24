source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-normalize_output_decimal_digits
body(reference)<-as.call(as.list(body(reference))[-2L])
args<-commandArgs(TRUE)
if(length(args)){e<-new.env(parent=.GlobalEnv);sys.source(args[[1]],e);reference<-e$normalize_output_decimal_digits;environment(reference)<-.GlobalEnv}
capture<-function(expr) {
 conditions<-character();set.seed(81)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,conditions=conditions,rng=.Random.seed)
}
values<-c(as.list(-10:10),as.list(seq(-1,6,.5)),list(NULL,integer(),NA_integer_,NA_real_,Inf,NaN,'3','bad',TRUE,list(3L),c(2L,4L),structure(3L,names='digits'),matrix(3L,1,1),factor('3'),structure(3L,class='probe')))
for(x in values)stopifnot(identical(capture(reference(x)),capture(normalize_output_decimal_digits(x)),num.eq=FALSE))
cat('PASS:',length(values),'exact normalization/type/condition/RNG comparisons.\n')
