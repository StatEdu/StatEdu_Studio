source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-format_p
restore<-function(expr) {
 if(identical(expr,quote(if (isTRUE(leading_zero) || !startsWith(text, "0.")) text else substring(text, 2L))))
  return(quote(if(isTRUE(leading_zero))text else sub('^0\\.','.',text)))
 if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
 expr
}
body(reference)<-restore(body(reference))
args<-commandArgs(TRUE)
if(length(args)){e<-new.env(parent=.GlobalEnv);sys.source(args[[1]],e);reference<-e$format_p;environment(reference)<-.GlobalEnv}
capture<-function(expr) {
 conditions<-character();set.seed(81)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,conditions=conditions,rng=.Random.seed)
}
values<-c(list(NULL,numeric(),NA_real_,NaN,Inf,-Inf,TRUE,FALSE,-0,-1,.000999999,.001,.0015,.0095,.05,.9995,1,1.5,100),
 as.list(c('', ' ','.001','0.001','<.001','<0.01','1e-2','not numeric','한글',NA_character_)),
 list(factor('0.04'),structure(.04,names='p'),list(.04),list(NULL),list(c(.03,.04))))
set.seed(94);values<-c(values,as.list(runif(500,-.1,1.1)))
checks<-0L
for(style in c('apa','leading_zero'))for(p in values) {
 options(statedu.p_value_format=style)
 stopifnot(identical(capture(reference(p)),capture(format_p(p)),num.eq=FALSE));checks<-checks+1L
}
cat('PASS:',checks,'exact p-string/type/condition/RNG comparisons.\n')
