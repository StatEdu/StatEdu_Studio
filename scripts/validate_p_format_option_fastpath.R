source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-normalize_p_value_format;body(reference)<-as.call(as.list(body(reference))[-2L])
args<-commandArgs(TRUE)
if(length(args)){e<-new.env(parent=.GlobalEnv);sys.source(args[[1]],e);reference<-e$normalize_p_value_format;environment(reference)<-.GlobalEnv}
as.character.format_option_probe<-function(x,...) {message('option diagnostic');unclass(x)}
capture<-function(expr) {
 conditions<-character();set.seed(81)
 value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
 list(value=value,conditions=conditions,rng=.Random.seed)
}
values<-c(as.list(c('apa','leading_zero','APA','LEADING_ZERO','leading-zero','zero','0','',' apa ','invalid','한글',NA_character_)),
 list(NULL,character(),0L,1,TRUE,NA,list('apa'),list('leading_zero'),c('apa','zero'),structure('apa',names='format'),factor('leading_zero'),structure('apa',class='format_option_probe')))
for(x in values)stopifnot(identical(capture(reference(x)),capture(normalize_p_value_format(x)),num.eq=FALSE))
cat('PASS:',length(values),'exact p-format option/type/condition/RNG comparisons.\n')
