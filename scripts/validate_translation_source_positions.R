source('R/utils.R',encoding='UTF-8')

make_table<-function(mode,overlays){
  env<-new.env(parent=.GlobalEnv);sys.source('R/labels.R',env)
 if(mode=='baseline'){
  count<-0L
  restore<-function(x){
   if(identical(x,quote(if(use_positions) translations[[key_index]] else translations[[key]]))){count<<-count+1L;return(quote(translations[[key]]))}
   if(is.call(x)&&identical(x[[1L]],as.name('<-'))&&identical(x[[2L]],as.name('use_positions'))){count<<-count+1L;return(quote(NULL))}
   if(is.call(x))for(i in seq_along(x))x[i]<-list(restore(x[[i]]))
   x
  }
  closure<-environment(env$statedu_translation_table)
  closure$initialize<-restore(closure$initialize);stopifnot(count==2L)
 }
 if(!is.null(overlays))env$statedu_locale_overlays<-function()overlays
 env$statedu_translation_table
}
capture<-function(f){
 conditions<-character();set.seed(941)
 stdout<-capture.output(value<-withCallingHandlers(tryCatch(f(),error=function(e)list(error=conditionMessage(e))),
 warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
 message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')}))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
cases<-list(NULL,list(),list(ko=list(ui.data='changed',new_key='new')),
 list(ko=setNames(list('first','second','existing','duplicate'),c('new','new','ui.data','ui.data'))),
 list(ko=list(ui.data=NULL,empty=character(),number=4,missing=NA_character_)),
 list(en=list(new='first'),ko=list(new='second'),zz=list(new='third')),
 list(ko='invalid',en=list('unnamed')),list(ko=list(language.xx='Unknown',language.en='English override')),
 list(ko=setNames(list('empty','value'),c('','ui.data'))),list(ko=setNames(list('missing','value'),c(NA_character_,'ui.data'))),
 list(ko=structure(list(ui.data='value'),class='custom_overlay')))
`[[.custom_overlay`<-function(x,i,...){if(is.numeric(i))stop('Numeric access not allowed');NextMethod()}
for(overlays in cases){
 before<-make_table('baseline',overlays);after<-make_table('current',overlays)
 for(i in 1:2)stopifnot(identical(capture(before),capture(after),num.eq=FALSE))
}

cat('PASS:',length(cases),'overlay scenarios; initial/cached values, conditions, stdout and RNG identical.\n')
