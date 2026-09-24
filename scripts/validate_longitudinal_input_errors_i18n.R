Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture<-function(expr)tryCatch(expr,error=identity)
d<-data.frame(id=rep(1:4,each=3),time=rep(1:3,4),y=seq_len(12)/3,w=rep(1,12))
bad<-d;bad$w[1]<-NA_real_
unaligned<-d;rownames(unaligned)<-paste0('other',1:12)
weight_call<-function(raw,analyzed,weight='w')longitudinal_prepare_analysis_weights(raw,analyzed,'y','id','time','time',weight=weight,weight_type='sampling')
errors<-list(binary_levels=capture(longitudinal_binary_outcome_numeric(c('a','b','c'))),binary_values=capture(longitudinal_binary_outcome_numeric(1:3)),weights=capture(longitudinal_trim_weights(c(1,0))),weight_alignment=capture(weight_call(d,unaligned)),weight_select=capture(weight_call(d,d,'missing')),weight_missing=capture(weight_call(bad,bad)),glmm_gaussian=capture(prepare_longitudinal_analysis_result(d,'y','id','time',model_type='glmm',family='gaussian')))
for(key in names(errors))stopifnot(inherits(errors[[key]],'error'),conditionMessage(errors[[key]])==statedu_t(paste0('longitudinal.input_error.',key),'en'))
callback<-NULL
walk<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('function'))&&grepl('longitudinal_input_error_text',paste(deparse(node),collapse=' '),fixed=TRUE)&&identical(node[[2]],formals(function(e)NULL)))callback<<-node
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk(child)
}
walk(parse('R/server_longitudinal.R',encoding='UTF-8'));stopifnot(!is.null(callback))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang;cleared<-FALSE
 longitudinal_results<-function(value){stopifnot(is.null(value));cleared<<-TRUE}
 showNotification<-function(ui,type,duration,...){notice<<-list(text=ui,type=type,duration=duration)}
 for(key in names(errors)){
  translated<-statedu_t(paste0('longitudinal.input_error.',key),lang)
  stopifnot(longitudinal_input_error_text(errors[[key]],lang)==translated)
  eval(callback)(errors[[key]])
  stopifnot(cleared,notice$type=='error',notice$duration==8,notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),translated))
 }
 external<-simpleError('External: 사용자 %s <&>')
 stopifnot(longitudinal_input_error_text(external,lang)==conditionMessage(external))
 cat('PASS longitudinal input:',lang,'7 real errors, notification callback, raw external detail\n')
}
stopifnot(identical(longitudinal_binary_outcome_numeric(c(1,2,1)),c(0L,1L,0L)),all(is.finite(longitudinal_trim_weights(c(1,2,3)))))
