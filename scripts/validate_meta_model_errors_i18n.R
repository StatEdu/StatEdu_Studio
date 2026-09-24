Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture<-function(expr){x<-tryCatch(expr,error=identity);stopifnot(inherits(x,'error'));x}
rows<-do.call(rbind,lapply(1:3,function(i)meta_normalize_effect(list(study_id=paste0('user',i),family='g',input_type='g_se',g=i/10,se=.1))))
bad<-rows;bad$status[1]<-'error'
invalid<-rows;invalid$vi[1]<-0
errors<-list(confidence=capture(meta_fit_model(rows,'g',conf_level=1)),
 estimator=capture(meta_fit_model(rows,'g',tau_method='unsupported')),
 input_errors=capture(meta_fit_model(bad,'g')),
 two_effects=capture(meta_fit_model(rows[1,,drop=FALSE],'g')))
errors$finite_effects<-capture(meta_fit_model(invalid,'g'))
family_error<-capture(meta_fit_model(rows,'unknown'))
calls<-list()
walk<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('showNotification'))&&grepl('meta_model_error_text',paste(deparse(node),collapse=' '),fixed=TRUE))calls[[length(calls)+1L]]<<-node
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk(child)
}
walk(parse('R/server_meta.R',encoding='UTF-8'));stopifnot(length(calls)==2L)
showNotification<-function(ui,type,duration,...){notice<<-list(text=ui,type=type,duration=duration)}
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 language<-function()lang
 for(key in names(errors)) {
   result<-errors[[key]];moderator<-result
   expected<-statedu_t(paste0('meta.model_error.',key),lang)
   stopifnot(conditionMessage(result)==statedu_t(paste0('meta.model_error.',key),'en'),meta_model_error_text(result,lang)==expected)
   for(call in calls){eval(call);stopifnot(notice$text==expected,notice$type=='error',notice$duration==8)}
 }
 stopifnot(meta_model_error_text(family_error,lang)==statedu_t('meta.input_error.family',lang))
 raw<-'External: 사용자 <&> %s';stopifnot(meta_model_error_text(simpleError(raw),lang)==raw)
 cat('PASS:',lang,'five actual model guards; both notification calls; shared family error; raw detail\n')
}
