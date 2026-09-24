Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture<-function(expr){x<-tryCatch(expr,error=identity);stopifnot(inherits(x,'error'));x}
rows<-do.call(rbind,lapply(1:3,function(i)meta_normalize_effect(list(study_id=paste0('user',i),family='g',input_type='g_se',g=i/10,se=.1,moderator_categorical='group=A',moderator_continuous='age=30'))))
fit<-meta_fit_model(rows,'g',model='fixed')
errors<-list(moderator_select=capture(meta_fit_moderator(fit,'invalid')),
 moderator_rank=capture(meta_regression_components(1:3,rep(1,3),cbind(1,rep(1,3)),0)),
 moderator_df=capture(meta_fit_regression_matrix(1:2,rep(1,2),diag(2),'fixed','REML',.95)),
 moderator_studies=capture(meta_fit_moderator(fit,'continuous::absent')),
 moderator_levels=capture(meta_fit_moderator(fit,'categorical::group')),
 moderator_values=capture(meta_fit_moderator(fit,'continuous::age')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in names(errors))stopifnot(conditionMessage(errors[[key]])==statedu_t(paste0('meta.model_error.',key),'en'),
  meta_model_error_text(errors[[key]],lang)==statedu_t(paste0('meta.model_error.',key),lang))
 raw<-'External moderator: 사용자 <&> %s';stopifnot(meta_model_error_text(simpleError(raw),lang)==raw)
 cat('PASS:',lang,'six actual moderator error branches; external detail preserved\n')
}
