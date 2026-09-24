Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
fit<-function(se)meta_fit_model(do.call(rbind,lapply(seq_along(se),function(i)meta_normalize_effect(list(study_id=paste0('User 한글 ',i),family='g',input_type='g_se',g=c(.1,.4,.2,.8,.5)[i],se=se[i])))),'g',model='fixed')
small<-meta_egger_test(fit(c(.1,.2)))
equal<-meta_egger_test(fit(rep(.1,4)))
valid<-meta_egger_test(fit(c(.1,.2,.15,.3,.22)))
stopifnot(!small$available,!equal$available,valid$available)
fixtures<-list(minimum=small,equal=equal,intercept_error=modifyList(small,list(message=statedu_t('meta.egger.intercept_error','en'))))
before<-list(fixtures,valid)
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 tr<-function(key)statedu_t(paste0('meta.egger.',key),lang)
 for(key in names(fixtures)) {
  tab<-meta_egger_results_table(fixtures[[key]],lang)
  stopifnot(identical(names(tab),c(tr('status'),tr('reason'))),tab[[1]]==tr('unavailable'),tab[[2]]==tr(key))
 }
 unknown<-modifyList(small,list(message='User 한글 %s <&>'))
 stopifnot(meta_egger_results_table(unknown,lang)[[2]]==unknown$message)
 for(p in c(.01,.05,.5)) {
  item<-modifyList(valid,list(p_value=p));tab<-meta_egger_results_table(item,lang)
  baseline<-meta_egger_results_table(item,'en')
  stopifnot(identical(unname(tab[1:7]),unname(baseline[1:7])),tab[[8]]==tr(if(p<.05)'positive' else 'negative'))
 }
 stopifnot(identical(before,list(fixtures,valid)))
 cat('PASS Egger:',lang,'guard reasons, interpretation boundary, numeric preservation\n')
}
