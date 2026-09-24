Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
values<-list(study_id='user <&> %s',family='g',input_type='g_se',g=.2,se=.1)
cases<-list(effect_variance=meta_result_ok(Inf,1,'test','test'),
 family=meta_normalize_effect(list(family='unsupported')),
 format=meta_normalize_effect(list(family='g',input_type='unsupported')),
 study_id=meta_normalize_effect(modifyList(values,list(study_id=''))))
maximum_year<-as.integer(format(Sys.Date(),'%Y'))+1L
years<-lapply(c(1799,maximum_year+1,.5),function(y)meta_normalize_effect(modifyList(values,list(publication_year=y))))
compound<-meta_normalize_effect(modifyList(values,list(se=0,moderator_continuous='bad',publication_year=1799)))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in names(cases))stopifnot(cases[[key]]$status=='error',
  meta_input_detail_text(cases[[key]]$message,lang)==statedu_t(paste0('meta.input_error.',key),lang))
 for(record in years)stopifnot(record$status=='error',record$study_id==values$study_id,
  meta_input_detail_text(record$message,lang)==sprintf(statedu_t('meta.input_error.year',lang),maximum_year))
 stopifnot(meta_input_detail_text(compound$message,lang)==paste(statedu_t('meta.input_error.g_se',lang),statedu_t('meta.input_error.pairs',lang),sprintf(statedu_t('meta.input_error.year',lang),maximum_year)))
 for(year in c('2027','2099'))stopifnot(meta_input_detail_text(sprintf(statedu_t('meta.input_error.year','en'),year),lang)==sprintf(statedu_t('meta.input_error.year',lang),year))
 raw<-paste(years[[1]]$message,'External <&> %s');stopifnot(meta_input_detail_text(raw,lang)==raw)
 for(y in c(1800,maximum_year))stopifnot(meta_normalize_effect(modifyList(values,list(publication_year=y)))$status=='valid')
 cat('PASS:',lang,'four common errors; year boundaries and dynamic value; triple compound; original user details\n')
}
