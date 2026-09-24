Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
families<-c('binomial','poisson','negative_binomial','gamma')
tables<-list()
for(family in families) {
 rationale<-longitudinal_model_rationale('glmm',family,'exchangeable',FALSE)
 suggestions<-longitudinal_sensitivity_recommendations('glmm',family,'exchangeable',FALSE)
 tab<-data.frame(Item=c('Model rationale',rep('Recommendation',length(suggestions))),Details=c(rationale,suggestions))
 before<-serialize(tab,NULL)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  translated<-longitudinal_appendix_table(tab,lang)
  expected<-longitudinal_appendix_text(family,lang)
  stopifnot(grepl(expected,translated[[2]][1],fixed=TRUE))
  if(lang!='en')stopifnot(all(translated[[2]]!=tab$Details))
  stopifnot(identical(before,serialize(tab,NULL)))
  cat('CHECK GLMM prose:',family,lang,'\n')
 }
 tables[[family]]<-tab
}
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 unknown<-longitudinal_model_rationale('glmm','custom_family_123','exchangeable',FALSE)
 stopifnot(grepl('custom_family_123',longitudinal_appendix_text(unknown,lang),fixed=TRUE))
}
dir.create('tmp/longitudinal-glmm-prose',recursive=TRUE,showWarnings=FALSE)
saveRDS(tables,'tmp/longitudinal-glmm-prose/tables.rds')
