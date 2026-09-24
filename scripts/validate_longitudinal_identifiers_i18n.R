Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
families<-c('auto','count','poisson','negative_binomial')
correlations<-unname(longitudinal_correlation_choices())
probe<-data.frame(Item=c(rep('Requested family',length(families)),rep('Correlation / random effect',length(correlations))),
 Value=c(families,paste0('Working correlation: ',correlations)))
missing<-character();before<-serialize(probe,NULL)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 translated<-longitudinal_appendix_table(probe,lang)
 for(i in seq_len(nrow(probe)))if(lang!='en' && identical(probe$Value[i],translated[[2]][i]))missing<-c(missing,paste(lang,probe$Value[i]))
 if(lang!='en')for(corstr in correlations) {
  value<-longitudinal_appendix_text(corstr,lang)
  if(value==corstr)missing<-c(missing,paste(lang,corstr))
  stopifnot(grepl(value,longitudinal_appendix_text(paste0('Working correlation: ',corstr),lang),fixed=TRUE))
 }
 for(choices in list(longitudinal_correlation_choices(),longitudinal_family_choices())) {
  localized<-longitudinal_ui_choices(choices,lang)
  stopifnot(identical(unname(localized),unname(choices)))
 }
 stopifnot(identical(before,serialize(probe,NULL)))
 cat('CHECK identifiers:',lang,'\n')
}
if(length(missing))stop(paste(missing,collapse='\n'))
dir.create('tmp/longitudinal-identifiers',recursive=TRUE,showWarnings=FALSE)
saveRDS(list(identifiers=probe),'tmp/longitudinal-identifiers/tables.rds')
