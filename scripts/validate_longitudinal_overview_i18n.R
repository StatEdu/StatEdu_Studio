Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
tables<-list()
for(label in c('Warning','Outcome','Value','사용자 결과')) {
 result<-list(outcome=label,id='Warning',time='Failed',offset_variable='Adequate',weight='No',
  n=120L,clusters=30L,time_points=4L,method='GEE',requested_family='gaussian',family='gamma',
  formula=stats::as.formula('`Warning` ~ `Failed`'),aic=123.456,bic=134.567)
 tab<-longitudinal_model_overview_table(list(result,result))
 before<-serialize(tab,NULL)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  for(family in c('gaussian','binomial','gamma'))stopifnot(identical(longitudinal_appendix_text(family,lang),statedu_t(paste0('longitudinal.family_name.',family),lang)))
  translated<-longitudinal_appendix_table(tab,lang)
  stopifnot(identical(names(translated)[-1],names(tab)[-1]))
  user_rows<-match(c('Outcome','ID','Time','Exposure / offset','Weight','Formula'),tab$Item)
  for(j in 2:ncol(tab)) {
   stopifnot(identical(translated[[j]][user_rows],tab[[j]][user_rows]))
   for(item in c('Requested family','Fitted family')) {
    i<-match(item,tab$Item)
    stopifnot(identical(translated[[j]][i],longitudinal_appendix_text(tab[[j]][i],lang)))
    if(lang!='en')stopifnot(translated[[j]][i]!=tab[[j]][i])
   }
   for(i in match(c('N','Clusters','Time points','AIC','BIC'),tab$Item))stopifnot(identical(translated[[j]][i],tab[[j]][i]))
  }
  stopifnot(identical(before,serialize(tab,NULL)))
  cat('PASS overview:',label,lang,'\n')
 }
 tables[[label]]<-tab
}
dir.create('tmp/longitudinal-overview',recursive=TRUE,showWarnings=FALSE)
saveRDS(tables,'tmp/longitudinal-overview/tables.rds')
