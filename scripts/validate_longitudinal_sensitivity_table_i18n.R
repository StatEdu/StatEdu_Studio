Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(925)
d<-data.frame(id=rep(1:40,each=5),time=rep(0:4,40),x=rnorm(200))
d$y<-1+.3*d$x+.1*d$time+rep(rnorm(40),each=5)+rnorm(200)
tables<-list();missing<-character()
for(model in c('gee','lmm','panel_fe')) {
 tab<-longitudinal_sensitivity_analysis_results(d,'y','id','time',c('time','x'),model,'gaussian','exchangeable')
 stopifnot(nrow(tab)>0,all(tab$Status!='Failed'));before<-serialize(tab,NULL)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  translated<-longitudinal_appendix_table(tab,lang)
  user_comparison<-data.frame(Comparison=c('exchangeable','Random intercept only'))
  stopifnot(identical(longitudinal_appendix_table(user_comparison,lang)[[1]],user_comparison[[1]]))
  for(j in match(c('Analysis','Comparison','Status','Note'),names(tab)))for(i in seq_len(nrow(tab))) {
   value<-tab[[j]][i]
   if(lang!='en' && nzchar(value) && identical(value,translated[[j]][i]))missing<-unique(c(missing,paste(lang,value)))
  }
  numeric_rows<-tab$Value!='Available'
  stopifnot(identical(tab$Value[numeric_rows],translated[[match('Value',names(tab))]][numeric_rows]),identical(before,serialize(tab,NULL)))
  cat('CHECK sensitivity:',model,lang,'\n')
 }
 tables[[model]]<-tab
}
if(length(missing)) {writeLines(unique(sub('^[a-z]+ ','',missing)),'tmp/sensitivity-table-missing.txt',useBytes=TRUE);stop('Untranslated sensitivity cells; see tmp/sensitivity-table-missing.txt')}
dir.create('tmp/longitudinal-sensitivity-table',recursive=TRUE,showWarnings=FALSE)
saveRDS(tables,'tmp/longitudinal-sensitivity-table/tables.rds')
