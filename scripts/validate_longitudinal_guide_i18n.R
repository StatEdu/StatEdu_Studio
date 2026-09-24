Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
fixtures<-list()
for(model in c('gee','lmm','glmm','panel_fe','panel_re'))for(slope in c(FALSE,TRUE)) {
 fixtures[[paste(model,slope)]]<-list(model_type=model,corstr='exchangeable',random_slope=slope,id='Warning. 사용자',time='Failed. 시점')
}
for(corstr in c('reml_un','reml_ar1'))fixtures[[corstr]]<-list(model_type='lmm',corstr=corstr)
tab<-longitudinal_assumption_review_table(fixtures);before<-serialize(tab,NULL);missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 translated<-longitudinal_appendix_table(tab,lang)
 for(j in 2:ncol(tab)) {
  stopifnot(identical(tab[[j]][3],translated[[j]][3]))
  for(i in 1:2) {
   if(lang!='en' && identical(tab[[j]][i],translated[[j]][i]))missing<-unique(c(missing,paste(lang,tab[[j]][i])))
   for(name in c('Warning. 사용자','Failed. 시점'))if(grepl(name,tab[[j]][i],fixed=TRUE))stopifnot(grepl(name,translated[[j]][i],fixed=TRUE))
  }
 }
 stopifnot(identical(before,serialize(tab,NULL)))
 cat('CHECK guide:',lang,'\n')
}
if(length(missing))stop(paste(missing,collapse='\n'))
dir.create('tmp/longitudinal-guide',recursive=TRUE,showWarnings=FALSE)
saveRDS(list(guide=tab),'tmp/longitudinal-guide/tables.rds')
