Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-normality-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
cases<-list(
 non_gaussian=longitudinal_check_normality(c(1,2,3),'binomial'),
 too_short=longitudinal_check_normality(c(1,2),'gaussian'),
 constant=longitudinal_check_normality(rep(1,10),'gaussian'),
 skewed=longitudinal_check_normality(c(rep(0,19),100),'gaussian'),
 normal=longitudinal_check_normality(qnorm((1:40-.5)/40),'gaussian'))
stopifnot(cases$skewed$Issue, !cases$normal$Issue)
before<-serialize(cases,NULL)
untranslated<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(name in names(cases)) {
  original<-cases[[name]];translated<-longitudinal_appendix_table(original,lang)
  for(column in c('Check','Result','Interpretation','Recommendation')) {
   stopifnot(column %in% names(original))
   values<-original[[column]]
   actual<-translated[[match(column,names(original))]]
   if(lang!='en' && any(nzchar(values)&values==actual))
    untranslated<-c(untranslated,paste(lang,name,column,values,sep=' | '))
  }
  for(column in intersect(c('Statistic','p','Issue'),names(original)))
   stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 }
 # Identical words in user identifiers must never enter the translation path.
 probe<-data.frame(Variable=cases$non_gaussian$Interpretation,Details=cases$non_gaussian$Interpretation)
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe$Variable))
 stopifnot(identical(before,serialize(cases,NULL)))
}
if(length(untranslated))stop(paste(untranslated,collapse='\n'),call.=FALSE)
saveRDS(cases,file.path(out,'tables.rds'))
cat('PASS five actual normality branches in eight languages; numeric results and user identifiers unchanged\n')
