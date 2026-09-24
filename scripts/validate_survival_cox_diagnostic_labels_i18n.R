Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-read.csv('scripts/fixtures/survival_validation.csv')
d$sex<-factor(d$sex);d$stratum<-ifelse(d$ph.ecog>1,'Warning','Value')
r<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1',strata='stratum')
tables<-list(strata=survival_cox_strata_table(r),joint=survival_cox_categorical_joint_test_table(r))
stopifnot(nrow(tables$strata)==2L,nrow(tables$joint)>0,all(tables$joint$Status=='Estimable'))
failed<-r;failed$categorical_joint_tests$Estimable<-FALSE
tables$not_estimable<-survival_cox_categorical_joint_test_table(failed)
out<-'tmp/survival-cox-diagnostic-labels-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-serialize(tables,NULL);missing<-list();captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 for(name in names(tables)) {
  original<-tables[[name]];translated<-survival_appendix_localize_table(original,lang)
  values<-c(names(original),if('Status'%in%names(original))original$Status)
  actual<-c(names(translated),if('Status'%in%names(original))translated[[match('Status',names(original))]])
  same_spelling<-c('df','p',if(lang=='fr')'Variance',if(lang%in%c('es','fr','de'))'Variable',if(lang=='de')'Status',if(lang%in%c('es','fr'))'Estimable')
  for(label in intersect(values,c('Stratum','Records','Levels','Wald chi-square','Estimable'))) {
   key<-paste0('analysis.ui.',gsub('[^a-z0-9]+','_',tolower(label)))
   stopifnot(all(actual[values==label]==statedu_t(key,lang)))
  }
  absent<-which(values==actual & !values%in%same_spelling)
  if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,english=values[i])
  for(column in setdiff(names(original),c('Status','Variance')))stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 }
 captured[[lang]]<-as.character(tagList(lapply(tables,survival_simple_table,table_language=lang)))
 stopifnot(identical(before,serialize(tables,NULL)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated Cox diagnostic labels',call.=FALSE)
}
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS fitted Cox strata and categorical tests plus injected non-estimable display in eight languages; numeric and user values preserved\n')
