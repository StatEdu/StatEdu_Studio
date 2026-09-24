Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$sex<-factor(d$sex)
tables<-list()
for(reps in c(0L,1L,25L)) {
 r<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1',adjusted_group='sex',adjusted_bootstrap_reps=reps,adjusted_times='100, 250')
 stopifnot(is.list(r$adjusted_survival))
 tables[[as.character(reps)]]<-survival_adjusted_survival_overview_table(r)
 stopifnot(identical(r$adjusted_survival$ci_available,reps==25L))
}
out<-'tmp/survival-adjusted-overview-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-serialize(tables,NULL);missing<-list();captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 for(original in tables) {
  translated<-survival_appendix_localize_table(original,lang)
  app<-!original$Item%in%c('Group variable','Bootstrap requested','Bootstrap successful','Effective bootstrap ratio','Bootstrap seed') | original$Value=='N/A'
  values<-c(original$Item,original$Value[app]);actual<-c(translated[[1]],translated[[2]][app])
  absent<-which(values==actual & !(lang=='es' & values=='No'))
  if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,english=values[i])
  stopifnot(identical(original$Value[!app],translated[[2]][!app]),identical(before,serialize(tables,NULL)))
 }
 captured[[lang]]<-as.character(tagList(lapply(tables,survival_simple_table,table_language=lang)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated adjusted survival overview',call.=FALSE)
}
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual adjusted Cox overview with absent, insufficient and available bootstrap CI in eight languages; values and source preserved\n')
