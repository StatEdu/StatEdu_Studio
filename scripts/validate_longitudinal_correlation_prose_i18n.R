Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
formats<-c('Working correlation structure: %s.','The selected working correlation is %s.',
 'Report the selected working correlation (%s) and robust sandwich inference.',
 'Compare the selected GEE working correlation (%s) with independence, exchangeable, and AR(1) when feasible.',
 'Use GEE when the target is a population-averaged longitudinal effect. Report the selected working correlation (%s) and robust sandwich inference.', 'GEE (%s)')
structures<-unname(longitudinal_correlation_choices())
tables<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(structure in structures)for(format in formats) {
  source<-sprintf(format,structure);translated<-longitudinal_appendix_text(source,lang)
  expected<-if(lang=='en')structure else statedu_t(paste0('longitudinal.identifier.',structure),lang)
  stopifnot(grepl(expected,translated,fixed=TRUE))
  if(lang=='en')stopifnot(identical(source,translated))
 }
 # Unknown text must not have identifiers replaced as substrings.
 name<-'사용자.exchangeable.Warning'
 stopifnot(grepl(name,longitudinal_appendix_text(sprintf(formats[[1]],name),lang),fixed=TRUE))
 cat('PASS correlation prose:',lang,'\n')
}
for(structure in structures)tables[[structure]]<-data.frame(Item='Recommendation',Details=sprintf(formats,structure))
dir.create('tmp/longitudinal-correlation-prose',recursive=TRUE,showWarnings=FALSE)
saveRDS(tables,'tmp/longitudinal-correlation-prose/tables.rds')
