Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(926)
d<-data.frame(id=rep(1:40,each=5),time=rep(0:4,40),x=rnorm(200))
d$y<-1+.3*d$x+.1*d$time+rep(rnorm(40),each=5)+rnorm(200)
r<-prepare_longitudinal_analysis_result(d,'y','id','time',predictors='x',model_type='lmm',family='gaussian',corstr='exchangeable')[[1]]
tables<-list(manuscript=r$manuscript_text,software=r$software_versions)
stopifnot(nrow(tables$manuscript)==5L,nrow(tables$software)>0)
out<-'tmp/longitudinal-manuscript-labels-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-serialize(tables,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(tables)) {
 original<-tables[[name]];translated<-longitudinal_appendix_table(original,lang)
 values<-c(names(original),if(name=='manuscript')original$Section)
 actual<-c(names(translated),if(name=='manuscript')translated[[1]])
 # These are valid native-language spellings, not untranslated fallbacks.
 same_spelling<-switch(lang,fr=c('Section','Version'),de=c('Software','Version'),es='Software',character())
 absent<-which(values==actual & !values %in% same_spelling)
 for(label in intersect(values,c('SuggestedText','Methods','Sensitivity','Software'))) {
  key<-paste0('analysis.ui.',tolower(label))
  expected<-statedu_t(key,lang)
  stopifnot(!identical(expected,key),all(actual[values==label]==expected))
 }
 if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,english=values[i])
 if(name=='software')for(j in seq_along(original))stopifnot(identical(original[[j]],translated[[j]]))
 if(name=='manuscript') {
  parts<-attr(original,'longitudinal_manuscript_parts')
  for(messages in parts$rows)for(message in messages[nzchar(messages)])if(lang!='en' && identical(message,longitudinal_appendix_text(message,lang)))missing[[length(missing)+1L]]<-data.frame(language=lang,english=message)
 }
 stopifnot(identical(before,serialize(tables,NULL)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated manuscript/software labels',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS actual LMM manuscript and software tables in eight languages; package versions and captured source preserved\n')
