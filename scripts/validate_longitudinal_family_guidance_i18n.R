Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-family-guidance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
tables<-list()
for(family in c('gaussian','binomial','count','poisson','negative_binomial','gamma')) {
 tables[[paste0('family_',family)]]<-longitudinal_check_response_family(family,'glmm')
}
for(family in c('binomial','poisson','negative_binomial','gamma')) {
 notes<-longitudinal_model_notes('glmm',family,'exchangeable',FALSE,TRUE,id='Subject',time='Visit')
 target<-notes[startsWith(notes,'Exponentiated coefficients are reported')]
 stopifnot(length(target)==1L)
 tables[[paste0('ratio_',family)]]<-data.frame(Interpretation=target)
}
before<-serialize(tables,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(tables)) {
 original<-tables[[name]];translated<-longitudinal_appendix_table(original,lang)
 for(column in intersect(c('Check','Result','Interpretation','Recommendation'),names(original))) {
  values<-original[[column]];actual<-translated[[match(column,names(original))]]
  absent<-which(nzchar(values)&values==actual)
  if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,table=name,english=values[i])
 }
 for(column in intersect(c('Statistic','p','Issue'),names(original)))stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 probe<-data.frame(Variable=original$Interpretation)
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe[[1]]),identical(before,serialize(tables,NULL)))
}
if(length(missing)) {
 missing<-do.call(rbind,missing);write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated family guidance',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS ten generated family/link and exponentiation guidance tables in eight languages; numeric and user data preserved\n')
