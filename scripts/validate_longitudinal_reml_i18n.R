Sys.setenv(STATEDU_MODULE_CACHE='false')
source('scripts/validate_repeated_lmm_integration.R',encoding='UTF-8')
out<-'tmp/longitudinal-reml-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
tables<-list();results<-list()
for(mode in c('reml_un','reml_ar1')) {
 r<-prepare_longitudinal_analysis_result(d,'y','id','time',predictors=c('group','interaction'),
  model_type='lmm',family='gaussian',corstr=mode)[[1]]
 results[[mode]]<-r
 tables[[paste0(mode,'_checks')]]<-r$assumption_checks
 tables[[paste0(mode,'_details')]]<-r$fit_details
 tables[[paste0(mode,'_recommendations')]]<-data.frame(Recommendation=r$recommendations)
}
before<-serialize(list(results,tables),NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(tables)) {
 original<-tables[[name]];stopifnot(is.data.frame(original),nrow(original)>0)
 translated<-longitudinal_appendix_table(original,lang)
 for(column in intersect(c('Check','Result','Interpretation','Recommendation','Item'),names(original))) {
  values<-original[[column]];actual<-translated[[match(column,names(original))]]
  absent<-which(nzchar(values)&values==actual)
  if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,table=name,english=values[i])
 }
 for(column in intersect(c('Statistic','p','Issue','Value'),names(original)))
  stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 probe<-data.frame(Variable=c('REML convergence','Maximum REML gradient'),Details=c('REML convergence','Maximum REML gradient'))
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe$Variable),identical(before,serialize(list(results,tables),NULL)))
}
if(length(missing)) {
 missing<-do.call(rbind,missing);write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated REML diagnostics',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS actual UN/AR1 REML diagnostic, detail and recommendation tables in eight languages; source results and numeric/technical values preserved\n')
