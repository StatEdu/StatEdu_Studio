source('scripts/validate_longitudinal_sensitivity_table_i18n.R',encoding='UTF-8')
out<-'tmp/longitudinal-fit-guidance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
tables<-setNames(tables,paste0('sensitivity_',names(tables)))
for(model in c('lmm','glmm','panel_fe','panel_re')) {
 input<-d;family<-'gaussian'
 if(model=='glmm') {input$y<-as.integer(d$y>median(d$y));family<-'binomial'}
 fitted<-longitudinal_fit_model(input,'y','id','time',c('time','x'),model,family,'exchangeable')
 tables[[paste0('details_',model)]]<-longitudinal_fit_details(fitted$model,model)
}
for(model in c('gee','lmm','glmm','panel_fe','panel_re','unknown')) {
 tables[[paste0('guidance_',model)]]<-data.frame(
  Interpretation=longitudinal_model_rationale(model,'gaussian','exchangeable',FALSE),
  Recommendation=longitudinal_sensitivity_recommendations(model,'gaussian','exchangeable',FALSE))
}
before<-serialize(tables,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(tables)) {
 original<-tables[[name]];translated<-longitudinal_appendix_table(original,lang)
 for(column in intersect(c('Item','Interpretation','Recommendation'),names(original))) {
  values<-original[[column]];actual<-translated[[match(column,names(original))]]
  absent<-which(nzchar(values)&values==actual)
  if(lang!='en' && length(absent))for(i in absent)missing[[length(missing)+1L]]<-data.frame(language=lang,table=name,english=values[i])
 }
 if(startsWith(name,'details_')) {
  numeric_rows<-grepl('^[+-]?[0-9.]+$',original$Value)
  stopifnot(identical(original$Value[numeric_rows],translated[[2]][numeric_rows]))
 }
 probe<-data.frame(Variable='Approximate ICC',Details='Approximate ICC')
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe$Variable),identical(before,serialize(tables,NULL)))
}
if(length(missing)) {
 missing<-do.call(rbind,missing);write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated fit guidance',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS thirteen fit/sensitivity/guidance tables in eight languages; numeric results and user labels preserved\n')
