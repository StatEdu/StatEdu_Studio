source('scripts/validate_longitudinal_manuscript_labels_i18n.R',encoding='UTF-8')
out<-'tmp/longitudinal-model-names-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
results<-list(lmm=r)
d$y<-as.integer(d$y>median(d$y))
results$glmm<-prepare_longitudinal_analysis_result(d,'y','id','time',predictors='x',model_type='glmm',family='binomial',corstr='exchangeable')[[1]]
tables<-list(overview=longitudinal_model_overview_table(results))
tables$labels<-data.frame(Method=c(longitudinal_model_label('lmm'),vapply(c('gaussian','binomial','count','poisson','negative_binomial','gamma'),function(f)longitudinal_model_label('glmm',f),character(1))))
before<-serialize(tables,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 overview<-longitudinal_appendix_table(tables$overview,lang)
 labels<-longitudinal_appendix_table(tables$labels,lang)
 for(j in seq_len(nrow(tables$labels)))if(lang!='en' && tables$labels[[1]][j]==labels[[1]][j])missing[[length(missing)+1L]]<-data.frame(language=lang,english=tables$labels[[1]][j])
 method_row<-which(tables$overview$Item=='Method')
 for(j in 2:ncol(overview))stopifnot(overview[[j]][method_row]==longitudinal_appendix_text(tables$overview[[j]][method_row],lang))
 cells<-attr(tables$overview,'result_user_cells')
 for(i in seq_len(nrow(cells)))stopifnot(identical(overview[cells[i,1],cells[i,2]],tables$overview[cells[i,1],cells[i,2]]))
 stopifnot(identical(names(overview)[-1],names(tables$overview)[-1]),identical(before,serialize(tables,NULL)))
 # Direction belongs to the publication main table and must remain English.
 options(statedu.app_language=lang)
 main<-longitudinal_role_table(longitudinal_publication_estimate_table(results$lmm),'main')
 stopifnot('Direction'%in%names(main))
 if(lang=='en')main_values<-unclass(main) else stopifnot(identical(lapply(main,identity),lapply(main_values,identity)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated model names',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS two fitted model overviews and seven model labels in eight languages; user cells and English publication table preserved\n')
