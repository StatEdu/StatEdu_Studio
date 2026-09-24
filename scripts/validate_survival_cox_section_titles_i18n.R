Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$sex<-factor(d$sex)
d$stratum<-ifelse(d$ph.ecog>1,'Review','Normality')
r<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1',strata='stratum',cluster='id')
adjusted<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1',adjusted_group='sex',adjusted_bootstrap_reps=0L)
titles<-c('Categorical reference levels and contrast coding','Stratum event counts','Robust-variance cluster summary','Supplementary statistics and diagnostics','Marginal adjusted survival')
tables<-list(r$categorical_reference_table,survival_cox_strata_table(r),survival_cox_cluster_summary_table(r),survival_cox_statistic_table(r),survival_adjusted_survival_overview_table(adjusted))
stopifnot(all(vapply(tables,nrow,integer(1))>0))
before<-serialize(list(r,adjusted),NULL);missing<-list();captured<-list()
out<-'tmp/survival-cox-section-titles-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 full<-as.character(tagList(survival_cox_results_panel(r,lang),survival_cox_results_panel(adjusted,lang)))
 doc<-xml2::read_html(full);headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h3|//h4|//h5'))
 for(title in titles) {
  actual<-survival_appendix_title(title,lang)
  stopifnot(any(grepl(actual,headings,fixed=TRUE)))
  if(lang!='en'&&actual==title)missing[[length(missing)+1L]]<-data.frame(language=lang,english=title)
 }
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"))
 stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 captured[[lang]]<-as.character(tagList(lapply(seq_along(titles),function(i)tagList(tags$h4(survival_appendix_title(titles[i],lang)),survival_simple_table(tables[[i]],table_language=lang)))))
 stopifnot(identical(before,serialize(list(r,adjusted),NULL)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated Cox section titles',call.=FALSE)
}
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS five real Cox panel section titles in eight languages; English main tables and source results preserved\n')
