Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-interpretation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
results<-list(km=prepare_km_single_analysis_result(d,'time','status'),cox=prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1'))
idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
results$competing<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
for(kind in names(results))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-results[[kind]]
 rows<-survival_interpretation_guide(r,language);english<-survival_interpretation_guide(r,'en')
 stopifnot(nrow(rows)==3)
 if(language!='en')stopifnot(all(rows$Guidance!=english$Guidance))
 symbols<-english$Topic%in%c('HR','CIF','sHR','Log-rank','RMST')
 stopifnot(identical(rows$Topic[symbols],english$Topic[symbols]))
 if(language!='en')stopifnot(all(rows$Topic[!symbols]!=english$Topic[!symbols]))
 full<-xml2::read_html(as.character(survival_reporting_guidance_panel(r,language)),encoding='UTF-8')
 ts<-xml2::xml_find_all(full,'//table');table<-ts[[length(ts)]]
 for(i in 1:2)stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(table,paste0('.//tbody/tr/td[',i,']')))),rows[[i]]))
 if(kind!='km'){
  main<-if(kind=='cox')as.character(survival_cox_result_html_table(r,language)) else as.character(survival_simple_table(survival_cause_specific_coef_table(r),table_role='main',table_language='en'))
  cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
  if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 }
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=as.character(tagList(tags$h3(statedu_localized_text(language,'Interpretation guide','해석 가이드')),HTML(as.character(table)))))
 cat('PASS:',kind,language,'actual panel, translated guidance, preserved statistical symbols and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
