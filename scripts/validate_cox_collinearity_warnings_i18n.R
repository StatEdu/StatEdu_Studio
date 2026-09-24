Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-collinearity-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
cases<-list(none=c(4.99,29.99),moderate=c(5,29.99),high=c(10,29.99),nonfinite=c(Inf,29.99),missing=c(NA,29.99),condition=c(1,30))
codes<-c(moderate='elevated_vif',high='high_vif',nonfinite='nonfinite_vif',missing='nonfinite_vif',condition='high_condition_number')
for(kind in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$collinearity_table<-data.frame(VIF=cases[[kind]][1]);r$condition_number<-cases[[kind]][2]
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind!='none'))
 if(nrow(rows)){
  stopifnot(rows$Code==codes[[kind]],rows$Level==if(kind%in%c('high','nonfinite','missing'))'high' else 'review')
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  if(!kind%in%c('nonfinite','missing'))stopifnot(endsWith(evidence,survival_format_number(cases[[kind]][if(kind=='condition')2 else 1])))
  if(language!='en')stopifnot(!grepl('Maximum design-column VIF|Design condition number|At least one design-column|Review exact collinearity|Review instability|Review covariate redundancy|Review numerical instability',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_cox_result_html_table(result,language));cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 cat('PASS:',kind,language,'VIF/condition boundaries, severity, values and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
