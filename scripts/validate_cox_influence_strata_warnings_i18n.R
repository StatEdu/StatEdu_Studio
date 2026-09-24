Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-influence-strata-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
codes<-c('dfbetas_influence_signal','zero_event_stratum','sparse_event_stratum')
for(kind in c('quiet','mixed','boundary'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result
 r$influence_table<-data.frame(`Review signal`=if(kind=='mixed')c(TRUE,FALSE,TRUE,NA) else c(FALSE,NA),check.names=FALSE)
 r$strata_table<-data.frame(Stratum=c('Review','사용자 <&> %s','Normality','None'),Events=if(kind=='mixed')c(0,4,1,5) else if(kind=='boundary')c(5,5,5,5) else c(10,20,30,40))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==if(kind=='mixed')3 else 0)
 if(nrow(rows)){
  stopifnot(identical(rows$Level,c('review','high','review')))
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  stopifnot(endsWith(evidence[1],'2'),endsWith(evidence[2],'Review'),endsWith(evidence[3],'사용자 <&> %s, Normality'),!grepl('None',paste(evidence,collapse=''),fixed=TRUE))
  if(language!='en')stopifnot(!grepl('Influential coefficient rows|No events in strata|Fewer than 5 events in strata|Review rows exceeding|Review baseline-hazard|Review coefficient and stratum',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_cox_result_html_table(result,language));cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 cat('PASS:',kind,language,'influence count, strata boundary, raw labels and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
