Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/gray-failure-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',group='sex',rate_times=c(100,250,500))
code<-'gray_test_not_estimable'
for(kind in c('valid','mixed','all'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$gray_tests<-data.frame(CauseCode=c('Review','사용자 <&> %s'),Estimable=switch(kind,valid=c(TRUE,TRUE),mixed=c(TRUE,FALSE),all=c(FALSE,FALSE)))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code==code,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind!='valid'))
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  expected<-paste(r$gray_tests$CauseCode[!r$gray_tests$Estimable],collapse=', ')
  stopifnot(endsWith(evidence,expected))
  if(kind=='mixed')stopifnot(!grepl('Review',evidence,fixed=TRUE))
  if(language!='en')stopifnot(!grepl('Non-estimable Gray tests for cause codes:|Do not report statistics or p-values',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_gray_display_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'Gray warning selection, codes and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
