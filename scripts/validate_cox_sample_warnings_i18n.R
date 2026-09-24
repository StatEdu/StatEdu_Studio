Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-sample-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
cases<-data.frame(epp=c(0,4.99,5,9.99,10),clusters=c(1,9,10,29,30))
for(i in seq_len(nrow(cases)))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$events_per_parameter<-cases$epp[i];r$cluster<-'Review 사용자 <&> %s';r$data[[r$cluster]]<-rep(seq_len(cases$clusters[i]),length.out=nrow(r$data))
 codes<-c('low_events_per_parameter','few_robust_variance_clusters');review<-survival_stability_review(r,language);rows<-review[review$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==if(i<5)2 else 0)
 if(nrow(rows)){
  stopifnot(all(rows$Level==if(i<3)'high' else 'review'))
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  stopifnot(endsWith(evidence[1],survival_format_number(cases$epp[i])),endsWith(evidence[2],as.character(cases$clusters[i])))
  if(language!='en')stopifnot(!grepl('Events/parameter|Robust-variance clusters|Review coefficient instability|With few clusters',xml2::xml_text(doc)))
  if(language=='ja')entries[[as.character(i)]]<-list(id=as.character(i),title=as.character(i),html=html)
 }
 main<-as.character(survival_cox_result_html_table(result,language))
 cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 cat('PASS:',i,language,'warning thresholds, severity, numeric evidence and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
