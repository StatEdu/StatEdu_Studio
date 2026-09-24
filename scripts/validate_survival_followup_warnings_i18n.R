Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-followup-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');result<-prepare_km_single_analysis_result(d,'time','status',rate_times=c(100,250,500))
stopifnot(nrow(survival_km_rate_table(result))>0)
cases<-data.frame(tail=c(9,10,10,9,NA),censor=c(.8,.8,.801,.9,NA))
codes<-c('sparse_tail_risk_set','high_censoring_proportion')
for(i in seq_len(nrow(cases)))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 followup<-data.frame(`At risk at 90th percentile time`=cases$tail[i],`Censoring proportion`=cases$censor[i],check.names=FALSE)
 rows<-survival_stability_review(result,language,followup_fn=function(r)followup)
 expected<-codes[c(isTRUE(cases$tail[i]<10),isTRUE(cases$censor[i]>.8))]
 if(!length(expected))expected<-'no_major_stability_signal'
 stopifnot(identical(rows$Code,expected))
 html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
 evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
 for(j in seq_len(nrow(rows))){
  if(rows$Code[j]==codes[1])stopifnot(endsWith(evidence[j],as.character(cases$tail[i])))
  if(rows$Code[j]==codes[2])stopifnot(endsWith(evidence[j],survival_format_number(cases$censor[i])))
 }
 if(language!='en')stopifnot(!grepl('At risk near follow-up tail|Censoring proportion =|Review follow-up processes|No major signal from|Continue reviewing risk-set|Tail curve, CIF',xml2::xml_text(doc)))
 if(language=='ja')entries[[as.character(i)]]<-list(id=as.character(i),title=as.character(i),html=html)
 main<-as.character(survival_simple_table(survival_km_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',i,language,'tail/censor boundaries, missing values, no-signal branch and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
