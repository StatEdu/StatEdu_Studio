Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/competing-sparse-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-data.frame(time=1:12,event=c(1,1,2,rep(0,9)),group=c(rep('Review',6),rep('사용자 <&> %s',6)))
result<-prepare_competing_risk_result(d,'time','event',group='group',rate_times=c(3,6))
codes<-c('few_interest_events','few_competing_events','zero_group_cause_events','sparse_group_cause_events')
prefixes<-c('Interest events = ','Competing events = ','Zero-event group-cause cells:','Group-cause cells with fewer than 5 events:')
en<-survival_stability_review(result,'en');en<-en[match(codes,en$Code),];stopifnot(identical(en$Code,codes))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 rows<-survival_stability_review(result,language);rows<-rows[match(codes,rows$Code),]
 html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
 evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
 guidance<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[4]')))
 for(i in seq_along(codes)){
  suffix<-substring(en$Evidence[i],nchar(prefixes[i])+1)
  stopifnot(endsWith(evidence[i],suffix))
 }
 if(language!='en')stopifnot(all(evidence!=en$Evidence),all(guidance!=en$Guidance))
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 if(language=='ja')entries[[1]]<-list(id='sparse',title='sparse',html=html)
 cat('PASS:',language,'four sparse warnings, suffixes, guidance and English main table\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
