Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/competing-epv-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',rate_times=c(100,250,500))
code<-'low_interest_events_per_covariate'
for(kind in c('cause_specific','fine_gray'))for(events in c(0,19,20))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$preflight$counts$events<-events
 r[[kind]]<-list(coef_table=data.frame(Term=c('Review','사용자 <&> %s'),B=c(.1,.2)))
 review<-survival_stability_review(r,language);rows<-review[review$Code==code,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(events<20))
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  stopifnot(endsWith(evidence,survival_format_number(events/2)))
  if(language!='en')stopifnot(!grepl('Interest events/covariate|Reconsider competing-risk regression',xml2::xml_text(doc)))
  if(language=='ja')entries[[paste(kind,events)]]<-list(id=paste(kind,events),title=paste(kind,events),html=html)
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,events,language,'EPV warning boundary, ratio and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
