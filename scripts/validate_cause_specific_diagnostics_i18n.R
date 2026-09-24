Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cause-specific-diagnostics-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',group='sex',rate_times=c(100,250,500))
codes<-c('cause_specific_ph_signal','cause_specific_dfbetas_signal')
for(kind in c('none','signal'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$cause_specific<-list(ph_table=data.frame(Term=c('Review','사용자 <&> %s','Normality'),p=c(.05,if(kind=='signal').01 else .5,NA)),
 influence_table=data.frame('Review signal'=c(kind=='signal',FALSE,kind=='signal'),check.names=FALSE))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==if(kind=='none')0L else 2L)
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  stopifnot(endsWith(evidence[1],'사용자 <&> %s'),!grepl('Review|Normality',evidence[1]),endsWith(evidence[2],'2'))
  if(language!='en')stopifnot(!grepl('Cause-specific Schoenfeld review signals:|Cause-specific influential coefficient rows|Review Schoenfeld residual plots|Review observations exceeding',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'PH boundary, influence count, variable and English CIF table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
