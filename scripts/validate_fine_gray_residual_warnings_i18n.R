Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-residual-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',rate_times=c(100,250,500))
codes<-c('fine_gray_residual_diagnostic_limited','fine_gray_residual_time_pattern','fine_gray_sparse_censoring_stratum')
for(kind in c('normal','limited','signals'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$fine_gray<-list(residual_data=data.frame('Event time'=rep(seq_len(if(kind=='limited')4 else 5),each=2),check.names=FALSE),
 residual_review=data.frame(Term=c('Normality','사용자 <&> %s'),'Review signal'=c(FALSE,kind=='signals'),check.names=FALSE))
 r$censoring_group_table<-data.frame('Censoring stratum'=c('Normality','Review <&> %s'),'Review signal'=c(FALSE,kind=='signals'),check.names=FALSE)
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==switch(kind,normal=0L,limited=1L,signals=2L))
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  if(kind=='limited')stopifnot(endsWith(evidence,'4'))else stopifnot(endsWith(evidence[1],'사용자 <&> %s'),endsWith(evidence[2],'Review <&> %s'),!any(grepl('Normality',evidence)))
  if(language!='en')stopifnot(!grepl('Unique target-failure times available|Schoenfeld-like residual time-pattern signals:|Sparse censoring-distribution strata:|With few target-failure times|Review the residual plots|Review instability in stratum-specific',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'unique times, selected names, warnings and English CIF table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
