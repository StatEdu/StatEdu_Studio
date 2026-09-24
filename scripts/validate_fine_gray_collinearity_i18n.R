Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-collinearity-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',group='sex',rate_times=c(100,250,500))
cases<-list(none=c(4.99,29.99),moderate=c(5,29.99),high=c(10,29.99),nonfinite=c(Inf,29.99),condition=c(1,30))
codes<-c(moderate='fine_gray_elevated_vif',high='fine_gray_high_vif',nonfinite='fine_gray_nonfinite_vif',condition='fine_gray_high_condition_number')
for(kind in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$fine_gray<-list(collinearity_table=data.frame(VIF=cases[[kind]][1]),condition_number=cases[[kind]][2])
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind!='none'))
 if(nrow(rows)){
  stopifnot(rows$Code==codes[[kind]])
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  if(kind!='nonfinite')stopifnot(endsWith(evidence,survival_format_number(cases[[kind]][if(kind=='condition')2 else 1])))
  if(language!='en')stopifnot(!grepl('Maximum Fine-Gray VIF|Fine-Gray design condition number|At least one Fine-Gray design-column|Review exact collinearity|Review instability in cause-specific|Review Fine-Gray covariate|Review numerical instability',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'VIF/condition boundaries, precision and English CIF table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
