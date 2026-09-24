Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cause-specific-fit-failure-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='cause_specific',rate_times=c(100,250,500))
cases<-list(normal=TRUE,failure=FALSE,missing=NA,absent=NULL)
for(kind in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$cause_specific$estimable<-cases[[kind]]
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code=='cause_specific_cox_not_estimable',,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind!='normal'),all(rows$Level=='high'))
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  if(language!='en')stopifnot(!grepl('At least one cause-specific Cox coefficient|Do not report non-estimable cause-specific Cox',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_cause_specific_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'cause-specific estimability flag, localized warning and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
