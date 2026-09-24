Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-fit-failures-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='fine_gray',rate_times=c(100,250,500))
codes<-c('fine_gray_not_converged','fine_gray_not_estimable')
cases<-list(normal=c(TRUE,TRUE),convergence=c(FALSE,TRUE),estimability=c(TRUE,FALSE),both=c(FALSE,FALSE),missing=c(NA,NA))
for(kind in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$fine_gray$converged<-cases[[kind]][1];r$fine_gray$estimable<-cases[[kind]][2]
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 expected<-codes[!vapply(cases[[kind]],isTRUE,logical(1))]
 stopifnot(identical(rows$Code,expected),all(rows$Level=='high'))
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  if(language!='en')stopifnot(!grepl('Fine-Gray convergence flag|At least one Fine-Gray coefficient|Do not report the Fine-Gray estimates|Do not report non-estimable Fine-Gray',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_fine_gray_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'fit status flags, warning severity and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
