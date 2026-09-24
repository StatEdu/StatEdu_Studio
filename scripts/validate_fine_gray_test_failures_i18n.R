Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-test-failures-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='fine_gray',rate_times=c(100,250,500))
codes<-c('fine_gray_model_test_not_estimable','fine_gray_categorical_joint_test_not_estimable')
for(kind in c('quiet','Statistic','df','p','joint','combined'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$fine_gray$model_tests<-data.frame(Statistic=1,df=1,p=.3)
 if(kind%in%c('Statistic','df','p'))r$fine_gray$model_tests[[kind]]<-NA_real_
 if(kind=='combined')r$fine_gray$model_tests$Statistic<-Inf
 joint<-kind%in%c('joint','combined')
 r$fine_gray$categorical_joint_tests<-data.frame(Variable=c('Review','사용자 <&> %s','Normality'),Estimable=if(joint)c(FALSE,FALSE,TRUE) else rep(TRUE,3))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind%in%c('Statistic','df','p','combined'))+as.integer(joint))
 if(nrow(rows)){
  stopifnot(all(rows$Level=='high'))
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  if(joint)stopifnot(endsWith(evidence[rows$Code==codes[2]],'Review, 사용자 <&> %s'),!any(grepl('Normality',evidence,fixed=TRUE)))
  if(language!='en')stopifnot(!grepl('The Fine-Gray pseudo|Non-estimable Fine-Gray categorical|Do not report a non-finite Fine-Gray|Review sparse levels',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_fine_gray_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'Fine-Gray test failure branches, raw variable names and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
