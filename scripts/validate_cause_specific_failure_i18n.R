Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cause-specific-failure-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',group='sex',rate_times=c(100,250,500))
codes<-c('cause_specific_model_test_not_estimable','cause_specific_categorical_joint_test_not_estimable')
for(kind in c('valid','statistic','df','p'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;tests<-data.frame(Statistic=2,df=1,p=.15)
 if(kind!='valid')tests[[switch(kind,statistic='Statistic',df='df',p='p')]]<-NA_real_
 r$cause_specific<-list(model_tests=tests,categorical_joint_tests=data.frame(Variable=c('Review','사용자 <&> %s'),Estimable=c(TRUE,kind=='valid')))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==if(kind=='valid')0L else 2L)
 if(nrow(rows)){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  stopifnot(endsWith(evidence[2],'사용자 <&> %s'),!grepl('Review',evidence[2],fixed=TRUE))
  if(language!='en')stopifnot(!grepl('At least one cause-specific|Non-estimable cause-specific categorical|Do not report non-finite|Review sparse levels',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',kind,language,'cause-specific failures, variable preservation and English CIF table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
