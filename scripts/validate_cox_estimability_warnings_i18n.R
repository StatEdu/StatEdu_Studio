Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-estimability-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
codes<-c('nonfinite_coefficient','categorical_joint_test_not_estimable')
for(kind in c('quiet','infinite','missing','joint','combined'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result;r$coef_table$B[]<-0.1
 if(kind%in%c('infinite','combined'))r$coef_table$B[1]<-Inf
 if(kind=='missing')r$coef_table$B[1]<-NA_real_
 joint<-kind%in%c('joint','combined')
 r$categorical_joint_tests<-data.frame(Variable=c('Review','사용자 <&> %s','Normality'),Estimable=if(joint)c(FALSE,FALSE,TRUE) else rep(TRUE,3))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(kind%in%c('infinite','missing','combined'))+as.integer(joint))
 if(nrow(rows)){
  stopifnot(all(rows$Level=='high'))
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  if(joint)stopifnot(endsWith(evidence[rows$Code==codes[2]],'Review, 사용자 <&> %s'),!any(grepl('Normality',evidence,fixed=TRUE)))
  if(language!='en')stopifnot(!grepl('One or more coefficients|Non-estimable categorical joint tests|Review sparsity|Review sparse levels',xml2::xml_text(doc)))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 main<-as.character(survival_cox_result_html_table(result,language));cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))
 cat('PASS:',kind,language,'estimability warning branches, raw variable names and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
