Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/competing-named-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='fine_gray',rate_times=c(100,250,500))
codes<-c('fine_gray_residual_time_pattern','fine_gray_sparse_censoring_stratum','cif_integrity_failure','gray_test_not_estimable')
for(label in c('Review','Normality','None','사용자 <&> %s'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result
 r$fine_gray$residual_review<-data.frame(Term=c(label,'unchanged'),`Review signal`=c(TRUE,FALSE),check.names=FALSE)
 r$censoring_group_table<-data.frame(`Censoring stratum`=c(label,'unchanged'),`Review signal`=c(TRUE,FALSE),check.names=FALSE)
 r$cif_integrity<-data.frame(Group=c(label,'unchanged'),`Integrity passed`=c(FALSE,TRUE),check.names=FALSE)
 r$gray_tests<-data.frame(CauseCode=c(label,'unchanged'),Estimable=c(FALSE,TRUE))
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code%in%codes,,drop=FALSE];stopifnot(nrow(rows)==4)
 html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
 evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
 stopifnot(all(endsWith(evidence,label)),!any(grepl('unchanged',evidence,fixed=TRUE)))
 if(language!='en')stopifnot(!grepl('CIF integrity failed for groups:|Non-estimable Gray tests for cause codes:|Schoenfeld-like residual time-pattern signals:|Sparse censoring-distribution strata:',paste(evidence,collapse='')))
 if(language=='ja')entries[[label]]<-list(id=label,title=label,html=html)
 main<-as.character(survival_simple_table(survival_fine_gray_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')baseline<-main else stopifnot(identical(main,baseline))
 cat('PASS:',label,language,'four named warnings, selected raw label and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
