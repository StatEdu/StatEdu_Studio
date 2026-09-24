Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-named-warnings-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');km<-prepare_km_single_analysis_result(d,'time','status',group='sex',rate_times=c(100,250,500))
idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
cr<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='cause_specific',rate_times=c(100,250,500))
codes<-c(crossing='crossing_survival_curves',ph='cause_specific_ph_signal',joint='cause_specific_categorical_joint_test_not_estimable')
labels<-c('Review','Normality','사용자 <&> %s','None')
for(kind in names(codes))for(signal in c(FALSE,TRUE))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-if(kind=='crossing')km else cr;flags<-c(signal,signal,signal,FALSE)
 if(kind=='crossing')r$crossing_table<-data.frame(Comparison=labels,`Review signal`=flags,check.names=FALSE)
 if(kind=='ph')r$cause_specific$ph_table<-data.frame(Term=labels,p=ifelse(flags,.01,.05))
 if(kind=='joint')r$cause_specific$categorical_joint_tests<-data.frame(Variable=labels,Estimable=!flags)
 rows<-survival_stability_review(r,language);rows<-rows[rows$Code==codes[[kind]],,drop=FALSE]
 stopifnot(nrow(rows)==as.integer(signal))
 if(signal){
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  evidence<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
  suffix<-paste(labels[1:3],collapse=if(kind=='crossing')'; ' else ', ')
  stopifnot(endsWith(evidence,suffix),!grepl('None',evidence,fixed=TRUE))
  if(language!='en')stopifnot(!grepl('Crossing survival curves:|Cause-specific Schoenfeld review signals:|Non-estimable cause-specific categorical joint tests:',evidence))
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 cat('PASS:',kind,signal,language,'warning selection and dictionary-collision names preserved\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
