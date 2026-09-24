Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-read.csv('scripts/fixtures/survival_validation.csv')
results<-list(km=prepare_km_single_analysis_result(d,'time','status'),cox=prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1'))
idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
results$competing<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
for(kind in names(results))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-results[[kind]];rows<-survival_followup_diagnostics(r);stopifnot(nrow(rows)==1,ncol(rows)==7)
 html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
 headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'));values<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
 if(language=='en')baseline<-values else stopifnot(identical(values,baseline),!any(headers%in%names(rows)))
 full<-xml2::read_html(as.character(survival_reporting_guidance_panel(r,language)),encoding='UTF-8')
 tables<-xml2::xml_find_all(full,'//table')
 stopifnot(identical(xml2::xml_text(xml2::xml_find_all(tables[[2]],'.//th')),headers),identical(xml2::xml_text(xml2::xml_find_all(tables[[2]],'.//td')),values))
 cat('PASS:',kind,language,'all seven follow-up headers localized, actual panel and numerical values unchanged\n')
}
stopifnot(nrow(survival_followup_diagnostics(list()))==0)
