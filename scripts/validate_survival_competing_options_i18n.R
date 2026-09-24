Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
variables<-c('Review','Normality','None','사용자 <&> %s')
controls<-function(doc)lapply(xml2::xml_find_all(doc,'//select|//input'),function(n)list(id=xml2::xml_attr(n,'id'),value=xml2::xml_attr(n,'value'),multiple=xml2::xml_attr(n,'multiple'),options=xml2::xml_attr(xml2::xml_find_all(n,'./option'),'value'),selected=xml2::xml_attr(xml2::xml_find_all(n,'./option[@selected]'),'value')))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(regression in c('none','cause_specific','fine_gray','both')) {
  values<-list(time=variables[1],event=variables[2],group=variables[3],covariates=variables[4],censoring_group=variables[4],regression=regression,censored_value='0',interest_value='1',competing_values='2, 3',rate_times='12, 36')
  doc<-xml2::read_html(as.character(survival_competing_setup_panel(variables,values,lang)),encoding='UTF-8')
  en<-xml2::read_html(as.character(survival_competing_setup_panel(variables,values,'en')),encoding='UTF-8')
  stopifnot(identical(controls(doc),controls(en)))
  stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(doc,'//*[@data-display-if]'),'data-display-if'),xml2::xml_attr(xml2::xml_find_all(en,'//*[@data-display-if]'),'data-display-if')))
  actual<-xml2::xml_text(xml2::xml_find_all(doc,'//select[@id="survival_competing_covariates"]/option'))
  stopifnot(identical(actual,variables))
  if(!lang %in% c('en','es','fr'))stopifnot(xml2::xml_text(xml2::xml_find_first(doc,'//h4'))!='1. Variables')
  if(lang!='en')stopifnot(xml2::xml_text(xml2::xml_find_first(doc,'//label[@for="survival_competing_regression"]'))!='Regression estimand')
 }
 raw<-'Review Normality None 사용자 <&> %s'
 missing<-survival_preflight(data.frame(x=1:2),list(data_shape='single_record',roles=list(time=raw,event='x')))
 invalid<-survival_preflight(data.frame(t=c('bad','2'),e=c(0,1)),list(data_shape='single_record',roles=list(time='t',event='e')))
 stopifnot('missing_columns' %in% missing$issues$code,'invalid_time_encoding' %in% invalid$issues$code)
 for(audit in list(missing,invalid)) {
  before<-audit
  messages<-survival_issue_text(audit$issues$code,audit$issues$message,lang)
  if('missing_columns' %in% audit$issues$code)stopifnot(grepl(raw,messages[audit$issues$code=='missing_columns'],fixed=TRUE))
  if(lang!='en')stopifnot(all(messages[audit$issues$code %in% c('missing_columns','invalid_time_encoding')]!=audit$issues$message[audit$issues$code %in% c('missing_columns','invalid_time_encoding')]))
  stopifnot(identical(before,audit))
 }
 for(code in c('competing_event_requires_competing_risk','unsupported_other_state'))if(lang!='en')stopifnot(survival_issue_text(code,'English fallback',lang)!='English fallback')
 cat('PASS:',lang,'4 estimands; control values, user labels, conditional expression; real missing-column/invalid-duration errors and 2 extra messages\n')
}
