Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 state<-list(survival_contract_unit='day',survival_contract_event='None',survival_contract_covariates=c('Review','사용자 <&> %s'),survival_contract_origin='Normality <&> %s')
 doc<-xml2::read_html(as.character(survival_contract_setup_panel(c('None','Review','사용자 <&> %s'),language=lang,saved_values=state)),encoding='UTF-8')
 for(id in c('survival_contract_unit','survival_contract_event','survival_contract_covariates')) {
  chosen<-xml2::xml_attr(xml2::xml_find_all(doc,paste0('//select[@id="',id,'"]/option[@selected]')),'value')
  stopifnot(identical(chosen,state[[id]]))
 }
 stopifnot(xml2::xml_attr(xml2::xml_find_first(doc,'//input[@id="survival_contract_origin"]'),'value')==state$survival_contract_origin)
 map<-survival_event_map_panel(c(0,1,2),lang,saved_values=list(survival_event_role_3='competing_event',survival_event_label_3='None <&> %s',survival_event_map_confirmed=TRUE))
 doc<-xml2::read_html(as.character(map),encoding='UTF-8')
 stopifnot(xml2::xml_attr(xml2::xml_find_first(doc,'//select[@id="survival_event_role_3"]/option[@selected]'),'value')=='competing_event',length(xml2::xml_find_all(doc,'//input[@id="survival_event_map_confirmed"][@checked]'))==1L)
 cat('PASS:',lang,'single/multiple selections; raw user text; event mapping and checkbox restore\n')
}
