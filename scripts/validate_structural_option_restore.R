Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 values<-list(structural_cfa_estimator='MLR',structural_cfa_reliability_bootstrap='1000',structural_cfa_common_method_enabled=FALSE,structural_cfa_validity_formula='model_implied',structural_cfa_common_method_methods=character(0),structural_cfa_power_details='사용자 <메모> & Normality',structural_cfa_mi_holdout_seed=123L)
 ui<-structural_equation_workspace(c('x1','x2'),analysis_type='cfa',language=lang,option_values=values)
 doc<-xml2::read_html(as.character(ui),encoding='UTF-8')
 stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(doc,'//select[@id="structural_cfa_estimator"]/option[@selected]'),'value'),'MLR'))
 stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(doc,'//input[@name="structural_cfa_validity_formula"][@checked]'),'value'),'model_implied'))
 stopifnot(length(xml2::xml_find_all(doc,'//input[@name="structural_cfa_common_method_methods"][@checked]'))==0)
 stopifnot(length(xml2::xml_find_all(doc,'//input[@id="structural_cfa_common_method_enabled"][@checked]'))==0)
 stopifnot(identical(xml2::xml_text(xml2::xml_find_first(doc,'//textarea[@id="structural_cfa_power_details"]')),values$structural_cfa_power_details))
 stopifnot(identical(xml2::xml_attr(xml2::xml_find_first(doc,'//input[@id="structural_cfa_mi_holdout_seed"]'),'value'),'123'))
 cat('PASS:',lang,'select/radio/cleared checkbox group, false checkbox, numeric seed and escaped user text\n')
}
