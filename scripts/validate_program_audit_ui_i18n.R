Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
phrases<-c('Create data','ID conditional statistic','Number of missing values','Complete case flag',
 'Draw paths on the shared canvas. The design variables set under Complex Samples are applied automatically.')
keys<-paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(phrases))))
codes<-c('id_stat','N_miss','F_miss');user<-c('Review 사용자 <&> %s','condition','value')
baseline_choices<-unname(transform_template_choices('en'))
baseline_expressions<-vapply(codes,transform_template_expression,character(1),variables=user)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 expected<-vapply(keys,statedu_t,character(1),language=lang)
 if(lang!='en')stopifnot(all(expected!=phrases))
 doc<-xml2::read_html(as.character(data_editor_id_aggregate_panel(lang)))
 stopifnot(xml2::xml_text(xml2::xml_find_first(doc,'//*[@id="run_id_aggregate"]'))==expected[1])
 choices<-transform_template_choices(lang)
 stopifnot(identical(unname(choices),baseline_choices),identical(unname(names(choices)[match(codes,unname(choices))]),unname(expected[2:4])))
 doc<-xml2::read_html(as.character(data_editor_variable_transformation_panel(lang)))
 for(label in expected[2:4])stopifnot(grepl(label,xml2::xml_text(doc),fixed=TRUE))
 doc<-xml2::read_html(as.character(complex_sample_custom_model_tab_panel(lang)))
 stopifnot(grepl(expected[5],xml2::xml_text(xml2::xml_find_first(doc,'//*[contains(@class,"app-subtitle")]')),fixed=TRUE),
  identical(vapply(codes,transform_template_expression,character(1),variables=user),baseline_expressions))
 cat('PASS',lang,'five visible labels; template codes and user expressions preserved\n')
}
