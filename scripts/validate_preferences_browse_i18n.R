Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 html<-as.character(about_preferences_tab_panel(language));doc<-xml2::read_html(html,encoding='UTF-8')
 button<-xml2::xml_find_all(doc,'//*[@id="browse_default_save_dir"]');stopifnot(length(button)==1L)
 text<-trimws(xml2::xml_text(button));stopifnot(identical(text,statedu_localized_text(language,'Browse','찾아보기')))
 if(language!='en')stopifnot(text!='Browse')
 # Translation must retain input identifiers, selected settings and action wiring.
 inputs<-xml2::xml_find_all(doc,'//input|//select|//button')
 ids<-xml2::xml_attr(inputs,'id');values<-xml2::xml_attr(inputs,'value')
 selected<-xml2::xml_attr(xml2::xml_find_all(doc,'//option[@selected]'),'value')
 selected<-selected[selected!=language]
 if(language=='en'){baseline_ids<-ids;baseline_values<-values;baseline_selected<-selected}else{
  stopifnot(identical(ids,baseline_ids),identical(values,baseline_values),identical(selected,baseline_selected))
 }
 menu<-xml2::read_html(as.character(shiny::navbarPage('Menu test',analysis_tab_panel(language=language))),encoding='UTF-8')
 panels<-xml2::xml_attr(xml2::xml_find_all(menu,'//a[@data-value]'),'data-value')
 ridge<-which(panels=='analysis_penalized');logistic<-which(panels=='analysis_logistic_regression')
 stopifnot(length(ridge)==1L,length(logistic)==1L,logistic==ridge+1L)
 cat('PASS:',language,'preferences Browse, stable controls/settings, penalized immediately before logistic\n')
}
js<-paste(readLines('www/easyflow.js',encoding='UTF-8',warn=FALSE),collapse='\n')
stopifnot(grepl("'analysis_penalized', 'analysis_logistic_regression'",js,fixed=TRUE))
