Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
values<-c('0','1','Review','Normality','None','사용자 <&> %s')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 html<-as.character(survival_event_map_panel(c(values,NA,'',values[1]),language));doc<-xml2::read_html(html,encoding='UTF-8')
 selects<-xml2::xml_find_all(doc,'//select');stopifnot(length(selects)==length(values))
 for(i in seq_along(selects)){
  opts<-xml2::xml_find_all(selects[[i]],'.//option')
  stopifnot(identical(xml2::xml_attr(opts,'value'),c('unknown','censored','event_of_interest','competing_event','exclude')))
  selected<-xml2::xml_attr(xml2::xml_find_first(selects[[i]],'.//option[@selected]'),'value')
  stopifnot(selected==if(i==1)'censored' else if(i==2)'event_of_interest' else 'unknown')
  if(language!='en')stopifnot(!any(xml2::xml_text(opts)%in%c('Unknown','Censored','Event of interest','Competing event','Exclude')))
 }
 inputs<-xml2::xml_find_all(doc,'//input[@type="text"]')
 stopifnot(identical(xml2::xml_attr(inputs,'value'),values))
 labels<-xml2::xml_text(xml2::xml_find_all(doc,'//div[@class="survival-event-map-raw"]/label'))
 stopifnot(all(endsWith(labels,values)))
 ids<-xml2::xml_attr(xml2::xml_find_all(doc,'//input|//select'),'id')
 if(language=='en')baseline<-ids else stopifnot(identical(ids,baseline),!grepl('Observed event-code mapping|I confirmed the meaning|The 0/1 roles',xml2::xml_text(doc)),!any(xml2::xml_attr(inputs,'placeholder')=='Event label'))
 stopifnot(length(xml2::xml_find_all(doc,'//input[@id="survival_event_map_confirmed"][@checked]'))==0)
 empty<-xml2::read_html(as.character(survival_event_map_panel(character(),language)),encoding='UTF-8')
 if(language!='en')stopifnot(!grepl('Select an event variable',xml2::xml_text(empty),fixed=TRUE))
 cat('PASS:',language,'event-map UI, raw values, role IDs/defaults, confirmation and empty state\n')
}
