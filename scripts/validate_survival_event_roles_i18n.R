Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-event-roles-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
roles<-c('event_of_interest','competing_event','censored','other_state','exclude','unknown')
map<-data.frame(raw_value=as.character(1:6),role=roles,label=c('Review','Normality','사용자 <&> %s','event_of_interest','exclude','unknown'))
preflight<-survival_preflight(data.frame(t=1:6,e=1:6),list(data_shape='single_record',roles=list(time='t',event='e'),event_of_interest='1',event_map=map))
actual<-survival_reporting_event_map(list(preflight=preflight));stopifnot(setequal(actual$Role,roles))
custom<-actual;custom$Role[1]<-'사용자 <&> %s';custom[['Raw value']]<-custom$Label
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 rows<-if(kind=='actual')actual else custom
 html<-as.character(tagList(tags$h4(survival_appendix_title('Event-code mapping',language)),survival_simple_table(rows,table_language=language)))
 doc<-xml2::read_html(html,encoding='UTF-8')
 for(i in c(1,3))stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',i,']')))),rows[[i]]))
 values<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[2]')));known<-rows$Role%in%roles
 if(language=='en')stopifnot(identical(values,rows$Role))else stopifnot(all(values[known]!=rows$Role[known]),!any(grepl('Survival event role',values)))
 stopifnot(identical(values[!known],rows$Role[!known]))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'six roles, raw codes and user labels\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
