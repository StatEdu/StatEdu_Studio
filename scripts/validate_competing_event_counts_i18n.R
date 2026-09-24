Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/competing-event-counts-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
map<-data.frame(raw_value=c('Review','None'),role=c('event_of_interest','competing_event'),label=c('Normality','사용자 <&> %s'))
counts<-survival_competing_event_count_table(c(rep(1,6),rep(2,2),0,0),c(rep('Review',8),rep('사용자 <&> %s',2)),map)
stopifnot(all(c(0,2,6)%in%counts$Events))
for(kind in c('counts','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 rows<-survival_competing_event_count_display_table(list(event_count_table=if(kind=='counts')counts else counts[FALSE,]))
 html<-as.character(tagList(tags$h4(survival_appendix_title('Group-by-cause event counts',language)),survival_simple_table(rows,table_language=language)))
 doc<-xml2::read_html(html,encoding='UTF-8')
 if(kind=='counts'){
  for(i in c(1,2,4))stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',i,']')))),rows[[i]]))
  values<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[position()>=5 and position()<=7]'))
  if(language=='en')baseline<-values else{
   stopifnot(identical(values,baseline),!'Event proportion'%in%xml2::xml_text(xml2::xml_find_all(doc,'//th')))
   roles<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[3]')))
   stopifnot(all(roles!=rows$Role))
   reviews<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[8]')))
   stopifnot(all(reviews[nzchar(rows$Review)]!=rows$Review[nzchar(rows$Review)]),all(!nzchar(reviews[!nzchar(rows$Review)])))
  }
  if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 cat('PASS:',kind,language,'event counts, review branches and user strings\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
