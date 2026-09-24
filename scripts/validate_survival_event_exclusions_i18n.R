Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-event-exclusions-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-data.frame(time=1:6,event=c(0,1,2,3,4,2))
mapping<-data.frame(raw_value=as.character(0:4),role=c('censored','event_of_interest','exclude','competing_event','other_state'),label=c('Review','사용자 <&> %s','exclude label','competing label','Normality'))
settings<-list(data_shape='single_record',roles=list(time='time',event='event'),event_of_interest='1',event_map=mapping)
standard<-survival_preflight(d,settings)
settings$objective<-'competing';competing<-survival_preflight(d,settings)
tables<-lapply(list(standard=standard,competing=competing),function(p)survival_cox_exclusion_table(list(preflight=p)))
codes<-c('excluded_event_code','competing_event_requires_competing_risk','unsupported_other_state')
stopifnot(all(codes%in%tables$standard[[1]]),!codes[2]%in%tables$competing[[1]],
 tables$standard$N[match(codes[1],tables$standard[[1]])]==2)
tables$custom<-data.frame('Exclusion reason'=c('Review','사용자 <&> %s','Normality'),N=c(2,3,1),check.names=FALSE)
for(kind in names(tables))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 rows<-tables[[kind]];html<-as.character(survival_simple_table(rows,table_language=language))
 doc<-xml2::read_html(html,encoding='UTF-8')
 values<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]')))
 counts<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[2]')))
 stopifnot(identical(as.numeric(counts),as.numeric(rows$N)))
 known<-rows[[1]]%in%codes
 if(language=='en'){baseline<-counts;stopifnot(identical(values,rows[[1]]))}else{
  stopifnot(identical(counts,baseline),all(values[known]!=rows[[1]][known]))
 }
 stopifnot(identical(values[!known],rows[[1]][!known]))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'event exclusions, counts and custom text\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
