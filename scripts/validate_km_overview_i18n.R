Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/km-overview-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
for(method in c('km','life_table')){
 result<-prepare_km_single_analysis_result(d,'time','status',analysis_method=method)
 for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language);r<-result
  if(kind=='custom'){
   r$time_origin<-'Review';r$time_unit<-'Normality';r$entry<-'사용자 <&> %s';r$time<-'Time';r$event<-'Events';r$event_value<-'None'
  }
  rows<-survival_km_overview_table(r,language)
  html<-as.character(survival_simple_table(rows,table_language=language));doc<-xml2::read_html(html,encoding='UTF-8')
  vals<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[2]')))
  stopifnot(identical(vals,as.character(rows$Value)))
  if(language=='en')baseline<-vals else stopifnot(identical(vals[c(2:7,9:12)],baseline[c(2:7,9:12)]))
  items<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]')))
  if(language!='en')stopifnot(!'Entry variable'%in%items)
  main<-as.character(survival_simple_table(survival_km_summary_table(result),table_role='main',table_language='en'))
  if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
  if(language=='ja')entries[[paste(method,kind)]]<-list(id=paste(method,kind),title=paste(method,kind),html=html)
  cat('PASS:',method,kind,language,'overview data, localized entry label and English main table\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
