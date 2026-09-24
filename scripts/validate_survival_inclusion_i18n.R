Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-inclusion-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$time[1]<-NA;d$status[2]<-NA;d$age[3]<-NA
result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
flow<-survival_reporting_data_flow(result);excluded<-survival_cox_exclusion_table(result)
stopifnot(all(c('missing_time','missing_event','missing_covariate')%in%excluded[[1]]))
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 rows<-if(kind=='actual')excluded else data.frame('Exclusion reason'=c('missing_group','Review','사용자 <&> %s'),N=c(2L,3L,4L),check.names=FALSE)
 panels<-list(survival_simple_table(flow,table_language=language),survival_simple_table(rows,table_language=language))
 for(i in 1:2){
  doc<-xml2::read_html(as.character(panels[[i]]),encoding='UTF-8');headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  if(language!='en')stopifnot(!any(c('Source subjects','Analysis subjects','Exclusion reason')%in%headers))
  cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  if(i==1){if(language=='en')baseline<-cells else stopifnot(identical(cells,baseline))}
  else{
   values<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]'));counts<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[2]'))
   counts<-trimws(counts);values<-trimws(values);stopifnot(identical(as.numeric(counts),as.numeric(rows$N)))
   if(language=='en')count_baseline<-counts else stopifnot(identical(counts,count_baseline))
   known<-grepl('^missing_',rows[[1]])
   if(language=='en')stopifnot(identical(values,rows[[1]]))else stopifnot(all(values[known]!=rows[[1]][known]))
   stopifnot(identical(values[!known],rows[[1]][!known]))
  }
 }
 main<-as.character(survival_cox_result_html_table(result,language));cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')main_baseline<-cells else stopifnot(identical(cells,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=paste(vapply(panels,as.character,character(1)),collapse='\n'))
 cat('PASS:',kind,language,'inclusion counts, exclusion codes, custom text and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
