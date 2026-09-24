Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
diff<-c(-2,-1,0,1,2,3,4,5,100,-80)
d<-data.frame(Review=rep(0,10),Normality=diff);info<-data.frame(name=names(d),measurement='continuous')
r<-prepare_paired_results(d,'Review','Normality',info,options=list(assumption_check=TRUE,effect_size=TRUE))
ids<-c(as.character(1:8),'Review <&> %s','Normality')
raw<-c(actual=r$checks$Outliers[1],named=paired_outlier_summary(diff,ids),count=paired_outlier_summary(diff,rep('',10)),none=paired_outlier_summary(1:10))
stopifnot(raw[1]=='2 detected (IDs: 9, 10)',raw[3]=='2 detected',raw[4]=='None detected')
before<-serialize(r,NULL);captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(kind in names(raw)) {
  current<-r;current$checks$Outliers[1]<-raw[[kind]]
  expected<-paired_appendix_text(raw[[kind]],lang)
  if(lang!='en')stopifnot(expected!=raw[[kind]])
  if(kind=='named')stopifnot(grepl('Review <&> %s, Normality',expected,fixed=TRUE))
  if(kind!='none')stopifnot(grepl('2',expected,fixed=TRUE))
  html<-as.character(paired_results_ui(current));doc<-xml2::read_html(html)
  stopifnot(expected%in%xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  main[[kind]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[kind]])>0)
  parts[[kind]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(r,NULL)))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
}
out<-'tmp/paired-outliers-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS paired outlier summary variants in eight languages; IDs, count, main tables and source preserved\n')
