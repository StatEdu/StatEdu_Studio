Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:12,Normality=c(2,4,3,7,5,9,6,10,12,8,13,15),third=c(4,3,6,5,9,7,12,9,13,15,11,16),ordinal=rep(1:3,4))
info<-data.frame(name=names(d),measurement=c(rep('continuous',3),'ordinal'))
missing<-'사용자 <&> %s missing'
results<-list(prepare_paired_results(d,c('Review','Review','Review'),c('Normality',missing,'ordinal'),info,options=list(assumption_check=FALSE)),prepare_paired_rm_results(d,variable_groups=list(c('Review','Normality','third'),c('Review','Normality',missing),c('Review','Normality','ordinal')),variable_info=info,options=list(assumption_check=FALSE)))
before<-serialize(results,NULL);captured<-list()
stopifnot(all(vapply(results,function(r)nrow(r$skipped)==2,logical(1))))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 for(i in seq_along(results)) {
  raw<-results[[i]]$skipped$Reason
  expected<-vapply(raw,paired_appendix_text,character(1),language=lang)
  if(lang!='en')stopifnot(all(expected!=raw))
  stopifnot(grepl(missing,expected[1],fixed=TRUE))
  html<-as.character(if(i==1)paired_results_ui(results[[i]])else paired_rm_results_ui(results[[i]]));doc<-xml2::read_html(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'));stopifnot(all(expected%in%cells))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  panels[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
out<-'tmp/paired-named-reasons-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS four actual paired/RM skipped reasons in eight languages; names, main tables and source preserved\n')
