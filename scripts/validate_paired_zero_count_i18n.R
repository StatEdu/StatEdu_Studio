Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:5,Normality=c(1,3,4,5,6));info<-data.frame(name=names(d),measurement='ordinal')
results<-list(prepare_paired_results(d,'Review','Normality',info,options=list(assumption_check=FALSE,effect_size=TRUE)),prepare_nonparametric_paired_results(d,'Review','Normality',info,options=list(effect_size=TRUE)))
before<-serialize(results,NULL);captured<-list()
tie<-'Tied absolute differences were present; the large-sample Wilcoxon approximation was used.'
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 for(raw in c('1 zero difference was omitted from the Wilcoxon signed-rank calculation.','7 zero differences were omitted from the Wilcoxon signed-rank calculation.','12 zero difference(s) were omitted from the Wilcoxon signed-rank calculation.'))for(suffix in c('',paste0(' ',tie))) {
  value<-paired_appendix_text(paste0(raw,suffix),lang)
  count<-sub(' .*','',raw);stopifnot(grepl(count,value,fixed=TRUE))
  if(lang!='en')stopifnot(!grepl('zero difference',value,fixed=TRUE))
  if(nzchar(suffix))stopifnot(grepl(paired_appendix_text(tie,lang),value,fixed=TRUE))
 }
 panels<-list();main<-list()
 for(i in seq_along(results)) {
  raw<-results[[i]]$warnings$Warning[1];stopifnot(startsWith(raw,'1 zero difference'))
  expected<-paired_appendix_text(raw,lang)
  html<-as.character(if(i==1)paired_results_ui(results[[i]])else nonparametric_paired_results_ui(results[[i]]))
  doc<-xml2::read_html(html);cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  stopifnot(expected%in%cells,'Review - Normality'%in%cells)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  panels[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
out<-'tmp/paired-zero-count-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual paired/nonparametric zero-count warnings and saved singular/plural variants in eight languages; source/main preserved\n')
