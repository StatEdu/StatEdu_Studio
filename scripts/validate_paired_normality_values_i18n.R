Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
results<-lapply(c(40,12),function(n){d<-data.frame(Review=seq_len(n),Normality=seq_len(n)+sin(seq_len(n))+seq_len(n)/20);info<-data.frame(name=names(d),measurement='continuous');prepare_paired_results(d,'Review','Normality',info,options=list(assumption_check=TRUE,effect_size=TRUE))})
stopifnot(results[[1]]$checks$`Check Result`=='Skewness/Kurtosis',results[[2]]$checks$`Check Result`=='Shapiro-Wilk')
before<-serialize(results,NULL);captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(i in seq_along(results)) {
  r<-results[[i]];raw<-paired_check_summary(r$checks);expected<-paired_appendix_text(raw,lang)
  if(i==1){if(lang!='en')stopifnot(expected!=raw);stopifnot(grepl(r$checks$Skewness,expected,fixed=TRUE),grepl(r$checks$Kurtosis,expected,fixed=TRUE))}else stopifnot(expected==raw)
  html<-as.character(paired_results_ui(r));doc<-xml2::read_html(html);cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  stopifnot(expected%in%cells,'Review - Normality'%in%cells)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  parts[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
}
out<-'tmp/paired-normality-values-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual skewness/kurtosis and Shapiro-Wilk branches in eight languages; precision, names, main tables and source preserved\n')
