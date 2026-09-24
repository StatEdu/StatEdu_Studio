Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:12,Normality=c(2,4,3,7,5,9,6,10,12,8,13,15),third=c(4,3,6,5,9,7,12,9,13,15,11,16),same1=1:12,same2=1:12,one1=c(1,rep(NA,11)),one2=c(2,rep(NA,11)),one3=c(3,rep(NA,11)),binary1=rep('yes',12),binary2=rep('yes',12))
info<-data.frame(name=names(d),measurement=c(rep('continuous',8),rep('binary',2)))
results<-list(prepare_paired_results(d,c('Review','binary1'),c('Normality','binary2'),info,options=list(assumption_check=FALSE)),prepare_paired_rm_results(d,variable_groups=list(c('Review','Normality','third'),c('one1','one2','one3'),c('Review','same1','same2')),variable_info=info,options=list(assumption_check=FALSE)))
phrases<-c('At least two complete paired cases with at least two observed categories are required.','At least two complete repeated-measures cases are required.','All repeated measurements are identical within subjects; no repeated-measures test was performed.')
stopifnot(identical(unname(unlist(lapply(results,function(r)r$skipped$Reason))),phrases))
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 for(i in seq_along(results)) {
  raw<-results[[i]]$skipped$Reason;expected<-vapply(raw,paired_appendix_text,character(1),language=lang)
  if(lang!='en'&&any(expected==raw))missing<-c(missing,lang)
  html<-as.character(if(i==1)paired_results_ui(results[[i]])else paired_rm_results_ui(results[[i]]));doc<-xml2::read_html(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'));stopifnot(all(expected%in%cells),'Review - Normality'%in%cells || i==2)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  panels[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
if(length(missing))stop(paste('Untranslated guard messages:',paste(unique(missing),collapse=', ')))
out<-'tmp/paired-remaining-guards-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three actual categorical/RM guards across eight languages; English main tables and source preserved\n')
