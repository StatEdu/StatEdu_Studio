Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:12,Normality=c(2,4,3,7,5,9,6,10,12,8,13,15),third=c(4,3,6,5,9,7,12,9,13,15,11,16))
info<-data.frame(name=names(d),measurement='continuous')
groups<-list(c('Review','Normality','third'),c('Review','Normality','Missing <&> %s'))
results<-list(prepare_paired_rm_results(d,variable_groups=groups,variable_info=info,options=list(assumption_check=FALSE)),prepare_nonparametric_paired_results(d,c('Review','Review'),c('Normality','Missing <&> %s'),info),prepare_nonparametric_paired_rm_results(d,variable_groups=groups,variable_info=info))
titles<-c('Warnings / skipped repeated-measures rows','Skipped pairs','Skipped repeated-measures rows')
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(i in seq_along(results)) {
  stopifnot(nrow(results[[i]]$skipped)>0)
  html<-as.character(if(i==1)paired_rm_results_ui(results[[i]])else nonparametric_paired_results_ui(results[[i]]));doc<-xml2::read_html(html)
  title<-paired_appendix_text(titles[i],lang)
  stopifnot(title%in%xml2::xml_text(xml2::xml_find_all(doc,'//h3')))
  if(lang!='en'&&title==titles[i])missing<-c(missing,paste(lang,titles[i]))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  parts[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
}
if(length(missing))stop(paste('Untranslated titles:',paste(missing,collapse='; ')))
out<-'tmp/paired-diagnostic-titles-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three actual diagnostic section titles in eight languages; English main tables and source preserved\n')
