Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:5,Normality=c(2,3,5,7,11),same=1:5,constant=2:6,one=c(1,NA,NA,NA,NA),two=c(2,NA,NA,NA,NA))
info<-data.frame(name=names(d),measurement='continuous')
phrases<-c('At least two complete paired cases are required.','The paired differences are all zero; no paired test was performed.','The paired differences have zero variance; paired t-test was not performed.')
results<-list(
 prepare_paired_results(d,c('Review','one'),c('Normality','two'),info,options=list(assumption_check=FALSE,effect_size=TRUE)),
 prepare_paired_results(d,c('Review','Review'),c('Normality','same'),info,options=list(assumption_check=FALSE,effect_size=TRUE)),
 prepare_paired_results(d,c('Review','Review'),c('Normality','constant'),info,options=list(assumption_check=FALSE,effect_size=TRUE)))
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(i in seq_along(results)) {
  doc<-xml2::read_html(as.character(paired_results_ui(results[[i]])))
  text<-xml2::xml_text(doc);expected<-paired_appendix_text(phrases[i],lang)
  if(!grepl(expected,text,fixed=TRUE)){print(list(lang=lang,case=i,expected=expected,cells=xml2::xml_text(xml2::xml_find_all(doc,'//td'))));stop('Expected guard missing')}
  stopifnot(grepl('Review',text,fixed=TRUE),grepl('Normality',text,fixed=TRUE))
  if(lang!='en'&&expected==phrases[i])missing<-c(missing,paste(lang,i))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  parts[[i]]<-as.character(paired_results_ui(results[[i]]))
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
 stopifnot(identical(before,serialize(results,NULL)))
}
if(length(missing))stop(paste('Untranslated paired guards:',paste(missing,collapse=', ')))
out<-'tmp/paired-guard-messages-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three actual paired guard branches in eight languages; user names, English main tables and source preserved\n')
