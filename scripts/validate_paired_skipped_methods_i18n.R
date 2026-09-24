Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:6,Normality=c(2,4,3,7,9,8),same=1:6,binary1=rep('yes',6),binary2=rep('yes',6))
info<-data.frame(name=names(d),measurement=c(rep('continuous',3),rep('binary',2)))
results<-list(prepare_paired_results(d,c('Review','binary1'),c('Normality','binary2'),info,options=list(assumption_check=FALSE)),prepare_nonparametric_paired_results(d,c('Review','Review','binary1'),c('Normality','same','binary2'),info,options=list(effect_size=TRUE)))
phrases<-c('Wilcoxon signed-rank test','Paired categorical test')
stopifnot(all(phrases%in%results[[2]]$skipped$Method))
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 expected<-vapply(phrases,paired_appendix_text,character(1),language=lang)
 if(lang!='en'&&any(expected==phrases))missing<-c(missing,lang)
 for(i in seq_along(results)) {
  html<-as.character(if(i==1)paired_results_ui(results[[i]])else nonparametric_paired_results_ui(results[[i]]));doc<-xml2::read_html(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
  stopifnot(expected[2]%in%cells,'Review - Normality'%in%cells)
  if(i==2)stopifnot(expected[1]%in%cells,'Review - same'%in%cells)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  panels[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
if(length(missing))stop(paste('Untranslated skipped method names:',paste(missing,collapse=', ')))
out<-'tmp/paired-skipped-methods-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual paired/nonparametric skipped method labels in eight languages; main tables and source preserved\n')
