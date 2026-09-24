Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
b<-data.frame(Review=rep(c('yes','no'),each=20),Normality=rep(c('no','yes'),each=20))
cdata<-expand.grid(Review=c('Review','Normality','사용자 <&> %s'),Normality=c('Review','Normality','사용자 <&> %s'),stringsAsFactors=FALSE);cdata<-cdata[rep(1:9,each=4),]
results<-list(prepare_paired_results(b,'Review','Normality',data.frame(name=names(b),measurement='binary')),prepare_paired_results(cdata,'Review','Normality',data.frame(name=names(cdata),measurement='categorical')),prepare_paired_results(cdata,'Review','Normality',data.frame(name=names(cdata),measurement='categorical'),options=list(bowker=TRUE)))
methods<-c('McNemar test','Stuart-Maxwell test','Bowker symmetry test')
short<-c('McNemar','Stuart-Maxwell','Bowker')
stopifnot(identical(vapply(results,function(r)r$table$Method[1],character(1)),methods))
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(i in seq_along(results)) {
  label<-paired_appendix_text(short[i],lang);long<-paired_appendix_text(methods[i],lang)
  if(lang!='en'&&(label==short[i]||long==methods[i]))missing<-c(missing,paste(lang,short[i]))
  html<-as.character(paired_results_ui(results[[i]]));doc<-xml2::read_html(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"));stopifnot(label%in%cells,'Review - Normality'%in%cells)
  tab<-paired_appendix_table(data.frame(Method=methods[i]));stopifnot(tab[[1]][1]==long)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  if(i>1)stopifnot(grepl('사용자 <&> %s',paste(main[[i]],collapse=''),fixed=TRUE))
  parts[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
}
if(length(missing))stop(paste('Untranslated categorical names:',paste(missing,collapse='; ')))
out<-'tmp/paired-categorical-names-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual McNemar/Stuart-Maxwell/Bowker in eight languages; user categories, English main tables and source preserved\n')
