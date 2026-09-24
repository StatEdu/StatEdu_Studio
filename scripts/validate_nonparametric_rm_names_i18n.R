Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=c(1,2,3,4,5,6),Normality=c(2,4,3,5,7,6),third=c(3,2,5,6,8,7))
b<-data.frame(Review=c('no','yes','no','yes','no','yes'),Normality=c('yes','yes','no','no','yes','yes'),third=c('yes','no','yes','yes','no','yes'))
results<-list(prepare_nonparametric_paired_rm_results(d,list(names(d)),data.frame(name=names(d),measurement='continuous')),prepare_nonparametric_paired_rm_results(b,list(names(b)),data.frame(name=names(b),measurement='binary')))
methods<-c('Friedman test',"Cochran's Q test");short<-c('Friedman','Cochran Q')
stopifnot(identical(vapply(results,function(r)r$table$Method[1],character(1)),methods))
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(i in seq_along(results)) {
  label<-paired_appendix_text(short[i],lang);long<-paired_appendix_text(methods[i],lang)
  if(lang!='en'&&(label==short[i]||long==methods[i]))missing<-c(missing,paste(lang,short[i]))
  html<-as.character(nonparametric_paired_results_ui(results[[i]]));doc<-xml2::read_html(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"));stopifnot(label%in%cells)
  tab<-paired_appendix_table(data.frame(Method=methods[i]));stopifnot(tab[[1]][1]==long)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  if(i==2)stopifnot(all(c('no','yes')%in%xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th"))))
  parts[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
}
if(length(missing))stop(paste('Untranslated nonparametric RM names:',paste(missing,collapse='; ')))
out<-'tmp/nonparametric-rm-names-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual Friedman/Cochran Q names in eight languages; category labels, English main tables/source preserved\n')
