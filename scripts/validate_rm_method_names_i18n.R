Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(918);base<-rnorm(100)
d<-data.frame(Review=base+rnorm(100),Normality=base+5*rnorm(100),third=base+.2*rnorm(100))
info<-data.frame(name=names(d),measurement='continuous')
results<-lapply(c(FALSE,TRUE),function(check)prepare_paired_rm_results(d,variables=names(d),variable_info=info,options=list(assumption_check=check)))
methods<-c('Standard RM ANOVA',"RM ANOVA + Wilks' lambda");short<-c('RM ANOVA','RM ANOVA + Wilks')
stopifnot(identical(vapply(results,function(r)r$table$Method[1],character(1)),methods))
before<-serialize(results,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);parts<-list();main<-list()
 for(i in seq_along(results)) {
  label<-paired_appendix_text(short[i],lang);long<-paired_appendix_text(methods[i],lang)
  if(lang!='en'&&(label==short[i]||long==methods[i]))missing<-c(missing,paste(lang,short[i]))
  html<-as.character(paired_rm_results_ui(results[[i]]));doc<-xml2::read_html(html)
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"));stopifnot(label%in%cells)
  tab<-paired_appendix_table(data.frame(Method=methods[i]));stopifnot(tab[[1]][1]==long)
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
  parts[[i]]<-html
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(parts),collapse='\n')
}
if(length(missing))stop(paste('Untranslated RM methods:',paste(missing,collapse='; ')))
out<-'tmp/rm-method-names-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual standard RM ANOVA and Wilks branches in eight languages; English main tables/source preserved\n')
