Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(925);n<-40
d<-data.frame(group=rep(c('Review','Normality'),each=n/2),pre=rnorm(n),post=rnorm(n,1),last=rnorm(n,2))
info<-data.frame(name=names(d),measurement=c('category',rep('continuous',3)))
base<-prepare_mixed_rm_anova_results(d,group_variable='group',repeated_variables=c('pre','post','last'),variable_info=info,options=list(assumption_check=TRUE,posthoc=FALSE))
aliases<-c('Shapiro-Wilk by group','Lilliefors (K-S)','Kolmogorov-Smirnov (Lilliefors)')
phrase<-'No matching time interaction was returned by the model.'
# Synthetic compatibility fixtures, not recalculated tests for these methods.
results<-lapply(aliases,function(alias){r<-base;attr(r$normality,'normality_method')<-alias;r})
# Exercise the real fallback formatter with an input lacking a time interaction.
exception<-base
exception$anova<-base$anova[base$anova$Effect=='Group',,drop=FALSE]
stopifnot(nrow(exception$anova)>0)
exception$recommendation<-mixed_rm_recommendation_table(exception$anova,exception$assumption,exception$normality,'group')
stopifnot(phrase%in%exception$recommendation$Reason)
results[[4]]<-exception
out<-'tmp/mixed-rm-compatibility-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(results,file.path(out,'synthetic-fixtures.rds'));results<-readRDS(file.path(out,'synthetic-fixtures.rds'))
before<-serialize(results,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 labels<-vapply(aliases,mixed_rm_appendix_normality_method,character(1),language=lang)
 fallback<-if(lang=='ko')mixed_rm_appendix_korean_text(phrase)else result_appendix_ui_text(phrase,lang)
 if(lang!='en'&&(any(labels==aliases)||fallback==phrase))failures<-c(failures,paste(lang,'catalog'))
 for(i in seq_along(results)) {
  panels[[i]]<-as.character(mixed_rm_anova_results_ui(results[[i]]));doc<-xml2::read_html(panels[[i]])
  if(i<=3)stopifnot(grepl(labels[i],xml2::xml_text(doc),fixed=TRUE))else stopifnot(fallback%in%xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
if(length(failures))stop(paste('Compatibility failures:',paste(failures,collapse=', ')))
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three synthetic restored normality aliases and missing-interaction fallback in eight languages; main/source preserved\n')
