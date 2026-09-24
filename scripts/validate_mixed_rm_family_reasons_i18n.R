Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(919)
datasets<-list(ordinal=matrix(sample(1:5,120,replace=TRUE),40,3),count=matrix(rpois(120,4),40,3),gamma=matrix(exp(rnorm(120,0,1.5)),40,3))
phrases<-c('At least one repeated-measures variable is ordinal.','The stacked repeated outcome is non-negative integer-like count data.','The stacked repeated outcome is positive and strongly right-skewed.')
results<-lapply(seq_along(datasets),function(i){
 d<-data.frame(group=rep(c('Review','Normality'),each=20),datasets[[i]]);names(d)[-1]<-c('pre','post','last')
 info<-data.frame(name=names(d),measurement=c('category',rep(if(i==1)'ordered'else'continuous',3)))
 r<-prepare_mixed_rm_anova_results(d,group_variable='group',repeated_variables=c('pre','post','last'),variable_info=info,options=list(assumption_check=TRUE,posthoc=FALSE))
 stopifnot(sum(grepl(phrases[i],r$recommendation$Reason,fixed=TRUE))==2)
 r
})
before<-serialize(results,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 for(i in seq_along(results)) {
  expected<-if(lang=='ko')mixed_rm_appendix_korean_text(phrases[i])else result_appendix_ui_text(phrases[i],lang)
  if(lang!='en'&&expected==phrases[i])failures<-c(failures,paste(lang,i,'catalog'))
  localized<-mixed_rm_appendix_table(results[[i]]$recommendation)
  reason<-localized[[which(names(results[[i]]$recommendation)=='Reason')]]
  if(sum(grepl(expected,reason,fixed=TRUE))!=2)failures<-c(failures,paste(lang,i,'combined reason'))
  panels[[i]]<-as.character(mixed_rm_anova_results_ui(results[[i]]));doc<-xml2::read_html(panels[[i]])
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
  if(sum(grepl(expected,cells,fixed=TRUE))<2)failures<-c(failures,paste(lang,i,'render'))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 probe<-mixed_rm_appendix_table(data.frame(Variable=phrases,Group=c('Review','Normality','Unknown'),Reason=phrases))
 stopifnot(identical(probe[[1]],phrases),identical(probe[[2]],c('Review','Normality','Unknown')))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
if(length(failures))stop(paste('Family reason failures:',paste(failures,collapse=', ')))
out<-'tmp/mixed-rm-family-reasons-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three actual mixed RM family reasons, standalone and combined, across eight languages; main/source/user labels preserved\n')
