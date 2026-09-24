Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(919)
datasets<-list(ordinal=matrix(sample(1:5,120,replace=TRUE),40,3),count=matrix(rpois(120,.8),40,3),gamma=matrix(exp(rnorm(120,0,1.5)),40,3))
phrases<-c('Consider ordinal mixed model as the main or sensitivity analysis.','Consider count GLMM as the main or sensitivity analysis.','Consider Gamma GLMM as the main or sensitivity analysis.')
results<-lapply(seq_along(datasets),function(i){
 d<-data.frame(group=rep(c('Review','Normality'),each=20),datasets[[i]]);names(d)[-1]<-c('pre','post','last')
 info<-data.frame(name=names(d),measurement=c('category',rep(if(i==1)'ordered'else'continuous',3)))
 r<-prepare_mixed_rm_anova_results(d,group_variable='group',repeated_variables=c('pre','post','last'),variable_info=info,options=list(assumption_check=TRUE,posthoc=FALSE))
 stopifnot(mixed_rm_normality_issue(r$normality),phrases[i]%in%r$recommendation$Recommendation)
 r
})
before<-serialize(results,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);panels<-list();main<-list()
 for(i in seq_along(results)) {
  expected<-if(lang=='ko')mixed_rm_appendix_korean_text(phrases[i])else result_appendix_ui_text(phrases[i],lang)
  if(lang!='en'&&expected==phrases[i])failures<-c(failures,paste(lang,i,'catalog'))
  localized<-mixed_rm_appendix_table(results[[i]]$recommendation)
  reason<-localized[[which(names(results[[i]]$recommendation)=='Recommendation')]]
  if(sum(reason==expected)!=1)failures<-c(failures,paste(lang,i,'recommendation'))
  panels[[i]]<-as.character(mixed_rm_anova_results_ui(results[[i]]));doc<-xml2::read_html(panels[[i]])
  cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
  if(sum(grepl(expected,cells,fixed=TRUE))<1)failures<-c(failures,paste(lang,i,'render'))
  main[[i]]<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main[[i]])>0)
 }
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 probe<-mixed_rm_appendix_table(data.frame(Variable=phrases,Group=c('Review','Normality','Unknown'),Reason=phrases))
 stopifnot(identical(probe[[1]],phrases),identical(probe[[2]],c('Review','Normality','Unknown')))
 captured[[lang]]<-paste(unlist(panels),collapse='\n')
}
if(length(failures))stop(paste('Alternative guidance failures:',paste(failures,collapse=', ')))
out<-'tmp/mixed-rm-alternative-guidance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS three actual mixed RM alternative recommendations, across eight languages; main/source/user labels preserved\n')
