Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(945)
d<-data.frame(x=rnorm(150),y=rnorm(150),binary=rbinom(150,1,.45),count=rpois(150,3),ord=ordered(rep(1:3,50)),cat=factor(rep(1:3,50)))
info<-data.frame(name=names(d),measurement=c('continuous','continuous','binary','continuous','ordered','category'),var_label=c('Model-based','사용자 결과','사용자 이분형','사용자 계수','사용자 순서형','사용자 명목형'))
fits<-list()
for(y in c('y','binary','count')){
 fit<-prepare_generalized_analysis_result(d,y,'x',family=switch(y,y='gaussian',binary='binomial',count='count'),variable_info=info)
 fits[[paste0('glm-',y)]]<-local({args<-list(fit,variable_table=info);list(type='scope_renderer',render=function()do.call(generalized_results_panel,args))})
}
for(y in c('binary','ord','cat')){
 fit<-prepare_logistic_analysis_results(d,y,'x',variable_info=info)
 fits[[paste0('logistic-',y)]]<-local({args<-list(fit,variable_table=info,show_b=TRUE,show_se=TRUE,split_ci=TRUE);list(type='scope_renderer',render=function()do.call(logistic_results_panel,args))})
}
main<-function(doc)xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td"))
for(name in names(fits)){
 options(statedu.app_language='ko');result<-fits[[name]]
 doc<-xml2::read_html(as.character(result$render()),encoding='UTF-8')
 cell<-xml2::xml_find_first(doc,"//table[@data-result-table-role='main']//td");xml2::xml_text(cell)<-'사용자 편집 50%'
 original<-paste(vapply(xml2::xml_children(xml2::xml_find_first(doc,'//body')),as.character,character(1)),collapse='\n');baseline<-main(doc)
 stopifnot(length(baseline)>0)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  translated<-xml2::read_html(analysis_scope_localize_one_group_snapshot(original,result,language),encoding='UTF-8')
  appendix<-xml2::xml_find_all(translated,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)>0,all(xml2::xml_attr(appendix,'data-result-table-language')==language),identical(main(translated),baseline))
 }
 cat('PASS:',name,'eight languages; appendix language; edited main cells preserved\n')
}
