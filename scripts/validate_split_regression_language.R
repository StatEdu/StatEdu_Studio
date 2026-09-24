Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(944);d<-data.frame(x=rnorm(80),z=rnorm(80));d$y<-2*d$x+.5*d$z+rnorm(80)
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','사용자 변수','사용자 결과'))
prepared<-prepare_hierarchical_analysis_results(d,'y','x','z',variable_info=info,auto_method=FALSE)
stopifnot(length(prepared$jobs)==0,length(prepared$results)==2)
args<-list(results=prepared$results,variable_table=info,show_vif=TRUE,show_f2=TRUE)
result<-list(type='scope_regression',render=function()do.call(hierarchical_results_panel,args))
options(statedu.app_language='ko');original<-as.character(result$render())
doc<-xml2::read_html(original,encoding='UTF-8');cell<-xml2::xml_find_first(doc,"//table[@data-result-table-role='main']//td");xml2::xml_text(cell)<-'사용자 편집 50%'
original<-paste(vapply(xml2::xml_children(xml2::xml_find_first(doc,'//body')),as.character,character(1)),collapse='\n')
main<-function(d)xml2::xml_text(xml2::xml_find_all(d,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td"))
baseline<-main(doc)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 translated<-xml2::read_html(analysis_scope_localize_one_group_snapshot(original,result,language),encoding='UTF-8')
 appendix<-xml2::xml_find_all(translated,"//table[@data-result-table-role='appendix']")
 stopifnot(length(appendix)>0,all(xml2::xml_attr(appendix,'data-result-table-language')==language),identical(main(translated),baseline))
 cat('PASS:',language,'actual two-block regression; appendix language; edited main cells preserved\n')
}
