Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(932);d<-data.frame(x=rnorm(90),y=rnorm(90),z=rep(c(rep(0,28),1,100),3),constant=1,sparse=c(1,2,rep(NA,88)))
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','사용자 결과','Yes','사용자 상수','사용자 희소'))
fit<-prepare_correlation_results(d,names(d),info,options=list(continuous_method='auto',normality=TRUE,reason=TRUE,p_ci=TRUE))
stopifnot(all(c('Pearson','Spearman') %in% fit$pairwise_table$Method),nrow(fit$omitted_table)==2L)
out<-'tmp/correlation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-NULL
keys<-c('skewness','kurtosis','omitted_variables','valid_n','unique_values','omitted_because_fewer_than_three_valid_values_were_available','omitted_because_fewer_than_two_unique_values_were_available','values_are_95_cis_and_p_values','pearson_correlation','spearman_correlation')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-as.character(correlation_results_ui(fit));doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 stopifnot(length(main)==1L,xml2::xml_attr(main,'data-result-table-language')=='en')
 content<-xml2::xml_text(xml2::xml_find_all(main,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))
 if(language=='en')baseline<-content
 stopifnot(identical(content,baseline))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(appendix)==4L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 for(t in appendix[1:2]){
  stopifnot(all(c('Normality','사용자 결과','Yes') %in% xml2::xml_text(xml2::xml_find_all(t,'.//th'))))
  stopifnot(all(c('Normality','사용자 결과','Yes') %in% xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[1]'))))
 }
 text<-xml2::xml_text(doc)
 for(key in keys){
  expected<-statedu_t(paste0('analysis.ui.',key),language)
  # Shared note rendering uses semicolons instead of terminal periods.
  expected<-sub('[.]$','',expected)
  if(!grepl(expected,text,fixed=TRUE))stop(language,' missing ',key,': ',expected)
 }
 if(language=='ja')saveRDS(list(list(id='correlation',title='Correlation',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual Pearson/Spearman; English main matrix; four appendices; variable labels and ten translated phrases\n')
}
