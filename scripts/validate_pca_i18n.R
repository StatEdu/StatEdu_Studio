Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(938);f<-rnorm(100);d<-as.data.frame(setNames(lapply(1:5,function(i)f+rnorm(100)),paste0('x',1:5)))
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','Yes','사용자 변수','Status','사용자 항목'))
fits<-list(correlation=prepare_pca_results(d,names(d),info,options=list(criterion='fixed',n_components=2,rotation='varimax')),covariance=prepare_pca_results(d,names(d),info,options=list(matrix_type='covariance',criterion='fixed',n_components=2,rotation='none')))
out<-'tmp/pca-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();exports<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in names(fits)){
  html<-as.character(pca_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  stopifnot(length(main)>=3L,all(xml2::xml_attr(main,'data-result-table-language')=='en'))
  if(language=='en')baseline[[name]]<-content
  stopifnot(identical(content,baseline[[name]]),all(info$var_label %in% unlist(content)))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)==2L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//th|.//td'))
  for(key in c('check','bartlett','component','eigenvalue','variance_percent','cumulative_percent','selected','yes'))stopifnot(statedu_t(paste0('analysis.pca.',key),language) %in% cells)
  expected<-pca_appendix_table(fits[[name]]$eigen_table)
  stopifnot(identical(expected[[2]],fits[[name]]$eigen_table[[2]]),identical(expected[[3]],fits[[name]]$eigen_table[[3]]),identical(expected[[4]],fits[[name]]$eigen_table[[4]]))
  if(language=='ja')exports[[name]]<-html
 }
 cat('PASS:',language,'actual correlation/varimax and covariance PCA; main English; appendix translations, percent units, labels and values\n')
}
saveRDS(list(list(id='pca',title='PCA',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))
