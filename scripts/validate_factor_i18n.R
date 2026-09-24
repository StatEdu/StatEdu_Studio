Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(939);f<-rnorm(120);g<-rnorm(120);d<-as.data.frame(setNames(lapply(1:6,function(i)if(i<4)f+rnorm(120,sd=.6)else g+rnorm(120,sd=.6)),paste0('x',1:6)))
info<-data.frame(name=names(d),measurement='continuous',var_label=c('Normality','Yes','사용자 변수','Status','사용자 항목','Factor'))
fits<-list(pa=prepare_factor_analysis_results(d,names(d),info,options=list(method='pa',criterion='fixed',n_factors=2,rotation='oblimin',normality=TRUE)),ml=prepare_factor_analysis_results(d,names(d),info,options=list(method='ml',criterion='fixed',n_factors=2,rotation='varimax',normality=TRUE,normality_method='mardia')))
ordinal<-as.data.frame(lapply(d,function(x)as.integer(cut(x,c(-Inf,-.7,0,.7,Inf)))))
oi<-info;oi$measurement<-'ordered'
fits$polychoric<-prepare_factor_analysis_results(ordinal,names(ordinal),oi,options=list(matrix_type='polychoric',normality=FALSE,method='pa',criterion='fixed',n_factors=2,rotation='none'))
stopifnot(fits$pa$method=='ml',fits$ml$method=='ml',fits$polychoric$method=='pa')
out<-'tmp/factor-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-list();exports<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in names(fits)){
  html<-as.character(factor_analysis_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  stopifnot(length(main)>=3L,all(xml2::xml_attr(main,'data-result-table-language')=='en'))
  if(language=='en')baseline[[name]]<-content
  stopifnot(identical(content,baseline[[name]]),all(info$var_label %in% unlist(content)))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)>=2L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//th|.//td'))
  stopifnot(statedu_t('analysis.ui.factor',language) %in% cells)
  if(name=='ml')for(key in c('mardia_skewness','mardia_kurtosis'))stopifnot(statedu_t(paste0('analysis.ui.',key),language) %in% cells)
  if(name=='pa')stopifnot(all(info$var_label %in% cells))
  if(language=='ja')exports[[name]]<-html
 }
 cat('PASS:',language,'normality-selected ML/oblimin, ML/Mardia/varimax, polychoric PA; main English; appendix roles and factor heading; labels\n')
}
saveRDS(list(list(id='factor',title='Factor analysis',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))
