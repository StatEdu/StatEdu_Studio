Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
info<-data.frame(name=c('y','g'),measurement=c('continuous','category'),var_label=c('사용자 결과','사용자 집단'))
datasets<-list(minimum_cases=data.frame(y=1:4,g='사용자 A'),constant_outcome=data.frame(y=rep(1,8),g=rep(c('A','B'),each=4)),
 small_groups=data.frame(y=1:5,g=c('Normality met 50%','B','B','B','B')),
 zero_sd=data.frame(y=c(rep(1,5),2:6),g=rep(c('Normality met 50%','B'),each=5)),
 automatic=data.frame(y=rep(c(rep(0,28),1,100),3),g=rep(c('A','B','C'),each=30)))
fits<-lapply(datasets,function(d)prepare_ttest_anova_results(d,'y','g',info,options=list(normality_enabled=TRUE)))
stopifnot(length(fits$automatic$results)>0L,any(grepl('K-W',unlist(fits$automatic$overview),fixed=TRUE)))
out<-'tmp/ttest-guards-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-list();export<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in names(fits)){
  html<-as.character(ttest_anova_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  cells<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  if(language=='en')baseline[[name]]<-cells
  stopifnot(identical(cells,baseline[[name]]))
  content<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']"))
  if(name!='automatic'){
   expected<-statedu_t(paste0('analysis.ttest.',name),language)
   if(name %in% c('small_groups','zero_sd'))expected<-sprintf(expected,'Normality met 50%')
   if(!any(grepl(expected,content,fixed=TRUE)))stop(language,'/',name,' expected: ',expected,' actual: ',paste(content,collapse=' | '))
  }else stopifnot(any(grepl(statedu_t('analysis.ui.normality_not_met',language),content,fixed=TRUE)))
  if(name %in% c('minimum_cases','constant_outcome','small_groups'))stopifnot(any(grepl(statedu_t('analysis.ttest.no_result',language),content,fixed=TRUE)))
  if(language=='ja')export[[name]]<-html
 }
 cat('PASS:',language,'four actual guard/warning paths and automatic nonparametric selection; labels and main content preserved\n')
}
saveRDS(list(list(id='ttest-guards',title='t-test / ANOVA diagnostics',html=paste(unlist(export),collapse='\n'))),file.path(out,'entries.rds'))
