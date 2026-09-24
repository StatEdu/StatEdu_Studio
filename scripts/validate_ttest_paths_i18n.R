Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(931);d<-data.frame(y=rnorm(90),g2=rep(c('Normality met','Yes'),45),g3=rep(c('A','B','C'),30))
info<-data.frame(name=names(d),measurement=c('continuous','binary','category'),var_label=c('Homogeneity','Normality met','사용자 집단'))
methods<-c('skew_kurtosis','sw','ks','forced')
fits<-setNames(lapply(methods,function(m)prepare_ttest_anova_results(d,'y',c('g2','g3'),info,options=list(normality_enabled=TRUE,normality_method=m,force_nonparametric=m=='forced',effect_size=TRUE))),methods)
out<-'tmp/ttest-paths-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-list();export<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in methods){
  html<-as.character(ttest_anova_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  stopifnot(length(main)>0L,all(xml2::xml_attr(main,'data-result-table-language')=='en'))
  cells<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  if(language=='en')baseline[[name]]<-cells
  stopifnot(identical(cells,baseline[[name]]))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
  stopifnot(length(appendix)==2L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  for(t in appendix){
   headers<-xml2::xml_text(xml2::xml_find_all(t,'.//th'))
   labels<-xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[1]'))
   stopifnot('Homogeneity' %in% headers,'Normality met' %in% labels,'사용자 집단' %in% labels)
  }
  text<-xml2::xml_text(appendix)
  if(name %in% c('sw','ks'))stopifnot(any(grepl(statedu_t('analysis.ui.normality_met',language),text,fixed=TRUE)),!any(grepl('정규성 만족',text,fixed=TRUE)))
  if(name=='forced')for(key in c('selected_by_analysis_menu','mann_whitney_u_test_wilcoxon_rank_sum_test'))stopifnot(any(grepl(statedu_t(paste0('analysis.ui.',key),language),text,fixed=TRUE)))
  if(name=='skew_kurtosis'){
   raw<-fits[[name]]$assumption_review[[3]][1]
   detail<-regmatches(raw,regexec('^skew=([^,]+), kurtosis=([^,]+), cutoff=(.+)$',raw))[[1]]
   stopifnot(length(detail)==4L)
   expected<-sprintf(statedu_t('analysis.ttest.skew_kurtosis_detail',language),detail[2],detail[3],detail[4])
   stopifnot(any(grepl(expected,text,fixed=TRUE)))
  }
  if(language=='ja')export[[name]]<-html
 }
 cat('PASS:',language,'four actual normality/nonparametric paths; English main tables; diagnostic translations; colliding user labels\n')
}
# One combined entry ensures export checks cover all four paths.
saveRDS(list(list(id='ttest-paths',title='t-test / ANOVA',html=paste(unlist(export),collapse='\n'))),file.path(out,'entries.rds'))
