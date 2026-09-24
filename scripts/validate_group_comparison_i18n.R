Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(930)
d<-data.frame(y=rnorm(90),g2=rep(c('Yes','Normality'),45),g3=rep(c('A','B','C'),30))
info<-data.frame(name=names(d),measurement=c('continuous','binary','category'),var_label=c('사용자 결과','사용자 집단','Status'))
ct<-prepare_crosstab_results(d,'g2','g3',info,options=list(row_percent=TRUE))
tt<-prepare_ttest_anova_results(d,'y',c('g2','g3'),info,options=list(effect_size=TRUE,normality_enabled=FALSE))
out<-'tmp/group-comparison-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-list();entries<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 for(name in c('crosstabs','ttest_anova')){
  html<-as.character(if(name=='crosstabs')crosstab_results_ui(ct)else ttest_anova_results_ui(tt))
  doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  stopifnot(length(main)>0L,all(xml2::xml_attr(main,'data-result-table-language')=='en'))
  content<-lapply(main,function(t)list(cells=xml2::xml_text(xml2::xml_find_all(t,'.//th|.//td')),
    title=xml2::xml_text(xml2::xml_find_all(t,'preceding::h3[1]')),
    notes=xml2::xml_text(xml2::xml_find_all(t,"ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]"))))
  if(language=='en')baseline[[name]]<-content
  stopifnot(identical(content,baseline[[name]]))
  if(name=='ttest_anova'){
   appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
   stopifnot(length(appendix)==2L,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
   values<-xml2::xml_text(xml2::xml_find_all(appendix,'.//th|.//td'))
   stopifnot(all(c('사용자 결과','사용자 집단','Status') %in% values))
   for(key in c('homogeneity','normality_not_checked'))stopifnot(statedu_t(paste0('analysis.ui.',key),language) %in% values)
  }
  if(language=='ja')entries[[length(entries)+1L]]<-list(id=name,title=name,html=html)
 }
 cat('PASS:',language,'actual cross-tabulation and t-test/ANOVA main content; diagnostic translations and user labels\n')
}
saveRDS(rev(entries),file.path(out,'entries.rds'))
