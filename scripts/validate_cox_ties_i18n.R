Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-ties-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$time<-pmax(1,round(d$time/5)*5)
note<-'The ties method is prespecified and reported with the observed extent of tied failures. It is not selected by searching for the smallest p-value.'
for(method in c('efron','breslow','exact')){
 result<-prepare_cox_analysis_result(d,time='time',event='status',covariates=c('age','sex'),event_value='1',ties_method=method)
 raw<-survival_cox_ties_summary_table(result);stopifnot(nrow(raw)==1L,raw[['Tied event times']]>0)
 before<-serialize(list(result,raw),NULL)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  options(statedu.app_language=language)
  panel<-tagList(tags$h4(survival_appendix_title('Tied-event summary',language)),survival_simple_table(raw,table_language=language),survival_appendix_note(language,note,'동률 처리 방법은 사전에 지정하며 관찰된 동률 사건 규모와 함께 보고합니다. 가장 작은 p값을 찾는 방식으로 선택하지 않습니다.'))
  html<-as.character(panel);doc<-xml2::read_html(html,encoding='UTF-8');headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  if(language!='en')stopifnot(!any(names(raw)[3:7]%in%headers),!grepl('Tied-event summary',xml2::xml_text(doc),fixed=TRUE),!grepl(note,xml2::xml_text(doc),fixed=TRUE))
  cells<-xml2::xml_text(xml2::xml_find_all(doc,'//td'))
  # Method is an application label in an appendix; counts and precision are invariant.
  expected_method<-if(method=='exact'&&!language%in%c('en','ko'))statedu_localized_text(language,'Exact')else raw$Method[[1]]
  stopifnot(identical(cells[[1]],expected_method))
  if(language=='en')baseline<-cells[-1] else stopifnot(identical(cells[-1],baseline))
  stopifnot(identical(before,serialize(list(result,raw),NULL)))
  main<-as.character(survival_cox_result_html_table(result,language));main_cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
  if(language=='en')main_baseline<-main_cells else stopifnot(identical(main_cells,main_baseline))
  if(language=='ja')entries[[method]]<-list(id=method,title=method,html=html)
  cat('PASS:',method,language,'actual tied Cox model, localized method/headers/title/note, unchanged counts/source and English main table\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
