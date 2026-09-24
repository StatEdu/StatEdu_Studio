Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-influence-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
raw<-survival_cox_influence_table(result);stopifnot(nrow(raw)>0)
custom<-result
custom$influence_table<-result$influence_table[rep(1,2),,drop=FALSE]
custom$influence_table$Term<-c('Review','사용자 <&> %s')
custom$influence_table[['Review signal']]<-c(TRUE,FALSE)
note<-'Standardized DFBETAS are compared with the heuristic 2/sqrt(N) screening threshold. Signals identify observations for sensitivity review; observations are not deleted automatically.'
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 rows<-survival_cox_influence_table(if(kind=='actual')result else custom)
 panel<-tagList(tags$h3(survival_appendix_title('Influence review',language)),survival_simple_table(rows,table_language=language),survival_appendix_note(language,note,'표준화 DFBETAS는 경험적 2/sqrt(N) 선별 임계값과 비교합니다. 신호는 민감도 검토 대상을 표시하며 관측치를 자동으로 삭제하지 않습니다.'))
 html<-as.character(panel);doc<-xml2::read_html(html,encoding='UTF-8')
 headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[not(position()=5)]')))
 signals<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[5]')))
 if(language=='en')baseline<-cells else{
  stopifnot(identical(cells,baseline),!any(names(rows)[-1]%in%headers),!any(c('Review','No strong signal')%in%signals),
   !grepl('Influence review',xml2::xml_text(doc),fixed=TRUE),!grepl(substr(note,1,50),xml2::xml_text(doc),fixed=TRUE))
 }
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]'))),rows$Term))
 main<-as.character(survival_cox_result_html_table(result,language))
 main_cells<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(main,encoding='UTF-8'),'//th|//td'))
 if(language=='en')main_baseline<-main_cells else stopifnot(identical(main_cells,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'influence headers, signals, note, values, labels and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
