Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_one_group_i18n.R',encoding='UTF-8')
options(statedu.app_language='ko')
original<-xml2::read_html(as.character(one_group_rm_anova_results_ui(fit)),encoding='UTF-8')
cell<-xml2::xml_find_first(original,"//table[@data-result-table-role='main']//td")
xml2::xml_text(cell)<-'사용자 편집 50%'
html<-paste(vapply(xml2::xml_children(xml2::xml_find_first(original,'//body')),as.character,character(1)),collapse='\n')
main<-function(doc)xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td"))
baseline<-main(original)
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')){
 rendered<-analysis_scope_localize_one_group_snapshot(html,fit,lang);doc<-xml2::read_html(rendered,encoding='UTF-8')
 tables<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(tables)>0,all(xml2::xml_attr(tables,'data-result-table-language')==lang),identical(main(doc),baseline))
 stopifnot(grepl(statedu_t('analysis.one_group.within_factors',lang),xml2::xml_text(doc),fixed=TRUE))
 cat('PASS:',lang,'split appendix language; original edited main cells preserved\n')
}
stopifnot(identical(analysis_scope_localize_one_group_snapshot(html,list(type='other'), 'ja'),html))
