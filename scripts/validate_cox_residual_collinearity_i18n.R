Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-residual-collinearity-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
custom<-result;custom$collinearity_table<-result$collinearity_table[rep(1,4),,drop=FALSE]
custom$collinearity_table[['Design column']]<-c('Review','Normality','사용자 <&> %s','High')
custom$collinearity_table$Review<-c('High','Review','No strong signal','Not estimable')
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-if(kind=='actual')result else custom
 col<-survival_cox_collinearity_table(r);res<-survival_cox_residual_table(r)
 panel<-tagList(tags$h3(survival_appendix_title('Design-matrix collinearity review',language)),survival_simple_table(col,table_language=language),survival_cox_collinearity_note(r,language),
 tags$h3(survival_appendix_title('Residual distribution review',language)),survival_simple_table(res,table_language=language),survival_appendix_note(language,
 'Martingale residuals support functional-form review; deviance residuals support unusual-observation review. These summaries do not establish model adequacy by themselves.',
 'Martingale 잔차는 함수형태 검토를, deviance 잔차는 이상 관측치 검토를 보조합니다. 이 요약만으로 모형 적합성을 확정할 수 없습니다.'))
 html<-as.character(panel);doc<-xml2::read_html(html,encoding='UTF-8');tabs<-xml2::xml_find_all(doc,'//table')
 labels<-trimws(xml2::xml_text(xml2::xml_find_all(tabs[[1]],'.//tbody/tr/td[1]')))
 stopifnot(identical(labels,col[[1]]))
 values<-c(xml2::xml_text(xml2::xml_find_all(tabs[[1]],'.//tbody/tr/td[position()=2 or position()=3]')),xml2::xml_text(xml2::xml_find_all(tabs[[2]],'.//tbody/tr/td[position()>1]')))
 if(language=='en')baseline<-values else{
  stopifnot(identical(values,baseline),!grepl('Design-matrix collinearity review|Residual distribution review|VIF is computed|These summaries do not',xml2::xml_text(doc)))
  stopifnot(!'Design column'%in%xml2::xml_text(xml2::xml_find_all(doc,'//th')))
 }
 stopifnot(grepl(survival_format_number(r$condition_number),xml2::xml_text(doc),fixed=TRUE))
 main<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(as.character(survival_cox_result_html_table(result,language)),encoding='UTF-8'),'//th|//td'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'diagnostics, values, user columns and main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
