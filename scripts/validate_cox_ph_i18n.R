Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-ph-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
custom<-result;custom$ph_table<-result$ph_table[rep(1,2),,drop=FALSE]
custom$ph_table$Term<-c('Review','사용자 <&> %s');custom$ph_table$p<-c(.001,.7)
notes<-c(
 'Covariate-specific Schoenfeld-residual tests are supplementary diagnostics for possible time-varying effects. The main table reports only the overall likelihood-ratio test and the PH GLOBAL test.',
 'Smoothed scaled Schoenfeld-residual plots support assessment of time-varying coefficients. Formal p-values and plots should be interpreted together; neither is an automatic pass/fail rule.',
 'The loess smooth of Martingale residuals against each continuous covariate is a functional-form screening plot. Systematic curvature suggests considering a prespecified transformation or spline and comparing substantive conclusions.')
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-if(kind=='actual')result else custom
 tables<-list(survival_ph_table(r,language),survival_ph_review_table(r,language))
 panels<-lapply(tables,function(t)survival_simple_table(t,table_language=language))
 doc<-xml2::read_html(paste(vapply(panels,as.character,character(1)),collapse=''),encoding='UTF-8')
 ts<-xml2::xml_find_all(doc,'//table')
 for(t in ts)stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(t,'.//tbody/tr/td[1]'))),r$ph_table$Term))
 values<-xml2::xml_text(xml2::xml_find_all(ts[[1]],'.//tbody/tr/td[position()>1]'))
 if(language=='en')baseline<-values else stopifnot(identical(values,baseline))
 if(language!='en')stopifnot(!any(trimws(xml2::xml_text(xml2::xml_find_all(ts[[2]],'.//tbody/tr/td[2]')))%in%c('Review possible time-varying effect','No strong signal in this test')))
 full<-xml2::read_html(as.character(survival_cox_results_panel(r,language)),encoding='UTF-8')
 expected_title<-statedu_localized_text(language,'Detailed proportional-hazards diagnostics','비례위험 가정 상세 진단')
 stopifnot(expected_title%in%xml2::xml_text(xml2::xml_find_all(full,'//h4')))
 if(language!='en')for(n in notes)stopifnot(!grepl(substr(n,1,60),xml2::xml_text(full),fixed=TRUE))
 main<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(as.character(survival_cox_result_html_table(result,language)),encoding='UTF-8'),'//th|//td'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja'){
  localized_notes<-lapply(notes,function(n)survival_table_note(statedu_localized_text(language,n)))
  html<-as.character(tagList(tags$h4(expected_title),panels,localized_notes))
  entries[[kind]]<-list(id=kind,title=kind,html=html)
 }
 cat('PASS:',kind,language,'PH table, notes, labels, values and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
