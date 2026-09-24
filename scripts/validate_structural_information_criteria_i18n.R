Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
out<-'tmp/structural-information-criteria-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
comparison<-cfa;comparison$baseline_fit<-cfa$fit;comparison$modified_from_baseline<-TRUE;comparison$comparison_label<-'Review 사용자 <&>'
different<-comparison;different$fit<-lavaan::cfa('Normality =~ x1+x2+x3\n사용자요인 =~ y1+y2+y3',data=d[-1,])
fixtures<-list(single=cfa,comparable=comparison,different=different)
entries<-list()
for(name in names(fixtures))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 bundle<-fixtures[[name]];raw<-structural_canvas_information_criteria(bundle)
 html<-as.character(structural_canvas_information_criteria_result_ui(bundle,language));doc<-read(html)
 rows<-xml2::xml_find_all(doc,'//tbody/tr');stopifnot(length(rows)==nrow(raw))
 numeric<-c('N','LogLik','Free parameters','AIC','BIC','Adjusted BIC','Delta AIC','Delta BIC','Delta Adjusted BIC')
 positions<-c(2,5,6,7,8,9,11,12,13)
 for(i in seq_along(rows)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(rows[[i]],'./td')))
  expected<-vapply(numeric,function(column)format_decimal3(raw[[column]][i]),character(1))
  stopifnot(identical(cells[positions],unname(expected)))
  stopifnot(cells[10]==structural_canvas_reporting_text(raw$`Comparison status`[i],language))
 }
 if(name!='single')stopifnot('Review 사용자 <&>' %in% xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 if(language!='en'){
  stopifnot(!grepl('Guide for Table 2:',xml2::xml_text(doc),fixed=TRUE))
  stopifnot(!grepl('Lower AIC, BIC',xml2::xml_text(doc),fixed=TRUE))
  stopifnot(!any(c('Admissible','Params','Adj BIC') %in% xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
  stopifnot(!any(raw$`Comparison status` %in% xml2::xml_text(xml2::xml_find_all(doc,'//td'))))
 }
 writeLines(html,file.path(out,paste0(language,'-',name,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[name]]<-list(id=name,title=name,html=html)
 cat('PASS:',language,name,'information criteria; all numeric cells and custom model names preserved\n')
}
for(language in c('ko','ja','zh','es','fr','de','vi'))stopifnot(structural_canvas_reporting_text('Not comparable; inadmissible model; delta suppressed',language)!='Not comparable; inadmissible model; delta suppressed')
saveRDS(unname(entries),file.path(out,'entries.rds'))
