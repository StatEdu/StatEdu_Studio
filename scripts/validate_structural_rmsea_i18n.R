Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
rmsea_out<-'tmp/structural-rmsea-i18n';dir.create(rmsea_out,recursive=TRUE,showWarnings=FALSE)
robust<-cfa;robust$fit<-lavaan::cfa('Normality =~ x1+x2+x3\n사용자요인 =~ y1+y2+y3',data=d,estimator='MLR');robust$estimator<-'MLR'
comparison<-cfa;comparison$baseline_fit<-cfa$fit;comparison$modified_from_baseline<-TRUE;comparison$comparison_label<-'Review 사용자 <&>'
fixtures<-list(ML=cfa,MLR=robust,comparison=comparison)
entries<-list()
for(name in names(fixtures))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 bundle<-fixtures[[name]];raw<-structural_canvas_rmsea_hypothesis_tests(bundle)
 html<-as.character(structural_canvas_rmsea_tests_result_ui(bundle,language));doc<-read(html)
 rows<-xml2::xml_find_all(doc,'//tbody/tr');stopifnot(length(rows)==nrow(raw))
 for(i in seq_along(rows)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(rows[[i]],'./td')))
  expected<-c(format_decimal3(raw$RMSEA[i]),format_decimal3(raw$`Close-fit H0`[i]),format_p(raw$`Close-fit p`[i]),format_decimal3(raw$`Not-close H0`[i]),format_p(raw$`Not-close p`[i]),raw$Source[i])
  stopifnot(identical(cells[2:7],unname(expected)))
 }
 if(name=='comparison')stopifnot('Review 사용자 <&>' %in% xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 if(language!='en'){
  stopifnot(!grepl('Guide for Table 2:',xml2::xml_text(doc),fixed=TRUE))
  stopifnot(!any(c('Close-fit H0','Close-fit p','Not-close H0','Not-close p') %in% xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
 }
 writeLines(html,file.path(rmsea_out,paste0(language,'-',name,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[name]]<-list(id=name,title=name,html=html)
 cat('PASS:',language,name,'RMSEA guidance; p/threshold/source values and custom model names preserved\n')
}
saveRDS(unname(entries),file.path(rmsea_out,'entries.rds'))
