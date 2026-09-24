Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
out<-'tmp/structural-additional-fit-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
robust<-cfa;robust$fit<-lavaan::cfa('Normality =~ x1+x2+x3\n사용자요인 =~ y1+y2+y3',data=d,estimator='MLR');robust$estimator<-'MLR'
entries<-list()
for(name in c('ML','MLR')){
 bundle<-if(name=='ML')cfa else robust
 selection<-structural_canvas_common_fit_measures(list(bundle$fit),bundle$estimator,.90)
 raw<-structural_canvas_additional_fit_indices_table(list(bundle$fit),'Review',selection)
 tables<-structural_canvas_additional_fit_indices_wide_tables(raw)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_canvas_additional_fit_indices_ui(raw,language=language));doc<-read(html)
  panels<-xml2::xml_find_all(doc,"//div[contains(@class,'structural-additional-fit-family')]")
  stopifnot(length(panels)==length(tables))
  for(i in seq_along(tables)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(panels[[i]],'.//tbody/tr/td')))
   stopifnot(identical(cells,unname(as.character(tables[[i]][1,]))))
   if(names(tables)[i] %in% c('incremental','rmsea','residual'))stopifnot(grepl('landscape-table-panel',xml2::xml_attr(panels[[i]],'class')))
  }
  stopifnot(all(xml2::xml_attr(xml2::xml_find_all(doc,"//*[@data-result-table-role='appendix']"),'data-result-table-language')==language))
  if(language!='en')stopifnot(!any(vapply(names(tables),structural_canvas_additional_fit_family_label,character(1)) %in% xml2::xml_text(xml2::xml_find_all(doc,'//h6'))))
  if(language!='en')stopifnot(!any(grepl('SCALED|ROBUST|SCALING FACTOR|NOTCLOSE|no mean|CI level',xml2::xml_text(xml2::xml_find_all(doc,'//th')))))
  writeLines(html,file.path(out,paste0(language,'-',name,'.html')),useBytes=TRUE)
  if(language=='ja'){
   captured<-bundle;captured$baseline_fit<-bundle$fit;captured$modified_from_baseline<-TRUE;captured$comparison_label<-'Review'
   full_html<-as.character(structural_canvas_fit_guidance_result_ui(captured,language))
   stopifnot(!grepl('Guide for Table 2:',full_html,fixed=TRUE))
   entries[[name]]<-list(id=name,title=name,html=full_html)
  }
  cat('PASS:',language,name,'family labels, literal Review model name, all cells and landscape panels\n')
 }
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
