Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
out<-'tmp/structural-fit-guidance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
comparison<-cfa;comparison$baseline_fit<-cfa$fit;comparison$modified_from_baseline<-TRUE;comparison$comparison_label<-'Review 사용자 <&>'
saturated<-cfa;saturated$fit<-lavaan::cfa('F =~ x1+x2+x3',data=d)
fixtures<-list(single=cfa,comparison=comparison,saturated=saturated);entries<-list()
for(name in names(fixtures))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 bundle<-fixtures[[name]]
 fits<-if(name=='comparison')list(bundle$baseline_fit,bundle$fit) else list(bundle$fit)
 selections<-structural_canvas_common_fit_measures(fits,bundle$estimator,.90)
 raw<-do.call(rbind,lapply(selections,function(s)structural_canvas_fit_guidance(s$values)))
 html<-as.character(structural_canvas_fit_guidance_result_ui(bundle,language));doc<-read(html)
 cells<-xml2::xml_find_all(doc,"//table[contains(@class,'structural-fit-guidance-table')]/tbody/tr/td[3]")
 stopifnot(identical(trimws(xml2::xml_text(cells)),vapply(raw$Value,format_decimal3,character(1))))
 if(name=='comparison')stopifnot('Review 사용자 <&>' %in% xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 notes<-xml2::xml_text(xml2::xml_find_all(doc,"//div[contains(@class,'structural-fit-guidance-result')]/p"))
 if(language!='en'){
  stopifnot(!grepl('Reference-only/review|Common reference points:|Fit guidance is not assessed',paste(notes,collapse=' ')))
  stopifnot(!grepl(': Reference only|: Not assessed|: Review',notes[1]))
  if(language!='ko')stopifnot(!any(raw$Reference %in% xml2::xml_text(xml2::xml_find_all(doc,"//table[contains(@class,'structural-fit-guidance-table')]//td"))))
 }
 if(name=='saturated')stopifnot(any(raw$Guidance=='Not assessed'),length(notes)==4L)
 writeLines(html,file.path(out,paste0(language,'-',name,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[name]]<-list(id=name,title=name,html=html)
 cat('PASS:',language,name,'fit guidance numeric values, summaries, notes and custom name\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
