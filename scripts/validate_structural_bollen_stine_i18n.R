Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
out<-'tmp/structural-bollen-stine-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
actual<-structural_canvas_bollen_stine(cfa$fit,reps=20L,seed=924L)
fixtures<-list(actual=actual)
for(valid in c(12L,4L)){
 row<-actual;row$`Valid replicates`<-valid;row$`Valid %`<-100*valid/20
 row$Status<-structural_canvas_bootstrap_status(valid,20)
 fixtures[[row$Status]]<-row
}
entries<-list()
for(name in names(fixtures))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 raw<-fixtures[[name]];bundle<-cfa;bundle$bollen_stine_result<-raw;bundle$modified_from_baseline<-name!='actual'
 html<-as.character(structural_canvas_bollen_stine_result_ui(bundle,language));doc<-read(html)
 cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td')))
 expected<-c(format_decimal3(raw$`Observed chi-square`),format_p(raw$`Bootstrap p`),format_decimal3(raw$`Monte Carlo SE`),format_decimal3(raw$`Monte Carlo 95% lower`),format_decimal3(raw$`Monte Carlo 95% upper`),as.character(raw$`Valid replicates`),as.character(raw$`Requested replicates`),paste0(format_decimal3(raw$`Valid %`),'%'))
 stopifnot(identical(cells[1:8],unname(expected)),cells[10]==as.character(raw$Seed))
 notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 stopifnot(length(notes)==if(name=='actual')3L+as.integer(raw$Status!='Adequate') else 5L)
 if(language!='en'){
  stopifnot(!grepl('The bootstrap p value|This transformed-data|This model was modified|Fewer than 80%',paste(notes,collapse=' ')))
  stopifnot(!any(c('Observed chi-square','Monte Carlo SE','Valid replicates','Requested replicates') %in% xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
  stopifnot(cells[9]!=raw$Status)
 }
 writeLines(html,file.path(out,paste0(language,'-',name,'.html')),useBytes=TRUE)
 if(language=='ja')entries[[name]]<-list(id=name,title=name,html=html)
 cat('PASS:',language,name,'Bollen-Stine numeric cells, notes, headers and status\n')
}
stopifnot(is.null(structural_canvas_bollen_stine_result_ui(cfa,'ja')))
saveRDS(unname(entries),file.path(out,'entries.rds'))
