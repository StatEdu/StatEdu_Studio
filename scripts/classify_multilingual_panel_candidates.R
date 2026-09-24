Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/multilingual-program-audit'
rows<-read.csv(file.path(out,'visible-fallback-candidates.csv'),stringsAsFactors=FALSE,fileEncoding='UTF-8')
catalog<-statedu_translation_table()
rows$classification<-'';rows$evidence_key<-''
title_functions<-list(
 lazy_data_editor_id_aggregate=function(lang)id_aggregate_text(lang,'One row per ID','ID당 한 줄'),
 lazy_analysis_structural_cfa=function(lang)structural_analysis_title('cfa',lang),
 lazy_analysis_structural_cbsem=function(lang)structural_analysis_title('cbsem',lang),
 lazy_analysis_structural_plssem=function(lang)structural_analysis_title('plssem',lang),
 lazy_analysis_complex_custom_model=function(lang)complex_sample_custom_model_title(lang),
 lazy_analysis_survival_cox=function(lang)survival_ui_text('Cox Regression',lang),
 lazy_analysis_survival_competing=function(lang)survival_ui_text('Competing Risks',lang))
for(i in seq_len(nrow(rows))) {
 text<-rows$english[i];lang<-rows$language[i]
 matches<-names(catalog)[vapply(catalog,function(entry) {
  target<-unname(entry[lang]);english<-unname(entry['en'])
  length(target)==1L&&length(english)==1L&&!is.na(target)&&!is.na(english)&&identical(target,text)&&!identical(english,text)
 },logical(1))]
 if(length(matches)) {
  rows$classification[i]<-'already_localized_catalog_value';rows$evidence_key[i]<-matches[1]
 } else if(identical(rows$panel[i],'lazy_analysis_ttest_anova')&&identical(text,'t-test / ANOVA')) {
  rows$classification[i]<-'statistical_menu_name'
 } else {
  fn<-title_functions[[rows$panel[i]]]
  if(is.function(fn)&&identical(fn(lang),text)&&!identical(fn('en'),text)) {
   rows$classification[i]<-'already_localized_title_function'
   rows$evidence_key[i]<-paste(deparse(body(fn)),collapse=' ')
  }else rows$classification[i]<-'needs_review'
 }
}
write.csv(rows,file.path(out,'classified-panel-candidates.csv'),row.names=FALSE,fileEncoding='UTF-8')
print(table(rows$classification));print(rows[rows$classification=='needs_review',],row.names=FALSE)
