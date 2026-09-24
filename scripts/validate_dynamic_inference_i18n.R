Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/dynamic-inference-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
methods<-c('Bootstrap bias-corrected and accelerated (BCa) 95% CI','Bootstrap bias-corrected (BC) 95% CI','Bootstrap percentile 95% CI','Bootstrap BCa 95% CI')
for(index in seq_along(methods)){
 sources<-unlist(lapply(c('', ' (R quantile type 6)',' (R quantile type 7)'),function(q)unlist(lapply(c('Adequate','Caution','Unreliable'),function(status)paste0(methods[index],q,c('; valid standardized bootstrap ','; valid '),'60/100 (60.0%); status ',status)))))
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  translated<-structural_canvas_localize_inference_text(sources,language)
  if(language=='en')stopifnot(identical(translated,sources))else stopifnot(all(translated!=sources),!any(grepl('; valid |; status |R quantile type',translated)))
  stopifnot(all(grepl('60/100 (60.0%)',translated,fixed=TRUE)))
  for(type in c('6','7'))stopifnot(all(grepl(type,translated[grepl(paste('type',type),sources,fixed=TRUE)],fixed=TRUE)))
  table<-data.frame(Path=rep('Review 사용자 <&> %s',length(sources)),'CI source'=sources,check.names=FALSE)
  localized<-structural_canvas_localize_reporting_metadata(table,language,rename_headers=FALSE)
  stopifnot(identical(localized$Path,table$Path),identical(localized[['CI source']],translated))
  if(language=='ja'){
   attr(localized,'result_user_columns')<-seq_along(localized)
   entries[[paste0('method',index)]]<-list(id=paste0('method',index),title=paste0('Method ',index),html=as.character(structural_canvas_basic_html_table(localized,language=language)))
  }
  cat('PASS: method',index,language,'numeric metadata, quantile type, status and path preservation\n')
 }
}
static<-c('Bootstrap requested - inference suppressed','Bootstrap pending - inference suppressed','Bootstrap canceled - inference suppressed','Bootstrap failed - inference suppressed','Bootstrap blocked - original model ineligible','Bootstrap unavailable - inference suppressed','Not estimated - insufficient valid bootstrap replicates','Not applicable - fixed parameter','Fixed parameter - no inferential test','Fixed effect - no inferential test','Bootstrap (empirical two-sided p)')
for(language in c('ko','ja','zh','es','fr','de','vi'))stopifnot(all(structural_canvas_localize_inference_text(static,language)!=static))
for(language in c('ja','zh','es','fr','de','vi')){
 unknown<-c('Review','Normality','사용자 <&> %s','Bootstrap custom 사용자','Review; valid 60/100 (60.0%); status Caution','Bootstrap percentile 95% CI; valid custom %s; status Caution')
 stopifnot(identical(structural_canvas_localize_inference_text(unknown,language),unknown))
}
fallback<-paste0('Not estimated - insufficient valid standardized bootstrap replicates; valid 40/100 (40.0%); status Unreliable')
for(language in c('ko','ja','zh','es','fr','de','vi')){
 text<-structural_canvas_localize_inference_text(fallback,language);stopifnot(text!=fallback,grepl('40/100 (40.0%)',text,fixed=TRUE))
}
all_sources<-c(static,fallback)
table<-data.frame(Path='Review 사용자 <&> %s','CI source'=all_sources,check.names=FALSE)
table<-structural_canvas_localize_reporting_metadata(table,'ja');attr(table,'result_user_columns')<-seq_along(table)
entries[['states']]<-list(id='states',title='States',html=as.character(structural_canvas_basic_html_table(table,language='ja')))
saveRDS(unname(entries),file.path(out,'entries.rds'))
