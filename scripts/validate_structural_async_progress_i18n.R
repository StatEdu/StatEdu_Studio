Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
messages<-c('%s bootstrap progress','Base-model results are available now.','Stop bootstrap','Starting','Complete','AVE/reliability','The %s bootstrap was stopped. Base-model results remain available.','The %s bootstrap is complete and result tables were updated.','The %s bootstrap did not complete.')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 tr<-function(text)structural_canvas_reporting_text(text,language)
 for(message in messages){
  translated<-tr(message)
  if(language!='en')stopifnot(translated!=message)
  if(grepl('%s',message,fixed=TRUE))stopifnot(grepl('CFA',sprintf(translated,'CFA'),fixed=TRUE),!grepl('%s',sprintf(translated,'SEM'),fixed=TRUE))
 }
 for(phase in c('starting','reliability','bollen_stine','htmt','complete')){
  label<-tr(switch(phase,reliability='AVE/reliability',bollen_stine='Bollen-Stine',htmt='HTMT',complete='Complete','Starting'))
  html<-as.character(statedu_bootstrap_status_ui(sprintf(tr('%s bootstrap progress'),'CFA'),paste0(label,' · 50% · 50/100'),percent=50,stop_input_id='test_stop',stop_label=tr('Stop bootstrap'),phase_label=label))
  text<-xml2::xml_text(xml2::read_html(html,encoding='UTF-8'))
  stopifnot(grepl('50/100',text,fixed=TRUE),grepl(label,text,fixed=TRUE),grepl(tr('Stop bootstrap'),text,fixed=TRUE))
 }
 cat('PASS:',language,'nine notification templates and five rendered phases\n')
}
previous<-list(phase='bollen_stine',completed=50L,total=100L,valid=48L)
stopifnot(identical(structural_canvas_cfa_bootstrap_progress_merge(previous,list(phase='reliability',completed=10L,total=100L,valid=9L)),previous))
next_state<-list(phase='complete',completed=100L,total=100L,valid=98L)
stopifnot(identical(structural_canvas_cfa_bootstrap_progress_merge(previous,next_state),next_state))
invisible(parse('R/setup_custom_model_canvas_structural_handlers.R'))
cat('PASS: progress monotonicity and handler syntax\n')
