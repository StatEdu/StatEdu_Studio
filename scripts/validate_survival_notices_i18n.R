Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false',STATEDU_RESULT_STORE=tempfile(fileext='.json'))
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
runs<-list();notices<-character();fail<-FALSE
register_analysis_command_handler<-function(run_id,...,run_fn)runs[[run_id]]<<-run_fn
showNotification<-function(ui,...) {notices<<-c(notices,as.character(ui));invisible('test')}
detail<-'External: D:/사용자 & %s/data.csv'
prepare_km_analysis_result<-prepare_cox_analysis_result<-prepare_competing_risk_result<-function(...) {
 if(fail)stop(detail);NULL
}
directory<-'D:/사용자 & %s/보고'
choose_figure_save_dir<-function()directory
save_survival_reporting_files<-function(result,directory,language) {
 stopifnot(language==active_language);c('one.csv','two.csv')
}
save_survival_competing_figure_files<-function(...)c('D:/사용자 & %s/a.png','D:/사용자 & %s/b.png')
shiny::testServer(function(input,output,session) {
 language<-reactiveVal('en')
 register_survival_handlers(input,output,session,function()c('time','event'),
  function()data.frame(time=1:4,event=c(0,1,0,1)),function()data.frame(),function()character(),function()data.frame(),function()NULL,app_language_fn=language)
}, {
 session$flushReact();n<-0L
 for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
  active_language<<-lang;language(lang);session$flushReact()
  for(kind in c('km','cox','competing')) {
   run<-runs[[paste0('run_survival_',kind)]]
   fail<<-FALSE;notices<<-character();shiny::isolate(run())
   stopifnot(statedu_t(paste0('survival.notice.',kind,'_done'),lang) %in% notices)
   fail<<-TRUE;notices<<-character();shiny::isolate(run())
   stopifnot(paste(statedu_t(paste0('survival.notice.',kind,'_failed'),lang),detail) %in% notices)
   state<-get(paste0(kind,'_result'),envir=environment(run));state(list(marker='fixture'))
   notices<<-character();n<-n+1L
   do.call(session$setInputs,setNames(list(n),paste0('save_survival_',kind,'_audit_dialog')))
   stopifnot(sprintf(statedu_t('survival.notice.reports_saved',lang),2L,directory) %in% notices)
  }
  notices<<-character();n<-n+1L
  session$setInputs(save_survival_competing_figures_dialog=n)
  stopifnot(sprintf(statedu_t('survival.notice.figures_saved',lang),paste(save_survival_competing_figure_files(),collapse=', ')) %in% notices)
  cat('PASS:',lang,'three analysis success/failure callbacks; three audit saves; competing figures; paths/external errors preserved\n')
 }
})
