Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
lines<-readLines('R/setup_custom_model_canvas_structural_handlers.R',encoding='UTF-8')
# Evaluate the actual handler display expressions; process and timing logic remain outside this fixture.
sem_start<-grep('            detail <- paste0(percent',lines,fixed=TRUE)
sem_end<-which(seq_along(lines)>sem_start & grepl('structural_canvas_show_notification(',lines,fixed=TRUE))[1]-1L
pls_start<-grep('          phase_label <- tr(switch(phase, starting',lines,fixed=TRUE)
pls_end<-which(seq_along(lines)>pls_start & grepl('structural_canvas_show_notification(',lines,fixed=TRUE))[1]-1L
sem_expr<-parse(text=lines[sem_start:sem_end]);pls_expr<-parse(text=lines[pls_start:pls_end])
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 tr<-function(text)structural_canvas_reporting_text(text,language)
 percent<-50;completed<-50;total<-100;valid<-48;rate<-2.5;eta<-20
 eval(sem_expr);stopifnot(grepl('50/100',detail,fixed=TRUE),grepl('48',detail,fixed=TRUE),grepl('2.5',detail,fixed=TRUE),grepl('20',detail,fixed=TRUE))
 if(language!='en')stopifnot(!grepl('resamples|valid models|/sec',detail))
 for(phase in c('starting','resampling','summarizing','complete','unknown'))for(mode in c('running','stalled','indeterminate')){
  percentage<-if(mode=='indeterminate')NA_real_ else 50
  stalled<-mode=='stalled';elapsed<-20;remaining<-20;rate<-if(stalled)NA_real_ else 2.5;job<-list(nboot=100)
  eval(pls_expr)
  stopifnot(!grepl('%s',detail,fixed=TRUE),grepl('20',detail,fixed=TRUE))
  if(mode=='stalled')stopifnot(grepl(tr('Current resample batch is slow; ETA paused.'),detail,fixed=TRUE))
  if(mode=='indeterminate')stopifnot(!grepl('NA',detail,fixed=TRUE))
  if(language!='en'){
   stopifnot(!grepl('elapsed|requested|Current resample|Preparing summaries|Starting|Running',detail))
   stopifnot(phase_label!=switch(phase,starting='Starting',resampling='Resampling',summarizing='Preparing summaries',complete='Complete','Running'))
  }
 }
 for(phase_text in c('Loading engine','Starting workers','Validating screened models','Summarizing','Stratified multi-group latent-moderation resampling'))if(language!='en')stopifnot(tr(phase_text)!=phase_text)
 cat('PASS:',language,'actual SEM/PLS display expressions, numeric fields, stalled/indeterminate branches\n')
}
invisible(parse('R/setup_custom_model_canvas_structural_handlers.R'))
