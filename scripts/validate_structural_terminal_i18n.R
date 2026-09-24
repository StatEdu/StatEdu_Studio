Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
counts<-c('Timeouts'=11L,'Estimation failures'=22L,'Nonconvergence'=33L,'Inadmissible solutions'=44L,'Statistic-contract failures'=55L,'Execution failures'=66L,'Cancellations'=77L)
errors<-c('사용자 <&> model: covariance error','literal %s / Review')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 for(model in c('SEM and multi-group latent-moderation','SEM path, indirect, and total-effect','PLS/PLSc'))for(state in c('stopped','complete','partial','failed')){
  text<-structural_canvas_bootstrap_terminal_text(state,model,language,errors)
  stopifnot(endsWith(text,paste(errors,collapse=' | ')))
  if(language!='en')stopifnot(!startsWith(text,'The '),!startsWith(text,'Only part'))
 }
 for(available in c(TRUE,FALSE)){
  text<-structural_canvas_pls_bootstrap_completion_text(1234L,5000L,counts,available,language)
  stopifnot(grepl('1,234/5,000',text,fixed=TRUE))
  for(label in names(counts))stopifnot(grepl(paste0(structural_canvas_reporting_text(label,language),' ',counts[[label]]),text,fixed=TRUE))
  stopifnot(grepl(structural_canvas_reporting_text(if(available)'Result tables were updated.' else 'Inference is suppressed because the valid ratio is below 80%.',language),text,fixed=TRUE))
  if(language!='en')stopifnot(!grepl('Valid resamples|Statistic-contract failures|Inference is suppressed|Result tables were updated',text))
 }
 cat('PASS:',language,'terminal branches, seven counts, inference state and literal engine errors\n')
}
invisible(parse('R/setup_custom_model_canvas_structural_handlers.R'))
