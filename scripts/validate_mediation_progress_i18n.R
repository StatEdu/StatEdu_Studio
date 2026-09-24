Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
dir.create('tmp/mediation-progress-i18n',showWarnings=FALSE,recursive=TRUE)
file<-'tmp/mediation-progress-i18n/progress.rds'
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 for(phase in c('starting','preparing','resampling','finalizing','serializing','complete','unknown'))for(known_rate in c(FALSE,TRUE)){
  now<-Sys.time();state<-new.env(parent=emptyenv())
  if(known_rate)state$rate_samples<-c(9,10,11)
  job<-list(progress_file=file,progress_state=state,requested_total=1000L,boot_r=500L,started_at=now-40)
  done<-if(phase=='complete')1000L else 500L
  saveRDS(list(phase=phase,done=done,total=1000L,boot_r=500L,focal='Review 사용자 <&>',updated_at=now),file)
  value<-mediation_moderation_bootstrap_job_progress(job,language)
  stopifnot(value$done==done,value$total==1000L,value$phase==phase,!grepl('%s',value$detail,fixed=TRUE))
  if(phase=='resampling'){
   stopifnot(value$percent==50,grepl('Review 사용자 <&>',value$detail,fixed=TRUE),grepl('500/1,000',value$detail,fixed=TRUE))
   if(known_rate)stopifnot(value$rate==10,value$remaining==50) else stopifnot(is.na(value$rate),is.na(value$remaining))
  }else{
   stopifnot(is.na(value$rate),is.na(value$remaining))
   if(phase=='complete')stopifnot(value$percent==100) else stopifnot(is.na(value$percent))
  }
  if(!language %in% c('en','ko'))stopifnot(!grepl('Starting worker|Preparing models|Computing bootstrap summaries|Saving results|elapsed|resamples planned|estimating resampling',value$detail))
  # A later stale snapshot must not regress completed work.
  saveRDS(list(phase='starting',done=0L,total=1000L,boot_r=500L,updated_at=now),file)
  next_value<-mediation_moderation_bootstrap_job_progress(job,language)
  stopifnot(next_value$done==done,next_value$phase==phase)
 }
 cat('PASS:',language,'seven phases, known/unknown ETA, literal variable, numeric state and stale snapshot guard\n')
}
