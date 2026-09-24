Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data<-data.frame(time=1:6,event=rep(0:1,3))
name<-'사용자 <&> %s. Select a time variable.'
cases<-list(
 list(data_shape='single_record',roles=list(time='',event='',covariates=name)),
 list(data_shape='entry_exit',roles=list(time='time',event='event',entry='',covariates=name)),
 list(data_shape='start_stop',roles=list(subject_id='',start='',stop='',event='event',covariates=name)),
 list(data_shape='single_record',roles=list(time=name,event=name)))
errors<-lapply(cases,function(settings) {
 preflight<-survival_preflight(data,settings)
 error<-tryCatch(survival_preflight_stop(preflight),error=identity)
 stopifnot(inherits(error,'statedu_survival_preflight_error'),
  identical(conditionMessage(error),paste(unique(preflight$issues$message[preflight$issues$severity %in% c('error','block')]),collapse=' ')))
 error
})
code_keys<-c(missing_time_role='time',missing_event_role='event',missing_entry_role='entry',missing_subject_id='subject_id',missing_interval_role='interval',conflicting_roles='conflicting_roles',missing_columns='missing_columns')
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(error in errors) {
  expected<-vapply(seq_len(nrow(error$issues)),function(i) {
   row<-error$issues[i,];text<-statedu_t(paste0('survival.input_error.',code_keys[[row$code]]),lang)
   if(row$code=='missing_columns')sprintf(text,row$variable) else text
  },character(1))
  actual<-survival_input_error_text(error,lang)
  stopifnot(actual==paste(unique(expected),collapse=' '),grepl(name,actual,fixed=TRUE))
  if(lang=='en')stopifnot(actual==conditionMessage(error))
 }
 issues<-survival_bind_issues(list(
  survival_issue('warning','warning',message='Warning must not block'),
  survival_issue('error','missing_time_role',message='Select a time variable.'),
  survival_issue('error','missing_time_role',message='Select a time variable.'),
  survival_issue('block','external',message='External detail: 사용자가 입력한 문장 %s')))
 error<-tryCatch(survival_preflight_stop(list(ok=FALSE,issues=issues)),error=identity)
 stopifnot(survival_input_error_text(error,lang)==paste(statedu_t('survival.input_error.time',lang),'External detail: 사용자가 입력한 문장 %s'))
 empty<-tryCatch(survival_preflight_stop(list(ok=FALSE,issues=survival_empty_issues())),error=identity)
 stopifnot(survival_input_error_text(empty,lang)==statedu_t('survival.input_error.preflight_failed',lang))
 cat('PASS:',lang,'actual combined role errors; original English contract; user variable text; order/deduplication; warnings/external details/fallback\n')
}
valid<-list(ok=TRUE,issues=survival_empty_issues());stopifnot(identical(survival_preflight_stop(valid),valid))
