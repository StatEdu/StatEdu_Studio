Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
scenarios <- list(
 recurrent=list(objective='recurrent'),
 multistate=list(objective='state_transition'),
 interval=list(data_shape='interval_censored'),
 time_dependent=list(time_dependent=TRUE),
 delayed_competing=list(objective='competing',data_shape='entry_exit',event_structure='competing'),
 interval_confirmation=list(data_shape='start_stop')
)
expected <- list()
for(name in names(scenarios)) {
 config <- modifyList(list(objective='association',data_shape='single_record',event_structure='single',time_dependent=FALSE,competing_estimand='both'),scenarios[[name]])
 result <- survival_recommend(config)
 stopifnot(result$status %in% c('unsupported','needs_confirmation'),is.null(result$target_tab))
 expected[[name]] <- list(config=config,status=result$status,languages=list())
 for(language in c('ja','zh','es','fr','de','vi','en','ko')) {
  expected[[name]]$languages[[language]] <- as.list(unname(survival_recommendation_text(unlist(result[c('primary','confirmations','blocked_by')],use.names=FALSE),language)))
 }
}
jsonlite::write_json(expected,'tmp/survival-restriction-branches.json',auto_unbox=TRUE,pretty=TRUE)
cat('PASS: six restricted engine branches and translated expectations\n')
