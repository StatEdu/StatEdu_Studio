Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
scenarios <- list(
 cause_specific=list(objective='competing',competing_estimand='cause_specific'),
 fine_gray=list(objective='competing',competing_estimand='cumulative_incidence'),
 gray=list(objective='group_comparison',competing_estimand='both'),
 confirmation=list(objective='competing',competing_estimand=''),
 unsupported=list(objective='prediction',competing_estimand='both')
)
expected <- list()
for (name in names(scenarios)) {
 result <- survival_recommend(c(scenarios[[name]],list(data_shape='single_record',event_structure='competing')))
 expected[[name]] <- list(status=result$status, languages=list())
 for (language in c('ja','zh','es','fr','de','vi','en','ko')) {
  strings <- unname(survival_recommendation_text(unlist(result[c('primary','confirmations','blocked_by')],use.names=FALSE),language))
  expected[[name]]$languages[[language]] <- as.list(strings)
 }
}
jsonlite::write_json(expected,'tmp/survival-remaining-recommendations.json',auto_unbox=TRUE,pretty=TRUE)
cat('PASS: generated expected translated recommendation names and notices for five branches / eight languages\n')
