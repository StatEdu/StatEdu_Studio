Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8')
load_app_packages(check=FALSE);source_app_modules()
captured <- list()
structural_canvas_show_notification <- function(message, type, duration) {
  captured[[length(captured)+1L]] <<- list(message=message,type=type,duration=duration)
}
paths <- c('Review 사용자 <&> %s', 'X.1, Normality')
english <- NULL
for (language in c('en','ko','ja','zh','es','fr','de','vi')) {
  captured <- list()
  for (analysis_type in c('cfa','cbsem','sem')) {
    structural_canvas_notify_missing_covariances(paths,analysis_type,language)
  }
  structural_canvas_notify_ignored_pls_covariances(list(ignored_covariances=paths),'plssem',language)
  stopifnot(length(captured)==4L)
  messages <- vapply(captured, function(x) x$message, character(1))
  stopifnot(length(unique(messages[1:3]))==1L)
  if (language=='en') english <- messages else stopifnot(all(messages!=english))
  for (entry in captured) {
    stopifnot(grepl(paste(paths,collapse=', '),entry$message,fixed=TRUE),
      entry$type=='warning',entry$duration==10)
  }
  captured <- list()
  structural_canvas_notify_missing_covariances(paths,'plssem',language)
  structural_canvas_notify_missing_covariances(character(),'sem',language)
  structural_canvas_notify_ignored_pls_covariances(list(ignored_covariances=paths),'sem',language)
  structural_canvas_notify_ignored_pls_covariances(list(),'plssem',language)
  stopifnot(length(captured)==0L)
  cat('PASS:',language,'covariance alerts, literal names, warning duration and silent branches\n')
}
