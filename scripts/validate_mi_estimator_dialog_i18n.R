Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
calls <- list()
walk <- function(x) {
 if(is.call(x)) {
  if(identical(x[[1]],as.name('function')))return(walk(x[[3]]))
  if(as.character(x[[1]])[1] %in% c('modalDialog','showNotification'))calls[[length(calls)+1L]] <<- x
 }
 if(is.recursive(x))for(i in seq_along(x)) {
  if(identical(x[[i]],quote(expr=)))next
  walk(x[[i]])
 }
}
walk(body(structural_canvas_register_interaction_events))
choose <- function(pattern)Filter(function(x)grepl(pattern,paste(deparse(x),collapse=' '),fixed=TRUE),calls)
dialog <- choose('Document MI modification')[[1]]
alerts <- c(choose('All selected MI paths'),choose('The selected MI paths were added'))
stopifnot(length(alerts)==2)
prefix <- 'test';parameters <- c('Review 사용자 <&> %s ~ Normality','X.1 ~~ Y')
result <- list(converged=TRUE)
showNotification <- function(ui,type)list(message=ui,type=type)
recommendation <- list(diagnosis=list(skew_p=0.0001,kurtosis_p=0.037,n=589,original_n=594))
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 app_language_fn <- function()language
 tr <- function(en,ko)statedu_localized_text(statedu_current_language(app_language_fn),en,ko)
 for(has_skipped in c(FALSE,TRUE)) {
  skipped_details <- if(has_skipped)'User detail <&> %s' else character()
  doc <- xml2::read_html(as.character(eval(dialog)))
  text <- xml2::xml_text(doc)
  stopifnot(grepl(paste(parameters,collapse=', '),text,fixed=TRUE),
    length(xml2::xml_find_all(doc,"//textarea[@id='test_mi_justification']"))==1,
    length(xml2::xml_find_all(doc,"//button[@id='test_mi_confirm_apply']"))==1)
  if(has_skipped)stopifnot(grepl(skipped_details,text,fixed=TRUE))
  if(language!='en')stopifnot(!grepl('Document MI modification|Paths to add:|Substantive justification|Apply and reanalyze',text))
 }
 for(converged in c(FALSE,TRUE)) {
  result$converged <- converged
  rendered <- lapply(alerts,eval)
  stopifnot(rendered[[1]]$type=='warning',rendered[[2]]$type==if(converged)'message' else 'warning')
  if(language!='en')stopifnot(!any(grepl('All selected MI paths|The selected MI paths',vapply(rendered,`[[`,character(1),'message'))))
 }
 for(bollen in c(FALSE,TRUE)) {
  doc <- xml2::read_html(as.character(structural_canvas_estimator_recommendation_modal(recommendation,'cfa',prefix,bollen,language)))
  text <- xml2::xml_text(doc)
  for(value in c(format_p(0.0001),format_p(0.037),'589','594'))stopifnot(grepl(value,text,fixed=TRUE))
  stopifnot(grepl('Bollen-Stine',text,fixed=TRUE)==bollen)
  for(id in c('test_run_with_ml','test_run_with_mlr'))stopifnot(length(xml2::xml_find_all(doc,paste0("//button[@id='",id,"']")))==1)
  if(language!='en')stopifnot(!grepl('Estimator recommendation|sample-size-sensitive|complete cases|Run with ML',text))
 }
 cat('PASS:',language,'MI modal and alerts, raw paths/details, estimator modal, four numbers, optional Bollen note and input IDs\n')
}
