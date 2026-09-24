Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# Evaluate actual UI/message expressions without running analysis observers.
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
dialog <- choose('Heywood-constrained reanalysis')[[1]]
notifications <- c(choose('analysis completed.'),choose('package is required.'),choose('The constrained model fixed'))
stopifnot(length(notifications)==4L)
prefix <- 'test';analysis_type <- 'cfa';package <- 'lavaan';result <- list(converged=TRUE,admissible=TRUE)
constraint <- list(variables=c('Review 사용자 <&> %s','Normality, X.1'),percent=0.1)
showNotification <- function(ui,type,duration=NULL)list(message=ui,type=type,duration=duration)
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 app_language_fn <- function()language
 tr <- function(en,ko)statedu_localized_text(statedu_current_language(app_language_fn),en,ko)
 html <- as.character(eval(dialog))
 doc <- xml2::read_html(html)
 input <- xml2::xml_find_first(doc,"//input[@id='test_heywood_percent']")
 stopifnot(xml2::xml_attr(input,'value')=='0.1',xml2::xml_attr(input,'min')=='0.01',
  xml2::xml_attr(input,'max')=='5',xml2::xml_attr(input,'step')=='0.01',
  length(xml2::xml_find_all(doc,"//button[@id='test_heywood_confirm']"))==1L)
 if(language!='en')stopifnot(!grepl('Heywood-constrained reanalysis|Run constrained model|Recommended starting value',html))
 rendered <- lapply(notifications,eval)
 stopifnot(grepl('lavaan',rendered[[3]]$message,fixed=TRUE),
  grepl(paste(constraint$variables,collapse=', '),rendered[[4]]$message,fixed=TRUE),
  grepl('0.1%',rendered[[4]]$message,fixed=TRUE),rendered[[4]]$duration==10)
 if(language!='en')stopifnot(!any(grepl('analysis completed|package is required|The constrained model fixed',vapply(rendered,`[[`,character(1),'message'))))
 result$admissible <- FALSE;stopifnot(eval(notifications[[4]])$type=='warning');result$admissible <- TRUE
 cat('PASS:',language,'actual dialog markup, input bounds, completion/package alerts and literal constraint names\n')
}
