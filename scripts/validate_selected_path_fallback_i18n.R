Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture_error <- function(expr) tryCatch({force(expr);stop('Expected engine rejection')},error=conditionMessage)
literal <- "Review 사용자 <&> %s, X's path"
actual <- c(
 capture_error(structural_canvas_pls_mga_snapshot_path_registry(list(nodes=list(list(id='x'),list(id='x'))))),
 capture_error(structural_canvas_pls_mga_direct_registry(list())),
 capture_error(structural_canvas_pls_mga_normalize_selected_paths(data.frame(Predictor=literal),list(),data.frame())),
 capture_error(structural_canvas_pls_mga_normalize_selected_paths(data.frame(Unrelated=literal),list(),data.frame())),
 capture_error(structural_canvas_pls_mga_selected_column(setNames(data.frame(a=1,b=2),c('Predictor','Source')),c('Predictor','Source'))))
stopifnot(!any(grepl('could not find function|Expected engine rejection',actual)))
# Read the exact static engine errors listed in the notification branch.
expressions <- parse('R/setup_custom_model_canvas_structural_execute_notifications.R',encoding='UTF-8')
find_registry <- function(x) {
 if(is.call(x)&&identical(x[[1]],as.name('function')))return(find_registry(x[[3]]))
 if(is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('selected_path_errors')))return(eval(x[[3]]))
 if(is.recursive(x))for(child in as.list(x)){found<-find_registry(child);if(length(found))return(found)}
 NULL
}
static <- find_registry(expressions);stopifnot(length(static)==13L)
engine <- paste(readLines('R/setup_custom_model_canvas_structural_invariance_evaluation.R',encoding='UTF-8'),
 readLines('R/setup_custom_model_canvas_structural_pls_mga_engine.R',encoding='UTF-8'),collapse='\n')
stopifnot(all(vapply(static,grepl,logical(1),x=engine,fixed=TRUE)))
dynamic <- c(paste0('PLS-MGA selected-path input has ambiguous columns: ',literal,'.'),
 paste0('One or more selected canvas paths were not estimable structural regressions in the multi-group SEM: ',literal,'.'),
 paste0("Selected path '",literal,"' was not represented by exactly one free regression coefficient in every group."))
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 messages <- c(static,dynamic,actual)
 translated <- unname(vapply(messages,structural_canvas_error_message,character(1),language=language))
 if(language=='en')stopifnot(identical(messages,translated))else stopifnot(all(messages!=translated))
 stopifnot(all(vapply(translated[14:16],grepl,logical(1),pattern=literal,fixed=TRUE)))
 if(!language %in% c('en','ko'))for(unknown in c('A custom selected-path failure', 'Saved file at selected path',literal))
   stopifnot(structural_canvas_error_message(unknown,language)==unknown)
 cat('PASS:',language,'13 engine source forms, three literal-name templates, five actual engine rejections\n')
}
