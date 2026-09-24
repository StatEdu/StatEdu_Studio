Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture <- function(expr) tryCatch({force(expr);stop('Expected failure')},error=function(e)e)
errors <- list(
 capture(hint8_score(data.frame(x=1))), capture(eq5d_score(data.frame(x=1))),
 capture(hint8_calculator_result(NULL,rep('x',8))),
 capture(eq5d_calculator_result(NULL,rep('x',5))),
 capture(frs_result(NULL,rep('x',8))),
 capture(metabolic_result(NULL,rep('x',9),NULL)),
 capture(metabolic_severity_result(NULL,rep('x',7))),
 capture(ascvd10_result(NULL,rep('x',nrow(ascvd10_variable_specs())))),
 capture(hint8_calculator_result(NULL,paste0('x',1:8))),
 capture(hint8_calculator_result(as.data.frame(setNames(rep(list(1),8),paste0('x',1:8))),paste0('x',1:8),variable_choices='none')),
 capture(eq5d_calculator_result(as.data.frame(setNames(rep(list(1),5),paste0('x',1:5))),paste0('x',1:5),variable_choices='none')))
files <- list.files('R',pattern='^calculator_.*[.]R$',full.names=TRUE)
handlers <- list()
walk <- function(x) {
 if(missing(x))return(invisible(NULL))
 if(is.call(x) && identical(x[[1]],as.name('function')) && identical(names(x[[2]]),'error') &&
    grepl('calculator_error_text',paste(deparse(x),collapse=' '),fixed=TRUE)) handlers[[length(handlers)+1L]] <<- x
 if(is.recursive(x))for(child in as.list(x)) {
   if(missing(child))next
   if(!is.symbol(child))walk(child)
 }
}
for(path in files)walk(parse(path,encoding='UTF-8'))
stopifnot(length(handlers)==6L)
external <- simpleError('Warning\n사용자.x <&> %s: 1.2300e-09')
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 for(key in c('item_count','selection_count','unavailable','required'))
   stopifnot(nzchar(statedu_t(paste0('calculator.error.',key),lang,fallback='')))
 for(error in errors) {
  translated <- calculator_error_text(error,lang)
  stopifnot(nzchar(translated),!grepl('%s|calculator.error.',translated,fixed=FALSE))
  if(lang!='en')stopifnot(translated!=conditionMessage(error))
  for(handler in handlers) {
   env<-new.env(parent=.GlobalEnv);env$language<-lang;env$message<-NULL
   env$showNotification<-function(ui,...)env$message<-ui
   eval(handler,env)(error)
   stopifnot(identical(env$message,translated))
  }
 }
 stopifnot(identical(calculator_error_text(external,lang),conditionMessage(external)))
}
cat('PASS 11 actual error paths, 6 notification handlers, 8 languages plus Korean return; external error details preserved\n')
