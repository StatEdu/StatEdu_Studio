Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8'); load_app_packages(check=FALSE); source_app_modules()
capture <- function(expr) { value <- tryCatch(expr,error=identity); stopifnot(inherits(value,'error')); value }
data <- data.frame(x=1:6,y=c(2,4,5,8,9,12))
fit <- lm(y~x,data=data)
# A fitted model without a time-interaction coefficient triggers the actual guard.
tv_error <- capture(survival_cox_time_varying_curve(fit,data,'x','y','x'))
# Preserve the matrix but make the coefficient name inconsistent to exercise reconstruction failure.
names(fit$coefficients)[2] <- 'unmatched_coefficient'
errors <- list(no_data=capture(survival_preflight(NULL,list())),
 settings_list=capture(survival_preflight(data,1)), tv_coefficients=tv_error,
 spline_design=capture(survival_cox_spline_curve(fit,data,'x')))
literal_errors <- character()
walk <- function(node) {
 if(missing(node))return(invisible(NULL))
 if(is.call(node) && identical(node[[1]],as.name('stop')) && length(node)>1L && is.character(node[[2]]))
   literal_errors <<- c(literal_errors,node[[2]])
 if(is.call(node)||is.expression(node)||is.pairlist(node))for(child in as.list(node))walk(child)
}
walk(parse('R/analysis_survival.R',encoding='UTF-8'))
literal_errors <- unique(literal_errors)
stopifnot(length(literal_errors)>0L)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(message in literal_errors) {
   translated <- survival_input_error_text(simpleError(message),lang)
   if(lang=='en')stopifnot(identical(translated,message)) else stopifnot(nzchar(translated),translated!=message)
 }
 for(key in names(errors)) {
   expected <- statedu_t(paste0('survival.input_error.',key),lang)
   stopifnot(conditionMessage(errors[[key]])==statedu_t(paste0('survival.input_error.',key),'en'),
     survival_input_error_text(errors[[key]],lang)==expected)
   if(lang!='en')stopifnot(expected!=conditionMessage(errors[[key]]))
 }
 raw <- 'External model error: 사용자 <&> %s. No data is loaded.'
 stopifnot(identical(survival_input_error_text(simpleError(raw),lang),raw))
 cat('PASS:',lang,'four actual error branches;',length(literal_errors),'literal stop messages; external detail preservation\n')
}
