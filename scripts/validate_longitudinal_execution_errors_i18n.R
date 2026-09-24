Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_longitudinal_input_errors_i18n.R',encoding='UTF-8')
d<-data.frame(y=c(1,3,2,5),x=1:4,.statedu_gee_id=c(1,1,2,2),.statedu_gee_waves=c(1,2,1,2))
bad<-d;bad$x<-1
errors<-list(adjusted_design=capture(longitudinal_gee_adjusted(y~x,bad,gaussian())),adjusted_family=capture(longitudinal_gee_adjusted(y~x,d,inverse.gaussian())))
bad<-d;bad$.statedu_gee_id[1]<-NA
errors$adjusted_ids<-capture(longitudinal_gee_adjusted(y~x,bad,gaussian()))
errors$adjusted_pairs<-capture(longitudinal_gee_adjusted(y~x,d,gaussian()))
for(key in names(errors))stopifnot(inherits(errors[[key]],'error'),conditionMessage(errors[[key]])==statedu_t(paste0('longitudinal.execution_error.',key),'en'))
# Exercise the actual isolated-process failure handler with a supplied external error.
process_handler<-NULL;package_expression<-NULL
collect<-function(node){
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('function'))&&identical(node[[2]],formals(function(e)NULL))&&grepl('Unstructured GEE could not complete',paste(deparse(node),collapse=' '),fixed=TRUE))process_handler<<-node
 if(is.call(node)&&identical(node[[1]],as.name('sprintf'))&&identical(node[[2]],'The %s package is required for this model.'))package_expression<<-node
 if(is.call(node)||is.expression(node))for(child in as.list(node))collect(child)
}
collect(body(longitudinal_gee_fit));collect(body(prepare_longitudinal_analysis_result))
stopifnot(!is.null(process_handler),!is.null(package_expression))
raw<-'External: C:/사용자/file.R\nname=<x&y> %s; eigenvalue=-1.20e-09'
process_error<-capture(eval(process_handler)(simpleError(raw)))
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 app_language_fn<-function()lang
 for(key in names(errors)) {
  expected<-statedu_t(paste0('longitudinal.execution_error.',key),lang)
  stopifnot(longitudinal_input_error_text(errors[[key]],lang)==expected)
  eval(callback)(errors[[key]])
  stopifnot(notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected))
 }
 expected<-paste0(statedu_t('longitudinal.execution_error.process',lang),raw)
 stopifnot(longitudinal_input_error_text(process_error,lang)==expected)
 eval(callback)(process_error)
 stopifnot(notice$text==paste(statedu_t('analysis.status.longitudinal_failed',lang),expected),notice$type=='error',notice$duration==8)
 for(package in c('geepack','lme4','mmrm','test.package2')) {
  error<-simpleError(eval(package_expression))
  stopifnot(longitudinal_input_error_text(error,lang)==sprintf(statedu_t('longitudinal.execution_error.package',lang),package))
 }
 stopifnot(longitudinal_input_error_text(simpleError(raw),lang)==raw)
 cat('PASS execution errors:',lang,'4 real guards, process callback, package names, raw detail\n')
}
