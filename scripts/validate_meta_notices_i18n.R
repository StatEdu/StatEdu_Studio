Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
validation <- NULL; completion <- NULL
walk <- function(node) {
 if(missing(node))return()
 if(is.call(node)) {
   if(identical(node[[1]],as.name('observeEvent')) && identical(node[[2]],quote(input$meta_validate_effects)))
     validation <<- node[[3]]
   if(identical(node[[1]],as.name('showNotification')) && grepl('meta.notice.analysis_complete',paste(deparse(node),collapse=' '),fixed=TRUE))
     completion <<- node
 }
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk(child)
}
walk(parse('R/server_meta.R',encoding='UTF-8'))
stopifnot(!is.null(validation),!is.null(completion))
# Execute the real notification observer with only effect-data boundaries isolated.
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 language <- function()lang
 current_family <- function()'g'
 original <- data.frame(study_name='사용자 <&> %s',yi=0.25)
 saved <- original; cleared <- FALSE; revalidated <- FALSE
 effects <- function(value) {if(missing(value))return(saved);saved<<-value}
 meta_revalidate_effects <- function(value,family) {revalidated<<-TRUE;value}
 analysis_result <- function(value) {stopifnot(is.null(value));cleared<<-TRUE}
 showNotification <- function(ui,type,duration,...) {notice<<-list(text=ui,type=type,duration=duration)}
 for(counts in list(c(12,0,0),c(9,3,0),c(8,2,2),c(0,0,0))) {
   meta_effect_summary <- function(...)list(valid=counts[1],warnings=counts[2],errors=counts[3])
   eval(validation)
   expected_type <- if(counts[3]>0)'error' else if(counts[2]>0)'warning' else 'message'
   stopifnot(identical(notice$text,sprintf(statedu_t('meta.notice.validation_complete',lang),counts[1],counts[2],counts[3])),
     identical(notice$type,expected_type),notice$duration==6,cleared,revalidated,identical(saved,original))
 }
 eval(completion)
 stopifnot(identical(notice$text,statedu_t('meta.notice.analysis_complete',lang)),notice$type=='message',notice$duration==5)
 cat('PASS:',lang,'actual validation observer and completion notification; counts/severity/data preservation\n')
}
