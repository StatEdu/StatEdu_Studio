Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out <- 'tmp/structural-recommendation-i18n'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
# Execute the actual server observer body with intercepted navigation/notification.
find_observer <- function(x) {
  if(is.call(x) && identical(x[[1]],as.name('observeEvent')) &&
     identical(paste(deparse(x[[2]]),collapse=''),'input$structural_automation_start')) return(list(x))
  if(is.recursive(x)) return(unlist(lapply(as.list(x),find_observer),recursive=FALSE))
  list()
}
observer <- find_observer(parse('R/app_server.R',encoding='UTF-8'))
stopifnot(length(observer)==1L)
source_calls <- function(x) {
  if(is.call(x) && identical(x[[1]],as.name('statedu_localized_text'))) return(list(x))
  if(is.recursive(x)) return(unlist(lapply(as.list(x),source_calls),recursive=FALSE))
  list()
}
calls <- c(source_calls(body(structural_automation_title)),source_calls(body(structural_automation_tab_panel)))
expected_values <- list(objective=c('measurement','theory','prediction'),construct=c('common_factor','composite','mixed'),indicator=c('continuous','ordered'))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  html <- as.character(structural_automation_tab_panel(lang))
  doc <- xml2::read_html(html,encoding='UTF-8')
  visible <- xml2::xml_text(doc)
  for(call in calls) {
    source <- call[[3]]; korean <- call[[4]]
    translated <- statedu_localized_text(lang,source,korean)
    stopifnot(grepl(translated,visible,fixed=TRUE))
    if(!lang %in% c('en','ko')) stopifnot(!identical(translated,source))
  }
  for(id in names(expected_values)) {
    nodes <- xml2::xml_find_all(doc,paste0('//select[@id="structural_automation_',id,'"]/option'))
    stopifnot(identical(xml2::xml_attr(nodes,'value'),expected_values[[id]]))
  }
  grid <- expand.grid(objective=expected_values$objective,construct=expected_values$construct,indicator=expected_values$indicator,stringsAsFactors=FALSE)
  for(i in seq_len(nrow(grid))) {
    row <- grid[i,]
    env <- new.env(parent=globalenv())
    env$input <- list(structural_automation_objective=row$objective,structural_automation_construct=row$construct,structural_automation_indicator=row$indicator)
    env$app_language <- local({value<-lang;function() value})
    env$session <- NULL; env$target <- NULL;env$notice <- NULL
    env$updateTabsetPanel <- function(session,inputId,selected) {env$target<-selected}
    env$showNotification <- function(ui,...) {env$notice<-ui}
    run <- eval(call('function',NULL,observer[[1]][[3]]),env)
    run()
    unsupported <- row$indicator=='ordered' && row$construct!='common_factor'
    if(unsupported) {
      stopifnot(is.null(env$target),is.character(env$notice),nzchar(env$notice))
      warning_source <- 'The current engine does not support ordered indicators combined with composite constructs. Review the construct specification.'
      if(!lang %in% c('en','ko')) stopifnot(!identical(env$notice,warning_source))
    } else {
      expected <- if(row$objective=='measurement' && row$construct=='common_factor') 'cfa' else if(row$construct!='common_factor' || row$objective=='prediction') 'plssem' else 'cbsem'
      stopifnot(is.null(env$notice),identical(env$target,paste0('analysis_structural_',expected)))
    }
  }
  writeLines(html,file.path(out,paste0(lang,'.html')),useBytes=TRUE)
  cat('PASS:',lang,'all UI text, stable selection values, 18 actual observer routing/warning combinations\n')
}
