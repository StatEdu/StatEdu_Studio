Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
root <- tempfile('history-errors-');dir.create(root)
paths <- setNames(file.path(root,paste0(c('read','structure','type','entries'),'.json')),c('read','structure','type','entries'))
payloads <- c('{broken','42','{"type":"other","entries":[]}','{"type":"easyflow_result_history","entries":42}')
for(i in seq_along(paths)) writeLines(payloads[[i]],paths[[i]],useBytes=TRUE)
original <- lapply(paths,function(path)readBin(path,'raw',n=file.info(path)$size))
errors <- lapply(paths,function(path)tryCatch(read_result_snapshot_store(path),error=identity))
# Exercise the migration failure without touching the application's real history.
legacy <- file.path(root,'legacy.json');writeLines('{"type":"easyflow_result_history","entries":[]}',legacy)
migrate <- read_result_snapshot_store
env <- new.env(parent=environment(migrate));environment(migrate)<-env
env$result_snapshot_store_path <- function()file.path(root,'target.json')
env$legacy_result_snapshot_store_path <- function()legacy
env$file.exists <- function(path)path==legacy
env$file.copy <- function(...)FALSE
Sys.unsetenv('STATEDU_RESULT_STORE')
errors$migration <- tryCatch(migrate(),error=identity)
langs <- c('en','ko','ja','zh','es','fr','de','vi')
notices <- new.env();notices$values <- character()
showNotification <- function(ui,...) {notices$values<-c(notices$values,as.character(ui));invisible('test')}
chosen <- paths[['read']]
choose_result_history_open_path <- function(language) {stopifnot(language==active_language);chosen}
for(language in langs) {
 for(key in names(errors)) {
  stopifnot(inherits(errors[[key]],'error'),
   identical(result_history_error_text(errors[[key]],language),statedu_t(paste0('result.history_error.',key),language)))
 }
 external <- simpleError('Reader: D:/사용자 <&> %s/결과.json')
 stopifnot(identical(result_history_error_text(external,language),conditionMessage(external)))
 active_language <- language
 Sys.setenv(STATEDU_RESULT_STORE=paths[['read']])
 notices$values <- character()
 server <- function(input,output,session) register_result_accumulator_outputs(input,output,session,function()active_language)
 shiny::testServer(server, {
  session$setInputs(open_result_history_dialog=0);session$flushReact()
  stopifnot(statedu_t('result.history_error.restore',active_language) %in% notices$values)
  preserved <- list(list(id='keep',title='사용자 결과',saved_at='2026-09-17',html='<p>Estimate 1.230</p>'))
  result_accumulator_store(session)(preserved)
  for(i in seq_along(paths)) {
   chosen <<- paths[[i]];notices$values<-character()
   session$setInputs(open_result_history_dialog=i)
   stopifnot(identical(shiny::isolate(result_accumulator_store(session)()),preserved),
    paste(statedu_t('result.open_failed',active_language),statedu_t(paste0('result.history_error.',names(paths)[[i]]),active_language)) %in% notices$values)
  }
 })
 stopifnot(identical(original,lapply(paths,function(path)readBin(path,'raw',n=file.info(path)$size))))
 cat('PASS:',language,'five errors; startup warning; actual open notifications; prior results and original files preserved\n')
}
