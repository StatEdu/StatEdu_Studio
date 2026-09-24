Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
root <- tempfile('history-flow-');dir.create(root)
Sys.setenv(STATEDU_RESULT_STORE=file.path(root,'session.json'))
entry <- list(id='user-id',title='사용자 <&> %s 結果',saved_at='2026-09-17',html='<p>Estimate 1.230 사용자 변수</p>')
notices <- new.env();notices$values<-character()
showNotification <- function(ui,...) {notices$values<-c(notices$values,as.character(ui));invisible('test')}
selected_path <- character()
choose_result_history_save_path <- function(language) {stopifnot(language==active_language);selected_path}
choose_result_history_open_path <- function(language) {stopifnot(language==active_language);selected_path}
empty_path <- file.path(root,'empty.efs-result');stopifnot(write_result_snapshot_store(list(),empty_path))
assert_notice <- function(value) stopifnot(value %in% notices$values)
shiny::testServer(function(input,output,session) {
 language<-reactiveVal('en');store<-result_accumulator_store(session)
 register_result_accumulator_outputs(input,output,session,language)
}, {
 session$setInputs(save_result_history_dialog=0,open_result_history_dialog=0,clear_saved_results=0)
 session$flushReact()
 n<-0L
 for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
  active_language <<- lang;language(lang);store(list());session$flushReact()
  doc <- xml2::read_html(output$saved_results_list$html)
  stopifnot(trimws(xml2::xml_text(xml2::xml_find_first(doc,'//div[@class="empty-message"]')))==statedu_t('result.empty_message',lang))
  notices$values<-character();n<-n+1L
  session$setInputs(save_result_history_dialog=n)
  assert_notice(paste(statedu_t('result.save_failed',lang),statedu_t('result.no_saved_results',lang)))
  store(list(entry));session$flushReact()
  selected_path <<- character();notices$values<-character();n<-n+1L
  session$setInputs(save_result_history_dialog=n,open_result_history_dialog=n)
  assert_notice(statedu_t('result.save_dialog_canceled',lang));assert_notice(statedu_t('result.open_dialog_canceled',lang))
  stopifnot(identical(store(),list(entry)))
  base_path <- file.path(root,paste0(lang,'_사용자 & %s'))
  selected_path <<- base_path;notices$values<-character();n<-n+1L
  session$setInputs(save_result_history_dialog=n)
  saved_path <- paste0(base_path,'.efs-result')
  assert_notice(sprintf(statedu_t('result.saved_path',lang),saved_path))
  stopifnot(identical(read_result_snapshot_store(saved_path),list(entry)))
  selected_path <<- empty_path;notices$values<-character();n<-n+1L
  session$setInputs(open_result_history_dialog=n)
  assert_notice(paste(statedu_t('result.open_failed',lang),statedu_t('result.file_empty',lang)))
  stopifnot(identical(store(),list(entry)))
  notices$values<-character();session$setInputs(clear_saved_results=n)
  assert_notice(statedu_t('result.cleared',lang));stopifnot(length(store())==0L,length(read_result_snapshot_store())==0L)
  selected_path <<- saved_path;notices$values<-character();n<-n+1L
  session$setInputs(open_result_history_dialog=n)
  assert_notice(sprintf(statedu_t('result.opened_path',lang),saved_path))
  stopifnot(identical(store(),list(entry)),identical(read_result_snapshot_store(),list(entry)))
  cat('PASS:',lang,'empty UI, no-results save, cancel, save/load, empty-file rejection, clear persistence; Unicode path and snapshot preserved\n')
 }
})
