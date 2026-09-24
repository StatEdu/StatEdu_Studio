Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# OS boundaries are mocked; snapshot serialization below uses real files.
captured <- NULL
chosen <- 'D:/사용자/결과 <&> %s.efs-result'
choose_windows_save_file <- function(default_name,title,filter,default_ext) {
 captured <<- list(title=title,filter=filter,ext=default_ext);chosen
}
choose_windows_open_file <- function(title,filter) {
 captured <<- list(title=title,filter=filter);chosen
}
choose_tk_save_file <- function(default_name,title,extension,filetypes) {
 captured <<- list(title=title,filter=filetypes,ext=extension);'fallback'
}
choose_tk_open_file <- function(title,filetypes) {
 captured <<- list(title=title,filter=filetypes);'fallback'
}
dir.create('tmp/history-dialog',recursive=TRUE,showWarnings=FALSE)
entry <- list(id='history_language_test',title='사용자 결과 <&> %s',
 saved_at='2026-09-17 12:00:00',html='<html><body><table><tr><th>Estimate</th><td>사용자 변수</td><td>1.230</td></tr></table><p>注記 &amp; note</p></body></html>')
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 options(statedu.app_language=lang)
 history_label <- statedu_t('file_dialog.history_file',lang)
 json_label <- sprintf(statedu_t('file_dialog.format_file',lang),'JSON')
 all_label <- statedu_t('file_dialog.all_files',lang)
 for(action in c('save','open')) {
  chooser <- get(paste0('choose_result_history_',action,'_path'))
  title <- statedu_t(paste0('file_dialog.',action,'_history'),lang)
  chosen <- 'D:/사용자/결과 <&> %s.efs-result'
  stopifnot(identical(chooser(),chosen),captured$title==title,
   captured$filter==sprintf('%s (*.efs-result)|*.efs-result|%s (*.json)|*.json|%s (*.*)|*.*',history_label,json_label,all_label))
  if(action=='save') stopifnot(captured$ext=='efs-result')
  chosen <- windows_dialog_cancel_marker
  stopifnot(length(chooser())==0L)
  chosen <- character(0)
  stopifnot(chooser()=='fallback',captured$title==title,
   captured$filter==sprintf('{{%s} {.efs-result}} {{%s} {.json}} {{%s} {*}}',history_label,json_label,all_label))
 }
 for(extension in c('efs-result','json')) {
  path <- file.path('tmp/history-dialog',paste0(lang,'_사용자.',extension))
  stopifnot(isTRUE(write_result_snapshot_store(list(entry),path)))
  restored <- read_result_snapshot_store(path)
  stopifnot(length(restored)==1L,restored[[1]]$title==entry$title,restored[[1]]$html==entry$html,restored[[1]]$id==entry$id)
 }
 cat('PASS:',lang,'history save/open arguments, cancel, Tk fallback; efs-result/JSON snapshot preservation\n')
}
