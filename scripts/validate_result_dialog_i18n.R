Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# Dialog-only changes: capture OS calls without opening native windows or writing reports.
enabled <- TRUE
analysis_save_feature_enabled <- function(format) enabled
captured <- NULL
chosen <- 'D:/사용자/결과 <&> %s'
choose_windows_save_file <- function(default_name,title,filter,default_ext) {
 captured <<- list(name=default_name,title=title,filter=filter,ext=default_ext);chosen
}
choose_tk_save_file <- function(default_name,title,extension,filetypes) {
 captured <<- list(name=default_name,title=title,filter=filetypes,ext=extension);'fallback'
}
formats <- c(excel='xlsx',html='html',pdf='pdf',word='docx',hwpx='hwpx')
labels <- c(excel='Excel',html='HTML',pdf='PDF',word='Word',hwpx='HWPX')
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 options(statedu.app_language=lang)
 for(fmt in names(formats)) {
  chooser <- get(paste0('choose_',fmt,'_save_path'))
  extension <- formats[[fmt]]
  title <- sprintf(statedu_t('file_dialog.save_results_format',lang),labels[[fmt]])
  file_label <- sprintf(statedu_t('file_dialog.format_file',lang),labels[[fmt]])
  all_label <- statedu_t('file_dialog.all_files',lang)
  chosen <- 'D:/사용자/결과 <&> %s'
  stopifnot(identical(chooser(),chosen),captured$title==title,captured$ext==extension,
   endsWith(captured$name,paste0('.',extension)),
   captured$filter==sprintf('%s (*.%s)|*.%s|%s (*.*)|*.*',file_label,extension,extension,all_label))
  chosen <- windows_dialog_cancel_marker
  stopifnot(length(chooser())==0L)
  chosen <- character(0)
  stopifnot(chooser()=='fallback',captured$title==title,captured$ext==paste0('.',extension),
   captured$filter==sprintf('{{%s} {.%s}} {{%s} {*}}',file_label,extension,all_label))
 }
 cat('PASS:',lang,'five result save dialogs; title/filter/extension/path/cancel/Tk fallback\n')
}
enabled <- FALSE
for(fmt in c('excel','pdf','word')) stopifnot(length(get(paste0('choose_',fmt,'_save_path'))())==0L)
cat('PASS: existing save feature gates preserved\n')
