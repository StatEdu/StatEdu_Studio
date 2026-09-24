Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# Stub OS boundaries only; no native window is displayed.
captured <- NULL
chosen <- 'D:/사용자/자료 <&> %s.csv'
choose_windows_save_file <- function(default_name,title,filter,default_ext) {
  captured <<- list(title=title,filter=filter,ext=default_ext); chosen
}
choose_tk_save_file <- function(default_name,title,extension,filetypes) {
  captured <<- list(title=title,filter=filetypes,ext=extension); 'tk.csv'
}
original_chooser <- choose_data_csv_save_path
data <- data.frame('사용자 변수'=c('Normality','morning <&> %s'),check.names=FALSE)
dir.create('tmp/data-csv-dialog',recursive=TRUE,showWarnings=FALSE)
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  choose_data_csv_save_path <- original_chooser
  chosen <- 'D:/사용자/자료 <&> %s.csv'
  stopifnot(identical(choose_data_csv_save_path(lang),chosen),
    captured$title==statedu_t('file_dialog.save_data',lang),captured$ext=='csv',
    captured$filter==sprintf('%s (*.csv)|*.csv|%s (*.*)|*.*',
      statedu_t('file_dialog.csv_file',lang),statedu_t('file_dialog.all_files',lang)))
  chosen <- windows_dialog_cancel_marker
  stopifnot(length(choose_data_csv_save_path(lang))==0L)
  chosen <- character(0)
  stopifnot(choose_data_csv_save_path(lang)=='tk.csv',captured$ext=='.csv',
    captured$title==statedu_t('file_dialog.save_data',lang),
    captured$filter==sprintf('{{%s} {.csv}} {{%s} {*}}',
      statedu_t('file_dialog.csv_file',lang),statedu_t('file_dialog.all_files',lang)))
  # Real CSV write with current-language handoff and a Unicode filename.
  seen_language <- NULL
  destination <- file.path('tmp/data-csv-dialog',paste0(lang,'_사용자 자료'))
  choose_data_csv_save_path <- function(language) { seen_language <<- language; destination }
  result <- save_wide_long_result_file(data,lang)
  stopifnot(result$saved,seen_language==lang,result$path==paste0(destination,'.csv'))
  restored <- readr::read_csv(result$path,show_col_types=FALSE,name_repair='minimal')
  stopifnot(identical(names(restored),names(data)),identical(restored[[1]],data[[1]]))
  destination <- character(0)
  stopifnot(!save_wide_long_result_file(data,lang)$saved,seen_language==lang)
  cat('PASS:',lang,'CSV dialog title/filter/cancel/fallback; language handoff; CSV names and values\n')
}
