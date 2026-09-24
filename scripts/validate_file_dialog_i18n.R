Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# Capture only the OS boundary: these tests do not show a native dialog.
real_windows_open <- windows_open_file_dialog
real_windows_save <- windows_save_file_dialog
captured <- NULL
chosen <- 'D:/테스트/Review <&> %s.csv'
open_file_dialog <- function(title,filetypes){captured <<- list(title=title,filetypes=filetypes);chosen}
windows_save_file_dialog <- function(title,filters,initial_dir,default_ext){captured <<- list(title=title,filters=filters,initial_dir=initial_dir,default_ext=default_ext);list(attempted=TRUE,path=chosen)}
windows_select_directory_dialog <- function(title,initial_dir){captured <<- list(title=title,initial_dir=initial_dir);list(attempted=TRUE,path=chosen)}
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 chosen <- 'D:/테스트/Review <&> %s.studio'
 stopifnot(identical(open_settings_file(lang),chosen),captured$title==statedu_t('file_dialog.open_settings',lang))
 filters <- attr(captured$filetypes,'windows_filters')
 stopifnot(filters[1,1]==statedu_t('file_dialog.settings_files',lang),filters[1,2]=='*.studio',grepl(filters[1,1],captured$filetypes,fixed=TRUE))
 chosen <- 'D:/테스트/Review <&> %s.csv'
 stopifnot(identical(open_data_file(lang),chosen),captured$title==statedu_t('file_dialog.open_data',lang))
 filters <- attr(captured$filetypes,'windows_filters')
 stopifnot(filters[1,1]==statedu_t('file_dialog.data_files',lang),filters[8,1]==statedu_t('file_dialog.all_files',lang),identical(unname(filters[,2]),c('*.sav;*.sas7bdat;*.xpt;*.dta;*.xlsx;*.xls;*.csv;*.dat','*.sav','*.sas7bdat;*.xpt','*.dta','*.xlsx;*.xls','*.csv','*.dat','*.*')))
 chosen <- 'D:/테스트/Review <&> %s'
 stopifnot(save_settings_file(language=lang)==paste0(chosen,'.studio'),captured$title==statedu_t('file_dialog.save_settings',lang),captured$filters[1,1]==statedu_t('file_dialog.settings_files',lang),captured$default_ext=='studio')
 chosen <- 'D:/테스트/Review <&> %s.stdesign'
 stopifnot(identical(open_complex_sample_design_file(lang),chosen),captured$title==statedu_t('file_dialog.open_design',lang))
 filters <- attr(captured$filetypes,'windows_filters')
 stopifnot(filters[1,1]==statedu_t('file_dialog.design_files',lang),filters[1,2]=='*.stdesign',grepl(filters[1,1],captured$filetypes,fixed=TRUE))
 chosen <- 'D:/테스트/Review <&> %s.studio'
 stopifnot(save_complex_sample_design_file(language=lang)=='D:/테스트/Review <&> %s.stdesign',captured$title==statedu_t('file_dialog.save_design',lang),captured$filters[1,1]==statedu_t('file_dialog.design_files',lang),captured$default_ext=='stdesign')
 chosen <- normalizePath('tmp',winslash='/',mustWork=TRUE)
 stopifnot(choose_default_save_dir(chosen,language=lang)==chosen,captured$title==statedu_t('file_dialog.choose_default_folder',lang),captured$initial_dir==chosen)
 chosen <- NULL;stopifnot(is.null(open_settings_file(lang)),is.null(save_settings_file(language=lang)),is.null(open_complex_sample_design_file(lang)),is.null(save_complex_sample_design_file(language=lang)),is.null(choose_default_save_dir(language=lang)))
 # The original Windows helpers must quote localized strings and preserve cancellation.
 script <- NULL
 run_windows_open_dialog_script <- function(value){script <<- value;open_dialog_cancel_marker}
 title <- paste(statedu_t('file_dialog.open_settings',lang),"Review's <&> %s")
 result <- real_windows_open(title,matrix(c(statedu_t('file_dialog.settings_files',lang),'*.studio'),ncol=2))
 stopifnot(result$attempted,is.null(result$path),grepl(open_dialog_ps_quote(title),script,fixed=TRUE))
 result <- real_windows_save(title,matrix(c(statedu_t('file_dialog.settings_files',lang),'*.studio'),ncol=2),default_ext='studio')
 stopifnot(result$attempted,is.null(result$path),grepl(open_dialog_ps_quote(title),script,fixed=TRUE))
 cat('PASS:',lang,'settings/data/design/folder dialog arguments; filters/extensions; Unicode path; cancel; Windows quoting\n')
}
