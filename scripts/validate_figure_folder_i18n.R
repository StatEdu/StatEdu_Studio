Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
root<-tempfile('figure-folder-');dir.create(root)
chosen<-file.path(root,'사용자 & %s');dir.create(chosen)
captured<-list();route<-'windows'
native<-choose_windows_directory
capture<-function(which,caption) {
 captured[[length(captured)+1L]]<<-list(route=which,caption=caption)
 if(route==which)chosen else character(0)
}
choose_windows_directory<-function(caption) {
 if(route=='cancel')return(windows_dialog_cancel_marker)
 capture('windows',caption)
}
assignInNamespace('choose.dir',function(default,caption)capture('utils',caption),ns='utils')
choose_rstudio_directory<-function(caption)capture('rstudio',caption)
stopifnot(requireNamespace('tcltk',quietly=TRUE))
assignInNamespace('tk_choose.dir',function(default,caption)capture('tk',caption),ns='tcltk')
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 options(statedu.app_language=lang)
 for(which in c('windows','utils','rstudio','tk')) {
  route<-which;captured<-list()
  stopifnot(identical(choose_figure_save_dir(),chosen),getOption('statedu.app_language')==lang,
   all(vapply(captured,function(x)x$caption==statedu_t('file_dialog.figure_folder',lang),logical(1))),
   tail(captured,1)[[1]]$route==which)
 }
 route<-'cancel';captured<-list()
 stopifnot(length(choose_figure_save_dir())==0L,length(captured)==0L)
 route<-'none';stopifnot(length(choose_figure_save_dir())==0L)
 route<-'windows';captured<-list();options(statedu.app_language='en')
 stopifnot(choose_figure_save_dir(lang)==chosen,captured[[1]]$caption==statedu_t('file_dialog.figure_folder',lang),getOption('statedu.app_language')=='en')
 # Inspect the real Windows helper's generated script; no window is displayed.
 script<-NULL
 run_windows_dialog_script<-function(value){script<<-value;windows_dialog_cancel_marker}
 title<-paste(statedu_t('file_dialog.figure_folder',lang),"Review's & %s")
 stopifnot(native(title)==windows_dialog_cancel_marker,grepl(ps_quote(title),script,fixed=TRUE),
  grepl('FolderBrowserDialog',script,fixed=TRUE))
 cat('PASS:',lang,'folder title across four boundaries; cancel; Unicode path; explicit language; Windows quoting\n')
}
