Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
source_dir<-normalizePath('tmp/canvas-export-validation',winslash='/')
file<-list(path=file.path(tempdir(),'upload.csv'),original_path=file.path(source_dir,'한글 데이터.csv'))
shiny::testServer(function(input,output,session){
 session$userData$result_data_file<-function()file
},{
 stopifnot(identical(result_save_directory(),source_dir))
 captured<-NULL
 original<-run_windows_dialog_script
 assign('run_windows_dialog_script',function(script){captured<<-script;windows_dialog_cancel_marker},envir=.GlobalEnv)
 tryCatch({
  for(fn in c('choose_figure_save_dir','choose_html_save_path','choose_pdf_save_path','choose_word_save_path','choose_hwpx_save_path','choose_excel_save_path')){
   stopifnot(!length(get(fn)()))
   expected<-if(fn=='choose_figure_save_dir')normalizePath(source_dir,winslash='\\') else source_dir
   stopifnot(grepl(ps_quote(expected),captured,fixed=TRUE))
   cat('PASS',fn,'source folder and cancellation\n')
  }
 },finally=assign('run_windows_dialog_script',original,envir=.GlobalEnv))
 file$original_path<-'';file$path<-file.path(source_dir,'restored.csv')
 stopifnot(identical(result_save_directory(),source_dir))
 file$path<-file.path(tempdir(),'upload.csv')
 stopifnot(!identical(result_save_directory(),normalizePath(tempdir(),winslash='/')))
})
entries<-lapply(c('mm','cfa','sem','pls'),function(menu)list(id=menu,title=menu,html=paste(readLines(file.path(source_dir,paste0(menu,'-cropped.html')),encoding='UTF-8'),collapse='\n')))
out<-'tmp/canvas-crop-exports';dir.create(out,showWarnings=FALSE)
for(mode in c('current','accumulated')){
 items<-entries
 if(mode=='accumulated')items<-c(list(list(id='prior',title='Prior',html='<p>Earlier result preserved.</p>')),items)
 stem<-file.path(out,mode)
 write_result_collection_html(items,paste0(stem,'.html'))
 write_result_collection_docx(items,paste0(stem,'.docx'))
 save_result_collection_excel_file(items,paste0(stem,'.xlsx'))
 write_result_collection_pdf(items,paste0(stem,'.pdf'))
 write_result_collection_hwpx(items,paste0(stem,'.hwpx'))
 cat('PASS',mode,'all five exports written\n')
}
