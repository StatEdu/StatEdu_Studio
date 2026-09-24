Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
notices<-new.env();notices$values<-character()
showNotification<-function(ui,...) {notices$values<-c(notices$values,as.character(ui));invisible('test')}
keys<-paste0('result.export_error.',c('no_content','no_tables','excel_package','browser','pdf_path','office_browser','pdf_failed'))
details<-'Tool: D:/사용자 & %s/report.pdf\ncode=17 <detail>'
messages<-c(vapply(keys,function(key)statedu_t(key,'en'),character(1)),paste0('PDF export failed.\n',details),paste0('External: ',details))
snapshot<-'<p>Estimate 1.230 사용자 변수 &amp; label</p>'
injected<-messages[[1]];writer_calls<-0L
choose_pdf_save_path<-function()file.path(tempdir(),'current.pdf')
choose_excel_save_path<-function()file.path(tempdir(),'current.xlsx')
write_pdf_from_html<-function(html,file,...) {writer_calls<<-writer_calls+1L;stop(injected)}
save_screen_excel_file<-function(html,file,...) {writer_calls<<-writer_calls+1L;stop(injected)}
shiny::testServer(function(input,output,session) {
 language<-reactiveVal('en')
 result<-reactiveVal(list(user='사용자',estimate=1.23))
 register_canvas_report_exports(input,session,'save_html','save_pdf','model_results',NULL,
  function()'사용자 제목',result,language,excel_id='save_excel')
}, {
 session$flushReact();nonce<-0L
 for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
  language(lang);session$flushReact()
  expected<-c(vapply(keys,function(key)statedu_t(key,lang),character(1)),paste0(statedu_t('result.export_error.pdf_failed',lang),'\n',details),paste0('External: ',details))
  for(i in seq_along(messages)) {
   injected<<-messages[[i]]
   for(format in c('html','pdf','excel')) {
    nonce<-nonce+1L;notices$values<-character()
    # HTML exercises capture failures; PDF/Excel exercise writer failures.
    payload<-list(html=snapshot,nonce=nonce,error=if(format=='html')injected else '')
    do.call(session$setInputs,setNames(list(payload),paste0('save_',format,'_snapshot')))
    stopifnot(unname(expected[[i]]) %in% notices$values,
     identical(result(),list(user='사용자',estimate=1.23)))
   }
  }
  cat('PASS:',lang,'shared current-result capture/writer error notifications; external details and result state preserved\n')
 }
 stopifnot(writer_calls==length(messages)*2L*9L)
})
