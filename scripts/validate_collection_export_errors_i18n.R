Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false',STATEDU_RESULT_STORE=tempfile(fileext='.json'))
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
keys<-paste0('result.export_error.',c('no_content','no_tables','excel_package','browser','pdf_path','office_browser','pdf_failed'))
diagnostics<-'Tool: D:/사용자 & %s/report.pdf\ncode=17 <details>\n'
errors<-lapply(keys,function(key)simpleError(statedu_t(key,'en')))
errors[[8]]<-simpleError(paste0('PDF export failed.\n',diagnostics))
errors[[9]]<-simpleError(paste0('External writer: ',diagnostics))
notices<-new.env();notices$values<-character()
showNotification<-function(ui,...) {notices$values<-c(notices$values,as.character(ui));invisible('test')}
analysis_save_feature_enabled<-function(...)TRUE
for(format in c('html','pdf','excel','word'))assign(paste0('choose_',format,'_save_path'),function()file.path(tempdir(),'result'))
injected<-errors[[1]]
# Inject failures at writer boundaries: no report generation is claimed by this test.
fail_writer<-function(...)stop(injected)
write_result_collection_html<-fail_writer;write_result_collection_pdf<-fail_writer
save_result_collection_excel_file<-fail_writer;write_result_collection_docx<-fail_writer
# Word now saves through the document-content selection dialog and cache.
# Keep this an injected writer-boundary test; exercise the confirmation event.
result_document_export_cache<-function(...)list(save=fail_writer,clear=function(...)invisible(NULL))
entries<-list(list(id='keep',title='사용자 <&> %s',saved_at='2026-09-17',html='<p>Estimate 1.230 사용자 변수</p>'))
shiny::testServer(function(input,output,session) {
 language<-reactiveVal('en');store<-result_accumulator_store(session);store(entries)
 register_result_accumulator_outputs(input,output,session,language)
}, {
 session$flushReact();nonce<-0L
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  language(lang);session$flushReact()
  expected<-c(vapply(keys,function(key)statedu_t(key,lang),character(1)),
   paste0(statedu_t('result.export_error.pdf_failed',lang),'\n',diagnostics),conditionMessage(errors[[9]]))
  for(i in seq_along(errors)) {
   injected<<-errors[[i]]
   stopifnot(identical(result_export_error_text(injected,lang),unname(expected[[i]])))
   for(format in c('html','pdf','excel','word')) {
    notices$values<-character();nonce<-nonce+1L
    do.call(session$setInputs,setNames(list(nonce),paste0('save_result_collection_',format,'_dialog')))
    if(format=='word')session$setInputs(result_document_contents=c('main','appendix','explanations','figures'),confirm_document_export=nonce)
    stopifnot(paste(statedu_t('result.collection_save_failed',lang),expected[[i]]) %in% notices$values,
     identical(store(),entries))
   }
  }
  cat('PASS:',lang,'7 owned errors + PDF diagnostics + external error; four collection export notifications; snapshots preserved\n')
 }
})
