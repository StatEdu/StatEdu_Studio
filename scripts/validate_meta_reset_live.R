Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
shiny::testServer(function(input,output,session){
 lang<-reactiveVal('ko');handles<-register_meta_server(input,output,session,function()lang())
},{
 session$setInputs(meta_reset_effects=0,meta_confirm_reset=0)
 rows<-meta_normalize_effect(list(study_id='User 한글 <&>',family='g',input_type='g_se',g=.3,se=.1),row_id=1)
 handles$effects(rows);handles$analysis_result(list(marker='saved result'))
 session$setInputs(meta_reset_effects=1)
 for(language in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
  lang(language);session$flushReact()
  stopifnot(output$meta_reset_title==statedu_t('meta.reset.title',language),output$meta_reset_message==statedu_t('meta.reset.confirm',language),output$meta_reset_cancel==meta_ui_text('cancel',language),identical(handles$effects(),rows),identical(handles$analysis_result(),list(marker='saved result')))
 }
 session$setInputs(meta_confirm_reset=1)
 stopifnot(nrow(handles$effects())==0,is.null(handles$analysis_result()))
 cat('PASS reset live: 8 languages, data/result preserved until explicit confirmation\n')
})
