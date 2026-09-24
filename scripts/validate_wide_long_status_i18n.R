Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
jsonlite::write_json(setNames(lapply(c('en','ko','ja','zh','es','fr','de','vi'),function(language)statedu_t('data_editor.wide_long_previewed',language)),c('en','ko','ja','zh','es','fr','de','vi')),'tmp/wide-long-status-templates.json',auto_unbox=TRUE)
data<-data.frame(id=1:2,x1=c(10,20),x2=c(30,40))
records<-new.env();records$count<-0L;records$saved<-TRUE;records$path<-'D:/사용자 <&> %s/Normality.csv'
# Isolate the OS save boundary; no native dialog or real file is touched.
save_wide_long_result_file<-function(data, language)list(saved=records$saved,path=if(records$saved)records$path else '')
server<-function(input,output,session) {
 lang<-reactiveVal('en')
 register_wide_long_handlers(input,output,session,function()data,function()list(name='test.csv'),function()NULL,function()NULL,
 function(data,...){records$count<-records$count+1L;records$data<-data;TRUE},function()NULL,lang)
}
shiny::testServer(server, {
 session$setInputs(preview_wide_long=0,run_wide_long=0,wide_long_set_spec=0,wide_long_move=0);session$flushReact()
 session$setInputs(wide_long_available=c('x1','x2'),wide_long_move=1)
 session$setInputs(wide_long_value_name='사용자_값',wide_long_index_name='사용자_시점',wide_long_index_values='Normality,morning',wide_long_unit_type='different',wide_long_id_variables='id',wide_long_fixed_mode='all')
 session$setInputs(wide_long_set_spec=1)
 check_status<-function(key,values,count) {
  for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
   lang(language);session$flushReact()
   html<-output$wide_long_message$html
   actual<-xml2::xml_text(xml2::read_html(as.character(html)))
   expected<-do.call(sprintf,c(list(statedu_t(key,language)),values))
   stopifnot(grepl(expected,actual,fixed=TRUE),records$count==count)
  }
 }
 check_status('data_editor.wide_long_set_group',list('사용자_값 (2)'),0L)
 session$setInputs(preview_wide_long=1)
 check_status('data_editor.wide_long_previewed',list(4,3,1),0L)
 session$setInputs(run_wide_long=1)
 check_status('data_editor.wide_long_saved_connected',list(4,3,records$path),1L)
 stopifnot(setequal(records$data[['사용자_시점']],c('Normality','morning')))
 records$saved<-FALSE;session$setInputs(run_wide_long=2)
 check_status('data_editor.wide_long_temp_connected',list(4,3),2L)
})
cat('PASS: 8-language group/preview/saved/temporary status; user path and indicators preserved; language changes never repeat replacement\n')
