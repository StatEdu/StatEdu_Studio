Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
save_wide_long_result_file<-function(data, language)list(saved=FALSE,path='')
server<-function(input,output,session) {
 lang<-reactiveVal('en');data<-reactiveVal(data.frame(id=1:2,x1=c(10,20),x2=c(30,40)))
 file<-reactiveVal(list(name='same.csv',path='synthetic/a'))
 register_wide_long_handlers(input,output,session,data,file,function()NULL,function()NULL,
 function(value,name,path,...){data(value);file(list(name=name,path=path));TRUE},function()NULL,lang)
}
for(language in c('en','ko','ja','zh','es','fr','de','vi'))shiny::testServer(server, {
 lang(language)
 session$setInputs(preview_wide_long=0,run_wide_long=0,wide_long_move=0,wide_long_set_spec=0);session$flushReact()
 configure<-function(i){
  session$setInputs(wide_long_available=c('x1','x2'),wide_long_move=i)
  session$setInputs(wide_long_value_name='사용자_값',wide_long_index_name='시점',wide_long_index_values='Normality,morning',wide_long_unit_type='different',wide_long_id_variables='id',wide_long_fixed_mode='all')
  session$setInputs(wide_long_set_spec=i,preview_wide_long=i);session$flushReact()
  stopifnot(!is.null(output$wide_long_message))
 }
 group_count<-function(){doc<-xml2::read_html(as.character(output$wide_long_setup$html));length(xml2::xml_find_all(doc,'//select[@id="wide_long_configured"]/option'))}
 configure(1);stopifnot(group_count()==1L)
 data(data.frame(id=1:2,x1=c(110,120),x2=c(130,140)));session$flushReact()
 stopifnot(group_count()==0L,is.null(output$wide_long_message))
 configure(2)
 file(list(name='same.csv',path='synthetic/b'));session$flushReact()
 stopifnot(group_count()==0L,is.null(output$wide_long_message))
 configure(3);session$setInputs(run_wide_long=1);session$flushReact()
 # The command's own replacement must keep its completion message/result.
 stopifnot(nrow(data())==4L,!is.null(output$wide_long_message))
 html<-xml2::xml_text(xml2::read_html(as.character(output$wide_long_message$html)))
 stopifnot(grepl(sprintf(statedu_t('data_editor.wide_long_temp_connected',language),4,3),html,fixed=TRUE))
 cat('PASS:',language,'same-name changed data, new source path reset; own conversion completion retained\n')
})
