Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
langs<-c('en','ko','ja','zh','es','fr','de','vi')
jsonlite::write_json(setNames(lapply(langs,function(language)list(updated=statedu_t('data_editor.wide_long_updated_group',language),removed=statedu_t('data_editor.wide_long_removed_group',language))),langs),'tmp/wide-long-group-update-templates.json',auto_unbox=TRUE)
data<-data.frame(id=1:2,x1=c(10,20),x2=c(30,40),y1=c(110,120),y2=c(130,140))
server<-function(input,output,session){
 lang<-reactiveVal('en')
 register_wide_long_handlers(input,output,session,function()data,function()list(name='test.csv'),function()NULL,function()NULL,function(...)TRUE,function()NULL,lang)
}
for(language in langs)shiny::testServer(server,{
 lang(language);session$setInputs(wide_long_move=0,wide_long_set_spec=0,wide_long_remove_spec=0,preview_wide_long=0);session$flushReact()
 configure<-function(prefix,i){
  session$setInputs(wide_long_available=paste0(prefix,1:2),wide_long_move=i)
  session$setInputs(wide_long_value_name='사용자_값',wide_long_index_name='시점',wide_long_index_values='Normality,morning',wide_long_unit_type='different',wide_long_id_variables='id',wide_long_fixed_mode='selected')
  session$setInputs(wide_long_set_spec=i);session$flushReact()
 }
 group_ids<-function(){doc<-xml2::read_html(as.character(output$wide_long_setup$html));xml2::xml_attr(xml2::xml_find_all(doc,'//select[@id="wide_long_configured"]/option'),'value')}
 configure('x',1);original_id<-group_ids();stopifnot(length(original_id)==1L)
 session$setInputs(preview_wide_long=1)
 configure('y',2);stopifnot(identical(group_ids(),original_id))
 message<-xml2::xml_text(xml2::read_html(as.character(output$wide_long_message$html)))
 stopifnot(grepl(sprintf(statedu_t('data_editor.wide_long_updated_group',language),'사용자_값 (2)'),message,fixed=TRUE))
 session$setInputs(wide_long_configured=original_id,wide_long_remove_spec=1);session$flushReact()
 stopifnot(length(group_ids())==0L)
 message<-xml2::xml_text(xml2::read_html(as.character(output$wide_long_message$html)))
 stopifnot(grepl(statedu_t('data_editor.wide_long_removed_group',language),message,fixed=TRUE))
 cat('PASS:',language,'same-name update retains one stable group ID; update/removal status localized\n')
})
