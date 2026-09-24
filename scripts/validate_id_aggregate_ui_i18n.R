Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- data.frame(id=c('사용자 <&> %s','사용자 <&> %s','Normality'),Review=c(2,4,9),check.names=FALSE)
result <- id_aggregate_dataset(data,'id',value_variable='Review',stat='mean',output_name='Review')
stopifnot(identical(result$id,c('사용자 <&> %s','Normality')),identical(result$Review,c(3,9)))
server <- function(input,output,session){
 lang<-reactiveVal('ko');records<-new.env();records$count<-0L;records$data<-NULL
 register_id_aggregate_handlers(input,output,session,function()data,function(value,...){records$count<-records$count+1L;records$data<-value;TRUE},function()NULL,language_fn=lang)
}
html_text <- function(out)xml2::xml_text(xml2::read_html(out$html,encoding='UTF-8'))
shiny::testServer(server,{
 session$setInputs(preview_id_aggregate=0,run_id_aggregate=0);session$flushReact()
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  lang(language);session$flushReact()
  doc<-xml2::read_html(output$id_aggregate_setup$html,encoding='UTF-8')
  options<-xml2::xml_find_all(doc,'//select[@id="id_aggregate_stat"]/option')
  codes<-c('sum','mean','median','sd','var','min','max','n')
  stopifnot(identical(xml2::xml_attr(options,'value'),codes),identical(xml2::xml_text(options),vapply(codes,function(k)statedu_t(paste0('id_aggregate.stat.',k),language),character(1),USE.NAMES=FALSE)))
  stopifnot(grepl(statedu_t('id_aggregate.output_name',language),html_text(output$id_aggregate_setup),fixed=TRUE))
  panel<-as.character(data_editor_id_aggregate_panel(language));stopifnot(grepl(statedu_t('id_aggregate.subtitle',language),panel,fixed=TRUE))
 }
 session$setInputs(id_aggregate_id='id',id_aggregate_value='Review',id_aggregate_stat='mean',id_aggregate_condition='',id_aggregate_output_name='Review',id_aggregate_empty='NA',preview_id_aggregate=1)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  lang(language);session$flushReact();text<-html_text(output$id_aggregate_message);stopifnot(length(text)==1L,grepl(sprintf(statedu_t('id_aggregate.preview_created',language),2),text,fixed=TRUE),records$count==0L)
 }
 session$setInputs(run_id_aggregate=1)
 stopifnot(identical(records$data,result),records$count==1L)
 for(language in c('en','ko','ja','zh','es','fr','de','vi')){
  lang(language);session$flushReact();text<-html_text(output$id_aggregate_message);stopifnot(length(text)==1L,grepl(sprintf(statedu_t('id_aggregate.loaded',language),2,2),text,fixed=TRUE),records$count==1L)
  cat('PASS:',language,'statistic labels and stable codes; status rerenders without replacing data; user strings and values preserved\n')
 }
})
