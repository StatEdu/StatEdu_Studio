Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(id=rep(1:10,each=3),time=rep(0:2,10),y=1:30,x=31:60,aux=61:90,aux2=91:120,wt=rep(c(1,2,3),10))
info<-data.frame(name=names(d),var_label=names(d),measurement='continuous')
initial<-normalize_longitudinal_settings(list(variables=list(outcome='y',id='id',time='time',predictors='x'),options=list(model_type='gee',missing_strategy='mi')))
ui<-fluidPage(selectInput('test_language','Language',c('ko','en','ja','zh','es','fr','de','vi'),'ko'),selectInput('test_dataset','Dataset',c('full','reduced','empty'),'full'),uiOutput('longitudinal_setup'),verbatimTextOutput('test_settings'))
server<-function(input,output,session) {
 restore<-reactiveVal(list(settings=initial))
 current_data<-reactive(switch(input$test_dataset, reduced=d[c('id','time','y','aux2')], empty=d[character()], d))
 module<-register_longitudinal_handlers(input,output,session,function()names(current_data()),current_data,function()info[info$name %in% names(current_data()),],function()character(),function()NULL,function(){},app_language_fn=function()input$test_language,restore_request_fn=restore)
 output$test_settings<-renderText(jsonlite::toJSON(module$settings(),auto_unbox=TRUE))
}
shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=43921,launch.browser=FALSE)
