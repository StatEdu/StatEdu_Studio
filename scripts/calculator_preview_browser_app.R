# Isolated synthetic-data FRS UI; never changes the production dataset/server.
Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data<-data.frame(sex=rep(c(1,2),30),age=rep(45:64,3),Smok=rep(c(0,1),30),HDLc=50,chol=200,HPd=0,SBP=135,DM=0)
ui<-fluidPage(tags$head(tags$style(HTML('.frequencies-setup-grid{display:flex;gap:24px}.analysis-transfer-column,.analysis-options-column{flex:1}.analysis-transfer-controls{display:none}'))),
 h3('Calculator synthetic-data verification'),
 selectInput('test_language','Language',choices=c('en','ko','ja','zh','es','fr','de','vi'),selected='en'),
 actionButton('test_fill','Fill test variables'),actionButton('run_frs_calculator','Calculate'),
 textOutput('frs_loaded_message'),uiOutput('frs_calculator_setup'),uiOutput('frs_calculator_summary'),DT::DTOutput('frs_calculator_preview'))
server<-function(input,output,session){
 register_frs_calculator_handlers(input,output,session,function()data,function()list(name='synthetic.csv'),function()NULL,function(...)NULL,language_fn=function()input$test_language)
 observeEvent(input$test_fill,{for(id in names(data))updateSelectInput(session,paste0('frs_',id),selected=id)})
}
shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=7903,launch.browser=FALSE)
