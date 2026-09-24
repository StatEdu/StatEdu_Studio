# Synthetic-data UI for reference selection; no production session access.
Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
eq<-setNames(as.data.frame(matrix(1:3,6,5)),paste0('item',1:5))
metabolic<-data.frame(sex=c(1,2),wc=c(95,81),glu=c(99,100),DMd=0,SBP=c(120,130),DBP=80,HPd=0,HDLc=c(45,49),TG=c(151,149))
ui<-fluidPage(tags$head(tags$style(HTML('.frequencies-setup-grid{display:flex;gap:20px}.analysis-transfer-column,.analysis-options-column{flex:1}.analysis-transfer-controls,.metabolic-reference-hidden{display:none}'))),
 h3('Reference UI synthetic-data verification'),selectInput('test_language','Language',choices=c('en','ko','ja','zh','es','fr','de','vi'),selected='en'),
 tabsetPanel(id='test_kind',tabPanel('EQ-5D',uiOutput('eq5d_calculator_setup')),tabPanel('Metabolic',uiOutput('metabolic_calculator_setup'))))
server<-function(input,output,session){
 register_eq5d_calculator_handlers(input,output,session,function()eq,function()list(name='eq.csv'),function()NULL,function(...)NULL,language_fn=function()input$test_language)
 register_metabolic_calculator_handlers(input,output,session,function()metabolic,function()list(name='metabolic.csv'),function()NULL,function(...)NULL,language_fn=function()input$test_language)
}
shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=7903,launch.browser=FALSE)
