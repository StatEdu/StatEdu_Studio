Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
data <- data.frame(time=1:4,event=c(0,1,2,1),x=2:5)
fixture <- survival_settings_defaults()
fixture$design <- modifyList(fixture$design,list(survival_design_objective='competing',survival_design_events='competing',survival_design_estimand='both',survival_contract_origin='Review 수술일 <&> %s',survival_contract_unit='other',survival_contract_custom_unit='사용자 주기 <&> %s',survival_contract_time='time',survival_contract_event='event',survival_contract_covariates='x',map_values=c('0','1','2'),map_roles=c('censored','event_of_interest','competing_event'),map_labels=c('None','Normality','사용자 Review <&> %s'),map_confirmed=TRUE,dataset_hash=digest::digest(data,algo='sha256')))
write_settings_json_file(list(survival=fixture),'tmp/survival-design-persistence.studio')
loaded <- normalize_survival_settings(read_settings_json_file('tmp/survival-design-persistence.studio')$survival)
stopifnot(identical(fixture,loaded))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
 server <- function(input,output,session){
  dat <- reactiveVal(data);request <- reactiveVal(list(revision=1,settings=loaded))
  api <- register_survival_handlers(input,output,session,function()names(dat()),dat,function()NULL,function()character(),function()data.frame(),function()NULL,app_language_fn=function()lang,restore_request_fn=request)
 }
 shiny::testServer(server,{
  session$flushReact();stopifnot(identical(isolate(api$settings()$design),fixture$design))
  session$setInputs(survival_contract_origin='edited');stopifnot(isolate(api$settings()$design$survival_contract_origin)=='edited')
  request(list(revision=2,settings=loaded));session$flushReact();stopifnot(identical(isolate(api$settings()$design),fixture$design))
  altered<-data;altered$time<-11:14;dat(altered);session$flushReact();stopifnot(!isolate(api$settings()$design$map_confirmed))
  request(list(revision=3,settings=loaded));session$flushReact();stopifnot(!isolate(api$settings()$design$map_confirmed))
 });cat('PASS:',lang,'design file roundtrip; hot restore; changed data rejects saved confirmation\n')
}
if('--serve' %in% commandArgs(TRUE)){
 ui<-fluidPage(selectInput('language','Language',c('en','ko','ja','zh','es','fr','de','vi'),'ko'),actionButton('restore','Restore saved fixture'),actionButton('change_data','Change data'),uiOutput('design'),verbatimTextOutput('snapshot'))
 server<-function(input,output,session){
  dat<-reactiveVal(data);request<-reactiveVal(NULL);session$userData$survival_design_revision<-reactiveVal(0L)
  api<-register_survival_handlers(input,output,session,function()names(dat()),dat,function()NULL,function()character(),function()data.frame(),function()NULL,app_language_fn=function()input$language,restore_request_fn=request)
  output$design<-renderUI({session$userData$survival_design_revision();tab_panel_content(survival_setup_tab_panel(input$language,session$userData$survival_design_values %||% list()))})
  observeEvent(input$restore,{request(list(revision=input$restore,settings=loaded))})
  observeEvent(input$change_data,{changed<-data;changed$time<-11:14;dat(changed)})
  output$snapshot<-renderText(jsonlite::toJSON(api$settings()$design,auto_unbox=TRUE))
 };shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=43874,launch.browser=FALSE)
}
