Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(916);d<-data.frame(q7=rep(c('일반병동','특수병동'),each=60),X=rnorm(120),C=rnorm(120));d$M<-.6*d$X+rnorm(120);d$Y<-ifelse(d$q7=='일반병동',.2,1.2)*d$X+.8*d$M+.2*d$C+rnorm(120)
info<-data.frame(name=names(d),measurement=c('binary',rep('continuous',4)),var_label=names(d))
shiny::addResourcePath('qa-assets',normalizePath('www'))
assets<-paste(gsub('href="','href="qa-assets/',as.character(app_stylesheet_link('qa')),fixed=TRUE),gsub('src="','src="qa-assets/',as.character(app_script_link('qa')),fixed=TRUE))
ui<-tagList(tags$head(HTML(assets)),navbarPage('Canvas QA',tabPanel('Cases',analysis_scope_panel('cases','ko')),tabPanel('Split',analysis_scope_panel('split','ko')),custom_model_canvas_tab_panel('Model','ko')))
server<-function(input,output,session){
 scope<-register_analysis_scope(input,output,session,function()d,function()'ko',function()'fixture',variable_info_fn=function()info)
 register_custom_model_canvas_handlers(input,output,session,scope$dataset,function()names(d),function()info,function()character(),mark_settings_dirty=function(){},app_language_fn=function()'ko')
}
shiny::runApp(shinyApp(ui,server),host='127.0.0.1',port=3878,launch.browser=FALSE)

