Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
shiny::addResourcePath("scope-assets",normalizePath("www"))
ui <- tagList(tags$head(tags$link(rel="stylesheet",href="scope-assets/style.css"),
 tags$script(src="scope-assets/analysis-scope.js"),tags$script(src="scope-assets/easyflow.js")),
 navbarPage("Split plots QA",tabPanel("Split",analysis_scope_panel("split","ko")),
 tabPanel("Regression",actionButton("run","Run"),uiOutput("regression_results"))))
server <- function(input,output,session) {
 set.seed(2026)
 d <- data.frame(sex=rep(c("Male","Female"),each=40),x=rnorm(80))
 d$y <- ifelse(d$sex=="Male",2*d$x,-3*d$x)+rnorm(80)
 scope <- register_analysis_scope(input,output,session,function()d,function()"ko",function()"fixture")
 results <- analysis_scope_result_val(NULL)
 observeEvent(input$run,analysis_scope_run(session,"run",function(){
   results(prepare_regression_analysis_results(scope$dataset(),"y","x",auto_method=FALSE)$results)
 },"regression_results"))
 register_regression_results_output(input,output,results,function()NULL,function()character(),function()NULL,function()NULL)
}
shiny::runApp(shinyApp(ui,server),host="127.0.0.1",port=3874,launch.browser=FALSE)
