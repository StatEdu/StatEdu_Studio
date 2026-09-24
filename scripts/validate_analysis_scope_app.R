Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
shiny::addResourcePath("scope-assets",normalizePath("www"))
ui <- tagList(tags$head(tags$link(rel="stylesheet",href="scope-assets/style.css"),
  tags$script(src="scope-assets/analysis-scope.js"),tags$script(src="scope-assets/easyflow.js")),
  navbarPage("Scope QA",
    tabPanel("Cases",analysis_scope_panel("cases","en")),
    tabPanel("Split",analysis_scope_panel("split","en")),
    frequencies_tab_panel("Frequencies","en")))
server <- function(input,output,session){
  d <- data.frame(sex=rep(c("Male","Female"),each=10),x=c(1:10,101:110))
  d$score <- d$x
  info <- data.frame(name=c("sex","x","score"),measurement=c("nominal","continuous","continuous"),var_label=c("Sex","Range variable","Score"),stringsAsFactors=FALSE)
  scope <- register_analysis_scope(input,output,session,function() d,function() "en",function() "demo")
  register_frequencies_handlers(input,output,session,scope$dataset,function() names(d),function() info,
    function() character(),function() NULL,reactiveVal(c("sex","score")),function() NULL,function() "en")
}
shiny::runApp(shinyApp(ui,server),host="127.0.0.1",port=3867,launch.browser=FALSE)
