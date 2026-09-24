Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
fixture <- readRDS("tmp/canvas-scores/fixture.rds")
prefix <- structural_analysis_prefix("cbsem")
test_language <- function(request) {
  value <- shiny::parseQueryString(request$QUERY_STRING %||% "")$lang %||% "ko"
  normalize_app_language(value)
}
ui <- function(request) {
  language <- test_language(request)
  shiny::fluidPage(tags$head(tags$link(rel="stylesheet",href="style.css"),tags$link(rel="stylesheet",href="model-canvas/canvas.css"),tags$script(src="easyflow.js"),
  lapply(c("state","layout","shiny-bridge","edges","nodes","dialogs","toolbar","canvas"),function(f)tags$script(src=paste0("model-canvas/",f,".js")))),
  structural_equation_workspace(names(fixture$data),analysis_type="cbsem",language=language))
}
server <- function(input,output,session){
  language <- shiny::reactive(normalize_app_language(shiny::parseQueryString(session$clientData$url_search %||% "")$lang %||% "ko"))
  fit <- shiny::reactiveVal(NULL)
  register_canvas_score_editor(input,output,session,function()fixture$data,prefix,paste0(prefix,"_canvas_state"),"cbsem",fit,function(){},language)
}
app <- shiny::shinyApp(ui,server)
app$staticPaths <- list("/"=httpuv::staticPath(normalizePath("www"),indexhtml=FALSE,fallthrough=TRUE))
shiny::runApp(app,host="127.0.0.1",port=43991,launch.browser=FALSE)
