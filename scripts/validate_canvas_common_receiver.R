Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8")
load_app_packages(check=FALSE);source_app_modules()
out <- "tmp/mm-figure-snapshot"
choose_figure_save_dir <- function() out
original_save <- save_canvas_figure_snapshots
saved_files <- character()
save_canvas_figure_snapshots <- function(...) {saved_files <<- original_save(...);saved_files}
for(menu in c("mm","cfa","sem","pls")) {
  prefix <- if(menu=="mm") "custom_model_canvas" else structural_analysis_prefix(switch(menu,cfa="cfa",sem="cbsem",pls="plssem"))
  input_prefix <- if(menu=="mm") prefix else paste0(prefix,"_canvas")
  payload <- jsonlite::read_json(file.path(out,paste0(menu,"-common-payload.json")))
  saved_files <- character()
  shiny::testServer(function(input,output,session) {
    register_canvas_report_exports(input,session,"save_html","save_pdf",paste0(prefix,"_results"),
      "canvas-root",function() "Test",function() NULL,canvas_input_prefix=input_prefix)
  }, {
    session$flushReact()
    do.call(session$setInputs,setNames(list(payload),paste0(input_prefix,"_figures_snapshot")))
    stopifnot(length(saved_files)==1L,file.exists(saved_files[[1]]))
  })
  message("PASS: ",menu," real Shiny figure receiver and disk save")
}
