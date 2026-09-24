Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
shiny::testServer(create_app_server("1.3.0"),{
 session$flushReact();session$setInputs(main_menu="analysis_penalized");session$flushReact()
 stopifnot(grepl("run_penalized_regularized",output$lazy_analysis_penalized$html,fixed=TRUE))
})
message("PASS: real application lazy menu and handler registration")
