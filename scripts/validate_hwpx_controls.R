Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
for (language in c("ko", "en", "ja")) {
  ui <- as.character(analysis_save_buttons(html_button_id = "html", hwpx_button_id = "hwpx", language = language, included_features = "word"))
  stopifnot(!grepl('id="hwpx"', ui, fixed = TRUE))
}
local({
  original_choose <- choose_hwpx_save_path
  original_write <- write_result_collection_hwpx
  on.exit({assign("choose_hwpx_save_path", original_choose, .GlobalEnv); assign("write_result_collection_hwpx", original_write, .GlobalEnv)})
  received <- NULL
  assign("choose_hwpx_save_path", function() "screen-output", .GlobalEnv)
  assign("write_result_collection_hwpx", function(entries, file) received <<- list(entries = entries, file = file), .GlobalEnv)
  shiny::testServer(function(input, output, session) {
    register_canvas_report_exports(input, session, "html", "pdf", "current_results", "canvas-root", function() "Title", function() list(valid = TRUE), function() "ko", hwpx_id = "hwpx")
  }, {
    session$flushReact()
    session$setInputs(hwpx = 1)
    stopifnot(is.null(received)) # Saving waits for the displayed snapshot.
    session$setInputs(hwpx_snapshot = list(html = "<h3>Visible title</h3><p>Visible note</p>"))
    stopifnot(identical(received$entries[[1]]$html, "<h3>Visible title</h3><p>Visible note</p>"))
    stopifnot(identical(received$file, "screen-output.hwpx"))
  })
})
message("PASS: individual-analysis controls hide HWPX; captured payload writer remains intact")
