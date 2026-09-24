Sys.setlocale("LC_CTYPE", "English_United States.utf8")
root <- normalizePath("tmp/loaded-language-regression", winslash="/", mustWork=FALSE)
dir.create(root, recursive=TRUE, showWarnings=FALSE)
Sys.setenv(STATEDU_MODULE_CACHE="false", STATEDU_NO_PACKAGE_INSTALL="true",
           STATEDU_USER_SETTINGS_DIR=file.path(root,"settings"), STATEDU_RESULT_STORE=file.path(root,"results.json"))
source("app.R", encoding="UTF-8")
application_server <- server
fixture_settings_path <- NULL
open_settings_file <- function(...) fixture_settings_path
fixture_dir <- file.path(root,"자료 폴더")
dir.create(fixture_dir, showWarnings=FALSE)
fixture_path <- file.path(fixture_dir,"자료.csv")
write.csv(data.frame(설명변수=1:40, 매개변수=(1:40)*.7+sin(1:40), 결과변수=(1:40)*1.2+cos(1:40)), fixture_path, row.names=FALSE, fileEncoding="UTF-8")
server <- function(input, output, session) {
  application_server(input, output, session)
  observeEvent(input$fixture_load, {
    frame <- session$userData$scope_app_frame
    frame$reset_session_settings()
    if (identical(input$fixture_load, "native")) {
      frame$active_data_file(list(path=fixture_path, original_path=fixture_path, name=basename(fixture_path), restored=FALSE))
    } else {
      settings <- list(data_file="embedded.csv", data_file_content_base64=jsonlite::base64_enc(readBin(fixture_path,"raw",file.info(fixture_path)$size)),
                       selected=character(), selection_applied=FALSE)
      settings_path <- file.path(fixture_dir,"복원.studio")
      if (identical(input$fixture_load, "settings-separate")) {
        separate_dir <- file.path(root, "separate settings folder")
        dir.create(separate_dir, recursive=TRUE, showWarnings=FALSE)
        settings_path <- file.path(separate_dir, "external.studio")
        settings$data_file <- basename(fixture_path)
        settings$data_file_path <- fixture_path
      }
      if (input$fixture_load %in% c("legacy", "settings-button")) {
        old_upload_dir <- file.path(root,"RtmpOLD123","upload-token")
        dir.create(old_upload_dir, recursive=TRUE, showWarnings=FALSE)
        old_upload <- file.path(old_upload_dir,"0.csv")
        file.copy(fixture_path, old_upload, overwrite=TRUE)
        settings$data_file_path <- old_upload
      }
      if (identical(input$fixture_load, "settings-button")) settings$data_file <- basename(fixture_path)
      write_settings_json_file(settings, settings_path)
      if (identical(input$fixture_load, "settings-button")) {
        fixture_settings_path <<- settings_path
        session$sendCustomMessage("fixture-settings-ready", TRUE)
      } else frame$apply_settings_object(read_settings_json_file(settings_path), settings_path)
    }
  })
  observeEvent(input$fixture_result, {
    append_result_snapshot(session, title="Test result", html="<p>Fixture content</p>")
  })
}
fixture_app <- shinyApp(ui, server)
fixture_app$staticPaths <- list("/"=httpuv::staticPath(normalizePath("www"), indexhtml=FALSE, fallthrough=TRUE))
shiny::runApp(fixture_app, host="127.0.0.1", port=43989, launch.browser=FALSE)
