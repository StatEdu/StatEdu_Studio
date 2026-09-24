Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
for (language in statedu_supported_languages()) {
  for (key in names(statedu_translation_table())) {
    value <- statedu_t(key, language)
    if (!all(validUTF8(value))) stop(sprintf("Invalid UTF-8 translation: %s/%s", language, key))
  }
}
stopifnot(identical(statedu_t("preferences.saved","ko"),"환경 설정을 저장했습니다."))
cat("PASS: translation catalog produces valid UTF-8 for every language/key\n")
dir.create("tmp/loaded-language-regression/자료 폴더",recursive=TRUE,showWarnings=FALSE)
directory <- normalizePath("tmp/loaded-language-regression/자료 폴더",winslash="/",mustWork=TRUE)
old_dir <- file.path(dirname(directory),"RtmpOLD456","upload-token")
dir.create(old_dir,recursive=TRUE,showWarnings=FALSE)
old_file <- file.path(old_dir,"0.csv")
writeLines("x,y\n1,2",old_file)
legacy <- list(data_file="legacy.csv",data_file_path=old_file)
project <- file.path(directory,"legacy.studio")
stopifnot(data_path_is_session_temporary(old_file))
stopifnot(identical(data_file_source_directory(list(path=old_file)),""))
stopifnot(identical(data_file_source_directory(settings_restored_data_file(legacy,project)),directory))
stopifnot(identical(data_file_source_directory(settings_external_data_switch(legacy,project)),directory))
stopifnot(identical(settings_restored_data_file(legacy,project)$name,"legacy.csv"))
cat("PASS: existing upload from a previous R session never becomes the dialog folder\n")
settings_directory <- file.path(dirname(directory), "separate settings folder")
dir.create(settings_directory, recursive = TRUE, showWarnings = FALSE)
external_path <- file.path(directory, "external.csv")
writeLines("x,y\n1,2", external_path)
separate_project <- file.path(settings_directory, "external.studio")
external_settings <- list(data_file = "external.csv", data_file_path = external_path)
for (restored in list(settings_restored_data_file(external_settings, separate_project),
                      settings_external_data_switch(external_settings, separate_project))) {
  stopifnot(identical(data_file_source_directory(restored), directory))
}
stopifnot(identical(data_file_source_directory(list(path = old_file,
  original_path = external_path, source_directory = settings_directory)), directory))
stopifnot(identical(data_file_source_directory(list(path = old_file,
  source_directory = settings_directory)), normalizePath(settings_directory, winslash = "/")))
cat("PASS: external data folder takes priority over a separate settings folder; embedded data keeps its fallback\n")
shiny::testServer(function(input, output, session) {}, {
  original_data <- file.path(directory,"원본.csv")
  writeLines("x,y\n3,4",original_data)
  settings <- list(data_file=basename(original_data),data_file_path=old_file)
  active <- reactiveVal(NULL)
  pending <- reactiveVal(NULL)
  restore <- create_restore_settings_state_fn(
    current_data_file_fn=active,pending_settings=pending,reset_on_dataset_load=reactiveVal(TRUE),
    active_data_file=active,apply_restored_settings_basics_fn=function(...) {},
    restore_settings_data_file_fn=create_restore_settings_data_file_fn(active),
    restore_settings_variable_info_only_fn=function(...) FALSE,
    restore_settings_for_current_data_fn=function(...) pending(NULL))
  restore(settings,project)
  stopifnot(identical(active()$path,original_data),!is.null(pending()))
  restore(pending()) # Exactly the second pass used by the dataset observer.
  stopifnot(identical(active()$path,original_data),is.null(pending()),
            identical(active()$name,"원본.csv"),identical(data_file_source_directory(active()),directory))
  stopifnot(is.null(attr(settings,"statedu_settings_path")))
  cat("PASS: pending settings retain the original file and project directory across dataset loading\n")
})
file <- list(path=file.path(directory,"자료.csv"))
shiny::testServer(function(input,output,session) {
  session$userData$result_data_file <- function() file
}, {
  original <- run_windows_dialog_script
  captured <- ""
  assign("run_windows_dialog_script",function(script) {captured<<-script;windows_dialog_cancel_marker},envir=.GlobalEnv)
  tryCatch({
    for (kind in c("native","upload","embedded")) {
      file <- switch(kind,
        native=list(path=file.path(directory,"자료.csv")),
        upload=list(path=file.path(tempdir(),"upload.csv"), original_path=file.path(directory,"자료.csv")),
        embedded=list(path=file.path(tempdir(),"embedded.csv"),source_directory=directory))
      stopifnot(identical(result_save_directory(),directory))
      for (fn in c("choose_figure_save_dir","choose_html_save_path","choose_pdf_save_path","choose_word_save_path","choose_hwpx_save_path","choose_excel_save_path")) {
        stopifnot(!length(get(fn)()))
        expected <- if(fn=="choose_figure_save_dir") normalizePath(directory,winslash="\\") else directory
        stopifnot(grepl(ps_quote(expected),captured,fixed=TRUE))
      }
      cat("PASS:",kind,"six export dialogs use the loaded folder; cancellation preserved\n")
    }
    stopifnot(identical(data_file_source_directory(NULL),""))
    stopifnot(identical(data_file_source_directory(list(path=file.path(tempdir(),"upload.csv"))),""))
  },finally=assign("run_windows_dialog_script",original,envir=.GlobalEnv))
})
