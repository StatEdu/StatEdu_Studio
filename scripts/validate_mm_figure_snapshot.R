Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
if ("--check" %in% commandArgs(TRUE)) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  source("R/result_export_files.R", encoding="UTF-8")
} else {
  source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
}
out <- "tmp/mm-figure-snapshot";dir.create(out,recursive=TRUE,showWarnings=FALSE)
if ("--check" %in% commandArgs(TRUE)) {
  payload <- jsonlite::read_json(file.path(out,"payload.json"),simplifyVector=FALSE)
  saved <- save_canvas_figure_snapshots(payload$files,out)
  stopifnot(length(saved)==length(payload$files),length(saved)>=3L)
  for(index in seq_along(saved)) stopifnot(identical(readBin(saved[[index]],"raw",n=file.info(saved[[index]])$size),jsonlite::base64_dec(sub("^data:image/png;base64,","",payload$files[[index]]$data))))
  stopifnot(any(grepl("johnson_neyman",saved)),any(grepl("conditional_effect",saved)))
  # Valid base64 is also accepted without optional trailing padding.
  unpadded <- payload$files
  unpadded <- lapply(unpadded,function(item) {item$data <- sub("=+$","",item$data);item})
  unpadded_saved <- save_canvas_figure_snapshots(unpadded,out)
  for(index in seq_along(saved)) stopifnot(identical(
    readBin(saved[[index]],"raw",n=file.info(saved[[index]])$size),
    readBin(unpadded_saved[[index]],"raw",n=file.info(unpadded_saved[[index]])$size)))
  browser_payload <- jsonlite::read_json(file.path(out,"browser-payload.json"),simplifyVector=FALSE)
  browser_saved <- save_canvas_figure_snapshots(browser_payload$files,out)
  for(index in seq_along(saved)) stopifnot(identical(
    readBin(saved[[index]],"raw",n=file.info(saved[[index]])$size),
    readBin(browser_saved[[index]],"raw",n=file.info(browser_saved[[index]])$size)))
  # Reject corrupt data before creating a partial export folder.
  before <- list.dirs(out,recursive=FALSE)
  bad <- payload$files;bad[[2]]$data <- paste0(bad[[2]]$data,"%not-base64")
  stopifnot(inherits(try(save_canvas_figure_snapshots(bad,out),silent=TRUE),"try-error"),
            identical(before,list.dirs(out,recursive=FALSE)))
  for(menu in c("mm","cfa","sem","pls")) {
    common <- jsonlite::read_json(file.path(out,paste0(menu,"-common-payload.json")))
    paths <- save_canvas_figure_snapshots(common$files,out,menu)
    stopifnot(length(paths)==1L,identical(readBin(paths[[1]],"raw",file.info(paths[[1]])$size),
      jsonlite::base64_dec(sub("^data:image/png;base64,","",common$files[[1]]$data))))
  }
  # Capture the generated native dialog script without opening a blocking UI.
  run_windows_dialog_script <- function(script) script
  dialog <- choose_windows_directory("그림 저장 위치 선택")
  stopifnot(grepl("FolderBrowserDialog",dialog,fixed=TRUE),
            !grepl("OpenFileDialog",dialog,fixed=TRUE),
            grepl("$dialog.SelectedPath",dialog,fixed=TRUE),
            grepl(windows_dialog_cancel_marker,dialog,fixed=TRUE))
  message("PASS: model and every displayed conditional/JN image saved byte-for-byte: ",length(saved)," PNGs")
} else {
  result <- readRDS("outputs/spss_phase36_20260907/moderated_mediation/analysis.rds")
  stopifnot(any(vapply(result$conditional_plot_specs,function(x) grepl("johnson_neyman",x$kind),logical(1))))
  writeLines(as.character(mediation_moderation_conditional_plots_ui(result$conditional_plot_specs)),file.path(out,"plots.html"),useBytes=TRUE)
}
