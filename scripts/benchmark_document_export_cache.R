if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out <- 'tmp/document-cache'; dir.create(out,recursive=TRUE,showWarnings=FALSE)
measurements <- list()
for (selection in c('main','all')) for (format in c('word','hwpx')) {
  cache <- result_document_export_cache()
  contents <- if(selection=='all') NULL else 'main'
  extension <- if(format=='word') '.docx' else '.hwpx'
  for (iteration in 1:3) {
    path <- file.path(out,paste0(selection,'-',format,'-',iteration,extension))
    elapsed <- system.time(cache$save(entries,path,format,contents,'ko'))[['elapsed']]
    measurements[[length(measurements)+1L]] <- data.frame(selection,format,iteration,elapsed)
    cat(selection,format,iteration,elapsed,'seconds\n');flush.console()
  }
  paths <- file.path(out,paste0(selection,'-',format,'-',1:3,extension))
  stopifnot(length(unique(unname(tools::md5sum(paths))))==1L)
  cache$clear()
}
write.csv(do.call(rbind,measurements),file.path(out,'timings.csv'),row.names=FALSE)
