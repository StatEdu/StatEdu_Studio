if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
legacy <- function(info) {
  table <- result_document_table(info,layout_only=TRUE)
  list(widths=table$body$colwidths,heights=c(table$header$rowheights,table$body$rowheights))
}
direct <- result_document_table_geometry
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out <- 'tmp/hwpx-geometry';dir.create(out,recursive=TRUE,showWarnings=FALSE)
measurements <- list()
for(round in 1:3) for(selection in c('main','all')) for(variant in if(round%%2)c('before','after') else c('after','before')) {
  result_document_table_geometry <- if(variant=='before')legacy else direct
  path <- file.path(out,paste0(round,'-',selection,'-',variant,'.hwpx'))
  contents <- if(selection=='all')NULL else 'main'
  elapsed <- system.time(write_result_collection_hwpx(entries,path,contents))[['elapsed']]
  measurements[[length(measurements)+1L]] <- data.frame(round,selection,variant,elapsed)
  cat(round,selection,variant,elapsed,'seconds\n');flush.console()
}
result_document_table_geometry <- direct
results<-do.call(rbind,measurements)
write.csv(results,file.path(out,'timings.csv'),row.names=FALSE)
print(aggregate(elapsed~selection+variant,results,median))
