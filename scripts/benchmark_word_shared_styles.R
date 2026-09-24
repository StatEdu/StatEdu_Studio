if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
original <- function(document) list(add=function(document,table)flextable::body_add_flextable(document,table),finish=function()invisible(NULL))
shared <- result_docx_shared_table_writer
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out <- 'tmp/word-shared-styles';dir.create(out,recursive=TRUE,showWarnings=FALSE)
measurements <- list()
for(round in 1:3) for(variant in if(round%%2) c('before','after') else c('after','before')) {
  result_docx_shared_table_writer <- if(variant=='before') original else shared
  path <- file.path(out,paste0(round,'-',variant,'.docx'))
  elapsed <- system.time(write_result_collection_docx(entries,path))[['elapsed']]
  measurements[[length(measurements)+1L]] <- data.frame(round,variant,elapsed)
  cat(round,variant,elapsed,'seconds\n');flush.console()
}
result_docx_shared_table_writer <- shared
results <- do.call(rbind,measurements)
write.csv(results,file.path(out,'timings.csv'),row.names=FALSE)
print(aggregate(elapsed~variant,results,median))
