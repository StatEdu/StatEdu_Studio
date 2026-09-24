if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
baseline <- commandArgs(TRUE)[1]
if(is.na(baseline))stop('Provide the pre-change result_saved_ui.R snapshot path.')
old <- new.env(parent=globalenv())
for(expr in parse(baseline,encoding='UTF-8')) if(is.call(expr) && identical(expr[[1]],as.name('<-')) &&
  identical(expr[[2]],as.name('result_document_table'))) eval(expr,old)
current <- result_document_table
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out <- 'tmp/document-cold-batches';dir.create(out,recursive=TRUE,showWarnings=FALSE)
measurements <- list()
# Alternate order; every call constructs a new document through the uncached writer.
for(round in 1:3) for(format in c('word','hwpx')) for(variant in if(round%%2) c('before','after') else c('after','before')) {
  result_document_table <- if(variant=='before') old$result_document_table else current
  path <- file.path(out,paste0(round,'-',variant,'-',format,if(format=='word') '.docx' else '.hwpx'))
  elapsed <- system.time(if(format=='word')write_result_collection_docx(entries,path) else write_result_collection_hwpx(entries,path))[['elapsed']]
  measurements[[length(measurements)+1L]] <- data.frame(round,format,variant,elapsed)
  cat(round,format,variant,elapsed,'seconds\n');flush.console()
}
result_document_table <- current
results <- do.call(rbind,measurements)
write.csv(results,file.path(out,'timings.csv'),row.names=FALSE)
print(aggregate(elapsed~format+variant,results,median))
