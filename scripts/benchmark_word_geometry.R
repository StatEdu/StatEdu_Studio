if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
expressions <- parse('tmp/result_saved_ui_before_word_geometry.R',encoding='UTF-8')
definition <- Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('result_document_table')),as.list(expressions))[[1]]
legacy <- eval(definition[[3]]); current <- result_document_table
entries <- read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out <- 'tmp/word-geometry';dir.create(out,recursive=TRUE,showWarnings=FALSE)
measurements <- list()
for(round in 1:3) for(selection in c('main','all')) for(variant in if(round%%2)c('before','after') else c('after','before')) {
  result_document_table <- if(variant=='before')legacy else current
  path <- file.path(out,paste0(round,'-',selection,'-',variant,'.docx'))
  elapsed <- system.time(write_result_collection_docx(entries,path,if(selection=='all')NULL else 'main'))[['elapsed']]
  measurements[[length(measurements)+1L]] <- data.frame(round,selection,variant,elapsed)
  cat(round,selection,variant,elapsed,'seconds\n');flush.console()
}
result_document_table <- current
results<-do.call(rbind,measurements);write.csv(results,file.path(out,'timings.csv'),row.names=FALSE)
print(aggregate(elapsed~selection+variant,results,median))
