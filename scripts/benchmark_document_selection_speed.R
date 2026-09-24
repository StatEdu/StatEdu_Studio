Sys.setlocale('LC_CTYPE','Korean_Korea.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
entries<-read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out<-'tmp/document-speed';dir.create(out,recursive=TRUE,showWarnings=FALSE)
phase<-commandArgs(TRUE)[1];if(is.na(phase))phase<-'before'
for(selection in c('main','all'))for(format in c('word','hwpx')) {
 name<-paste(phase,selection,format,sep='-');contents<-if(selection=='all')NULL else 'main'
 Rprof(file.path(out,paste0(name,'.prof')),interval=.01)
 elapsed<-system.time(if(format=='word')write_result_collection_docx(entries,file.path(out,paste0(name,'.docx')),contents) else write_result_collection_hwpx(entries,file.path(out,paste0(name,'.hwpx')),contents))['elapsed']
 Rprof(NULL);cat(name,elapsed,'seconds\n');flush.console()
 print(head(summaryRprof(file.path(out,paste0(name,'.prof')))$by.self,10));flush.console()
}
