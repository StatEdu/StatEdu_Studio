if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
entries<-read_result_snapshot_store('sample/StatEdu_Studio_result_history_20260917_154949.efs-result')
out<-'tmp/user-history-hwpx-benchmark';dir.create(out,recursive=TRUE,showWarnings=FALSE)
cases<-c(setNames(lapply(seq_along(entries),function(i)entries[i]),paste0('entry-',seq_along(entries))),list(all=entries))
rows<-list()
for(key in names(cases)) {
 gc();file<-file.path(out,paste0(key,'.hwpx'))
 timing<-system.time(write_result_collection_hwpx(cases[[key]],file))
 rows[[length(rows)+1L]]<-data.frame(case=key,title=paste(vapply(cases[[key]],`[[`,character(1),'title'),collapse=' + '),seconds=unname(timing['elapsed']),bytes=file.info(file)$size)
 write.csv(do.call(rbind,rows),file.path(out,'timings.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(key,unname(timing['elapsed']),'seconds\n');flush.console()
}
