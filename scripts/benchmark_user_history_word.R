if(.Platform$OS.type=='windows')Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
path<-'sample/StatEdu_Studio_result_history_20260917_154949.efs-result'
entries<-read_result_snapshot_store(path)
out<-'tmp/user-history-word-benchmark';dir.create(out,recursive=TRUE,showWarnings=FALSE)
optimized<-write_result_collection_docx
code<-paste(deparse(optimized,width.cutoff=500L),collapse='\n')
code<-gsub(', ns = word_ns','',code,fixed=TRUE)
code<-gsub('"w:w"','"w"',code,fixed=TRUE);code<-gsub('"w:val"','"val"',code,fixed=TRUE)
legacy<-eval(parse(text=code));environment(legacy)<-environment(optimized)
cases<-c(setNames(lapply(seq_along(entries),function(i)entries[i]),paste0('entry-',seq_along(entries))),list(all=entries))
rows<-list()
for(key in names(cases)) {
 e<-cases[[key]]
 d<-xml2::read_html(paste(vapply(e,`[[`,character(1),'html'),collapse='\n'))
 for(version in c('before','after')) {
  gc();writer<-if(version=='before')legacy else optimized
  file<-file.path(out,paste0(key,'-',version,'.docx'))
  timing<-system.time(writer(e,file))
  rows[[length(rows)+1L]]<-data.frame(case=key,title=paste(vapply(e,`[[`,character(1),'title'),collapse=' + '),version=version,seconds=unname(timing['elapsed']),tables=length(xml2::xml_find_all(d,'//table')),cells=length(xml2::xml_find_all(d,'//th|//td')),images=length(xml2::xml_find_all(d,'//img')))
  write.csv(do.call(rbind,rows),file.path(out,'timings.csv'),row.names=FALSE,fileEncoding='UTF-8')
  cat(key,version,unname(timing['elapsed']),'seconds\n');flush.console()
 }
}
