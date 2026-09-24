Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
directory<-tempfile('autorestore-',tmpdir='tmp');dir.create(directory)
path<-file.path(directory,'results.json');Sys.setenv(STATEDU_RESULT_STORE=path)
session<-function(){x<-new.env();x$userData<-new.env();x}
entries<-lapply(1:3,function(i)list(id=paste0('result-',i),title=paste('Result',i),saved_at='2026-09-15',html=paste0('<h4>Table ',i,'</h4><p>Note ',i,'</p>')))
stopifnot(write_result_snapshot_store(entries))
s<-session();store<-result_accumulator_store(s)
stopifnot(identical(isolate(store()),entries))
moved<-result_collection_edit(entries,'result-1','down');stopifnot(write_result_snapshot_store(moved))
stopifnot(identical(isolate(result_accumulator_store(s)()),entries)) # no reload during same session
s2<-session();stopifnot(identical(isolate(result_accumulator_store(s2)()),moved))
clear_result_accumulator_store(s2)
stopifnot(length(isolate(result_accumulator_store(session())()))==0)
writeLines('{invalid JSON',path);original<-readBin(path,'raw',n=file.info(path)$size)
bad<-session();stopifnot(length(isolate(result_accumulator_store(bad)()))==0,!is.null(bad$userData$result_restore_error),
 identical(readBin(path,'raw',n=file.info(path)$size),original))
stopifnot(write_result_snapshot_store(entries))
backups<-list.files(directory,pattern='unreadable-',full.names=TRUE)
stopifnot(length(backups)==1,identical(readBin(backups[1],'raw',n=file.info(backups[1])$size),original),
 identical(read_result_snapshot_store(),entries))
writeLines('{"type":"unrelated","entries":[]}',path)
stopifnot(inherits(try(read_result_snapshot_store(),silent=TRUE),'try-error'))
stopifnot(write_result_snapshot_store(entries))
shiny::testServer(function(input,output,session){
  register_result_accumulator_outputs(input,output,session,function()'ko')
},{
  session$flushReact()
  stopifnot(grepl('Table 1',output$saved_results_list$html,fixed=TRUE))
  session$setInputs(saved_result_entry_action=list(id='result-1',action='down'))
})
shiny::testServer(function(input,output,session){
  register_result_accumulator_outputs(input,output,session,function()'ko')
},{
  session$flushReact()
  restored<-result_accumulator_store(session)()
  stopifnot(identical(vapply(restored,`[[`,character(1),'id'),c('result-2','result-1','result-3')))
  session$setInputs(clear_saved_results=1)
  stopifnot(length(result_accumulator_store(session)())==0)
})
stopifnot(length(isolate(result_accumulator_store(session())()))==0)
message('PASS: automatic ordered restoration, same-session stability, clear persistence, corrupt-file preservation and backup, wrong-file rejection')
