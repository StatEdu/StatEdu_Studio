.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/category-chunk-server-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run_id<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run_id))run_id<-1L
n<-500L
tab<-data.frame(source_order=seq_len(n),name=paste0('v',seq_len(n)))
base<-normalize_category_label_table(data.frame(name=tab$name),category_label_edit_columns())
functions<-list(old=compiler::cmpfun(collect_category_label_inputs_from_table),new=compiler::cmpfun(collect_category_label_event_inputs))
seconds<-function(start)as.numeric(difftime(Sys.time(),start,units='secs'))
run_case<-function(version,kind){
 fields<-category_label_edit_columns()
 rows<-if(kind=='sparse')seq.int(1L,n,by=10L)else seq_len(n)
 if(kind=='sparse')fields<-c('var_label','reference','value_1','label_1')
 ids<-unlist(lapply(rows,function(i)paste0('category_',fields,'_input_',i)),use.names=FALSE)
 values<-setNames(rep(list('1'),length(ids)),ids)
 if(kind=='empty')values<-list()
 env<-new.env(parent=.GlobalEnv);sys.source('R/server_selection.R',env)
 state<-new.env(parent=emptyenv());state$current<-base;state$records<-list();state$payloads<-list();state$results<-list()
 env$collect_category_label_event_inputs<-function(...){
  start<-Sys.time();value<-functions[[version]](...)
  state$collect_seconds<-seconds(start);value
 }
 server<-function(input,output,session){
  env$register_category_label_observers(input,function(...)NULL,function(...)NULL,
   apply_category_label_snapshot=function(payload){
    start<-Sys.time();result<-apply_category_label_snapshot(state$current,payload$category_labels)
    state$snapshot_seconds<-seconds(start)
    state$current<-result$table
    state$payloads[[length(state$payloads)+1L]]<-payload
    state$results[[length(state$results)+1L]]<-result
   },category_label_table_data_fn=function()tab)
 }
 shiny::testServer(server,{
  if(length(values))do.call(session$setInputs,values)
  # First apply includes new dependency keys and actual changed values.
  # Second apply repeats the same values with warmed input objects.
  for(button in 1:2){
   gc();start<-Sys.time();session$setInputs(apply_category_labels_button=button)
   elapsed<-seconds(start)
   stopifnot(length(state$results)==button)
   state$records[[button]]<-data.frame(run=run_id,n,kind,version,button,
    collector=state$collect_seconds,snapshot=state$snapshot_seconds,event_flush=elapsed,
    changed=state$results[[button]]$changed)
   cat(run_id,kind,version,button,'collector',state$collect_seconds,'flush',elapsed,'\n')
  }
 })
 list(records=do.call(rbind,state$records),payloads=state$payloads,results=state$results)
}
set.seed(20260915);all_times<-list();evidence<-list()
for(kind in c('empty','sparse','full')){
 cases<-list();seed<-.Random.seed
 for(version in if(run_id%%2L)c('old','new')else c('new','old')){
  .Random.seed<-seed;cases[[version]]<-run_case(version,kind)
  all_times[[length(all_times)+1L]]<-cases[[version]]$records
 }
 stopifnot(identical(cases$old$payloads,cases$new$payloads,num.eq=FALSE),
  identical(cases$old$results,cases$new$results,num.eq=FALSE))
 stopifnot(!cases$new$results[[2L]]$changed)
 evidence[[kind]]<-list(payloads=cases$new$payloads,results=cases$new$results)
}
write.csv(do.call(rbind,all_times),file.path(root,paste0('times-',run_id,'.csv')),row.names=FALSE)
saveRDS(evidence,file.path(root,paste0('results-',run_id,'.rds')))
cat('PASS: all registered event payloads, snapshot tables, changed flags and label updates identical\n')
