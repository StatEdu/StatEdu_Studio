.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/category-input-chunks-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L]);if(is.na(run))run<-1L
fns<-list(old=compiler::cmpfun(collect_category_label_inputs_from_table),new=compiler::cmpfun(collect_category_label_event_inputs))
times<-list()
for(n in c(200L,1000L)){
 tab<-data.frame(source_order=seq_len(n),name=paste0('v',seq_len(n)))
 ids<-unlist(lapply(seq_len(n),function(i)paste0('category_',category_label_edit_columns(),'_input_',i)),use.names=FALSE)
 input<-do.call(shiny::reactiveValues,setNames(rep(list('label'),length(ids)),ids))
 expected<-shiny::isolate(fns$old(tab,input))
 stopifnot(identical(expected,shiny::isolate(fns$new(tab,input)),num.eq=FALSE))
 for(version in if(run%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-shiny::isolate(fns[[version]](tab,input))
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  times[[length(times)+1L]]<-data.frame(run,n,version,seconds=elapsed)
  cat(n,version,elapsed,'seconds\n')
 }
}
write.csv(do.call(rbind,times),file.path(root,paste0('times-',run,'.csv')),row.names=FALSE)
