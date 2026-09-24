source('scripts/validate_category_snapshot_batch.R')
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L])
if(is.na(run))run<-1L
timings<-list()
for(n in c(500L,2000L))for(kind in c('changed','unchanged')){
 tab<-make_table(n)
 if(kind=='unchanged')tab<-old$apply_category_label_snapshot(tab,list())$table
 args<-list(current=tab,incoming=list())
 expected<-capture(old,args)
 stopifnot(identical(expected,capture(new,args),num.eq=FALSE))
 for(iteration in seq_len(3L))for(version in if((run+iteration)%%2L)c('old','new')else c('new','old')){
  gc();start<-Sys.time();actual<-capture(get(version),args)
  elapsed<-as.numeric(difftime(Sys.time(),start,units='secs'))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  timings[[length(timings)+1L]]<-data.frame(run,n,kind,iteration,version,seconds=elapsed)
 }
}
result<-do.call(rbind,timings)
write.csv(result,file.path(root,paste0('times-',run,'.csv')),row.names=FALSE)
print(aggregate(seconds~n+kind+version,result,median),row.names=FALSE)
