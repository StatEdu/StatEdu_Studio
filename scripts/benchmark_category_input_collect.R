source('scripts/validate_category_input_collect.R')
run<-as.integer(commandArgs(trailingOnly=TRUE)[1L])
if(is.na(run))run<-1L
timings<-list()
for(n in c(100L,1000L))for(kind in c('empty','full')){
 tab<-make_table(n)
 input<-do.call(shiny::reactiveValues,if(kind=='empty')list()else make_input(tab,11L))
 args<-list(table_data=tab,input=input)
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
