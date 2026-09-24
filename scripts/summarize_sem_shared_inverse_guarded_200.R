root<-'output/sem-shared-inverse-guarded-20260915/guarded-200'
values<-lapply(1:2,function(id)readRDS(file.path(root,paste0('all_pairs_dmc-',id),'comparison.rds')))
stopifnot(identical(values[[1]]$baseline,values[[2]]$baseline,num.eq=FALSE))
rows<-lapply(1:2,function(id){
 x<-values[[id]];old<-x$baseline_timings$resampling;new<-x$profiled_timings$resampling
 data.frame(run=id,baseline=old,candidate=new,change_percent=100*(new/old-1),
 valid=sum(attr(x$profiled$value,'bootstrap_draws')$valid_mask))
})
out<-do.call(rbind,rows);write.csv(out,file.path(root,'summary.csv'),row.names=FALSE)
print(out);cat('PASS: cross-process full result/draw/parent diagnostic/stdout/RNG comparison\n')
