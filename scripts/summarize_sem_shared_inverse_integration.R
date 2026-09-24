root<-'output/sem-shared-inverse-integration-20260915/worker-reuse'
evidence<-lapply(1:3,function(id)readRDS(file.path(root,paste0('run-',id),'comparison.rds')))
stopifnot(identical(evidence[[1]]$baseline,evidence[[2]]$baseline,num.eq=FALSE),
 identical(evidence[[1]]$baseline,evidence[[3]]$baseline,num.eq=FALSE))
rows<-lapply(1:3,function(id){
 x<-evidence[[id]];old<-x$baseline_timings$resampling;new<-x$profiled_timings$resampling
 data.frame(run=id,baseline=old,candidate=new,change_percent=100*(new/old-1),
  valid=sum(attr(x$profiled$value,'bootstrap_draws')$valid_mask))
})
out<-do.call(rbind,rows);write.csv(out,file.path(root,'summary.csv'),row.names=FALSE)
print(out);cat('PASS: 2 cross-process full result/draws/parent diagnostic/stdout/RNG comparisons\n')
