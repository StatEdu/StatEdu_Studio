root<-'output/sem-shared-inverse-product-long-20260915'
values<-lapply(1:2,function(id)readRDS(file.path(root,paste0('run-',id),'comparison.rds')))
without_time<-function(x){x$timings<-NULL;x$elapsed<-NULL;x}
for(version in c('disabled','enabled'))
 stopifnot(identical(without_time(values[[1]][[version]]),without_time(values[[2]][[version]]),num.eq=FALSE))
out<-do.call(rbind,lapply(1:2,function(id){
 x<-values[[id]];a<-x$disabled;b<-x$enabled
 stopifnot(identical(without_time(a),without_time(b),num.eq=FALSE),all(b$value$valid==5000L))
 data.frame(run=id,baseline_wall=a$elapsed,product_wall=b$elapsed,
  wall_saved=a$elapsed-b$elapsed,wall_reduction=100*(1-b$elapsed/a$elapsed),
  baseline_resampling=a$timings$resampling,product_resampling=b$timings$resampling,
  resampling_reduction=100*(1-b$timings$resampling/a$timings$resampling),valid=min(b$value$valid))
}))
write.csv(out,file.path(root,'summary.csv'),row.names=FALSE)
print(out)
cat('PASS: two A/B and two cross-process comparisons; only timings and measured elapsed excluded\n')
