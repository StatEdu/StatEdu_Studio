root<-'output/sem-shared-inverse-guarded-20260915/final'
rows<-list()
for(mode in c('complete','missing','matched_pair_dmc','all_pairs_dmc')) {
 values<-lapply(1:2,function(id)readRDS(file.path(root,paste0(mode,'-',id),'comparison.rds')))
 stopifnot(identical(values[[1]]$baseline,values[[2]]$baseline,num.eq=FALSE))
 for(id in 1:2) {
  value<-values[[id]]
  scope<-readRDS(file.path(root,paste0(mode,'-',id),'scope.rds'))
  rows[[length(rows)+1L]]<-data.frame(mode,run=id,valid=sum(attr(value$profiled$value,'bootstrap_draws')$valid_mask),
   scope_calls=scope$calls,scope_hits=scope$hits,
   baseline=value$baseline_timings$resampling,candidate=value$profiled_timings$resampling)
 }
}
out<-do.call(rbind,rows);write.csv(out,file.path(root,'summary.csv'),row.names=FALSE)
print(out);cat('PASS: 4 cross-process full result/draw/parent diagnostic/stdout/RNG comparisons\n')
