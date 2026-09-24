.libPaths(R.home('library'))
root<-'output/sem-product-hessian-audit-20260915'
records<-list();baselines<-list()
for(id in 1:3) {
 dir<-file.path(root,paste0('run-',id))
 x<-readRDS(file.path(dir,'comparison.rds'))
 baselines[[id]]<-x$results$baseline
 rows<-read.csv(file.path(dir,'calls.csv'))
 stopifnot(nrow(rows)==24L,all(rows$method=='richardson'),all(rows$parameter_count==57L),
  all(rows$h==1e-6),all(rows$gradient_calls==4L*rows$parameter_count),all(rows$repeated_inputs==0L))
 rows$run<-id;records[[id]]<-rows
}
stopifnot(identical(baselines[[1]],baselines[[2]],num.eq=FALSE),
 identical(baselines[[1]],baselines[[3]],num.eq=FALSE))
rows<-do.call(rbind,records)
summary<-aggregate(cbind(gradient_calls,repeated_inputs)~run+method+parameter_count,rows,sum)
write.csv(summary,file.path(root,'summary.csv'),row.names=FALSE)
writeLines(deparse(get('lav_model_hessian',asNamespace('lavaan'))),
 file.path(root,'bundled-lav_model_hessian.R'))
print(summary)
cat('PASS: 2 cross-process full result/draws/parent diagnostic/stdout/RNG comparisons\n')
