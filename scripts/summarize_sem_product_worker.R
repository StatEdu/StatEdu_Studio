root<-'output/sem-product-worker-profile-20260915'
targets<-c('lavaan::lavaan','lav_step11_estoptim','lav_step13_vcov_boot','lav_model_hessian',
 'lav_step03_data','lav_step05_samp','context$extract_fit')
records<-list();modes<-list();baselines<-list()
for(id in 1:3) {
 directory<-file.path(root,paste0('run-',id))
 evidence<-readRDS(file.path(directory,'comparison.rds'))
 baselines[[id]]<-evidence$baseline
 table<-read.csv(file.path(directory,'by.total.csv'))
 table$name<-gsub('"','',table$name,fixed=TRUE)
 stopifnot(all(targets %in% table$name))
 selected<-table[match(targets,table$name),];selected$run<-id
 records[[id]]<-selected
 files<-list.files(directory,pattern='[.]Rprof$',full.names=TRUE)
 for(mode in c('screen','full')) {
   matched<-files[grepl(paste0('-',mode,'-'),basename(files),fixed=TRUE)]
   seconds<-sum(vapply(matched,function(f)summaryRprof(f)$sampling.time,numeric(1)))
   modes[[length(modes)+1L]]<-data.frame(run=id,mode,files=length(matched),sample_seconds=seconds)
 }
}
stopifnot(identical(baselines[[1]],baselines[[2]],num.eq=FALSE),
 identical(baselines[[1]],baselines[[3]],num.eq=FALSE))
out<-do.call(rbind,records);mode_summary<-do.call(rbind,modes)
write.csv(out,file.path(root,'phase-summary.csv'),row.names=FALSE)
write.csv(mode_summary,file.path(root,'mode-summary.csv'),row.names=FALSE)
print(out[c('run','name','sample_percent')]);print(mode_summary)
cat('PASS: 2 cross-process baseline comparisons, including draws/diagnostics/stdout/RNG\n')
