source('native/fine_gray/tests/common.R')
count<-0L
for(p in c(1,3,10))for(kind in c('plain','ties','groups','binary','duplicate','near','timevarying')) {
 d<-make(100,p,kind);outputs<-lapply(versions,capture_all,d=d)
 for(mode in c('original','candidate'))if(!identical(outputs$installed,outputs[[mode]],num.eq=FALSE)){cat(p,kind,mode,'FAILED\n');print(all.equal(outputs$installed,outputs[[mode]]));saveRDS(outputs,file.path(root,'mismatch.rds'));stop('Mismatch')}
 count<-count+1L
}
cat('PASS:',count,'conditions: installed vs rebuilt original AND cached candidate, full results/conditions/RNG.\n')
