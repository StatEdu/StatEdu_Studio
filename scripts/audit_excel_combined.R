source('output/excel-combined-audit-20260914/common.R')
records<-list();checks<-list()
for(id in 1:3) {
  for(v in c('baseline','current'))records[[length(records)+1L]]<-read.csv(file.path(root,paste0(v,'-',id,'.csv')))
  for(name in c('correlation_30','survival_km','collection')) {
    a<-file.path(root,paste0('baseline-',id,'-',name,'.xlsx'))
    b<-file.path(root,paste0('current-',id,'-',name,'.xlsx'))
    stopifnot(identical(readRDS(paste0(a,'.rds')),readRDS(paste0(b,'.rds')),num.eq=FALSE))
    parts<-compare_packages(a,b,paste0('verified-',id,'-',name))
    checks[[length(checks)+1L]]<-data.frame(id=id,workload=name,sheets=length(openxlsx::getSheetNames(b)),noncore_parts=parts)
  }
}
records<-do.call(rbind,records)
summary<-aggregate(elapsed~variant+workload,records,median)
write.csv(records,file.path(root,'measurements.csv'),row.names=FALSE)
write.csv(summary,file.path(root,'summary.csv'),row.names=FALSE)
write.csv(do.call(rbind,checks),file.path(root,'verification.csv'),row.names=FALSE)
print(summary);print(do.call(rbind,checks))
cat('PASS: all nine paired packages and condition/RNG records identical except created/modified timestamps.\n')
