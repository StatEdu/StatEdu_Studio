root<-'output/sem-gradient-preparation-20260915'
references<-lapply(1:3,function(id)readRDS(file.path(root,paste0('run-',id),'reference.rds')))
stopifnot(identical(references[[1]],references[[2]],num.eq=FALSE),
 identical(references[[1]],references[[3]],num.eq=FALSE))
rows<-do.call(rbind,lapply(1:3,function(id)read.csv(file.path(root,paste0('run-',id),'times.csv'))))
medians<-aggregate(seconds~run+version,rows,median)
summary<-merge(subset(medians,version=='reference',select=-version),
 subset(medians,version=='candidate',select=-version),by='run',suffixes=c('_reference','_candidate'))
summary$change_percent<-100*(summary$seconds_candidate/summary$seconds_reference-1)
write.csv(summary,file.path(root,'summary.csv'),row.names=FALSE)
print(summary)
cat('PASS: 2 cross-process gradient/diagnostic/stdout/RNG comparisons\n')
