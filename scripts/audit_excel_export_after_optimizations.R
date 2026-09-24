source('output/table-parse-reuse-20260914/common.R')
root <- 'output/excel-export-profile-20260914'
ids <- c('after-parse-1','after-parse-2')
totals <- list()
for (id in ids) {
  run <- readRDS(file.path(root,paste0('run-',id,'.rds')))
  stopifnot(!length(run$warnings))
  records <- read.csv(file.path(root,paste0('helpers-',id,'.csv')))
  rows <- aggregate(elapsed~helper,records,sum)
  rows <- rbind(data.frame(helper='whole_save',elapsed=run$elapsed),rows)
  rows$run <- id
  totals[[id]] <- rows
  compare_packages('output/table-render-combined-20260914/current-correlation_30.xlsx',
    file.path(root,paste0('profile-',id,'.xlsx')),paste0('verified-',id))
}
measurements <- do.call(rbind,totals)
summary <- aggregate(elapsed~helper,measurements,median)
write.csv(measurements,file.path(root,'after-optimizations-measurements.csv'),row.names=FALSE)
write.csv(summary,file.path(root,'after-optimizations-summary.csv'),row.names=FALSE)
print(summary)
cat('PASS: two profiled exports; complete package equality except created/modified timestamps.\n')
