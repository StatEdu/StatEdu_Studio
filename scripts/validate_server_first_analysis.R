root <- 'output/server-first-analysis-20260913'
args <- commandArgs(TRUE)
ids <- if (length(args)) args else as.character(1:3)
rows <- list()
for (id in ids) {
  old <- readRDS(file.path(root,paste0('baseline-',id,'.rds')))
  new <- readRDS(file.path(root,paste0('current-',id,'.rds')))
  stopifnot(identical(old,new, num.eq=FALSE),length(new$warnings)==0L,
    length(new$results)==4L)
  for (mode in c('baseline','current')) {
    rows[[length(rows)+1L]] <- read.csv(file.path(root,paste0(mode,'-',id,'.csv')))
  }
}
rows <- do.call(rbind,rows)
summary <- aggregate(rows[setdiff(names(rows),c('mode','id'))],list(mode=rows$mode),median)
print(summary)
write.csv(summary,file.path(root,'summary.csv'),row.names=FALSE)
cat('PASS:',length(ids),'pairs of full results, live result HTML/dependencies and notifications match exactly.\n')
