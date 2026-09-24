summarize <- function(root, prefix, keys) {
  rows <- do.call(rbind, lapply(1:2, function(run)
    read.csv(file.path(root, paste0(prefix, 'times-', run, '.csv')))))
  medians <- aggregate(rows['seconds'], rows[c('run', keys, 'version')], median)
  result <- merge(subset(medians, version == 'old', select = -version),
                  subset(medians, version == 'new', select = -version),
                  by = c('run', keys), suffixes = c('_old', '_candidate'))
  result$change_percent <- 100 * (result$seconds_candidate / result$seconds_old - 1)
  write.csv(result, file.path(root, paste0(prefix, 'summary.csv')), row.names = FALSE)
  print(result)
}
summarize('output/survival-reason-empty-20260915', '', c('n', 'kind'))
summarize('output/survival-reason-empty-20260915', 'focused-', c('n', 'kind'))
summarize('output/km-reason-empty-20260915', 'focused-', c('n', 'pattern', 'kind'))
