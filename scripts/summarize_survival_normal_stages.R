root <- 'output/survival-normal-stages-20260915'
phases <- do.call(rbind, lapply(1:2, function(run)
  read.csv(file.path(root, paste0('phases-', run, '.csv')))))
times <- do.call(rbind, lapply(1:2, function(run)
  read.csv(file.path(root, paste0('times-', run, '.csv')))))
phase_summary <- aggregate(seconds ~ run + n + kind + phase, phases, median)
time_summary <- aggregate(seconds ~ run + n + kind, times, median)
write.csv(phase_summary, file.path(root, 'phase-summary.csv'), row.names = FALSE)
write.csv(time_summary, file.path(root, 'time-summary.csv'), row.names = FALSE)
checks <- 0L
for (n in unique(times$n)) for (kind in unique(times$kind)) {
  a <- readRDS(file.path(root, paste(kind, n, 1, 'rds', sep = '.')))
  b <- readRDS(file.path(root, paste(kind, n, 2, 'rds', sep = '.')))
  stopifnot(identical(a, b, num.eq = FALSE))
  checks <- checks + 1L
}
print(subset(phase_summary, kind == 'complete'))
print(time_summary)
cat('PASS:', checks, 'cross-process full result/diagnostic/stdout/RNG comparisons\n')
