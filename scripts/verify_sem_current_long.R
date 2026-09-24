root <- 'output/sem-current-long-20260915'
modes <- c('complete', 'missing', 'matched_pair_dmc', 'all_pairs_dmc')
rows <- list()
for (mode in modes) {
  values <- lapply(1:2, function(id) readRDS(file.path(root, paste0(mode, '-', id, '.rds'))))
  timing <- lapply(values, attr, which = 'timings')
  expected_seed <- switch(mode, matched_pair_dmc = 20260832L,
                          all_pairs_dmc = 20260833L, 20260826L)
  stopifnot(identical(timing[[1]]$rng, timing[[2]]$rng, num.eq = FALSE))
  for (t in timing) {
    stopifnot(t$workers == 4L, t$chunk_size == 100L, t$fixed_index$active,
              t$fixed_index$fallbacks == 0L, t$rng$seed == expected_seed)
    if (mode %in% c('matched_pair_dmc', 'all_pairs_dmc')) stopifnot(t$fixed_index$product_aware)
  }
  # Only elapsed-time metadata is removed. No numeric tolerance or environment normalization.
  values <- lapply(values, function(x) { attr(x, 'timings') <- NULL; x })
  stopifnot(identical(values[[1]], values[[2]], num.eq = FALSE))
  rng <- lapply(1:2, function(id) readRDS(file.path(root, paste0(mode, '-', id, '-rng.rds'))))
  stopifnot(identical(rng[[1]], rng[[2]], num.eq = FALSE))
  for (id in 1:2) {
    row <- read.csv(file.path(root, paste0(mode, '-', id, '.csv')))
    row$worker_startup_seconds <- timing[[id]]$worker_startup
    row$preparation_seconds <- timing[[id]]$preparation
    row$product_aware <- timing[[id]]$fixed_index$product_aware
    rows[[length(rows) + 1L]] <- row
  }
  cat('PASS:', mode, 'full returned result excluding timings and paired parent RNG states\n')
}
combined <- do.call(rbind, rows)
write.csv(combined, file.path(root, 'summary.csv'), row.names = FALSE)
print(combined[c('mode', 'run', 'elapsed_seconds', 'resampling_seconds', 'valid', 'product_aware')])
