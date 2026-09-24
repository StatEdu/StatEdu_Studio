root <- 'output/sem-shared-inverse-final-5000-20260915'
values <- lapply(1:2, function(id) readRDS(file.path(root, paste0('all_pairs_dmc-', id), 'comparison.rds')))
for (version in c('baseline', 'profiled')) {
  stopifnot(identical(values[[1]][[version]], values[[2]][[version]], num.eq = FALSE))
}
rows <- lapply(1:2, function(id) {
  x <- values[[id]]
  stopifnot(identical(x$baseline, x$profiled, num.eq = FALSE))
  draws <- attr(x$profiled$value, 'bootstrap_draws')
  stopifnot(length(draws$sample_indices) == 5000L, length(draws$valid_mask) == 5000L,
            nrow(draws$raw) == 5000L, nrow(draws$standardized) == 5000L,
            all(draws$valid_mask))
  old <- x$baseline_timings$resampling
  new <- x$profiled_timings$resampling
  data.frame(run = id, baseline_seconds = old, candidate_seconds = new,
             saved_seconds = old - new, reduction_percent = 100 * (1 - new / old),
             valid = sum(draws$valid_mask))
})
out <- do.call(rbind, rows)
write.csv(out, file.path(root, 'summary.csv'), row.names = FALSE)
print(out)
cat('PASS: two A/B and two cross-process full result/draw/parent diagnostic/stdout/RNG comparisons; timings excluded only\n')
