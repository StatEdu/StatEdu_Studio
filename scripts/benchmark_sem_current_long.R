.libPaths(R.home('library'))
Sys.setenv(R_LIBS_USER = normalizePath(R.home('library'), winslash = '/'))
args <- commandArgs(TRUE)
mode <- args[1]; id <- as.integer(args[2])
stopifnot(mode %in% c('complete', 'missing', 'matched_pair_dmc', 'all_pairs_dmc'),
          length(id) == 1L, !is.na(id))
root <- 'output/sem-current-long-20260915'
dir.create(root, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(STATEDU_BENCHMARK_REPS = '5000', STATEDU_BENCHMARK_WORKERS = '4',
           STATEDU_BENCHMARK_CHUNK_SIZE = '100',
           STATEDU_BENCHMARK_MISSING_RATE = if (mode == 'missing') '.1' else '0',
           STATEDU_NO_PACKAGE_INSTALL = 'true')
is_assignment <- function(x, name) is.call(x) && identical(x[[1]], as.name('<-')) &&
  identical(x[[2]], as.name(name))
execute <- function() {
  on.exit({
    if (exists('job', inherits = FALSE)) {
      if (job$process$is_alive()) statedu_stop_background_process_tree(job$process)
      if (dir.exists(job$directory)) {
        target <- normalizePath(job$directory, winslash = '/', mustWork = TRUE)
        allowed <- paste0(normalizePath(tempdir(), winslash = '/', mustWork = TRUE), '/')
        stopifnot(startsWith(tolower(target), tolower(allowed)))
        structural_canvas_cleanup_effect_bootstrap_job(job)
      }
    }
  }, add = TRUE)
  for (expression in parse('scripts/benchmark_sem_complete_bootstrap_5000.R')) {
    if (is.call(expression) && identical(expression[[1]], as.name('on.exit'))) next
    if (is_assignment(expression, 'job') && mode %in% c('matched_pair_dmc', 'all_pairs_dmc'))
      expression[[3]][['seed']] <- fixture$bootstrap_seed
    if (is_assignment(expression, 'started')) parent_rng_before <- .Random.seed
    eval(expression, environment())
    if (is_assignment(expression, 'script_path'))
      script_path <- normalizePath('scripts/benchmark_sem_complete_bootstrap_5000.R', winslash = '/')
    if (is_assignment(expression, 'snapshot') && mode %in% c('matched_pair_dmc', 'all_pairs_dmc')) {
      # Import only existing fixture definitions; do not execute its long A/B benchmark.
      definitions <- parse('scripts/benchmark_sem_product_bootstrap_5000.R')
      wanted <- c('node', 'edge', 'stable_data', 'stable_snapshot', 'stable_fixture')
      found <- 0L
      for (definition in definitions) for (name in wanted) if (is_assignment(definition, name)) {
        eval(definition, environment()); found <- found + 1L
      }
      stopifnot(found == length(wanted))
      fixture <- stable_fixture(mode, if (mode == 'matched_pair_dmc') 3L else 9L,
                                if (mode == 'matched_pair_dmc') 20260832L else 20260833L)
      data <- fixture$data; snapshot <- fixture$snapshot
    }
  }
  stopifnot(is.data.frame(result), nrow(result) > 0, all(is.finite(result$valid)),
            all(result$valid > 0), all(result$valid <= 5000L))
  summary$mode <- mode; summary$run <- id
  saveRDS(result, file.path(root, paste0(mode, '-', id, '.rds')))
  saveRDS(list(before = parent_rng_before, after = .Random.seed),
          file.path(root, paste0(mode, '-', id, '-rng.rds')))
  write.csv(summary, file.path(root, paste0(mode, '-', id, '.csv')), row.names = FALSE)
}
execute()
