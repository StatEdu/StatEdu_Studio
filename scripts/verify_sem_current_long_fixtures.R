.libPaths(R.home('library'))
source('R/app_bootstrap.R')
load_app_packages(check = FALSE)
source_app_modules()
wanted <- c('node', 'edge', 'stable_data', 'stable_snapshot', 'stable_fixture')
found <- 0L
for (x in parse('scripts/benchmark_sem_product_bootstrap_5000.R')) {
  if (is.call(x) && identical(x[[1]], as.name('<-')) &&
      as.character(x[[2]])[1] %in% wanted) {
    eval(x); found <- found + 1L
  }
}
stopifnot(found == length(wanted))
records <- list()
for (mode in c('matched_pair_dmc', 'all_pairs_dmc')) {
  fixture <- stable_fixture(mode, if (mode == 'matched_pair_dmc') 3L else 9L,
                            if (mode == 'matched_pair_dmc') 20260832L else 20260833L)
  original <- suppressWarnings(run_structural_canvas_analysis(
    fixture$snapshot, fixture$data, 'sem', estimator = 'ML', missing = 'fiml',
    std_lv = FALSE, ordered = character(0), nominal = character(0),
    residual_variance_fixes = numeric(0)))
  stopifnot(inherits(original$fit, 'lavaan'), original$converged, original$admissible,
            length(original$moderation_definitions) == 1L)
  definition <- original$moderation_definitions[[1L]]
  stopifnot(identical(definition$product_indicator_method, mode),
            definition$product_indicator_count == fixture$expected_products)
  records[[mode]] <- list(rows = nrow(fixture$data), indicators = ncol(fixture$data),
                          method = definition$product_indicator_method,
                          products = definition$product_indicator_count,
                          seed = fixture$bootstrap_seed)
  cat('PASS:', mode, 'product indicators:', definition$product_indicator_count, '\n')
}
saveRDS(records, 'output/sem-current-long-20260915/fixture-contracts.rds')
