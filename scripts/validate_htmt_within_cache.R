source('R/utils.R', encoding = 'UTF-8')
source('R/setup_custom_model_canvas_structural_validity.R', encoding = 'UTF-8')
source('R/setup_custom_model_canvas_structural_bootstrap.R', encoding = 'UTF-8')

# Previous implementation: recompute both within-factor means for every pair.
reference_htmt <- function(correlations, indicators_by_factor, threshold = .85, include_pairs = TRUE) {
  correlations <- as.matrix(correlations)
  factor_names <- names(indicators_by_factor)
  matrix_result <- matrix(NA_real_, length(factor_names), length(factor_names), dimnames = list(factor_names, factor_names))
  pairs <- list()
  if (length(factor_names) < 2L) return(list(matrix = matrix_result, pairs = data.frame(), threshold = threshold))
  pair_index <- 0L
  for (indices in utils::combn(seq_along(factor_names), 2L, simplify = FALSE)) {
    first <- factor_names[[indices[[1L]]]]
    second <- factor_names[[indices[[2L]]]]
    first_indicators <- unique(indicators_by_factor[[first]])
    second_indicators <- unique(indicators_by_factor[[second]])
    reason <- ''; value <- NA_real_
    if (length(first_indicators) < 2L || length(second_indicators) < 2L) {
      reason <- 'At least two indicators per factor are required'
    } else if (length(intersect(first_indicators, second_indicators))) {
      reason <- 'Cross-loaded indicators prevent standard HTMT calculation'
    } else if (!all(c(first_indicators, second_indicators) %in% rownames(correlations))) {
      reason <- 'Indicator correlations are unavailable'
    } else {
      heterotrait <- abs(correlations[first_indicators, second_indicators, drop = FALSE])
      first_monotrait <- abs(correlations[first_indicators, first_indicators, drop = FALSE][lower.tri(correlations[first_indicators, first_indicators, drop = FALSE])])
      second_monotrait <- abs(correlations[second_indicators, second_indicators, drop = FALSE][lower.tri(correlations[second_indicators, second_indicators, drop = FALSE])])
      denominator <- sqrt(mean(first_monotrait, na.rm = TRUE) * mean(second_monotrait, na.rm = TRUE))
      if (is.finite(denominator) && denominator > 0) value <- mean(heterotrait, na.rm = TRUE) / denominator else reason <- 'Within-factor correlations are insufficient'
    }
    matrix_result[first, second] <- matrix_result[second, first] <- value
    if (!isTRUE(include_pairs)) next
    pair_index <- pair_index + 1L
    pairs[[pair_index]] <- data.frame(Factor1 = first, Factor2 = second, HTMT = value,
      Criterion = if (is.finite(value)) if (value < threshold) 'Below reference' else 'Review needed' else 'Not assessed',
      Reason = reason, stringsAsFactors = FALSE)
  }
  list(matrix = matrix_result, pairs = if (isTRUE(include_pairs)) do.call(rbind, pairs) else data.frame(), threshold = threshold)
}
capture_htmt <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning')},
    message = function(m) {messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage')})
  list(value = value, warnings = warnings, messages = messages,
    rng = if (exists('.Random.seed', .GlobalEnv)) .Random.seed else NULL)
}
set.seed(77)
htmt_data <- as.data.frame(matrix(rnorm(80*12), 80, 12))
names(htmt_data) <- paste0('x', 1:12)
spec <- setNames(split(names(htmt_data), rep(1:4, each = 3)), LETTERS[1:4])
corr <- cor(htmt_data)
# Four factors have six pairs: four reusable within means + six between means.
# A second call must recompute them for the new matrix, including cached NaNs.
local({
  calls <- 0L
  counted <- structural_canvas_htmt
  environment(counted) <- list2env(list(mean = function(...) {
    calls <<- calls + 1L
    base::mean(...)
  }), parent = .GlobalEnv)
  counted(corr, spec)
  stopifnot(calls == 10L)
  changed <- corr
  changed[1:3,1:3] <- NA_real_
  before <- capture_htmt(reference_htmt(changed,spec))
  after <- capture_htmt(counted(changed,spec))
  stopifnot(identical(before,after,num.eq=FALSE), calls == 17L)
})
specs <- list(spec, spec[1:2], spec[1], list(), unname(spec), spec[c(4,2,1,3)],
  modifyList(spec, list(A = 'x1')), modifyList(spec, list(A = c('x1','absent'))),
  modifyList(spec, list(B = c('x1','x4','x5'))),
  modifyList(spec, list(A = c('x1','x2','x2','x3'))),
  setNames(spec, c('A','A','C','D')), setNames(spec, c('','B','C','D')))
all_na <- corr; all_na[1:3,1:3] <- NA_real_
nonfinite <- corr; nonfinite[2,1] <- Inf; nonfinite[3,1] <- NaN
zero <- corr; zero[1:3,1:3] <- 0
matrices <- list(corr, -corr, all_na, nonfinite, zero, corr[,12:1], corr[,1:10], as.data.frame(corr))
comparisons <- 0L
for (mat in matrices) for (s in specs) for (include in c(TRUE, FALSE)) {
  before <- capture_htmt(reference_htmt(mat, s, include_pairs = include))
  after <- capture_htmt(structural_canvas_htmt(mat, s, include_pairs = include))
  stopifnot(identical(before, after, num.eq = FALSE))
  comparisons <- comparisons + 1L
}
reference_boot <- structural_canvas_htmt_bootstrap
environment(reference_boot) <- list2env(list(structural_canvas_htmt = reference_htmt), parent = .GlobalEnv)
missing_data <- htmt_data; missing_data[1:6,1] <- NA_real_
constant_data <- htmt_data; constant_data$x1 <- 1
ordinal_data <- as.data.frame(lapply(htmt_data, function(x) as.integer(cut(x, c(-Inf,-.5,.5,Inf)))))
boot_cases <- list(list(data=htmt_data), list(data=missing_data), list(data=constant_data),
  list(data=ordinal_data, ordered=names(ordinal_data)),
  list(data=htmt_data, indicators_by_factor=specs[[9]]))
for (case in boot_cases) for (method in c('percentile','bias_corrected','bca')) {
  run <- function(fun) {
    progress <- list(); checks <- 0L
    args <- modifyList(list(data=htmt_data, indicators_by_factor=spec, reps=30L, seed=42L, ci_method=method), case)
    args$progress <- function(...) progress[[length(progress)+1L]] <<- list(...)
    args$cancel <- function() {checks <<- checks + 1L; FALSE}
    result <- capture_htmt(do.call(fun,args))
    list(result=result,progress=progress,checks=checks)
  }
  before <- run(reference_boot); after <- run(structural_canvas_htmt_bootstrap)
  stopifnot(is.null(before$result$value$error), identical(before,after,num.eq=FALSE))
  comparisons <- comparisons + 1L
}
for (seed_exists in c(TRUE,FALSE)) {
  if (seed_exists) set.seed(713) else rm('.Random.seed', envir=.GlobalEnv)
  run <- function(fun) {
    checks <- 0L
    capture_htmt(fun(htmt_data,spec,reps=30,seed=42,cancel=function() {checks <<- checks+1L; checks == 4L}))
  }
  before <- run(reference_boot); after <- run(structural_canvas_htmt_bootstrap)
  stopifnot(identical(before,after,num.eq=FALSE), identical(before$value$error,'HTMT bootstrap canceled.'))
  comparisons <- comparisons + 1L
}
cat('PASS:',comparisons,'exact HTMT output/diagnostic/RNG/progress/cancellation comparisons.\n')
