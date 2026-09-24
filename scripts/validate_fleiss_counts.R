source('R/utils.R', encoding = 'UTF-8')
source('R/analysis_interrater_agreement.R', encoding = 'UTF-8')
reference <- new.env(parent = .GlobalEnv)
sys.source('R/analysis_interrater_agreement.R', reference)
body(reference$interrater_fleiss_kappa) <- quote({
  if (ncol(frame) < 3 || length(levels) < 2) return(NA_real_)
  counts <- t(apply(frame, 1L, function(row) {
    tabulate(match(as.character(row[!is.na(row)]), levels), nbins = length(levels))
  }))
  ratings_per_subject <- rowSums(counts)
  counts <- counts[ratings_per_subject >= 2, , drop = FALSE]
  ratings_per_subject <- ratings_per_subject[ratings_per_subject >= 2]
  if (nrow(counts) < 2 || length(unique(ratings_per_subject)) != 1L) return(NA_real_)
  n_raters <- ratings_per_subject[[1]]
  p_i <- (rowSums(counts^2) - n_raters) / (n_raters * (n_raters - 1))
  p_j <- colSums(counts) / (nrow(counts) * n_raters)
  p_bar <- mean(p_i)
  p_e <- sum(p_j^2)
  if (!is.finite(p_e) || abs(1 - p_e) < .Machine$double.eps) return(NA_real_)
  (p_bar - p_e) / (1 - p_e)
})
args <- commandArgs(TRUE)
if (length(args)) sys.source(args[[1]], reference)
capture <- function(expr) {
  warnings <- messages <- character()
  result <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(result = result, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(619)
checks <- 0L
for (n in c(0, 1, 2, 30, 1000)) for (k in c(2, 3, 5, 20)) {
  m <- matrix(sample(c('A', 'B', 'C', NA), n * k, TRUE), n, k)
  for (frame in list(m, as.data.frame(m), as.data.frame(lapply(as.data.frame(m), factor)),
                     matrix(NA_real_, n, k), matrix(sample(c(1, 2, 3, Inf, NaN), n*k, TRUE), n, k))) {
    for (lev in list(c('A', 'B', 'C'), c('C', 'A', 'B', 'unused'), c('A', NA, 'B'),
                     c('A', 'A', 'B'), character(), as.character(c(1, 2, 3, Inf)))) {
      before <- capture(reference$interrater_fleiss_kappa(frame, lev))
      after <- capture(interrater_fleiss_kappa(frame, lev))
      stopifnot(identical(before, after, num.eq = FALSE))
      checks <- checks + 1L
    }
  }
}
cat('PASS:', checks, 'exact Fleiss result/condition/RNG comparisons.\n')
