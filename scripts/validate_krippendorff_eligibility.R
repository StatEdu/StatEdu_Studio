source('R/utils.R', encoding = 'UTF-8')
source('R/analysis_interrater_agreement.R', encoding = 'UTF-8')
reference <- interrater_krippendorff_alpha
b <- body(reference)
replaced <- 0L
for (i in seq_along(b)) {
  expr <- b[[i]]
  if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
      identical(expr[[2]], as.name('eligible_rows'))) {
    b[[i]] <- quote(eligible_rows <- apply(!is.na(values), 1L, sum) >= 2)
    replaced <- replaced + 1L
  }
}
stopifnot(replaced == 1L)
body(reference) <- b
args <- commandArgs(TRUE)
if (length(args)) {
  old <- new.env(parent = .GlobalEnv)
  sys.source(args[[1]], old)
  reference <- old$interrater_krippendorff_alpha
}
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(value = value, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(872)
checks <- 0L
for (n in c(0, 1, 2, 30)) for (k in c(0, 1, 2, 5)) {
  m <- matrix(sample(c(1:5, NA), n*k, TRUE), n, k)
  for (frame in list(m, as.data.frame(m), matrix(NA_real_, n, k), matrix(1, n, k))) {
    for (level in c('nominal', 'ordinal', 'continuous')) {
      before <- capture(reference(frame, as.character(1:5), level))
      after <- capture(interrater_krippendorff_alpha(frame, as.character(1:5), level))
      stopifnot(identical(before, after, num.eq = FALSE))
      checks <- checks + 1L
    }
  }
}
cat('PASS:', checks, 'exact Krippendorff result/condition/RNG comparisons.\n')
