.libPaths(R.home('library'))
source('R/app_bootstrap.R'); load_app_packages(check=FALSE)
source_app_modules(); statedu_apply_preferences()

# Restore the previous preparation condition without storing another source copy.
baseline <- new.env(parent=.GlobalEnv)
current <- new.env(parent=.GlobalEnv)
baseline$prepare <- current$prepare <- prepare_correlation_results
replacements <- 0L
restore <- function(node) {
  if (is.call(node) && identical(node[[1]], as.name('<-')) &&
      identical(node[[2]], as.name('rank_test')) &&
      is.call(node[[3]]) && identical(node[[3]][[1]], as.name('if'))) {
    node[[3]][[2]] <- quote(!use_latent_correlations)
    replacements <<- replacements + 1L
    return(node)
  }
  if (is.call(node)) for (i in seq_along(node))
    if (!identical(node[[i]], quote(expr=))) node[i] <- list(restore(node[[i]]))
  node
}
body(baseline$prepare) <- restore(body(baseline$prepare))
stopifnot(replacements == 1L)
for (env in list(baseline, current)) {
  environment(env$prepare) <- env
  env$calls <- 0L
  env$correlation_rank_test <- eval(quote(function(...) {
    calls <<- calls + 1L
    get('correlation_rank_test', envir=.GlobalEnv)(...)
  }), env)
}
capture <- function(f) {
  set.seed(99); conditions <- list()
  stdout <- capture.output(value <- tryCatch(withCallingHandlers(f(),
    warning=function(w) {
      conditions[[length(conditions)+1L]] <<- list(class(w), conditionMessage(w))
      invokeRestart('muffleWarning')
    }, message=function(m) {
      conditions[[length(conditions)+1L]] <<- list(class(m), conditionMessage(m))
      invokeRestart('muffleMessage')
    }), error=function(e) list(error=class(e), message=conditionMessage(e))))
  list(value=value, conditions=conditions, stdout=stdout, rng=.Random.seed)
}
checked <- 0L
for (method in c('kendall', 'spearman', 'pearson', 'auto'))
for (kind in c('continuous', 'mixed_ordered', 'ordered', 'mixed_category'))
for (missing in c(FALSE, TRUE))
for (omitted in c(FALSE, TRUE)) {
  set.seed(33)
  d <- as.data.frame(matrix(sample(1:5, 600L, TRUE), 150L, 4L))
  measures <- switch(kind, continuous=rep('continuous', 4),
    mixed_ordered=rep(c('continuous', 'ordered'), 2), ordered=rep('ordered', 4),
    mixed_category=rep(c('continuous', 'category'), 2))
  if (missing) d[seq(1L, 150L, 7L), 2] <- NA
  if (omitted) { d$constant <- 1; measures <- c(measures, 'ordered') }
  info <- data.frame(name=names(d), measurement=measures)
  opts <- list(continuous_method=method, normality=FALSE, latent_correlations=FALSE)
  baseline$calls <- current$calls <- 0L
  a <- capture(function() baseline$prepare(d, names(d), variable_info=info, options=opts))
  b <- capture(function() current$prepare(d, names(d), variable_info=info, options=opts))
  stopifnot(identical(a, b, num.eq=FALSE), is.null(b$value$error), baseline$calls == 1L,
    current$calls == as.integer(!(method == 'kendall' && kind == 'continuous')))
  checked <- checked + 1L
}
cat(checked, 'complete results, conditions, stdout, RNG and provider call counts identical/as expected\n')
