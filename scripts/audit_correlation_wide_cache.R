.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
root <- 'output/correlation-wide-missing-20260915'
rewrite <- function(x) {
  if(!is.call(x)) return(x)
  if(identical(x[[1L]],quote(base::rank))) x[[1L]] <- as.name('.audit_rank')
  if(identical(x[[1L]],quote(base::unique))) x[[1L]] <- as.name('.audit_unique')
  for(i in seq_along(x)[-1L]) if(!identical(x[[i]],quote(expr=))) x[i] <- list(rewrite(x[[i]]))
  x
}
records <- list()
for(name in c('pearson_complete','spearman_complete','spearman_shared','spearman_different')) {
  expected <- readRDS(file.path(root,paste0(name,'-1.rds')))
  scope <- new.env(parent=environment(correlation_rank_test))
  scope$rank_calls <- 0L;scope$unique_calls <- 0L;scope$cache <- NULL
  scope$.audit_rank <- function(x,...) {scope$rank_calls <- scope$rank_calls+1L;base::rank(x,...)}
  scope$.audit_unique <- function(x,...) {scope$unique_calls <- scope$unique_calls+1L;base::unique(x,...)}
  factory <- correlation_rank_test;body(factory) <- rewrite(body(factory));environment(factory) <- scope
  host <- new.env(parent=environment(prepare_correlation_results))
  host$correlation_rank_test <- function(limit,cache_unique=FALSE) {scope$cache <- factory(limit,cache_unique);scope$cache}
  analysis <- prepare_correlation_results;environment(analysis) <- host
  data <- expected$value$data
  info <- data.frame(name=names(data),measurement='continuous')
  assign('.Random.seed',expected$rng,envir=.GlobalEnv)
  diagnostics <- list()
  stdout <- capture.output(value <- withCallingHandlers(analysis(data,names(data),variable_info=info,options=expected$value$options),
    warning=function(e){diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
    message=function(e){diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
  stopifnot(identical(expected,list(value=value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed),num.eq=FALSE))
  entries <- if(is.function(scope$cache)) environment(scope$cache)$entries else list()
  records[[name]] <- data.frame(case=name,pairs=nrow(value$pairwise_table),rank_calls=scope$rank_calls,
    cached_unique_calls=scope$unique_calls,final_entries=length(entries),cache_object_bytes=as.numeric(object.size(entries)))
}
result <- do.call(rbind,records)
write.csv(result,file.path(root,'cache-audit.csv'),row.names=FALSE)
print(result);cat('PASS: four instrumented complete results/diagnostics/stdout/RNG exactly equal uninstrumented captures\n')
