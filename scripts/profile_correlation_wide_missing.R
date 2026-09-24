.libPaths(R.home('library'))
source('R/app_bootstrap.R'); load_app_packages(check=FALSE); source_app_modules()
statedu_apply_preferences(statedu_initial_preferences())
run <- as.integer(commandArgs(trailingOnly=TRUE)[1L])
stopifnot(length(run)==1L, !is.na(run), run %in% 1:2)
root <- 'output/correlation-wide-missing-20260915'
dir.create(root, recursive=TRUE, showWarnings=FALSE)
set.seed(20260915)
complete <- as.data.frame(matrix(rnorm(10000L*40L), ncol=40L))
names(complete) <- paste0('v', seq_len(40L))
shared <- complete; shared[seq_len(1000L), ] <- NA_real_
different <- complete
for (j in seq_along(different)) different[sample.int(nrow(different),1000L),j] <- NA_real_
info <- data.frame(name=names(complete), measurement='continuous')
cases <- list(pearson_complete=list(data=complete,method='pearson'),
  spearman_complete=list(data=complete,method='spearman'),
  spearman_shared=list(data=shared,method='spearman'),
  spearman_different=list(data=different,method='spearman'))
capture <- function(case) {
  diagnostics <- list()
  stdout <- capture.output(value <- withCallingHandlers(
    prepare_correlation_results(case$data,names(case$data),variable_info=info,
      options=list(continuous_method=case$method,normality=TRUE)),
    warning=function(e) {diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},
    message=function(e) {diagnostics[[length(diagnostics)+1L]] <<- list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}))
  list(value=value,diagnostics=diagnostics,stdout=stdout,rng=.Random.seed)
}
timings <- list(); profiles <- list()
for (name in if(run==1L) names(cases) else rev(names(cases))) {
  case <- cases[[name]]; expected <- capture(case)
  stopifnot(nrow(expected$value$pairwise_table)==780L)
  saveRDS(expected,file.path(root,paste0(name,'-',run,'.rds')))
  if(run==2L) stopifnot(identical(expected,readRDS(file.path(root,paste0(name,'-1.rds'))),num.eq=FALSE))
  for(i in seq_len(3L)) {
    gc(); started <- Sys.time(); actual <- capture(case)
    seconds <- as.numeric(difftime(Sys.time(),started,units='secs'))
    stopifnot(identical(expected,actual,num.eq=FALSE))
    timings[[length(timings)+1L]] <- data.frame(run,case=name,iteration=i,seconds)
  }
  path <- file.path(root,paste0(name,'-',run,'.Rprof'))
  Rprof(path,interval=0.01)
  actual <- tryCatch(capture(case),finally=Rprof(NULL))
  stopifnot(identical(expected,actual,num.eq=FALSE))
  p <- summaryRprof(path)
  write.csv(p$by.self,file.path(root,paste0(name,'-',run,'-self.csv')))
  write.csv(p$by.total,file.path(root,paste0(name,'-',run,'-total.csv')))
  profiles[[name]] <- data.frame(run,case=name,sampled_seconds=p$sampling.time,interval=p$sample.interval)
  cat(name,': exact results/diagnostics/stdout/RNG passed; median seconds=',median(vapply(tail(timings,3L),function(x)x$seconds,numeric(1))),'\n')
  print(head(p$by.self,8L))
}
write.csv(do.call(rbind,timings),file.path(root,paste0('timings-',run,'.csv')),row.names=FALSE)
write.csv(do.call(rbind,profiles),file.path(root,paste0('profiles-',run,'.csv')),row.names=FALSE)
cat('PASS: 16 within-process exact comparisons',if(run==2L)'and 4 cross-process exact comparisons' else '', '\n')
