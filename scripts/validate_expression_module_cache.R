.libPaths(R.home('library'))
scope<-new.env(parent=.GlobalEnv);sys.source('R/app_bootstrap.R',scope)
root<-tempfile('expression-cache-',tmpdir='output');dir.create(root)
modules<-file.path(root,'modules');dir.create(modules)
old_env<-Sys.getenv(c('STATEDU_MODULE_CACHE_DIR','STATEDU_MODULE_CACHE'),unset=NA_character_)
old_options<-options(keep.source=FALSE,verbose=FALSE)
restore<-function(){
 options(old_options)
 for(name in names(old_env))if(is.na(old_env[[name]]))Sys.unsetenv(name)else do.call(Sys.setenv,setNames(list(old_env[[name]]),name))
 rm(list=intersect(c('expression_cache_probe','expression_cache_fn'),ls(.GlobalEnv)),envir=.GlobalEnv)
}
tryCatch({
 Sys.setenv(STATEDU_MODULE_CACHE_DIR=file.path(root,'cache'),STATEDU_MODULE_CACHE='true')
 scope$app_module_files<-c('one.R','two.R');scope$utf8_app_module_files<-scope$app_module_files
 scope$optional_app_module_files<-c(latent_mplus='optional.R')
 writeLines('expression_cache_probe <- 1L',file.path(modules,'one.R'))
 writeLines('expression_cache_fn <- function(x) x + expression_cache_probe',file.path(modules,'two.R'))
 run<-function(){stopifnot(isTRUE(scope$source_app_modules(dir=modules)));expression_cache_fn(2L)}
 stopifnot(run()==3L)
 paths<-scope$app_module_cache_paths();cached<-readRDS(paths$expressions)
 stopifnot(is.expression(cached$expressions),identical(cached$version,R.version.string))
 # A valid expression cache succeeds even if the combined text cannot be parsed.
 text<-readBin(paths$source,'raw',file.info(paths$source)$size)
 writeLines('invalid <- (',paths$source);stopifnot(run()==3L)
 # Previously written default-gzip and uncompressed RDS files remain readable.
 for(compression in c(TRUE,FALSE)){
  saveRDS(cached,paths$expressions,compress=compression)
  stopifnot(run()==3L)
 }
 writeBin(text,paths$source)
 writeLines('expression_cache_probe <- 1234L',file.path(modules,'one.R'));stopifnot(run()==1236L)
 writeBin(charToRaw('broken RDS'),paths$expressions);stopifnot(run()==1236L,is.expression(readRDS(paths$expressions)$expressions))
 for(field in c('version','manifest','expressions')){
  bad<-readRDS(paths$expressions);bad[[field]]<-'invalid';saveRDS(bad,paths$expressions)
  stopifnot(run()==1236L,is.expression(readRDS(paths$expressions)$expressions))
 }
 original_paths<-scope$app_module_cache_paths
 scope$app_module_cache_paths<-function(){p<-original_paths();p$expressions<-file.path(root,'missing-parent','cache.rds');p}
 warnings<-character()
 stopifnot(withCallingHandlers(run(),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})==1236L,!length(warnings))
 scope$app_module_cache_paths<-original_paths
 # A failed write/copy must close its stream, remove its temporary file, and
 # retain the successful module evaluation without adding a warning.
 for(failure in c('serialize','copy','copy_false')){
  writeBin(charToRaw('broken RDS'),paths$expressions)
  connections_before<-rownames(showConnections(all=TRUE))
  temporaries_before<-list.files(dirname(paths$expressions),pattern='^app-expressions-')
  captured_connection<-NULL
  scope$gzfile<-function(...){captured_connection<<-base::gzfile(...);captured_connection}
  if(failure=='serialize')scope$saveRDS<-function(...)stop('simulated serialization failure')
  if(failure=='copy')scope$file.copy<-function(...)stop('simulated copy failure')
  if(failure=='copy_false')scope$file.copy<-function(...)FALSE
  warnings<-character()
  stopifnot(isTRUE(withCallingHandlers(scope$source_cached_app_modules(paths,readRDS(paths$manifest)),
   warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})),
   expression_cache_fn(2L)==1236L,!length(warnings),
   !is.null(captured_connection),!tryCatch(isOpen(captured_connection),error=function(e)FALSE),
   identical(connections_before,rownames(showConnections(all=TRUE))),
   identical(temporaries_before,list.files(dirname(paths$expressions),pattern='^app-expressions-')))
  rm(list=intersect(c('gzfile','saveRDS','file.copy'),ls(scope)),envir=scope)
 }
 stopifnot(run()==1236L)
 cached<-readRDS(paths$expressions);cached$expressions<-expression(stop('must not run'));saveRDS(cached,paths$expressions)
 options(keep.source=TRUE);stopifnot(run()==1236L,!is.null(attr(expression_cache_fn,'srcref')))
 options(keep.source=FALSE,verbose=TRUE)
 expected<-capture.output(source(paths$source,local=FALSE,encoding='UTF-8'))
 actual<-capture.output(scope$source_cached_app_modules(paths,readRDS(paths$manifest)))
 stopifnot(identical(expected,actual))
 options(verbose=FALSE)
 warnings<-character()
 stopifnot(withCallingHandlers(run(),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})==1236L,
           length(warnings)==1L,grepl('Combined app-module cache failed',warnings[[1]],fixed=TRUE))
 Sys.setenv(STATEDU_MODULE_CACHE='false');stopifnot(run()==1236L)
 Sys.setenv(STATEDU_MODULE_CACHE='true')
 # Custom file selection also bypasses the combined and expression caches.
 stopifnot(isTRUE(scope$source_app_modules(files='one.R',dir=modules)),expression_cache_probe==1234L)
 writeLines('expression_cache_probe <- expression_cache_probe + 10L',file.path(modules,'optional.R'))
 stopifnot(run()==1246L)
 cat('PASS: miss/hit, legacy gzip/plain compatibility, changed source, corrupt/type/manifest/version mismatch, write/copy failures and closed streams, keep.source/verbose, evaluation failure fallback, cache off/custom selection, optional module\n')
},finally=restore())
