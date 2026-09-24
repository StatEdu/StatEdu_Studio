source('R/app_bootstrap.R')
reference <- new.env(parent = .GlobalEnv)
sys.source('R/app_bootstrap.R', reference)
reference$app_module_cache_manifest <- function(paths) {
  info <- file.info(paths)
  data.frame(path = normalizePath(paths, winslash = '/', mustWork = FALSE),
    size = as.numeric(info$size), mtime = as.numeric(info$mtime), stringsAsFactors = FALSE)
}
restore_file_info <- function(expr) {
  if (identical(expr, quote(file.info(path, extra_cols = FALSE)))) return(quote(file.info(path)))
  if (is.call(expr)) for (i in seq_along(expr)) expr[[i]] <- restore_file_info(expr[[i]])
  expr
}
body(reference$write_combined_app_module_cache) <- restore_file_info(body(reference$write_combined_app_module_cache))
args <- commandArgs(TRUE)
if (length(args)) sys.source(args[[1]], reference)
root <- tempfile('manifest-check-')
dir.create(root)
fixture <- file.path(root, 'module with spaces.R')
writeLines('x <- 1', fixture)
Sys.setFileTime(fixture, as.POSIXct('2026-01-01', tz = 'UTC'))
paths <- file.path('R', app_module_files)
cases <- list(paths, character(), fixture, c(fixture, fixture),
              c(fixture, file.path(root, 'missing.R')), c(root, fixture),
              normalizePath(paths, winslash = '/'))
for (p in cases) stopifnot(identical(app_module_cache_manifest(p),
                                    reference$app_module_cache_manifest(p)))
original <- app_module_cache_manifest(fixture)
original_time <- file.info(fixture, extra_cols = FALSE)$mtime
writeLines('x <- 10000', fixture)
Sys.setFileTime(fixture, original_time)
resized <- app_module_cache_manifest(fixture)
stopifnot(!identical(original, resized), identical(original$mtime, resized$mtime))
Sys.setFileTime(fixture, original_time + 10)
retimed <- app_module_cache_manifest(fixture)
stopifnot(!identical(resized, retimed), identical(resized$size, retimed$size),
          identical(retimed, reference$app_module_cache_manifest(fixture)))
before <- file.path(root, 'before.R')
after <- file.path(root, 'after.R')
reference$write_combined_app_module_cache(paths, before)
write_combined_app_module_cache(paths, after)
read_bytes <- function(path) readBin(path, 'raw', n = file.info(path, extra_cols = FALSE)$size)
stopifnot(identical(read_bytes(before), read_bytes(after)))
cat('PASS: 7 manifest scenarios, size/mtime invalidation, combined module bytes identical.\n')
