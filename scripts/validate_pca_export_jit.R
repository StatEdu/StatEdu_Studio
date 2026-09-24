.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
original_jit <- compiler::enableJIT(0)
tryCatch({
 for(level in 0:3) for(fail in c(FALSE,TRUE)) {
  target <- saved_pca_results_html
  scope <- new.env(parent=environment(target))
  scope$render <- function(result,css_path,report_mode,plot_renderer) {
   observed <- compiler::enableJIT(0)
   stopifnot(observed==0L)
   if(fail) stop('expected render error')
   list(result,css_path,report_mode)
  }
  environment(target) <- scope
  compiler::enableJIT(level)
  value <- tryCatch(target(17,'custom.css',TRUE),error=function(e)e)
  restored <- compiler::enableJIT(0)
  stopifnot(restored==level)
  if(fail) stopifnot(inherits(value,'error'),conditionMessage(value)=='expected render error')
  else stopifnot(identical(value,list(17,'custom.css',TRUE)))
 }
},finally=invisible(compiler::enableJIT(original_jit)))
cat('PASS: JIT 0..3 restored on success/error, rendering runs at JIT 0, arguments preserved\n')
