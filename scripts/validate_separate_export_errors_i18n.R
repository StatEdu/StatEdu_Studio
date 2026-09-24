Sys.setlocale('LC_CTYPE','English_United States.utf8')
Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
files<-c('server_analysis.R','server_correlation.R','server_crosstabs.R','server_factor_analysis.R','server_frequencies.R','server_interrater_agreement.R','server_nonparametric.R','server_pca.R','server_reliability.R','server_ttest_anova.R','analysis_scope.R')
callbacks<-list()
walk<-function(node,file) {
 if(!is.call(node)&&!is.expression(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('tryCatch'))) {
  args<-as.list(node)[-1L]
  handler<-args[['error']]
  if(!is.null(handler)&&grepl('result_export_error_text',paste(deparse(handler),collapse=' '),fixed=TRUE)&&!grepl('result.figures_save_failed',paste(deparse(handler),collapse=' '),fixed=TRUE))
   callbacks[[length(callbacks)+1L]]<<-list(file=file,handler=handler)
 }
 for(child in as.list(node))if(!missing(child)&&(is.call(child)||is.expression(child)))walk(child,file)
}
for(file in files)walk(parse(file.path('R',file),encoding='UTF-8'),file)
stopifnot(length(callbacks)==34L)
notices<-character()
capture<-function(ui,...) {notices<<-c(notices,as.character(ui));invisible('test')}
showNotification<-capture
# Only notification dispatch is replaced; execute each actual parsed error callback.
assignInNamespace('showNotification',capture,ns='shiny')
keys<-paste0('result.export_error.',c('no_content','no_tables','excel_package','browser','pdf_path','office_browser','pdf_failed'))
detail<-'Tool: D:/사용자 & %s/report.pdf\ncode=17 <detail>'
errors<-c(lapply(keys,function(key)simpleError(statedu_t(key,'en'))),list(simpleError(paste0('PDF export failed.\n',detail)),simpleError(paste0('External: ',detail))))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 app_language_fn<-function()lang;language_fn<-function()lang
 expected<-c(vapply(keys,function(key)statedu_t(key,lang),character(1)),paste0(statedu_t('result.export_error.pdf_failed',lang),'\n',detail),paste0('External: ',detail))
 for(item in callbacks) {
  handler<-eval(item$handler)
  for(i in seq_along(errors)) {
   notices<-character();handler(errors[[i]])
   stopifnot(length(notices)==1L,endsWith(notices[[1]],expected[[i]]))
  }
 }
 cat('PASS:',lang,'34 actual error callbacks; owned translations and untouched external/PDF diagnostics\n')
}
