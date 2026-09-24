Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
capture_error <- function(f) {result<-tryCatch(f(),error=identity);stopifnot(inherits(result,'error'));result}
data <- data.frame(id=1:2,x1=c(10,20),x2=c(30,40))
spec <- wide_long_make_spec(c('x1','x2'),'사용자_값',index_name='사용자_시점',index_values=c('Review','Normality'))
bad_spec <- spec;bad_spec$index_name <- ''
missing_dialog <- save_wide_long_result_file;environment(missing_dialog)<-new.env(parent=baseenv())
errors <- list(
 no_rows=capture_error(function()wide_long_transform_configured(data[FALSE,],list(spec))),
 repeated=capture_error(function()wide_long_transform(data,character())),
 source=capture_error(function()wide_long_make_spec(character(),'value')),
 dimensions=capture_error(function()wide_long_make_spec(c('x1','x2'),'value',unit_type='same',group_count=2L,time_count=2L)),
 save_dialog=capture_error(function()missing_dialog(data)),
 groups=capture_error(function()wide_long_transform_configured(data,list())),
 missing_source=capture_error(function()wide_long_transform_configured(data,list(wide_long_make_spec('missing','value')))),
 indicator=capture_error(function()wide_long_transform_configured(data,list(bad_spec)))
)
langs <- c('en','ko','ja','zh','es','fr','de','vi')
for(language in langs) {
 for(key in names(errors)) {
  actual <- wide_long_error_text(errors[[key]],language)
  stopifnot(identical(actual,statedu_t(paste0('wide_long.error.',key),language)))
  if(language=='en')stopifnot(identical(actual,conditionMessage(errors[[key]])))
  else stopifnot(!identical(actual,conditionMessage(errors[[key]])))
 }
 raw<-simpleError('External reader: D:/사용자 <&> %s/Review.csv')
 stopifnot(identical(wide_long_error_text(raw,language),conditionMessage(raw)))
 cat('PASS:',language,'8 actual error branches; external details preserved\n')
}
actual<-wide_long_transform_configured(data,list(spec),id_variables='id')
stopifnot(nrow(actual)==4L,all(c('사용자_값','사용자_시점') %in% names(actual)),
          identical(sort(actual[['사용자_값']]),c(10,20,30,40)),setequal(actual[['사용자_시점']],c('Review','Normality')))
notices<-new.env();notices$values<-character()
showNotification<-function(ui,...) {notices$values<-c(notices$values,as.character(ui));invisible('test')}
server<-function(input,output,session) {
 lang<-reactiveVal('en')
 register_wide_long_handlers(input,output,session,function()data,function()list(name='test.csv'),function()NULL,function()NULL,
                            function(...)stop('Invalid conversion must not replace data'),function()NULL,lang)
}
shiny::testServer(server, {
 session$setInputs(preview_wide_long=0,wide_long_set_spec=0,wide_long_move=0);session$flushReact()
 for(i in seq_along(langs)) {
  lang(langs[[i]]);session$flushReact();notices$values<-character()
  session$setInputs(preview_wide_long=i)
  stopifnot(identical(notices$values,statedu_t('wide_long.error.groups',langs[[i]])))
 }
 session$setInputs(wide_long_available=c('x1','x2'),wide_long_move=1)
 session$setInputs(wide_long_unit_type='same',wide_long_group_count=2,wide_long_time_count=2,wide_long_value_name='사용자_값')
 for(i in seq_along(langs)) {
  lang(langs[[i]]);session$flushReact();notices$values<-character()
  session$setInputs(wide_long_set_spec=i)
  stopifnot(identical(notices$values,statedu_t('wide_long.error.dimensions',langs[[i]])))
  cat('PASS:',langs[[i]],'preview and group setup notifications use current language\n')
 }
})
