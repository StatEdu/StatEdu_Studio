Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(7502)
d<-data.frame(id=rep(1:40,each=3),time=rep(1:3,40),group=rep(rep(0:1,each=20),each=3))
d$y<-2+d$group+.3*d$time+rep(rnorm(40),each=3)+rnorm(120)
info<-data.frame(name=names(d),var_label=names(d),measurement='continuous')
paths<-tempfile();dir.create(paths)
for(mode in c('reml_un','reml_ar1')) {
 settings<-normalize_longitudinal_settings(list(variables=list(outcome='y',id='id',time='time',predictors='group'),options=list(model_type='lmm',corstr=mode,missing_strategy='available',missing_imputations=9L,missing_iterations=7L,assumption_checks=FALSE)))
 reference<-NULL; saved<-NULL
 shiny::testServer(function(input,output,session){
  restore<-reactiveVal(list(settings=settings))
  module<-register_longitudinal_handlers(input,output,session,function()names(d),function()d,function()info,function()character(),function()NULL,function(){},restore_request_fn=restore)
 }, {
  session$flushReact()
  stopifnot(identical(module$settings(),settings))
  session$setInputs(run_longitudinal=1);session$flushReact()
  reference<<-module$results()[[1]]$coef_table
  saved<<-write_settings_json_file(list(longitudinal=module$settings()),file.path(paths,mode))$path
 })
 restored<-read_settings_json_file(saved)$longitudinal
 shiny::testServer(function(input,output,session){
  restore<-reactiveVal(list(settings=restored))
  module<-register_longitudinal_handlers(input,output,session,function()names(d),function()d,function()info,function()character(),function()NULL,function(){},restore_request_fn=restore)
 }, {
  session$flushReact()
  stopifnot(identical(module$settings(),settings))
  # Mimic controls rebinding in a new browser: unchanged model must not reset options.
  opts<-module$settings()$options
  do.call(session$setInputs,setNames(opts,paste0('longitudinal_',names(opts))))
  session$flushReact()
  stopifnot(identical(module$settings(),settings))
  session$setInputs(run_longitudinal=1);session$flushReact()
  stopifnot(identical(reference,module$results()[[1]]$coef_table))
  session$setInputs(longitudinal_options_tab='Checks');session$flushReact()
  stopifnot(identical(reference,module$results()[[1]]$coef_table))
  restore(list(settings=NULL));session$flushReact()
  stopifnot(length(module$settings()$variables$outcome)==0,module$settings()$options$model_type=='gee',is.null(module$results()))
 })
 cat(mode,'new-session settings, input rebinding, identical coefficient/SE/df/p/CI, and legacy reset passed\n')
}
