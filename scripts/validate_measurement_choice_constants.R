.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/measurement-label-constants-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
for(env in list(old,new)) {
 sys.source('R/utils.R',env)
 sys.source('R/data_io.R',env)
}
sys.source('scripts/fixtures/measurement_choices_reference.R',old)
for(env in list(old,new))for(nm in c('statedu_measurement_choices','statedu_measurement_label','measurement_select_html','variable_table_display_data','variable_table_render_state'))env[[nm]]<-compiler::cmpfun(env[[nm]])
capture<-function(env,fn,args){
 ds<-list();stdout<-capture.output(v<-tryCatch(withCallingHandlers(do.call(env[[fn]],args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=v,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915);checks<-0L
for(lang in list('en','ko','EN','KO','bad','',NULL,NA_character_,c('ko','en'))) {
 a<-capture(old,'statedu_measurement_choices',list(language=lang));b<-capture(new,'statedu_measurement_choices',list(language=lang));stopifnot(identical(a,b,num.eq=FALSE));checks<-checks+1L
 stopifnot(identical(Encoding(names(a$value)),Encoding(names(b$value))))
 for(value in list('binary','category','ordered','continuous','ordinal','nominal','BAD','',NA_character_,NULL,c('binary','category')))for(name in c('v1','한글 <&"')) {
  args<-list(name=name,value=value,source_order=1L,language=lang)
  a<-capture(old,'measurement_select_html',args);b<-capture(new,'measurement_select_html',args)
  stopifnot(identical(a,b,num.eq=FALSE));checks<-checks+1L
 }
}
cat('Exact choices/HTML/diagnostics/stdout/RNG:',checks,'\n')

