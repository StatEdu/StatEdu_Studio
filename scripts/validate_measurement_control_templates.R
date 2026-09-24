.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/measurement-control-template-20260915'
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
sys.source('scripts/fixtures/measurement_control_table_reference.R',old);sys.source('R/data_io.R',new)
for(env in list(old,new))for(nm in c('variable_table_display_data','variable_table_render_state')) env[[nm]]<-compiler::cmpfun(env[[nm]])
capture<-function(env,args){
 ds<-list();stdout<-capture.output(v<-tryCatch(withCallingHandlers(do.call(env$variable_table_render_state,args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=v,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
make_info<-function(n){data.frame(source_order=seq_len(n),name=if(n)paste0('v',seq_len(n))else character(),var_label=rep('<한글 & label>',n),measurement=rep(c('continuous','binary','category','ordered'),length.out=n),storage_type=rep('numeric',n),n_unique=rep(3L,n),n_missing=rep(0L,n),min_value=rep('0',n),max_value=rep('2',n))}
set.seed(20260915);checks<-0L
for(n in c(0L,1L,12L))for(lang in c('en','ko'))for(applied in c(FALSE,TRUE))for(role in c('dependent','independent','control'))for(mode in c('normal','duplicate','factor')){
 info<-make_info(n)
 if(mode=='duplicate'&&n>1L)info$name[2]<-info$name[1]
 if(mode=='factor') info$name<-factor(info$name)
 vars<-as.character(info$name)
 args<-list(info=info,checked_names=head(vars,2),selected_names=vars,assigned_elsewhere=tail(vars,1),dependent=head(vars,3),independent=head(vars,5),controls=tail(vars,5),selection_applied=applied,active_role=role,language=lang,measurement_overrides=c(v1='ordered'))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,a=a,b=b),file.path(root,'mismatch.rds'));stop('Exact mismatch')};checks<-checks+1L
}
cat('Exact display state, HTML cells, attributes, diagnostics, stdout, RNG:',checks,'\n')
render_checks<-0L
old_control<-new.env(parent=.GlobalEnv);new_control<-new.env(parent=.GlobalEnv)
for(lang in list('en','ko','bad','',NULL,NA_character_,c('ko','en'))) {
 renderer<-measurement_select_renderer(lang)
 old_control$variable_table_render_state<-function(name,value,source_order)measurement_select_html(name,value,source_order,lang)
 new_control$variable_table_render_state<-renderer
 for(repeat_index in 1:2)for(value in list('binary','category','ordered','continuous','ordinal','nominal','BAD','',NA_character_,NULL,c('binary','category'),factor('binary'),matrix('binary',1)))for(name in list('v1','한글 <&" % \\ data-name=""',NA_character_,factor('v1'),42)){
  args<-list(name=name,value=value,source_order=repeat_index)
  seed<-.Random.seed;a<-capture(old_control,args);.Random.seed<-seed;b<-capture(new_control,args)
  stopifnot(identical(a,b,num.eq=FALSE));render_checks<-render_checks+1L
 }
}
cat('Direct renderer comparisons including reuse:',render_checks,'\n')
for(language in c('en','ko')) {
 info<-make_info(12L)
 args<-list(info=info,checked_names=c('v1','v2'),selected_names=info$name,selection_applied=TRUE,language=language)
 render<-function(env){
  table<-do.call(env$variable_table_render_state,args)$table_data
  widget<-DT::datatable(table,rownames=FALSE,colnames=data_table_colnames(names(table),language),escape=FALSE,options=variable_table_options(language,compact=TRUE))
  widget$elementId<-'measurement-template-comparison'
  as.character(htmltools::renderTags(widget)$html)
 }
 stopifnot(identical(render(old),render(new),num.eq=FALSE))
}
cat('Fixed-ID table HTML comparisons: 2\n')

