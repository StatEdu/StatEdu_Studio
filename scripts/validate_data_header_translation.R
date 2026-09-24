.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/data_header_translation_reference.R',old)
sys.source('R/data_ui_tables.R',new)
for(env in list(old,new)) for(nm in c('data_table_header_labels','data_table_colnames')) env[[nm]]<-compiler::cmpfun(env[[nm]])
capture<-function(env,fn,args){
 ds<-list();stdout<-capture.output(value<-tryCatch(withCallingHandlers(do.call(env[[fn]],args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=value,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
set.seed(20260915);checks<-0L
columns<-list(names(old$data_table_header_labels('en')),c('value_1','label_11','name','name','unknown','<>&',NA),character(),NULL,c(a='Message',b='Variable'),factor(c('name','Label')),c(1,2),matrix(c('value_11','label_1'),1))
for(language in list('en','ko','EN','KO','bad','',NULL,NA_character_,c('ko','en'))) {
 for(fn in c('data_table_header_labels','data_table_colnames')) for(col in if(fn=='data_table_header_labels') list(NULL) else columns) {
  args<-list(language=language);if(fn=='data_table_colnames') args['columns']<-list(col)
  seed<-.Random.seed;a<-capture(old,fn,args);.Random.seed<-seed;b<-capture(new,fn,args)
  stopifnot(identical(a,b,num.eq=FALSE));checks<-checks+1L
 }
}
cat('Exact header/colname values, attributes, diagnostics, stdout and RNG:',checks,'\n')
for(language in c('en','ko')) {
 columns<-names(old$data_table_header_labels(language))
 data<-as.data.frame(matrix(seq_len(2L*length(columns)),nrow=2L))
 names(data)<-columns
 render<-function(env){
  widget<-DT::datatable(data,rownames=FALSE,colnames=env$data_table_colnames(columns,language))
  widget$elementId<-'header-comparison'
  as.character(htmltools::renderTags(widget)$html)
 }
 stopifnot(identical(render(old),render(new),num.eq=FALSE))
}
cat('Fixed-ID full-header table HTML comparisons: 2\n')
if(length(commandArgs(TRUE))) {
 root<-'output/data-header-translation-20260915';dir.create(root,recursive=TRUE,showWarnings=FALSE)
 rows<-list()
 for(i in 1:5)for(kind in if(i%%2) c('old','new') else c('new','old')){
  env<-get(kind);elapsed<-system.time(for(j in 1:500) env$data_table_header_labels('ko'))[['elapsed']]
  rows[[length(rows)+1L]]<-data.frame(iteration=i,kind=kind,seconds=elapsed)
 }
 write.csv(do.call(rbind,rows),file.path(root,paste0('times-',commandArgs(TRUE)[1],'.csv')),row.names=FALSE)
 print(aggregate(seconds~kind,do.call(rbind,rows),median))
}
